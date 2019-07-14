#pragma once
#include "stdafx_async.h"
#include <mutex>
#include <variant>
#include <future>
#include <functional>

namespace ASYNC_NAMESPACE
{
	enum class execution
	{
		async = 0,
		wait = 1
	};
	namespace details
	{
		template<typename T>
		struct is_tuple : std::false_type {};


		template<typename... Ts>
		struct is_tuple<std::tuple<Ts...>> : std::true_type {};
		template<typename T>
		struct state_object
		{
			std::mutex mutex;
			std::condition_variable condition_variable;
			std::variant<std::monostate, std::exception_ptr, T> data;
		};

		template<typename T>
		struct promise
		{
			state_object<T>* state;

			template<int I, typename... Args>
			void set(Args&& ... args)
			{
				auto lock = std::unique_lock(state->mutex);
				state->data.template emplace<I>(std::forward<Args>(args)...);
				state->condition_variable.notify_one();
			}

			template<typename... Args>
			void set_value(Args&& ... args) { set<2>(std::forward<Args>(args)...); }
			template<typename Y>
			void set_exception(Y&& e) { set<1>(std::forward<Y>(e)); }
		};

		template<typename param_t, typename fn_t>
		struct promise_wrapper
		{
			param_t param;
			fn_t func;

			template<typename... Args>
			auto set_value(Args&& ... args)
			{
				if constexpr (is_tuple<param_t>::value)
				{
					param.set_value(std::apply(func, std::forward<Args>(args)...));
				}
				else
				{
					param.set_value(std::invoke(func, std::forward<Args>(args)...));
				}
			}

			template<typename T>
			void set_exception(T&& e) { param.set_exception(std::forward<T>(e)); }
		};


		template<typename T>
		struct function_traits;

		template<typename R, typename ...Args>
		struct function_traits<std::function<R(Args...)>>
		{
			static const size_t nargs = sizeof...(Args);

			typedef R result_type;

			template <size_t i>
			struct arg
			{
				typedef typename std::tuple_element<i, std::tuple<Args...>>::type type;
			};
		};


		template<typename T>
		struct type_wrapper
		{
			using type = T;
		};

		template<typename T>
		using promise_variant = std::variant<details::promise<T>, std::promise<T>>;

		template<typename T>
		struct is_variant : std::false_type {};


		template<typename... Ts>
		struct is_variant<std::variant<Ts...>> : std::true_type {};


	}

	struct invocation_index
	{
		operator size_t() const noexcept { return index; };
		size_t index{};
	};


	template<typename FN1, typename FN2>
	auto then(FN1&& task1, FN2&& task2)
	{
		return[task1 = std::forward<FN1>(task1), task2 = std::forward<FN2>(task2)](auto p)
		{
			using param_t = decltype(p);

			if constexpr (std::is_same<param_t, void*>::value)
			{
				return details::type_wrapper<typename details::function_traits<decltype(std::function{ std::declval<FN2>() }) > ::result_type > {};
			}
			else
			{
				if constexpr (details::is_variant<decltype(p)>::value)
				{
					std::visit([&](auto&& value) {
						std::invoke(task1, details::promise_wrapper<decltype(value), decltype(task2)>{std::forward<decltype(value)>(value), task2}); }, p);
				}
				else
				{
					std::invoke(task1, details::promise_wrapper<param_t, decltype(task2)>{std::forward<param_t>(p), task2});
				}
			}
		};
	}

	template<typename FN1, typename FN2, typename ... FNn>
	auto then(FN1&& task1, FN2&& task2, FNn&& ... tasks)
	{
		return then(then(std::forward<FN1>(task1), std::forward<FN2>(task2)), std::forward<FNn>(tasks)...);
	}


	namespace details
	{
		template<typename FN, typename... FNn>
		auto into_impl(FN&& task2, FNn&& ... tasks)
		{
			return then([tasks = std::forward_as_tuple(tasks...)](auto p)
			{
				std::apply([&](auto&&... args) 
					{
						p.set_value(compute<execution::wait>(args)...);
					}, std::move(tasks));
			}, std::forward<FN>(task2));
		}

		template <typename... T, std::size_t... I>
		auto subtuple_(std::tuple<T...>&& t, std::index_sequence<I...>) {
			return std::make_tuple(std::get<I>(t)...);
		}

		template <int Trim, typename... T>
		auto subtuple(const std::tuple<T...>& t) {
			return subtuple_(t, std::make_index_sequence<sizeof...(T) - Trim>());
		}
	}

	template<typename... FNn>
	auto into(FNn&&... tasks)
	{
		auto task2 = std::get<sizeof...(FNn) - 1 >(std::tie(tasks...));
		auto subtuple = details::subtuple_(std::forward_as_tuple(tasks...), std::make_index_sequence<sizeof...(FNn) - 1>());
		return then([tasks = std::forward<decltype(subtuple)>(subtuple)](auto p)
		{
			std::apply([&](auto&& ... args)
				{
					p.set_value(compute<execution::wait>(args)...);
				}, std::move(tasks));
		}, std::forward<decltype(task2 )>(task2));
	}

	template<typename FN>
	auto parallel_n(FN&& task, size_t count = 0)
	{
		return [task = std::forward<FN>(task), count](auto p)
		{
			using result_t = typename decltype(std::declval<FN>()(std::declval<void*>()))::type;

			if constexpr (std::is_same<decltype(p), void*>::value)
			{
				return details::type_wrapper<std::vector<std::future<result_t>>>{};
			}
			else
			{
				std::vector<std::future<result_t>> futures;
				for (auto i = 0u; i < count; ++i)
				{
					futures.emplace_back(std::move(compute<execution::async>(task)));
				}
				p.set_value(std::move(futures));
			}
		};
	}

	template<typename... FN>
	auto parallel(FN&& ... tasks)
	{
		static_assert(sizeof...(FN) > 1);
		return[tasks = std::forward_as_tuple(tasks...)](auto p)
		{
			using result_t = typename decltype(std::declval<decltype(std::get<0>(std::declval<std::tuple<FN...>>()))>()(std::declval<void*>()))::type;

			if constexpr (std::is_same<decltype(p), void*>::value)
			{
				return details::type_wrapper<std::vector<std::future<result_t>>>{};
			}
			else
			{
				std::vector<std::future<result_t>> futures;

				std::apply([&](auto&& ... task)
					{
						(futures.emplace_back(std::move(compute<execution::async>(task))), ...);
					}, tasks);

				if constexpr (details::is_variant<decltype(p)>::value)
				{
					std::visit([&](auto&& value) {
						value.set_value(std::move(futures));}, p);
				}
				else
				{
					p.set_value(std::move(futures));
				}
			}
		};
	}

	template<execution ex = execution::async, typename Task, typename T = typename decltype(std::declval<Task>()(std::declval<void*>()))::type>
	auto compute(Task && task)
	{
		if constexpr (ex == execution::async)
		{
			details::state_object<T> state;

			std::promise<T> promise;
			auto future = promise.get_future();
			task(std::move(promise));
			return future;
		}
		else
		{
			details::state_object<T> state;

			task(details::promise_variant<T>{details::promise<T>{&state}});

			{
				auto lock = std::unique_lock{ state.mutex };
				state.condition_variable.wait(lock, [&state]()
					{return state.data.index() != 0; });
			}

			if (state.data.index() == 1)
				std::rethrow_exception(std::get<1>(state.data));

			return std::move(std::get<2>(state.data));
		}
	}

	struct sink
	{
		template<typename... Args>
		void set_value(Args&& ... args) { }

		template<typename T>
		void set_exception(T&& e) { std::terminate(); }
	};
}