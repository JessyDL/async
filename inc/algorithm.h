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

			template <size_t I>
			struct arg
			{
				typedef typename std::tuple_element<I, std::tuple<Args...>>::type type;
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

		template<typename Fn>
		auto wrap(Fn&& fn)
		{
			if constexpr (std::is_invocable<Fn>::value)
			{
				return then([](auto p) { p.set_value(); }, std::forward<Fn>(fn));
			}
			else
			{
				return fn;
			}
		}

		template<typename... Fn>
		auto wrap_all(Fn&&... fn)
		{
			return std::make_tuple(wrap(std::forward<Fn>(fn))...);
		}
	}


	template<typename FN1, typename FN2>
	auto then(FN1&& task1, FN2&& task2)
	{
		return[task1 = details::wrap(std::forward<FN1>(task1)), task2 = std::forward<FN2>(task2)](auto p)
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
				std::apply([&](auto&& ... args)
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
	auto into(FNn&& ... tasks)
	{
		auto task2 = std::get<sizeof...(FNn) - 1 >(std::tie(tasks...));
		auto subtuple = details::subtuple_(std::forward_as_tuple(tasks...), std::make_index_sequence<sizeof...(FNn) - 1>());
		return then([tasks = std::forward<decltype(subtuple)>(subtuple)](auto p)
		{
			std::apply([&](auto&& ... args)
				{
					p.set_value(compute<execution::wait>(args)...);
				}, std::move(tasks));
		}, std::forward<decltype(task2)>(task2));
	}

	template<typename FN>
	auto parallel_n(FN&& task, size_t count = 0)
	{
		return[task = details::wrap(std::forward<FN>(task)), count](auto p)
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
		
		return[tasks = details::wrap_all(std::forward<FN>(tasks)...)](auto p)
		{
			using result_t = typename decltype(std::declval<decltype(std::get<0>(std::declval<decltype(tasks )>()))>()(std::declval<void*>()))::type;

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
						value.set_value(std::move(futures)); }, p);
				}
				else
				{
					p.set_value(std::move(futures));
				}
			}
		};
	}

	template<execution ex = execution::async, typename Task>
	auto compute(Task && task)
	{
		// we are dealing with a task lambda
		if constexpr (!std::is_invocable<Task, void*>::value)
		{
			return compute<ex>(details::wrap(task));
		}
		else
		{
			using T = typename decltype(std::declval<Task>()(std::declval<void*>()))::type;
			if constexpr (ex == execution::async)
			{
				details::state_object<T> state;

				std::promise<T> promise;
				auto future = promise.get_future();

				std::thread t
				{
					[task = std::forward<Task>(task)] (std::promise<T> promise) mutable
					{
						task(std::move(promise));
					}, std::move(promise)
				};
				t.detach();

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
	}

	/*
	template<execution ex = execution::async, typename Task>
	auto compute(Task&& task)
	{
		if constexpr (std::is_invocable<Task, void>::value)
		{
			using T = decltype(std::declval<Task>()());
			if constexpr (ex == execution::async)
			{
				details::state_object<T> state;
				std::promise<T> promise;
				promise = std::move(task());
				return promise.get_future();
			}
			else
			{
				return task();
			}
		}
		else
		{
			using T = decltype(std::declval<Task>()(std::declval<void*>()));
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
	}
	*/

	struct sink
	{
		template<typename... Args>
		void set_value(Args&& ... args) { }

		template<typename T>
		void set_exception(T&& e) { std::terminate(); }
	};
}