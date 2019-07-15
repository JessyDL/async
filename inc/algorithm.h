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
	struct invocation
	{
		size_t index{ 0u };
		size_t count{ 0u };
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

		template<>
		struct state_object<void>
		{
			std::mutex mutex;
			std::condition_variable condition_variable;
			std::variant<std::monostate, std::exception_ptr, int> data;
		};

		template<typename T>
		struct promise
		{
			state_object<T>* state;

			template<int I, typename... Args>
			void set(Args&& ... args)
			{
				auto lock = std::unique_lock(state->mutex);
				if constexpr (std::is_same<T, void>::value && I == 2)
				{
					state->data.template emplace<I>(1);
				}
				else
				{
					state->data.template emplace<I>(std::forward<Args>(args)...);
				}
				state->condition_variable.notify_one();
			}

			template<typename... Args>
			void set_value(Args&& ... args) { set<2>(std::forward<Args>(args)...); }
			template<typename Y>
			void set_exception(Y&& e) { set<1>(std::forward<Y>(e)); }
		};

		struct task_tag_type {};

		struct scheduler
		{
			~scheduler()
			{
				for (auto& thread : m_Workers)
					thread.join();
			}
			template<typename T>
			void start_work(T& p)
			{
				if (m_Execution == execution::wait)
				{
					p.set_value();
				}
				else
				{
					m_Workers.emplace_back(
						[=]() mutable
						{
							p.set_value();
						}
					);
				}
			}

			execution m_Execution;
			std::vector<std::thread> m_Workers;
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
		struct promise_type { using type = T; };
		template<typename T>
		struct promise_type<std::promise<T>> { using type = T; };
		template<typename T>
		struct promise_type<async::details::promise<T>> { using type = T; };


		template<typename param_t, typename fn_t, typename... fArgs>
		struct promise_wrapper
		{
			param_t param;
			fn_t func;
			details::scheduler* scheduler;
			std::tuple<fArgs...> fargs;

			void spawn()
			{
				scheduler->start_work(*this);
			}

			template<typename... Args>
			auto set_value(Args&& ... args)
			{
				if constexpr (is_packaged_task< fn_t>::value)
				{
					static_assert(sizeof...(Args) == 0); // we don't support chaining into another task with results, todo;
					// example: 
					// then(then([](){}, [](){ return 0;}), then([](int value) { return value + 1;}, [](int value){ return value + 2; }));

					using return_t = typename std::invoke_result<decltype(func), details::task_tag_type, details::scheduler*>::type::type;
					if constexpr (std::is_same<return_t, void>::value)
					{
						compute<execution::wait>(func);
						param.set_value();
					}
					else
						param.set_value(compute<execution::wait>(func));
				}
				else
				{
					if constexpr (is_tuple<param_t>::value)
					{
						if constexpr (std::is_same<decltype(std::apply(std::declval<fn_t>(), std::declval<Args>()...)), void>::value)
						{
							std::apply(func, std::forward<Args>(args)...);
							param.set_value();
						}
						else
						{
							param.set_value(std::apply(func, std::forward<Args>(args)...));
						}
					}
					else
					{
						if constexpr (std::is_same<decltype(std::invoke(std::declval<fn_t>(), std::declval<Args>()...)), void>::value)
						{
							std::invoke(func, std::forward<Args>(args)...);
							param.set_value();
						}
						else
						{
							param.set_value(std::invoke(func, std::forward<Args>(args)...));
						}
					}
				}
			}

			template<typename T>
			void set_exception(T&& e) { param.set_exception(std::forward<T>(e)); }
		};

		template<typename T>
		struct type_wrapper
		{
			using type = T;
		};

		template<typename T>
		struct type_wrapper<type_wrapper<T>> : public type_wrapper<T>
		{
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
				return then([](auto p)
					{
						p.spawn();
					}, std::forward<Fn>(fn));
			}
			else
			{
				return fn;
			}
		}

		template<typename Fn>
		auto parallel_wrap(Fn&& fn)
		{
			if constexpr (std::is_invocable<Fn>::value || std::is_invocable<Fn, invocation>::value)
			{
				return then([](auto p)
					{
						p.spawn();
					}, std::forward<Fn>(fn));
			}
			else
			{
				return fn;
			}
		}

		template<typename... Fn>
		auto wrap_all(Fn&& ... fn)
		{
			return std::make_tuple(wrap(std::forward<Fn>(fn))...);
		}

		template<typename Fn>
		struct is_packaged_task : public std::is_invocable<Fn, details::task_tag_type, details::scheduler*>
		{
		};

		template<typename T, typename SFINAE = void>
		struct packaged_task_result
		{
			using type = typename std::invoke_result<decltype(details::wrap(std::declval<T>())), details::task_tag_type, details::scheduler*>::type::type;
		};

		template<typename T>
		struct packaged_task_result<T, typename std::enable_if<!details::is_packaged_task<T>::value&& std::is_invocable<T, invocation>::value>::type>
		{
			using type = typename std::invoke_result<T, invocation>::type;
		};
	}


	template<typename FN1, typename FN2>
	auto then(FN1&& task1, FN2&& task2)
	{
		return[task1 = details::wrap(std::forward<FN1>(task1)), task2 = std::forward<FN2>(task2)](auto p, details::scheduler* scheduler)
		{
			using param_t = decltype(p);

			if constexpr (std::is_same<param_t, details::task_tag_type>::value)
			{
				// when chaining into a packaged task
				if constexpr (details::is_packaged_task<FN2>::value)
				{
					return typename std::invoke_result<FN2, details::task_tag_type, details::scheduler*>::type{};
				}
				else
				{
					using func_t = details::function_traits<decltype(std::function{ std::declval<FN2>() }) > ;
					return details::type_wrapper<typename func_t::result_type> {};
				}
			}
			else
			{
				if constexpr (details::is_variant<decltype(p)>::value)
				{
					if constexpr (details::is_packaged_task<decltype(task1)>::value)
					{
						std::visit([&](auto&& value)
							{
								std::invoke(task1, details::promise_wrapper<decltype(value), decltype(task2)>{std::forward<decltype(value)>(value), task2, scheduler}, scheduler);
							}, p);
					}
					else
					{
						std::visit([&](auto&& value)
							{
								std::invoke(task1, details::promise_wrapper<decltype(value), decltype(task2)>{std::forward<decltype(value)>(value), task2, scheduler});
							}, p);
					}
				}
				else
				{
					if constexpr (details::is_packaged_task<decltype(task1)>::value)
					{
						std::invoke(task1, details::promise_wrapper<param_t, decltype(task2)>{std::forward<param_t>(p), task2, scheduler}, scheduler);
					}
					else
					{
						std::invoke(task1, details::promise_wrapper<param_t, decltype(task2)>{std::forward<param_t>(p), task2, scheduler});
					}
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
		return then([tasks = details::subtuple_(std::forward_as_tuple(tasks...), std::make_index_sequence<sizeof...(FNn) - 1>())](auto p)
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
		using result_t = typename details::packaged_task_result<FN>::type;

		return then([task = details::wrap(std::forward<FN>(task)), count]() {
			std::vector<std::future<result_t>> futures;
			for (auto i = 0u; i < count; ++i)
			{
				if constexpr (std::is_invocable<FN, invocation>::value && !details::is_packaged_task<FN>::value)
				{
					futures.emplace_back(std::move(compute<execution::async>(then([i, count]() { return invocation{ i, count }; }, task))));
				}
				else
				{
					futures.emplace_back(std::move(compute<execution::async>(task)));
				}
			}
			return futures;
		}, [](std::vector<std::future<result_t>> futures)
		{
			if constexpr (!std::is_same< result_t, void>::value)
			{
				std::vector<result_t> values;
				values.reserve(futures.size());
				for (auto&& value : futures)
					values.emplace_back(std::move(value.get()));
				return values;
			}
			else
			{
				for (auto&& value : futures)
					value.get();
			}
		});
	}

	template<typename... FN>
	auto parallel(FN&& ... tasks)
	{
		static_assert(sizeof...(FN) > 1);

		using result_t = typename std::invoke_result<decltype(std::get<0>(std::declval<decltype(details::wrap_all(std::declval<FN>()...))>())), details::task_tag_type, details::scheduler*>::type::type;

		return then([tasks = details::wrap_all(std::forward<FN>(tasks)...)]()
		{
			std::vector<std::future<result_t>> futures;
			std::apply([&](auto&& ... task)
				{
					(futures.emplace_back(std::move(compute<execution::async>(task))), ...);
				}, tasks);
			return futures;
		},
			[](std::vector<std::future<result_t>> futures) {
			if constexpr (!std::is_same< result_t, void>::value)
			{
				std::vector<result_t> values;
				values.reserve(futures.size());
				for (auto&& value : futures)
					values.emplace_back(std::move(value.get()));
				return values;
			}
			else
			{
				for (auto&& value : futures)
					value.get();
			}
		});
	}

	template<execution ex = execution::async, typename Task, typename... Args>
	auto compute(Task&& task, Args&& ... args)
	{
		if constexpr (!details::is_packaged_task<Task>::value)
		{
			return compute<ex>(details::wrap(std::forward<Task>(task)));
		}
		else
		{
			using T = typename std::invoke_result<Task, details::task_tag_type, details::scheduler*>::type::type;
			details::state_object<T> state;
			if constexpr (ex == execution::async)
			{
				return std::async(
					[task = std::forward<Task>(task)]() mutable
				{
					if constexpr (!std::is_same<T, void>::value)
						return compute<execution::wait>(std::forward<Task>(task));
					else
						compute<execution::wait>(std::forward<Task>(task));
				}
				);
			}
			else
			{
				details::scheduler scheduler{ execution::wait };

				task(details::promise_variant<T>{details::promise<T>{&state}}, & scheduler, std::forward<Args>(args)...);

				{
					auto lock = std::unique_lock{ state.mutex };
					state.condition_variable.wait(lock, [&state]()
						{return state.data.index() != 0; });
				}

				if (state.data.index() == 1)
					std::rethrow_exception(std::get<1>(state.data));

				if constexpr (!std::is_same<T, void>::value)
					return std::move(std::get<2>(state.data));
			}
		}
	}
}