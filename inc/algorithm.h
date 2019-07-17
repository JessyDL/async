#pragma once
#include "stdafx_async.h"
#include <mutex>
#include <variant>
#include <future>
#include <functional>
//
//namespace ASYNC_NAMESPACE
//{
//	enum class execution
//	{
//		async = 0,
//		wait = 1
//	};
//	struct invocation
//	{
//		size_t index{ 0u };
//		size_t count{ 0u };
//	};
//	namespace details
//	{
//		template<typename T>
//		struct is_tuple : std::false_type {};
//
//		template<typename... Ts>
//		struct is_tuple<std::tuple<Ts...>> : std::true_type {};
//
//		template<typename FN1, typename FN2>
//		struct continuation {};
//
//		template<typename T>
//		struct is_continuation : std::false_type {};
//		template<typename FN1, typename FN2>
//		struct is_continuation<continuation<FN1, FN2>> : std::true_type {};
//		template<typename FN1, typename FN2>
//		struct is_continuation<continuation<FN1, FN2>&> : std::true_type {};
//		template<typename FN1, typename FN2>
//		struct is_continuation<const continuation<FN1, FN2>&> : std::true_type {};
//		template<typename FN1, typename FN2>
//
//		struct is_continuation<const continuation<FN1, FN2>> : std::true_type {};
//		template<typename T>
//		struct state_object
//		{
//			std::mutex mutex;
//			std::condition_variable condition_variable;
//			std::variant<std::monostate, std::exception_ptr, T> data;
//		};
//
//		template<>
//		struct state_object<void>
//		{
//			std::mutex mutex;
//			std::condition_variable condition_variable;
//			std::variant<std::monostate, std::exception_ptr, int> data;
//		};
//
//		template<typename T>
//		struct promise
//		{
//			state_object<T>* state;
//
//			template<int I, typename... Args>
//			void set(Args&& ... args)
//			{
//				auto lock = std::unique_lock(state->mutex);
//				if constexpr (std::is_same<T, void>::value && I == 2)
//				{
//					state->data.template emplace<I>(1);
//				}
//				else
//				{
//					state->data.template emplace<I>(std::forward<Args>(args)...);
//				}
//				state->condition_variable.notify_one();
//			}
//
//			template<typename... Args>
//			void set_value(Args&& ... args) { set<2>(std::forward<Args>(args)...); }
//			template<typename Y>
//			void set_exception(Y&& e) { set<1>(std::forward<Y>(e)); }
//		};
//
//		struct task_tag_type {};
//
//		struct scheduler
//		{
//			~scheduler()
//			{
//				for (auto& thread : m_Workers)
//					thread.join();
//			}
//			template<typename T>
//			void start_work(T& p)
//			{
//				if (m_Execution == execution::wait)
//				{
//					p.set_value();
//				}
//				else
//				{
//					m_Workers.emplace_back(
//						[=]() mutable
//						{
//							p.set_value();
//						}
//					);
//				}
//			}
//
//			execution m_Execution;
//			std::vector<std::thread> m_Workers;
//		};
//
//		template<typename T>
//		struct function_traits;
//
//		template<typename R, typename ...Args>
//		struct function_traits<std::function<R(Args...)>>
//		{
//			static const size_t nargs = sizeof...(Args);
//
//			typedef R result_type;
//
//			template <size_t I>
//			struct arg
//			{
//				typedef typename std::tuple_element<I, std::tuple<Args...>>::type type;
//			};
//		};
//
//
//		template<typename T>
//		struct promise_type { using type = T; };
//		template<typename T>
//		struct promise_type<std::promise<T>> { using type = T; };
//		template<typename T>
//		struct promise_type<async::details::promise<T>> { using type = T; };
//
//
//		template<typename param_t, typename fn_t>
//		struct promise_wrapper
//		{
//			param_t param;
//			fn_t func;
//			details::scheduler* scheduler;
//
//			void spawn()
//			{
//				scheduler->start_work(*this);
//			}
//
//			template<typename... Args>
//			auto set_value(Args&& ... args)
//			{
//				if constexpr (is_packaged_task<fn_t>::value)
//				{
//					static_assert(sizeof...(Args) == 0); // we don't support chaining into another task with results, todo;
//					// example: 
//					// then(then([](){}, [](){ return 0;}), then([](int value) { return value + 1;}, [](int value){ return value + 2; }));
//
//					using return_t = typename std::invoke_result<decltype(func), details::task_tag_type, details::scheduler*>::type::type;
//					if constexpr (std::is_same<return_t, void>::value)
//					{
//						compute<execution::wait>(func);
//						param.set_value();
//					}
//					else
//						param.set_value(compute<execution::wait>(func));
//				}
//				else
//				{
//					if constexpr (is_tuple<param_t>::value)
//					{
//						if constexpr (std::is_same<decltype(std::apply(std::declval<fn_t>(), std::declval<Args>()...)), void>::value)
//						{
//							std::apply(func, std::forward<Args>(args)...);
//							param.set_value();
//						}
//						else
//						{
//							param.set_value(std::apply(func, std::forward<Args>(args)...));
//						}
//					}
//					else
//					{
//						if constexpr (std::is_same<decltype(std::invoke(std::declval<fn_t>(), std::declval<Args>()...)), void>::value)
//						{
//							std::invoke(func, std::forward<Args>(args)...);
//							param.set_value();
//						}
//						else
//						{
//							param.set_value(std::invoke(func, std::forward<Args>(args)...));
//						}
//					}
//				}
//			}
//
//			template<typename T>
//			void set_exception(T&& e) { param.set_exception(std::forward<T>(e)); }
//		};
//
//		template<typename FN1, typename FN2>
//		struct continuation
//		{
//			template<typename T>
//			auto operator()(T&& p, details::scheduler* scheduler)
//			{
//				if constexpr (std::is_same<T, details::task_tag_type>::value)
//				{
//					// when chaining into a packaged task
//					if constexpr (details::is_continuation<FN2>::value)
//					{
//						return typename std::invoke_result<FN2, details::task_tag_type, details::scheduler*>::type{};
//					}
//					else
//					{
//						using func_t = details::function_traits<decltype(std::function{ std::declval<FN2>() }) > ;
//						return details::type_wrapper<typename func_t::result_type> {};
//					}
//				}
//				else
//				{
//					if constexpr (details::is_variant<T>::value)
//					{
//						if constexpr (details::is_continuation<FN1>::value)
//						{
//							std::visit([&](auto&& value)
//								{
//									std::invoke(first, details::promise_wrapper<decltype(value), FN2>{std::forward<decltype(value)>(value), second, scheduler}, scheduler);
//								}, p);
//						}
//						else
//						{
//							std::visit([&](auto&& value)
//								{
//									std::invoke(first, details::promise_wrapper<decltype(value), FN2>{std::forward<decltype(value)>(value), second, scheduler});
//								}, p);
//						}
//					}
//					else
//					{
//						if constexpr (details::is_continuation<FN1>::value)
//						{
//							std::invoke(first, details::promise_wrapper<T, FN2>{std::forward<T>(p), second, scheduler}, scheduler);
//						}
//						else
//						{
//							std::invoke(first, details::promise_wrapper<T, FN2>{std::forward<T>(p), second, scheduler});
//						}
//					}
//				}
//			}
//
//			auto unpack()
//			{
//				if constexpr (is_continuation<FN1>::value)
//				{
//					return then2_nowrap(first.unpack(), second);
//				}
//				else
//				{
//					return second;
//				}
//			}
//
//			template<typename Fn>
//			auto rebase(Fn&& fn)
//			{
//				if constexpr (is_continuation<FN1>::value)
//				{
//					return then(first.rebase(std::forward<Fn>(fn)), std::move(second));
//				}
//				else
//				{
//					return then(std::forward<Fn>(fn), std::move(second));
//				}
//			}
//
//			auto get_tail()
//			{
//				if constexpr (is_continuation<FN2>::value)
//				{
//					return second.get_tail();
//				}
//				else
//				{
//					return second;
//				}
//			}
//
//			FN1 first;
//			FN2 second;
//		};
//
//
//		template<typename T>
//		struct type_wrapper
//		{
//			using type = T;
//		};
//
//		template<typename T>
//		struct type_wrapper<type_wrapper<T>> : public type_wrapper<T>
//		{
//		};
//
//		template<typename T>
//		using promise_variant = std::variant<details::promise<T>, std::promise<T>>;
//
//		template<typename T>
//		struct is_variant : std::false_type {};
//
//
//		template<typename... Ts>
//		struct is_variant<std::variant<Ts...>> : std::true_type {};
//
//		/*template<typename Fn>
//		auto wrap(Fn&& fn)
//		{
//			if constexpr (std::is_invocable<Fn>::value)
//			{
//				return then([](auto p)
//					{
//						p.spawn();
//					}, std::forward<Fn>(fn));
//			}
//			else
//			{
//				return fn;
//			}
//		}*/
//
//
//		template<typename Fn>
//		auto wrap(Fn&& fn)
//		{
//			if constexpr (is_continuation<Fn>::value)
//			{
//				return std::forward<Fn>(fn);
//			}
//			else
//			{
//				auto lambda = [](auto p)
//				{
//					p.spawn();
//				};
//				return continuation<decltype(lambda), Fn>{std::move(lambda), std::forward<Fn>(fn) };
//			}
//		}
//
//		template<typename FN1, typename FN2>
//		auto wrap(continuation<FN1, FN2>&& fn)
//		{
//			return std::forward<continuation<FN1, FN2>>(fn);
//		}
//
//		template<typename... Fn>
//		auto wrap_all(Fn&& ... fn)
//		{
//			return std::make_tuple(wrap(std::forward<Fn>(fn))...);
//		}
//
//		template<typename Fn>
//		struct is_packaged_task : public std::is_invocable<Fn, details::task_tag_type, details::scheduler*>
//		{
//		};
//
//		template<typename T, typename SFINAE = void>
//		struct packaged_task_result
//		{
//			using type = typename std::invoke_result<decltype(details::wrap(std::declval<T>())), details::task_tag_type, details::scheduler*>::type::type;
//		};
//
//		template<typename T>
//		struct packaged_task_result<T, typename std::enable_if<!details::is_packaged_task<T>::value&& std::is_invocable<T, invocation>::value>::type>
//		{
//			using type = typename std::invoke_result<T, invocation>::type;
//		};
//	}
//	template<typename FN1, typename FN2>
//	auto then(FN1&& task1, FN2&& task2)
//	{
//		return details::continuation<decltype(details::wrap(std::forward<FN1>(task1))), FN2>{ details::wrap(std::forward<FN1>(task1)), std::forward<FN2>(task2) };
//	}
//
//	template<typename C1FN1, typename C1FN2, typename C2FN1, typename C2FN2>
//	auto then(details::continuation<C1FN1, C1FN2>& c1, details::continuation<C2FN1, C2FN2>& c2)
//	{
//		return c2.rebase(c1);
//	}
//
//	template<typename FN1, typename FN2>
//	auto then2_nowrap(FN1&& task1, FN2&& task2)
//	{
//		return details::continuation<decltype(std::forward<FN1>(task1)), FN2>{ std::forward<FN1>(task1), std::forward<FN2>(task2) };
//	}
//
//	//template<typename FN1, typename FN2>
//	//auto then(FN1&& task1, FN2&& task2)
//	//{
//	//	return[task1 = details::wrap(std::forward<FN1>(task1)), task2 = std::forward<FN2>(task2)](auto p, details::scheduler* scheduler)
//	//	{
//	//		using param_t = decltype(p);
//
//	//		constexpr bool is_packaged = details::is_packaged_task<FN2>::value;
//
//	//		if constexpr (std::is_same<param_t, details::task_tag_type>::value)
//	//		{
//	//			// when chaining into a packaged task
//	//			if constexpr (details::is_packaged_task<FN2>::value)
//	//			{
//	//				return typename std::invoke_result<FN2, details::task_tag_type, details::scheduler*>::type{};
//	//			}
//	//			else
//	//			{
//	//				using func_t = details::function_traits<decltype(std::function{ std::declval<FN2>() }) > ;
//	//				return details::type_wrapper<typename func_t::result_type> {};
//	//			}
//	//		}
//	//		else
//	//		{
//	//			if constexpr (details::is_variant<decltype(p)>::value)
//	//			{
//	//				if constexpr (details::is_packaged_task<decltype(task1)>::value)
//	//				{
//	//					std::visit([&](auto&& value)
//	//						{
//	//							std::invoke(task1, details::promise_wrapper<decltype(value), decltype(task2)>{std::forward<decltype(value)>(value), task2, scheduler}, scheduler);
//	//						}, p);
//	//				}
//	//				else
//	//				{
//	//					std::visit([&](auto&& value)
//	//						{
//	//							std::invoke(task1, details::promise_wrapper<decltype(value), decltype(task2)>{std::forward<decltype(value)>(value), task2, scheduler});
//	//						}, p);
//	//				}
//	//			}
//	//			else
//	//			{
//	//				if constexpr (details::is_packaged_task<decltype(task1)>::value)
//	//				{
//	//					std::invoke(task1, details::promise_wrapper<param_t, decltype(task2)>{std::forward<param_t>(p), task2, scheduler}, scheduler);
//	//				}
//	//				else
//	//				{
//	//					std::invoke(task1, details::promise_wrapper<param_t, decltype(task2)>{std::forward<param_t>(p), task2, scheduler});
//	//				}
//	//			}
//	//		}
//	//	};
//	//}
//
//	template<typename FN1, typename FN2, typename ... FNn>
//	auto then(FN1&& task1, FN2&& task2, FNn&& ... tasks)
//	{
//		return then(then(std::forward<FN1>(task1), std::forward<FN2>(task2)), std::forward<FNn>(tasks)...);
//	}
//
//
//	namespace details
//	{
//		template <typename... T, std::size_t... I>
//		auto subtuple_(std::tuple<T...>&& t, std::index_sequence<I...>) {
//			return std::make_tuple(std::get<I>(t)...);
//		}
//
//		template <int Trim, typename... T>
//		auto subtuple(const std::tuple<T...>& t) {
//			return subtuple_(t, std::make_index_sequence<sizeof...(T) - Trim>());
//		}
//	}
//
//	template<typename... FNn>
//	auto into(FNn&& ... tasks)
//	{
//		auto task2 = std::get<sizeof...(FNn) - 1 >(std::tie(tasks...));
//		auto lambda{ [tasks = details::subtuple_(std::forward_as_tuple(tasks...), std::make_index_sequence<sizeof...(FNn) - 1>())] (auto p)
//		{
//			std::apply([&](auto&& ... args)
//				{
//					p.set_value(compute<execution::wait>(args)...);
//				}, std::move(tasks));
//		} };
//
//		return details::continuation<decltype(lambda), decltype(task2)>{std::move(lambda), std::forward<decltype(task2)>(task2) };
//
//		/*
//		auto task2 = std::get<sizeof...(FNn) - 1 >(std::tie(tasks...));
//		return then([tasks = details::subtuple_(std::forward_as_tuple(tasks...), std::make_index_sequence<sizeof...(FNn) - 1>())](auto p)
//		{
//			std::apply([&](auto&& ... args)
//				{
//					p.set_value(compute<execution::wait>(args)...);
//				}, std::move(tasks));
//		}, std::forward<decltype(task2)>(task2));
//		*/
//	}
//
//
//	template<typename FN>
//	auto parallel_n(FN&& task, size_t count = 0)
//	{
//		using result_t = typename details::packaged_task_result<FN>::type;
//
//		return then([task = details::wrap(std::forward<FN>(task)), count]() {
//			std::vector<std::future<result_t>> futures;
//			for (auto i = 0u; i < count; ++i)
//			{
//				if constexpr (std::is_invocable<FN, invocation>::value && !details::is_packaged_task<FN>::value)
//				{
//					futures.emplace_back(std::move(compute<execution::async>(then([i, count]() { return invocation{ i, count }; }, task))));
//				}
//				else
//				{
//					futures.emplace_back(std::move(compute<execution::async>(task)));
//				}
//			}
//			return futures;
//		}, [](std::vector<std::future<result_t>> futures)
//		{
//			if constexpr (!std::is_same< result_t, void>::value)
//			{
//				std::vector<result_t> values;
//				values.reserve(futures.size());
//				for (auto&& value : futures)
//					values.emplace_back(std::move(value.get()));
//				return values;
//			}
//			else
//			{
//				for (auto&& value : futures)
//					value.get();
//			}
//		});
//	}
//
//	template<typename... FN>
//	auto parallel(FN&& ... tasks)
//	{
//		static_assert(sizeof...(FN) > 1);
//
//		using result_t = typename std::invoke_result<decltype(std::get<0>(std::declval<decltype(details::wrap_all(std::declval<FN>()...))>())), details::task_tag_type, details::scheduler*>::type::type;
//
//		return then([tasks = details::wrap_all(std::forward<FN>(tasks)...)]()
//		{
//			std::vector<std::future<result_t>> futures;
//			std::apply([&](auto&& ... task)
//				{
//					(futures.emplace_back(std::move(compute<execution::async>(task))), ...);
//				}, tasks);
//			return futures;
//		},
//			[](std::vector<std::future<result_t>> futures) {
//			if constexpr (!std::is_same< result_t, void>::value)
//			{
//				std::vector<result_t> values;
//				values.reserve(futures.size());
//				for (auto&& value : futures)
//					values.emplace_back(std::move(value.get()));
//				return values;
//			}
//			else
//			{
//				for (auto&& value : futures)
//					value.get();
//			}
//		});
//	}
//
//	template<execution ex = execution::async, typename Task, typename... Args>
//	auto compute(Task&& task, Args&& ... args)
//	{
//		if constexpr (!details::is_packaged_task<Task>::value)
//		{
//			return compute<ex>(details::wrap(std::forward<Task>(task)));
//		}
//		else
//		{
//			using T = typename std::invoke_result<Task, details::task_tag_type, details::scheduler*>::type::type;
//			details::state_object<T> state;
//			if constexpr (ex == execution::async)
//			{
//				return std::async(
//					[task = std::forward<Task>(task)]() mutable
//				{
//					if constexpr (!std::is_same<T, void>::value)
//						return compute<execution::wait>(std::forward<Task>(task));
//					else
//						compute<execution::wait>(std::forward<Task>(task));
//				}
//				);
//			}
//			else
//			{
//				details::scheduler scheduler{ execution::wait };
//
//				task(details::promise_variant<T>{details::promise<T>{&state}}, & scheduler, std::forward<Args>(args)...);
//
//				{
//					auto lock = std::unique_lock{ state.mutex };
//					state.condition_variable.wait(lock, [&state]()
//						{return state.data.index() != 0; });
//				}
//
//				if (state.data.index() == 1)
//					std::rethrow_exception(std::get<1>(state.data));
//
//				if constexpr (!std::is_same<T, void>::value)
//					return std::move(std::get<2>(state.data));
//			}
//		}
//	}
//}


namespace ASYNC2_NAMESPACE
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
		template <typename... T, std::size_t... I>
		auto subtuple_(std::tuple<T...>&& t, std::index_sequence<I...>) {
			return std::make_tuple(std::get<I>(t)...);
		}
		
		template <int Trim, typename... T>
		auto subtuple(const std::tuple<T...>& t) {
			return subtuple_(t, std::make_index_sequence<sizeof...(T) - Trim>());
		}

		template<typename T>
		struct is_tuple : std::false_type {};

		template<typename... Ts>
		struct is_tuple<std::tuple<Ts...>> : std::true_type {};

		template<typename T>
		struct type_wrapper { using type = T; };

		template<typename T>
		struct type_wrapper<type_wrapper<T>> : public type_wrapper<T> {};


		template<typename FN1, typename FN2>
		struct continuation;

		template<typename T>
		struct is_continuation : std::false_type {};
		template<typename FN1, typename FN2>
		struct is_continuation<continuation<FN1, FN2>> : std::true_type {};
		template<typename FN1, typename FN2>
		struct is_continuation<continuation<FN1, FN2>&> : std::true_type {};
		template<typename FN1, typename FN2>
		struct is_continuation<const continuation<FN1, FN2>&> : std::true_type {};
		template<typename FN1, typename FN2>

		struct is_continuation<const continuation<FN1, FN2>> : std::true_type {};


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



		template<typename T>
		struct continuation_result
		{
			using type = typename function_traits<decltype(std::function{ std::declval<T>() })>::result_type;
		};

		template<typename FN1, typename FN2>
		struct continuation_result<continuation<FN1, FN2>>	:	public continuation_result<FN2>
		{
		};

		template<typename Fn, typename... Args>
		struct invoke_result	: public std::invoke_result<Fn, Args...>
		{
		};

		template<typename Fn>
		struct invoke_result<Fn, void> : public std::invoke_result<Fn>
		{
		};
		template<typename Fn, typename... Args>
		struct invoke_result<Fn, std::tuple<Args...>> : public std::invoke_result<Fn, Args...>
		{
		};


		template<typename Fn, typename... Args>
		using invoke_result_t = typename invoke_result<Fn, Args...>::type;

		template<typename FN1, typename FN2>
		struct continuation
		{
			constexpr continuation(FN1&& fn1, FN2&& fn2) noexcept : first(std::forward<FN1>(fn1)), second(std::forward<FN2>(fn2)) {};

			template<typename T, typename... Args>
			auto operator()(T&& p, Args&&... args)
			{
				if constexpr (details::is_continuation<FN1>::value && details::is_continuation<FN2>::value)
				{
					using result_t = typename decltype(std::declval<FN1>().template get_type<Args...>())::type;
					if constexpr (std::is_same<result_t, void>::value)
					{
						compute<execution::wait>(first, std::forward<Args>(args)...);
						if constexpr (std::is_same<typename details::promise_type<T>::type, void>::value)
						{
							compute<execution::wait>(second);
							p.set_value();
						}
						else
						{
							p.set_value(compute<execution::wait>(second));
						}
					}
					else
					{
						p.set_value(compute<execution::wait>(second, compute<execution::wait>(first, std::forward<Args>(args)...)));
					}
				}
				else if constexpr (details::is_continuation<FN1>::value)
				{
					std::invoke(first, details::promise_wrapper<T, FN2>{std::forward<T>(p), second}, std::forward<Args>(args)...);
				}
				else
				{
					// start of the chain
					if constexpr(std::is_same<decltype(std::invoke(std::declval<FN1>(), std::forward<Args>(args)...)), void>::value)
					{
						std::invoke(first, std::forward<Args>(args)...);
						details::promise_wrapper<T, FN2>{std::forward<T>(p), second}.set_value();
					}
					else
					{
						details::promise_wrapper<T, FN2>{std::forward<T>(p), second}.set_value(std::invoke(first, std::forward<Args>(args)...));
					}
				}
			}

			template<typename... Args>
			auto get_type() const noexcept
			{
				if constexpr (is_continuation<FN1>::value && is_continuation<FN2>::value)
				{
					using result_t = typename decltype(first.template get_type<Args...>())::type;
					return type_wrapper<typename decltype(second.template get_type<result_t>())::type>{};
				}
				else if constexpr (is_continuation<FN1>::value)
				{
					using result_t = typename decltype(first.template get_type<Args...>())::type;
					return type_wrapper<invoke_result_t<FN2, result_t>>{};
				}
				else if constexpr (is_continuation<FN2>::value)
				{
					using result_t = invoke_result_t<FN1, Args...>;
					return type_wrapper<typename decltype(second.template get_type<result_t>())::type>{};
				}
				else
				{
					using result_t = invoke_result_t<FN1, Args...>;
					return type_wrapper<invoke_result_t<FN2, result_t>>{};
				}
			}

			FN1 first;
			FN2 second;
		};


		template<typename T>
		struct remove_cvref
		{
			using type = std::remove_reference_t<std::remove_cv_t<T>>;
		};

		template<typename T>
		using remove_cvref_t = typename remove_cvref<T>::type;

		template<typename T>
		struct promise_type { using type = T; };
		template<typename T>
		struct promise_type<std::promise<T>> { using type = T; };
		template<typename T>
		struct promise_type<details::promise<T>> { using type = T; };

		template<typename param_t, typename fn_t>
		struct promise_wrapper
		{
			param_t param;
			fn_t func;
			/*details::scheduler* scheduler;

			void spawn()
			{
				scheduler->start_work(*this);
			}*/

			template<typename... Args>
			auto set_value(Args&& ... args)
			{
				if constexpr (sizeof...(Args) == 1 && !std::is_invocable<fn_t, Args...>::value)
				{
					// allow for empty continuations
					if constexpr (std::is_same<decltype(std::apply(func, std::forward<Args>(args)...)), void>::value)
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
					// allow for empty continuations
					if constexpr (std::is_same<invoke_result_t<fn_t, Args...>, void>::value)
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

			template<typename T>
			void set_exception(T&& e) { param.set_exception(std::forward<T>(e)); }
		};
		template<typename param_t, typename fn_t>
		struct promise_type<promise_wrapper <param_t, fn_t>> : public promise_type< param_t> {};
	}


	template<typename FN1, typename FN2>
	auto then(FN1&& fn1, FN2&& fn2)
	{
		return details::continuation<FN1, FN2>{std::forward<FN1>(fn1), std::forward<FN2>(fn2)};
	}

	template<typename FN1, typename FN2, typename ... FNn>
	auto then(FN1&& fn1, FN2&& fn2, FNn&& ... fnn)
	{
		return then(then(std::forward<FN1>(fn1), std::forward<FN2>(fn2)), std::forward<FNn>(fnn)...);
	}


	template<typename... FNn>
	auto into(FNn&& ... tasks)
	{
		auto task2 = std::get<sizeof...(FNn) - 1 >(std::tie(tasks...));
		auto lambda{ [tasks = details::subtuple_(std::forward_as_tuple(tasks...), std::make_index_sequence<sizeof...(FNn) - 1>())] () mutable
		{
			return std::apply([&](auto&& ... args) mutable
				{
					// todo this can run deferred
					return std::make_tuple(compute<execution::wait>(args)...);
				}, std::move(tasks));
		} };
	
		return details::continuation<decltype(lambda), decltype(task2)>{std::move(lambda), std::forward<decltype(task2)>(task2) };
	}

	namespace details
	{
		template<typename T, typename... Args>
		struct get_invocation_type
		{
			using type = std::invoke_result_t<T, Args...>;
		};

		template<typename FN1, typename FN2, typename... Args>
		struct get_invocation_type<continuation<FN1, FN2>, Args...>
		{
			using type = typename decltype(std::declval<continuation<FN1, FN2>>().template get_type<Args...>())::type;
		};
	}

	template<typename... FN>
	auto parallel(FN&& ... tasks)
	{
		static_assert(sizeof...(FN) > 1);

		using result_t = typename details::get_invocation_type<std::tuple_element_t<0, std::tuple<FN...>>>::type;

		return then([tasks = std::make_tuple(std::forward<FN>(tasks)...)]() mutable
		{
			return std::move(std::apply([](auto&& ... task) mutable
				{
					std::vector<std::future<result_t>> values;
					values.reserve(sizeof...(task));
					( values.emplace_back( compute<execution::async>(task)), ... );
					return std::move(values);
				}, std::forward<decltype(tasks)>(tasks)));
		},
			[](auto futures) mutable {
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

	template<typename T>
	struct parallel_n_invocation
	{
		static const bool supports_invocation = std::is_invocable<T, invocation>::value;
		static const bool is_continuation = false;
	};

	template<typename FN1, typename FN2>
	struct parallel_n_invocation<details::continuation<FN1, FN2>>
	{
		static const bool supports_invocation = std::is_invocable<details::promise<typename decltype(std::declval<details::continuation<FN1, FN2>>().template get_type<invocation>())::type>, invocation>::value;
		static const bool is_continuation = true;
	};

	template<typename R, typename FN>
	auto parallel_n_impl(FN&& task, size_t count)
	{
		using result_t = R;

		return then([task = std::forward<FN>(task), count]() mutable
		{
			std::vector<std::future<result_t>> futures;
			/*for (auto i = 0u; i < count; ++i)
			{
				if constexpr (!details::is_continuation<FN>::value)
				{
					if constexpr (parallel_n_invocation<FN>::supports_invocation)
					{
						futures.emplace_back(std::move(compute<execution::async>(task, invocation{ i, count })));
					}
					else
					{
						futures.emplace_back(std::move(compute<execution::async>(task)));
					}
				}
				else
				{
					if constexpr (parallel_n_invocation<FN>::supports_invocation)
					{
						futures.emplace_back(std::move(compute<execution::async>(task, invocation{ i, count })));
					}
					else
					{
						futures.emplace_back(std::move(compute<execution::async>(task)));
					}
				}
			}*/
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

	template<typename FN>
	auto parallel_n(FN&& task, size_t count)
	{
		constexpr bool continuation_pack = details::is_continuation<FN>::value;
		if constexpr (parallel_n_invocation<FN>::supports_invocation)
		{
			return parallel_n_impl<typename details::get_invocation_type<FN, invocation>::type>(std::forward<FN>(task), count);
		}
		else
		{
			return parallel_n_impl<typename details::get_invocation_type<FN>::type>(std::forward<FN>(task), count);
		}
		
	}

	template<execution ex = execution::wait, typename Task, typename... Args>
	auto compute(Task&& task, Args&& ... args)
	{
		if constexpr (!details::is_continuation<Task>::value)
		{
			using T = details::invoke_result_t<Task, Args...>;
			if constexpr (ex == execution::async)
			{
				return std::async(
					[task = std::forward<Task>(task)](auto... params) mutable
				{
					if constexpr (!std::is_same<T, void>::value)
						return compute<execution::wait>(std::forward<Task>(task), std::forward<decltype(params)>(params)...);
					else
						compute<execution::wait>(std::forward<Task>(task), std::forward<decltype(params)>(params)...);
				}, std::forward<Args>(args)...
					);
			}
			else
			{
				if constexpr (std::is_same_v<T, void>)
					task(std::forward<Args>(args)...);
				else
					return task(std::forward<Args>(args)...);
			}
		}
		else
		{
			using T = typename decltype(std::declval<Task>().template get_type<Args...>())::type;

			details::state_object<T> state;
			if constexpr (ex == execution::async)
			{
				return std::async(
					[task = std::forward<Task>(task)](auto... params) mutable
				{
					if constexpr (!std::is_same<T, void>::value)
						return compute<execution::wait>(std::forward<Task>(task), std::forward<decltype(params)>(params)...);
					else
						compute<execution::wait>(std::forward<Task>(task), std::forward<decltype(params)>(params)...);
				}, std::forward<Args>(args)...
					);
			}
			else
			{
				task(details::promise<T>{&state}, std::forward<Args>(args)...);

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