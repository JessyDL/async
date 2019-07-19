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
		template <typename... T, std::size_t... I>
		auto subtuple_(std::tuple<T...>&& t, std::index_sequence<I...>) {
			return std::make_tuple(std::get<I>(t)...);
		}

		template <std::size_t O, std::size_t ... Is>
		std::index_sequence<(O + Is)...> add_offset(std::index_sequence<Is...>)
		{
			return {};
		}

		template <std::size_t N, std::size_t O = 0>
		auto make_index_sequence()
		{
			return add_offset<O>(std::make_index_sequence<N>{});
		}

		template <std::size_t Length, std::size_t Offset, typename... T>
		auto subtuple(std::tuple<T...>&& t) 
		{
			static_assert(Length + Offset <= sizeof...(T));
			if constexpr (sizeof...(T) <= (Offset))
				return std::tuple<>{};
			else
				return subtuple_(std::forward<decltype(t)>(t), make_index_sequence<sizeof...(T) - (Length + Offset), Offset>());
		}

		template<unsigned...s> struct seq { typedef seq<s...> type; };
		template<unsigned max, unsigned... s> struct make_seq :make_seq<max - 1, max - 1, s...> {};
		template<unsigned...s> struct make_seq<0, s...> :seq<s...> {};

		template<unsigned... s, typename Tuple>
		auto extract_tuple(seq<s...>, Tuple& tup) {
			return std::make_tuple(std::get<s>(tup)...);
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

	template<typename FN1>
	auto disconnect(FN1&& fn1)
	{
		return then([](auto... v) -> void {}, std::forward<FN1>(fn1));
	}


	template<typename FN1, typename FN2, typename FN3>
	auto then_or(FN1&& fn1, FN2&& fn2, FN3&& fn3)
	{
		return then(std::forward<FN1>(fn1), [fn2 = std::forward<FN2>(fn2), fn3 = std::forward<FN3>(fn3)](bool success, auto... values)
		{
			if (success)
				return std::invoke(fn2, std::forward<decltype(values)>(values)...);
			else
				return std::invoke(fn3, std::forward<decltype(values)>(values)...);			
		});
	}

	template<typename FN1, typename FN2, typename FN3, typename Pred>
	auto then_or(FN1&& fn1, FN2&& fn2, FN3&& fn3, Pred&& pred)
	{
		using R = typename details::get_invocation_type<pred>::type;
		static_assert(std::is_same<R, bool>::value || std::is_trivially_constructible<bool, R>::value);
		return then(std::forward<FN1>(fn1), then_or(std::forward<Pred>(pred), std::forward<FN2>(fn2), std::forward<FN3>(fn3)));
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

		return[tasks = std::make_tuple(std::forward<FN>(tasks)...)]() mutable -> typename std::conditional<std::is_same<result_t, void>::value, void, std::vector<result_t>>::type
		{
			auto futures = std::apply([](auto&& ... task) mutable
				{
					std::vector<std::future<result_t>> values;
					values.reserve(sizeof...(task));
					(values.emplace_back(compute<execution::async>(task)), ...);
					return std::move(values);
				}, std::forward<decltype(tasks)>(tasks));

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
		};
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

		return[task = std::forward<FN>(task), count]() mutable -> typename std::conditional<std::is_same<result_t, void>::value, void, std::vector<result_t>>::type
		{
			std::vector<std::future<result_t>> futures;
			for (auto i = 0u; i < count; ++i)
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
			}

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
		};
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