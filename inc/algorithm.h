#pragma once

#ifndef ASYNC_NAMESPACE
#define ASYNC_NAMESPACE async
#endif

#include <variant>
#include <mutex>
#include <future>
#include <functional> // std::invoke && std::apply
#ifndef ASYNC_MINIMAL_INCLUDE
#include <vector>
#include <array>
#endif
namespace ASYNC_NAMESPACE
{
	enum class execution
	{
		async = 0,
		wait  = 1
	};

	template <execution ex = execution::wait, typename Task, typename... Args>
	auto compute(Task&& task, Args&&... args);

	struct invocation
	{
		size_t index{0u};
		size_t count{0u};
	};
	namespace details
	{
		template <typename T>
		struct remove_cvref
		{
			using type = std::remove_reference_t<std::remove_cv_t<T>>;
		};

		template <typename T>
		using remove_cvref_t = typename remove_cvref<T>::type;

		// scheduler like object
		class scheduler
		{
		  public:
			template <typename F>
			void do_work(F&& f)
			{}

		  private:
		};

		template <typename... T, std::size_t... I>
		auto subtuple_(std::tuple<T...>&& t, std::index_sequence<I...>)
		{
			return std::make_tuple(std::get<I>(t)...);
		}

		template <std::size_t O, std::size_t... Is>
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
			if constexpr(sizeof...(T) <= (Offset))
				return std::tuple<>{};
			else
				return subtuple_(std::forward<decltype(t)>(t),
								 make_index_sequence<sizeof...(T) - (Length + Offset), Offset>());
		}

		template <unsigned... s, typename Tuple>
		auto extract_tuple(std::index_sequence<s...>, Tuple& tup)
		{
			return std::make_tuple(std::get<s>(tup)...);
		}

		template <typename T>
		struct is_tuple : std::false_type
		{};

		template <typename... Ts>
		struct is_tuple<std::tuple<Ts...>> : std::true_type
		{};

		template <typename T>
		struct is_variant : std::false_type
		{};

		template <typename... Ts>
		struct is_variant<std::variant<Ts...>> : std::true_type
		{};

		template <typename T>
		struct type_wrapper
		{
			using type = T;
		};

		template <typename T>
		struct type_wrapper<type_wrapper<T>> : public type_wrapper<T>
		{};


		template <typename FN1, typename FN2>
		struct continuation;

		template <typename T>
		struct is_continuation : std::false_type
		{};
		template <typename FN1, typename FN2>
		struct is_continuation<continuation<FN1, FN2>> : std::true_type
		{};
		template <typename FN1, typename FN2>
		struct is_continuation<continuation<FN1, FN2>&> : std::true_type
		{};
		template <typename FN1, typename FN2>
		struct is_continuation<const continuation<FN1, FN2>&> : std::true_type
		{};
		template <typename FN1, typename FN2>

		struct is_continuation<const continuation<FN1, FN2>> : std::true_type
		{};


		template <typename T>
		struct function_traits;

		template <typename R, typename... Args>
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

		template <typename T>
		struct state_object
		{
			std::mutex mutex;
			std::condition_variable condition_variable;
			std::variant<std::monostate, std::exception_ptr, T> data;
		};

		template <>
		struct state_object<void>
		{
			std::mutex mutex;
			std::condition_variable condition_variable;
			std::variant<std::monostate, std::exception_ptr, int> data;
		};

		template <typename T>
		struct promise
		{
			state_object<T>* state;

			template <int I, typename... Args>
			void set(Args&&... args)
			{
				auto lock = std::unique_lock(state->mutex);
				if constexpr(std::is_same<T, void>::value && I == 2)
				{
					state->data.template emplace<I>(1);
				}
				else
				{
					state->data.template emplace<I>(std::forward<Args>(args)...);
				}
				state->condition_variable.notify_one();
			}

			template <typename... Args>
			void set_value(Args&&... args)
			{
				set<2>(std::forward<Args>(args)...);
			}
			template <typename Y>
			void set_exception(Y&& e)
			{
				set<1>(std::forward<Y>(e));
			}
		};

		template <typename T>
		struct promise_type
		{
			using type = T;
		};
		template <typename T>
		struct promise_type<std::promise<T>>
		{
			using type = T;
		};
		template <typename T>
		struct promise_type<details::promise<T>>
		{
			using type = T;
		};

		template <typename T>
		struct continuation_result
		{
			using type = typename function_traits<decltype(std::function{std::declval<T>()})>::result_type;
		};

		template <typename FN1, typename FN2>
		struct continuation_result<continuation<FN1, FN2>> : public continuation_result<FN2>
		{};

		template <typename Fn, typename... Args>
		struct invoke_result : public std::invoke_result<Fn, Args...>
		{};

		template <typename Fn>
		struct invoke_result<Fn, void> : public std::invoke_result<Fn>
		{};
		template <typename Fn, typename... Args>
		struct invoke_result<Fn, std::tuple<Args...>> : public std::invoke_result<Fn, Args...>
		{};

		template <typename Fn, typename... Args>
		using invoke_result_t = typename invoke_result<Fn, Args...>::type;


		template <typename param_t, typename fn_t>
		struct promise_wrapper
		{
			param_t param;
			fn_t func;
			/*details::scheduler* scheduler;

			void spawn()
			{
				scheduler->start_work(*this);
			}*/

			template <typename... Args>
			auto set_value(Args&&... args)
			{
				if constexpr(sizeof...(Args) == 1 && !std::is_invocable<fn_t, Args...>::value)
				{
					// allow for empty continuations
					if constexpr(std::is_same<decltype(std::apply(func, std::forward<Args>(args)...)), void>::value)
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
					if constexpr(std::is_same<invoke_result_t<fn_t, Args...>, void>::value)
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

			template <typename T>
			void set_exception(T&& e)
			{
				param.set_exception(std::forward<T>(e));
			}
		};
		template <typename param_t, typename fn_t>
		struct promise_type<promise_wrapper<param_t, fn_t>> : public promise_type<param_t>
		{};


		template <typename FN1, typename FN2>
		struct continuation
		{
			constexpr continuation(FN1&& fn1, FN2&& fn2) noexcept
				: first(std::forward<FN1>(fn1)), second(std::forward<FN2>(fn2)){};

			template <typename T, typename... Args>
			auto operator()(T&& p, Args&&... args)
			{
				if constexpr(details::is_continuation<FN1>::value && details::is_continuation<FN2>::value)
				{
					using result_t = typename decltype(std::declval<FN1>().template get_type<Args...>())::type;
					if constexpr(std::is_same<result_t, void>::value)
					{
						compute<execution::wait>(first, std::forward<Args>(args)...);
						if constexpr(std::is_same<typename details::promise_type<T>::type, void>::value)
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
						p.set_value(compute<execution::wait>(
							second, compute<execution::wait>(first, std::forward<Args>(args)...)));
					}
				}
				else if constexpr(details::is_continuation<FN1>::value)
				{
					std::invoke(first, details::promise_wrapper<T, FN2>{std::forward<T>(p), second},
								std::forward<Args>(args)...);
				}
				else
				{
					// start of the chain
					if constexpr(std::is_same<decltype(std::invoke(std::declval<FN1>(), std::forward<Args>(args)...)),
											  void>::value)
					{
						std::invoke(first, std::forward<Args>(args)...);
						details::promise_wrapper<T, FN2>{std::forward<T>(p), second}.set_value();
					}
					else
					{
						details::promise_wrapper<T, FN2>{std::forward<T>(p), second}.set_value(
							std::invoke(first, std::forward<Args>(args)...));
					}
				}
			}

			template <typename... Args>
			auto get_type() const noexcept
			{
				if constexpr(is_continuation<FN1>::value && is_continuation<FN2>::value)
				{
					using result_t = typename decltype(first.template get_type<Args...>())::type;
					return type_wrapper<typename decltype(second.template get_type<result_t>())::type>{};
				}
				else if constexpr(is_continuation<FN1>::value)
				{
					using result_t = typename decltype(first.template get_type<Args...>())::type;
					return type_wrapper<invoke_result_t<FN2, result_t>>{};
				}
				else if constexpr(is_continuation<FN2>::value)
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


		template <typename T, typename... Args>
		struct get_invocation_type
		{
			using type = std::invoke_result_t<T, Args...>;
		};

		template <typename FN1, typename FN2, typename... Args>
		struct get_invocation_type<continuation<FN1, FN2>, Args...>
		{
			using type = typename decltype(std::declval<continuation<FN1, FN2>>().template get_type<Args...>())::type;
		};

		template <typename T, typename... Args>
		struct is_invocable : std::is_invocable<T, Args...>
		{};

		template <typename FN1, typename FN2, typename... Args>
		struct is_invocable<continuation<FN1, FN2>, Args...> : std::is_invocable<FN2, Args...>
		{};


		template <typename FN, typename SFINAE = void>
		struct parallel_invocation_result_type
		{
			using type = typename get_invocation_type<FN>::type;
		};

		template <typename FN>
		struct parallel_invocation_result_type<FN,
											   typename std::enable_if<std::is_invocable<FN, invocation>::value>::type>
		{
			using type = typename get_invocation_type<FN, invocation>::type;
		};
	}

	template <typename FN1, typename FN2>
	auto then(FN1&& fn1, FN2&& fn2)
	{
		return details::continuation<FN1, FN2>{std::forward<FN1>(fn1), std::forward<FN2>(fn2)};
	}

	template <typename FN1, typename FN2, typename... FNn>
	auto then(FN1&& fn1, FN2&& fn2, FNn&&... fnn)
	{
		return then(then(std::forward<FN1>(fn1), std::forward<FN2>(fn2)), std::forward<FNn>(fnn)...);
	}


	/// \brief Disconnects computation results from previous results
	///
	/// \details When you don't need, or care, for the results of the previous computation then using this function
	/// will wrap your given invocable into a wrapper that will throw away the results of the previous computation.
	template <typename FN1>
	auto disconnect(FN1&& fn1)
	{
// workaround for [[maybe_unused]] not working in current version of MSVC
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4100)
		return then([](auto... v) -> void {}, std::forward<FN1>(fn1));
#pragma warning(pop)
#else
		return then([]([[maybe_unused]] auto... v) -> void {}, std::forward<FN1>(fn1));
#endif
	}

	namespace details
	{
		template <typename FN1, typename FN2, typename T, typename... Ts, size_t... S1, size_t... S2>
		auto then_or(FN1&& fn1, FN2&& fn2, std::tuple<T, Ts...>&& result, std::index_sequence<S1...>,
					 std::index_sequence<S2...>)
		{
			if(std::get<0>(result))
			{
				if constexpr(details::is_invocable<FN1, T, Ts...>::value)
				{
					return compute<execution::wait>(
						std::forward<FN1>(fn1),
						std::forward<typename std::tuple_element<S1, std::tuple<T, Ts...>>::type>(
							std::get<S1>(result))...);
				}
				else
				{
					return compute<execution::wait>(
						std::forward<FN1>(fn1),
						std::forward<typename std::tuple_element<S2, std::tuple<T, Ts...>>::type>(
							std::get<S2>(result))...);
				}
			}
			else
			{
				if constexpr(details::is_invocable<FN2, T, Ts...>::value)
				{
					return compute<execution::wait>(
						std::forward<FN2>(fn2),
						std::forward<typename std::tuple_element<S1, std::tuple<T, Ts...>>::type>(
							std::get<S1>(result))...);
				}
				else
				{
					return compute<execution::wait>(
						std::forward<FN2>(fn2),
						std::forward<typename std::tuple_element<S2, std::tuple<T, Ts...>>::type>(
							std::get<S2>(result))...);
				}
			}
		}

		template <typename FN1, typename FN2, typename... Ts>
		auto then_or(FN1&& fn1, FN2&& fn2, std::tuple<Ts...>&& result)
		{
			return then_or(std::forward<FN1>(fn1), std::forward<FN2>(fn2), std::forward<std::tuple<Ts...>>(result),
						   details::make_index_sequence<sizeof...(Ts), 0>(),
						   details::make_index_sequence<sizeof...(Ts) - 1, 1>());
		}

		template <typename FN1, typename FN2, typename T, typename Y>
		auto then_or(FN1&& fn1, FN2&& fn2, std::variant<T, Y>&& value)
		{
			if(value.index() == 0)
			{
				return compute<execution::wait>(std::forward<FN1>(fn1),
												std::forward<decltype(std::get<0>(value))>(std::get<0>(value)));
			}
			else
			{
				return compute<execution::wait>(std::forward<FN2>(fn2),
												std::forward<decltype(std::get<1>(value))>(std::get<1>(value)));
			}
		}
		template <typename FN1, typename FN2, typename T, typename... Ts>
		auto then_or(FN1&& fn1, FN2&& fn2, T&& value, Ts&&... result)
		{
			if(value)
			{
				if constexpr(details::is_invocable<FN1, T, Ts...>::value)
				{
					return compute<execution::wait>(std::forward<FN1>(fn1), std::forward<T>(value),
													std::forward<Ts>(result)...);
				}
				else
				{
					return compute<execution::wait>(std::forward<FN1>(fn1), std::forward<Ts>(result)...);
				}
			}
			else
			{
				if constexpr(details::is_invocable<FN2, T, Ts...>::value)
				{
					return compute<execution::wait>(std::forward<FN2>(fn2), std::forward<T>(value),
													std::forward<Ts>(result)...);
				}
				else
				{
					return compute<execution::wait>(std::forward<FN2>(fn2), std::forward<Ts>(result)...);
				}
			}
		}
	}

	/// \brief Complexer then statement that allows a forked behaviour based on the first result.
	///
	/// \details A continuation that will look at the result of the first invocable (FN1), and will then call either
	/// FN2 (true) or FN3 (false).
	/// FN1 can return a tuple to pass arguments forward into the following tasks, as long as the first element of the
	/// tuple is a bool indicating success or fail.
	template <typename FN1, typename FN2, typename FN3>
	auto then_or(FN1&& fn1, FN2&& fn2, FN3&& fn3)
	{
		return then(std::forward<FN1>(fn1),
					[fn2 = std::forward<FN2>(fn2), fn3 = std::forward<FN3>(fn3)](auto... results) mutable {
						return details::then_or(std::forward<FN2>(fn2), std::forward<FN3>(fn3),
												std::forward<decltype(results)>(results)...);
					});
	}

	/// \brief Complexer then statement that allows a forked behaviour based on the first result.
	///
	/// \details A continuation that will look at the result of the first invocable (FN1), and will then call either
	/// FN2 (true) or FN3 (false) based on the result of the predicate.
	/// FN1 can return a tuple to pass arguments forward into the following tasks, as long as the first element of the
	/// tuple is a bool indicating success or fail.
	/// The Pred can return a tuple to pass arguments forward into the following tasks, as long as the first element of
	/// the tuple is a bool indicating success or fail
	template <typename FN1, typename FN2, typename FN3, typename Pred>
	auto then_or(FN1&& fn1, FN2&& fn2, FN3&& fn3, Pred&& pred)
	{
		using R = typename details::get_invocation_type<Pred>::type;
		static_assert(std::is_same<R, bool>::value || std::is_trivially_constructible<bool, R>::value);
		return then(std::forward<FN1>(fn1),
					then_or(std::forward<Pred>(pred), std::forward<FN2>(fn2), std::forward<FN3>(fn3)));
	}

	namespace details
	{
		template <typename T, typename... FN>
		struct all_return
			: std::bool_constant<(std::is_same<T, typename details::get_invocation_type<FN>::type>{} && ...)>
		{};

		// machinery to make an index sequence of a tuple that filters out the given type (match), or that makes a tuple
		// with only the given type (mismatch)
		template <size_t... first, size_t... second>
		constexpr std::index_sequence<first..., second...> append(std::index_sequence<first...>,
																  std::index_sequence<second...>)
		{
			return {};
		}

		template <typename P, size_t Current, typename T, typename Tu, typename SFINEA = void>
		struct match
		{
			using type = P;
		};

		template <size_t... S, size_t Current, typename T, typename Y, typename... Args>
		struct match<std::index_sequence<S...>, Current, T, std::tuple<Y, Args...>,
					 std::enable_if_t<!std::is_same<T, Y>::value>>
			: match<decltype(append(std::index_sequence<S...>{}, std::index_sequence<Current>{})), Current + 1, T,
					std::tuple<Args...>>
		{};
		template <size_t... S, size_t Current, typename T, typename Y, typename... Args>
		struct match<std::index_sequence<S...>, Current, T, std::tuple<Y, Args...>,
					 std::enable_if_t<std::is_same<T, Y>::value>>
			: match<std::index_sequence<S...>, Current + 1, T, std::tuple<Args...>>
		{};

		template <typename P, size_t Current, typename T, typename Tu, typename SFINEA = void>
		struct mismatch
		{
			using type = P;
		};

		template <size_t... S, size_t Current, typename T, typename Y, typename... Args>
		struct mismatch<std::index_sequence<S...>, Current, T, std::tuple<Y, Args...>,
						std::enable_if_t<std::is_same<T, Y>::value>>
			: mismatch<decltype(append(std::index_sequence<S...>{}, std::index_sequence<Current>{})), Current + 1, T,
					   std::tuple<Args...>>
		{};
		template <size_t... S, size_t Current, typename T, typename Y, typename... Args>
		struct mismatch<std::index_sequence<S...>, Current, T, std::tuple<Y, Args...>,
						std::enable_if_t<!std::is_same<T, Y>::value>>
			: mismatch<std::index_sequence<S...>, Current + 1, T, std::tuple<Args...>>
		{};

		template <typename T, typename U>
		struct make_index_sequence_match
		{};

		template <typename T, typename... Args>
		struct make_index_sequence_match<T, std::tuple<Args...>>
			: match<std::index_sequence<>, 0, T, std::tuple<Args...>>
		{};

		template <typename T, typename U>
		struct make_index_sequence_mismatch
		{};

		template <typename T, typename... Args>
		struct make_index_sequence_mismatch<T, std::tuple<Args...>>
			: mismatch<std::index_sequence<>, 0, T, std::tuple<Args...>>
		{};

		template <size_t... S1, size_t... S2, typename F>
		auto get(std::index_sequence<S1...>, std::index_sequence<S2...>, F&& f)
		{
			(void(std::get<S1>(f).get()), ...);
			return std::tuple(std::move(std::get<S2>(f).get())...);
		}
	}

	namespace details
	{
		template <typename T, typename F, size_t... S>
		auto parallel_helper(F&& f, std::index_sequence<S...>)
		{
			return std::array<T, sizeof...(S)>{std::move(f[S].get())...};
		}
		template <typename... FN>
		auto parallel(std::tuple<FN...>&& tasks)
		{
			static_assert(sizeof...(FN) > 1);

			using result_t = typename details::get_invocation_type<std::tuple_element_t<0, std::tuple<FN...>>>::type;
			if constexpr(details::all_return<result_t, FN...>::value)
			{
				return [tasks = std::forward<std::tuple<FN...>>(tasks)]() mutable ->
					   typename std::conditional<std::is_same<result_t, void>::value, void,
												 std::array<result_t, sizeof...(FN)>>::type
				{
					auto futures = std::apply(
						[](auto&&... task) mutable {
							return std::array<std::future<result_t>, sizeof...(task)>{
								compute<execution::async>(task)...};
						},
						std::forward<decltype(tasks)>(tasks));

					if constexpr(!std::is_same<result_t, void>::value)
					{
						return parallel_helper<result_t>(std::move(futures), std::index_sequence_for<FN...>{});
					}
					else
					{
						for(auto&& value : futures) value.get();
					}
				};
			}
			else
			{
				return [tasks = std::forward<std::tuple<FN...>>(tasks)]() mutable {
					auto futures = std::move(std::apply(
						[](auto&&... task) mutable { return std::make_tuple(compute<execution::async>(task)...); },
						std::forward<decltype(tasks)>(tasks)));

					using void_types =
						typename details::make_index_sequence_mismatch<std::future<void>, decltype(futures)>::type;
					using real_types =
						typename details::make_index_sequence_match<std::future<void>, decltype(futures)>::type;

					return details::get(void_types{}, real_types{}, futures);
				};
			}
		}
	}

	template <typename... FN>
	auto parallel(FN&&... tasks)
	{
		return details::parallel(std::forward_as_tuple(tasks...));
	}

	template <typename FN>
	auto parallel_n(FN&& task, size_t count)
	{
		using result_t = typename details::parallel_invocation_result_type<FN>::type;
		return [task = std::forward<FN>(task), count]() mutable ->
			   typename std::conditional<std::is_same<result_t, void>::value, void, std::vector<result_t>>::type
		{
			constexpr bool supports_invocation = std::is_invocable<FN, invocation>::value;
			std::vector<std::future<result_t>> futures;
			for(auto i = 0u; i < count; ++i)
			{
				if constexpr(supports_invocation)
				{
					futures.emplace_back(std::move(compute<execution::async>(task, invocation{i, count})));
				}
				else
				{
					futures.emplace_back(std::move(compute<execution::async>(task)));
				}
			}

			if constexpr(!std::is_same<result_t, void>::value)
			{
				std::vector<result_t> values;
				values.reserve(futures.size());
				for(auto&& value : futures) values.emplace_back(std::move(value.get()));
				return values;
			}
			else
			{
				for(auto&& value : futures) value.get();
			}
		};
	}

	template <size_t S, typename FN>
	auto parallel_n(FN&& task)
	{
		using result_t = typename details::parallel_invocation_result_type<FN>::type;
		return [task = std::forward<FN>(task)]() mutable ->
			   typename std::conditional<std::is_same<result_t, void>::value, void, std::array<result_t, S>>::type
		{
			constexpr bool supports_invocation = std::is_invocable<FN, invocation>::value;
			std::array<std::future<result_t>, S> futures;
			auto current_future = std::begin(futures);
			for(auto i = 0u; i < S; ++i)
			{
				if constexpr(supports_invocation)
				{
					*current_future = std::move(compute<execution::async>(task, invocation{i, S}));
				}
				else
				{
					*current_future = std::move(compute<execution::async>(task));
				}
				++current_future;
			}

			if constexpr(!std::is_same<result_t, void>::value)
			{
				return details::parallel_helper<result_t>(std::move(futures), std::make_index_sequence<S>{});
			}
			else
			{
				for(auto&& value : futures) value.get();
			}
		};
	}

	namespace details
	{
		template <size_t... S, typename T>
		auto into(std::index_sequence<S...>, T&& t)
		{
			return then(
				details::parallel(std::forward_as_tuple(std::get<S>(t)...)),
				std::forward<decltype(std::get<std::tuple_size_v<T> - 1>(t))>(std::get<std::tuple_size_v<T> - 1>(t)));
		}

		template <typename T>
		auto into(T&& t)
		{
			return into(std::make_index_sequence<std::tuple_size_v<T> - 1>{}, std::forward<T>(t));
		}
	}

	/// \brief Aggregates the results of the preceding tasks.
	///
	/// \details Allows aggregegating the results of the previous tasks into a final task. Each of the preceding tasks
	/// are considered a bifurcation, and will be (potentially) ran concurrently.
	template <typename... FNn>
	auto into(FNn&&... tasks)
	{
		return details::into(std::forward_as_tuple(tasks...));
	}

	template <execution ex, typename Task, typename... Args>
	auto compute(Task&& task, Args&&... args)
	{
		if constexpr(!details::is_continuation<Task>::value)
		{
			using T = details::invoke_result_t<Task, Args...>;
			if constexpr(ex == execution::async)
			{
				return std::async(
					[task = std::forward<Task>(task)](auto... params) mutable {
						if constexpr(!std::is_same<T, void>::value)
							return compute<execution::wait>(std::forward<Task>(task),
															std::forward<decltype(params)>(params)...);
						else
							compute<execution::wait>(std::forward<Task>(task),
													 std::forward<decltype(params)>(params)...);
					},
					std::forward<Args>(args)...);
			}
			else
			{
				if constexpr(std::is_same_v<T, void>)
					task(std::forward<Args>(args)...);
				else
					return task(std::forward<Args>(args)...);
			}
		}
		else
		{
			using T = typename decltype(std::declval<Task>().template get_type<Args...>())::type;

			details::state_object<T> state;
			if constexpr(ex == execution::async)
			{
				return std::async(
					[task = std::forward<Task>(task)](auto... params) mutable {
						if constexpr(!std::is_same<T, void>::value)
							return compute<execution::wait>(std::forward<Task>(task),
															std::forward<decltype(params)>(params)...);
						else
							compute<execution::wait>(std::forward<Task>(task),
													 std::forward<decltype(params)>(params)...);
					},
					std::forward<Args>(args)...);
			}
			else
			{
				task(details::promise<T>{&state}, std::forward<Args>(args)...);

				{
					auto lock = std::unique_lock{state.mutex};
					state.condition_variable.wait(lock, [&state]() { return state.data.index() != 0; });
				}

				if(state.data.index() == 1) std::rethrow_exception(std::get<1>(state.data));

				if constexpr(!std::is_same<T, void>::value) return std::move(std::get<2>(state.data));
			}
		}
	}
}