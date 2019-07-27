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

// forward declarations
namespace ASYNC_NAMESPACE
{
	enum class execution
	{
		async = 0,
		wait  = 1
	};

	struct invocation
	{
		size_t index{0u};
		size_t count{0u};
	};

	template <execution ex = execution::wait, typename Task, typename... Args>
	auto compute(Task&& task, Args&&... args);

	namespace details
	{
		class scheduler;

		template <typename T, typename... Args>
		struct task_result_type;

		template <typename FN, typename SFINAE, typename... Args>
		struct parallel_invocation_result_type;

		template <typename Task, typename... Args>
		auto compute_unfold(details::scheduler* scheduler, Task&& task, [[maybe_unused]] std::tuple<Args...>&& args);

		template <typename FN1, typename FN2>
		struct continuation;

		// each task will be run concurrently
		template <typename... FNs>
		struct parallel_task;

		// the given task will be repeated N times (decided at compile time using the template parameter)
		template <typename FN, size_t Count = 0>
		struct repeat_task;

		// the given task will be repeated N times (decided at runtime)
		template <typename FN>
		struct repeat_task<FN, 0>;


		template <typename T>
		struct promise;
	} // namespace details

	template <typename Task, typename... Args>
	auto execute(details::scheduler* scheduler, Task&& task, Args&&... args);
} // namespace ASYNC_NAMESPACE

// helpers and utilities
namespace ASYNC_NAMESPACE::details
{
	template <typename T>
	struct remove_cvref
	{
		using type = std::remove_reference_t<std::remove_cv_t<T>>;
	};

	template <typename T>
	using remove_cvref_t = typename remove_cvref<T>::type;

	template <typename T>
	struct type_wrapper
	{
		using type = T;
	};

	template <typename T>
	struct type_wrapper<type_wrapper<T>> : public type_wrapper<T>
	{};

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
	struct is_parallel_task : std::false_type
	{};

	template <typename... FNs>
	struct is_parallel_task<parallel_task<FNs...>> : std::true_type
	{};


	template <typename... FNs>
	struct is_parallel_task<const parallel_task<FNs...>> : std::true_type
	{};
	template <typename... FNs>
	struct is_parallel_task<const parallel_task<FNs...>&> : std::true_type
	{};
	template <typename... FNs>
	struct is_parallel_task<parallel_task<FNs...>&> : std::true_type
	{};

	template <typename T>
	struct is_repeat_task : std::false_type
	{};

	template <typename FN, size_t C>
	struct is_repeat_task<repeat_task<FN, C>> : std::true_type
	{};

	template <typename FN, size_t C>
	struct is_repeat_task<const repeat_task<FN, C>> : std::true_type
	{};

	template <typename FN, size_t C>
	struct is_repeat_task<repeat_task<FN, C>&> : std::true_type
	{};

	template <typename FN, size_t C>
	struct is_repeat_task<const repeat_task<FN, C>&> : std::true_type
	{};

	template <typename T>
	struct is_dynamic_repeat_task : std::bool_constant<T::N == 0>
	{
		static const size_t count = T::N;
	};

	template <typename T>
	struct is_task
		: std::bool_constant<is_continuation<remove_cvref_t<T>>::value || is_parallel_task<remove_cvref_t<T>>::value ||
							 is_repeat_task<remove_cvref_t<T>>::value>
	{};


	template <typename T, typename F, typename N>
	struct all_return : std::false_type
	{};

	template <typename T, typename... FN, typename... Args>
	struct all_return<T, type_wrapper<std::tuple<FN...>>, type_wrapper<std::tuple<Args...>>>
		: std::bool_constant<(std::is_same<T, typename details::task_result_type<FN, Args...>::type>{} && ...)>
	{};

	// specialized invoke version that throws away the first arg in case of void, and applies std::tuple if it's the
	// first arg
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
	struct make_index_sequence_match<T, std::tuple<Args...>> : match<std::index_sequence<>, 0, T, std::tuple<Args...>>
	{};

	template <typename T, typename U>
	struct make_index_sequence_mismatch
	{};

	template <typename T, typename... Args>
	struct make_index_sequence_mismatch<T, std::tuple<Args...>>
		: mismatch<std::index_sequence<>, 0, T, std::tuple<Args...>>
	{};

	/// \details helper to unfold a tuple of future<T>, forcing the void returns to complete (S1), and returning those
	/// that have non-void types in a tuple (S2)
	template <size_t... S1, size_t... S2, typename F>
	auto get(std::index_sequence<S1...>, std::index_sequence<S2...>, F&& f)
	{
		(void(std::get<S1>(f).get()), ...);
		return std::tuple(std::move(std::get<S2>(f).get())...);
	}

	template <typename T, typename S>
	struct _task_parallel_result_type_helper2
	{};

	template <typename T, size_t... S>
	struct _task_parallel_result_type_helper2<type_wrapper<T>, std::index_sequence<S...>>
	{
		using type = std::tuple<typename std::tuple_element<S, T>::type...>;
	};

	template <typename T, typename... Args>
	auto _task_result_type_helper();

	template <typename... FNs, typename... Args>
	auto _task_parallel_result_type_helper(type_wrapper<std::tuple<FNs...>>, type_wrapper<std::tuple<Args...>>)
	{
		using result_t =
			typename decltype(_task_result_type_helper<std::tuple_element_t<0, std::tuple<FNs...>>, Args...>())::type;
		if constexpr(details::all_return<result_t, type_wrapper<std::tuple<FNs...>>,
										 type_wrapper<std::tuple<Args...>>>::value)
		{
			if constexpr(std::is_same_v<void, result_t>)
				return type_wrapper<void>{};
			else
				return type_wrapper<std::array<result_t, sizeof...(FNs)>>{};
		}
		else
		{
			// todo possible error here in pack expansion
			using functions  = std::tuple<typename details::task_result_type<FNs, Args...>::type...>;
			using real_types = typename details::make_index_sequence_match<void, functions>::type;
			return type_wrapper<
				typename _task_parallel_result_type_helper2<type_wrapper<functions>, real_types>::type>{};
		}
	}

	template <typename T, typename... Args>
	auto _task_result_type_helper()
	{
		using task_t = details::remove_cvref_t<T>;
		if constexpr(is_continuation<task_t>::value)
		{
			using FN1	  = decltype(std::declval<task_t>().first);
			using FN2	  = decltype(std::declval<task_t>().second);
			using result_t = typename decltype(_task_result_type_helper<FN1, Args...>())::type;
			if constexpr(std::is_same_v<result_t, void>)
			{
				return _task_result_type_helper<FN2>();
			}
			else
			{
				return _task_result_type_helper<FN2, result_t>();
			}
		}
		else if constexpr(is_parallel_task<task_t>::value)
		{
			using FNs = decltype(std::declval<task_t>().fns);
			return _task_parallel_result_type_helper(type_wrapper<FNs>{}, type_wrapper<std::tuple<Args...>>{});
		}
		else if constexpr(is_repeat_task<task_t>::value)
		{
			using FN = remove_cvref_t<decltype(std::declval<task_t>().fn)>;
			if constexpr(std::is_invocable<FN, invocation, Args...>::value)
			{
				using result_t = typename decltype(_task_result_type_helper<FN, invocation, Args...>())::type;
				if constexpr(std::is_same_v<result_t, void>)
				{
					return type_wrapper<void>{};
				}
				else if constexpr(is_dynamic_repeat_task<task_t>::value)
				{
					return type_wrapper<std::vector<result_t>>{};
				}
				else
				{
					constexpr auto count = is_dynamic_repeat_task<task_t>::count;
					return type_wrapper<std::array<result_t, count>>{};
				}
			}
			else
			{
				using result_t = typename decltype(_task_result_type_helper<FN, Args...>())::type;
				if constexpr(std::is_same_v<result_t, void>)
				{
					return type_wrapper<void>{};
				}
				else if constexpr(is_dynamic_repeat_task<task_t>::value)
				{
					return type_wrapper<std::vector<result_t>>{};
				}
				else
				{
					constexpr auto count = is_dynamic_repeat_task<task_t>::count;
					return type_wrapper<std::array<result_t, count>>{};
				}
			}
		}
		else
		{
			return type_wrapper<invoke_result_t<task_t, Args...>>{};
		}
	}

	template <typename T, typename... Args>
	struct task_result_type
	{
		using type = typename decltype(_task_result_type_helper<remove_cvref_t<T>, Args...>())::type;
	};

	template <typename T, typename... Args>
	struct is_invocable : std::is_invocable<T, Args...>
	{};

	template <typename FN1, typename FN2, typename... Args>
	struct is_invocable<continuation<FN1, FN2>, Args...> : std::is_invocable<FN2, Args...>
	{};


	template <typename FN, typename SFINAE = void, typename... Args>
	struct parallel_invocation_result_type
	{
		using type = typename task_result_type<FN, Args...>::type;
	};

	template <typename FN, typename... Args>
	struct parallel_invocation_result_type<
		FN, typename std::enable_if<std::is_invocable<FN, Args..., invocation>::value>::type, Args...>
	{
		using type = typename task_result_type<FN, invocation, Args...>::type;
	};
} // namespace ASYNC_NAMESPACE::details


namespace ASYNC_NAMESPACE
{
	namespace details
	{
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

		template <typename param_t, typename fn_t>
		struct promise_wrapper
		{
			param_t param;
			fn_t func;

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
					if constexpr(std::is_same<typename task_result_type<fn_t, Args...>::type, void>::value)
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

		// scheduler like object
		class scheduler
		{
		  public:
			template <typename F, typename T, typename... Args>
			auto execute(F&& f, T promise, Args&&... args)
			{
				using vT = typename task_result_type<F, Args...>::type;
				if constexpr(is_task<F>::value)
				{
					f(std::move(promise), *this, std::forward<Args>(args)...);
				}
				else
				{
					if constexpr(std::is_same_v<vT, void>)
					{
						f(std::forward<Args>(args)...);
						promise.set_value();
					}
					else
						promise.set_value(f(std::forward<Args>(args)...));
				}
			}
		};

		template <typename... FNs>
		struct parallel_task
		{
			static_assert(sizeof...(FNs) > 1);
			constexpr parallel_task(FNs&&... fns) noexcept : fns(std::forward_as_tuple(fns...)){};

			constexpr parallel_task(const parallel_task& other) noexcept : fns(other.fns){};
			constexpr parallel_task(parallel_task&& other) noexcept : fns(std::move(other.fns)){};
			constexpr parallel_task& operator=(const parallel_task& other) noexcept
			{
				if(this != &other)
				{
					fns = other.fns;
				}
				return *this;
			};
			constexpr parallel_task& operator=(parallel_task&& other) noexcept
			{
				if(this != &other)
				{
					fns = std::move(other.fns);
				}
				return *this;
			};

		  private:
			template <typename T, typename F, size_t... S>
			auto parallel_helper(F&& f, std::index_sequence<S...>)
			{
				return std::array<T, sizeof...(S)>{std::move(f[S].get())...};
			}

		  public:
			template <typename T, typename... Args>
			auto operator()(T&& p, details::scheduler* scheduler, Args&&... args)
			{
				using result_t = typename details::task_result_type<std::tuple_element_t<0, std::tuple<FNs...>>>::type;
				if constexpr(details::all_return<result_t, type_wrapper<std::tuple<FNs...>>,
												 type_wrapper<std::tuple<Args...>>>::value)
				{
					auto futures = std::apply(
						[scheduler, arg_pack = std::forward_as_tuple(args...)](auto&&... task) mutable {
							return std::array<std::future<result_t>, sizeof...(task)>{
								compute_unfold(scheduler, task, std::forward<decltype(arg_pack)>(arg_pack))...};
						},
						std::forward<decltype(fns)>(fns));

					if constexpr(!std::is_same<result_t, void>::value)
					{
						p.set_value(
							parallel_helper<result_t>(std::move(futures), std::make_index_sequence<sizeof...(FNs)>{}));
					}
					else
					{
						for(auto&& value : futures) value.get();
						p.set_value();
					}
				}
				else
				{
					auto futures = std::move(std::apply(
						[scheduler, arg_pack = std::forward_as_tuple(args...)](auto&&... task) mutable {
							return std::make_tuple(
								compute_unfold(scheduler, task, std::forward<decltype(arg_pack)>(arg_pack))...);
						},
						std::forward<decltype(fns)>(fns)));

					using void_types =
						typename details::make_index_sequence_mismatch<std::future<void>, decltype(futures)>::type;
					using real_types =
						typename details::make_index_sequence_match<std::future<void>, decltype(futures)>::type;

					p.set_value(details::get(void_types{}, real_types{}, futures));
				}
			}

			std::tuple<FNs...> fns;
		};


		template <typename FN, size_t Count>
		struct repeat_task
		{
			static constexpr const auto N{Count};

		  private:
			template <typename T, typename F, size_t... S>
			auto parallel_helper(F&& f, std::index_sequence<S...>)
			{
				return std::array<T, sizeof...(S)>{std::move(f[S].get())...};
			}

		  public:
			template <typename T, typename... Args>
			auto operator()(T&& p, details::scheduler* scheduler, Args&&... args)
			{
				using result_t = typename details::parallel_invocation_result_type<FN, Args...>::type;
				constexpr bool supports_invocation = std::is_invocable<FN, invocation, Args...>::value;

				std::array<std::future<result_t>, Count> futures;
				auto current_future = std::begin(futures);
				for(auto i = 0u; i < Count; ++i)
				{
					if constexpr(supports_invocation)
					{
						*current_future = std::move(
							execute(scheduler, std::forward<FN>(fn), invocation{i, N}, std::forward<Args>(args)...));
					}
					else
					{
						*current_future =
							std::move(execute(scheduler, std::forward<FN>(fn), std::forward<Args>(args)...));
					}
					++current_future;
				}

				if constexpr(!std::is_same<result_t, void>::value)
				{
					p.set_value(
						std::move(parallel_helper<result_t>(std::move(futures), std::make_index_sequence<Count>{})));
				}
				else
				{
					for(auto&& value : futures) value.get();
					p.set_value();
				}
			}

			FN fn;
		};

		template <typename FN>
		struct repeat_task<FN, 0>
		{
			static constexpr const auto N{0};
			template <typename T, typename... Args>
			auto operator()(T&& p, details::scheduler* scheduler, Args&&... args)
			{
				using result_t = typename details::parallel_invocation_result_type<FN, Args...>::type;
				constexpr bool supports_invocation = std::is_invocable<FN, invocation, Args...>::value;

				std::vector<std::future<result_t>> futures;
				for(auto i = 0u; i < count; ++i)
				{
					if constexpr(supports_invocation)
					{
						futures.emplace_back(std::move(execute(scheduler, std::forward<FN>(fn), invocation{i, count},
															   std::forward<Args>(args)...)));
					}
					else
					{
						futures.emplace_back(
							std::move(execute(scheduler, std::forward<FN>(fn), std::forward<Args>(args)...)));
					}
				}

				if constexpr(!std::is_same<result_t, void>::value)
				{
					std::vector<result_t> values;
					values.reserve(futures.size());
					for(auto&& value : futures) values.emplace_back(std::move(value.get()));
					p.set_value(std::move(values));
				}
				else
				{
					for(auto&& value : futures) value.get();
					p.set_value();
				}
			}


			FN fn;
			size_t count;
		};

		template <typename FN1, typename FN2>
		struct continuation
		{
			constexpr continuation(FN1&& fn1, FN2&& fn2) noexcept
				: first(std::forward<FN1>(fn1)), second(std::forward<FN2>(fn2)){};

			template <typename T, typename... Args>
			auto operator()(T&& p, details::scheduler* scheduler, Args&&... args)
			{
				using result_t = typename task_result_type<FN1, Args...>::type;
				if constexpr(details::is_task<FN1>::value && details::is_task<FN2>::value)
				{
					details::state_object<result_t> state;
					std::invoke(first, details::promise<result_t>{&state}, scheduler, std::forward<Args>(args)...);
					if constexpr(std::is_same<result_t, void>::value)
					{
						std::invoke(second, p, scheduler);
					}
					else
					{
						std::invoke(second, p, scheduler, std::get<2>(state.data));
					}
				}
				else if constexpr(details::is_task<FN1>::value)
				{
					std::invoke(first, details::promise_wrapper<T, FN2>{std::forward<T>(p), second}, scheduler,
								std::forward<Args>(args)...);
				}
				else if constexpr(!details::is_task<FN1>::value && details::is_task<FN2>::value)
				{
					if constexpr(std::is_same<result_t, void>::value)
					{
						std::invoke(first, std::forward<Args>(args)...);
						std::invoke(second, p, scheduler);
					}
					else
					{
						std::invoke(second, p, scheduler, std::invoke(first, std::forward<Args>(args)...));
					}
				}
				else
				{
					// start of the chain
					scheduler->execute(first, details::promise_wrapper<T, FN2>{std::forward<T>(p), second},
									   std::forward<Args>(args)...);
				}
			}

			FN1 first;
			FN2 second;
		};

		template <typename Task, typename... Args, size_t... S>
		auto compute_unfold(details::scheduler* scheduler, Task&& task, std::tuple<Args...>&& args,
							std::index_sequence<S...>)
		{
			using T = typename details::task_result_type<Task, Args...>::type;
			std::promise<T> promise;
			auto future = promise.get_future();
			scheduler->execute(std::forward<Task>(task), std::move(promise),
							   std::forward<std::tuple_element_t<S, std::tuple<Args...>>>(std::get<S>(args))...);
			return future;
		}

		template <typename Task, typename... Args>
		auto compute_unfold(details::scheduler* scheduler, Task&& task, [[maybe_unused]] std::tuple<Args...>&& args)
		{
			if constexpr(sizeof...(Args) == 0)
			{
				using T = typename details::task_result_type<Task>::type;
				std::promise<T> promise;
				auto future = promise.get_future();
				scheduler->execute(std::forward<Task>(task), std::move(promise));
				return future;
			}
			else
			{
				return compute_unfold(scheduler, std::forward<Task>(task), std::forward<std::tuple<Args...>>(args),
									  std::make_index_sequence<sizeof...(Args)>{});
			}
		}
	} // namespace details

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
	} // namespace details

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
		using R = typename details::task_result_type<Pred>::type;
		static_assert(std::is_same<R, bool>::value || std::is_trivially_constructible<bool, R>::value);
		return then(std::forward<FN1>(fn1),
					then_or(std::forward<Pred>(pred), std::forward<FN2>(fn2), std::forward<FN3>(fn3)));
	}

	template <typename... FN>
	auto parallel(FN&&... tasks)
	{
		return details::parallel_task{std::forward<FN>(tasks)...};
	}

	template <typename FN>
	auto parallel_n(FN&& task, size_t count)
	{
		return details::repeat_task<FN, 0>{std::forward<FN>(task), count};
	}

	template <size_t S, typename FN>
	auto parallel_n(FN&& task)
	{
		return details::repeat_task<FN, S>{std::forward<FN>(task)};
	}

	namespace details
	{
		template <size_t... S, typename T>
		auto into(std::index_sequence<S...>, T&& t)
		{
			return then(
				details::parallel_task<typename std::tuple_element<S, T>::type...>(std::get<S>(t)...),
				std::forward<decltype(std::get<std::tuple_size_v<T> - 1>(t))>(std::get<std::tuple_size_v<T> - 1>(t)));
		}

		template <typename T>
		auto into(T&& t)
		{
			return into(std::make_index_sequence<std::tuple_size_v<T> - 1>{}, std::forward<T>(t));
		}
	} // namespace details

	/// \brief Aggregates the results of the preceding tasks.
	///
	/// \details Allows aggregegating the results of the previous tasks into a final task. Each of the preceding tasks
	/// are considered a bifurcation, and will be (potentially) ran concurrently.
	template <typename... FNn>
	auto into(FNn&&... tasks)
	{
		return details::into(std::forward_as_tuple(tasks...));
	}

	template <typename Task, typename... Args>
	auto execute(details::scheduler* scheduler, Task&& task, Args&&... args)
	{
		using T = typename details::task_result_type<Task, Args...>::type;
		std::promise<T> promise;
		auto future = promise.get_future();
		scheduler->execute(std::forward<Task>(task), std::move(promise), std::forward<Args>(args)...);
		return future;
	}

	template <execution ex, typename Task, typename... Args>
	auto compute(Task&& task, Args&&... args)
	{
		using task_t = details::remove_cvref_t<Task>;
		if constexpr(!details::is_task<task_t>::value)
		{
			using T = typename details::task_result_type<task_t, Args...>::type;
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
			using T = typename details::task_result_type<task_t, Args...>::type;
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
				details::state_object<T> state;
				details::scheduler* scheduler = nullptr;
				task(details::promise<T>{&state}, scheduler, std::forward<Args>(args)...);

				{
					auto lock = std::unique_lock{state.mutex};
					state.condition_variable.wait(lock, [&state]() { return state.data.index() != 0; });
				}

				if(state.data.index() == 1) std::rethrow_exception(std::get<1>(state.data));

				if constexpr(!std::is_same<T, void>::value) return std::move(std::get<2>(state.data));
			}
		}
	}
} // namespace ASYNC_NAMESPACE