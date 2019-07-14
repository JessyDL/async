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
				//state->data.emplace<I>((args...);
				state->data.template emplace<I>(std::forward<Args>(args)...);
				state->condition_variable.notify_one();
			}

			template<typename... Args>
			void set_value(Args&& ... args) { set<2>(std::forward<Args>(args)...); }
			template<typename Y>
			void set_exception(Y&& e) { set<1>(std::forward<Y>(e)); }
		};

		template<typename param_t, typename fn_t>
		struct promise_like
		{
			param_t param;
			fn_t func;

			template<typename... Args>
			auto set_value(Args&& ... args)
			{
				param.set_value(std::invoke(func, std::forward<Args>(args)...));
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
	}


	template<typename FN1, typename FN2>
	auto then(FN1&& task1, FN2&& task2)
	{
		auto lambda = [task1 = task1, task2 = task2](auto p)
		{
			using param_t = decltype(p);
			using fn_t = decltype(task2);


			if constexpr (std::is_same< param_t, void*>::value)
			{
				return 0;
				//return std::declval<typename details::function_traits<decltype(std::function(std::declval<FN2>()))>::result_type>();
			}
			else
			{
				std::invoke(task1, details::promise_like<param_t, fn_t>{std::forward<param_t>(p), task2});
			}
		};

		return lambda;

	}

	template<typename FN1, typename FN2, typename ... FNn>
	auto then(FN1&& task1, FN2&& task2, FNn&& ... tasks)
	{
		return then(then(task1, task2), std::forward<FNn>(tasks)...);
	}

	template<typename Task, typename T = decltype(std::declval<Task>()(std::declval<void*>()))>
	T sync_wait(Task && task)
	{
		details::state_object<T> state;

		task(details::promise<T>{&state});

		{
			auto lock = std::unique_lock{ state.mutex };
			state.condition_variable.wait(lock, [&state]()
				{return state.data.index() != 0; });
		}

		if (state.data.index() == 1)
			std::rethrow_exception(std::get<1>(state.data));

		return std::move(std::get<2>(state.data));
	}

	template<typename Task, typename T = decltype(std::declval<Task>()(std::declval<void*>()))>
	std::future<T> async_compute(Task && task)
	{
		details::state_object<T> state;

		std::promise<T> promise;
		auto future = promise.get_future();
		task(std::move(promise));
		return future;
	}


	template<execution ex = execution::async, typename Task, typename T = decltype(std::declval<Task>()(std::declval<void*>()))>
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

			task(details::promise<T>{&state});

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