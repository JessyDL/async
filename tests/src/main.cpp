#include "algorithm.h"
#include <iostream>

auto thread_like() { return [](auto p)
{
	p.set_value();
}; }
auto new_thread()
{ 
	return [](auto p) 
	{
		std::thread t
		{ 
			[p = std::move(p)] () mutable 
			{
				p.set_value(); 
			} 
		}; 
		t.detach();
	}; 
}


template<typename LT>
class task_object
{
public:
	task_object(LT&& lambda) : lambda(std::forward<LT>(lambda)) {};

	template<typename... Args>
	auto operator()(Args&& ... args) { return std::invoke(lambda, std::forward<Args>(args)...); }
	LT lambda;
};

int main()
{
	auto thread_like_task = task_object{ thread_like() };
	using namespace ASYNC_NAMESPACE;
	auto task = then(thread_like(), []()
		{
			return 42;
		}, [](int value) -> int
		{
			printf("%d\n", value);
			return 0;
		},
			[](int x)
		{
			return 42;
		});


	auto t1 = ([]() { return true; });
	auto t2 = then([]() { return 10; }, [](int previous) { return previous + 10; });
	auto t3 = into(t1, t2, [](bool truth, int value) 
		{ return (truth) ? 100 : 2; });


	auto t4 = parallel_n(t1, 2);
	auto t5 = parallel([]() { return 30; }, t2, t2);
	auto t6 = into(t5, [](std::vector<std::future<int>> values) 
		{ 
			int sum = 0;
			for (auto& future : values)
			{
				sum += future.get();
			}
			return sum;
		});
	//auto t3 = details::into_impl([](bool truth, int value) { return (truth) ? 100 : 2; }, t1, t2);

	//token >> token1 >> parallel(8, token2) >> print_token;

	//auto tuple = std::tuple{ compute<execution::wait>(t1), compute<execution::wait>(t2)};

	auto a = compute<execution::async>([]()
		{ return true; });
	auto x = compute<execution::wait>(t3);

	auto y = compute<execution::async>(t6);

	auto resa = a.get();
	auto resy = y.get();
	return 0;
}