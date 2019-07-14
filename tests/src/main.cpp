#include "algorithm.h"
#include <iostream>

auto thread_like() { return [](auto p)
{
		p.set_value();
}; }
auto new_thread() { return [](auto p) {std::thread t{ [p = std::move(p)] () mutable {p.set_value(); } }; t.detach(); }; }


int main()
{
	using namespace ASYNC_NAMESPACE;
	auto task1 = then(thread_like(), []()
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

	//auto task2 = then(task1, );


	auto x = compute<execution::wait>(task1);

	return 0;
}