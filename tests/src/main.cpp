#include "algorithm.h"
#include <iostream>
#include <cassert>
#include <numeric>
using namespace ASYNC_NAMESPACE;

void test_then1()
{
	auto task = then([]() {return true; }, [](bool value) { return 5; });
	auto result = compute<execution::wait>(task);
	assert(result == 5);
}

void test_then2()
{
	auto task = then([]() {return true; }, [](bool value) { return 5; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 5);
}

void test_then3()
{
	auto task = then([]() {return true; }, [](bool value) { return 0; }, [](int value) { return 10; });
	auto result = compute<execution::wait>(task);;
	assert(result == 10);
}

void test_then4()
{
	auto task = then([]() {return true; }, [](bool value) { return 0; }, [](int value) { return 10; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 10);
}

void test_into1()
{
	auto task = into(
		[]() 
		{ return true; },
		[]() 
		{return 5; }, 
		[]() 
		{ return 10; }, 
		[](bool test, int val1, int val2) 
		{return (test) ? val1 : val2; });
	auto result = compute<execution::wait>(task);
	assert(result == 5);
}

void test_into2()
{
	auto task = into([]() { return true; }, []() {return 5; }, []() { return 10; }, [](bool test, int val1, int val2) {return (test) ? val1 : val2; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 5);
}

void test_parallel1()
{
	auto task = into(parallel([]() { return 30; }, []() { return 10; }, []() { return 20; }), 		
		[](std::vector<int> futures) 
		{
			return std::accumulate(std::begin(futures), std::end(futures), 0, [](int sum, int f) { return f + sum; }); 
		});
	auto result = compute<execution::wait>(task);
	assert(result == 60);
}

void test_parallel2()
{
	auto task = into(parallel([]() { return 30; }, []() { return 10; }, []() { return 20; }), [](std::vector<int> futures) {return std::accumulate(std::begin(futures), std::end(futures), 0, [](int sum, int f) { return f + sum; }); });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 60);

}
int main()
{
	test_then1();
	test_then2();
	test_then3();
	test_then4();
	test_into1();
	test_into2();
	test_parallel1();
	test_parallel2();
	return 0;
}