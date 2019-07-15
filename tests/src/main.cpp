#include "stdafx_async.h"
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

/// more complex version of the 'then' functionality
/// this one creates 2 task chains, and then runs them sequantially
void test_then5()
{
	auto sub_task1 = then([]() {}, []() {});
	auto sub_task2 = then([]() {}, []() {return 10; });
	auto task = then(sub_task1, sub_task2);
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

void test_parallel_n1()
{
	auto task = into(parallel_n([]() { return 10; }, 10), [](std::vector<int> values)
		{ return std::accumulate(std::begin(values), std::end(values), 0, [](int sum, int value) { return sum + value; }); });

	auto result = compute<execution::wait>(task);
	assert(result == 100);
}

void test_parallel_n2()
{
	auto task = into(parallel_n([]() { return 10; }, 10), [](std::vector<int> values)
		{ return std::accumulate(std::begin(values), std::end(values), 0, [](int sum, int value) { return sum + value; }); });

	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 100);
}

void test_disconnected_then1()
{
	auto task = then([]() {}, []() { return 10; });
	auto result = compute<execution::wait>(task);
	assert(result == 10);
}

void test_disconnected_then2()
{
	auto task = then([]() {}, []() {}, []() { return 10; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 10);
}

void complex_test1()
{
	constexpr size_t data_count = 10000;
	std::vector<int> data(data_count);
	std::iota(std::begin(data), std::end(data), 0);
	void* cache = malloc(sizeof(int) * data_count);
	auto cleanup = [cache]() { free(cache); };

	auto copy_to_cache = parallel([cache, &data, i = 0, max = 4]()
	{
		size_t count = data.size() / max;
		size_t offset = count * i;
		void* dst = (void*)((int*)(cache)+offset);
		std::memcpy(dst, data.data() + offset, sizeof(int)* count);
	},
		[cache, &data, i = 1, max = 4]()
	{
		size_t count = data.size() / max;
		size_t offset = count * i;
		void* dst = (void*)((int*)(cache)+offset);
		std::memcpy(dst, data.data() + offset, sizeof(int)* count);
	},
		[cache, &data, i = 2, max = 4]()
	{
		size_t count = data.size() / max;
		size_t offset = count * i;
		void* dst = (void*)((int*)(cache)+offset);
		std::memcpy(dst, data.data() + offset, sizeof(int)* count);
	},
		[cache, &data, i = 3, max = 4]()
	{
		size_t count = data.size() / max;
		size_t offset = count * i;
		void* dst = (void*)((int*)(cache)+offset);
		std::memcpy(dst, data.data() + offset, sizeof(int)* count);
	});

	auto change_data = [cache, count = data.size()]()
	{
		int* data_begin = (int*)(cache);
		int* data_end = data_begin + count;

		for (auto it = data_begin; it != data_end; ++it)
			(*it) += 10;
	};

	auto copy_from_cache = [cache, &data]()
	{
		int* it = (int*)(cache);

		for (auto& value : data)
		{
			value = (*it);
			++it;
		}
	};

	auto task = then(copy_to_cache, change_data, copy_from_cache, cleanup);
	auto future = compute<execution::async>(task);
	future.get();

	int expected = 10;
	for (const auto& value : data)
	{
		assert(expected == value);
		++expected;
	}
}
void complex_test2()
{
	constexpr size_t data_count = 10000;
	std::vector<int> data(data_count);
	std::iota(std::begin(data), std::end(data), 0);
	void* cache = malloc(sizeof(int) * data_count);
	auto cleanup = [cache]() { free(cache); };

	auto copy_to_cache = parallel_n([cache, &data](invocation invocation)
	{
		size_t count = data.size() / invocation.count;
		size_t offset = count * invocation.index;
		void* dst = (void*)((int*)(cache)+offset);
		std::memcpy(dst, data.data() + offset, sizeof(int)* count);
	}, 4);


	auto change_data = [cache, count = data.size()]()
	{
		int* data_begin = (int*)(cache);
		int* data_end = data_begin + count;

		for (auto it = data_begin; it != data_end; ++it)
			(*it) += 10;
	};

	auto copy_from_cache = parallel_n([cache, &data](invocation invocation)
		{
			size_t count = data.size() / invocation.count;
			size_t offset = count * invocation.index;
			void* src = (void*)((int*)(cache)+offset);
			std::memcpy(data.data() + offset, src, sizeof(int) * count);
		}, 4);

	auto task = then(copy_to_cache, change_data, copy_from_cache, cleanup);
	auto future = compute<execution::async>(task);
	future.get();

	int expected = 10;
	for (const auto& value : data)
	{
		assert(expected == value);
		++expected;
	}
}

void complex_test3()
{
	constexpr size_t data_count = 10000;
	std::vector<int> data(data_count);
	std::iota(std::begin(data), std::end(data), 0);
	void* cache = malloc(sizeof(int) * data_count);

	auto task = then
	(
		parallel_n([cache, &data](invocation invocation)
		{
			size_t count = data.size() / invocation.count;
			size_t offset = count * invocation.index;
			void* dst = (void*)((int*)(cache)+offset);
			std::memcpy(dst, data.data() + offset, sizeof(int) * count);
		}, 4),
		[cache, count = data.size()]()
		{
			int* data_begin = (int*)(cache);
			int* data_end = data_begin + count;

			for (auto it = data_begin; it != data_end; ++it)
				(*it) += 10;
		},
		[cache, &data]()
		{
			int* it = (int*)(cache);

			for (auto& value : data)
			{
				value = (*it);
				++it;
			}
		}, 
		[cache]() { free(cache); }
	);

	auto future = compute<execution::async>(task);
	future.get();

	int expected = 10;
	for (const auto& value : data)
	{
		assert(expected == value);
		++expected;
	}
}

int main()
{
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
	test_then1();
	test_then2();
	test_then3();
	test_then4();

	test_then5();

	test_into1();
	test_into2();

	test_parallel1();
	test_parallel2();

	test_parallel_n1();
	test_parallel_n2();

	test_disconnected_then1();
	test_disconnected_then2();

	complex_test1();
	complex_test2();
	return 0;
}