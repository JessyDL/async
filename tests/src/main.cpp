#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#include "algorithm.h"
#include <cassert>
#include <numeric>
#include <cstring>
#include <vector>
#include <array>
#include <optional>
using namespace ASYNC_NAMESPACE;
#define TEST_THEN
#define TEST_DISCONNECTED_THEN
#define TEST_PARALLEL_N
#define TEST_COMPLEX
#define TEST_PARALLEL
#define TEST_INTO

void test_continuation()
{
	auto task   = then([]() { return true; }, [](bool value) { return (value) ? 5 : 0; });
	auto result = compute<execution::wait>(task);
	assert(result == 5);

	auto task2 =
		then([]() { return true; }, [](bool value) { return (value) ? 5 : 0; }, [](int value) { return value * 2; });
	auto result2 = compute<execution::wait>(task2);
	assert(result2 == 10);

	auto task3   = then([](bool value) { return (value) ? 5 : 10; }, [](int value) { return value * 2; });
	auto result3 = compute(task3, true);
	assert(result3 == 10);
}

void test_continuation2()
{
	auto sub_task1 = then([]() -> void {}, []() { return true; });
	auto sub_task2 =
		then([](bool value) { return (value) ? 2.0 : 0.0; }, [](double value) { return (value == 2.0) ? 10 : 0; });

	auto task   = then(sub_task1, sub_task2);
	auto result = compute<execution::wait>(task);
	assert(result == 10);
}

void test_continuation3()
{
	bool state	 = false;
	auto sub_task1 = then([]() -> void {}, [&state]() -> void { state = true; });
	auto sub_task2 =
		then([&state]() { return (state) ? 2.0 : 0.0; }, [](double value) { return (value == 2.0) ? 10 : 0; });
	auto task   = then(sub_task1, sub_task2);
	auto result = compute<execution::wait>(task);
	assert(result == 10);
}
void test_continuation4()
{
	auto task = then([]() { return true; }, [](bool value) { return (value) ? 5 : 0; }, disconnect([]() { return 10; }),
					 [](int value) { return value * 2; });
	auto result = compute<execution::wait>(task);
	assert(result == 20);
}

void test_then1()
{
#ifdef TEST_THEN
	auto task   = then([]() { return true; }, [](bool value) { return (value) ? 5 : 0; });
	auto result = compute<execution::wait>(task);
	assert(result == 5);
#endif
}

void test_then2()
{
#ifdef TEST_THEN
	auto task   = then([]() { return true; }, [](bool value) { return (value) ? 5 : 0; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 5);
#endif
}

void test_then3()
{
#ifdef TEST_THEN
	auto task =
		then([]() { return true; }, [](bool value) { return (value) ? 0 : 5; }, [](int value) { return value + 10; });
	auto result = compute<execution::wait>(task);
	;
	assert(result == 10);
#endif
}

void test_then4()
{
#ifdef TEST_THEN
	auto task =
		then([]() { return true; }, [](bool value) { return (value) ? 0 : 5; }, [](int value) { return value + 10; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 10);
#endif
}

/// more complex version of the 'then' functionality
/// this one creates 2 task chains, and then runs them sequantially
void test_then5()
{
#ifdef TEST_THEN
	auto sub_task1 = then([]() {}, []() {});
	auto sub_task2 = then([]() {}, []() { return 10; });
	auto task	  = then(sub_task1, sub_task2);
	{
		auto future = compute<execution::async>(task);
		auto result = future.get();
		assert(result == 10);
	}
	{
		auto result = compute<execution::wait>(task);
		assert(result == 10);
	}
#endif
}

void test_then6()
{
	{
		auto future = compute<execution::async>([]() { return 10; });
		auto result = future.get();
		assert(result == 10);
	}
	{
		auto result = compute<execution::wait>([]() { return 10; });
		assert(result == 10);
	}
}

void test_then_or1()
{
	{
		auto task   = then_or([]() { return std::optional<int>(10); },
							  [](std::optional<int> value) { return value.value(); }, []() { return 2; });
		auto future = compute<execution::async>(task);
		auto result = future.get();
		assert(result == 10);
	}
	{
		auto task   = then_or([](bool value) { return value; }, []() { return 10; }, []() { return 20; });
		auto result = compute<execution::wait>(task, true);
		assert(result == 10);
	}
}

void test_then_or_variant1()
{
	{
		auto task = then_or(
			[](bool value) {
				if(value)
					return std::variant<int, double>(10);
				else
					return std::variant<int, double>(20.0);
			},
			[](int value) { return value; },
			then([](double value) { return value * 2; }, [](double value) -> int { return (int)value; }));
		auto future = compute<execution::async>(task, false);
		auto result = future.get();
		assert(result == 40);
	}
	{
		auto task   = then_or([](bool value) { return value; }, []() { return 10; }, []() { return 20; });
		auto result = compute<execution::wait>(task, true);
		assert(result == 10);
	}
}
void test_into1()
{
#ifdef TEST_THEN
#ifdef TEST_INTO
	auto task = into([]() { return true; }, []() { return 5; }, []() { return 10; },
					 [](bool test, int val1, int val2) { return (test) ? val1 : val2; });


	auto result = compute<execution::wait>(task);
	assert(result == 5);
#endif
#endif
}

void test_into2()
{
#ifdef TEST_THEN
#ifdef TEST_INTO
	auto task   = into([]() { return true; }, []() { return 5; }, []() { return 10; },
					   [](bool test, int val1, int val2) { return (test) ? val1 : val2; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 5);
#endif
#endif
}

void test_into3()
{
#ifdef TEST_THEN
#ifdef TEST_INTO
	// todo it would be nice to support this
	// auto task = into(
	//	[]()
	//	{ return true; },
	//	[]()
	//	{return 5; },
	//	[]()
	//	{ return 10; },
	//	[](std::tuple<bool, int, int> val)
	//	{return (std::get<0>(val)) ? std::get<1>(val) : std::get<2>(val); });


	// auto result = compute<execution::wait>(task);
	// assert(result == 5);
#endif
#endif
}

void test_parallel1()
{
#ifdef TEST_THEN
#ifdef TEST_PARALLEL
	auto task   = parallel([]() { return 30; }, []() { return 10; }, []() { return 20; });
	auto future = compute<execution::async>(task);

	auto result = future.get();
	assert(std::accumulate(std::begin(result), std::end(result), 0, [](int sum, int f) { return f + sum; }) == 60);
#endif
#endif
}

void test_parallel2()
{
#ifdef TEST_THEN
#ifdef TEST_PARALLEL
	auto task = then(
		parallel([]() { return 30; }, []() { return 10; }, []() { return 20; }), [](std::array<int, 3> values) mutable {
			return std::accumulate(std::begin(values), std::end(values), 0, [](int sum, int f) { return f + sum; });
		});
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 60);
#endif
#endif
}

void test_parallel3()
{
	auto task   = then(parallel([]() { return 30; }, []() { return false; }, []() { return 10; }),
					   [](int v1, bool t, int v2) { return (t) ? v1 : v2; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 10);
}

void test_parallel4()
{
	std::tuple<int, bool> v;

	auto task   = then(parallel([]() { return 30; }, []() {}, []() { return false; }, []() { return 10; }, []() {}),
					   [](int v1, bool t, int v2) { return (t) ? v1 : v2; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 10);
}

void test_parallel_n1()
{
#ifdef TEST_THEN
#ifdef TEST_PARALLEL_N
	auto task = then(parallel_n<10>([]() { return 10; }), [](std::array<int, 10> values) {
		return std::accumulate(std::begin(values), std::end(values), 0, [](int sum, int value) { return sum + value; });
	});

	auto result = compute<execution::wait>(task);
	assert(result == 100);
#endif
#endif
}

void test_parallel_n2()
{
#ifdef TEST_THEN
#ifdef TEST_PARALLEL_N
	auto task = then(parallel_n([]() { return 10; }, 10), [](std::vector<int> values) {
		return std::accumulate(std::begin(values), std::end(values), 0, [](int sum, int value) { return sum + value; });
	});

	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 100);
#endif
#endif
}

void test_disconnected_then1()
{
#ifdef TEST_THEN
#ifdef TEST_DISCONNECTED_THEN
	auto task   = then([]() {}, []() { return 10; });
	auto result = compute<execution::wait>(task);
	assert(result == 10);
#endif
#endif
}

void test_disconnected_then2()
{
#ifdef TEST_THEN
#ifdef TEST_DISCONNECTED_THEN
	auto task   = then([]() {}, []() {}, []() { return 10; });
	auto future = compute<execution::async>(task);
	auto result = future.get();
	assert(result == 10);
#endif
#endif
}

void complex_test1()
{
#ifdef TEST_THEN
#ifdef TEST_COMPLEX
#ifdef TEST_PARALLEL
	constexpr size_t data_count = 10000;
	std::vector<int> data(data_count);
	std::iota(std::begin(data), std::end(data), 0);
	void* cache  = malloc(sizeof(int) * data_count);
	auto cleanup = [cache]() { free(cache); };

	auto copy_to_cache = parallel(
		[cache, &data, i = 0, max = 4]() {
			size_t count  = data.size() / max;
			size_t offset = count * i;
			void* dst	 = (void*)((int*)(cache) + offset);
			std::memcpy(dst, data.data() + offset, sizeof(int) * count);
		},
		[cache, &data, i = 1, max = 4]() {
			size_t count  = data.size() / max;
			size_t offset = count * i;
			void* dst	 = (void*)((int*)(cache) + offset);
			std::memcpy(dst, data.data() + offset, sizeof(int) * count);
		},
		[cache, &data, i = 2, max = 4]() {
			size_t count  = data.size() / max;
			size_t offset = count * i;
			void* dst	 = (void*)((int*)(cache) + offset);
			std::memcpy(dst, data.data() + offset, sizeof(int) * count);
		},
		[cache, &data, i = 3, max = 4]() {
			size_t count  = data.size() / max;
			size_t offset = count * i;
			void* dst	 = (void*)((int*)(cache) + offset);
			std::memcpy(dst, data.data() + offset, sizeof(int) * count);
		});

	auto change_data = [cache, count = data.size()]() {
		int* data_begin = (int*)(cache);
		int* data_end   = data_begin + count;

		for(auto it = data_begin; it != data_end; ++it) (*it) += 10;
	};

	auto copy_from_cache = [cache, &data]() {
		int* it = (int*)(cache);

		for(auto& value : data)
		{
			value = (*it);
			++it;
		}
	};

	auto task   = then(copy_to_cache, change_data, copy_from_cache, cleanup);
	auto future = compute<execution::async>(task);
	future.get();

	int expected = 10;
	for(const auto& value : data)
	{
		assert(expected == value);
		++expected;
	}
#endif
#endif
#endif
}
void complex_test2()
{
#ifdef TEST_THEN
#ifdef TEST_COMPLEX
#ifdef TEST_PARALLEL_N
	constexpr size_t data_count = 10000;
	std::vector<int> data(data_count);
	std::iota(std::begin(data), std::end(data), 0);
	void* cache  = malloc(sizeof(int) * data_count);
	auto cleanup = [cache]() { free(cache); };

	auto copy_to_cache = parallel_n(
		[cache, &data](invocation invocation) {
			size_t count  = data.size() / invocation.count;
			size_t offset = count * invocation.index;
			void* dst	 = (void*)((int*)(cache) + offset);
			std::memcpy(dst, data.data() + offset, sizeof(int) * count);
		},
		4);


	auto change_data = [cache, count = data.size()]() {
		int* data_begin = (int*)(cache);
		int* data_end   = data_begin + count;

		for(auto it = data_begin; it != data_end; ++it) (*it) += 10;
	};

	auto copy_from_cache = parallel_n(
		[cache, &data](invocation invocation) {
			size_t count  = data.size() / invocation.count;
			size_t offset = count * invocation.index;
			void* src	 = (void*)((int*)(cache) + offset);
			std::memcpy(data.data() + offset, src, sizeof(int) * count);
		},
		4);

	auto task = then(copy_to_cache, change_data, copy_from_cache, cleanup);

	auto future = compute<execution::async>(task);
	future.get();

	int expected = 10;
	for(const auto& value : data)
	{
		assert(expected == value);
		++expected;
	}
#endif
#endif
#endif
}

void complex_test3()
{
#ifdef TEST_THEN
#ifdef TEST_COMPLEX
#ifdef TEST_PARALLEL_N
	constexpr size_t data_count = 10000;
	std::vector<int> data(data_count);
	std::iota(std::begin(data), std::end(data), 0);
	void* cache = malloc(sizeof(int) * data_count);

	auto task = then(parallel_n(
						 [cache, &data](invocation invocation) {
							 size_t count  = data.size() / invocation.count;
							 size_t offset = count * invocation.index;
							 void* dst	 = (void*)((int*)(cache) + offset);
							 std::memcpy(dst, data.data() + offset, sizeof(int) * count);
						 },
						 4),
					 [cache, count = data.size()]() {
						 int* data_begin = (int*)(cache);
						 int* data_end   = data_begin + count;

						 for(auto it = data_begin; it != data_end; ++it) (*it) += 10;
					 },
					 [cache, &data]() {
						 int* it = (int*)(cache);

						 for(auto& value : data)
						 {
							 value = (*it);
							 ++it;
						 }
					 },
					 [cache]() { free(cache); });

	auto future = compute<execution::async>(task);
	future.get();

	int expected = 10;
	for(const auto& value : data)
	{
		assert(expected == value);
		++expected;
	}
#endif
#endif
#endif
}

auto make_unique_task()
{
	std::unique_ptr<int> v{new int(100)};
	return [v = std::move(v)]() mutable { return *v; };
}

void complex_test4()
{
	auto task = then(make_unique_task(), [](int v) { return v * 2; });

	auto future = compute<execution::async>(std::move(task));
	auto result = future.get();
	assert(result == 200);
}

int main()
{
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
	test_continuation();
	test_continuation2();
	test_continuation3();

	test_then1();
	test_then2();
	test_then3();
	test_then4();

	test_then5();
	test_then6();

	test_then_or1();
	test_then_or_variant1();

	test_into1();
	test_into2();
	test_into3();

	test_parallel1();
	test_parallel2();
	test_parallel3();
	test_parallel4();

	test_parallel_n1();
	test_parallel_n2();

	test_disconnected_then1();
	test_disconnected_then2();

	complex_test1();
	complex_test2();
	complex_test3();
	complex_test4();
	return 0;
}