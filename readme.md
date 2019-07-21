# Async

Async is a small header only utility library to provide functional object chaining in an algorithmic way as showcased in [Eric Niebler's talk](https://www.youtube.com/watch?v=KJIluFH011k). It uses the talk as an inspirational starting point, and then adds more facilities such as parallel tasks, and merging results to be passed into subsequent tasks. As well as allow a custom scheduler to control how many threads are used for the task chain.

## Requirements

A C++17 compliant compiler is required due to the usage of `std::variant` and `if constexpr()`. For the rest the library has been tested on CLang 7.0.0 and higher, and MSVC 2019, and GCC 7.1.0.

## Options

By default the library will be built as a header-only library, but this can be tweaked with `-DASYNC_HEADER_ONLY=OFF`, which will then build the library as a static library.

You can change the default namespace of the library with the define `ASYNC_NAMESPACE`, which can also be controlled in CMake using the option `-DASYNC_NAMESPACE=your replacement`.

You can improve the compile times by enabling `-DASYNC_MINIMAL_INCLUDE`, this will disable the auto include of `std::vector` and `std::array`.

Lastly, tests are not built by default, but can be enabled by passing `-DASYNC_TESTS=ON`.

## Customization points

By default the scheduler will spawn a thread for every branching task, this can be improved by providing your own scheduler in the compute signature.

## API overview
### algorithms
**then**
Basic building block, allows sequential execution of the tasks in the chain. The results of the previous task will be fed into the next task in the chain.

**disconnect**
Allows a tasks' result to be disconnected/thrown away from the previous task. You would use this in case you don't care about the results of the previous task.

**then_or**
Allows for a task to be chosen depending on the output of the previous type. 2 variations exist, depending on the output of the previous task.
**version 1**
If the previous task returns a tuple, the first element has to be a boolean type (or implicitly convertible to one). This will decide which of the two subsequent tasks should be invoked, (true) task1 or (false) task2. The boolean type will potentially be absorbed into the task unless it is accepted as an input parameter in the subsequent task.
**version 2**
If the previous task returns a variant, it will check the index to decide which task to invoke.

**parallel**
The parallel algorithm allows task to run concurrently as they do not depend on eachother. The return value with either be a std::tuple or std::array depending on if all return values are the same or not.

**parallel_n**
The parallel_n algorithm allows for a single task to be ran N times, this value can both be decided at compile time as well as runtime. The return value depends on if the N count is know at compile time, or known as a runtime value. When known at compile time it will return a std::array<result_type, N>, otherwise it will return a std::vector<result_type>.

**into**
The into algorithm is a wrapper around `then(parallel(tasks...), final_task);`.

**compute**
Accepts any form of invocable, and will compute the results. Depending on the `async::execution` value, this will block or return a `std::future` of the result type.

## Examples

**simple then chain**
```cpp
auto seed_task = [](){ return 100; };
auto final_task = async::then(seed_task, [](int value) { return value == 100;});
auto result = async::compute<async::execution::wait>(final_task); // returns the type of the last task.
assert(result);
```

**complexer parallel execution into a task**
```cpp
auto seed_task = async::parallel([](){ return 100; }, [](){ return true; }, []() -> std::string { return "hello"; });

auto final_task = async::then(seed_task, [](int value1, bool value2, std::string message) 
{ /* do anything */ });
auto result = async::compute<async::execution::wait>(final_task); 
```