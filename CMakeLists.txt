cmake_minimum_required(VERSION 3.11)
enable_language(CXX)
set(PROJECT async CACHE INTERNAL "")
project(${PROJECT} VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(CMakeDependentOption)

OPTION(ASYNC_TESTS "generate the tests" OFF)
OPTION(ASYNC_HEADER_ONLY "header-only mode" ON)
OPTION(ASYNC_NAMESPACE "default namespace to use for the library" "async")

set(CMAKE_CXX_EXTENSIONS OFF )
set(CXX_EXTENSIONS OFF )
set(CMAKE_CXX_STANDARD 17)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
add_compile_definitions(ASYNC_NAMESPACE=${ASYNC_NAMESPACE})

include(inc.txt) 
include(src.txt)

source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/inc" PREFIX "inc" FILES ${INC}) 
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/src" PREFIX "src" FILES ${SRC}) 

find_package (Threads)

if(${ASYNC_HEADER_ONLY})
	add_library(${PROJECT} INTERFACE)
	target_include_directories(${PROJECT}
			INTERFACE 
			$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/inc>
	)
else()
	add_library(${PROJECT} STATIC ${INC} ${SRC} ${NATVIS})
	target_include_directories(${PROJECT}
			PUBLIC 
			$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/inc>
	)
  set_target_properties(${PROJECT} PROPERTIES LINKER_LANGUAGE CXX)
endif()
if(LINUX)
	target_link_libraries(${PROJECT} PUBLIC libc++experimental.a ${CMAKE_THREAD_LIBS_INIT})
else()
	target_link_libraries(${PROJECT} PUBLIC ${CMAKE_THREAD_LIBS_INIT})
endif()


if(${ASYNC_TESTS})
	add_subdirectory(tests)
endif()