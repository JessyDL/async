cmake_minimum_required(VERSION 3.11)
enable_language(CXX)
set(PROJECT async CACHE INTERNAL "")
project(${PROJECT} VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(CMakeDependentOption)

set(CMAKE_CXX_EXTENSIONS OFF )
set(CXX_EXTENSIONS OFF )
set(CMAKE_CXX_STANDARD 17)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include(inc.txt) 
include(src.txt)

source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/inc" PREFIX "inc" FILES ${INC}) 
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/src" PREFIX "src" FILES ${SRC}) 

find_package (Threads)

add_library(${PROJECT} STATIC ${INC} ${SRC} ${NATVIS})
target_include_directories(${PROJECT}
    PUBLIC 
		$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/inc>
)

set_target_properties(${PROJECT} PROPERTIES LINKER_LANGUAGE CXX)
if(LINUX)
	target_link_libraries(${PROJECT} PUBLIC libc++experimental.a ${CMAKE_THREAD_LIBS_INIT})
else()
	target_link_libraries(${PROJECT} PUBLIC ${CMAKE_THREAD_LIBS_INIT})
endif()


add_subdirectory(tests)