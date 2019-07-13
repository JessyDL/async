cmake_minimum_required(VERSION 3.11)
enable_language(CXX)
set(PROJECT async CACHE INTERNAL "")
project(${PROJECT} VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(CMakeDependentOption)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(PE_BUILD_DIR "${CMAKE_BINARY_DIR}/builds/" CACHE PATH "target location where to build the project to")

###############################################################################
###                    setup output directories                             ###
###############################################################################
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${ASYNC_BUILD_DIR}/default/lib 
	CACHE PATH "Output directory for static libraries.")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ASYNC_BUILD_DIR}/default/lib
	CACHE PATH "Output directory for shared libraries.")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ASYNC_BUILD_DIR}/default/bin
	CACHE PATH "Output directory for executables and DLL's.")
	
file(MAKE_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
file(MAKE_DIRECTORY "${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}")
file(MAKE_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")

foreach(OUTPUTCONFIG ${CMAKE_CONFIGURATION_TYPES})
	string(TOUPPER ${OUTPUTCONFIG} OUTPUTCONFIG)
	string(TOLOWER ${OUTPUTCONFIG} OUTPUTCONFIG_FOLDERNAME)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_${OUTPUTCONFIG} "${ASYNC_BUILD_DIR}/${OUTPUTCONFIG_FOLDERNAME}/${ARCHI}/bin" CACHE PATH "" FORCE)
	set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_${OUTPUTCONFIG} "${ASYNC_BUILD_DIR}/${OUTPUTCONFIG_FOLDERNAME}/${ARCHI}/lib" CACHE PATH "" FORCE)
	set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${OUTPUTCONFIG} "${ASYNC_BUILD_DIR}/${OUTPUTCONFIG_FOLDERNAME}/${ARCHI}/lib" CACHE PATH "" FORCE)
	
	file(MAKE_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY_${OUTPUTCONFIG}}")
	file(MAKE_DIRECTORY "${CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${OUTPUTCONFIG}}")
	file(MAKE_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY_${OUTPUTCONFIG}}")
endforeach()



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