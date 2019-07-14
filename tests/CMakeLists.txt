set(PROJECT_TESTS async_tests CACHE INTERNAL "")
project(${PROJECT_TESTS} VERSION 1.0.0 LANGUAGES CXX)

include(inc.txt) 
include(src.txt) 
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/inc" PREFIX "inc" FILES ${INC}) 
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/src" PREFIX "src" FILES ${SRC}) 

add_executable(${PROJECT_TESTS} ${SRC} ${INC} ${NATVIS})

target_include_directories(${PROJECT_TESTS}
	PUBLIC 
      ${CMAKE_CURRENT_SOURCE_DIR}/inc
)

target_link_libraries(${PROJECT_TESTS} PUBLIC ${PROJECT})