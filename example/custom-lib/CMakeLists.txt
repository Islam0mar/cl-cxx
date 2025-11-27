cmake_minimum_required(VERSION 3.0)

# set the name of the library to compile
project(MyLib)

# set the directory to save the compiled lib
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib)

# optionally include the code for additional libs
# include(modules/link/AbletonLinkConfig.cmake)

# add the include directory for clcxx
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# set the cpp file to conpile
add_library(MyLib SHARED ${CMAKE_CURRENT_SOURCE_DIR}/src/my-lib.cpp)

# optionally add libraries needed for linking
# target_link_libraries(AbletonLink PRIVATE Ableton::Link)


