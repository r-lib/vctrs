cmake_minimum_required(VERSION 3.6)
project(vctrs)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++98")

set(R_LIBRARY "~/R-library")
set(R_INCLUDE "/usr/share/R/include")

include_directories(${R_LIBRARY}/Rcpp/include)
include_directories(${R_INCLUDE})
include_directories(inst/include)

file(GLOB SOURCE_FILES src/*.cpp src/*.c)
add_library(vctrs ${SOURCE_FILES})

add_custom_target(
    Rinstall
    # Using MAKEFLAGS=-j$(nproc) didn't work
    COMMAND MAKEFLAGS=-j8 nice R CMD INSTALL --libs-only --no-test-load ${vctrs_SOURCE_DIR}
)
