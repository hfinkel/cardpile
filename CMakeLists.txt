#          Copyright Hal Finkel 2023.
# Distributed under the Boost Software License, Version 1.0.
#    (See accompanying file LICENSE.txt or copy at
#          https://www.boost.org/LICENSE_1_0.txt)
# SPDX-License-Identifier: BSL-1.0

cmake_minimum_required(VERSION 3.16.0)

project(CardPile LANGUAGES CXX)

if(NOT DEFINED CMAKE_CXX_STANDARD OR CMAKE_CXX_STANDARD LESS 20)
  set(CMAKE_CXX_STANDARD 20)
endif()
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

include(CheckCXXCompilerFlag)

# A simple macro to check a CXX compiler flag, and if supported, add it to the
# default CXX flags.
macro(check_and_add_cxx_compiler_flag flag flagvar)
  check_cxx_compiler_flag(${flag} ${flagvar})
  if(${flagvar})
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${flag}")
  endif()
endmacro()

# We require a lot of constexpr evaluation. Push the limit near the maximum.
set(ConstexprSteps 2147483647)
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  if(CMAKE_CXX_COMPILER_FRONTEND_VARIANT STREQUAL "MSVC")
    check_and_add_cxx_compiler_flag("/constexpr:steps${ConstexprSteps}" COMPILER_HAS_MSVC_CONSTEXPR_STEPS)
  else()
    check_and_add_cxx_compiler_flag("-fconstexpr-steps=${ConstexprSteps}" COMPILER_HAS_CONSTEXPR_STEPS)
  endif()
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  check_and_add_cxx_compiler_flag("/constexpr:steps${ConstexprSteps}" COMPILER_HAS_MSVC_CONSTEXPR_STEPS)
else()
  check_and_add_cxx_compiler_flag("-fconstexpr-ops-limit=${ConstexprSteps}" COMPILER_HAS_CONSTEXPR_OPS_LIMIT)
endif()

include(GNUInstallDirs)

# Install headers in $INSTALL_DIR/${CMAKE_INSTALL_PREFIX}.
install(DIRECTORY include/
  DESTINATION include
  USE_SOURCE_PERMISSIONS
  FILES_MATCHING PATTERN "*.hpp")

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

include(CTest)
enable_testing()
# Define "make check" as alias for "make test"
add_custom_target(check COMMAND ctest)
add_subdirectory(tests)

