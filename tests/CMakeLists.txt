#          Copyright Hal Finkel 2023.
# Distributed under the Boost Software License, Version 1.0.
#    (See accompanying file LICENSE.txt or copy at
#          https://www.boost.org/LICENSE_1_0.txt)
# SPDX-License-Identifier: BSL-1.0

add_executable(test1 test1.cpp)
add_test(NAME test1 COMMAND test1)
set_tests_properties (test1 PROPERTIES PASS_REGULAR_EXPRESSION "^PASSED")
set_tests_properties (test1 PROPERTIES FAIL_REGULAR_EXPRESSION "^FAILED")

