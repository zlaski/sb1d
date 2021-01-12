/******************************************************************************
 * exception.cpp                                                              *
 *                                                                            *
 * STLEXT -- Extension to the Standard Template Library                       *
 *                                                                            *
 * Copyright (c) 1997-2021 by Ziemowit Laski.  All rights reserved.           *
 *                                                                            *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESSED  OR       *
 * IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED          *
 * WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR          *
 * PURPOSE.                                                                   *
 *                                                                            *
 ******************************************************************************/

#include "exception.h"
#include "string.h"

namespace std {

// ==================================== string_exception

string_exception::string_exception(const string &exc_msg) throw():
      exception() {
  msg = exc_msg;
}
const char* string_exception::what() const throw() {
   return msg.c_str();
}
string_exception::~string_exception(void) noexcept {
   // nothing
}

// ==================================== assert_failure

assert_failure::assert_failure(const string &file, int line, const string &expr):
      string_exception(string(file + "(" + str(line) + "): assert(" + expr +")").c_str()) {
   // nothing
}

assert_failure::~assert_failure(void) {
   // nothing
}

}  // namespace std
