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

// ==================================== assert_failure

assert_failure::assert_failure(const string &file, int line, const string &expr):
      runtime_error(string(file + "(" + str(line) + "): assert(" + expr +")")) {
   // nothing
}

}  // namespace std
