/******************************************************************************
 * slim_error.h                                                               *
 *                                                                            *
 * The Slim Binary(tm) Decoder                                                *
 *                                                                            *
 * Copyright (c) 1997-1999 by the Regents of the University of California     *
 * Copyright (c) 2000-2021 by Ziemowit Laski                                  *
 *                                                                            *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESSED  OR       *
 * IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED          *
 * WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR          *
 * PURPOSE.                                                                   *
 *                                                                            *
 *****************************************************************************/

#ifndef __juice_slim_error
#define __juice_slim_error

#include <string>
#include <iostream>
#include "exception.h"
using namespace std;

namespace juice {

// ==== slim_error.cpp

enum slim_errno {
   UnknownError,
   InvalidHeader,
   InvalidType,
   InvalidImportedType,
   InvalidSymTblSection,
   EndOfSymExpected,
   InvalidPointee,
   InvalidBaseType,
   InvalidCodeEnum,
   InvalidModuleEnum,
   BeginCodeExpected,
   EndOfCodeExpected,
   EndOfWithExpected,
   InvalidCaseType,
   MissingBaseType,
   MissingNamespace,
   CannotEnumerateEntity,
   InvalidConstType,
   AssertNotNegated,
   InvalidGlobalEnum,
   InvalidLocalEnum,
   CannotOpenModule
};

class slim_error: public runtime_error {
   static const char *msg[]; // error message array
   static string build_msg(slim_errno e, const string& f, streampos& o);
public:
   slim_error(slim_errno e, const string &f, streampos &o);
};


}  // namespace juice
#endif // #ifndef __juice_slim_error
