/******************************************************************************
 * time.cpp                                                                   *
 *                                                                            *
 * STLEXT -- Extension to the Standard Template Library                       *
 *                                                                            *
 * Copyright (c) 1997-2020 by Ziemowit Laski.  All rights reserved.           *
 *                                                                            *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESSED  OR       *
 * IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED          *
 * WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR          *
 * PURPOSE.                                                                   *
 *                                                                            *
 ******************************************************************************/

#include <ctime>
#include "time.h"

namespace std {

const string &current_date_and_time(void) {

   static string s;
   time_t t;

   time(&t);
   char c[32];
   ctime_s(c, 32, &t);
   s = string(c, c + 24);
   return s;
}


}  // namespace std
