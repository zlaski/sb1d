/******************************************************************************
 * string                                                                     *
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

#ifndef __STLEXT_STRING
#define __STLEXT_STRING

#include <locale>
#include <sstream>
#include <string>
#include "stream.h"

namespace std {

// create a string representation of any type

template <class C>
string str(const C &c) {
   string r;
   ostringstream s;

   s << c /*<< '\0'*/;
   r = s.str();
   return r;
}

// convert from string representation back to type

template <class C>
C val(const string &r) {
   static C c;
   istringstream s(r);

   s >> c;
   return c;
}


string hex(const long &val, int wide = 0, char filler = '0');

// c literal conversions

const int digit_val(const int c);
const int digit_str(const int c);

string literal(const unsigned char c);
const unsigned char literal(const string &s);
const unsigned char literal(ifstream &f);

string quoted_literal(const unsigned char c);
string quoted_literal(const string &s);

} // namespace std
#endif // #ifndef __STLEXT_STRING

