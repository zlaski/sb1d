/******************************************************************************
 * string                                                                     *
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

#ifndef __STLEXT_STRING
#define __STLEXT_STRING

#include <locale>
#include <strstream>
#include <string>
#include "stream.h"

namespace std {

// create a string representation of any type

template <class C>
const string &str(const C &c) {
   static string r;
   char buf[240];
   ostrstream s(buf, 240);

   s << c << '\0';
   r = s.str();
   return r;
}

// convert from string representation back to type

template <class C>
const C &val(const string &r) {
   static C c;
   istrstream s(r.c_str());

   s >> c;
   return c;
}


// decimal, hex and octal conversions for integral types

const string &dec(const long &val, int wide = 0, char filler = '0');
const string &hex(const long &val, int wide = 0, char filler = '0');
const string &oct(const long &val, int wide = 0, char filler = '0');

// c literal conversions

const int digit_val(const int c);
const int digit_str(const int c);

const string &literal(const unsigned char c);
const unsigned char literal(const string &s);
const unsigned char literal(ifstream &f);

const string &quoted_literal(const unsigned char c);
const string &quoted_literal(const string &s);

} // namespace std
#endif // #ifndef __STLEXT_STRING

