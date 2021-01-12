/******************************************************************************
 * slim_data.h                                                                *
 *                                                                            *
 * The SlimBinary(tm) Decoder                                                 *
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

#ifndef __juice_slim_data
#define __juice_slim_data

#include <string>
#include <iostream>
#include <sstream>
using namespace std;

#include "string.h"
using namespace std;

namespace juice {

// oberon DIV and MOD operators have DIFFERENT semantics than
// the C++ '/' and '%' operators!

long div(long n, long d);
long mod(long n, long d);

char hex_value(char c);
char hex_digit(char c);

const string &printable(const string &str);

// base class for slim binary data

class slim_datum {
  virtual void _calcval(void) = 0;
 public:
  string str;  // this will store the underlying byte representation
  void print_hex(ostream &out);
  void assign_str(const string &rhs);
  friend ostream &operator <<(ostream &out, const slim_datum &d);
};


// slim binary integers (of all sizes -- they are encoded similarly

class slim_int: public slim_datum {
  virtual void _calcval(void);
 public:
  long val;
  slim_int(long i = 0);
  slim_int &operator =(long i);
  friend istream &operator >>(istream &in, slim_int &i);
};


// slim binary reals -- normal IEEE floats!

class slim_real: public slim_datum {
  virtual void _calcval(void);
 public:
  float val;
  slim_real(float r = 0.0);
  slim_real &operator =(float r);
  friend istream &operator >>(istream &in, slim_real &r);
};


// slim binary long reals -- double IEEE floats!

class slim_longreal: public slim_datum {
  virtual void _calcval(void);
 public:
  double val;
  slim_longreal(double r = 0.0);
  slim_longreal &operator =(double r);
  friend istream &operator >>(istream &in, slim_longreal &r);
};


// slim binary strings

class slim_str: public slim_datum {
  virtual void _calcval(void);
 public:
  string val;
  slim_str(const string &s = "");
  slim_str &operator =(const string &s);
  friend istream &operator >>(istream &in, slim_str &s);
};


}  // namespace juice
#endif // #ifndef __juice_slim_data
