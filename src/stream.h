/******************************************************************************
 * stream                                                                     *
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

#ifndef __STLEXT_STREAM
#define __STLEXT_STREAM

#include <fstream>
#include <string>
#include <stack>
#include "string.h"
#include "exception.h"

namespace std {

// test for the existence of a file

bool file_exists(const string &name);
void split_filename(string &path, string &base);

// UTF-8 streams

class utf8ifstream: private ifstream {
public:
  typedef ifstream::pos_type pos_type;
  typedef ifstream::int_type int_type;
private:
  struct pos_info {
    pos_type pos;
    unsigned row, col;
  };
  stack<pos_info> mark_stack;
  unsigned row, col;
  void init(void);
public:
  explicit utf8ifstream(void);
  explicit utf8ifstream(const string &name);
  void open(const string &name);
  void close(void);
  int_type get(void);
  wstring getline(void);
  void push(void);
  void pop(void) /*throw(string_exception)*/;
  void reset(void) /*throw(string_exception)*/;
};

class utf8ofstream: private ofstream {
public:
  explicit utf8ofstream(void);
  explicit utf8ofstream(const string &name);
  void open(const string &name);
  void close(void);
  void put(const wchar_t ch);
  void putstr(const wstring &str);
};

} // namespace std
#endif // #ifndef __STLEXT_STREAM

