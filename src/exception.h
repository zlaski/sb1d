/******************************************************************************
 * exception                                                                  *
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

#ifndef __STLEXT_EXCEPTION
#define __STLEXT_EXCEPTION

#include <string>
#include <exception>
 
namespace std {

class string_exception: public exception {
  public:
  string msg;
  explicit string_exception(const string &exc_msg) throw();
  virtual ~string_exception(void) noexcept;
  virtual const char* what() const throw();
};

class assert_failure: public string_exception {
  public:
  explicit assert_failure(const string &file, int line, const string &expr);
  virtual ~assert_failure(void);
};

#undef assert

#if defined(DEBUG) || defined(_DEBUG)
  #define assert(expr) if(!(expr)) throw assert_failure(__FILE__, __LINE__, #expr)
#else
  #define assert(expr)
#endif

} // namespace std
#endif // #ifndef __STLEXT_EXCEPTION

