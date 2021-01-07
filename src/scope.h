/******************************************************************************
 * scope                                                                      *
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

#ifndef __STLEXT_SCOPE
#define __STLEXT_SCOPE

#include <string>
#include <vector>
#include <iostream>

namespace std {

// scope can have zero or more slots


class scope {
  protected:
   scope *_owner, *_base;
   vector<scope *> _slot;
   vector<bool> _owns;
  public:
   string name;

   scope(const string &n = "");
   ~scope(void);

   int size(void) const;
   int ext_size(void) const;

   int print(ostream &out, int depth = -1, int indent = 0) const;
   void clear(void);

   bool eq(const string &n) const;
   bool operator ==(const string &n) const;
   bool ne(const string &n) const;
   bool operator !=(const string &n) const;

   const scope &owner(void) const;
   bool has_owner(void) const;
   bool has_owner(const scope &o) const;
   void set_owner(void);
   void set_owner(scope &o);

   bool ext_has_owner(const scope &o) const;

   const scope &base(void) const;
   bool has_base(void) const;
   bool has_base(const scope &b) const;
   void set_base(void);
   void set_base(scope &b);

   bool ext_has_base(const scope &b) const;

   // locating a slot of a given value

   int has(const string &n, int start = 1) const;
   int has(const scope &obj, int start = 1) const;
   bool has(int num) const;
   int ext_has(const string &n, int start = 1) const;
   int ext_has(const scope &obj, int start = 1) const;
   bool ext_has(int num) const;

   int owns(const string &n, int start = 1) const;
   int owns(const scope &obj, int start = 1) const;
   bool owns(int num) const;
   int ext_owns(const string &n, int start = 1) const;
   int ext_owns(const scope &obj, int start = 1) const;
   bool ext_owns(int num) const;

   int find_slot_obj(const scope &obj, int start = 1) const;
   int ext_find_slot_obj(const scope &obj, int start = 1) const;

   // query subscopes by name, creating them if necessary.  creation occurs
   // always in the current scope, never in the base scope

   scope &new_slot(const string &n, int new_pos = 0);   // always create
   scope &operator [](const string &n);
   scope &operator <<(const string &n);
   scope &new_slot(scope &obj, int new_pos = 0);
   scope &operator [](scope &obj);
   scope &operator <<(scope &obj);

   scope &slot(const string &n, int new_pos = 0);   // optionally create
   scope &operator ()(const string &n);
   scope &operator <(const string &n);
   scope &slot(scope &obj, int new_pos = 0);
   scope &operator ()(scope &obj);
   scope &operator <(scope &obj);

   scope &old_slot(const string &n) const;   // never create
   scope &old_slot(int num) const;
   scope &operator ()(int num) const;

   scope &ext_old_slot(int num) const;

   scope &append_scope(scope &s);          // always append

};

}  // namespace std
#endif // #ifndef __STLEXT_SCOPE
