/******************************************************************************
 * scope                                                                      *
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
   int size_incl_base(void) const;

   int print(ostream &out, int depth = -1, int indent = 0) const;
   void clear(void);

   bool eq(const string &n) const;
   bool ne(const string &n) const;

   const scope *owner(void) const;
   bool has_owner(void) const;
   bool has_owner(const scope *o) const;
   void set_owner(void);
   void set_owner(scope *o);

   bool has_owner_incl_base(const scope *o) const;

   const scope *base(void) const;
   bool has_base(void) const;
   bool has_base(const scope *b) const;
   void set_base(void);
   void set_base(scope *b);

   bool has_base_incl_base(const scope *b) const;

   // locating a slot of a given value

   int has_slot(const string &n, int start = 1) const;
   int has_slot(const scope *obj, int start = 1) const;
   bool has_at_least_slots(int num) const;
   int has_slot_incl_base(const string &n, int start = 1) const;
   int has_slot_incl_base(const scope *obj, int start = 1) const;
   bool has_at_least_slots_incl_base(int num) const;

   int owns(const string &n, int start = 1) const;
   int owns(const scope *obj, int start = 1) const;
   bool owns(int num) const;
   int owns_incl_base(const string &n, int start = 1) const;
   int owns_incl_base(const scope *obj, int start = 1) const;
   bool owns_incl_base(int num) const;

   int slot_offset(const scope *obj, int start = 1) const;
   int slot_offset_incl_base(const scope *obj, int start = 1) const;

   // query subscopes by name, creating them if necessary.  creation occurs
   // always in the current scope, never in the base scope

   scope *new_slot(const string &n, int new_pos = 0);   // always create
   scope *new_slot(scope *obj, int new_pos = 0);

   scope *slot(const string &n, int new_pos = 0);   // optionally create
   scope *slot(scope *obj, int new_pos = 0);

   scope *existing_slot(const string &n) const;   // never create
   scope *existing_slot(int num) const;

   scope *existing_slot_incl_base(int num) const;

   scope *append_scope(scope *s);          // always append

};

}  // namespace std
#endif // #ifndef __STLEXT_SCOPE
