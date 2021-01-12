/******************************************************************************
 * scope.cpp                                                                  *
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

#include <assert.h>

#include <algorithm>

#include "scope.h"
#include "string.h"
#include "exception.h"

namespace std {

// ==================================== scope::scope

scope::scope(const string &n) {
   _owner = _base = NULL;
   name = n;
}


// ==================================== scope::~scope

scope::~scope(void) {
   set_owner();   // detach from parent, if any
   set_base();
   clear();
}


// ==================================== scope::size

int scope::size(void) const {
   assert(_slot.size() == _owns.size());
   return _slot.size();
}


// ==================================== scope::size_incl_base

int scope::size_incl_base(void) const {
   int s = size();
   if(has_base()) {
      s += base()->size_incl_base();
   }
   return s;
}


// ==================================== scope::print

int scope::print(ostream &out, int depth, int indent) const {

   int num_lines = 1;   // at least!
   out << string(indent, ' ');

   out << quoted_literal(name)
         << " <0" << hex((unsigned long)this, 0) << "H>";
   if(has_owner()) {
      out << " [0" << hex((unsigned long)owner(), 0) << "H]";
   }
   if(has_base()) {
      out << ": (0" << hex((unsigned long)base(), 0) << "H)";
   }
   if(depth > 0 || depth == -1) {
      if(size() > 0) {
         out << " {\n";
         for(int i = 1; i <= size(); i++) {
            if(owns(i)) {
               num_lines += existing_slot(i)->print(out, (depth > 0? depth - 1: -1),
                     indent + 2);
            }
            else {
               num_lines += existing_slot(i)->print(out, 0, indent + 2);
            }
         }
         out << string(indent, ' ') << "}";
         if(num_lines >= 10) {
            out << " // " << quoted_literal(name)
                  << " (0" << hex((unsigned long)this, 0) << "H)";
         }
      }
   }
   else if(_slot.size() > 0) out << " {...}";
   out << endl;

   return num_lines;
}


// ==================================== scope::clear

void scope::clear(void) {
   name.erase();
   assert(_slot.size() == _owns.size());
   while(!_slot.empty()) {
      // only destroy the stuff we own
      if(_owns.back()) delete _slot.back();
      _slot.pop_back();
      _owns.pop_back();
   }
}


// ==================================== scope::eq

bool scope::eq(const string &n) const {
   return (name == n);
}


// ==================================== scope::ne

bool scope::ne(const string &n) const {
   return (name != n);
}


// ==================================== scope::owner

const scope *scope::owner(void) const {
  assert(_owner != NULL);
  return _owner;
}


// ==================================== scope::has_owner

bool scope::has_owner(void) const {
   return (_owner != NULL);
}


bool scope::has_owner(const scope *o) const {
   assert(o != NULL);
   return (_owner == o); 
}


// ==================================== scope::set_owner

void scope::set_owner(void) {
   if(_owner) {
      int pos = owner()->slot_offset(this);
      // _owner MUST own this object at this point!
      assert(pos > 0);
      assert(owner()->owns(pos));
      _owner->_owns[pos - 1] = false;
      _owner = NULL;
   }
}


void scope::set_owner(scope *o) {
   assert(o);
   set_owner();   // clear out previous owner, if any

   // the prospective new owner must already contain the object
   // in its scope!
   int pos = o->slot_offset(this);
   assert(pos > 0);
   assert(o->_owns[pos - 1] == false);
   o->_owns[pos - 1] = true;
   _owner = o;
}


// ==================================== scope::has_owner_incl_base

bool scope::has_owner_incl_base(const scope *o) const {
   assert(&o != NULL);
   return (this == o || has_owner(o)
         || (has_owner() && owner()->has_owner_incl_base(o)));
}


// ==================================== scope::base

const scope *scope::base(void) const {
  assert(_base != NULL);
  return _base;
}


// ==================================== scope::has_base

bool scope::has_base(void) const {
   return (_base != NULL);
}


bool scope::has_base(const scope *b) const {
   assert(b != NULL);
   return (_base == b);
}


// ==================================== scope::set_base

void scope::set_base(void) {
   _base = NULL;
}


void scope::set_base(scope *b) {
   assert(b);
   _base = b;
}


// ==================================== scope::has_base_incl_base

bool scope::has_base_incl_base(const scope *b) const {
   assert(b != NULL);
   return (this == b || has_base(b)
         || (has_base() && base()->has_base_incl_base(b)));
}


// ==================================== scope::has

int scope::has_slot(const string &n, int start) const {
   int i;
   if(start > 0) for(i = start; i <= size(); i++) {
      if(existing_slot(i)->name == n) {
         return i;    // 1-based
      }
   }
   if(start < 0) for(i = start; -i <= size(); i--) {
      if(existing_slot(i)->name == n) {
         return i;    // -1-based
      }
   }
   return 0; // indicates name was not found
}


int scope::has_slot(const scope *obj, int start) const {
   return has_slot(obj->name, start);
}


bool scope::has_at_least_slots(int num) const {
   assert(num != 0); // invalid query
   return abs(num) <= size();
}


// ==================================== scope::has_incl_base

int scope::has_slot_incl_base(const string &n, int start) const {
   int i;
   if(start > 0) for(i = start; i <= size_incl_base(); i++) {
      if(existing_slot_incl_base(i)->name == n) {
         return i;    // 1-based
      }
   }
   if(start < 0) for(i = start; -i <= size_incl_base(); i--) {
      if(existing_slot_incl_base(i)->name == n) {
         return i;    // -1-based
      }
   }
   return 0; // indicates name was not found
}


// ==================================== scope::owns

int scope::owns(const string &n, int start) const {
   int i;
   if(start > 0) for(i = start; i <= size(); i++) {
      if(existing_slot(i)->name == n && owns(i)) {
         return i;    // 1-based
      }
   }
   if(start < 0) for(i = start; -i <= size(); i--) {
      if(existing_slot(i)->name == n && owns(i)) {
         return i;    // -1-based
      }
   }
   return 0; // indicates name was not found
}


int scope::owns(const scope *obj, int start) const {
   return owns(obj->name, start);
}


bool scope::owns(int num) const {
   if(num < 0) num += size() + 1;
   assert(num >= 1 && num <= size());

   return _owns[num - 1];
}


// ==================================== scope::owns_incl_base

int scope::owns_incl_base(const string &n, int start) const {
   int i;
   if(start > 0) for(i = start; i <= size_incl_base(); i++) {
      if(existing_slot_incl_base(i)->name == n && owns_incl_base(i)) {
         return i;    // 1-based
      }
   }
   if(start < 0) for(i = start; -i <= size_incl_base(); i--) {
      if(existing_slot_incl_base(i)->name == n && owns_incl_base(i)) {
         return i;    // -1-based
      }
   }
   return 0; // indicates name was not found
}


int scope::owns_incl_base(const scope *obj, int start) const {
   return owns_incl_base(obj->name, start);
}


bool scope::owns_incl_base(int num) const {
   if(num < 0) num += size_incl_base() + 1;
   assert(num >= 1 && num <= size_incl_base());
   int th = size_incl_base() - size();
   if(num > th) {
      return owns(num - th);  // local symbol
   }
   else {
      return base()->owns_incl_base(num);
   }
}


// ==================================== scope::slot_offset

int scope::slot_offset(const scope *obj, int start) const {
   int i;
   if(start > 0) for(i = start; i <= size(); i++) {
      if(existing_slot(i) == obj) {
         return i;    // 1-based
      }
   }
   if(start < 0) for(i = start; -i <= size(); i--) {
      if(existing_slot(i) == obj) {
         return i;    // -1-based
      }
   }
   return 0; // indicates name was not found
}


// ==================================== scope::slot_offset_incl_base

int scope::slot_offset_incl_base(const scope *obj, int start) const {
   int i;
   if(start > 0) for(i = start; i <= size_incl_base(); i++) {
      if(existing_slot_incl_base(i) == obj) {
         return i;    // 1-based
      }
   }
   if(start < 0) for(i = start; -i <= size_incl_base(); i--) {
      if(existing_slot_incl_base(i) == obj) {
         return i;    // -1-based
      }
   }
   return 0; // indicates name was not found
}


// ==================================== scope::new_slot

scope *scope::new_slot(const string &n, int new_pos) {
   scope *p = new scope(n);
   return new_slot(p, new_pos);
}


scope *scope::new_slot(scope *obj, int new_pos) {
   assert(&obj != NULL);
   if(new_pos <= 0) new_pos += size() + 1;
   assert(new_pos >= 1 && new_pos <= size() + 1);

   _slot.insert(_slot.begin() + (new_pos - 1), obj);
   _owns.insert(_owns.begin() + (new_pos - 1), false);
   if(!obj->has_owner()) {
      // circular ownership will lead to infinite recursion.  if we
      // are to own this object, then it cannot own us, even indirectly
      assert(!has_owner_incl_base(obj));
      obj->set_owner(this);
   }

   assert(_slot.size() == _owns.size());
   return obj;
}


// ==================================== scope::slot

scope *scope::slot(const string &n, int new_pos) {
   int p = has_slot(n, (new_pos? new_pos: -1));
   if(p != 0) {
      return existing_slot(p);
   }
   return new_slot(n, new_pos);
}


scope *scope::slot(scope *obj, int new_pos) {
   int p = has_slot(obj, (new_pos? new_pos: -1));
   if(p != 0) {
      return existing_slot(p);
   }
   return new_slot(obj, new_pos);
}


// ==================================== scope::existing_slot

scope *scope::existing_slot(int num) const {
   if(num < 0) num += size() + 1;
   assert(num >= 1 && num <= size());

   return _slot[num - 1];
}


// ==================================== scope::existing_slot_incl_base

scope *scope::existing_slot_incl_base(int num) const {
   if(num < 0) num += size_incl_base() + 1;
   assert(num >= 1 && num <= size_incl_base());
   int th = size_incl_base() - size();
   if(num > th) {
      return existing_slot(num - th);  // local symbol
   }
   else {
      return base()->existing_slot_incl_base(num);
   }
}


// ==================================== scope::append_scope

scope *scope::append_scope(scope *s) {
   for(int i = 1; i <= s->size(); i++) {
      new_slot(s->existing_slot(i));
   }
   return this;
}


} // namespace std
