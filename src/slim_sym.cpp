/******************************************************************************
 * slim_sym.cpp                                                               *
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

#include <iostream>
using namespace std;

#include "slim_binary.h"
#include "slim_data.h"
#include "slim_error.h"

namespace juice {

// ==================================== slim_binary: check_visibility

sym_vis slim_binary::check_visibility(sym_vis vis) {
   switch((char)f.peek()) {
      case hiddenSym:
         f.get(); vis = hidden;
         break;
      case readOnlySym:
         f.get(); vis = read_only;
         break;
   }
   return vis;
}


// ==================================== slim_binary: check_leaf

bool slim_binary::check_leaf(void) {
   return((char)f.peek() == leafSym? f.get(), true: false);
}


// ==================================== slim_binary: append_attributes

void slim_binary::append_attributes(scope *s, sym_vis vis, bool /* leaf */) {
   switch(vis) {
      case hidden: s->slot(bi->slot("HIDDEN")); break;
      case read_only: s->slot(bi->slot("READONLY")); break;
      case visible: s->slot(bi->slot("VISIBLE")); break;
   }
   // if(leaf) s < bi->slot("LEAF");
}


// ==================================== slim_binary::PublicInterface

void slim_binary::PublicInterface(sym_vis vis) {
   // see if we have a valid header
   sb_byte = (sb_type)f.get();
   if(sb_byte != oberonMagic && sb_byte != oberonFileMap) {
       streampos beg(0L);
       throw slim_error(InvalidHeader, current_module->name, beg);
   }

   if (sb_byte == oberonFileMap) {
       f.get();
       char fname[FILEMAP_HEADER];
       f.read(fname, FILEMAP_HEADER - 2);
       current_module->slot("NAME IN HEADER")->slot(fname);
       if (f.get() != oberonMagic) {
           streampos sp(FILEMAP_HEADER);
           throw slim_error(InvalidHeader, current_module->name, sp);
       }
   }

   sb_hdr = (hdr_type)f.get();

   string binary_type = "UNKNOWN BINARY";
   switch (sb_hdr) {
   case x86Binary:
       binary_type = ("x86 binary");
       break;
   case macPPCBinary:
   case linuxPPCBinary:
       binary_type = ("PowerPC binary");
       break;
   case mac68kBinary:
       binary_type = ("68K binary");
       break;
   case slimBinary:
       binary_type = (SLIMBIN);
       break;
   }
#if 0
   string binary_type = (sb_hdr == slimBinary ? SLIMBIN : "Native binary");
#endif

   string name = current_module->slot("FILENAME")->existing_slot(1)->name;
   cout << "  " << fdir << name;
   unordered_map<string, string>::iterator t = moduleMap.find(capitalize(name));
   if (t != moduleMap.end()) {
       cout << " (" << t->second << ")";
   }

   cout << ": " << binary_type;
   if (current_module == &top) {  // indicate which is the top-most module
       cout << " <<<";
   }
   cout << endl;
   current_module->slot("TYPE")->slot(bi->slot(binary_type));

   append_attributes(curr, vis);

   slim_int tbl_size;
   f >> tbl_size;   // read in size of public symbol table
   SymbolTable(vis);  // read in symbol table

}


// ==================================== slim_binary: ModuleList

void slim_binary::ModuleList(sym_vis vis) {
   slim_str modname;
   while(true) {
      f >> modname;
      if(modname.val.empty()) break;  // no (more) modules
      // since the import relation is recursive, we may encounter the
      // names of certain modules more than once -- hence, to simplify,
      // all modules will be listed linearly under the "GLOBAL MODULE ENUM"
      // banner
      string m = modname.val;
      if(!top.slot("GLOBAL MODULE ENUM")->has_slot(m)) {
         scope *prev_curr = curr; // save current module addr
         scope *prev_mod = current_module;
         scope *prev_psym = psym;

         // keep the number of file handles used to a minimum
         streampos prev_offs = f.tellg();
         f.close();

         psym = curr = current_module = top.slot("GLOBAL MODULE ENUM")->slot(m);
         assert(top.slot("GLOBAL MODULE ENUM")->has_slot(m));
         prev_mod->slot("NAMESPACE")->slot(curr);
         prev_mod->slot("MODULE ENUM")->slot(curr);

         if(!open_module(current_module->name + ".Obj", 0)) {
             streampos beg(0L);
             throw slim_error(CannotOpenModule, current_module->name, beg);
         }
         // now read the public symbols from the imported module
         PublicInterface(vis);
         f.close();

         curr = prev_curr;  // restore and reopen previous module
         current_module = prev_mod;
         psym = prev_psym;
         if(!open_module(current_module->name + ".Obj", (int)prev_offs)) {
            throw slim_error(CannotOpenModule, current_module->name, prev_offs);
         }
      }
      else {
         // we already read this module in -- just insert a reference to it
         // in the current namespace
         scope *imported_mod = top.slot("GLOBAL MODULE ENUM")->slot(m);
         current_module->slot("NAMESPACE")->slot(imported_mod);
         current_module->slot("MODULE ENUM")->slot(imported_mod);
      }
   }
}


// ==================================== slim_binary: SymbolTable

void slim_binary::SymbolTable(sym_vis vis, bool get_modules) {

   // first, process names of MODULEs from which we are importing
   // symbols
    if (get_modules) {
        ModuleList(vis);
    }

   // the symbol table contains the following entities: constants, variables,
   // procedure variables, type aliases and new type definitions
   while(f.peek() != endSym) {
      streampos offs = f.tellg();
      switch(int sym_type = f.get()) {
         case constSym:
            while(f.peek() < constSym || f.peek() > endSym) {
               ConstDecl(vis);
            }
            break;
         case varSym:
            while(f.peek() < constSym || f.peek() > endSym) {
               int gtl = 1, ltl = 1;
               VarDecl(vis, gtl, ltl);
            }
            break;
         case lProcSym:
            while(f.peek() < constSym || f.peek() > endSym) {
               int gtl = 1, ltl = 1;
               ProcDecl(hidden, gtl, ltl);
            }
            break;
         case xProcSym:
         case cProcSym:
            while(f.peek() < constSym || f.peek() > endSym) {
               int gtl = 1, ltl = 1;
               ProcDecl(vis, gtl, ltl, (sym_type == cProcSym? code_proc: normal_proc));
            }
            break;
         case aliasSym:
            while(f.peek() < constSym || f.peek() > endSym) {
               TypeDecl(vis, true);
            }
            break;
         case typeSym:
            while(f.peek() < constSym || f.peek() > endSym) {
               TypeDecl(vis);
            }
            break;
         default: throw slim_error(InvalidSymTblSection, current_module->name, offs);
      }
   }
   f.get();
}


// ==================================== slim_binary: Type

// the following will find or create a type
scope *slim_binary::Type(sym_vis vis, int &gtl, int &ltl) {
   streampos type_offs = f.tellg();

   vis = check_visibility(vis);
   slim_int type_num; f >> type_num;
   int type_id = type_num.val;
   scope* b;

   // first, extract the appropriate type symbol from the namespace

   switch(type_id) {

      case noBaseType: return bi->slot("NO BASE TYPE");
      case boolSym: { b = bi->slot("BOOLEAN"); b->slot(bi->slot("INTEGRAL TYPE")); return b; }
      case charSym: { b = bi->slot("CHAR"); b->slot(bi->slot("INTEGRAL TYPE")); return b; }
      case shortIntSym: { b = bi->slot("SHORTINT"); b->slot(bi->slot("INTEGRAL TYPE")); return b; }
      case intSym: { b = bi->slot("INTEGER"); b->slot(bi->slot("INTEGRAL TYPE")); return b; }
      case longIntSym: { b = bi->slot("LONGINT"); b->slot(bi->slot("INTEGRAL TYPE")); return b; }
      case setSym: { b = bi->slot("SET"); b->slot(bi->slot("INTEGRAL TYPE")); return b; }
      case realSym: { b = bi->slot("REAL"); b->slot(bi->slot("REAL TYPE")); return b; }
      case longRealSym: { b = bi->slot("LONGREAL"); b->slot(bi->slot("REAL TYPE")); return b; }
      case stringSym: { b = bi->slot("STRING"); b->slot(bi->slot("ARRAY TYPE")); return b; }
      case noReturnType: return bi->slot("NO RETURN TYPE");
      case byteSym: { b = bi->slot("SYSTEM.BYTE"); b->slot(bi->slot("INTEGRAL TYPE")); return b; }
      case sysPtrSym: { b = bi->slot("SYSTEM.PTR"); b->slot(bi->slot("POINTER")); return b; }

      case arraySym:
      case dynArraySym: {
         type_offs = f.tellg();
         scope *array_sym = gtype->slot("ARRAY" + hex((long)type_offs, 0));
         //(*curr)("NAMESPACE") << *array_sym;
         // propagate type name to procedure/module level
         psym->slot("NAMESPACE")->slot(array_sym);

         scope *base_type = Type(vis, gtl, ltl);
         // we absolutely MUST have a type to point to!
         if (!base_type) {
             throw slim_error(InvalidBaseType, current_module->name, type_offs);
         }

         slim_str array_name; f >> array_name;
         if (!array_name.val.empty()) {
             array_sym->name = array_name.val;
         }
         else {
             vis = hidden;
         }

         // insert type into current namespace
         array_sym->slot(bi->slot("ARRAY"));
         array_sym->slot("TYPE")->slot(base_type);

         // static arrays also have the size
         if(type_id == arraySym) {
            slim_int arr_size; f >> arr_size;
            array_sym->slot("SIZE")->slot(str(arr_size.val));
         }

         // this will make it easier to print the Oberon program
         psym->slot("DECLARATIONS")->slot("TYPE")->slot(array_sym);
         append_attributes(array_sym, vis, false);

         return array_sym;
      }

      case pointerSym: {
         // The SYSTEM.PTR pointer type is coded as pointer to no type
         if((char)f.peek() == noBaseType) {
            f.get();
            scope *sys_ptr = bi->slot("PTR");
            sys_ptr->slot(bi->slot("POINTER"));
            sys_ptr->slot("TYPE")->slot(bi->slot("BYTE"));
            return sys_ptr;
         }
         type_offs = f.tellg();
         scope *pointer_sym = gtype->slot("POINTER" + hex((long)type_offs, 0));
         //(*curr)("NAMESPACE") << *pointer_sym;
         // propagate type name to procedure/module level
         psym->slot("NAMESPACE")->slot(pointer_sym);

         scope *pointee = Type(vis, gtl, ltl);
         // we absolutely MUST have a type to point to!
         if (!pointee) {
             throw slim_error(InvalidPointee, current_module->name, type_offs);
         }

         slim_str type_name; f >> type_name;
         if (!type_name.val.empty()) {
             pointer_sym->name = type_name.val;
         }
         else {
             vis = hidden;
         }

         // insert type into current namespace
         pointer_sym->slot(bi->slot("POINTER"));
         pointer_sym->slot("TYPE")->slot(pointee);
         // merge the namespaces of pointer and pointee
         // *pointer_sym < (*pointee)("NAMESPACE");

         // emit the conversion guards to be used by the code
         int new_gtl = 1, new_ltl = 1;
         bool c_gtl = (gtl != 0), c_ltl = (ltl != 0);

         // the enum for this record goes BEFORE the enumeration of its base type!
         // however, gtl and ltl still need to be adjusted at the end
         if(c_gtl) gtl -= new_gtl; if(c_ltl) ltl -= new_ltl;
         generate_sym_enum(pointer_sym, new_gtl, new_ltl);
         if(c_gtl) gtl += new_gtl; if(c_ltl) ltl += new_ltl;

         // this will make it easier to print the Oberon program
         psym->slot("DECLARATIONS")->slot("TYPE")->slot(pointer_sym);
         append_attributes(pointer_sym, vis, false);

         return pointer_sym;
      }

      case recordSym: {
         // this record may have a base type -- get it first
         type_offs = f.tellg();
         scope *record_sym = gtype->slot("RECORD" + hex((long)type_offs, 0));
         record_sym->slot(bi->slot("RECORD"));

         //(*curr)("NAMESPACE") << *record_sym;
         // propagate type name to module level
         psym->slot("NAMESPACE")->slot(record_sym);

         scope *base_type = Type(vis, gtl, ltl);
         // we absolutely MUST have a type to point to!
         if (!base_type) {
             throw slim_error(InvalidBaseType, current_module->name, type_offs);
         }
         // next, get the name of our record
         type_offs = f.tellg();
         slim_str type_name; f >> type_name;
         if (!type_name.val.empty()) {
             record_sym->name = type_name.val;
         }
         else {
             vis = hidden;
         }

         record_sym->slot("TYPE")->slot(base_type);

         // emit the conversion guards to be used by the code
         int new_gtl = 1, new_ltl = 1;
         bool c_gtl = (gtl != 0), c_ltl = (ltl != 0);

         // the enum for this record goes BEFORE the enumeration of its base type!
         // however, gtl and ltl still need to be adjusted at the end
         if (c_gtl) {
             gtl -= new_gtl;
         }
         if (c_ltl) {
             ltl -= new_ltl;
         }
         generate_sym_enum(record_sym, new_gtl, new_ltl);

         // now we shall process the nested declarations of the symbols
         base_type = curr;
         curr = record_sym;  // emit into the record, not outside of it
         while((char)f.peek() != endSym && (char)f.peek() != tProcSym) {
            // field access immediately follows type guard enumeration
            // (performed above)
            VarDecl(visible, new_gtl, new_ltl);
         }
         // The record definition also contains any member functions
         // (with the record OR the related pointer type as receivers)
         if((char)f.peek() == tProcSym) {
            f.get();
            while((char)f.peek() != endSym) {
               ProcDecl(vis, new_gtl, new_ltl);
            }
         }
         curr = base_type;   // restore
         f.get(); // endSym

         if (c_gtl) {
             gtl += new_gtl;
         }
         if (c_ltl) {
             ltl += new_ltl;
         }

         // this will make it easier to print the Oberon program
         psym->slot("DECLARATIONS")->slot("TYPE")->slot(record_sym);
         append_attributes(record_sym, vis, false);

         return record_sym;
      }

      case procVarSym: {
         type_offs = f.tellg();
         // emit the conversion guards to be used by the code
         // int new_gtl = 1, new_ltl = 1;
         scope *procvar_sym = ProcDecl(vis, gtl, ltl, proc_variable);
         psym->slot("NAMESPACE")->slot(procvar_sym);

         // propagate type name to procedure/module level
         psym->slot("DECLARATIONS")->slot("TYPE")->slot(procvar_sym);
         append_attributes(procvar_sym, vis, false);

         return procvar_sym;
      }

      case sysFlag: {
         // the type which follows is attributed with a system flag
         type_offs = f.tellg();
         slim_int flag_val;  f >> flag_val;
         scope *base_type = Type(vis, gtl, ltl);
         base_type->slot("SYSFLAG")->slot("["+str(flag_val.val)+"]");

         return base_type;
      }

      default: {
         if(type_id < 0) {
            // we can now look up the type in our type enumeration name space
            // (the symbol slots are -1-based -- i.e., the types added first are
            // at the END of the list)
            return gtype->existing_slot(-type_id);
         }

         else if(type_id >= module01 && type_id <= otherMod) {
            int mod_no;
            if(type_id < otherMod) {
               mod_no = type_id - module01 + 1;   // 1-based
            }
            else {
               type_offs = f.tellg();
               f >> type_num;  // the actual type # follows
               mod_no = type_num.val + 1; // TODO: check base
            }
            // NB: current_module->slot("MODULE ENUM") contains ONLY the modules that we
            // are importing DIRECTLY.  see top("GLOBAL MODULE ENUM") for
            // ALL modules.
            scope *mod_enum = current_module->slot("MODULE ENUM");
            if(mod_enum->size() < mod_no) {
               throw slim_error(InvalidModuleEnum, current_module->name, type_offs);
            }
            scope *mod_sym = mod_enum->existing_slot(mod_no);
            scope *imported_type = current_module->slot("IMPORTED TYPE ENUM")->slot(mod_sym->name);
            // now read in the name of the imported type
            slim_str type_name;
            type_offs = f.tellg();
            f >> type_name;
            if(type_name.val.empty()) {
               type_offs = f.tellg();
               f >> type_num; // name has been read previously, just get the #
               type_id = type_num.val;
               if(imported_type->size() < type_id) {
                  throw slim_error(InvalidImportedType, current_module->name, type_offs);
               }
            }
            else {
               // the name of this type SHOULD already exist in the module's
               // namespace, with or without the module name prepended.
               scope *new_type;
               if(mod_sym->slot("NAMESPACE")->has_slot(type_name.val)) {
                  new_type = mod_sym->slot("NAMESPACE")->slot(type_name.val);
                  if(current_module->name == top.name) {
                     new_type->name = mod_sym->name + "." + new_type->name;
                  }
                  new_type->slot("IMPORTED FROM")->slot(mod_sym->name);
               }
               else if(mod_sym->slot("NAMESPACE")->has_slot(mod_sym->name + "." + type_name.val)) {
                  new_type = mod_sym->slot("NAMESPACE")->slot(mod_sym->name + "." + type_name.val);
               }
               else {
                  throw slim_error(InvalidImportedType, current_module->name, type_offs);
               }

               // generate_sym_enum(&new_type, tl); // NOT HERE --> fingerprint section
               imported_type->slot(new_type);
               type_id = imported_type->size();
            }
            return imported_type->existing_slot(type_id);
         }
      } // default
   } // switch

   throw slim_error(InvalidType, current_module->name, type_offs);
   return NULL;
}


// ==================================== slim_binary: ConstDecl

scope *slim_binary::ConstDecl(sym_vis vis) {

   // visibility applies to the symbol, not its type
   vis = check_visibility(vis);
   int tl = 0;

   scope *type_sym = Type(vis, tl, tl);   // get type and name first

   streampos const_offs = f.tellg();
   scope *const_sym = psym->slot("NAMESPACE")->slot("CONST" + hex((long)const_offs, 0));
   slim_str const_name; f >> const_name;
   if (!const_name.val.empty()) {
       const_sym->name = const_name.val;
   }
   else {
       vis = hidden;
   }

   const_sym->slot(bi->slot("CONST"));
   const_sym->slot("TYPE")->slot(type_sym);
   append_attributes(const_sym, vis, false);

   scope *value_sym = new scope;
   if(type_sym->eq("BOOLEAN")) {
      value_sym->name = literal(booleanConst);
   }
   else if(type_sym->eq("CHAR")) {
      value_sym->name = literal(charConst);
   }
   else if(type_sym->eq("SET")) {
      value_sym->name = literal(setConst);
   }
   else if(type_sym->eq("INTEGER")) {
      value_sym->name = literal(intConst);
   }
   else if (type_sym->eq("LONGINT")) {
       value_sym->name = literal(longIntConst);
   }
   else if (type_sym->eq("SHORTINT")) {
       value_sym->name = literal(shortIntConst);
   }
   else if(type_sym->eq("REAL")) {
      value_sym->name = literal(realConst);
   }
   else if(type_sym->eq("LONGREAL")) {
      value_sym->name = literal(longRealConst);
   }
   else if(type_sym->eq("STRING")) {
      value_sym->name = literal(stringConst);
   }
   else {
      throw slim_error(InvalidConstType, current_module->name, const_offs);
   }

   // *gcode << *value_sym;
   const_sym->slot("VALUE")->slot(value_sym);
   int const_tl = (lcode == gcode? 1: 0);
   generate_sym_enum(const_sym, const_tl, const_tl);

   // this will make it easier to print the Oberon program
   psym->slot("DECLARATIONS")->slot("CONST")->slot(const_sym);

   return const_sym;
}


// ==================================== slim_binary: VarDecl

scope *slim_binary::VarDecl(sym_vis vis, int &gtl, int &ltl) {
   // visibility applies to the symbol, not its type
   vis = check_visibility(vis);
   bool leaf = check_leaf();
   streampos var_offs = f.tellg();
   scope *type_sym = Type(vis, gtl, ltl);
   // we absolutely MUST have a type to point to!
   if (!type_sym) {
       throw slim_error(InvalidBaseType, current_module->name, var_offs);
   }

   var_offs = f.tellg();
   scope *var_sym = curr->slot("NAMESPACE")->slot("VAR" + hex((long)var_offs, 0));

   slim_str var_name; f >> var_name;
   if (!var_name.val.empty()) {
       var_sym->name = var_name.val;
   }
   else {
       vis = hidden;
   }

   var_sym->slot(bi->slot("VAR"));
   var_sym->slot("TYPE")->slot(type_sym);

   // MOST variables get encoded on the enumeration stack in REVERSE ORDER, except
   // for procedure parameters!!!!!
   bool proc_parm = ((curr->has_slot("PROCEDURE") || curr->has_slot("PROCVAR"))
         && !curr->has_slot("ARITY"));

   // if this variable is merely a field, only one enumeration is needed
   if(!proc_parm && curr->has_slot("RECORD")) {
      var_sym->slot(bi->slot("FIELD"));
   }

   // insert the variable itself, as well as several useful variations (this
   // depends on the variable's type) to the atom enumeration
   generate_sym_enum(var_sym, gtl, ltl);

   // this will make it easier to print the Oberon program
   if(!proc_parm) {
      // symPUBLIC implies symREADABLE
      append_attributes(var_sym, vis, leaf);
      curr->slot("DECLARATIONS")->slot("VAR")->slot(var_sym);
   }

   return var_sym;
}


// ==================================== slim_binary: ProcDecl

scope *slim_binary::ProcDecl(sym_vis vis, int &gtl, int &ltl,
      proc_type proctype) {

   // visibility applies to the symbol, not its type
   vis = check_visibility(vis);
   bool leaf = check_leaf();
   scope *type_sym = Type(vis, gtl, ltl); // return type
   long proc_offs = (long)f.tellg();
   scope *proc_sym = psym->slot("NAMESPACE")->slot("PROCEDURE" + hex(proc_offs, 0));

   slim_str proc_name; f >> proc_name;
   if (!proc_name.val.empty()) {
       proc_sym->name = proc_name.val;
   }
   else {
       vis = hidden;
   }

   if(proctype == proc_variable) {
      gtype->new_slot(proc_sym);  // this is the name of the TYPE, not the VAR!
      proc_sym->set_owner(gtype);
      proc_sym->slot(bi->slot("PROCVAR"));
      proc_sym->slot("TYPE")->slot(type_sym);
   }
   else {
      proc_sym->slot(bi->slot("PROCEDURE"));
      if(proctype == code_proc) {
         proc_sym->slot(bi->slot("CODE PROC"));  // CProc
      }
      if(curr->has_slot("RECORD")) {
         proc_sym->slot(bi->slot("METHOD"));   // TProc
      }
      proc_sym->slot("TYPE")->slot(type_sym);  // return type
      generate_sym_enum(proc_sym, gtl, ltl);
   }
   // symPUBLIC implies symREADABLE
   append_attributes(proc_sym, vis, leaf);

   // now process the parameters for this procedure (variable type) -- these are
   // stored as a local symbol table
   scope *curr_scope = curr;
   scope *curr_lcode = lcode;

   // switch to a separate stack for local (i.e., PROCEDURE) code
   // enumerations
   lcode = proc_sym->slot("CODE ENUM");
   // the very first enumeration for PROCEDUREs is the RETURN statement.
   lcode->new_slot(bi->slot("RETURN"), 0);
   assert(lcode->has_slot("RETURN") == 1);

   // make enclosing enumerations accessible via the '..._incl_base' functions
   if(curr_lcode != gcode && proctype != proc_variable) {
      lcode->set_base(curr_lcode);
   }
   curr = proc_sym;

   // NB: note that the 'psym' variable still points to the ENCLOSING
   // scope -- this is where any new types should go...

   int arity = 0;
   while((char)f.peek() != endSym) {
      // here, varSym is used to denote reference parameters, NOT
      // variables in general!
      bool var_par = false;
      if((char) f.peek() == varSym) {
         var_par = true;
         f.get();
      }
      // PROCEDURE/PROCVAR parameters are all "hidden," i.e., do not
      // have '*' or '-' qualifiers! -- their types, however, are
      // still visible!!!
      int parm_tl = 0;
      scope *parm_sym = VarDecl(visible, gtl, parm_tl);
      // *parm_sym < bi->slot("PARAMETER");
      if(var_par) {
         parm_sym->slot(bi->slot("REFERENCE"));
      }

      // the first parameter of a TProc is the receiver object
      if(!arity && proc_sym->has_slot("METHOD")) {
         proc_sym->slot("RECEIVER")->slot(parm_sym);
      }
      else {
         proc_sym->slot("PARAMETERS")->slot(parm_sym);
      }

      arity++;
   }
   f.get();

   curr = curr_scope;   // restore parent scope
   lcode = curr_lcode;

   proc_sym->slot("ARITY")->slot(str(arity));

   // Declarations for code procedures are immediately followed by the
   // machine code sequence.
   if(proctype == code_proc) {
     slim_int code_size; f >> code_size;
     string code_sequence;
     for(int i = 0; i < code_size.val; i++) {
       int opcode = f.get();
       if(i) {
         code_sequence += ", ";
       }
       code_sequence += (hex(opcode, 3) + "H");
     }
     proc_sym->slot("CODE SEQUENCE")->slot(code_sequence);
   }

   // this will make it easier to print the Oberon program
   // (procedure variable TYPES will get a separate name in VarDecl)
   if(proctype != proc_variable) {
      // NB: More than 1 proc may have the same name! (overriden methods)
      psym->slot("DECLARATIONS")->slot("PROCEDURE")->new_slot(proc_sym);
   }

   return proc_sym;
}


// ==================================== slim_binary: TypeDecl

scope *slim_binary::TypeDecl(sym_vis vis, bool type_alias) {

   vis = check_visibility(vis);
   bool leaf = check_leaf();
   streampos var_offs = f.tellg();
   // the Type routine will retrieve the name of the type also!
   int gtl = 1, ltl = 1;
   scope *type_sym = Type(vis, gtl, ltl);
   // we absolutely MUST have a type to point to!
   if (!type_sym) {
       throw slim_error(InvalidBaseType, current_module->name, var_offs);
   }

   if(type_alias) {
      long alias_offs = (long)f.tellg();
      scope *alias_sym = psym->slot("NAMESPACE")->slot("TYPE" + hex(alias_offs, 0));
      // propagate type name to module level
      // psym->slot("NAMESPACE")->slot(*alias_sym;

      slim_str alias_name; f >> alias_name;
      if(!alias_name.val.empty()) alias_sym->name = alias_name.val;
      else vis = hidden;

      alias_sym->slot(bi->slot("ALIAS"));
      alias_sym->slot("TYPE")->slot(type_sym);
      type_sym = alias_sym;
   }
   append_attributes(type_sym, vis, leaf);

   // this will make it easier to print the Oberon program
   psym->slot("DECLARATIONS")->slot("TYPE")->slot(type_sym);

   return type_sym;
}


// ==================================== slim_binary: generate_sym_enum

void slim_binary::generate_sym_enum(scope *enum_sym, int &gtl, int &ltl,
      const string &module_name, bool mandatory) {

   // rename imported symbols "in-place" to include the name of the module
   // if(enum_sym->has_slot("FINGERPRINT") && !enum_sym->has_slot("FIELD")) {
   //    // only top-level imported symbols will be prepended with the module
   //    // name
   //    if(enum_sym->has_slot("IMPORTED FROM")) {
   //       enum_sym->name = (*enum_sym)("IMPORTED FROM")->existing_slot(1)->name + "." + enum_sym->name;
   //    }
   // }

   // if tl is 0, means append at end of enum.  otherwise, insert at position
   // indicated and increment

   // procedures and variables will be emitted into local scope.  make sure the
   // RETURN statement is by-passed.

   if(ltl && lcode->has_slot("RETURN")) {
      assert(lcode->has_slot("RETURN") == 1);
      ltl++;       // leave the return statement intact
   }

   // make sure we are incrementing the appropriate counter!
   int *tl = (lcode == gcode? &gtl: &ltl);

   // first, process the types, which always go into the global enumeration
   if(enum_sym->has_slot("POINTER") || enum_sym->has_slot("RECORD") /* || enum_sym->has_slot("ARRAY")*/) {
      // imported types need not be enumerated unless specifically requested
      if(!mandatory && enum_sym->has_slot("ENUM DONE")) {
          return;
      }

      if(!module_name.empty()) {
         // if this type comes from ANOTHER imported module, we
         // not want to enumerate it here only if it has not been
         // enumerated already within its own module
         const scope *imp_module = enum_sym->owner()->owner();
         if(imp_module->name != module_name) {
            bool mod_fp = (top.slot("GLOBAL MODULE ENUM")->slot(imp_module->name)->has_slot("FINGERPRINT") != 0);
            // which imported types do or don't get enumerated is anyone's guess...
            if(enum_sym->has_slot("RECORD")) {
               return;
            }
         }
         if((enum_sym->name.find('.')) == -1) {
            enum_sym->name = imp_module->name
                  + "." + enum_sym->name;
         }
         if(imp_module->name == module_name && enum_sym->has_slot("RECORD")) {
            enum_sym->slot(bi->slot("ENUM DONE"));
         }
      }
      scope *ce = (code_gen_phase? lcode: gcode);
      int *cl = (code_gen_phase? &ltl: &gtl);
      ce->new_slot("(" + enum_sym->name + ")", (*cl? (*cl)++: 0))->slot(bi->slot("RIGHT"));
      ce->new_slot("(" + enum_sym->name + ")", (*cl? (*cl)++: 0))->slot(bi->slot("RIGHT"));
      scope* is = ce->new_slot(" IS " + enum_sym->name, (*cl ? (*cl)++ : 0));
      is->slot(bi->slot("RIGHT"));
      is->slot(bi->slot("ADD PARENTH"));
   }

   else if(enum_sym->has_slot("CONST")) {
      // what Oberon expects here is NOT the constant name, but
      // rather its VALUE.
      assert(enum_sym->has_slot("VALUE") && enum_sym->has_slot("TYPE"));
      scope *const_type = enum_sym->slot("TYPE");

      // constants have only one enumeration, regardless of type; Boolean constants are
      // not enumerated at all.
      if(!const_type->has_slot("BOOLEAN")) {
        scope *const_val = enum_sym->slot("VALUE")->existing_slot(1);
        const_val->slot(const_type);
        gcode->new_slot(const_val, (gtl? gtl++: 0));
      }
   }

   else if(enum_sym->has_slot("FIELD")) {
      scope *ce = (code_gen_phase? lcode: gcode);
      int *cl = (code_gen_phase? &ltl: &gtl);
      // fields have only one enumeration, regardless of type
      scope* s = ce->new_slot("." + enum_sym->name, (*cl ? (*cl)++ : 0));
      s->slot(enum_sym->slot("TYPE"));
      s->slot(bi->slot("RIGHT"));
   }

   else if(enum_sym->has_slot("PROCEDURE")) {
      if(!module_name.empty() && !enum_sym->has_slot("ENUM DONE")) {
         enum_sym->name = module_name + "." + enum_sym->name;
         enum_sym->slot(bi->slot("ENUM DONE"));
      }
      lcode->new_slot("(" + enum_sym->name + ")", (*tl? (*tl)++: 0));
      lcode->new_slot(enum_sym, (*tl? (*tl)++: 0)); // < bi->slot("LEFT");
      // TProcs seem to be inserted twice here... time will tell
      if(enum_sym->has_slot("METHOD")) {
         lcode->new_slot(enum_sym, (*tl? (*tl)++: 0)); // < bi->slot("LEFT");
      }
      // imported procedures need not be defined!
      if(!enum_sym->has_slot("FINGERPRINT")) {
         lcode->new_slot("PROCEDURE " + enum_sym->name, (*tl? (*tl)++: 0));
      }
   }

   // the rest will be variables

   else if(enum_sym->has_slot("VAR")) {
      if(!module_name.empty() && !enum_sym->has_slot("ENUM DONE")) {
         enum_sym->name = module_name + "." + enum_sym->name;
         enum_sym->slot(bi->slot("ENUM DONE"));
      }
      // check the type of the variable
      scope *type_sym = enum_sym->slot("TYPE")->existing_slot(1);

      // take care of the simple types first
      if(type_sym->has_slot("INTEGRAL TYPE") || type_sym->has_slot("REAL TYPE")) {
         lcode->new_slot(enum_sym, (*tl? (*tl)++: 0));  // at the BEGINNING (1-based)!
         if(!enum_sym->has_slot("FINGERPRINT")) {
            lcode->new_slot(" := " + enum_sym->name, (*tl? (*tl)++: 0))->slot(bi->slot("RIGHT"));
            lcode->new_slot(enum_sym->name + " := ", (*tl? (*tl)++: 0))->slot(bi->slot("LEFT"));
         }
      }
      else {
         // all composite types will have a name (assigned either by the user or
         // by the SlimBinaryReader (i.e., RECORD@001B).  the underlying type
         // is stored in the "TYPE" slot.
         while(type_sym->has_slot("ALIAS")) {
            type_sym = type_sym->slot("TYPE")->existing_slot(1);
         }

         if(type_sym->has_slot("RECORD") || type_sym->has_slot("PROCVAR")) {
            lcode->new_slot(enum_sym, (*tl? (*tl)++: 0));  // at the BEGINNING (1-based)!
            if(!enum_sym->has_slot("FINGERPRINT")) {
               lcode->new_slot(" := " + enum_sym->name, (*tl? (*tl)++: 0))->slot(bi->slot("RIGHT"));
               lcode->new_slot(enum_sym->name + " := ", (*tl? (*tl)++: 0))->slot(bi->slot("LEFT"));
            }
         }
         else if(type_sym->has_slot("POINTER")) {
            lcode->new_slot(enum_sym, (*tl? (*tl)++: 0));  // at the BEGINNING (1-based)!
            lcode->new_slot(" := " + enum_sym->name, (*tl? (*tl)++: 0))->slot(bi->slot("RIGHT"));
            lcode->new_slot(enum_sym->name + " := ", (*tl? (*tl)++: 0))->slot(bi->slot("LEFT"));
            lcode->new_slot(enum_sym->name + "^", (*tl? (*tl)++: 0))->slot(type_sym->slot("TYPE"));
         }
         else if(type_sym->has_slot("ARRAY")) {
            lcode->new_slot(enum_sym, (*tl? (*tl)++: 0));  // at the BEGINNING (1-based)!
            if(!enum_sym->has_slot("FINGERPRINT")) {
               lcode->new_slot(" := " + enum_sym->name, (*tl? (*tl)++: 0))->slot(bi->slot("RIGHT"));
               lcode->new_slot(enum_sym->name + " := ", (*tl? (*tl)++: 0))->slot(bi->slot("LEFT"));
               scope* s = lcode->new_slot(enum_sym->name + "[", (*tl ? (*tl)++ : 0));
               s->slot(bi->slot("ARRAY INDEX"));
               s->slot(type_sym->slot("TYPE"));
            }
         }
      }
   }

   if(ltl && lcode->has_slot("RETURN")) {
      assert(lcode->has_slot("RETURN") == 1);
      ltl--;
   }
}


// ==================================== slim_binary: ObjectKey

void slim_binary::ObjectKey(scope *curr_scope, int &gtl, int &ltl,
      const string &module_name) {

   streampos ent_offs = f.tellg();

   // if recordEu occurs at the very beginning of a scope, it
   // denotes the opening of a sub-class scope
   if(f.peek() == recordEu) {
      if(!curr_scope->has_slot("TYPE")) {
         throw slim_error(MissingBaseType, current_module->name, ent_offs);
      }
      f.get();
      scope *base_type = curr_scope->slot("TYPE")->existing_slot(1);
      generate_sym_enum(base_type, gtl, ltl, module_name, true);
      ObjectKey(base_type, gtl, ltl, module_name);
   }

   while(f.peek() != endEu) {
      slim_int fingerprint;  f >> fingerprint;
      ent_offs = f.tellg();
      slim_str entityname;  f >> entityname;

      // the identifier must have occurred previously in the symbol table
      scope *entity_sym;
      if(curr_scope->slot("NAMESPACE")->has_slot(entityname.val)) {
         entity_sym = curr_scope->slot("NAMESPACE")->slot(entityname.val);
         entity_sym->slot("IMPORTED FROM")->slot(module_name);
      }
      else if(curr_scope->slot("NAMESPACE")->has_slot(module_name + "." + entityname.val)) {
         entity_sym = curr_scope->slot("NAMESPACE")->slot(module_name + "." + entityname.val);
      }
      else {
         throw slim_error(InvalidImportedType, current_module->name, ent_offs);
      }

      entity_sym->slot("FINGERPRINT")->slot(str(fingerprint.val));
      generate_sym_enum(entity_sym, gtl, ltl, module_name, false);
      // if this was a record or pointer variable, enumerate the
      // underlying type also
      bool is_global_var = false;
      if(entity_sym->has_slot("VAR")) {
         is_global_var = (entity_sym->has_slot("FIELD") == 0);
         entity_sym = entity_sym->slot("TYPE")->existing_slot(1);
         // handle the enumeration of imported array variables
         if(is_global_var && !entity_sym->has_slot("FINGERPRINT") && entity_sym->has_slot("POINTER")
               && entity_sym->slot("TYPE")->existing_slot(1)->has_slot("ARRAY")) {
            generate_sym_enum(entity_sym, gtl, ltl, module_name, true);
            if((char)f.peek() == recordEu) {
               generate_sym_enum(entity_sym, gtl, ltl, module_name, true);
               f.get();
               assert((char)f.get() == endEu);  // arrays should have no field elaboration
            }
         }
         // enumerate imported record and pointer types
         if((!is_global_var || (char)f.peek() == recordEu)
               && (entity_sym->has_slot("RECORD") || entity_sym->has_slot("POINTER"))) {
            // if we are about to enumerate fields, print record type, not pointer type
            if(is_global_var && (char)f.peek() == recordEu
                  && entity_sym->has_slot("POINTER")) {
               entity_sym = entity_sym->slot("TYPE")->existing_slot(1);
            }
            generate_sym_enum(entity_sym, gtl, ltl, module_name, true);
            if(is_global_var) {
               entity_sym->slot(bi->slot("ENUM DONE"));
            }
         }
      }

      // we may need to process nested structures/type declarations
      if((char)f.peek() == recordEu) {
         f.get();
         // eliminate indirection
         bool record_at_end = false;
         if(entity_sym->has_slot("POINTER")) {
            entity_sym = entity_sym->slot("TYPE")->existing_slot(1);
            record_at_end = true;
            if(!is_global_var) {
               generate_sym_enum(entity_sym, gtl, ltl, module_name, true);
            }
         }
         ObjectKey(entity_sym, gtl, ltl, module_name);
         // for global imported variables (e.g., Objects.NewObj),
         // the enumeration for the base record type comes AFTER
         // the enumeration for the fields!
         if(is_global_var && record_at_end) {
            generate_sym_enum(entity_sym, gtl, ltl, module_name, true);
         }
      }
   }
   f.get();
}


void slim_binary::ImportedKeys(void) {
   long offs = (long)f.tellg();

   /* DEBUG */ // throw slim_error(UnknownError, current_module->name, offs);

   // the symbol table contains the following entities: constants, variables,
   // procedure variables, type aliases and new type definitions
   while(f.peek() != endEu) {
      slim_str modname; f >> modname;
      // locate the desired module and the fingerprint enumeration within it
      int gtl = 0, ltl = 0;  // append imported symbols at end of enumeration!
      scope *fp_module = top.slot("GLOBAL MODULE ENUM")->slot(modname.val);
      ObjectKey(fp_module, gtl, ltl, modname.val);
      fp_module->slot(bi->slot("FINGERPRINT"));  // indicate completion of fingerprinting
   }
   f.get();
}


} // namespace juice
