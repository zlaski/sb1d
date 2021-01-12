/******************************************************************************
 * slim_print.cpp                                                             *
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

#include "slim_binary.h"
#include "slim_data.h"
#include "slim_error.h"
using namespace juice;

#include "exception.h"
using namespace std;

namespace juice {

static const int indent_step = 2;

void slim_binary::pretty_print(ostream &out) {
   out << "MODULE " << top.name << ";    (* " << top.slot("TYPE")->existing_slot(2)->name << " *)\n\n";


   if(top.has_slot("MODULE ENUM")) {
      ppImport(top.slot("MODULE ENUM"), out, indent_step);
   }

   if(top.has_slot("DECLARATIONS")) {
      ppDeclarations(top.slot("DECLARATIONS"), out, indent_step);
   }

   if(top.has_slot("BLOCK")) {
      out << "BEGIN  (* MODULE " << top.name << " *)\n";
      ppBlock(top.slot("BLOCK"), out, indent_step);
   }

   out << "END " << top.name << ".\n";
}


void slim_binary::ppImport(scope *sc, ostream &out, int indent) {

   out << string(indent, ' ') << "IMPORT\n"
         << string(indent + indent_step, ' ');
   int s = sc->size();
   for(int i = 1; i <= s; i++) {
      out << sc->existing_slot(i)->name << (i < s? ", ": ";\n");
   }
   out << endl;
}


void slim_binary::ppDeclarations(scope *sc, ostream &out, int indent) {

   if(sc->has_slot("CONST")) {
      out << string(indent, ' ') << "CONST\n";
      ppConst(sc->slot("CONST"), out, indent + indent_step);
      out << endl;
   }

   if(sc->has_slot("TYPE")) {
      out << string(indent, ' ') << "TYPE\n";
      ppType(sc->slot("TYPE"), out, indent + indent_step);
      out << endl;
   }

   if(sc->has_slot("VAR")) {
      out << string(indent, ' ') << "VAR\n";
      ppVar(sc->slot("VAR"), out, indent + indent_step);
      out << endl;
   }

   if(sc->has_slot("PROCEDURE")) {
      ppProcedure(sc->slot("PROCEDURE"), out, indent);
      out << endl;
   }

}


void slim_binary::ppConst(scope *sc, ostream &out, int indent) {

   for(int i = sc->size(); i >= 1; i--) {
      scope *const_sym = sc->existing_slot(i);
      out << string(indent, ' ') << ppIdent(const_sym) << " = "
          << const_sym->slot("VALUE")->existing_slot(1)->name << ";\n";
   }
}


void slim_binary::ppType(scope *sc, ostream &out, int indent) {

   for(int i = 1; i <= sc->size(); i++) {
      // type dependencies must be taken into account here
      ppType1(sc->existing_slot(i), out, indent);
   }
}


void slim_binary::ppType1(scope *sc, ostream &out, int indent) {

   // do not print (1) simple types, (2) types which have already
   // been printed, or (3) imported types
   int dot_pos = sc->name.find('.');
   int has_type = sc->has_slot("TYPE");
   int already_printed = sc->has_slot("PRINTED");
   if(!has_type || already_printed || (dot_pos >= 0)) {
      return;
   }
   sc->slot(bi->slot("PRINTED"));
   scope *base_sc = sc->slot("TYPE")->existing_slot(1);

   if(sc->has_slot("POINTER")) {
      // NB: do NOT check if the base type is enumerated!
      out << string(indent, ' ') << ppIdent(sc) << " = POINTER";
      if(sc->has_slot("SYSFLAG")) {
        out << sc->slot("SYSFLAG")->existing_slot(1)->name;
      }
      out << " TO " << base_sc->name << ";\n";
      return;
   }

   if(sc->has_slot("RECORD")) {
      // make sure the base type is enumerated as well!
      ppType1(base_sc, out, indent);
      // the member variables all have types also!
      if(sc->has_slot("DECLARATIONS")) {
         if(sc->slot("DECLARATIONS")->has_slot("VAR")) {
            for(int i = 1; i <= sc->slot("DECLARATIONS")->slot("VAR")->size(); i++) {
               assert(sc->slot("DECLARATIONS")->slot("VAR")->existing_slot(i)->has_slot("TYPE"));
               ppType1(sc->slot("DECLARATIONS")->slot("VAR")->existing_slot(i)
                   ->slot("TYPE")->existing_slot(1), out, indent);
            }
         }
      }

      out << string(indent, ' ') << ppIdent(sc) << " = RECORD";
      if(sc->has_slot("SYSFLAG")) {
        out << sc->slot("SYSFLAG")->existing_slot(1)->name;
      }
      if(base_sc->has_slot("TYPE")) {
         out << "(" << base_sc->name << ")";
      }
      out << "\n";
      if(sc->has_slot("DECLARATIONS")) {
         if(sc->slot("DECLARATIONS")->has_slot("VAR")) {
            ppVar(sc->slot("DECLARATIONS")->slot("VAR"), out, indent + indent_step);
         }
      }
      out << string(indent, ' ') << "END;\n";

      return;
   }

   if(sc->has_slot("ARRAY")) {
      // make sure the return type is enumerated as well!
      ppType1(base_sc, out, indent);
      out << string(indent, ' ') << ppIdent(sc) << " = ARRAY ";
      if(sc->has_slot("SIZE")) {
         out << sc->slot("SIZE")->existing_slot(1)->name << " ";
      }
      out << "OF " << base_sc->name << ";\n";
      return;
   }

   if(sc->has_slot("PROCVAR")) {
      // make sure the base type is enumerated as well!
      ppType1(base_sc, out, indent);
      // the proc parameters all have types also!
      if(sc->has_slot("PARAMETERS")) {
         for(int i = 1; i <= sc->slot("PARAMETERS")->size(); i++) {
            assert(sc->slot("PARAMETERS")->existing_slot(i)->has_slot("TYPE"));
            ppType1(sc->slot("PARAMETERS")->existing_slot(i)->slot("TYPE")
                ->existing_slot(1), out, indent);
         }
      }

      out << string(indent, ' ') << ppIdent(sc) << " = ";
      ppProcSignature(sc, out, false);
      return;
   }

   // default action -- print the type name verbatim
   out << string(indent, ' ') << ppIdent(sc) << " = " << base_sc->name << ";\n";
   return;
}


void slim_binary::ppVar(scope *sc, ostream &out, int indent, bool new_line) {

   for(int i = 1; i <= sc->size(); i++) {
      if(new_line) out << string(indent, ' ');
      ppVar1(sc->existing_slot(i), out);
      if(new_line || i < sc->size()) {
         out << ";" << (new_line? "\n": " ");
      }
   }
}


void slim_binary::ppVar1(scope *sc, ostream &out) {

   // indentation, if any, is done by ppVar above
   scope *base_sc = sc->slot("TYPE")->existing_slot(1);
   if(sc->has_slot("REFERENCE")) out << "VAR ";
   out << ppIdent(sc) << ": " << base_sc->name;
}


void slim_binary::ppProcedure(scope *sc, ostream &out, int indent) {

   // print forward declarations first
   int i;
   if(sc->size() >= 2) {
      out << string(indent, ' ') << "(* Forward PROCEDURE declarations *)\n\n";
      for(i = sc->size(); i >= 1; i--) {
         out << string(indent, ' ');
         ppProcSignature(sc->existing_slot(i), out, true, true);
      }
   }
   for(i = sc->size(); i >= 1; i--) {
      ppProcedure1(sc->existing_slot(i), out, indent);
   }
}


void slim_binary::ppProcedure1(scope *sc, ostream &out, int indent) {

   out << "\n" << string(indent, ' ');
   ppProcSignature(sc, out, true);
   out << "\n";

   if(sc->has_slot("DECLARATIONS")) {
      ppDeclarations(sc->slot("DECLARATIONS"), out, indent + indent_step);
   }

   if(sc->has_slot("BLOCK")) {
      out << string(indent, ' ') << "BEGIN  (* PROCEDURE "
            << sc->name << " *)\n";
      ppBlock(sc->slot("BLOCK"), out, indent + indent_step);
   }

   out << string(indent, ' ') << "END " << sc->name << ";\n\n";
}


void slim_binary::ppBlock(scope *sc, ostream &out, int indent) {
   for(int i = 1; i <= sc->size(); i++) {
      // type dependencies must be taken into account here
      ppStatement(sc->existing_slot(i), out, indent);
   }
}


void slim_binary::ppStatement(scope *sc, ostream &out, int indent) {

   // handle more complex statements first...
   if(sc->name == "IF") {
      ppIFStatement(sc, out, indent);
      return;
   }
   if(sc->name == "WITH") {
      ppWITHStatement(sc, out, indent);
      return;
   }
   if(sc->name == "CASE") {
      ppCASEStatement(sc, out, indent);
      return;
   }
   if(sc->name == "WHILE") {
      ppWHILEStatement(sc, out, indent);
      return;
   }
   if(sc->name == "REPEAT") {
      ppREPEATStatement(sc, out, indent);
      return;
   }
   if(sc->name == "LOOP") {
      ppLOOPStatement(sc, out, indent);
      return;
   }

   // default action -- print the statement verbatim
   out << string(indent, ' ') << sc->name << ";\n";
   return;
}


void slim_binary::ppIFStatement(scope *sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc->name;
   if(sc->name == "IF" || sc->name == "ELSIF") {
      out << " " << sc->slot("CONDITION")->existing_slot(1)->name << " THEN";
   }
   out << endl;

   if(sc->has_slot("BLOCK")) {
      ppBlock(sc->slot("BLOCK"), out, indent + indent_step);
   }
   if(sc->has_slot("ELSIF")) {
      ppIFStatement(sc->slot("ELSIF"), out, indent);
   }
   else if(sc->has_slot("ELSE")) {
      ppIFStatement(sc->slot("ELSE"), out, indent);
   }
   else {
      out << string(indent, ' ') << "END;  (* IF *)\n";
   }
}


void slim_binary::ppWITHStatement(scope *sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc->name;
   if(sc->name == "WITH" || sc->name == "|") {
      out << " " << sc->slot("CONDITION")->existing_slot(1)->name << " DO";
   }
   out << endl;

   if(sc->has_slot("BLOCK")) {
      ppBlock(sc->slot("BLOCK"), out, indent + indent_step);
   }
   if(sc->has_slot("|")) {
      ppWITHStatement(sc->slot("|"), out, indent);
   }
   else if(sc->has_slot("ELSE")) {
      ppWITHStatement(sc->slot("ELSE"), out, indent);
   }
   else {
      out << string(indent, ' ') << "END;  (* WITH *)\n";
   }
}


void slim_binary::ppCASEStatement(scope *sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc->name;
   if(sc->name == "CASE") {
      out << " " << sc->slot("EXPRESSION")->existing_slot(1)->name << " OF" << endl;
   }
   else if(sc->name == "|") {
      out << " " << sc->slot("CONDITION")->existing_slot(1)->name << ":" << endl;
   }
   if(sc->has_slot("BLOCK")) {
      ppBlock(sc->slot("BLOCK"), out, indent + indent_step);
   }
   if(sc->has_slot("|")) {
      ppCASEStatement(sc->slot("|"), out, indent);
   }
   else if(sc->has_slot("ELSE")) {
      ppCASEStatement(sc->slot("ELSE"), out, indent);
   }
   else {
      out << string(indent, ' ') << "END;  (* CASE *)\n";
   }
}


void slim_binary::ppWHILEStatement(scope *sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc->name;
   out << " " << sc->slot("CONDITION")->existing_slot(1)->name << " DO\n";

   if(sc->has_slot("BLOCK")) {
      ppBlock(sc->slot("BLOCK"), out, indent + indent_step);
   }
   out << string(indent, ' ') << "END;  (* WHILE *)\n";
}


void slim_binary::ppREPEATStatement(scope *sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc->name << endl;

   if(sc->has_slot("BLOCK")) {
      ppBlock(sc->slot("BLOCK"), out, indent + indent_step);
   }
   out << string(indent, ' ') << "UNTIL "
         << sc->slot("UNTIL")->existing_slot(1)->name << ";\n";
}


void slim_binary::ppLOOPStatement(scope *sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc->name << endl;

   if(sc->has_slot("BLOCK")) {
      ppBlock(sc->slot("BLOCK"), out, indent + indent_step);
   }
   out << string(indent, ' ') << "END;  (* LOOP *)\n";
}


const string &slim_binary::ppIdent(scope *id) {
   static string ident;
   ident = id->name;
   if (id->has_slot("VISIBLE")) {
       ident += "*";
   }
   else if (id->has_slot("READONLY")) {
       ident += "-";
   }
   return ident;
}


void slim_binary::ppProcSignature(scope *sc, ostream &out, bool print_name,
      bool print_forward) {

   // Code procedures (CProc) are emitted only in the forward section!
   if(sc->has_slot("CODE PROC") && !print_forward) {
     return;
   }

   scope *base_sc = sc->slot("TYPE")->existing_slot(1);
   out << "PROCEDURE";
   if(print_forward) out << (sc->has_slot("CODE PROC")? "-": "^");

   if(sc->has_slot("RECEIVER")) {
      out << "(";
      ppVar(sc->slot("RECEIVER"), out, 0, false);
      out << ")";
   }

   if(print_name) {
      out << " " << ppIdent(sc);
   }

   out << "(";  // print parenths even if no parameters exist
   if(sc->has_slot("PARAMETERS")) {
      ppVar(sc->slot("PARAMETERS"), out, 0, false);
   }
   out << ")";
   if(base_sc->ne("NO RETURN TYPE")) {
      out << ": " << base_sc->name;
   }
   if(sc->has_slot("CODE PROC")) {  // print machine code for CProcs
     out << " " << sc->slot("CODE SEQUENCE")->existing_slot(1)->name;
   }
   out << ";\n";
}


} // namespace juice
