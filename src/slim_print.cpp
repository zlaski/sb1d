/******************************************************************************
 * slim_print.cpp                                                             *
 *                                                                            *
 * The SlimBinary(tm) Decoder                                                 *
 *                                                                            *
 * Copyright (c) 1997-1999 by the Regents of the University of California     *
 * Copyright (c) 2000-2020 by Ziemowit Laski                                  *
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
   out << "MODULE " << top.name << ";    (* " << top("TYPE")(2).name << " *)\n\n";


   if(top.has("MODULE ENUM")) {
      ppImport(top("MODULE ENUM"), out, indent_step);
   }

   if(top.has("DECLARATIONS")) {
      ppDeclarations(top("DECLARATIONS"), out, indent_step);
   }

   if(top.has("BLOCK")) {
      out << "BEGIN  (* MODULE " << top.name << " *)\n";
      ppBlock(top("BLOCK"), out, indent_step);
   }

   out << "END " << top.name << ".\n";
}


void slim_binary::ppImport(scope &sc, ostream &out, int indent) {

   out << string(indent, ' ') << "IMPORT\n"
         << string(indent + indent_step, ' ');
   int s = sc.size();
   for(int i = 1; i <= s; i++) {
      out << sc(i).name << (i < s? ", ": ";\n");
   }
   out << endl;
}


void slim_binary::ppDeclarations(scope &sc, ostream &out, int indent) {

   if(sc.has("CONST")) {
      out << string(indent, ' ') << "CONST\n";
      ppConst(sc("CONST"), out, indent + indent_step);
      out << endl;
   }

   if(sc.has("TYPE")) {
      out << string(indent, ' ') << "TYPE\n";
      ppType(sc("TYPE"), out, indent + indent_step);
      out << endl;
   }

   if(sc.has("VAR")) {
      out << string(indent, ' ') << "VAR\n";
      ppVar(sc("VAR"), out, indent + indent_step);
      out << endl;
   }

   if(sc.has("PROCEDURE")) {
      ppProcedure(sc("PROCEDURE"), out, indent);
      out << endl;
   }

}


void slim_binary::ppConst(scope &sc, ostream &out, int indent) {

   for(int i = sc.size(); i >= 1; i--) {
      scope &const_sym = sc(i);
      out << string(indent, ' ') << ppIdent(const_sym) << " = " << const_sym("VALUE")(1).name << ";\n";
   }
}


void slim_binary::ppType(scope &sc, ostream &out, int indent) {

   for(int i = 1; i <= sc.size(); i++) {
      // type dependencies must be taken into account here
      ppType1(sc(i), out, indent);
   }
}


void slim_binary::ppType1(scope &sc, ostream &out, int indent) {

   // do not print (1) simple types, (2) types which have already
   // been printed, or (3) imported types
   int dot_pos = sc.name.find('.');
   int has_type = sc.has("TYPE");
   int already_printed = sc.has("PRINTED");
   if(!has_type || already_printed || (dot_pos >= 0)) {
      return;
   }
   sc < (*bi)("PRINTED");
   scope &base_sc = sc("TYPE")(1);

   if(sc.has("POINTER")) {
      // NB: do NOT check if the base type is enumerated!
      out << string(indent, ' ') << ppIdent(sc) << " = POINTER";
      if(sc.has("SYSFLAG")) {
        out << sc("SYSFLAG")(1).name;
      }
      out << " TO " << base_sc.name << ";\n";
      return;
   }

   if(sc.has("RECORD")) {
      // make sure the base type is enumerated as well!
      ppType1(base_sc, out, indent);
      // the member variables all have types also!
      if(sc.has("DECLARATIONS")) {
         if(sc("DECLARATIONS").has("VAR")) {
            for(int i = 1; i <= sc("DECLARATIONS")("VAR").size(); i++) {
               assert(sc("DECLARATIONS")("VAR")(i).has("TYPE"));
               ppType1(sc("DECLARATIONS")("VAR")(i)("TYPE")(1), out, indent);
            }
         }
      }

      out << string(indent, ' ') << ppIdent(sc) << " = RECORD";
      if(sc.has("SYSFLAG")) {
        out << sc("SYSFLAG")(1).name;
      }
      if(base_sc.has("TYPE")) {
         out << "(" << base_sc.name << ")";
      }
      out << "\n";
      if(sc.has("DECLARATIONS")) {
         if(sc("DECLARATIONS").has("VAR")) {
            ppVar(sc("DECLARATIONS")("VAR"), out, indent + indent_step);
         }
      }
      out << string(indent, ' ') << "END;\n";

      return;
   }

   if(sc.has("ARRAY")) {
      // make sure the return type is enumerated as well!
      ppType1(base_sc, out, indent);
      out << string(indent, ' ') << ppIdent(sc) << " = ARRAY ";
      if(sc.has("SIZE")) {
         out << sc("SIZE")(1).name << " ";
      }
      out << "OF " << base_sc.name << ";\n";
      return;
   }

   if(sc.has("PROCVAR")) {
      // make sure the base type is enumerated as well!
      ppType1(base_sc, out, indent);
      // the proc parameters all have types also!
      if(sc.has("PARAMETERS")) {
         for(int i = 1; i <= sc("PARAMETERS").size(); i++) {
            assert(sc("PARAMETERS")(i).has("TYPE"));
            ppType1(sc("PARAMETERS")(i)("TYPE")(1), out, indent);
         }
      }

      out << string(indent, ' ') << ppIdent(sc) << " = ";
      ppProcSignature(sc, out, false);
      return;
   }

   // default action -- print the type name verbatim
   out << string(indent, ' ') << ppIdent(sc) << " = " << base_sc.name << ";\n";
   return;
}


void slim_binary::ppVar(scope &sc, ostream &out, int indent, bool new_line) {

   for(int i = 1; i <= sc.size(); i++) {
      if(new_line) out << string(indent, ' ');
      ppVar1(sc(i), out);
      if(new_line || i < sc.size()) {
         out << ";" << (new_line? "\n": " ");
      }
   }
}


void slim_binary::ppVar1(scope &sc, ostream &out) {

   // indentation, if any, is done by ppVar above
   scope &base_sc = sc("TYPE")(1);
   if(sc.has("REFERENCE")) out << "VAR ";
   out << ppIdent(sc) << ": " << base_sc.name;
}


void slim_binary::ppProcedure(scope &sc, ostream &out, int indent) {

   // print forward declarations first
   int i;
   if(sc.size() >= 2) {
      out << string(indent, ' ') << "(* Forward PROCEDURE declarations *)\n\n";
      for(i = sc.size(); i >= 1; i--) {
         out << string(indent, ' ');
         ppProcSignature(sc(i), out, true, true);
      }
   }
   for(i = sc.size(); i >= 1; i--) {
      ppProcedure1(sc(i), out, indent);
   }
}


void slim_binary::ppProcedure1(scope &sc, ostream &out, int indent) {

   out << "\n" << string(indent, ' ');
   ppProcSignature(sc, out, true);
   out << "\n";

   if(sc.has("DECLARATIONS")) {
      ppDeclarations(sc("DECLARATIONS"), out, indent + indent_step);
   }

   if(sc.has("BLOCK")) {
      out << string(indent, ' ') << "BEGIN  (* PROCEDURE "
            << sc.name << " *)\n";
      ppBlock(sc("BLOCK"), out, indent + indent_step);
   }

   out << string(indent, ' ') << "END " << sc.name << ";\n\n";
}


void slim_binary::ppBlock(scope &sc, ostream &out, int indent) {
   for(int i = 1; i <= sc.size(); i++) {
      // type dependencies must be taken into account here
      ppStatement(sc(i), out, indent);
   }
}


void slim_binary::ppStatement(scope &sc, ostream &out, int indent) {

   // handle more complex statements first...
   if(sc.name == "IF") {
      ppIFStatement(sc, out, indent);
      return;
   }
   if(sc.name == "WITH") {
      ppWITHStatement(sc, out, indent);
      return;
   }
   if(sc.name == "CASE") {
      ppCASEStatement(sc, out, indent);
      return;
   }
   if(sc.name == "WHILE") {
      ppWHILEStatement(sc, out, indent);
      return;
   }
   if(sc.name == "REPEAT") {
      ppREPEATStatement(sc, out, indent);
      return;
   }
   if(sc.name == "LOOP") {
      ppLOOPStatement(sc, out, indent);
      return;
   }

   // default action -- print the statement verbatim
   out << string(indent, ' ') << sc.name << ";\n";
   return;
}


void slim_binary::ppIFStatement(scope &sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc.name;
   if(sc.name == "IF" || sc.name == "ELSIF") {
      out << " " << sc("CONDITION")(1).name << " THEN";
   }
   out << endl;

   if(sc.has("BLOCK")) {
      ppBlock(sc("BLOCK"), out, indent + indent_step);
   }
   if(sc.has("ELSIF")) {
      ppIFStatement(sc("ELSIF"), out, indent);
   }
   else if(sc.has("ELSE")) {
      ppIFStatement(sc("ELSE"), out, indent);
   }
   else {
      out << string(indent, ' ') << "END;  (* IF *)\n";
   }
}


void slim_binary::ppWITHStatement(scope &sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc.name;
   if(sc.name == "WITH" || sc.name == "|") {
      out << " " << sc("CONDITION")(1).name << " DO";
   }
   out << endl;

   if(sc.has("BLOCK")) {
      ppBlock(sc("BLOCK"), out, indent + indent_step);
   }
   if(sc.has("|")) {
      ppWITHStatement(sc("|"), out, indent);
   }
   else if(sc.has("ELSE")) {
      ppWITHStatement(sc("ELSE"), out, indent);
   }
   else {
      out << string(indent, ' ') << "END;  (* WITH *)\n";
   }
}


void slim_binary::ppCASEStatement(scope &sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc.name;
   if(sc.name == "CASE") {
      out << " " << sc("EXPRESSION")(1).name << " OF" << endl;
   }
   else if(sc.name == "|") {
      out << " " << sc("CONDITION")(1).name << ":" << endl;
   }
   if(sc.has("BLOCK")) {
      ppBlock(sc("BLOCK"), out, indent + indent_step);
   }
   if(sc.has("|")) {
      ppCASEStatement(sc("|"), out, indent);
   }
   else if(sc.has("ELSE")) {
      ppCASEStatement(sc("ELSE"), out, indent);
   }
   else {
      out << string(indent, ' ') << "END;  (* CASE *)\n";
   }
}


void slim_binary::ppWHILEStatement(scope &sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc.name;
   out << " " << sc("CONDITION")(1).name << " DO\n";

   if(sc.has("BLOCK")) {
      ppBlock(sc("BLOCK"), out, indent + indent_step);
   }
   out << string(indent, ' ') << "END;  (* WHILE *)\n";
}


void slim_binary::ppREPEATStatement(scope &sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc.name << endl;

   if(sc.has("BLOCK")) {
      ppBlock(sc("BLOCK"), out, indent + indent_step);
   }
   out << string(indent, ' ') << "UNTIL "
         << sc("UNTIL")(1).name << ";\n";
}


void slim_binary::ppLOOPStatement(scope &sc, ostream &out, int indent) {

   out << string(indent, ' ') << sc.name << endl;

   if(sc.has("BLOCK")) {
      ppBlock(sc("BLOCK"), out, indent + indent_step);
   }
   out << string(indent, ' ') << "END;  (* LOOP *)\n";
}


const string &slim_binary::ppIdent(scope &id) {
   static string ident;
   ident = id.name;
   if(id.has("VISIBLE")) ident += "*";
   else if(id.has("READONLY")) ident += "-";
   return ident;
}


void slim_binary::ppProcSignature(scope &sc, ostream &out, bool print_name,
      bool print_forward) {

   // Code procedures (CProc) are emitted only in the forward section!
   if(sc.has("CODE PROC") && !print_forward) {
     return;
   }

   scope &base_sc = sc("TYPE")(1);
   out << "PROCEDURE";
   if(print_forward) out << (sc.has("CODE PROC")? "-": "^");

   if(sc.has("RECEIVER")) {
      out << "(";
      ppVar(sc("RECEIVER"), out, 0, false);
      out << ")";
   }

   if(print_name) {
      out << " " << ppIdent(sc);
   }

   out << "(";  // print parenths even if no parameters exist
   if(sc.has("PARAMETERS")) {
      ppVar(sc("PARAMETERS"), out, 0, false);
   }
   out << ")";
   if(base_sc != "NO RETURN TYPE") {
      out << ": " << base_sc.name;
   }
   if(sc.has("CODE PROC")) {  // print machine code for CProcs
     out << " " << sc("CODE SEQUENCE")(1).name;
   }
   out << ";\n";
}


} // namespace juice
