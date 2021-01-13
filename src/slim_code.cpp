/******************************************************************************
 * slim_code.cpp                                                              *
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


// ==================================== slim_binary: literal

const string &slim_binary::literal(code_type lit_type) {

   static string str_literal;
   streampos lit_offs = f.tellg();

   switch(lit_type) {
      case booleanConst: {
         slim_int bool_val; f >> bool_val;
         str_literal = (bool_val.val? "TRUE": "FALSE");
         break;
      }
      case charConst: {
         slim_int int_val; f >> int_val;
         assert(int_val.val >= 0);
         unsigned val = int_val.val;
         // some characters are directly printable
         if(val >= 32 && val < 127 && val != 34) {
            str_literal = "\"";
            unsigned char char_val = (unsigned char)val;
            str_literal += char_val;
            str_literal += "\"";
         }
         else {
            str_literal = "0" + hex(val, 2) + "X";
         }
         break;
      }
      case shortIntConst: case intConst: case longIntConst: {
         slim_int int_val; f >> int_val;
         str_literal = str(int_val.val);
         // detect special min and max constants -- Oberon finds the
         // numeric constants just too large in this case!!
         if(str_literal == "-2147483648") {
            str_literal = "MIN(LONGINT)";
         }
         break;
      }
      case setConst: {
         slim_int int_val; f >> int_val;
         long setval = int_val.val;
         // we need to "unpack" the set from a binary format into an Oberon
         // expression "{nn, .. }"
         str_literal = "{";
         int curr_num = 0;
         while(setval != 0) {
            bool this_bit = (setval & 1);
            setval >>= 1;
            if(this_bit) {
               str_literal += str(curr_num);
               if(setval != 0) {
                  str_literal += ",";
               }
            }
            curr_num++;
         }
         str_literal += "}";  // close brace
         break;
      }
      case realConst: {
         slim_real real_val; f >> real_val;
         str_literal = str(real_val.val);
         int dot_pos = str_literal.find('.');
         if(dot_pos < 0) str_literal += ".0";
         break;
      }
      case longRealConst: {
         slim_longreal real_val; f >> real_val;
         str_literal = str(real_val.val);
         // indicate double precision!
         int dot_pos = str_literal.find('e');
         if(dot_pos < 0) dot_pos = str_literal.find('E');
         if(dot_pos >= 0) str_literal[dot_pos] = 'D';
         else {
            dot_pos = str_literal.find('.');
            if(dot_pos < 0) str_literal += ".0";
         }
         // detect special min and max constants -- Oberon finds the
         // numeric constants just too large in this case!!
         if(str_literal == "1.79769D+308") {
            str_literal = "MAX(LONGREAL)";
         }
         break;
      }
      case stringConst: {
         slim_str str_val; f >> str_val;
         str_literal = "\"" + str_val.val + "\"";
         break;
      }
      default: {
         throw slim_error(InvalidConstType, current_module->name, lit_offs);
      }
   } // switch

   return str_literal;
}


// ==================================== slim_binary: Expr

scope *slim_binary::Expr(int &code_id) {
   streampos code_offs = f.tellg();

   scope *expr_sym = NULL, *enum_scope;
   slim_int code_num; f >> code_num;
   code_id = code_num.val;

   // first, return the entity which corresponds to code_id

   switch(code_id) {

      // pre-defined constants and simple statements
      case nilConst: code_id = 1; return bi->slot("NIL");
      case falseConst: return bi->slot("FALSE");
      case trueConst: return bi->slot("TRUE");

      case exitCode: return bi->slot("EXIT");

      case assertFunc: {   // the assert function is different in many respects
         scope *assert_sym = new scope("ASSERT(");
         slim_int trap_no;  f >> trap_no;
         assert_sym->slot("TRAP")->slot(str(trap_no.val));

         int assert_no;
         scope *assert_cond = Expr(assert_no);
         string assert_expr = assert_cond->name;
         // remove negation, if possible
         if(assert_expr[0] == '~') {
            assert_expr = assert_expr.substr(1);
         }
         else {
            assert_expr = "~(" + assert_expr + ")";
         }

         assert_sym->name += assert_expr;
         if(trap_no.val > 0) {
            assert_sym->name += (", " + str(trap_no.val));
         }
         assert_sym->name += ")";

         // calls to assert are not enumerated
         return assert_sym;
      }

      case haltFunc: {   // the halt function is different in many respects
         scope *halt_sym = new scope("HALT(");
         slim_int trap_no;  f >> trap_no;

         halt_sym->name += (str(trap_no.val) + ")");

         // calls to halt are not enumerated
         return halt_sym;
      }

      // user-defined constants - value needs to be read in
      case charConst: case shortIntConst: case intConst: case longIntConst:
      case setConst: case realConst: case longRealConst: case stringConst: {
         scope *const_obj = new scope(literal((code_type)code_id));
         const_obj->new_slot(bi->slot("CONST"));
         switch(code_id) {
            case charConst:
               const_obj->slot("TYPE")->slot(bi->slot("CHAR"));
               break;
            case shortIntConst:
               const_obj->slot("TYPE")->slot(bi->slot("SHORTINT"));
               break;
            case intConst:
               const_obj->slot("TYPE")->slot(bi->slot("INTEGER"));
               break;
            case longIntConst:
               const_obj->slot("TYPE")->slot(bi->slot("LONGINT"));
               break;
            case setConst:
               const_obj->slot("TYPE")->slot(bi->slot("SET"));
               break;
            case realConst:
               const_obj->slot("TYPE")->slot(bi->slot("REAL"));
               break;
            case longRealConst:
               const_obj->slot("TYPE")->slot(bi->slot("LONGREAL"));
               break;
            case stringConst:
               const_obj->slot("TYPE")->slot(bi->slot("STRING"));
               break;
         }
         gcode->new_slot(const_obj);
         return const_obj;
      }

      // built-in monadic functions
      case notFunc: case negFunc: case absFunc: case capFunc: case oddFunc:
      case shortIntCast: case ordFunc: case entierFunc: case realCast:
      case longRealCast: case derefFunc: case newFunc: case chrFunc: 
      case singletonSet: {
         scope *monadic = new scope;
         scope *op = NULL, *lhs = NULL;
         int lhs_code;
         lhs = Expr(lhs_code);
         scope *arg_type = (lhs->has_slot("TYPE")? lhs->slot("TYPE")->existing_slot(1): lhs);

         switch(code_id) {
            case notFunc: op = bi->slot("~"); op->slot(bi->slot("PREFIX")); break;
            case negFunc: op = bi->slot("-"); op->slot(bi->slot("PREFIX")); break;
            case absFunc: op = bi->slot("ABS"); break;
            case capFunc: op = bi->slot("CAP"); break;
            case oddFunc: op = bi->slot("ODD"); break;
            case newFunc: op = bi->slot("NEW"); break;
            case derefFunc: op = bi->slot(""); /* was "^" */ op->slot(bi->slot("POSTFIX")); break;
            case singletonSet: op = bi->slot("{}"); op->slot(bi->slot("SET")); break;
            // type casts
            case chrFunc: {
               op = bi->slot("CHR");
               monadic->slot("TYPE")->slot(bi->slot("CHAR"));
               break;
            }
            case shortIntCast: {
               if(arg_type->eq("REAL") || arg_type->eq("LONGREAL")) {
                   op = bi->slot("SHORT(SHORT(ENTIER"); op->slot(bi->slot("NEST3"));
               }
               else if (arg_type->eq("LONGINT")) {
                   op = bi->slot("SHORT(SHORT"); op->slot(bi->slot("NEST2"));
               }
               else {
                  op = bi->slot("SHORT");
               }
               monadic->slot("TYPE")->slot(bi->slot("SHORTINT"));
               break;
            }
            case ordFunc: {
               // function name depends on argument type!
               if(arg_type->eq("LONGINT")) {
                  op = bi->slot("SHORT");  // --> INTEGER
               }
               else if(arg_type->eq("SHORTINT")) {
                  op = bi->slot("LONG"); // --> INTEGER
               }
               else if(arg_type->eq("REAL") || arg_type->eq("LONGREAL")) {
                   op = bi->slot("SHORT(ENTIER"); op->slot(bi->slot("NEST2"));
               }
               else {
                  op = bi->slot("ORD");
               }
               monadic->slot("TYPE")->slot(bi->slot("INTEGER"));
               break;
            }
            case entierFunc: {
               // function name depends on argument type!
               if(arg_type->eq("INTEGER")) {
                  op = bi->slot("LONG");
               }
               // not sure if "CHAR" should not have a separate "LONG(LONG(LONG"...
               else if(arg_type->eq("CHAR") || arg_type->eq("SHORTINT")) {
                   op = bi->slot("LONG(LONG"); op->slot(bi->slot("NEST2"));
               }
               else {
                  op = bi->slot("ENTIER"); // largest integer not greater than real x
               }
               monadic->slot("TYPE")->slot(bi->slot("LONGINT"));
               break;
            }
            case realCast: {
               // function name depends on argument type!
               if(arg_type->eq("LONGREAL")) {
                  op = bi->slot("SHORT");
               }
               else {
                   op = bi->slot("REAL CAST"); op->slot(bi->slot("IMPLICIT"));
               }
               monadic->slot("TYPE")->slot(bi->slot("REAL"));
               break;
            }
            case longRealCast: {
               // function name depends on argument type!
               if(arg_type->eq("REAL")) {
                  op = bi->slot("LONG");
               }
               else {
                   op = bi->slot("LONGREAL CAST"); op->slot(bi->slot("IMPLICIT"));
               }
               monadic->slot("TYPE")->slot(bi->slot("LONGREAL"));
               break;
            }
         }

         if (code_id != derefFunc) {
             // not sure if this would hurt...
             monadic->slot("FUNCTION")->slot(op);
             monadic->slot("ARGUMENTS")->new_slot(lhs);
         }

         if(op->has_slot("PREFIX")) {
            monadic->name = op->name + lhs->name;
         }
         else if(op->has_slot("IMPLICIT")) {
            monadic->name = lhs->name;    // omit fn name during some casts
         }
         else if(op->has_slot("POSTFIX")) {
            monadic->name = lhs->name + op->name;
         }
         else if(op->has_slot("SET")) {
            monadic->name = "{" + lhs->name + "}";
         }
         else {
            monadic->name = op->name + "(" + lhs->name;
            if(op->has_slot("NEST3")) {
               monadic->name += ")))";
            }
            else if(op->has_slot("NEST2")) {
               monadic->name += "))";
            }
            else {
               monadic->name += ")";
            }
         }
         // place into either global or local enumeration, depending on whether
         // the constituent parts are global or local
         enum_scope = (lhs_code > 0? gcode: lcode);
         code_id = (lhs_code > 0? 1: -1);
         enum_scope->new_slot(monadic);

         // propagate type information
         if(!monadic->has_slot("TYPE") && lhs->has_slot("TYPE")) {
            monadic->slot(lhs->slot("TYPE"));
         }
         return monadic;
      }

      // built-in dyadic functions
      case andFunc: case orFunc: case mulFunc: case divFunc: case idivFunc:
      case imodFunc: case addFunc: case subFunc: case maskFunc: case ashFunc:
      case lenFunc: case inFunc: case eqFunc: case neFunc: case ltFunc:
      case leFunc: case gtFunc: case geFunc: case assignFunc:
      case incFunc: case decFunc: case inclFunc: case exclFunc: case copyFunc:
      case indexFunc: case rangeSet: {
         scope *dyadic = new scope;
         scope *op = NULL, *lhs = NULL, *rhs = NULL;
         switch(code_id) {
            case andFunc: op = bi->slot("&"); op->slot(bi->slot("INFIX")); break;
            case orFunc: op = bi->slot("OR"); op->slot(bi->slot("PARENTH INFIX")); break;
            case mulFunc: op = bi->slot("*"); op->slot(bi->slot("INFIX")); break;
            case divFunc: op = bi->slot("/"); op->slot(bi->slot("INFIX")); break;
            case idivFunc: op = bi->slot("DIV"); op->slot(bi->slot("INFIX")); break;
            case imodFunc: op = bi->slot("MOD"); op->slot(bi->slot("INFIX")); break;
            case addFunc: op = bi->slot("+"); op->slot(bi->slot("PARENTH INFIX")); break;
            case subFunc: op = bi->slot("-"); op->slot(bi->slot("PARENTH INFIX")); break;
            case maskFunc: op = bi->slot("MOD"); op->slot(bi->slot("INFIX")); break;
            case ashFunc: op = bi->slot("ASH"); break;
            case lenFunc: op = bi->slot("LEN"); break;
            case inFunc: op = bi->slot("IN"); op->slot(bi->slot("PARENTH INFIX")); break;
            case eqFunc: op = bi->slot("="); op->slot(bi->slot("PARENTH INFIX")); break;
            case neFunc: op = bi->slot("#"); op->slot(bi->slot("PARENTH INFIX")); break;
            case ltFunc: op = bi->slot("<"); op->slot(bi->slot("PARENTH INFIX")); break;
            case leFunc: op = bi->slot("<="); op->slot(bi->slot("PARENTH INFIX")); break;
            case gtFunc: op = bi->slot(">"); op->slot(bi->slot("PARENTH INFIX")); break;
            case geFunc: op = bi->slot(">="); op->slot(bi->slot("PARENTH INFIX")); break;
            case assignFunc: op = bi->slot(":="); op->slot(bi->slot("INFIX")); break;
            case incFunc: op = bi->slot("INC"); break;
            case decFunc: op = bi->slot("DEC"); break;
            case inclFunc: op = bi->slot("INCL"); break;
            case exclFunc: op = bi->slot("EXCL"); break;
            case copyFunc: op = bi->slot("COPY"); op->slot(bi->slot("REVERSE")); break;
            case indexFunc: op = bi->slot("[]"); op->slot(bi->slot("INDEX")); break;
            case rangeSet: op = bi->slot("{..}"); op->slot(bi->slot("SET")); break;
         }
         dyadic->slot("FUNCTION")->slot(op);
         int lhs_code, rhs_code;
         lhs = Expr(lhs_code);
         rhs = Expr(rhs_code);
         dyadic->slot("ARGUMENTS")->new_slot(lhs);
         dyadic->slot("ARGUMENTS")->new_slot(rhs);

         // negate the second argument for the MASK function!
         string rhs_name = rhs->name;
         if(code_id == maskFunc) {
            if(rhs_name[0] == '-') {
               rhs_name = rhs_name.substr(1);
            }
            else {
               rhs_name = "-" + rhs_name;
            }
         }
         if(op->has_slot("INFIX")) {
            dyadic->name = lhs->name + " " + op->name + " " + rhs_name;
         }
         else if(op->has_slot("PARENTH INFIX")) {
            dyadic->name = "(" + lhs->name + " " + op->name + " " + rhs_name + ")";
         }
         else if(op->has_slot("INDEX")) {
            dyadic->name = lhs->name + "[" + rhs_name + "]";
         }
         else if(op->has_slot("SET")) {
            dyadic->name = "{" + lhs->name + ".." + rhs->name + "}";
         }
         else if(op->has_slot("REVERSE")) {
            dyadic->name = op->name + "(" + rhs_name + ", " + lhs->name + ")";
         }
         else {
            dyadic->name = op->name + "(" + lhs->name + ", " + rhs_name + ")";
         }
         // place into either global or local enumeration, depending on whether
         // the constituent parts are global or local
         enum_scope = (lhs_code > 0 && rhs_code > 0? gcode: lcode);
         code_id = (lhs_code > 0 && rhs_code > 0? 1: -1);
         enum_scope->new_slot(dyadic);

         // propagate type information
         if(lhs->has_slot("TYPE")) {
            dyadic->slot(lhs->slot("TYPE"));
         }
         else if(rhs->has_slot("TYPE")) {
            dyadic->slot(rhs->slot("TYPE"));
         }
         return dyadic;
      }

      // procedure variable call -- handled similarly to a procedure call proper
      case procVarCall: {
         code_offs = f.tellg();
         expr_sym = Expr(code_id);   // retrieve procedure variable
         assert(expr_sym->has_slot("TYPE"));
         scope *proctype_sym = expr_sym->slot("TYPE")->existing_slot(1);
         assert(proctype_sym->has_slot("PROCVAR"));  // check for type

         bool global_call = (code_id > 0);
         string call_expr = expr_sym->name;
         bool imported = (expr_sym->has_slot("FINGERPRINT") != 0);
         int arity = val<int>(proctype_sym->slot("ARITY")->existing_slot(1)->name);

         // enumerate all of the "partial" call primitives, including
         // the one with no parameters at all
         for(int i = 0; i <= arity; i++) {

            if(i) {
               if(i == 1) call_expr += "(";

               int parm_id;
               scope *parm = Expr(parm_id);
               string parm_expr = parm->name;
               // fix up the syntactic representation of the parameter
               int s = parm_expr.size();
               if(parm_expr[0] == '(' && parm_expr.find(')') == s - 1) {
                  // get rid of enclosing parenths, if any
                  parm_expr = parm_expr.substr(1, s - 2);
               }
               //
               if(parm_id < 0) {
                  global_call = false;
               }
               call_expr += parm_expr;

               if(i == arity) {
                  call_expr += ")";
               }
               else {
                  call_expr += ", ";
               }
            }
            else if(arity == 0) {
               call_expr += "()";
            }
            // place the (semi-)finished call into the enumeration
            expr_sym = new scope(call_expr);
            expr_sym->slot(bi->slot("PROCEDURE"));
            // propagate return type
            assert(proctype_sym->has_slot("TYPE"));
            expr_sym->slot(proctype_sym->slot("TYPE"));

            expr_sym->slot("ARITY")->slot(str(arity));
            expr_sym->slot("PARTIAL")->slot(str(i + 1));

            enum_scope = (global_call? gcode: lcode);
            enum_scope->new_slot(expr_sym);
         }
         return expr_sym;
      }

      // if statement
      case ifCode: {
         scope *if_sym = new scope("IF");
         int cond_id;
         scope *cond = Expr(cond_id);
         if_sym->slot("CONDITION")->new_slot(cond);
         // THEN
         while((unsigned char)f.peek() > elseCode) {
            scope *stmt_sym = Expr(cond_id);   // emit statements into the block
            if_sym->slot("BLOCK")->new_slot(stmt_sym);
            code_offs = f.tellg();
         }
         // ELSIF
         scope *elsif_sym = if_sym;
         while((char)f.peek() == elsifCode) {
            f.get();
            elsif_sym = elsif_sym->slot("ELSIF");   // <-- note the square brackets!!
            cond = Expr(cond_id);
            elsif_sym->slot("CONDITION")->slot(cond);
            while((unsigned char)f.peek() > elseCode) {
               scope *stmt_sym = Expr(cond_id);   // emit statements into the block
               elsif_sym->slot("BLOCK")->new_slot(stmt_sym);
               code_offs = f.tellg();
            }
         }
         // ELSE
         if((char)f.peek() == elseCode) {
            f.get();
            elsif_sym = elsif_sym->slot("ELSE");
            while((char)f.peek() != endCode) {
               scope *stmt_sym = Expr(cond_id);   // emit statements into the block
               elsif_sym->slot("BLOCK")->new_slot(stmt_sym);
               code_offs = f.tellg();
            }
         }
         code_offs = f.tellg();
         if((char)f.get() != endCode) {
            throw slim_error(EndOfCodeExpected, current_module->name, code_offs);
         }
         return if_sym;
      }

      // repeat statement
      case repeatCode: {
         scope *repeat_sym = new scope("REPEAT");
         int cond_id;
         // BLOCK
         while((char)f.peek() != endCode) {
            scope *stmt_sym = Expr(cond_id);   // emit statements into the block
            repeat_sym->slot("BLOCK")->new_slot(stmt_sym);
         }
         if((char)f.get() != endCode) {
            throw slim_error(EndOfCodeExpected, current_module->name, code_offs);
         }
         scope *cond = Expr(cond_id);
         repeat_sym->slot("UNTIL")->slot(cond);
         return repeat_sym;
      }

      // while statement
      case whileCode: {
         scope *while_sym = new scope("WHILE");
         int cond_id;
         scope *cond = Expr(cond_id);
         while_sym->slot("CONDITION")->new_slot(cond);
         // BLOCK
         while((char)f.peek() != endCode) {
            scope *stmt_sym = Expr(cond_id);   // emit statements into the block
            while_sym->slot("BLOCK")->new_slot(stmt_sym);
         }
         code_offs = f.tellg();
         if((char)f.get() != endCode) {
            throw slim_error(EndOfCodeExpected, current_module->name, code_offs);
         }
         return while_sym;
      }

      // loop statement
      case loopCode: {
         scope *loop_sym = new scope("LOOP");
         int cond_id;
         // BLOCK
         while((char)f.peek() != endCode) {
            scope *stmt_sym = Expr(cond_id);   // emit statements into the block
            loop_sym->slot("BLOCK")->new_slot(stmt_sym);
         }
         code_offs = f.tellg();
         if((char)f.get() != endCode) {
            throw slim_error(EndOfCodeExpected, current_module->name, code_offs);
         }
         return loop_sym;
      }

      // with statement
      case withCode: {
         scope *with_sym = new scope("WITH");
         int cond_id;
         scope *cond = Expr(cond_id);
         string cond_expr = cond->name;
         // fix up the syntactic representation of the condition
         int s = cond_expr.size();
         if(cond_expr[0] == '(' && cond_expr.find(')') == s - 1) {
            // get rid of enclosing parenths, if any
            cond_expr = cond_expr.substr(1, s - 2);
         }
         s = cond_expr.find(" IS ");
         if(s > 0) {
            cond_expr = cond_expr.substr(0, s) + ": " + cond_expr.substr(s + 4);
         }
         with_sym->slot("CONDITION")->slot(cond_expr);

         // the WITH statement maintains its own code enumeration.
         // it is merged with the enclosing enumeration if an ELSE
         // clause is present!!!!!
         scope *outer_lcode = lcode;
         lcode = with_sym->slot("CODE ENUM");
         lcode->set_base(outer_lcode);

         // now enumerate the control variable, if one exists
         if(cond->has_slot("COMPONENTS") && cond->slot("COMPONENTS")->has_at_least_slots(2)) {
            int gtl = 0, ltl = 0;
            generate_sym_enum(cond->slot("COMPONENTS")->existing_slot(2), gtl, ltl);
         }

         // THEN
         while((unsigned char)f.peek() > wendCode) {
            scope *stmt_sym = Expr(cond_id);   // emit statements into the block
            with_sym->slot("BLOCK")->new_slot(stmt_sym);
         }
         // |
         scope *elsif_sym = with_sym;
         while((char)f.peek() == elsifCode) {
            f.get();
            elsif_sym = elsif_sym->slot("|");   // <-- [] means multiple insertions!
            cond = Expr(cond_id);
            cond_expr = cond->name;
            // fix up the syntactic representation of the condition
            s = cond_expr.size();
            if(cond_expr[0] == '(' && cond_expr.find(')') == s - 1) {
               // get rid of enclosing parenths, if any
               cond_expr = cond_expr.substr(1, s - 2);
            }
            s = cond_expr.find(" IS ");
            if(s > 0) {
               cond_expr = cond_expr.substr(0, s) + ": " + cond_expr.substr(s + 4);
            }
            elsif_sym->slot("CONDITION")->new_slot(cond_expr);
            while((unsigned char)f.peek() > wendCode) {
               scope *stmt_sym = Expr(cond_id);   // emit statements into the block
               elsif_sym->slot("BLOCK")->new_slot(stmt_sym);
            }
         }
         // ELSE
         if((char)f.peek() == elseCode) {
            f.get();
            elsif_sym = elsif_sym->slot("ELSE");
            while((unsigned char)f.peek() > wendCode) {
               scope *stmt_sym = Expr(cond_id);   // emit statements into the block
               elsif_sym->slot("BLOCK")->new_slot(stmt_sym);
            }
            // "promote" the local enumeration by attaching it to the
            // outer block
            outer_lcode->append_scope(lcode);
         }
         lcode = outer_lcode;  // restore enum stack
         code_offs = f.tellg();
         if((char)f.peek() != endCode && (char)f.peek() != wendCode) {
            throw slim_error(EndOfWithExpected, current_module->name, code_offs);
         }
         f.get();
         return with_sym;
      }

      // case statement
      case caseCode: {
         scope *case_sym = new scope("CASE");

         // the following is only useful to the loader...
         slim_int smallest, largest;  f >> smallest >> largest;

         int cond_id;
         code_offs = f.tellg();

         scope *case_expr = Expr(cond_id);
         case_sym->slot("EXPRESSION")->slot(case_expr);
         assert(case_expr->has_slot("TYPE"));

         code_type expr_type;
         if(case_expr->slot("TYPE")->existing_slot(1)->eq("CHAR")) {
            expr_type = charConst;
         }
         else if(case_expr->slot("TYPE")->existing_slot(1)->eq("SET")) {
            expr_type = setConst;
         }
         else {
            expr_type = longIntConst;
         }

         // Default (ELSE) action comes first!!
         scope *else_sym = NULL;
         if((char)f.peek() == caseNoElse) {
            f.get();
         }
         else {
           else_sym = new scope("ELSE");
           while((char)f.peek() > caseSubsRangeLabel || (char)f.peek() < endCode) {
              scope *stmt_sym = Expr(cond_id);   // emit statements into the block
              else_sym->slot("BLOCK")->new_slot(stmt_sym);
           }
         }
         // |
         scope *elsif_sym = NULL;
         while((char)f.peek() >= caseValLabel && (char)f.peek() <= caseSubsRangeLabel) {
            code_offs = f.tellg();
            elsif_sym = (elsif_sym? elsif_sym->slot("|"): case_sym->slot("|"));
            string switch_vals;

            // each CASE may consist of a multitude of labels
            while((char)f.peek() >= caseValLabel && (char)f.peek() <= caseSubsRangeLabel) {
                if (!switch_vals.empty()) {
                    switch_vals += ", ";
                }
              char label_type = f.get();
              switch_vals += literal(expr_type);
              // read the first label (via proc 'literal')
              if(label_type == caseRangeLabel || label_type == caseSubsRangeLabel) {
                 // read the second label (via proc 'literal')
                 switch_vals += (".." + literal(expr_type));
              }
            }
            elsif_sym->slot("CONDITION")->slot(switch_vals);
            // now read the action code for the label
            while((char)f.peek() > caseSubsRangeLabel || (char)f.peek() < endCode) {
               scope *stmt_sym = Expr(cond_id);   // emit statements into the block
               elsif_sym->slot("BLOCK")->new_slot(stmt_sym);
            }
         }
         // put the ELSE (if it exists) at the end of the cascade
         assert(elsif_sym);
         if(else_sym) {
           elsif_sym->slot(else_sym);
         }

         if ((char)f.get() != endCode) {
             throw slim_error(EndOfCodeExpected, current_module->name, code_offs);
         }
         return case_sym;
      }

      // generated (i.e., previously enumerated) values
      default: {
         if(code_id >= 0x3d) { // global enum
            if(gcode->size() < code_id - 0x3d + 1) {
               throw slim_error(InvalidGlobalEnum, current_module->name, code_offs);
            }
            expr_sym = gcode->existing_slot(code_id - 0x3d + 1);
            code_id = 1;
         }
         else if(code_id <= -1) { // local enum
            if(lcode->size_incl_base() < -code_id) {
               throw slim_error(InvalidLocalEnum, current_module->name, code_offs);
            }
            // check enclosing functions also...
            expr_sym = lcode->existing_slot_incl_base(-code_id);
            code_id = -1;
         }
         else {
            throw slim_error(InvalidCodeEnum, current_module->name, code_offs);
            return NULL;
         }
      }
   }

   // ****** NODE POST-PROCESSING

   // the node may be a RETURN statement
   if(expr_sym->eq("RETURN")) {
      assert(psym->has_slot("PROCEDURE"));
      if(psym->has_slot("TYPE")
            && psym->slot("TYPE")->existing_slot(1)->ne("NO RETURN TYPE")) {
         expr_sym = new scope("RETURN");
         int retval_id;
         scope *retval_sym = Expr(retval_id);
         expr_sym->slot("RETURN VALUE")->slot(retval_sym);
         expr_sym->name += (" " + retval_sym->name);

         // "RETURN nn" is enumerated; just "RETURN" is not
         lcode->new_slot(expr_sym);
      }
      return expr_sym;
   }

   // the node may be a procedure call
   if(expr_sym->has_slot("PROCEDURE")) {
      bool global_call = (code_id > 0);
      string call_expr = expr_sym->name;
      bool method_call = (expr_sym->has_slot("METHOD") != 0);
      int arity = val<int>(expr_sym->slot("ARITY")->existing_slot(1)->name);

      assert(expr_sym->has_slot("TYPE"));
      scope *proc_type = expr_sym->slot("TYPE");

      int partial;
      if(expr_sym->has_slot("PARTIAL")) {
         partial = val<int>(expr_sym->slot("PARTIAL")->existing_slot(1)->name);
      }
      else {
         partial = 1;
      }


      // now build the call: "procname(parm, ...)";
      for(int i = partial; i <= arity; i++) {


         int parm_id;
         scope *parm = Expr(parm_id);
         string parm_expr = parm->name;
         // fix up the syntactic representation of the parameter
         int s = parm_expr.size();
         if(parm_expr[0] == '(' && parm_expr.find(')') == s - 1) {
            // get rid of enclosing parenths, if any
            parm_expr = parm_expr.substr(1, s - 2);
         }
         //
         if(parm_id < 0) {
            global_call = false;
         }
         if(method_call) {
             if (i == 1) {
                 call_expr = parm_expr + "." + call_expr;
             }
            else {
                 if (i == 2) {
                     call_expr += "(";
                 }
               call_expr += parm_expr;
               if (i < arity) {
                   call_expr += ", ";
               }
            }
         }
         else {
             if (i == 1) {
                 call_expr += "(";
             }
            call_expr += parm_expr;
            if (i < arity) {
                call_expr += ", ";
            }
         }
         if(i == arity) {
            call_expr += ")";
         }

         // place the (semi-)finished call into the enumeration
         expr_sym = new scope(call_expr);
         expr_sym->slot(bi->slot("PROCEDURE"));
         if (method_call) {
             expr_sym->slot(bi->slot("METHOD"));
         }
         expr_sym->slot("ARITY")->slot(str(arity));
         expr_sym->slot("PARTIAL")->slot(str(i + 1));
         expr_sym->slot(proc_type);   // propagate return type

         enum_scope = (global_call? gcode: lcode);
         code_id = (global_call? 1: -1);
         enum_scope->new_slot(expr_sym);
      }

      if(arity == 0 && expr_sym->name.find('(') == -1) {
         expr_sym = new scope(expr_sym->name + "()");
      }
      return expr_sym;
   }

   // array access
   if(expr_sym->has_slot("ARRAY INDEX")) {
      scope *arr_sym = new scope(expr_sym->name);
      int idx_id;
      scope *idx_sym = Expr(idx_id);
      arr_sym->name += (idx_sym->name + "]");
      arr_sym->slot("INDEX")->new_slot(idx_sym);

      assert(expr_sym->has_slot("TYPE"));
      arr_sym->slot(expr_sym->slot("TYPE"));

      // place into either global or local enumeration, depending on whether
      // the constituent parts are global or local
      enum_scope = (code_id > 0 && idx_id > 0? gcode: lcode);
      code_id = (code_id > 0 && idx_id > 0? 1: -1);
      enum_scope->new_slot(arr_sym);

      return arr_sym;
   }

   // the oper_name may indicate a half-node!
   if(expr_sym->has_slot("LEFT") || expr_sym->has_slot("RIGHT")) {
      scope *first_half = expr_sym;
      int second_half_id;
      scope *second_half = Expr(second_half_id);
      expr_sym = new scope;
      expr_sym->new_slot(bi->slot("STATEMENT"));
      assert(second_half != NULL);
      expr_sym->slot("COMPONENTS")->new_slot(first_half);
      expr_sym->slot("COMPONENTS")->new_slot(second_half);

      // assemble the expression correctly -- note that we may still
      // have an incomplete (i.e., left or right) expression
      if(first_half->has_slot("LEFT")) {
         expr_sym->name = (first_half->name + second_half->name);
         if (second_half->has_slot("LEFT")) {
             expr_sym->slot(bi->slot("LEFT"));
         }
         // propagate type information
         if (second_half->has_slot("TYPE")) {
             expr_sym->slot(second_half->slot("TYPE"));
         }
         else if (first_half->has_slot("TYPE")) {
             expr_sym->slot(first_half->slot("TYPE"));
         }
      }
      else {
         assert(first_half->has_slot("RIGHT"));
         expr_sym->name = (second_half->name + first_half->name);
         // combination indirection/cast is illegal in the source language!
         // eliminate it
         int ind_cast = expr_sym->name.find("^(");
         if(ind_cast > 0) {
            expr_sym->name = second_half->name;
         }
         if (second_half->has_slot("RIGHT")) {
             expr_sym->slot(bi->slot("RIGHT"));
         }
         // propagate type information
         if (first_half->has_slot("TYPE")) {
             expr_sym->slot(first_half->slot("TYPE"));
         }
         else if (second_half->has_slot("TYPE")) {
             expr_sym->slot(second_half->slot("TYPE"));
         }
      }

      if(first_half->has_slot("ADD PARENTH") || second_half->has_slot("ADD PARENTH")) {
         expr_sym->name = "(" + expr_sym->name + ")";
      }
      // place into either global or local enumeration, depending on whether
      // the constituent parts are global or local
      if(code_id > 0 && second_half_id > 0) {
         gcode->new_slot(expr_sym);
         code_id = 1;  // return to caller
      }
      else {
         lcode->new_slot(expr_sym);
         code_id = -1; // return to caller
      }
   }
   return expr_sym;
}


// ==================================== slim_binary: BlockCode

void slim_binary::BlockCode(void) {
   streampos block_offs = f.tellg();
   if((char)f.get() != beginCode) {
      throw slim_error(BeginCodeExpected, current_module->name, block_offs);
   }
   scope *curr_scope = curr;
   curr = curr->slot("BLOCK");
   while(f.peek() != endCode) {
      int block_id;
      scope *stmt_sym = Expr(block_id);   // emit statements into the block
      curr->new_slot(stmt_sym);
   }
   curr = curr_scope;
   f.get();
}


// ==================================== slim_binary: ProcCode

void slim_binary::ProcCode(void) {
   streampos proc_offs = f.tellg();

   slim_int proc_num; f >> proc_num;
   int proc_id = proc_num.val;

   // proc_id currently points to the declaration primitive.  skip 1 slot
   // in the enumeration to reach the proc object itself.
   scope *proc_sym;
   if(proc_id >= 0) {
      proc_sym = gcode->existing_slot(proc_id - 0x3d + 1 - 1);
   }
   else {
      proc_sym = lcode->existing_slot_incl_base(-proc_id - 1);
   }

   // now process the parameters for this procedure -- these are
   // stored as a local symbol table
   scope *curr_scope = curr;
   scope *curr_lcode = lcode;
   scope *curr_psym = psym;

   // switch to a separate stack for local (i.e., PROCEDURE) code
   // enumerations -- this should have been created during symbol
   // table processing

   assert(proc_sym->has_slot("CODE ENUM"));
   lcode = proc_sym->slot("CODE ENUM");

   curr = proc_sym;
   psym = proc_sym;

   SymbolTable(hidden, false);  // get local symbols...
   // procedure code precedes main module code
   while(f.peek() != beginCode) {
      ProcCode();
   }

   BlockCode();                  // ...and local code

   curr = curr_scope;   // restore parent scope
   lcode = curr_lcode;
   psym = curr_psym;

}


// ==================================== slim_binary: CodeSection

void slim_binary::CodeSection(void) {

   // procedure code precedes main module code
   while(f.peek() != beginCode) {
      ProcCode();
   }
   BlockCode();
}


} // namespace juice
