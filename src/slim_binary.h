/******************************************************************************
 * slim_binary.h                                                              *
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

#ifndef __juice_slim_binary
#define __juice_slim_binary

#define SB1D_VERSION "2.03"
#define SLIMBIN      "Slim Binary(tm)"
#define FILEMAP_HEADER 0x22
#define FILEDIR_FILE "$FILEDIR.$$$"

#include <string>
#include <iostream>
#include <fstream>
#include <unordered_map>

using namespace std;

#include "scope.h"
using namespace std;

#include "slim_data.h"

namespace juice {

// used to control symbol access

enum sym_vis {
   hidden,
   read_only,
   visible
};

// the slim binary class is a program-level abstraction of slim binary
// files used by the Oberon system.  the user can initialize it by reading
// from a file using the '<<' operator and/or modify its contents and write
// it back out.

class slim_binary {
   string fdir, fname;  // current module being read
   ifstream f;
   bool code_gen_phase;

   // the entire slim binary, including information about imported MODULEs,
   // is accessible via a single symbol
   scope
      top,        // the top-most symbol for the whole associative hierarchy
      *curr,      // current position within the hierarchy
      *current_module,  // current module being processed
      *bi,        // sub-container for built-in symbols (data types, etc.)
      *gcode,     // global code enumeration
      *lcode,     // local code enumeration
      *gtype,     // global type enumeration
      *psym;      // current procedure being processed

   bool open_module(const string &name, int pos = 0);
   sym_vis check_visibility(sym_vis vis);
   bool check_leaf(void);
   void append_attributes(scope *s, sym_vis vis, bool leaf = false);
   void generate_sym_enum(scope *enum_sym, int &gtl, int &ltl,
         const string &module_name = "", bool mandatory = false);

   // pretty-printing
   void ppImport(scope *sc, ostream &out, int indent);
   void ppDeclarations(scope *sc, ostream &out, int indent);
   void ppConst(scope *sc, ostream &out, int indent);
   void ppType(scope *sc, ostream &out, int indent);
   void ppType1(scope *sc, ostream &out, int indent);
   void ppVar(scope *sc, ostream &out, int indent, bool new_line = true);
   void ppVar1(scope *sc, ostream &out);
   void ppProcedure(scope *sc, ostream &out, int indent);
   void ppProcedure1(scope *sc, ostream &out, int indent);
   void ppBlock(scope *sc, ostream &out, int indent);
   void ppStatement(scope *sc, ostream &out, int indent);
   void ppIFStatement(scope *sc, ostream &out, int indent);
   void ppWITHStatement(scope *sc, ostream &out, int indent);
   void ppCASEStatement(scope *sc, ostream &out, int indent);
   void ppWHILEStatement(scope *sc, ostream &out, int indent);
   void ppREPEATStatement(scope *sc, ostream &out, int indent);
   void ppLOOPStatement(scope *sc, ostream &out, int indent);
   const string &ppIdent(scope *id);
   void ppProcSignature(scope *sc, ostream &out, bool print_name = true,
         bool print_forward = false);

  public:
   slim_binary(void);
   virtual ~slim_binary(void);
   void set_directory(const string &dir);
   bool read(const string &name);
   void print(ostream &out);
   void pretty_print(ostream &out);

   // the following are used when reading slim binary files.  exceptions are
   // used to abort any unsuccessful parses.  the names of the member functions
   // should roughly correspond to those of nonterminal symbols, found in
   // SlimBinaryLayout.Txt

  private:
   void SlimBinary(void);

   // ==== symbol table/data

   enum sb_type {
       oberonMagic = 0xBB,
       oberonFileMap = 0x6F
   } sb_byte;

   enum hdr_type {
      slimBinary = 0x88,
      x86Binary = 0x55,  // Win32 or Linux
      macPPCBinary = 0x44,
      linuxPPCBinary = 0x45, 
      mac68kBinary = 0x11
   } sb_hdr;

   enum sym_type {
      boolSym = 0x01,   // bool
      charSym = 0x02,  // char
      shortIntSym = 0x03, // unsigned char
      intSym = 0x04, // short
      longIntSym = 0x05, // long int
      realSym = 0x06, // float
      longRealSym = 0x07, // double
      setSym = 0x08,
      stringSym = 0x09,

      noBaseType = 0x00,
      noReturnType = 0x0a,

      byteSym = 0x0c, sysPtrSym = 0x0d,
      module01 = 0x0e, module31 = 0x2c,
      otherMod = 0x2d,

      dynArraySym = 0x2e, arraySym = 0x2f, pointerSym = 0x30,
      recordSym = 0x31, procVarSym = 0x32,

      sysFlag = 0x33, // System attribute # immediately follows

      privateSym = 0x00, hiddenSym = 0x34, readOnlySym = 0x35,

      leafSym = 0x36, // useful for optimizations

      constSym = 0x37, varSym = 0x38,
      lProcSym = 0x39,     // local procedure
      xProcSym = 0x3A,     // exported procedure
      cProcSym = 0x3B,     // code (machine language) procedure
      tProcSym = 0x3C,     // type-bound procedure (method)

      aliasSym = 0x3D, typeSym = 0x3E,
      endSym = 0x3F
   };

   enum proc_type {
      normal_proc = 0x00,
      proc_variable = 0x01,
      code_proc = 0x02
   };

   unordered_map<string, string> fileMap, moduleMap;

   void PublicInterface(sym_vis vis);
   void SymbolTable(sym_vis vis, bool get_modules = true);
   void ModuleList(sym_vis vis);
   scope *Type(sym_vis vis, int &gtl, int &ltl);
   scope *TypeDecl(sym_vis vis, bool type_alias = false);
   scope *ConstDecl(sym_vis vis);
   scope *VarDecl(sym_vis vis, int &gtl, int &ltl);
   scope *ProcDecl(sym_vis vis, int &gtl, int &ltl, proc_type proctype = normal_proc);

   enum fprint_type {
      recordEu = 0x01,
      endEu = 0x00
   };

   void ObjectKey(scope *curr_scope, int &gtl, int &ltl, const string &module_name);
   void ImportedKeys(void);
   void FileMapping(void);
   string LookupFile(const string &f);
   string LookupModule(const string &m, bool truncExtension = true);

   // ==== code

   enum code_type {
      beginCode = 0x09,
      endCode = 0x00,

      nilConst = 0x00,    // constants
      booleanConst = 0x01,
      falseConst = 0x01,
      trueConst = 0x02,
      charConst = 0x03,
      shortIntConst = 0x04,
      intConst = 0x05,
      longIntConst = 0x06,
      setConst = 0x07,
      realConst = 0x08,
      longRealConst = 0x09,
      stringConst = 0x0A,

      ifCode = 0x2E,    // structured constructs
      caseCode = 0x2F,
      whileCode = 0x30,
      repeatCode = 0x31,
      loopCode = 0x32,
      exitCode = 0x33,
      withCode = 0x34,
      untilCode = 0x00,
      elsifCode = 0x01,
      elseCode = 0x02,
      wendCode = 0x03,
      caseNoElse = 0x04,
      caseValLabel = 0x05,
      caseRangeLabel = 0x06,
      caseSubsValLabel = 0x07,         // subsequent values for a single CASE
      caseSubsRangeLabel = 0x08,       // subsequent ranges for a single CASE

      derefFunc = 0x0b,
      indexFunc = 0x0c,
      notFunc = 0x0D,    // built-in functions
      negFunc = 0x0E,
      absFunc = 0x0F,
      capFunc = 0x10,
      oddFunc = 0x11,
      andFunc = 0x12,
      orFunc = 0x13,
      addFunc = 0x18, subFunc = 0x19, mulFunc = 0x14, divFunc = 0x15,
      idivFunc = 0x16, imodFunc = 0x17,   // integer DIV and MOD
      maskFunc = 0x1A,
      ashFunc = 0x1B,
      lenFunc = 0x1C,
      inFunc = 0x1D,
      eqFunc = 0x1E, neFunc = 0x1F, ltFunc = 0x20,
      leFunc = 0x21, gtFunc = 0x22, geFunc = 0x23,
      rangeSet = 0x24,
      procVarCall = 0x25,
      newFunc = 0x26,
      assignFunc = 0x27,
      incFunc = 0x28, decFunc = 0x29,
      inclFunc = 0x2A, exclFunc = 0x2B,
      copyFunc = 0x2C,
      assertFunc = 0x2D,

      haltFunc = 0x35, chrFunc = 0x36,
      shortIntCast = 0x37, ordFunc = 0x38, entierFunc = 0x39, realCast = 0x3A,
      longRealCast = 0x3B, singletonSet = 0x3c 

   };

   const string &literal(code_type lit_type);
   void ProcCode(void);
   scope *Expr(int &code_id);
   void BlockCode(void);
   void CodeSection(void);
};


}  // namespace juice

// some global functions

void print_disclaimer(ostream &fout);

string capitalize(const string &s);

#endif // #ifndef __juice_slim_binary
