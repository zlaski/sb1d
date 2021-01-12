/******************************************************************************
 * slim_binary.cpp                                                            *
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


#include <iomanip>
#include <algorithm>
using namespace std;
#include <ctype.h>
#include <string.h>

#include "scope.h"
#include "exception.h"
using namespace std;

#include "slim_binary.h"
#include "slim_error.h"
#include "slim_data.h"

namespace juice {

void slim_binary::set_directory(const string &dir) {
   fdir = dir;
}


bool slim_binary::open_module(const string &file, int pos) {
   string fn = LookupFile(file);
   fname = fdir + fn;
   f.open(fname.c_str(), ios_base::in | ios_base::binary);
   if(!f.is_open()) {
      return false;
   }

   current_module->slot("TYPE")->slot(bi->slot("MODULE"));  // record type and source file
   current_module->slot("ORIG FILENAME")->slot(file);
   current_module->slot("FILENAME")->slot(fn);
   current_module->slot("CANON FILENAME")->slot(LookupModule(file, false));

   gcode = current_module->slot("CODE ENUM");
   lcode = gcode;  // for now
   gtype = current_module->slot("TYPE ENUM");

   current_module->name = LookupModule(fn);
   // top("GLOBAL MODULE ENUM") < current_module->name;

   f.seekg(pos);
   return true;
}


// all of the following find or allocate, and return appropriate specializations
// of symbol_info

// ==================================== slim_binary: constructor, destructor

slim_binary::slim_binary(void) {
}


slim_binary::~slim_binary(void) {
   if(f.is_open()) f.close();
}


//
void slim_binary::FileMapping(void) {
    string files = fdir + FILEDIR_FILE;
    if (FILE * mapping = fopen(files.c_str(), "r")) {
        cout << "  " << fdir << FILEDIR_FILE ": File mapping\n";

        fseek(mapping, 0, SEEK_END);
        int size = ftell(mapping);
        fseek(mapping, 0L, SEEK_SET);
        char *m = (char *)malloc(size);
        fread(m, 1, size, mapping);
        char *r = m;
        while (r < m + size) {
            r += 2;
            string modulename(r);
            r += strlen(r) + 1;
            if (modulename == FILEDIR_FILE) {
                continue;  // no mapping for the mapping file :-)
            }
            string filename(r);
            r += strlen(r) + 1;
            // Module names are in mixed case (e.g., 'EditorPane.Obj') so
            // we capitalize them for easier lookup
            fileMap[capitalize(modulename)] = filename;
            moduleMap[filename] = modulename;
        }
        free(m);
        fclose(mapping);
    }
}
// ==================================== reading, writing

bool slim_binary::read(const string &name) {
    top.clear();
    top.name = name;
    bi = top.slot("BUILT IN");
    psym = curr = current_module = &top; // start at the top of the slim binary
    code_gen_phase = false;

    FileMapping();
    top.slot("GLOBAL MODULE ENUM")->slot(LookupModule(name, true))->slot(LookupModule(name, false));
    if (!open_module(name, 0)) {
        return false;
    }
    SlimBinary();
    return true;
}


// ==================================== slim_binary::SlimBinary (top-level read routine)

void slim_binary::SlimBinary(void) {

   PublicInterface(visible);  // this is read for every imported module as well...
   if(!current_module->slot("TYPE")->has_slot(SLIMBIN)) {
      return;
   }

   slim_int tbl_size, below, above;
   f >> tbl_size;   // read in size of private symbol table
   f >> below >> above;
   SymbolTable(hidden); // read in private symbol table

   ImportedKeys(); // do the fingerprint magic

   // /* DEBUG */ top.print(cout);
   code_gen_phase = true;
   CodeSection();
}


// ==================================== top-level read routine

void slim_binary::print(ostream &out) {
   top.print(out);
}

inline bool ends_with(std::string const & value, std::string const & ending)
{
    if (ending.size() > value.size()) return false;
    return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
}

string slim_binary::LookupFile(const string &f) {
    string i = f;
    unordered_map<string, string>::iterator t = fileMap.find(capitalize(f));
    if (t != fileMap.end()) {
        i = t->second;
    }
    return i;
}

string slim_binary::LookupModule(const string &f, bool truncExtension) {
    string i = f;
    unordered_map<string, string>::iterator t = moduleMap.find(capitalize(f));
    if (t != moduleMap.end()) {
        i = t->second;
    }

    size_t dotOffs = i.find_last_of('.');
    dotOffs = std::min(dotOffs, i.size());
    string truncated(i.begin(), i.begin() + dotOffs);
    return truncExtension? truncated: i;
}

} // namespace juice

string capitalize(const string &s) {
    string t = s;
    std::transform(t.begin(), t.end(), t.begin(), ::toupper);
    return t;
}


