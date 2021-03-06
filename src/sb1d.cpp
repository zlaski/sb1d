/******************************************************************************
 * sb1d.cpp                                                                   *
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

#include <iostream>
#include <cstring>
using namespace std;

#include "slim_binary.h"
using namespace juice;

#include "stream.h"
#include "time.h"

void print_disclaimer(const string &fdir, const string &filename, ostream &fout) {
   fout << "(***************************************************************************\n\n";
   fout << "   Input File:   " << fdir << filename << "\n";
   fout << "   Date/Time:    " << current_date_and_time() << "\n";
   fout << "   -------------------------------------------------------\n\n";
   fout << "   Generated by " SLIMBIN " Decoder Version " SB1D_VERSION "\n";
   fout << "\n";
   fout << " ***************************************************************************)\n\n";
}


int main(int argc, char *argv[]) {
   cout << SLIMBIN " Decoder Version " SB1D_VERSION "\n";
   cout << "(C) Copyright 1997-1999 Regents of the University of California\n";
   cout << "(C) Copyright 2000-2021 Ziemowit Laski\n\n";
   if(argc < 2 || argc > 5) {
      print_usage:
      cout << "   Usage: sb1d [-r|--raw] filename [-o|--output filename]\n\n";
      cout << "   Imported modules (.OBJ, .OBK, etc.) must reside in the same directory\n";
      cout << "   as the main module.  (The " FILEDIR_FILE " module-mapping file will be\n";
      cout << "   used if present.)  The \'--raw\' option activates a complete\n";
      cout << "   (and usually huge) dump of the decoder's internal data\n";
      cout << "   store (\'.Dump\').  Otherwise, an Oberon-2 source program (\'.Mod\')\n";
      cout << "   is generated.  In either case, the output is written into the\n";
      cout << "   current directory, unless an \'--output\' path is also specified.\n\n";
      return 1;
   }

   bool print_raw = false, specify_output = false;
   string path, filename, ext, outname;

   for (char** a = argv + 1; *a; ++a) {
       char* a0 = *a;
       if (!strcmp(a0, "-r") || !strcmp(a0, "--raw")) {
	   print_raw = true;
       }
       else if (!strcmp(a0, "-o") || !strcmp(a0, "--output")) {
	   specify_output = true;
       }
       else if (specify_output && outname.empty()) {
	   outname = a0;
       }
       else if (filename.empty()) {
	   filename = a[0];
       }
       else {
	   goto print_usage;
       }
   }

   if (filename.empty() || filename[0] == '-'
       || (specify_output && (outname.empty() || outname[0] == '-'))) {
       goto print_usage;
   }

   split_filename(path, filename);

   slim_binary my_binary;
   my_binary.set_directory(path);

   cout << "Reading file(s) . . . \n";
   try {
       bool success = my_binary.read(filename);

       //filename = capitalize(filename);
       if (outname.empty()) {
	   outname = filename + (print_raw ? ".Dump" : ".Mod");
       }
       ofstream fout(outname.c_str());
       cout << "\nWriting results . . . \n  " << outname << endl;
       if (!fout.is_open()) {
	   cout << "Fatal: Could not open \'" << outname << "\' for output\n";
	   return 1;
       }

       if (print_raw || success) {
	   print_disclaimer(path, filename, fout);
       }

       if (print_raw) {
	   my_binary.print(fout);
	   //cout << "Raw data written to \'" << outname << "\'\n";
       }
       else if (success) {
	   my_binary.pretty_print(fout);
	   //cout << "Decompiled source code written to \'" << outname << "\'\n";
       }
       fout.close();
   }
   catch (exception& e) {
       cout << "\n\nFATAL: " << e.what() << endl;
       return 1;
   }

   return 0;
}
