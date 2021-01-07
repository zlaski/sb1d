/******************************************************************************
 * string.cpp                                                                 *
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

#include "exception.h"
#include "string.h"

namespace std {

// decimal, hex and octal conversions for integral types

const string &dec(const long &val, int wide, char filler) {
   static string r;
   char buf[240];
   ostrstream s(buf, 240);

   s.setf(ios_base::dec, ios_base::basefield);
   if(wide) s.width(wide);
   s.fill(filler);
   s <<  val << '\0';
   r = s.str();
   return r;
}


const string &hex(const long &val, int wide, char filler) {
   static string r;
   char buf[240];
   ostrstream s(buf, 240);

   s.setf(ios_base::uppercase);
   s.setf(ios_base::hex, ios_base::basefield);
   if(wide) s.width(wide);
   s.fill(filler);
   s <<  val << '\0';
   r = s.str();
   return r;
}


const string &oct(const long &val, int wide, char filler) {
   static string r;
   char buf[240];
   ostrstream s(buf, 240);

   s.setf(ios_base::uppercase);
   s.setf(ios_base::oct, ios_base::basefield);
   if(wide) s.width(wide);
   s.fill(filler);
   s <<  val << '\0';
   r = s.str();
   return r;
}


// conversion between literal and printable string formats

// ==================================== digit

const int digit_val(const int c) {
   if(isdigit(c, locale::classic())) return c - '0';
   if(c >= 'A' && c <= 'F') return c - 'A' + 10;
   if(c >= 'a' && c <= 'f') return c - 'a' + 10;
   return -1;  // some large #
}


const int digit_str(const int d) {
   if(d < 0) return -1;
   if(d < 10) return d + '0';
   if(d < 16) return d - 10 + 'A';
   return -1;
}


// ==================================== literal

const string &literal(const unsigned char c) {
   static string s;
   switch(c) {
      case 7:  s = "\\a"; break;
      case 8:  s = "\\b"; break;
      case 9:  s = "\\t"; break;
      case 10: s = "\\n"; break;
      case 11: s = "\\v"; break;
      case 12: s = "\\f"; break;
      case 13: s = "\\r"; break;
      case '\\': s = "\\\\"; break;
      case '\'': s = "\\\'"; break;
      case '\"': s = "\\\""; break;
      default:
         if(isprint(c, locale::classic())) s = c;
         else s = "\\0x" + hex((long)c, 2);
         break;
   }
   return s;
}


const unsigned char literal(const string &s) {
   int i = 0;
   unsigned char c = s[i++];
   if(c == '\\') {
      switch(c = s[i++]) {
         case 'a': c = 7; break;
         case 'b': c = 8; break;
         case 't': c = 9; break;
         case 'n': c = 10; break;
         case 'v': c = 11; break;
         case 'f': c = 12; break;
         case 'r': c = 13; break;
         default:
            if(isdigit(c, locale::classic())) {
               //       \0xnn Hex
               //       \nnn  Octal
               int d = 0; char base = 8;
               if(c == '0') {
                  c = s[i++];
                  if(c == 'x' || c == 'X') { base = 16; c = s[i++]; }
               }
               while(digit_val(c) >= 0 && digit_val(c) < base) {
                  d *= base; d += digit_val(c);
                  c = s[i++];
               }
               c = d;
            }
            break;
      }
   }
   return c;
}


const unsigned char literal(ifstream &f) {
   int c = f.get();
   if(c == '\\') {
      switch(c = f.get()) {
         case 'a': c = 7; break;
         case 'b': c = 8; break;
         case 't': c = 9; break;
         case 'n': c = 10; break;
         case 'v': c = 11; break;
         case 'f': c = 12; break;
         case 'r': c = 13; break;
         default:
            if(isdigit(c, locale::classic())) {
               //       \0xnn Hex
               //       \nnn  Octal
               int d = 0; char base = 8;
               if(c == '0') {
                  c = f.get();
                  if(c == 'x' || c == 'X') { base = 16; c = f.get(); }
               }
               while(digit_val(c) < base) {
                  d *= base; d += digit_val(c);
                  c = f.get();
               }
               if(c != EOF) f.putback(c);
               c = d;
            }
            break;
      }
   }
   return c;
}


// ==================================== quoted_literal

const string &quoted_literal(const unsigned char c) {
   static string t;
   t = "\'" + literal(c) + "\'";
   return t;
}


const string &quoted_literal(const string &s) {
   static string t;
   t = "\"";
   for(size_t i = 0; i < s.size(); i++) {
      t += literal(s[i]);
   }
   t += "\"";
   return t;
}


}  // namespace std
