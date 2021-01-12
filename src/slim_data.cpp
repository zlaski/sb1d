/******************************************************************************
 * slim_data.cpp                                                              *
 *                                                                            *
 * The Slim Binary(tm) Decoder                                                *
 *                                                                            *
 * Copyright (c) 1997-1999 by the Regents of the University of California     *
 * Copyright (c) 2000-2019 by Ziemowit Laski                                  *
 *                                                                            *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESSED  OR       *
 * IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED          *
 * WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR          *
 * PURPOSE.                                                                   *
 *                                                                            *
 *****************************************************************************/

#include <ctype.h>

//#include <sstream>
using namespace std;

#include "slim_data.h"

namespace juice {

// ==================================== DIV and MOD operators (Oberon-2 semantics)

long div(long n, long d) {
   long res = n / d;
   if (n % d < mod(n, d)) {
       res--;
   }
   return res;
}


long mod(long n, long d) {
   long res = n % d;
   if(res < 0) { 
       if (d > 0) {
	   res += d;
       }
       else res -= d;
   }
   return res;
}


char hex_value(char c) {
   if (isdigit(c)) {
      c -= '0';
   }
   else if(isxdigit(c)) {
      c -= (islower(c)? 'a': 'A');
      c += 10;
   }
   return c;
}


char hex_digit(char c) {
   static char digit[] = "0123456789ABCDEF";
   return digit[hex_value(c)];
}

const string &printable(const string &str) {
   static string lit;

   lit.erase();
   for(size_t i = 0; i < str.size(); i++) {
      unsigned char c = str[i];
      switch(c) {
         case '\\': lit += "\\\\"; break;
         case '\"': lit += "\\\""; break;
         case '\'': lit += "\\\'"; break;
         case ' ': lit += " "; break;
         default: {
            if(isprint(c) && !isspace(c)) lit += c;
            else {
               lit += "\\x";
               lit += hex_digit(c / 16);
               lit += hex_digit(c % 16);
            }
         }
      }
   }
   return lit;
}


// ==================================== slim data base class

void slim_datum::print_hex(ostream &out) {
   unsigned char c;
   for(size_t i = 0; i < str.size(); i++) {
      c = str[i];
      if(i) out << " ";
      out << hex_digit(c / 16) << hex_digit(c % 16);
   }
}


void slim_datum::assign_str(const string &rhs) {
   str = rhs;
   _calcval();
}


ostream &operator <<(ostream &out, const slim_datum &d) {
   for(size_t i = 0; i < d.str.size(); i++) {
      out << d.str[i];
   }
   return out;
}


// ==================================== slim integers

slim_int::slim_int(long i) {
   *this = i;
}


/* Writes a number in a compressed format. *)
	PROCEDURE WriteNum*(VAR R: Rider; x: LONGINT);
	BEGIN
		WHILE (x < - 64) OR (x > 63) DO
         Write(R, CHR(x MOD 128 + 128));
         x := x DIV 128
      END;
		Write(R, CHR(x MOD 128))
	END WriteNum;
*/

slim_int &slim_int::operator =(long i) {
    uint8_t c;
    val = i;
    str.erase();
    while(i < -64 || i > 63) {
       c = (uint8_t)mod(i, 128L) + 128;
       str += c;
       i = div(i, 128L);
    }
    c = (uint8_t)mod(i, 128L);
    str += c;
    return *this;
}


/* Reads a number in compressed variable length notation using the minimum amount of bytes. *)
	PROCEDURE ReadNum*(VAR R: Rider; VAR x: LONGINT);
		VAR s: SHORTINT; ch: CHAR; n: LONGINT;
	BEGIN s := 0; n := 0; Read(R, ch);
		WHILE ORD(ch) >= 128 DO INC(n, ASH(ORD(ch) - 128, s) ); INC(s, 7); Read(R, ch) END;
		x := n + ASH(ORD(ch) MOD 64 - ORD(ch) DIV 64 * 64, s)
	END ReadNum;
*/

void slim_int::_calcval(void) {
	unsigned char s = 0, ch;
   long i = 0;
   val = 0;
	ch = str[i++];
	while(ch >= 128) {
      val += ((ch - 128) << s);
      s += 7;
      ch = str[i++];
   }
   val += ((mod(ch, 64) - div((long)ch, (long)64) * 64) << s);
}


istream &operator >>(istream &in, slim_int &i) {
   int c;
   string int_str;
   do {
      c = in.get();
      int_str += (unsigned char)c;
   }
   while(c != char_traits<char>::eof() && c >= 128);
   i.assign_str(int_str);
   return in;
}


// ==================================== slim reals

static union {   // used to extract the individual bytes from a float
   float f;
   unsigned char c[4];
} rn;

slim_real::slim_real(float f) {
   *this = f;
}


slim_real &slim_real::operator =(float f) {
   val = rn.f = f;
   str.erase();
   for(char i = 0; i < 4; i++) str += rn.c[i];
	return *this;
}


void slim_real::_calcval(void) {
   for(size_t i = 0; i < str.size() && i < 4; i++) rn.c[i] = str[i];
   val = rn.f;
}


istream &operator >>(istream &in, slim_real &f) {
   for(char i = 0; i < 4; i++) rn.c[i] = in.get();
   f = rn.f;   // use operator = above
   return in;
}


// ==================================== slim long reals

static union {   // used to extract the individual bytes from a double
   double f;
   unsigned char c[8];
} lrn;

slim_longreal::slim_longreal(double f) {
   *this = f;
}


slim_longreal &slim_longreal::operator =(double f) {
   val = lrn.f = f;
   str.erase();
   for(char i = 0; i < 8; i++) str += lrn.c[i];
	return *this;
}


void slim_longreal::_calcval(void) {
   for(size_t i = 0; i < str.size() && i < 8; i++) lrn.c[i] = str[i];
   val = lrn.f;
}


istream &operator >>(istream &in, slim_longreal &f) {
   for(char i = 0; i < 8; i++) lrn.c[i] = in.get();
   f = lrn.f;   // use operator = above
   return in;
}


// ==================================== slim strings


slim_str::slim_str(const string &s) {
   *this = s;
}


/* ----- auxiliaries ----- *)

	PROCEDURE WString(VAR R: Files.Rider; VAR s: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN	i := 0; ch := s[i];
		IF ch = 0X THEN Files.Write(R, 0X); RETURN END;
		WHILE (ch # 0X) & (ch < 7FX) DO INC(i); ch := s[i] END;
		IF i > 1 THEN Files.WriteBytes(R, s, i-1) END;
		IF ch = 0X THEN Files.Write(R, CHR(ORD(s[i-1])+80H))
		ELSE
			IF i > 0 THEN Files.Write(R, s[i-1]) END;
			Files.Write(R, 7FX);
         REPEAT Files.Write(R, ch); INC(i); ch := s[i] UNTIL ch = 0X;
			Files.Write(R, 0X)
		END
	END WString;
*/

slim_str &slim_str::operator =(const string &s) {
   int i = 0; unsigned char ch;
   val = s;
   str.erase(); ch = s[i];
   if(!ch) { 
       str += (char)0x00; 
       return *this;
   }
   while(ch && ch < 0x7F) { 
       ch = s[++i];
   }
   if (i > 1) {
       str += s.substr(0, i - 1);
   }
   if (!ch) {
       str += (char)(s[i - 1] + 0x80);
   }
   else {
       if (i > 0) {
	   str += s[i - 1];
       }
      str += (char)0x7F;
      do { 
	  str += (char)ch; ch = s[++i];
      } while(ch);
      str += (char)0x00;
   }
   return *this;
}


/* ----- sym input ------ *)

	PROCEDURE ReadString*(VAR R: Files.Rider; VAR string: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN i := 0;
		LOOP
         Files.Read(R, ch);
			IF ch = 0X THEN string[i] := 0X; RETURN
			ELSIF ch < 7FX THEN string[i]:=ch; INC(i)
			ELSIF ch > 7FX THEN string[i] := CHR(ORD(ch)-80H); string[i+1] := 0X; RETURN
			ELSE (* ch = 7FX *) EXIT END
		END;
		LOOP
         Files.Read(R, ch);
			IF ch = 0X THEN string[i]:=0X; RETURN
			ELSE string[i]:=ch; INC(i) END
		END
	END ReadString;
*/

void slim_str::_calcval(void) {
   int j = 0; unsigned char ch;

   val.erase();
   while(true) {
      ch = str[j++];
      if (!ch) {
	  return;
      }
      else if (ch < 0x7F) {
	  val += ch;
      }
      else if(ch > 0x7F) {
	  val += (char)(ch - 0x80);
	  return;
      }
      else /* ch == 0x7F */ break;
   }
   while(true) {
      ch = str[j++];
      if(!ch) return;
      else val += ch;
   }
}


istream &operator >>(istream &in, slim_str &s) {
   int ch;
   s.str.erase();
   while(true) {
      ch = in.get();
      if (!ch || ch == char_traits<char>::eof()) {
	  goto compute_val;
      }
      else if (ch < 0x7F) {
	  s.str += (char)ch;
      }
      else if(ch > 0x7F) { 
	  s.str += (char)ch; goto compute_val;
      }
      else /* ch == 0x7F */ break;
   }
   while(true) {
      ch = in.get();
      if (!ch || ch == char_traits<char>::eof()) {
	  goto compute_val;
      }
      else {
	  s.str += (char)ch;
      }
   }
  compute_val:
   s._calcval();
   return in;
}


} // namespace stdext
