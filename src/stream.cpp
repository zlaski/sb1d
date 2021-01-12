/******************************************************************************
 * stream.cpp                                                                 *
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

#include <locale>
#include "stream.h"

namespace std {

bool file_exists(const string &name) {
   ifstream f(name.c_str());
   bool exists = f.is_open();
   f.close();
   return exists;
};

void split_filename(string &path, string &base) {

   // initially, base contains the entire filename
   path.erase();

   // special handling for parent and current directories
   if(base == "." || base == "..") {
      path = base + "//";
      base.erase();
      return;
   }

   int l = base.size();

   // search for the path name
   while(l > 0) {
      l--;
      char c = base[l];
      if(c == '/' || c == '\\' || c == ':') {
         path = base.substr(0, l + 1);
         base = base.substr(l + 1);
         break;
      }
   }
};

// UTF-8 streams

utf8ifstream::utf8ifstream(void): ifstream() {
  init();
};

utf8ifstream::utf8ifstream(const string &name): 
    ifstream(name.c_str(), ios_base::in | ios_base::binary) {
  init();
};

void utf8ifstream::init(void) {
  row = col = 1;
};

void utf8ifstream::open(const string &name) {
  ifstream::open(name.c_str(), ios_base::in | ios_base::binary);
};

void utf8ifstream::close(void) {
  ifstream::close();
};

utf8ifstream::int_type utf8ifstream::get(void) {
  int_type c = ifstream::get(), d;
  // sometimes, a single character is all it takes!
  if(c == char_traits<char>::eof()) {
    return c;
  }
  if(c == L'\\' && ifstream::peek() == L'u') {
    ifstream::get();  // eat the 'u'
    c = 0;
    // we allow fewer than 4 hex digits!
    for(char e = 0; e < 4; e++) {
      if((d = ifstream::get()) == char_traits<char>::eof()) {
        return d;
      };
      if(!isxdigit(d, locale::classic())) {
        break;
      }
      (c <<= 4) |= digit_val(d);
    }
  }
  else if(c >= L'\x00c0') {
    if(c >= L'\x00e0') { 
      (c &= 0x000f) <<= 12;
      if((d = ifstream::get()) == char_traits<char>::eof()) {
        return d;
      };
      c |= ((d & 0x003f) << 6);
    }
    else {
      (c &= 0x001f) <<= 6;
    }
    if((d = ifstream::get()) == char_traits<char>::eof()) {
      return d;
    };
    c |= (d & 0x003f);
  }
  // now c contains the 16-bit character
  col++;
  if(c == L'\r' || c == L'\n') {
    row++; col = 1;
    if(c == L'\r') {
      if(ifstream::peek() == L'\n') {
        ifstream::get();
      }
      c = L'\n';   // normalize to LF
    }
  }
  return c;
};

wstring utf8ifstream::getline(void) {
  wstring line;
  while(true) {
    int_type c = get();
    if(c == char_traits<char>::eof() || c == L'\n') {
      break;
    }
    line += (wchar_t)c;
  }
  return line;
};
  
void utf8ifstream::push(void) {
  pos_info info;
  info.pos = tellg();
  info.row = row; info.col = col;
  mark_stack.push(info);
};

void utf8ifstream::pop(void) /*throw(string_exception)*/ {
  if(mark_stack.empty()) {
    throw string_exception("std::utf8ifstream: position stack empty");
  }
  mark_stack.pop();
};

void utf8ifstream::reset(void) /*throw(string_exception)*/ {
  if(mark_stack.empty()) {
    throw string_exception("std::utf8ifstream: position stack empty");
  }
  pos_info &info = mark_stack.top();
  ifstream::seekg(info.pos);
  row = info.row; col = info.col;
};

utf8ofstream::utf8ofstream(void): ofstream() {
};

utf8ofstream::utf8ofstream(const string &name): 
    ofstream(name.c_str(), ios_base::out | ios_base::binary) {
};

void utf8ofstream::open(const string &name) {
  ofstream::open(name.c_str(), ios_base::out | ios_base::binary);
};

void utf8ofstream::close(void) {
  ofstream::close();
};

void utf8ofstream::put(const wchar_t ch) {
  if(ch <= L'\x007f') {
    ofstream::put((uint8_t)ch);
  }
  else {
    if(ch > L'\x07ff') {
      ofstream::put(0xe0 | ((ch & 0xf000) >> 12));
      ofstream::put(0x80 | ((ch & 0x0fc0) >> 6));
    }
    else {
      ofstream::put(0xc0 | ((ch & 0x07c0) >> 6));
    }
    ofstream::put(0x80 | (ch & 0x003f));
  }
};

void utf8ofstream::putstr(const wstring &str) {
  int l = str.length();
  for(int i = 0; i < l; i++) {
    put(str[i]);
  }
};

}  // namespace std
