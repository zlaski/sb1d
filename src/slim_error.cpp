/******************************************************************************
 * slim_error.cpp                                                             *
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

#include "string.h"
using namespace std;

#include "slim_error.h"

namespace juice {

const char* slim_error::msg[] = {
    "Unknown Error",
    "Invalid Binary header ID",
    "Invalid Type ID",
    "Invalid Imported Type Name or ID",
    "Invalid Symbol Table section (0x39.. 0x3E expected)",
    "End of Symbol (0x3F) expected",
    "Invalid pointee type for POINTER",
    "Invalid base type for RECORD",
    "Invalid Code enumeration ID",
    "Invalid Imported Module ID",
    "Beginning of Code (0x09) expected",
    "End of Code Construct (0x00) expected",
    "End of WITH Construct (0x00 or 0x03) expected",
    "Invalid CASE expression type (must be integral -- 0x03.. 0x07)",
    "Type does not have a (Base) Type",
    "Missing name space for Type",
    "Cannot generate enumerations for entity",
    "Invalid type for a CONST symbol",
    "ASSERT condition should be negated",
    "Global Enumeration ID Not in Dictionary",
    "Local Enumeration ID Not in Dictionary",
    "Cannot open MODULE (.Obj) file"
};

string slim_error::build_msg(slim_errno e, const string& f, streampos& o) {
    string m;
    m += f;
    m += "[0";
    m += hex((long)o, 8);
    m += "H]: ";
    m += msg[e];
    return m;
}

slim_error::slim_error(slim_errno e, const string &f, streampos &o) throw():
   string_exception(build_msg(e, f, o)) {
   // nothing
}
slim_error::~slim_error(void) {
   // nothing
}

} // namespace juice
