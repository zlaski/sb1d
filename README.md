# The Slim Binary&trade; Decoder

This software decompiles Slim Binary&trade; files back into the Oberon-2 source language.
For accompanying native (e.g., x86, PowerPC) binaries, the decoder will extract their 
external interfaces (but not procedure bodies).

## Getting Started

Download `sb1d.`_platform_`.7z` (where _platform_ is `Win32`, etc.) from this repository 
and extract the executable from the archive.

## Using the Decoder

Simply running
```
sb1d
```
will display usage instructions; `sb1d` is usually invoked with a single argument naming
the file to be decompiled.

Should the decompiled program reference other modules, the binaries corresponding to such 
modules must be present in the same folder.

Note that `sb1d` does _not_ support the decompilation of .COB files found in the XOberon 
RTOS, as those are low-level (PowerPC) binaries.

Enjoy!
