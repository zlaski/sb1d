# The Slim Binary&trade; Decoder

This software decompiles Slim Binary&trade; files back into Oberon-2 source code.
For accompanying native (e.g., x86, PowerPC) binaries, the decoder will extract their 
external interfaces (but not procedure bodies).

## Getting Started

### Windows

Open and build the project `win32\sb1d.vcxproj`.  The result (`sb1d.exe`) will be found
inside the top-level `bld/` folder.

### Other platforms (macOS, Linux, etc.)

Run the `make` command from the root directory (i.e., where this `README.md` file
resides).  The result (`sb1d`) will be found inside the top-level `bld/` folder.

## Using the Decoder

Simply running
```
sb1d
```
will display usage instructions; `sb1d` is usually invoked with a single argument naming
the file to be decompiled.

Should the decompiled program reference other modules, the binaries corresponding to such 
modules must be present in the same folder.

Note that `sb1d` does _not_ support the decompilation of .COB files found on the XOberon 
RTOS, as those are low-level (PowerPC) binaries.

Please create an issue on GitHub if you run into problems.

Enjoy!
