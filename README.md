# The Slim Binary&trade; Decoder

This software decompiles Slim Binary&trade; files back into Oberon-2 source code.
For accompanying native (e.g., x86, PowerPC) binaries, the decoder will extract their 
external interfaces (but not procedure bodies).

## Getting Started

### Windows

Open and build the [Visual Studio project](win32/sb1d.vcxproj).  The result (`sb1d.exe`) will be found
inside the top-level [bld/](bld) folder.

### Other platforms (macOS, Linux, etc.)

Run the `make` command from the root directory (i.e., where this `README.md` file
resides).  The result (`sb1d`) will be found inside the top-level [bld/](bld) folder.

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

## Other Potentially Useful Stuff

### Oberon System 3

The [System3/](System3) folder contains binary distributions for Mac OS 7 and for Windows 3.1/95/NT.
The Windows binary seems to work on newer Win32 versions.  The Mac version should
_hopefully_ run on a PowerPC machine, maybe even under Mac OS X in classic mode,
but I haven't tried it.

### Caught on Safari

I came across vintage Slim Binary:tm: browser plugin sources 
[here](https://github.com/berkus/Juice)
and [here](https://github.com/Spirit-of-Oberon/Juice). Both appear to be Thomas Kistler's
original UC Irvine Juice:tm: distribution, and should supercharge your Netscape Navigator.

The vintage, pre-built Juice:tm: plug-ins for [Mac OS](doc/Juice12.sea.hqx)
and [Windows](doc/Juice11.zip) may be found in the [doc/](doc) folder.

### This Time We're Gonna Go back

The Internet Archive's [Wayback Machine](https://web.archive.org/) contains
ancient snapshots of the UC Irvine ICS web pages devoted to 
[Juice:tm:](https://web.archive.org/web/20061125105032/http://www.oberon.ws/UCI/juice/)
and [Oberon](http://web.archive.org/web/19990429113811/http://www.ics.uci.edu/~oberon/).
