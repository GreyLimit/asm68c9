# asm68c9
## Overview
A simple (and simple minded) assembler for the Motorola 6809.


(updated Dec 2022).

A refreshed readme to replace the "place holder", but probably still insufficient.

Excerpts from the earlier document:

This is a relatively simple minded, simple assembler for the Motorola 6809 written as a single C source file accepting a fairly broad set of assembler directives accepting the main code related directives from the early Motorola code through to more recently accepted common directives.

It only accepts the Motorola 6809 op-codes, though with modest work I would expect it would be easily extended to cover the Hitachi 6309 derivatives.

Speed was not a design criteria, so the program executes at least three passes of the source code to ensure that all labels have "stabilised" on a set value, the last pass generating the output.

Some basic testing has been undertaken against a version of the "assist09.asm" source code originally published by Motorola and comparing the output of this against both the published output and that generated by another '09 assembler.  As this software currently stands it seems to produce the same resulting code as Motorola expected, and differences with the other assembler used can be tracked down to different interpretations of operator precedence and character constant handling.

December 2022 revisions:

The assembler has been "tweaked" to (optionally) accept original Motorola formatted source code as an alternative to a more modern syntax.  This is not a change in any op-codes or arguments but relates to handling of the following elements:

*	Labels starting in column 1 do not need terminate with a colon.
*	The period symbol is now valid within a label.
*	Character constants are preceded by only a single quote (').
*	String constants are delimited by the slash character (/).

Enabling 'legacy syntax' also enables a few possible warnings relating to potential clashes between the default syntax interpretation against the legacy version.

This may not be enough to assemble every original assembler file (or even most), but should assist in doing so.

Finally, the command line options accepted by the program (as displayed by '--help'):

```
Usage: asm68c9 [ {options} ] {filename}
Options:-
	--hex		Output raw hexadecimal values
	--intel		Output Intel Hex format data
	--motorola	Output Motorola S records
	--legacy-syntax	Accept legacy Motorola syntax
	--symbols	Output Symbol table
	--dump-opcodes	Display op-codes table
	--help		Display this help information
	--debug		Enable additional debugging output
```

Jeff
