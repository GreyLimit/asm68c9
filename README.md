# as6809
A simple (and simple minded) assembler for the Motorola 6809.

A quick readme as a bit of a "place holder";

This is a relatively simple minded, simple assembler for the Motorola 6809 written as a single C source file accepting a fairly broad set of assembler directives accepting the main code related directives from the early Motorola code through to more recently accepted common directives.

It only accepts 6809 opcodes, though with modest work I would expect it would be easily extended to cover 6809 derivatives.

The program does not "output" any form of executable or object file, instead outputing lines starting with a 4 digit hex address and followed by one or more
2 digit hex byte values.  A symbol table is displayed.

Speed was not a design criteria, so the program executes at least three passes of the source code to ensure that all labels have "stabalised" on a set value, the last pass outputs the resulting hexidecimal data.

I wrote this to assist with the creation of a mc6809 emulator for which I needed to embed some "ROM" based software.  Assembling to text based Hexidecimal values enables me to post-process the output into a source file that can be included into the emulation.

So far (Late Jan '21) no comprehensive testing has been completed though an initial manual scan looks promising.

Jeff
