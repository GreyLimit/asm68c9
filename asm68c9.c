///
///	asm68c9: Motorola 6809 Assembler.
///
///	This software is composed of a single source file:
///
///		asm68c9.c
///
///	Copyright (C) 2021, Jeff Penfold, jeff.penfold@googlemail.com
///	
///	This program is free software: you can redistribute it and/or modify
///	it under the terms of the GNU General Public License as published by
///	the Free Software Foundation, either version 3 of the License, or
///	(at your option) any later version.
///
///	This program is distributed in the hope that it will be useful,
///	but WITHOUT ANY WARRANTY; without even the implied warranty of
///	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///	GNU General Public License for more details.
///
///	You should have received a copy of the GNU General Public License
///	along with this program.  If not, see <https://www.gnu.org/licenses/>.
///

///
///	I give you another 6809 assembler: "asm68c9".
///
///	Yet another assembler for a defunct retro CPU.
///
///	Features:
///
///	o	Output to hexadecimal text
///	o	Intel '.hex' format
///	o	Motorola '.srec' format
///	o	Directed to screen or file
///	o	Optional support for some legacy syntax
///	o	Wide(ish) selection of assembler directives
///		supported
///

//
//	Include system definitions
//
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <malloc.h>
#include <alloca.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <limits.h>

//
//	We define some basic types which work well
//	with an assembler implementation.
//
typedef uint8_t bool;
typedef uint8_t byte;
typedef uint16_t word;

//
//	Some basic constants.
//
#define ERROR		(-1)
#define BUFFER		1024
#define MAX_CONSTANTS	64
#define MAX_INSTRUCTION	8
#define MAX_FILL	(16*1024)

#define true		(0==0)
#define false		(0==1)

#define EOS		'\0'
#define QUOTE		'\''
#define QUOTES		'\"'
#define ESCAPE		'\\'
#define SLASH		'/'
#define STAR		'*'
#define USCORE		'_'
#define PERIOD		'.'
#define ZERO		'0'
#define NINE		'9'
#define DOLLAR		'$'
#define PERCENT		'%'
#define COLON		':'
#define SPACE		' '
#define AT		'@'

//
//	Syntactic sugar
//
#define FUNC(a)		(*(a))
#define NEW(t)		((t *)malloc(sizeof( t )))
#define STACK(t)	((t *)alloca(sizeof( t )))
#define STACK_N(t,n)	((t *)alloca(sizeof( t )*(n)))

#define H(v)		(((v)>>8)&0xff)
#define L(v)		((v)&0xff)

//
//	Define a set of flags that are used to configure
//	and control the assembler.  These are OR'd together
//	into the option_flags variable.
//
typedef enum {
	OPTIONS_NONE		= 0000000,
	//
	DISPLAY_TEXT		= 0000001,
	DISPLAY_INTEL		= 0000002,
	DISPLAY_MOTOROLA	= 0000004,
	DISPLAY_NOTHING		= 0000010,
	DISPLAY_LISTING		= 0000020,
	DISPLAY_MASK		= (DISPLAY_TEXT|DISPLAY_INTEL|DISPLAY_MOTOROLA|DISPLAY_NOTHING|DISPLAY_LISTING),
	DISPLAY_STDOUT		= 0000040,
	//
	RELOCATION_DATA		= 0000100,
	___0000200___		= 0000200,
	___0000400___		= 0000400,
	//
	LEGACY_SYNTAX		= 0001000,
	DISPLAY_SYMBOLS		= 0002000,
	HITACHI_6309		= 0004000,
	//
	DISPLAY_OPCODES		= 0010000,
	DISPLAY_HELP		= 0020000,
	DISPLAY_DEBUG		= 0040000,
	DISPLAY_VEBOSE		= 0100000
} program_options;

//
//	This is where the flags are combined.
//
static program_options option_flags = OPTIONS_NONE;

//
//	COMPILE TIME DEBUGGING
//	======================
//
//	Define ENABLE_DEBUG to include options
//	for including additional checking code
//	and optional debugging output messages.
//

#ifdef ENABLE_DEBUG
//
//	Define for debugging.
//
#define ASSERT(v)		do{ if(!(v)) { fprintf( stderr, "Assert Failed \"%s\" (%s:%d:%s).\n", #v, __FILE__, __LINE__, __FUNCTION__ ); abort(); }}while(0)
#define PRINT_DEBUG(a)		do{ if( option_flags & DISPLAY_DEBUG ) printf a; }while(0)
#define PRINT_VERBOSE(a)	do{ if( option_flags & DISPLAY_VEBOSE ) printf a; }while(0)

#else
//
//	Define for not debugging.
//
#define ASSERT(v)
#define PRINT_DEBUG(a)
#define PRINT_VERBOSE(a)

#endif

//
//	The ABORT macro is always fully specified.  What is
//	the point of crashing without giving some indication
//	of why or where?
//
#define ABORT(m)	do{ fprintf( stderr, "Program Abort \"%s\" (%s:%d:%s).\n", (m), __FILE__, __LINE__, __FUNCTION__ ); abort(); }while(0)



//
//	Syntax and Semantics
//	====================
//
//	Start by defining what constitutes an op code
//	and its supporting (optional) argument.
//
//	Excluding the assemblers intrinsic commands,
//	opcodes are defined in-line with the Motorola
//	standard (in either upper or lower case).
//
//	The optional argument is one of the following
//	forms:
//
//	Immediate:		#value
//
//		value is placed into the target register
//		inherently associated with the opcode.
//
//	Extended:		value
//
//		16-bit value is used as an absolute address
//		from which the data to be used is gathered.
//
//	Direct Page:		value
//
//		8-bit value is used as the bottom half of an
//		address which is combined with the DP register
//		as the top half.  The data gathered is found
//		at the resulting address.
//
//		The assembler needs a consistent and clear
//		method for distinguishing between "Extended" and
//		"Direct Page" modes.  The syntax of the assembler
//		does not offer this but used the following method:
//
//		A "symbolic" in-assembler DP register is maintained
//		(initially with a value of 0), and if an apparent
//		"Extended" access has the same top byte value as this
//		symbolic DP register then the argument is taken to
//		to be a "Direct Page" instead.
//
//		This implies that there is a symbolic opcode for
//		the value of this DP register:- "SETDP".
//
//	Offset Index:		value,ireg
//
//		The address where the data to be gathered from is
//		calculated as the result of the content of the index
//		register specified with a constant offset applied to
//		it.
//
//		The assembler has a range of possible opcodes it can
//		generate, based on the size of the constant offset.
//		has no impact on the assembler syntax.
//
//	Indirect Offset Index:	[value,ireg]
//
//		An indirect version of the above where the "value,ireg"
//		generates an address from with the actual required
//		address is recovered.
//
//	Acc Offset Index:	areg,ireg
//
//		The address generated is calculated as the sum of an
//		accumulator (sign extrended if 8-bit) added to an
//		index register.
//
//	Ind Acc Offset Index:	[areg,ireg]
//
//	Post increment (+1):	,ireg+
//	Post increment (+2):	,ireg++
//	Pre decrement (-1):	,-ireg
//	Pre decrement (-2):	,--ireg
//	Ind Post inc (+2):	[,ireg++]
//	Ind Pre dec (-2):	[,--ireg]
//	Relative:		value,PC
//	Ind Relative:		[value,PC]
// 	Indirect Extended	[value]
//	Where:-
//
//		areg: A, B or D
//		ireg: X, Y, U or S
//
//	As a note on terminology; All addresing modes (with the
//	exception the immediate value) generate an Effective Address
//	(EA) which is where data is either gathered from (or placed
//	into).
//
//	Legacy Syntax
//	=============
//
//	There are a number of twists in the accepted syntax for
//	6809 assembly language.  The following details capture
//	those that I have recognised so far:
//
//	Numerical Representation
//	------------------------
//
//	The presentation of numeric values not in base 10 have
//	effectively three cases that have been employed which
//	can be characterised by the following descriptions:
//
//	Symbol Prefix		A unique symbol is set aside for the
//				introduction non-decimal base numbers.
//				Within the assembly world the following
//				symbols have been used by 6800/6809
//				assemblers:
//
//				'$'	Hexadecimal	0-9,A-F
//				'@'	Octal		0-7
//				'%'	Binary		0,1
//
//	Suffixes		The base of a number is determined by the
//				last character of its representation.  The
//				following characters are those that I have
//				determined (so far):
//
//				H|h	Hexadecimal	0-9,A-F
//				B|b	Binary		0-7
//
//	C Language		While (as far as I am aware) the C language
//				system of prefix characters does not have
//				its basis in assembly languages, but has
//				become a familiar and broadly applied syntax:
//
//				0x___	Hexadecimal	0-9,A-F
//				0___	Octal		0-7
//				___	Decimal		0-9
//
//	Data Sizing
//	-----------
//
//	The 6809 CPU can address memory in a number of ways either through
//	registers containing an address or though the program containing
//	immediate values.  In the case of hard coded immediate values the
//	CPU support two approaches characterised as 'Direct' and 'Extended'
//	that align with an 8-bit value for Direct access and a 16-bit value
//	for extended access.  Obviously an 8-bit value is insufficient for
//	more than 256 bytes of RAM, but the CPU takes the content of the DP
//	register (Direct Page) to provide the missing upper 8-bits of the
//	address.
//
//	Setting the DP register and access RAM through Direct mode is
//	faster than accessing the same memory using 16-bit Extended access and
//	reduces the size of the program.
//
//	The assembler, with the assistance of the SETDP assembler directive,
//	can reliably choose which access mode to use based on the value of
//	the address provided.
//
//	Historically assemblers have provided two symbols that could be
//	seen as 'hints' or explicit 'directive' to the assembler and guide
//	how the assembler instruction was translated into machine code:
//
//	'<'	Direct mode
//	'>'	Extended mode
//
//	So, placing one of these symbols ahead of an address or label
//	would direct the assembler to generate an instruction using the
//	indicated access mode.
//
//	However, There are historic examples of assembler source code
//	where (notably the direct symbol) precedes access modes other
//	than direct page access.
//
//	The conclusion is that these symbols should be interpreted in an
//	alternative fashion, thus:
//
//	'<'	Use as 8-bit byte value
//	'>'	Use as 16-bit word value
//
//	This assembler will take the approach outlined above which is a
//	more flexible system and remains compatible with legacy code
//	(when used with or without enabling legacy mode).
//	

//
//	Define symbolic values representing each of the effective
//	address modes available in the processor.
//
typedef enum {
	EA_NONE = 0,			// Invalid EA mode
	//
	EA_OFFSET_INDEX,		// value,ireg
	EA_IND_OFFSET_INDEX,		// [value,ireg]
	EA_ACC_INDEX,			// areg,ireg
	EA_IND_ACC_INDEX,		// [areg,ireg]
	EA_POST_INC_ONE,		// ,ireg+
	EA_POST_INC_TWO,		// ,ireg++
	EA_PRE_DEC_ONE,			// ,-ireg
	EA_PRE_DEC_TWO,			// ,--ireg
	EA_IND_POST_INC_TWO,		// [,ireg++]
	EA_IND_PRE_DEC_TWO,		// [,--ireg]
	EA_OFFSET_PC,			// value,PC
	EA_IND_OFFSET_PC,		// [value,PC]
	EA_IND_EXTENDED			// [value]
} ea_mode;

//
//	Define symbolic values which summarise the various
//	opcode types (ie what sort of argument data the opcode
//	is expecting).
//
typedef enum {
	OP_NONE = 0,			// Invalid OP mode
	//
	OP_INHERENT,			// No argument required
	OP_IMM_UNDEF,			// #value (size not defined)
	OP_IMM_BYTE,			// #value (8-bit)
	OP_IMM_WORD,			// #value (16-bit)
	OP_DIRECT,			// value (8-bit with DP)
	OP_EXTENDED,			// value (16-bit)
	OP_EADRS,			// Effective Address
	OP_SRELATIVE,			// -128..+127 relative
	OP_LRELATIVE,			// -32768..32767 relative
	OP_REG_PAIR,			// Exchange or Transfer
	OP_REG_LIST			// Push or Pull
} op_mode;

//
//	Define a set of Argument extensions which modify
//	the base machine instruction (before additional
//	instruction byte are appended).
//
typedef struct {
	op_mode	mode;
	word	adds;
} op_arg;

//
//	We define here each of the adjustes a machine
//	instruction has as a result of the nature of its
//	applied argument.
//

//
//	Direct / Indexed / Extended : $0?, $6? - $7?
//
static op_arg arg_dir_ind_ext1[] = {{ OP_DIRECT, 0x0000 }, { OP_EADRS, 0x0060 }, { OP_EXTENDED, 0x0070 }, { OP_NONE }};

//
//	Short Relative
//
static op_arg arg_srel[] = {{ OP_SRELATIVE, 0x0000 }, { OP_NONE }};

//
//	Long Relative
//
static op_arg arg_lrel[] = {{ OP_LRELATIVE, 0x0000 }, { OP_NONE }};

//
//	Immediate / Direct / Indexed / Extended : $8? - $f?
//
static op_arg arg_imm_dir_ind_ext8[] = {{ OP_IMM_BYTE, 0x0000 }, { OP_DIRECT, 0x0010 }, { OP_EADRS, 0x0020 }, { OP_EXTENDED, 0x0030 }, { OP_NONE }};
static op_arg arg_imm_dir_ind_ext16[] = {{ OP_IMM_WORD, 0x0000 }, { OP_DIRECT, 0x0010 }, { OP_EADRS, 0x0020 }, { OP_EXTENDED, 0x0030 }, { OP_NONE }};

//
//	Direct / Indexed / Extended : $8? - $f?
//
static op_arg arg_dir_ind_ext2[] = {{ OP_DIRECT, 0x0010 }, { OP_EADRS, 0x0020 }, { OP_EXTENDED, 0x0030 }, { OP_NONE }};

//
//	Indexed
//
static op_arg arg_ind[] = {{ OP_EADRS, 0x0000 }, { OP_NONE }};

//
//	Immediate
//
static op_arg arg_imm8[] = {{ OP_IMM_BYTE, 0x0000 }, { OP_NONE }};

//
//	Reg Pair (Exchange)
//
static op_arg arg_pair[] = {{ OP_REG_PAIR, 0x0000 }, { OP_NONE }};

//
//	Reg List (Push/Pull)
//
static op_arg arg_list[] = {{ OP_REG_LIST, 0x0000 }, { OP_NONE }};

//
//	An inherent argument (ie none!)
//
static op_arg arg_inherent[] = {{ OP_INHERENT, 0x0000 }, { OP_NONE }};

//
//	Define a set of "op code" extensions which modify
//	the base machine instruction (before additional
//	instruction byte are appended).
//
typedef struct {
	char	*extn;
	word	adds;
} op_ext;

//
//	We define here a set of static tables which capture
//	each of the ways the basic opcode is modified by the
//	extend character appended to the base opcode.
//


//
//	A / B : $4? - $5?
//
static op_ext ext_ab1[] = {{ "a", 0x0000 }, { "b", 0x0010 }, { NULL }};

//
//	X / Y / S / U : $?0 - $?3
//
static op_ext ext_xysu1[] = {{ "x", 0x0000 }, { "y", 0x0001 }, { "s", 0x0002 }, { "u", 0x0003 }, { NULL }};

//
//	S / U : $?4 - $?7
//
static op_ext ext_su1[] = {{ "s", 0x0000 }, { "u", 0x0002 }, { NULL }};

//
//	A / B : $8? - $F?
//
static op_ext ext_ab2[] = {{ "a", 0x0000 }, { "b", 0x0040 }, { NULL }};

//
//	"add" extensions
//
static op_ext ext_add8[] = {{ "a", 0x0000 }, { "b", 0x0040 }, { NULL }};
static op_ext ext_add16[] = {{ "d", 0x0038 }, { NULL }};

//
//	"sub" extensions
//
static op_ext ext_sub8[] = {{ "a", 0x0000 }, { "b", 0x0040 }, { NULL }};
static op_ext ext_sub16[] = {{ "d", 0x0003 }, { NULL }};

//
//	X / Y / U / S : $8? - $F?
//
//	I encoded the follow list, but do not seem to have
//	used it anywhere.  This is either an indication that
//	I have missed an instruction group, or that I resolved
//	this in another way.  It's been too long for me to
//	determine which :-/
//
//static op_ext ext_xysu2[] = {{ "x", 0x0000 }, { "u", 0x0040 }, { "y", 0x1000 }, { "s", 0x1040 }, { NULL }};

//
//	"cmp" extensions
//
static op_ext ext_cmp8[] = {{ "a", 0x0000 }, { "b", 0x0040 }, { NULL }};
static op_ext ext_cmp16[] = {{ "d", 0x1002 }, { "s", 0x110b }, { "u", 0x1102 }, { "x", 0x000b }, { "y", 0x100b }, { NULL }};

//
//	"ld" and "st" extensions
//
static op_ext ext_ld_st8[] = {{ "a", 0x0000 }, { "b", 0x0040 }, { NULL }};
static op_ext ext_ld_st16[] = {{ "d", 0x0046 }, { "s", 0x1048 }, { "u", 0x0048 }, { "x", 0x0008 }, { "y", 0x1008 }, { NULL }};

//
//	An empty extension
//
static op_ext ext_none[] = {{ "", 0x0000 }, { NULL }};

//
//	Branch Codes : $?0 - $?f
//
//	Note duplicate masks for hs/cc and lo/cs, same tests different names.
//
static op_ext ext_sbra[] = {
	{ "ra", 0x0000 }, { "rn", 0x0001 }, { "hi", 0x0002 }, { "ls", 0x0003 },
	{ "hs", 0x0004 }, { "cc", 0x0004 }, { "lo", 0x0005 }, { "cs", 0x0005 },
	{ "ne", 0x0006 }, { "eq", 0x0007 }, { "vc", 0x0008 }, { "vs", 0x0009 },
	{ "pl",	0x000a }, { "mi", 0x000b }, { "ge", 0x000c }, { "lt", 0x000d },
	{ "gt", 0x000e }, { "le", 0x000f },
	{ NULL }
};
static op_ext ext_lbra[] = {
	                  { "rn", 0x0001 }, { "hi", 0x0002 }, { "ls", 0x0003 },
	{ "hs", 0x0004 }, { "cc", 0x0004 }, { "lo", 0x0005 }, { "cs", 0x0005 },
	{ "ne", 0x0006 }, { "eq", 0x0007 }, { "vc", 0x0008 }, { "vs", 0x0009 },
	{ "pl",	0x000a }, { "mi", 0x000b }, { "ge", 0x000c }, { "lt", 0x000d },
	{ "gt", 0x000e }, { "le", 0x000f },
	{ NULL }
};

//
//	Software Interrupt extensions
//
static op_ext ext_swi[] = {{ "1", 0x0000 }, { "2", 0x1000 }, { "3", 0x1100 }, { NULL }};

//
//	Define machine code instruction flags.
//
//	This are, primarily, associated with selection
//	of the instruction set to be supported (the ISA,
//	Instruction Set Architecture) or optional
//	extensions specific to this assembler.
//
//	ISO is short for 'Instruction Set Option':
//
typedef enum {
	ISO_NONE		= 00000,	// No target ISA selected
	ISO_MC6809		= 00001,	// Original documented Motorola instructions
	ISO_UNDOCUMENTED	= 00002,	// Undocumented Motorola instructions
	ISO_HD6309		= 00004,	// Hitachi extended instuctions
	ISO_ALIASES		= 00010		// Include meaningful instruction aliases
						//	where this aids clarity.
} inst_options;

//
//	Declare a global flag to hold the current
//	set of instruction set options being applied.
//
static inst_options	iso_options = ISO_NONE;

//
//	Define a data structure which captures an opcode and
//	its supported modes with rules to convert to target
//	binary machine code.
//
typedef struct {
	char		*name;			// The initial op code name
	word		len;			// Length of the initial op code name
	inst_options	iso;			// Options applicable to this instruction
	word		base;			// Base value for the instruction
	op_ext		*exts;			// List of valid extentions
	op_arg		*args;			// List of valid arguments
} op_code;


////////////////////////////////////////////////////////////////////////
//
//	Define the primary operation codes lookup table
//
////////////////////////////////////////////////////////////////////////

static op_code instruction_table[] = {
	{ "abx",	3,	ISO_MC6809,		0x003a,	ext_none,	arg_inherent		},//
	{ "adc",	3,	ISO_MC6809,		0x0089,	ext_ab2,	arg_imm_dir_ind_ext8	},//
	{ "add",	3,	ISO_MC6809,		0x008b,	ext_add8,	arg_imm_dir_ind_ext8	},//
	{ "add",	3,	ISO_MC6809,		0x008b,	ext_add16,	arg_imm_dir_ind_ext16	},//
	{ "and",	3,	ISO_MC6809,		0x0084,	ext_ab2,	arg_imm_dir_ind_ext8	},//
	{ "andcc",	5,	ISO_MC6809,		0x001c,	ext_none,	arg_imm8		},//
	{ "asl",	3,	ISO_MC6809,		0x0008,	ext_none,	arg_dir_ind_ext1	},//
	{ "asl",	3,	ISO_MC6809,		0x0048,	ext_ab1,	arg_inherent		},//
	{ "asr",	3,	ISO_MC6809,		0x0007,	ext_none,	arg_dir_ind_ext1	},//
	{ "asr",	3,	ISO_MC6809,		0x0047,	ext_ab1,	arg_inherent		},//
	{ "b",		1,	ISO_MC6809,		0x0020, ext_sbra,	arg_srel		},//
	{ "bit",	3,	ISO_MC6809,		0x0085,	ext_ab2,	arg_imm_dir_ind_ext8	},//
	{ "bsr",	3,	ISO_MC6809,		0x008d,	ext_none,	arg_srel		},//
	{ "clr",	3,	ISO_MC6809,		0x000f,	ext_none,	arg_dir_ind_ext1	},//
	{ "clr",	3,	ISO_MC6809,		0x004f,	ext_ab1,	arg_inherent		},//
	{ "cmp",	3,	ISO_MC6809,		0x0081,	ext_cmp8,	arg_imm_dir_ind_ext8	},//
	{ "cmp",	3,	ISO_MC6809,		0x0081,	ext_cmp16,	arg_imm_dir_ind_ext16	},//
	{ "com",	3,	ISO_MC6809,		0x0003,	ext_none,	arg_dir_ind_ext1	},//
	{ "com",	3,	ISO_MC6809,		0x0043,	ext_ab1,	arg_inherent		},//
	{ "cwai",	4,	ISO_MC6809,		0x003c,	ext_none,	arg_imm8		},//
	{ "daa",	3,	ISO_MC6809,		0x0019,	ext_none,	arg_inherent		},//
	{ "dec",	3,	ISO_MC6809,		0x000a,	ext_none,	arg_dir_ind_ext1	},//
	{ "dec",	3,	ISO_MC6809,		0x004a,	ext_ab1,	arg_inherent		},//
	{ "eor",	3,	ISO_MC6809,		0x0088,	ext_ab2,	arg_imm_dir_ind_ext8	},//
	{ "exg",	3,	ISO_MC6809,		0x001e,	ext_none,	arg_pair		},//
	{ "inc",	3,	ISO_MC6809,		0x000c,	ext_none,	arg_dir_ind_ext1	},//
	{ "inc",	3,	ISO_MC6809,		0x004c,	ext_ab1,	arg_inherent		},//
	{ "jmp",	3,	ISO_MC6809,		0x000e,	ext_none,	arg_dir_ind_ext1	},//
	{ "jsr",	3,	ISO_MC6809,		0x008d,	ext_none,	arg_dir_ind_ext2	},//
	{ "lb",		2,	ISO_MC6809,		0x1020, ext_lbra,	arg_lrel		},//
	{ "lbra",	4,	ISO_MC6809,		0x0016,	ext_none,	arg_lrel		},//
	{ "lbsr",	4,	ISO_MC6809,		0x0017,	ext_none,	arg_lrel		},//
	{ "ld",		2,	ISO_MC6809,		0x0086,	ext_ld_st8,	arg_imm_dir_ind_ext8	},//
	{ "ld",		2,	ISO_MC6809,		0x0086,	ext_ld_st16,	arg_imm_dir_ind_ext16	},//
	{ "lea",	3,	ISO_MC6809,		0x0030, ext_xysu1,	arg_ind			},//
	{ "lsl",	3,	ISO_MC6809,		0x0008,	ext_none,	arg_dir_ind_ext1	},//
	{ "lsl",	3,	ISO_MC6809,		0x0048,	ext_ab1,	arg_inherent		},//
	{ "lsr",	3,	ISO_MC6809,		0x0004,	ext_none,	arg_dir_ind_ext1	},//
	{ "lsr",	3,	ISO_MC6809,		0x0044,	ext_ab1,	arg_inherent		},//
	{ "mul",	3,	ISO_MC6809,		0x003d, ext_none,	arg_inherent		},//
	{ "neg",	3,	ISO_MC6809,		0x0000,	ext_none,	arg_dir_ind_ext1	},//
	{ "neg",	3,	ISO_MC6809,		0x0040,	ext_ab1,	arg_inherent		},//
	{ "nop",	3,	ISO_MC6809,		0x0012,	ext_none,	arg_inherent		},//
	{ "or",		2,	ISO_MC6809,		0x008a,	ext_ab2,	arg_imm_dir_ind_ext8	},//
	{ "orcc",	4,	ISO_MC6809,		0x001a, ext_none,	arg_imm8		},//
	{ "psh",	3,	ISO_MC6809,		0x0034, ext_su1,	arg_list		},//
	{ "pul",	3,	ISO_MC6809,		0x0035, ext_su1,	arg_list		},//
	{ "rol",	3,	ISO_MC6809,		0x0009,	ext_none,	arg_dir_ind_ext1	},//
	{ "rol",	3,	ISO_MC6809,		0x0049,	ext_ab1,	arg_inherent		},//
	{ "ror",	3,	ISO_MC6809,		0x0006,	ext_none,	arg_dir_ind_ext1	},//
	{ "ror",	3,	ISO_MC6809,		0x0046,	ext_ab1,	arg_inherent		},//
	{ "rti",	3,	ISO_MC6809,		0x003b,	ext_none,	arg_inherent		},//
	{ "rts",	3,	ISO_MC6809,		0x0039,	ext_none,	arg_inherent		},//
	{ "sbc",	3,	ISO_MC6809,		0x0082,	ext_ab2,	arg_imm_dir_ind_ext8	},//
	{ "sex",	3,	ISO_MC6809,		0x001d,	ext_none,	arg_inherent		},//
	{ "st",		2,	ISO_MC6809,		0x0087,	ext_ld_st8,	arg_dir_ind_ext2	},//
	{ "st",		2,	ISO_MC6809,		0x0087,	ext_ld_st16,	arg_dir_ind_ext2	},//
	{ "sub",	3,	ISO_MC6809,		0x0080,	ext_sub8,	arg_imm_dir_ind_ext8	},//
	{ "sub",	3,	ISO_MC6809,		0x0080,	ext_sub16,	arg_imm_dir_ind_ext16	},//
	{ "swi",	3,	ISO_MC6809,		0x003f, ext_none,	arg_inherent		},//
	{ "swi",	3,	ISO_MC6809,		0x003f,	ext_swi,	arg_inherent		},//
	{ "sync",	4,	ISO_MC6809,		0x0013, ext_none,	arg_inherent		},//
	{ "tfr",	3,	ISO_MC6809,		0x001f,	ext_none,	arg_pair		},//
	{ "tst",	3,	ISO_MC6809,		0x000d,	ext_none,	arg_dir_ind_ext1	},//
	{ "tst",	3,	ISO_MC6809,		0x004d,	ext_ab1,	arg_inherent		},//
	{ NULL }
};

////////////////////////////////////////////////////////////////////////
//
//	Simple routine to dump the opcode tables in full
//	to enable proof of coverage
//
////////////////////////////////////////////////////////////////////////

static void _displ_opcode( inst_options iso, char *name, char *extn, char *param, word inst ) {
	int	hi, lo;
	char	M, U, H, A;

	M = ( iso & ISO_MC6809 )? 'M': '-';
	U = ( iso & ISO_UNDOCUMENTED )? 'U': '-';
	H = ( iso & ISO_HD6309 )? 'H': '-';
	A = ( iso & ISO_ALIASES )? 'A': '-';

	lo = inst & 0xff;
	hi = ( inst >> 8 )&0xff;

	printf( "%c%c%c%c\t%s%s\t%s\t", M,U,H,A, name, extn, param );
	if( hi ) {
		printf( "%02X %02X\n", hi, lo );
	}
	else {
		printf( "%02X\n", lo );
	}
}

static void _dump_opcodes( inst_options iso, char *name, char *extn, word inst, op_arg *args ) {
	if( args ) {
		for( op_arg *ar = args; ar->mode != OP_NONE; ar++ ) {
			switch( ar->mode ) {
				case OP_INHERENT: {
					_displ_opcode( iso, name, extn, "", inst + ar->adds );
					break;
				}
				case OP_IMM_BYTE: {
					_displ_opcode( iso, name, extn, "#byte", inst + ar->adds );
					break;
				}
				case OP_IMM_WORD: {
					_displ_opcode( iso, name, extn, "#word", inst + ar->adds );
					break;
				}
				case OP_DIRECT: {
					_displ_opcode( iso, name, extn, "<value", inst + ar->adds );
					break;
				}
				case OP_EXTENDED: {
					_displ_opcode( iso, name, extn, ">value", inst + ar->adds );
					break;
				}
				case OP_EADRS: {
					_displ_opcode( iso, name, extn, "indexed", inst + ar->adds );
					break;
				}
				case OP_SRELATIVE: {
					_displ_opcode( iso, name, extn, "srel", inst + ar->adds );
					break;
				}
				case OP_LRELATIVE: {
					_displ_opcode( iso, name, extn, "lrel", inst + ar->adds );
					break;
				}
				case OP_REG_PAIR: {
					_displ_opcode( iso, name, extn, "r1,r2", inst + ar->adds );
					break;
				}
				case OP_REG_LIST: {
					_displ_opcode( iso, name, extn, "r1..rn", inst + ar->adds );
					break;
				}
				default: ABORT( "Unrecognised argument mode" );
			}
		}
	}
	else {
		_displ_opcode( iso, name, extn, "", inst );
	}
}

static void dump_opcodes( void ) {
	for( op_code *op = instruction_table; op->name; op++ ) {
		if( op->exts ) {
			for( op_ext *ex = op->exts; ex->extn; ex++ ) {
				_dump_opcodes( op->iso, op->name, ex->extn, op->base + ex->adds, op->args );
			}
		}
		else {
			_dump_opcodes( op->iso, op->name, "", op->base, op->args );
		}
	}
}

////////////////////////////////////////////////////////////////////////
//
//	Primitive Error collating systems to simplify
//	error coordination with input data.
//
////////////////////////////////////////////////////////////////////////

//
//	Define the maximum number of errors we will cache.
//
#define MAX_WARNING_CACHE	10
#define MAX_ERROR_CACHE		10

//
//	The fixed string warning and error cache.
//
static char *cached_warning[ MAX_WARNING_CACHE ];
static char *cached_error[ MAX_ERROR_CACHE ];

//
//	How many cached to far
//
static int warning_cache = 0;
static int error_cache = 0;

//
//	routines to log warning and error strings, recover such strings,
//	and clear the caches.
//

static void log_warning( char *text ) {
	if( warning_cache < MAX_WARNING_CACHE ) cached_warning[ warning_cache++ ] = text;
}
static void log_error( char *text ) {
	if( error_cache < MAX_ERROR_CACHE ) cached_error[ error_cache++ ] = text;
}

static char *warning_text( int idx ) {
	if(( idx >= 0 )&&( idx < warning_cache )) return( cached_warning[ idx ]);
	return( NULL );
}
static char *error_text( int idx ) {
	if(( idx >= 0 )&&( idx < error_cache )) return( cached_error[ idx ]);
	return( NULL );
}

static void reset_warning_cache( void ) {
	warning_cache = 0;
}
static void reset_error_cache( void ) {
	error_cache = 0;
}

////////////////////////////////////////////////////////////////////////
//
//	The data output system.
//
////////////////////////////////////////////////////////////////////////

//
//	Define some generic API variables which the specific
//	output implementations can initialise and use as appropiate.
//
#define MAX_OUTPUT_BUFFER 16
static byte output_buffer[ MAX_OUTPUT_BUFFER ];
static int buffered_output = 0;
static word buffered_address = 0;
static FILE *output_file = NULL;

//
//	This will be structured around a data structure containing
//	pointers to the routines which implement the individual
//	API routines.
//

typedef struct {
	void	FUNC( init_output )( char *source );
	void	FUNC( next_line )( int line, char *code );
	void	FUNC( set_address )( word adrs );
	void	FUNC( set_start )( word adrs );
	void	FUNC( add_byte )( byte data );
	void	FUNC( end_output )( void );
} output_api;

//
//	No Output
//	---------
//
//	A dummy output system which does not output
//	anything.
//
static void _null_init_output( char *source ) {
}
static void _null_next_line( int line, char *code ) {
}
static void _null_set_address( word adrs ) {
}
static void _null_set_start( word adrs ) {
}
static void _null_add_byte( byte data ) {
}
static void _null_end_output( void ) {
}

static output_api _null_output_api = {
	_null_init_output,
	_null_next_line,
	_null_set_address,
	_null_set_start,
	_null_add_byte,
	_null_end_output
};

//
//	The listing output system.
//	--------------------------
//
//	For source code listing these are reuqired.  The buffer is
//	allocated only if required.
//
//	Ensure MAX_SOURCE_OUTPUT_BUFFER <= MAX_OUTPUT_BUFFER
//
#define MAX_SOURCE_CODE_BUFFER 80
#define MAX_SOURCE_OUTPUT_BUFFER 4
static int _listing_this_line = 0;
static char _listing_source_code[ MAX_SOURCE_CODE_BUFFER+1 ];  // +1 for EOS

static void _listing_init_output( char *source ) {
	buffered_output = 0;
	buffered_address = 0;
	_listing_this_line = 0;
}
static void _listing_flush_output( void ) {
	int	i;
	
	//
	//	The output format will be:
	//
	//	address		%04X
	//	space
	//	data		%02X		Repeat to fixed limit
	//	source		|...		To end of line
	//
	if(( buffered_output == 0 )&&( _listing_this_line == 0 )) return;
	if( buffered_output ) {
		fprintf( output_file, "%04X ", (int)buffered_address );
		for( i = 0; i < buffered_output; fprintf( output_file, "%02X", (int)output_buffer[ i++ ]));
		while( i++ < MAX_SOURCE_OUTPUT_BUFFER ) fprintf( output_file, "  " );
		buffered_address += buffered_output;
		buffered_output = 0;
	}
	else {
		fprintf( output_file, "     " );
		for( i = 0; i < MAX_SOURCE_OUTPUT_BUFFER; i++ ) fprintf( output_file, "  " );
	}
	if( _listing_this_line ) {
		fprintf( output_file, " |%4d|%s\n", _listing_this_line, _listing_source_code );
		_listing_this_line = 0;
	}
	else {
		fprintf( output_file, "\n" );
	}
}
static void _listing_next_line( int line, char *code ) {
	ASSERT( line > 0 );
	
	//
	//	Flush out anything we have pending..
	//
	_listing_flush_output();
	//
	//	Save this line for later..
	//
	strncpy( _listing_source_code, code, MAX_SOURCE_CODE_BUFFER );
	_listing_source_code[ MAX_SOURCE_CODE_BUFFER ] = EOS;
	_listing_this_line = line;
}
static void _listing_set_address( word adrs ) {
	if(( buffered_address + buffered_output ) != adrs ) {
		if( buffered_output ) _listing_flush_output();
		buffered_address = adrs;
	}
}
static void _listing_set_start( word adrs ) {
}

static void _listing_add_byte( byte data ) {
	output_buffer[ buffered_output++ ] = data;
	if( buffered_output >= MAX_SOURCE_OUTPUT_BUFFER ) _listing_flush_output();
}

static void _listing_end_output( void ) {
	_listing_flush_output();
}

static output_api _listing_output_api = {
	_listing_init_output,
	_listing_next_line,
	_listing_set_address,
	_listing_set_start,
	_listing_add_byte,
	_listing_end_output
};

//
//	The hexadecimal output system.
//	------------------------------
//
static void _hexadecimal_init_output( char *source ) {
	buffered_output = 0;
	buffered_address = 0;
}
static void _hexadecimal_next_line( int line, char *code ) {
}
static void _hexadecimal_flush_output( void ) {
	if( buffered_output ) {
		fprintf( output_file, "%04X", (int)buffered_address );
		for( int i = 0; i < buffered_output; fprintf( output_file, " %02X", (int)output_buffer[ i++ ]));
		fprintf( output_file, "\n" );
		buffered_address += buffered_output;
		buffered_output = 0;
	}
}
static void _hexadecimal_set_address( word adrs ) {
	if(( buffered_address + buffered_output ) != adrs ) {
		if( buffered_output ) _hexadecimal_flush_output();
		buffered_address = adrs;
	}
}
static void _hexadecimal_set_start( word adrs ) {
}
static void _hexadecimal_add_byte( byte data ) {
	output_buffer[ buffered_output++ ] = data;
	if( buffered_output >= MAX_OUTPUT_BUFFER ) _hexadecimal_flush_output();
}

static void _hexadecimal_end_output( void ) {
	_hexadecimal_flush_output();
}

static output_api _hexadecimal_output_api = {
	_hexadecimal_init_output,
	_hexadecimal_next_line,
	_hexadecimal_set_address,
	_hexadecimal_set_start,
	_hexadecimal_add_byte,
	_hexadecimal_end_output
};

//
//	The Motorola S record output system.
//	------------------------------------
//

static bool _motorola_have_start = false;
static word _motorola_start_address = 0x0000;
static word _motorola_srec_count = 0;

static void _motorola_init_output( char *source ) {
	int	l;
	byte	s;
	
	buffered_output = 0;
	buffered_address = 0;
	_motorola_have_start = false;
	_motorola_start_address = 0x0000;
	_motorola_srec_count = 0;
	//
	//	Send out header record.
	//
	//	For the moment This is not following the header format:
	//
	//		20 bytes	Module name
	//		2 bytes		Version
	//		2 bytes		Revision
	//		0-36 bytes	Comment
	//
	s = 0;
	l = strlen( source );
	fprintf( output_file, "S0%02X0000", l + 3 );	// 3 accounts for address '0000' + checksum 'nn'
	for( int i = 0; i < l; i++ ) {
		s += source[ i ];
		fprintf( output_file, "%02X", source[ i ]);
	}
	fprintf( output_file, "%02X\r\n", ~s & 0xff );
}
static void _motorola_next_line( int line, char *code ) {
}
static void _motorola_flush_output( void ) {
	if( buffered_output ) {
		byte	s;

		s = H( buffered_address ) + L( buffered_address ) + buffered_output;
		fprintf( output_file, "S1%02X%04X", 3 + buffered_output, buffered_address );
		for( int i = 0; i < buffered_output; i++ ) {
			s += output_buffer[ i ];
			fprintf( output_file, "%02X", output_buffer[ i ]);
		}
		fprintf( output_file, "%02X\r\n", ~s & 0xff );
		buffered_address += buffered_output;
		buffered_output = 0;
		_motorola_srec_count++;
	}
}
static void _motorola_set_address( word adrs ) {
	if(( buffered_address + buffered_output ) != adrs ) {
		if( buffered_output ) _motorola_flush_output();
		buffered_address = adrs;
	}
}
static void _motorola_set_start( word adrs ) {
	if( _motorola_have_start ) {
		log_error( "Duplicate START address specified" );
	}
	else {
		_motorola_have_start = true;
		_motorola_start_address = adrs;	
	}
}
static void _motorola_add_byte( byte data ) {
	output_buffer[ buffered_output++ ] = data;
	if( buffered_output >= MAX_OUTPUT_BUFFER ) _motorola_flush_output();
}

static void _motorola_end_output( void ) {
	byte	s;
	
	_motorola_flush_output();
	//
	//	The end records:
	//
	s = H( _motorola_srec_count ) + L( _motorola_srec_count ) + 3;
	fprintf( output_file, "S503%04X%02X\r\n", _motorola_srec_count, ~s & 0xff );
	s = H( _motorola_start_address ) + L( _motorola_start_address ) + 3;
	fprintf( output_file, "S903%04X%02X\r\n", _motorola_start_address, ~s & 0xff );
}

static output_api _motorola_output_api = {
	_motorola_init_output,
	_motorola_next_line,
	_motorola_set_address,
	_motorola_set_start,
	_motorola_add_byte,
	_motorola_end_output
};

//
//	The Intel Hex record output system.
//
static void _intel_init_output( char *source ) {
	buffered_output = 0;
	buffered_address = 0;
}
static void _intel_next_line( int line, char *code ) {
}
static void _intel_flush_output( void ) {
	if( buffered_output ) {
		byte	s;

		s = buffered_output + H( buffered_address ) + L( buffered_address );
		fprintf( output_file, ":%02X%04X00", buffered_output, buffered_address );
		for( int i = 0; i < buffered_output; i++ ) {
			s += output_buffer[ i ];
			fprintf( output_file, "%02X", output_buffer[ i ]);
		}
		fprintf( output_file, "%02X\n", -s & 0xff );
		buffered_address += buffered_output;
		buffered_output = 0;
	}
}
static void _intel_set_address( word adrs ) {
	if(( buffered_address + buffered_output ) != adrs ) {
		if( buffered_output ) _intel_flush_output();
		buffered_address = adrs;
	}
}
static void _intel_set_start( word adrs ) {
}
static void _intel_add_byte( byte data ) {
	output_buffer[ buffered_output++ ] = data;
	if( buffered_output >= MAX_OUTPUT_BUFFER ) _intel_flush_output();
}

static void _intel_end_output( void ) {
	_motorola_flush_output();
	fprintf( output_file, ":00000001FF\n" );
}

static output_api _intel_output_api = {
	_intel_init_output,
	_intel_next_line,
	_intel_set_address,
	_intel_set_start,
	_intel_add_byte,
	_intel_end_output
};

//
//	This is the entry point into the output API, we defaullt to
//	the null output option to ensure something is available.
//
static output_api *output_routine = &_null_output_api;

static bool init_output( char *source ) {
	char	*suffix;

	suffix = NULL;
	switch( option_flags & DISPLAY_MASK ) {
		case DISPLAY_TEXT: {
			output_routine = &_hexadecimal_output_api;
			suffix = ".text";
			break;
		}
		case DISPLAY_INTEL: {
			output_routine = &_intel_output_api;
			suffix = ".hex";
			break;
		}
		case DISPLAY_MOTOROLA: {
			output_routine = &_motorola_output_api;
			suffix = ".srec";
			break;
		}
		case DISPLAY_LISTING: {
			output_routine = &_listing_output_api;
			suffix = ".text";
			break;
		}
		case DISPLAY_NOTHING: {
			output_routine = &_null_output_api;
			break;
		}
	}
	if(( suffix == NULL )||( option_flags & DISPLAY_STDOUT )) {
		output_file = stdout;
	}
	else {
		char	*fname, *dot;

		fname = STACK_N( char, ( strlen( source ) + strlen( suffix ) + 1 ));
		strcpy( fname, source );
		if(( dot = strrchr( fname, PERIOD )) != NULL ) *dot = EOS;
		strcat( fname, suffix );
		if(( output_file = fopen( fname, "w" )) == NULL ) {
			log_error( "Unable to open output file" );
			return( false );
		}
	}
	FUNC( output_routine->init_output )( source );
	return( true );
}
static void next_line( int line, char *code ) {
	FUNC( output_routine->next_line )( line, code );
}
static void set_address( word adrs ) {
	FUNC( output_routine->set_address )( adrs );
}
static void set_start( word adrs ) {
	FUNC( output_routine->set_start )( adrs );
}
static void add_byte( byte data ) {
	FUNC( output_routine->add_byte )( data );
}
static void end_output( void ) {
	FUNC( output_routine->end_output )();
	if(( output_file )&&(!( option_flags & DISPLAY_STDOUT ))) {
		fclose( output_file );
		output_file = NULL;
	}
}

////////////////////////////////////////////////////////////////////////
//
//	Capture the multi-pass approach to the assembler
//
////////////////////////////////////////////////////////////////////////

//
//	The Assembler implements a "three phase, multi-pass" mechanism
//	for creating the assembly output.  This means that there are
//	three distinct phases to how the assembler works, and that
//	while the first and last are executed only once, the middle
//	phase is repeated until an error is detected or success achieved. 
//

typedef enum {
	INITIALISATION_PHASE,	// Assembler before parsing a file.
	GATHER_PHASE,		// Gather all symbols and any fixed values.
	NORMALISE_PHASE,	// Recalculate "derived" symbol values until
				// all symbol values stabalise.
	GENERATOR_PHASE,	// Generate the output code.
	RELOCATION_PHASE	// Generate relocation tables (uncoded).
} assemble_phase;

//
//	To reduce the overhead of passing round what is essentially
//	a constant value, the assembler pass/phase will be globally
//	stored here:
//
assemble_phase assembler_pass = INITIALISATION_PHASE;

////////////////////////////////////////////////////////////////////////
//
//	Symbol table collected by the following code.
//
////////////////////////////////////////////////////////////////////////

//
//	Simple record to keep track of symbols and their values.
//
typedef struct _sym_entry {
	char			*symbol;
	bool			defined;
	word			value;
	struct _sym_entry	*left,
				*right,
				*link;
} sym_entry;

//
//	Root of the symbol table.
//
static sym_entry	*sym_root = NULL,
			*sym_sort = NULL;

//
//	The symbol normalisation counter is
//	here.  The routine which reads it also resets it.
//
static int symbols_tweaked = 0;

//
//	get count of symbols which had to be modified.
//
static int normalisation_count( void ) {
	int	r;

	r = symbols_tweaked;
	symbols_tweaked = 0;
	return( r );
}

//
//	backend symbol routine; always returns a valid
//	address to a symbol record.
//
static sym_entry *_find_symbol( sym_entry **adrs, char *name ) {
	sym_entry	*ptr;
	int		test;

	while( true ) {
		if(( ptr = *adrs ) == NULL ) {
			ptr = NEW( sym_entry );
			ptr->symbol = strdup( name );
			ptr->defined = false;
			ptr->value = 0;
			ptr->left = NULL;
			ptr->right = NULL;
			ptr->link = NULL;
			*adrs = ptr;
			break;
		}
		if(( test = strcmp( name, ptr->symbol )) == 0 ) {
			break;
		}
		adrs = ( test < 0 )? &( ptr->left ): &( ptr->right );
	}
	return( ptr );
}

//
//	Lookup / create routine for a symbol.
//
static sym_entry *find_symbol( char *name ) {
	return( _find_symbol( &sym_root, name ));
}

//
//	Assign a value to a symbol, return true
//	if the symbol had no value before OR if the
//	new value matches the previous value.
//
static bool set_symbol( char *name, word value ) {
	sym_entry	*sym;

	sym = find_symbol( name );
	PRINT_DEBUG(( "Set %s=%04X\n", name, value ));
	switch( assembler_pass ) {
		case GATHER_PHASE: {
			//
			//	In the gather phase we can set a symbol,
			//	but only once.
			//
			if( sym->defined ) return( false );
			sym->defined = true;
			sym->value = value;
			return( true );
		}
		case NORMALISE_PHASE: {
			//
			//	In the normalisation phase we allow a symbol
			//	to me changed, but count how many were adjusted.
			//
			if( !sym->defined ) return( false );
			if( sym->value != value ) symbols_tweaked++;
			sym->value = value;
			return( true );
		}
		default: {
			//
			//	Fall through for other options.
			//
			break;
		}
	}
	//
	//	Code generator phase: we can set a symbol again,
	//	but only if it has already been set and to the
	//	same value.
	//

	ASSERT( sym->defined );

	return( sym->value == value );
}

//
//	Return the value of a symbol (and produce an error if
//	it is undefined while not gathering).
//
static word symbol_value( char *name ) {
	sym_entry	*sym;

	sym = find_symbol( name );
	if( sym->defined ) return( sym->value );
	if( assembler_pass != GATHER_PHASE ) log_error( "Undefined symbol value" );
	return( 0 );
}

//
//	Symbol Sorting Routine
//
static sym_entry *sort_symbols( sym_entry *here, sym_entry *tail ) {
	if( here == NULL ) return( tail );
	here->link = sort_symbols( here->right, tail );
	return( sort_symbols( here->left, here ));
}

//
//	Dump the symbol table in ASCII order
//
static void show_symbols( sym_entry *list ) {
	sym_entry	*ptr;

	for( ptr = list; ptr != NULL; ptr = ptr->link ) {
		printf( "%16s\t", ptr->symbol );
		if( ptr->defined ) {
			printf( "%04x(%d)\n", ptr->value, ptr->value );
		}
		else {
			printf( "?\n" );
		}
	}
}	


////////////////////////////////////////////////////////////////////////
//
//	An assortment of minor support routines
//
////////////////////////////////////////////////////////////////////////

//
//	Compare two strings a and b, return true if a can be found
//	at the head of b
//
static bool ishead( char *a, char *b ) {
	char	c, d;

	while(( c = *a++ ) != EOS ) if((( d = *b++ ) == EOS )||( c != d )) return( false );
	return( true );
}

//
//	As above, but ignoring the CASE of any letters
//
static bool iscasehead( char *a, char *b ) {
	char	c, d;

	while(( c = *a++ ) != EOS ) if((( d = *b++ ) == EOS )||( toupper( c ) != toupper( d ))) return( false );
	return( true );
}

////////////////////////////////////////////////////////////////////////
//
//	Define the "Global" environment for the assembler while
//	it parses a source file.  The "reset_conditions()" routine
//	is provided to return all these parameters back to base line.
//
////////////////////////////////////////////////////////////////////////

//
//	Define the current "address" of the assembler.
//
static word	this_address = 0x0000;

//
//	Define the current "direct page" of the assembler.
//
static byte	direct_page = 0x00;


//
//	The reset condition routine.
//
static void reset_conditions( assemble_phase pass ) {
	this_address = 0x0000;
	direct_page = 0x00;
	assembler_pass = pass;
}

////////////////////////////////////////////////////////////////////////
//
//	Breakdown argument into its logical structure
//
////////////////////////////////////////////////////////////////////////

//
//	Define the token types we can handle (we also cheat here
//	a bit and create synthetic tokens to help with parse tree
//	creation).
//
typedef enum {
	TOK_SYMBOL, TOK_VALUE,				// Variable Tokens
	TOK_STRING, TOK_CHARACTER,			//
	TOK_LEGACY_STRING,				//	Legacy string and
	TOK_LEGACY_CHARACTER,				//	character constants
	//
	TOK_BYTE_VAL, TOK_WORD_VAL,			// < >
	TOK_IMMEDIATE,					// #
	TOK_A, TOK_B, TOK_D,				// A B D
	TOK_X, TOK_Y,					// X Y
	TOK_U, TOK_S,					// U S
	TOK_DP, TOK_PC,	TOK_CC,				// DP PC CC
	TOK_PREDEC1, TOK_PREDEC2,			// -r --r
	TOK_POSTINC1, TOK_POSTINC2,			// r+ r++
	TOK_ADDRESS,					// the symbol for current address
	//
	TOK_OBRACKET, TOK_CBRACKET,			// [ ]
	TOK_OPAREN, TOK_CPAREN,				// ( )
	TOK_COMMA, TOK_SEMICOLON,			// , ;
	TOK_PLUS, TOK_MINUS,				// + - (binary)
	TOK_POSITIVE, TOK_NEGATIVE,			// + - (unary)
	TOK_PLUSPLUS, TOK_MINUSMINUS,			// ++ --
	TOK_MUL, TOK_DIV,				// * /
	TOK_LSHIFT, TOK_RSHIFT,				// << >>
	TOK_AND, TOK_OR,				// & |
	TOK_XOR, TOK_NOT,				// ^ ~
	//
	TOK_ERROR,					// Unrecognised characters
	TOK_EOS						// End of string
} arg_tokens;

//
//	The fixed tokens
//
typedef struct {
	char		*text;
	int		len;
	arg_tokens	token;
} token_defn;

static token_defn fixed_tokens[] = {
	{ "++",	2,	TOK_PLUSPLUS	},
	{ "--",	2,	TOK_MINUSMINUS	},
	{ "<<",	2,	TOK_LSHIFT	},
	{ ">>",	2,	TOK_RSHIFT	},
	{ "[",	1,	TOK_OBRACKET	},
	{ "]",	1,	TOK_CBRACKET	},
	{ "(",	1,	TOK_OPAREN	},
	{ ")",	1,	TOK_CPAREN	},
	{ ",",	1,	TOK_COMMA	},
	{ ";",	1,	TOK_SEMICOLON	},
	{ "+",	1,	TOK_PLUS	},
	{ "-",	1,	TOK_MINUS	},
	{ "*",	1,	TOK_MUL		},
	{ "/",	1,	TOK_DIV		},
	{ "&",	1,	TOK_AND		},
	{ "|",	1,	TOK_OR		},
	{ "^",	1,	TOK_XOR		},
	{ "~",	1,	TOK_NOT		},
	{ "<",	1,	TOK_BYTE_VAL	},
	{ ">",	1,	TOK_WORD_VAL	},
	{ "#",	1,	TOK_IMMEDIATE	},
	{ NULL }
};

static token_defn fixed_register[] = {
	{ "ACCA",4,	TOK_A		},
	{ "ACCB",4,	TOK_B		},
	{ "ACCD",4,	TOK_D		},
	{ "PCR",3,	TOK_PC		},
	{ "DPR",3,	TOK_DP		},
	{ "CCR",3,	TOK_CC		},
	{ "IX",	2,	TOK_X		},
	{ "IY",	2,	TOK_Y		},
	{ "US",	2,	TOK_U		},
	{ "SP",	2,	TOK_S		},
	{ "PC",	2,	TOK_PC		},
	{ "DP",	2,	TOK_DP		},
	{ "CC",	2,	TOK_CC		},
	{ "A",	1,	TOK_A		},
	{ "B",	1,	TOK_B		},
	{ "D",	1,	TOK_D		},
	{ "X",	1,	TOK_X		},
	{ "Y",	1,	TOK_Y		},
	{ "U",	1,	TOK_U		},
	{ "S",	1,	TOK_S		},
	{ NULL }
};

//
//	Define a small token handling structure
//
typedef struct _a_token {
	arg_tokens	tok;
	char		*start;
	int		len;
	struct _a_token	*a, *b,		//  Token tree pointers (a op b)
			*c;		//  Token list pointer (c for chain).
} a_token;

//
//	Small selection of routines to ease the recognition and
//	scanning of various syntactic elements.
//

//
//	Identification of an identifier; first then subsequent characters:
//
static bool is_ident_1( char c ) {
	return(( isalpha( c )||( c == USCORE ))||(( option_flags & LEGACY_SYNTAX )&&( c == PERIOD )));
}
static bool is_ident_2( char c ) {
	return(( isalnum( c )||( c == USCORE )));
}

//
//	Identification of a number; first then subsequent characters:
//
static bool is_number_1( char c ) {
	return( isdigit( c )||( c == DOLLAR )||( c == PERCENT )||( c == AT ));
}
static bool is_number_2( char c ) {
	return( isalnum( c ));
}

//
//	Pull the next token off a string (white space is ignored)
//
static char *get_token( char *input, a_token *tok ) {
	char	c, q;

	//
	//	Clear off leading white spaces...
	//
	while( isspace( c = *input )) input++;
	//
	//	Some token starts here!
	//
	tok->start = input;
	tok->len = 0;
	//
	//	End of the string?
	//
	if( c == EOS ) {
		tok->tok = TOK_EOS;
		return( input );
	}
	//
	//	Numeric value (of one sort or another).
	//
	if( is_number_1( c )) {
		tok->len++;
		c = *++input;
		while( is_number_2( c )) {
			tok->len++;
			c = *++input;
		}
		tok->tok = TOK_VALUE;
		return( input );
	}
	//
	//	Identifier?
	//
	if( is_ident_1( c )) {
		tok->len++;
		c = *++input;
		while( is_ident_2( c )) {
			tok->len++;
			c = *++input;
		}
		//
		//	But before we assume its an identifier, it could be
		//	a register name...
		//
		for( token_defn *look = fixed_register; look->text != NULL; look++ ) {
			if(( tok->len == look->len ) && iscasehead( look->text, tok->start )) {
				tok->tok = look->token;
				return( input );
			}
		}
		tok->tok = TOK_SYMBOL;
		return( input );
	}
	//
	//	Quoted characters or strings.
	//
	if( option_flags & LEGACY_SYNTAX ) {
		//
		//	Here there needs to be specific code to handle the legacy
		//	handling of these elements.
		//
		//	"Quoted" string constants are delimted with the slash
		//	symbol, thus:	/Example text/
		//
		//	Literal characters as values are preceeded only with a
		//	single quote, no "closing quote" is required. The following
		//	is a quoted dollar symbol:	'$
		//
		//	Due to the legacy nature of this system, the content
		//	of the character or string is limited.  I assume this
		//	is normal for the state of programming at that time.
		//
		if( c == QUOTE ) {
			tok->len++;
			if(( c = *++input ) == EOS ) {
				tok->tok = TOK_LEGACY_CHARACTER;
				return( input );
			}
			tok->len++;
			tok->tok = TOK_LEGACY_CHARACTER;
			return( input + 1 );
		}
		if( c == SLASH ) {
			char	*peek, p;
			int	l;
			bool	warn;
			
			//
			//	The use of the SLASH for quoted strings clashes
			//	with the use of it for division.  I do not know
			//	of a clean way of resolving this given the manner
			//	in which this program is structured.
			//
			l = 1;
			peek = input + 1;
			warn = false;
			while( true ) {
				if(( p = *peek++ ) == EOS ) {
					log_warning( "Unterminated legacy string constant?" );
					break;
				}
				l++;
				if( p == SLASH ) {
					//
					//	End of legacy string!
					//
					if( warn ) log_warning( "Invalid legacy constant value?" );
					tok->len = l;
					tok->tok = TOK_LEGACY_STRING;
					return( peek );
				}
				if( !isalnum( p ) && !isspace( p )) warn = true;
			}
			//
			//	Falling out of the above loop implies the potential
			//	for a malformed legacy string - which might actually
			//	be just a normal division symbol!
			//
		}
	}
	if(( c == QUOTE )||( c == QUOTES )) {
		q = c;
		do {
			tok->len++;
			if(( c = *++input ) == ESCAPE ) {
				tok->len++;
				if(( c = *++input ) != EOS ) {
					tok->len++;
					c = *++input;
				}
			}				
		} while(( c != EOS )&&( c != q ));
		if( c == EOS ) {
			log_error( "Unterminated string" );
		}
		else {
			input++;
			tok->len++;
		}
		tok->tok = ( c == QUOTES )? TOK_STRING: TOK_CHARACTER;
		return( input );
	}
	//
	//	Specific operators and symbols.
	//
	for( token_defn *look = fixed_tokens; look->text != NULL; look++ ) {
		if( ishead( look->text, input )) {
			tok->tok = look->token;
			tok->len = look->len;
			input += look->len;
			return( input );
		}
	}
	//
	//	An unrecognised symbol in the input stream.
	//
	tok->tok = TOK_ERROR;
	tok->len = 1;
	input += 1;
	return( input );
}

////////////////////////////////////////////////////////////////////////
//
//	Expression Evaluation routines.
//
////////////////////////////////////////////////////////////////////////

//
//	Small structure used to link together resync tokens in the string.
//
typedef struct _resync {
	arg_tokens	stok;
	struct _resync	*prev;
} resync;

//
//	Define a bit set type to assist with analysis
//	of an arguement.
//
typedef enum {
	HAS_NONE	= 0000,
	HAS_8_BIT	= 0001,
	HAS_16_BIT	= 0002,
	HAS_U_REG	= 0004,
	HAS_S_REG	= 0010,
	HAS_PC_REG	= 0020,
	HAS_CC_REG	= 0040,
	HAS_DP_REG	= 0100
} arg_content;

//
//	Declare a mechanism for tracking how
//	big a numeric value is expected to be.
//
typedef enum {
	SIZE_UNKNOWN	= 0,
	SIZE_BYTE	= 1,
	SIZE_WORD	= 2
} data_size;

//
//	Arguments are consolidated down to an example
//	of the following structure
//
typedef struct {
	//
	//	High level breakdown
	//
	op_mode		op;
	ea_mode		ea;
	//
	//	Details
	//
	int		value;
	data_size	size;
	arg_tokens	a_reg;
	arg_tokens	i_reg;
	//
	//	This is the bit pattern for the register pair
	//	parameter.  Bit 7-4 represent arg 1 (source)
	//	bits 3-0 represent arg 2 (destination)
	//
	//	16 bit	0000	D
	//		0001	X
	//		0010	Y
	//		0011	U
	//		0100	S
	//		0101	PC
	//		0110	n/a
	//		0111	n/a
	//
	//	8 bit	1000	A
	//		1001	B
	//		1010	CC
	//		1011	DP
	//		1100	n/a
	//		1101	n/a
	//		1110	n/a
	//		1111	n/a
	//
	word		reg_pair;
	//
	//	This is the bit pattern for the register list
	//	parameter.  The following bit patter is used
	//	for both push and pull
	//
	//	bit	7  6  5  4  3  2  1  0
	//
	//	Reg	PC U  Y  X  DP B  A  CC
	//	  or	   S
	//
	word		reg_list;
	//
	//	Here are the flags created while the
	//	argument data is being consolidated,
	//	and how many registers made up that
	//	list;
	//
	arg_content	flags;
	int		reg_count;
} arg_data;

//
//	look for a resync, return token skipped to.
//
static a_token *do_resync( a_token *list, resync *toks, bool log ) {
	resync	*check;

	ASSERT( list != NULL );
	while( list->tok != EOS ) {
		for( check = toks; check != NULL; check = check->prev ) {
			if( check->stok == list->tok ) return( list );
		}
		list = list->c;
		if( log ) log_error( "skipping token" );
		ASSERT( list != NULL );
	}
	return( list );
}

//
//	Empty the argument data record.
//
static void empty_arg_data( arg_data *data ) {
	data->op = OP_INHERENT;
	data->ea = EA_NONE;
	data->value = 0;
	data->size = SIZE_UNKNOWN;
	data->a_reg = TOK_EOS;
	data->i_reg = TOK_EOS;
	data->reg_pair = 0;
	data->reg_list = 0;
	data->flags = HAS_NONE;
	data->reg_count = 0;
}

//
//	This routine fills in the argument data structure.
//	with an analysis
//
static void scan_register_list( a_token *list, arg_data *data ) {
	int	count;

	ASSERT( list != NULL );
	ASSERT( data != NULL );

	count = 0;
	while( list->tok != TOK_EOS ) {
		//
		//	Register found?
		//
		count++;
		switch( list->tok ) {
			case TOK_A: {
				data->flags |= HAS_8_BIT;
				switch( count ) {
					case 1: data->reg_pair |= 0x80; break;
					case 2: data->reg_pair |= 0x08; break;
				}
				data->reg_list |= 0x02;
				break;
			}
			case TOK_B: {
				data->flags |= HAS_8_BIT;
				switch( count ) {
					case 1: data->reg_pair |= 0x90; break;
					case 2: data->reg_pair |= 0x09; break;
				}
				data->reg_list |= 0x04;
				break;
			}
			case TOK_D: {
				data->flags |= HAS_16_BIT;
				// data->reg_pair does not need to be
				// modified as D is represent by 0.
				data->reg_list |= 0x06;
				break;
			}
			case TOK_X: {
				data->flags |= HAS_16_BIT;
				switch( count ) {
					case 1: data->reg_pair |= 0x10; break;
					case 2: data->reg_pair |= 0x01; break;
				}
				data->reg_list |= 0x10;
				break;
			}
			case TOK_Y: {
				data->flags |= HAS_16_BIT;
				switch( count ) {
					case 1: data->reg_pair |= 0x20; break;
					case 2: data->reg_pair |= 0x02; break;
				}
				data->reg_list |= 0x20;
				break;
			}			
			case TOK_U: {
				data->flags |= HAS_16_BIT | HAS_U_REG;
				switch( count ) {
					case 1: data->reg_pair |= 0x30; break;
					case 2: data->reg_pair |= 0x03; break;
				}
				data->reg_list |= 0x40;
				break;
			}
			case TOK_S: {
				data->flags |= HAS_16_BIT | HAS_S_REG;
				switch( count ) {
					case 1: data->reg_pair |= 0x40; break;
					case 2: data->reg_pair |= 0x04; break;
				}
				data->reg_list |= 0x40;
				break;
			}
			case TOK_DP: {
				data->flags |= HAS_8_BIT | HAS_DP_REG;
				switch( count ) {
					case 1: data->reg_pair |= 0xB0; break;
					case 2: data->reg_pair |= 0x0B; break;
				}
				data->reg_list |= 0x08;
				break;
			}
			case TOK_PC: {
				data->flags |= HAS_16_BIT | HAS_PC_REG;
				switch( count ) {
					case 1: data->reg_pair |= 0x50; break;
					case 2: data->reg_pair |= 0x05; break;
				}
				data->reg_list |= 0x80;
				break;
			}
			case TOK_CC: {
				data->flags |= HAS_8_BIT | HAS_CC_REG;
				switch( count ) {
					case 1: data->reg_pair |= 0xA0; break;
					case 2: data->reg_pair |= 0x0A; break;
				}
				data->reg_list |= 0x01;
				break;
			}
			default: {
				//
				//	Something other than a register at this point
				//	means this isn't a proper list
				//
				return;
			}
		}
		//
		//	look for end of list or a comma...
		//
		list = list->c;

		ASSERT( list != NULL );

		if( list->tok == TOK_EOS ) {
			//
			//	return from here if this is a proper register list.
			//
			data->reg_count = count;
			return;
		}
		if( list->tok != TOK_COMMA ) {
			data->flags = HAS_NONE;
			return;
		}
		//
		//	Skip comma and look for another register
		//
		list = list->c;

		ASSERT( list != NULL );
	}
	//
	//	Return from here is register list is truncated
	//
}

//
//	Individual digit values
//
static int digit_value( char v ) {
	if(( v >= '0' )&&( v <= '9' )) return( v - '0' );
	if(( v >= 'A' )&&( v <= 'F' )) return(( v - 'A' )+10 );
	if(( v >= 'a' )&&( v <= 'f' )) return(( v - 'a' )+10 );
	return( ERROR );
}

//
//	Cheaky little octal routine
//
static bool isoctal( char c ) {
	return(( c >= '0' )&&( c <= '7' ));
}

//
//	Convert a series of characters into a value.
//
static int numeric_value( char *text, int len ) {
	int	value, digit, base;

	//
	//	Default base
	//
	base = 10;
	value = 0;
	//
	//	Adjust base?
	//
	switch( *text ) {
		case DOLLAR: {
			//
			//	"$" on its own is a symbolic "this address"
			//	reference.
			//
			if( len == 1 ) return( this_address );
			//
			//	No, just a hex number.
			//
			base = 16;
			text++;
			len--;
			break;
		}
		case PERCENT: {
			base = 2;
			text++;
			len--;
			break;
		}
		case AT:
		case '0': {
			base = 8;
			text++;
			len--;
			break;
		}
		default: {
			//
			//	Anything else we just fall through.
			//
			break;
		}
	}
	//
	//	Scan number
	//
	while( len-- ) {
		if(( digit = digit_value( *text++ )) == ERROR ) {
			log_error( "Invalid numerical digit" );
			break;
		}
		if( digit >= base ) {
			log_error( "Numerical digit not in base range" );
			break;
		}
		value = value * base + digit;
	}
	return( value );
}

//
//	return the value of a character within a string.
//
static int character_value( char **s, int *l ) {
	char	*ss, c, d;
	int	ll;

	ASSERT( s != NULL );
	ASSERT( l != NULL );
	
	ss = *s;
	ll = *l;

	ASSERT( ss != NULL );
	ASSERT( l >= 0 );

	if( ll < 1 ) {
		log_error( "Empty character constant" );
		return( 0 );
	}
	c = *ss++;
	ll--;
	if( c == ESCAPE ) {
		if( ll < 1 ) {
			log_error( "Invalid escape sequence" );
			c = EOS;
		}
		else {
			c = *ss++;
			ll--;
			if( isoctal( c )) {
				int	v;
				
				//
				//	Escaped octal
				//
				v = 0;
				for( int i = (( ll < 3 )? ll: 3); i ; i-- ) {
					if( isoctal( d = *ss )) {
						ss++;
						ll--;
						v = v * 8 + d;
					}
					else {
						log_error( "non-octal digit in escape sequence" );
					}
				}
				c = v;
			}
			else {
				switch( c ) {
					case 'n': {
						c = '\n';
						break;
					}
					case 'r': {
						c = '\r';
						break;
					}
					case 'b': {
						c = '\b';
						break;
					}
					case 'f': {
						c = '\f';
						break;
					}
					case 'a': {
						c = '\a';
						break;
					}
					case 't': {
						c = '\t';
						break;
					}
					default: {
						//
						//	If it is not recognised we are just
						//	escaping the actual character itself.
						break;
					}
				}
			}
		}
	}
	//
	//	Return what we have found.
	//
	*s = ss;
	*l = ll;
	return( c );
}

//
//	Perform a calculation of a value tree and
//	place value in the result area.  Return
//	true on success or false if there was an error.
//
static int evaluate_tree( a_token *here ) {

	if( here == NULL ) return( true );
	
	switch( here->tok ) {
		case TOK_SYMBOL: {
			char	*str;

			ASSERT( here->len > 0 );

			str = memcpy( alloca( here->len+1 ), here->start , here->len );
			str[ here->len ] = EOS;
			return( symbol_value( str ));
		}
		case TOK_VALUE: {
			return( numeric_value( here->start , here->len ));
		}
		case TOK_STRING:
		case TOK_CHARACTER: {
			char	*s;
			int	l;
			int	v;

			s = here->start+1;
			l = here->len-2;
			v = character_value( &s, &l );
			if( l ) log_error( "Invalid character constant" );
			return( v );
		}
		case TOK_LEGACY_STRING:
		case TOK_LEGACY_CHARACTER: {
			char	c;
			//
			//	The character to return is always the one
			//	after the symbol.
			//
			if(( c = here->start[ 1 ]) == EOS ) {
				//
				//	We are going to assume that in this
				//	(odd case) the programmer anticipates
				//	a SPACE is used instead.
				//
				return( SPACE );
			}
			return( c );
		}
		case TOK_PLUS: {
			return( evaluate_tree( here->a ) + evaluate_tree( here->b ));
		}
		case TOK_MINUS: {
			return( evaluate_tree( here->a ) - evaluate_tree( here->b ));
		}
		case TOK_POSITIVE: {
			return( evaluate_tree( here->a ));
		}
		case TOK_NEGATIVE: {
			return( -evaluate_tree( here->a ));
		}
		case TOK_MUL: {
			return( evaluate_tree( here->a ) * evaluate_tree( here->b ));
		}
		case TOK_DIV: {
			int	d;

			d = evaluate_tree( here->b );
			if( d != 0 ) return( evaluate_tree( here->a ) / d );
			log_error( "Division by 0 detected" );
			return( 0 );
		}
		case TOK_LSHIFT: {
			return( evaluate_tree( here->a ) << evaluate_tree( here->b ));
		}
		case TOK_RSHIFT: {
			return( evaluate_tree( here->a ) >> evaluate_tree( here->b ));
		}
		case TOK_AND: {
			return( evaluate_tree( here->a ) & evaluate_tree( here->b ));
		}
		case TOK_OR: {
			return( evaluate_tree( here->a ) | evaluate_tree( here->b ));
		}
		case TOK_XOR: {
			return( evaluate_tree( here->a ) ^ evaluate_tree( here->b ));
		}
		case TOK_NOT: {
			return( ~evaluate_tree( here->a ));
		}
		case TOK_ADDRESS: {
			return( this_address );
		}
		default: {
			//
			//	Fall through for all other cases.
			//
			break;
		}
	}
	log_error( "Unrecognised token in value tree (programmer error)" );
	return( 0 );
}

//
//	Here we are converting token into a numerical expression.
//
//	The precedence and order of the operators is implemented in
//	a limited fashion with 3 levels:
//
//		Highest(Atomic):	(), Unary +, Unary -, Not ~, Address *,
//					values or symbols
//
//		Middle:			*, /, <<, >>, &
//
//		Lowest:			+, -, |, ^
//
static a_token *organise_value( a_token *list, resync *toks, a_token **tree, bool log );

static a_token *organise_atomic( a_token *list, resync *toks, a_token **tree, bool log ) {
	resync	sync;
	
	switch( list->tok ) {
		case TOK_OPAREN: {
			sync.stok = TOK_CPAREN;
			sync.prev = toks;
			list = organise_value( list->c, &sync, tree, log );
			if( list->tok == TOK_CPAREN ) {
				list = list->c;
			}
			else {
				if( log ) log_error( "')' missing from sub-expression" );
				list = do_resync( list, toks, log );
			}
			break;
		}
		case TOK_PLUS: {
			list->tok = TOK_POSITIVE;
			*tree = list;
			list = organise_atomic( list->c, &sync, &( list->a ), log );
			break;
		}
		case TOK_MINUS: {
			list->tok = TOK_NEGATIVE;
			*tree = list;
			list = organise_atomic( list->c, &sync, &( list->a ), log );
			break;
		}
		case TOK_NOT: {
			*tree = list;
			list = organise_atomic( list->c, &sync, &( list->a ), log );
			break;
		}
		case TOK_MUL: {
			//
			//	Motorola this address represented as "*"
			//
			list->tok = TOK_ADDRESS;
			*tree = list;
			list = list->c;
			break;
		}
		case TOK_VALUE:
		case TOK_STRING:
		case TOK_LEGACY_STRING:
		case TOK_CHARACTER:
		case TOK_LEGACY_CHARACTER:
		case TOK_SYMBOL: {
			*tree = list;
			list = list->c;
			break;
		}
		case TOK_EOS: {
			if( log ) log_error( "Unexpected end of line" );
			*tree = NULL;
			return( list );
		}
		default: {
			if( log ) log_error( "Unexpected symbol" );
			list = do_resync( list, toks, log );
			*tree = NULL;
			return( list );
		}
	}
	return( list );
}

static a_token *organise_middle( a_token *list, resync *toks, a_token **tree, bool log ) {
	resync	sync1, sync2, sync3, sync4, sync5;
	
	sync1.stok = TOK_MUL;
	sync1.prev = &sync2;
	sync2.stok = TOK_DIV;
	sync2.prev = &sync3;
	sync3.stok = TOK_LSHIFT;
	sync3.prev = &sync4;
	sync4.stok = TOK_RSHIFT;
	sync4.prev = &sync5;
	sync5.stok = TOK_AND;
	sync5.prev = toks;
	list = organise_atomic( list, &sync1, tree, log );
	while(( list->tok == TOK_AND )||( list->tok == TOK_MUL )||( list->tok == TOK_DIV )||( list->tok == TOK_LSHIFT )||( list->tok == TOK_RSHIFT )) {
		list->a = *tree;
		*tree = list;
		list = organise_atomic( list->c, &sync1, &( list->b ), log );
	}
	return( list );
}

static a_token *organise_value( a_token *list, resync *toks, a_token **tree, bool log ) {
	resync	sync1, sync2, sync3, sync4;
	
	sync1.stok = TOK_MINUS;
	sync1.prev = &sync2;
	sync2.stok = TOK_PLUS;
	sync2.prev = &sync3;
	sync3.stok = TOK_OR;
	sync3.prev = &sync4;
	sync4.stok = TOK_XOR;
	sync4.prev = toks;
	list = organise_middle( list, &sync1, tree, log );
	while(( list->tok == TOK_MINUS )||( list->tok == TOK_PLUS )||( list->tok == TOK_OR )||( list->tok == TOK_XOR )) {
		list->a = *tree;
		*tree = list;
		list = organise_middle( list->c, &sync1, &( list->b ), log );
	}
	return( list );
}

static a_token *organise_reg_or_value( a_token *list, resync *toks, a_token **tree, bool ind, arg_data *data ) {

	switch( list->tok ) {
		case TOK_A:
		case TOK_B:
		case TOK_D: {
			data->a_reg = list->tok;
			*tree = list;
			list = list->c;
			data->ea = ind? EA_IND_ACC_INDEX: EA_ACC_INDEX;
			data->op = OP_EADRS;
			break;
		}
		default: {
			list = organise_value( list, toks, tree, ( data->reg_count == 0 ));
			data->value = evaluate_tree( *tree );
			if( ind ) {
				data->ea = EA_IND_EXTENDED;
				data->op = OP_EADRS;
			}
			else {
				data->op = OP_EXTENDED;
			}
			break;
		}
	}
	return( list );
}

static a_token *organise_index_reg( a_token *list, resync *toks, a_token **tree, bool ind, arg_data *data ) {
	bool	log;

	log = ( data->reg_count == 0 );
	
	switch( list->tok ) {
		case TOK_MINUS:
		case TOK_MINUSMINUS: {
			if(( data->ea != EA_NONE )&& log ) log_error( "Invalid register offset effective address (-/--)" );
			switch( list->c->tok ) {
				case TOK_X:
				case TOK_Y:
				case TOK_U:
				case TOK_S: {
					if( ind ) {
						if( list->tok == TOK_MINUS ) {
							if( log ) log_error( "Invalid indirected single pre-decrement" );
						}
						else {
							data->ea = EA_IND_PRE_DEC_TWO;
						}
					}
					else {
						data->ea = (list->tok == TOK_MINUS )? EA_PRE_DEC_ONE: EA_PRE_DEC_TWO;
					}
					data->i_reg = list->c->tok;
					*tree = list;
					list = list->c->c;
					break;
				}
				default: {
					if( log ) log_error( "Invalid register following '-' or '--'" );
					list = do_resync( list, toks, log );
					break;
				}
			}
			break;
		}
		case TOK_X:
		case TOK_Y:
		case TOK_U:
		case TOK_S: {
			switch( list->c->tok ) {
				case TOK_PLUS:
				case TOK_PLUSPLUS: {
					if(( data->ea != EA_NONE )&& log ) log_error( "Invalid register offset effective address (+/++)" );
					if( ind ) {
						if( list->c->tok == TOK_PLUS ) {
							if( log ) log_error( "Invalid indirected single post-increment" );
						}
						else {
							data->ea = EA_IND_POST_INC_TWO;
						}
					}
					else {
						data->ea = ( list->c->tok == TOK_PLUS )? EA_POST_INC_ONE: EA_POST_INC_TWO;
					}
					data->i_reg = list->tok;
					*tree = list;
					list = list->c->c;
					break;
				}
				default: {
					if( data->ea == EA_ACC_INDEX ) {
						if( ind ) data->ea = EA_IND_ACC_INDEX;
					}
					else {
						data->ea = ind? EA_IND_OFFSET_INDEX: EA_OFFSET_INDEX;
					}
					data->i_reg = list->tok;
					*tree = list;
					list = list->c;
					break;
				}
			}
			break;
		}
		case TOK_PC: {
			data->ea = ind? EA_IND_OFFSET_PC: EA_OFFSET_PC;
			data->i_reg = list->tok;
			*tree = list;
			list = list->c;
			break;
		}
		default: {
			if( log ) log_error( "Index register expected" );
			list = do_resync( list, toks, log  );
			break;
		}
	}
	return( list );
}

//
//	Handle those fiddle BYTE/DIRECT or WORD/EXTENDED operators.
//
static a_token *organise_size( a_token *list, arg_data *data ) {
	
	ASSERT( list != NULL );
	ASSERT( data != NULL );
	
	switch( list->tok ) {
		case TOK_BYTE_VAL: {
			data->size = SIZE_BYTE;
			return( list->c );
		}
		case TOK_WORD_VAL: {
			data->size = SIZE_WORD;
			return( list->c );
		}
		default:break;
	}
	return( list );
}

//
//	Here we convert the linked list of tokens into a tree structure
//
static a_token *organise_arg( a_token *list, resync *toks, a_token **tree, arg_data *data ) {
	resync		sync1,
			sync2;
	bool		log;

	ASSERT( list != NULL );
	ASSERT( toks != NULL );
	ASSERT( tree != NULL );
	ASSERT( data != NULL );

	//
	//	Before converting into a tree, check to see
	//	what list capacity this has.
	//
	scan_register_list( list, data );
	log = ( data->reg_count == 0 );
	//
	//	Now analyse content based on tokens presented.
	//
	switch( list->tok ) {
		case TOK_OBRACKET: {
			//
			//	All Indirect Effective Address arguments
			//	are handled in this case.
			//
			list = list->c;			// skip bracket.
			//
			//	Size operator?
			//
			list = organise_size( list, data );
			//
			//	At this point only interested in
			//	the pending close bracket.
			//
			sync1.stok = TOK_CBRACKET;
			sync1.prev = toks;
			//
			//	
			if( list->tok == TOK_COMMA ) {
				//
				//		     Here
				//			v
				//		[	,IReg]		
				//		[	,IReg++]
				//		[	,--IReg]
				//
				list = organise_index_reg( list->c, &sync1, tree, true, data );
			}
			else {
				//
				//	Now interested in a comma and the
				//	outstanding close bracket.
				//
				sync2.stok = TOK_COMMA;
				sync2.prev = &sync1;
				//
				//		     Here
				//			v
				//		[	Reg,IReg]
				//		[	Num,IReg]
				//		[	Num,PC]
				//
				//		[	Num]
				//
				list = organise_reg_or_value( list, &sync2, tree, true, data );
				if( list->tok == TOK_COMMA ) {
					//
					//		     Here
					//			v
					//		[Reg	,IReg]
					//		[Num	,IReg]
					//		[Num	,PC]
					//
					//	Push the comma in above the previously
					//	collected element.
					//
					list->a = *tree;
					*tree = list;
					list = organise_index_reg( list->c, &sync1, &( list->b ), true, data );
				}
				else {
					//
					//		[Num	]
					//
					data->ea = EA_IND_EXTENDED;
				}
					
			}
			//
			//	Now only need to close down the indirect
			//	syntax.
			//
			if( list->tok == TOK_CBRACKET ) {
				list = list->c;		// skip bracket.
			}
			else {
				if( log ) log_error( "']' missing from indirection" );
				list = do_resync( list, toks, log );
			}
			data->op = OP_EADRS;
			break;
		}
		case TOK_IMMEDIATE: {
			//
			//	Skip immediate.
			//
			list = list->c;
			//
			//	Size operator?
			//
			list = organise_size( list, data );
			//
			//
			list = organise_value( list, toks, tree, log );
			data->value = evaluate_tree( *tree );
			data->op = OP_IMM_UNDEF;
			break;
		}
		case TOK_COMMA: {
			//
			//		,IReg	
			//		,IReg++
			//		,--IReg
			//
			//	Push the comma in above what ever follows.
			//
			list = organise_index_reg( list->c, &sync2, tree, false, data );
			data->op = OP_EADRS;
			break;
		}
		default: {
			//
			//	Size operator?
			//
			list = organise_size( list, data );
			//
			//	All arguments which start with a value or a register; something
			//	other than an explicitly predictable token.
			//
			sync1.stok = TOK_COMMA;
			sync1.prev = toks;
			list = organise_reg_or_value( list, &sync1, tree, false, data );
			if( list->tok == TOK_COMMA ) {
				//
				//		Reg,IReg
				//		Num,IReg
				//		Num,PC
				//
				list->a = *tree;
				*tree = list;
				list = organise_index_reg( list->c, toks, &( list->b ), false, data );
				data->op = OP_EADRS;
			}
			else {
				//
				//		Num
				//
				data->op = OP_EXTENDED;
			}
			break;
		}
	}
	return( list );
}

//
//	Some more direct debugging assist.
//
#ifdef ENABLE_DEBUG
static void display_indent( FILE *to, int width ) {
	while( width-- > 0 )  fprintf( to, "  " );
}
static char *display_token_id( arg_tokens id ) {
	token_defn	*look;
	char		*found;

	switch( id ) {
		case TOK_SYMBOL:	return( "Symbol" );
		case TOK_VALUE:		return( "Value" );
		case TOK_STRING:	return( "String" );
		case TOK_CHARACTER:	return( "Character" );
		case TOK_LEGACY_STRING:	return( "LegacyString" );
		case TOK_LEGACY_CHARACTER: return( "LegacyChar" );
		case TOK_ERROR:		return( "Error" );
		case TOK_EOS:		return( "End" );
		default: break;
	}
	found = NULL;
	for( look = fixed_tokens; look->text; look++ ) {
		if( id == look->token ) found = look->text;
	}
	if( found ) return( found );
	for( look = fixed_register; look->text; look++ ) {
		if( id == look->token ) found = look->text;
	}
	return( found );
}
static void _display_token_tree( FILE *to, int depth, a_token *tree ) {
	if( tree ) {
		char	*text;

		display_indent( to, depth );
		if(( text = display_token_id( tree->tok ))) {
			fprintf( to, "%s", text );
		}
		else {
			fprintf( to, "tok %d?", tree->tok );
		}
		if(( text = tree->start )) {
			int	l;

			fprintf( to, "'" );
			l = tree->len;
			while( l-- ) fprintf( to, "%c", *text++ );
			fprintf( to, "'" );
		}
		fprintf( to, "\n" );
		_display_token_tree( to, depth+1, tree->a );
		_display_token_tree( to, depth+1, tree->b );
	}
}
static void display_token_tree( FILE *to, char *label, a_token *tree ) {
	fprintf( stdout, "%s:\n", label );
	_display_token_tree( stdout, 1, tree );
}
static void display_token_list( FILE *to, char *label, a_token *tree ) {
	fprintf( stdout, "%s:\n", label );
	while( tree ) {
		char	*text;

		display_indent( to, 1 );
		if(( text = display_token_id( tree->tok ))) {
			fprintf( to, "%s", text );
		}
		else {
			fprintf( to, "tok %d?", tree->tok );
		}
		if((( text = tree->start ))&&( tree->len > 0 )) {
			int	l;

			fprintf( to, "'" );
			l = tree->len;
			while( l-- ) fprintf( to, "%c", *text++ );
			fprintf( to, "'" );
		}
		fprintf( to, "\n" );
		tree = tree->c;
	}
}
#endif

//
//	given a string, return the object described (true on success,
//	false on error)
//
static bool analyse_arg( char *input, arg_data *result ) {
	a_token		*head, **tail, *ptr;
	resync		sync;
	
	//
	//	Start by converting the input into a linked
	//	series of token records.
	//
	tail = &head;
	do {
		ptr = STACK( a_token );
		input = get_token( input, ptr );
		ptr->a = NULL;
		ptr->b = NULL;
		*tail = ptr;
		tail = &( ptr->c );
	} while( ptr->tok != TOK_EOS );
	*tail = NULL;
	
#ifdef ENABLE_DEBUG
	//
	//	Display the argument before analysis.
	//
	if( option_flags & DISPLAY_VEBOSE ) display_token_list( stdout, "tokenised", head );
#endif

	//
	//	Tokens all chained together on "b" pointer.
	//	Convert to a parse tree (of sorts).
	//
	sync.stok = TOK_EOS;
	sync.prev = NULL;
	ptr = NULL;
	if(( head = organise_arg( head, &sync, &ptr, result )) == NULL ) {
		log_error( "Unrecognised argument structure" );
		return( false );
	}

	
#ifdef ENABLE_DEBUG
	//
	//	Display the argument after analysis.
	//
	if( option_flags & DISPLAY_VEBOSE ) display_token_tree( stdout, "parsed", ptr );
#endif

	//
	//	If the head record is not TOK_EOS and the register pair
	//	or list list code did not find anything then something
	//	has gone wrong
	//
	if(( head->tok != TOK_EOS )&&( result->reg_count == 0 )) {
		log_error( "Malformed argument" );
		return( false );
	}
	//
	//	Done
	//
	return( true );
}

//
//	Given a string, return one (or more) values which
//	the evaluation of the string results in.
//
//	When split is true then we will split strings into
//	a series of byte values, otherwise only the first
//	value will be returned (with warning generated).
//
static int analyse_value( char *input, bool split, int max, word *result ) {
	a_token	*head, **tail, *ptr, *look;
	resync	sync;
	int	count;
	
	//
	//	Start by converting the input into a linked
	//	series of token records (all deconstruction
	//	data is kept on the stack to simplify memory
	//	clean up on return from routine).
	//
	tail = &head;
	do {
		ptr = STACK( a_token );
		input = get_token( input, ptr );
		ptr->a = NULL;
		ptr->b = NULL;
		*tail = ptr;
		tail = &( ptr->c );
	} while( ptr->tok != TOK_EOS );
	*tail = NULL;
	
#ifdef ENABLE_DEBUG
	//
	//	Display the argument before analysis.
	//
	if( option_flags & DISPLAY_VEBOSE ) display_token_list( stdout, "tokenised", head );
#endif

	//
	//	Tokens all chained together on "c".  Now we look for
	//	comma symbols, and break the list into smaller pieces
	//	which are then handled individually.
	//
	count = 0;
	while( head->tok != TOK_EOS ) {
		//
		//	find comma or eos
		//
		for( look = head; look->tok != TOK_EOS; look = look->c ) {
			if( look->tok == TOK_COMMA ) break;
		}
		//
		//	on a comma convert to eos and skip it
		//
		if( look->tok == TOK_COMMA ) {
			look->tok = TOK_EOS;
			look = look->c;
		}
		//
		//	Are we "de-string-ing" a value?
		//
		if( split && (( head->tok == TOK_STRING )||( head->tok == TOK_LEGACY_STRING ))) {
			char	*s;
			int	l;

			s = head->start+1;
			l = head->len-2;
			while( l ) {
				int v;
				
				v = character_value( &s, &l );
				if( count < max ) {
					result[ count++ ] = (word)v;
				}
				else {
					log_error( "constant values buffer overflowed" );
				}
			}
		}
		else {
			//
			//	process tokens at head as an expression.
			//
			sync.stok = TOK_EOS;
			sync.prev = NULL;
			ptr = NULL;
			if(( head = organise_value( head, &sync, &ptr, true )) == NULL ) {
				log_error( "Unrecognised value structure" );
			}
			else {
				if( head->tok != TOK_EOS ) {
					log_error( "Malformed value" );
				}
				else {
					if( count < max ) {
						//
						//	Evaluate tree at ptr and add to results
						//
						result[ count++ ] = evaluate_tree( ptr );
					}
					else {
						log_error( "Too many values provided" );
					}
				}
			}
		}
		//
		//	move head to look for next value argument.
		//
		head = look;
	}
	//
	//	Return the list of words.
	//
	return( count );
}

////////////////////////////////////////////////////////////////////////
//
//	Machine Instructions captured in this section.
//
////////////////////////////////////////////////////////////////////////

static word last_pc = 0;
static bool possible_wrap = false;

static word advance_pc( word pc, word step ) {
	if(( pc < last_pc )&& possible_wrap ) log_warning( "Address advance rolls through $0000" );
	last_pc = pc;
	possible_wrap = true;
	return( pc + step );
}

static word set_pc( word pc ) {
	last_pc = pc;
	possible_wrap = false;
	return( pc );
}

static byte ea_index_register( arg_tokens reg ) {
	switch( reg ) {
		case TOK_X: return( 0x00 );
		case TOK_Y: return( 0x20 );
		case TOK_U: return( 0x40 );
		case TOK_S: return( 0x60 );
		default: ABORT( "Token does not represent index register" );
	}
	log_error( "Invalid index reg (assembler programming error)" );
	return( 0 );
}

static bool process_machine_inst( int line, char *opcode, char *arg ) {
	arg_data	data;

	//
	//	reset argument data
	//
	empty_arg_data( &data );
	//
	//	If there is an argument provided, we will process it ahead of
	//	the opcode lookup as we really only want to do this once.
	//
	if( arg != NULL ) {
		if( !analyse_arg( arg, &data )) {
			log_error( "Invalid argument" );
			return( false );
		}
	}
	//
	//	Outer loop: seach for opcode records which match the head of
	//	the supplied opcode
	//
	for( op_code *o = instruction_table; o->name; o++ ) {
		if( iscasehead( o->name, opcode )) {
			char	*oo;
			op_ext	*e;
			op_arg	*a;

			word	i, j, len;
			byte	inst[ MAX_INSTRUCTION ];

			ASSERT( o->exts != NULL );
			ASSERT( o->args != NULL );

			//
			//	Found a match on the start of the opcode.
			//
			PRINT_VERBOSE(( "Opcode='%s'.\n", o->name ));

			//
			//	If there are extensions, then we need to find
			//	the one that matches.
			//
			oo = opcode + o->len;
			for( e = o->exts; e->extn; e++ ) {
				if( strcasecmp( oo, e->extn ) == 0 ) break;
			}
			if( e->extn == NULL ) {
				PRINT_VERBOSE(( "No match\n" ));
				continue;
			}

			//
			//	Confirm supported instruction
			//
			if(!( iso_options & o->iso )) {
				log_error( "Unsupported on target CPU" );
				continue;
			}

			//
			//	Found a match on the opcode extension
			//
			PRINT_VERBOSE(( "Match extend='%s'.\n", e->extn ));
			
			//
			//	If there should be an argument then we process it, and try to
			//	match against the list provided.
			//
			for( a = o->args; a->mode != OP_NONE; a++ ) {
				//
				//	Here we need to explicitly handle the irregularity
				//	of the op code options as the argument processing code
				//	cannot make a reliable decision what to call some arguements.
				//
				//	If the opcode specifies a page direct mode and the
				//	argument provided is extended *and* the top byte of
				//	the extended address is the same as the declared DP
				//	then we have a match.
				//
				if(( a->mode == OP_DIRECT )&&( data.op == OP_EXTENDED )) {
					if(( H( data.value ) == direct_page )||( data.size == SIZE_BYTE )) {
						PRINT_VERBOSE(( "Arg: Direct\n" ));
						break;
					}
				}
				//
				//	Immediate values are always 'undefined' and so need
				//	either an immediate byte or word opcode.
				//
				if((( a->mode == OP_IMM_BYTE )||( a->mode == OP_IMM_WORD ))&&( data.op == OP_IMM_UNDEF )) {
					PRINT_VERBOSE(( "Arg: Immediate\n" ));
					break;
				}
				//
				//	Register lists match either a pair of registers or
				//	a bit map set of registers - depeneding on the number
				//	of registers provided.
				//
				if(( a->mode == OP_REG_PAIR )&&( data.reg_count == 2 )) {
					PRINT_VERBOSE(( "Arg: Reg Pair\n" ));
					break;
				}
				if(( a->mode == OP_REG_LIST )&&( data.reg_count > 0 )) {
					PRINT_VERBOSE(( "Arg: Reg List\n" ));
					break;
				}
				//
				//	An extended argument is also a euphamism for any jump
				//	or call target address.
				//
				if((( a->mode == OP_SRELATIVE )||( a->mode == OP_LRELATIVE ))&&( data.op == OP_EXTENDED )) {
					PRINT_VERBOSE(( "Arg: Relative jump\n" ));
					break;
				}
				//
				//	Obviously if the format of the argument actually matches
				//	the specification of the op-code then we have a winner!
				//
				if( data.op == a->mode ) {
					PRINT_VERBOSE(( "Arg: exact match!\n" ));
					break;
				}
			}
			if( a->mode == OP_NONE ) {
				PRINT_VERBOSE(( "No argument match\n" ));
				continue;
			}
			//
			//	At this point we *should* have everything we need to generate
			//	the complete machine code;
			//
			//	o	->	The base opcode
			//	e	->	Extension modifications
			//	a	->	Argument modifications
			//
			PRINT_DEBUG(( "Decode:%d|%s|%s|\n", line, o->name, e->extn ));
			//
			//	Build the base machine instruction value.
			//
			i = o->base + e->adds + a->adds;
			//
			//	Start building the output array
			//
			if( H( i )) {
				len = 2;
				inst[ 0 ] = H( i );
				inst[ 1 ] = L( i );
			}
			else {
				len = 1;
				inst[ 0 ] = L( i );
			}
			//
			//	Apply argument values as appropiate
			//
			switch( a->mode ) {
				case OP_INHERENT: {			// No argument required
					//
					//	Nothing else to do!
					//
					break;
				}
				case OP_IMM_BYTE: {			// #byte
					if(( data.value < -128 )||( data.value > 255 )) if( assembler_pass == GENERATOR_PHASE ) log_error( "Immediate value out of byte range" );
					inst[ len++ ] = L( data.value ); 
					break;
				}
				case OP_IMM_WORD: {			// #word
					if(( data.value < -32768 )||( data.value > 65535 )) if( assembler_pass == GENERATOR_PHASE ) log_error( "Immediate value out of word range" );
					inst[ len++ ] = H( data.value );
					inst[ len++ ] = L( data.value );
					break;
				}
				case OP_DIRECT: {			// value (8-bit with DP)
					//
					//	Here we need to be aware that we migght be called with
					//	either and 8 bit value or a 16 bit value.  If an 8 bit
					//	value is supplied (value 0..255) then it is a valid
					//	direct page index.  If it is outside this range then
					//	the top byte of the value must match the assembler
					//	copy of the DP register.
					//
					if((( data.value < 0 )||( data.value > 255 ))&&( H( data.value ) != direct_page )) if( assembler_pass == GENERATOR_PHASE ) log_error( "Direct page index out of range" );
					inst[ len++ ] = L( data.value );
					break;
				}
				case OP_EXTENDED: {			// value (16-bit)
					//
					//	There ought to be a good sanity check here, but I do not
					//	want to accidentally catch "wrap around" calculations.
					//
					inst[ len++ ] = H( data.value );
					inst[ len++ ] = L( data.value );
					break;
				}
				case OP_EADRS: {			// Effective Address
					//
					//	Need to break with down based on the effective address
					//
					//		Mode Type			| Variation	| Direct	| Indirect
					//		--------------------------------+---------------+---------------+-------------
					//	1	Constant Offset from Register	| No Offset	| 1RR00100	| 1RR10100
					//	2	(twos Complement Offset)	| 5-Bit Offset	| 0RRnnnnn	| Default to 8-bit
					//	3					| 8-Bit Offset	| 1RR01000	| 1RR11000
					//	4					| 16-Bit Offset	| 1RR01001	| 1RR11001
					//		--------------------------------+---------------+---------------+-------------
					//	5	Accumulator Offset from Reg	| A Acc Offset	| 1RR00110	| 1RR10110
					//	6	(twos Complement Offset)	| B Acc Offset	| 1RR00101	| 1RR10101
					//	7					| D Acc Offset	| 1RR01011	| 1RR11011
					//		--------------------------------+---------------+---------------+-------------
					//	8	Auto Increment/Decrement from	| Inc by 1	| 1RR00000 	| Not Allowed
					//	9	Register			| Inc by 2	| 1RR00001	| 1RR10001
					//	10					| Dec by 1	| 1RR00010	| Not Allowed
					//	11					| Dec by 2	| 1RR00011	| 1RR10011
					//		--------------------------------+---------------+---------------+-------------
					//	12	Constant Offset from Program	| 8-Bit Offset	| 1XX01100	| 1XX11100
					//	13	Counter				| 16-Bit Offset	| 1Xx01101	| 1XX11101
					//		--------------------------------+---------------+---------------+-------------
					//	14	Extended Indirect		| 16-Bit Address| n/a		| 10011111
					//
					switch( data.ea ) {
						case EA_OFFSET_INDEX:			// value,ireg
						case EA_IND_OFFSET_INDEX: {		// [value,ireg]
							if( data.value == 0 ) {
								//
								//	Zero offset is explicitly coded for (Row 1)
								//
								inst[ len++ ] = (( data.ea == EA_OFFSET_INDEX )? 0x84: 0x94 )|ea_index_register( data.i_reg );
								break;
							}
							if(( data.value >= -16 )&&( data.value <= 15 )&&( data.ea == EA_OFFSET_INDEX )) {
								//
								//	5 bit signed constant (Row 2)
								//
								inst[ len++ ] = ea_index_register( data.i_reg )|( data.value & 0x1f );
								break;
							}
							if(( data.value >= -128 )&&( data.value <= 127 )) {
								//
								//	8 bit signed offset (Row 3)
								//
								inst[ len++ ] = (( data.ea == EA_OFFSET_INDEX )? 0x88: 0x98 )|ea_index_register( data.i_reg );
								inst[ len++ ] = L( data.value );
								break;
							}
							//
							//	16-bit signed offset (Row 4)
							//
							inst[ len++ ] = (( data.ea == EA_OFFSET_INDEX )? 0x89: 0x99 )|ea_index_register( data.i_reg );
							inst[ len++ ] = H( data.value );
							inst[ len++ ] = L( data.value );
							break;
						}
						case EA_ACC_INDEX:			// areg,ireg
						case EA_IND_ACC_INDEX: {		// [areg,ireg]
							switch( data.a_reg ) {
								case TOK_A: {
									//
									//	Acc A as offset (Row 5)
									//
									inst[ len++ ] = (( data.ea == EA_ACC_INDEX )? 0x86: 0x96 )|ea_index_register( data.i_reg );
									break;
								}
								case TOK_B: {
									//
									//	Acc B as offset (Row 6)
									//
									inst[ len++ ] = (( data.ea == EA_ACC_INDEX )? 0x85: 0x95 )|ea_index_register( data.i_reg );
									break;
								}
								case TOK_D: {
									//
									//	Acc D as offset (Row 7)
									//
									inst[ len++ ] = (( data.ea == EA_ACC_INDEX )? 0x8B: 0x9B )|ea_index_register( data.i_reg );
									break;
								}
								default: {
									ABORT( "Invalid Accumulator in EA (assembler programming error)" );
									break;
								}
							}
							break;
						}
						case EA_POST_INC_ONE: {			// ,ireg+
							inst[ len++ ] = 0x80 | ea_index_register( data.i_reg );
							break;
						}
						case EA_POST_INC_TWO: {			// ,ireg++
							inst[ len++ ] = 0x81 | ea_index_register( data.i_reg );
							break;
						}
						case EA_PRE_DEC_ONE: {			// ,-ireg
							inst[ len++ ] = 0x82 | ea_index_register( data.i_reg );
							break;
						}
						case EA_PRE_DEC_TWO: {			// ,--ireg
							inst[ len++ ] = 0x83 | ea_index_register( data.i_reg );
							break;
						}
						case EA_IND_POST_INC_TWO: {		// [,ireg++]
							inst[ len++ ] = 0x91 | ea_index_register( data.i_reg );
							break;
						}
						case EA_IND_PRE_DEC_TWO: {		// [,--ireg]
							inst[ len++ ] = 0x93 | ea_index_register( data.i_reg );
							break;
						}
						case EA_OFFSET_PC:			// value,PC
						case EA_IND_OFFSET_PC: {		// [value,PC]
							int	off;

							//
							//	Here we need to do some thunking.  First calculate the
							//	offset *assuming* it is going to be a valid 8 bit
							//	result (TWO added to PC to cover the EA space requirement)
							//
							off = data.value - ( this_address + len + 2 );
							if(( off >= -128 )&&( off <= 127 )) {
								inst[ len++ ] = ( data.ea == EA_OFFSET_PC )? 0x8C: 0x9C;
								inst[ len++ ] = L( off );
								break;
							}
							//
							//	No?  Now subtract an extra 1 from the offset as we
							//	have to use a 16 bit value (so EA now needs THREE bytes).
							//
							off--;
							inst[ len++ ] = ( data.ea == EA_OFFSET_PC )? 0x8D: 0x9D;
							inst[ len++ ] = H( off );
							inst[ len++ ] = L( off );
							break;
						}
						case EA_IND_EXTENDED: {			// [value]
							inst[ len++ ] = 0x9f;
							inst[ len++ ] = H( data.value );
							inst[ len++ ] = L( data.value );
							break;
						}
						default: {
							ABORT( "Invalid EA (assembler programming error)" );
							break;
						}
					}
					break;
				}
				case OP_SRELATIVE: {			// -128..+127 relative
					// Remember where to put the
					// relative offset but also
					// get final length of inst.
					i = len++;
					// Calculate the relative
					// jump.
					j = data.value - ( this_address + len );
					//
					//	Range check
					//
					if(( H( j ) != 0 )&&( H( j ) !=  0xff )) {
						if( assembler_pass == GENERATOR_PHASE ) log_error( "Short relative jump out of range" );
					}
					inst[ i ] = L( j );
					break;
				}
				case OP_LRELATIVE: {			// -32768..32767 relative
					// As above but 16 bit.
					i = len;
					len += 2;
					j = data.value - ( this_address + len );
					inst[ i ] = H( j );
					inst[ i+1 ] = L( j );
					break;
				}
				case OP_REG_PAIR: {			// Exchange or Transfer
					inst[ len++ ] = data.reg_pair;
					break;
				}
				case OP_REG_LIST: {			// Push or Pull
					//
					//	really ought to check to see if the source code
					//	is attempting to push/pull S to/from the S stack
					//	and likewise with U and the U stack.  The data flags
					//	contain the means of working this out.
					//
					inst[ len++ ] = data.reg_list;
					break;
				}
				default: {
					ABORT( "Unrecognised OP mode (Assembler programmer error)" );
					break;
				}
			}
			
			ASSERT( len <= MAX_INSTRUCTION );
			
			//
			//	Output instruction (if appropiate)
			//
			if( assembler_pass == GENERATOR_PHASE ) {
				set_address( this_address );
				for( i = 0; i < len; i++ ) add_byte( inst[ i ]);
			}
			//
			//	Move this address forward
			//
			this_address = advance_pc( this_address, len );
			//
			//	Done.
			//
			return( true );
		}
	}
	log_error( "Machine instruction not recognised" );
	return( false );
}

////////////////////////////////////////////////////////////////////////
//
//	Assembler Directives captured in this section.
//
////////////////////////////////////////////////////////////////////////

static bool asm_equ( int line, char *label, char *opcode, char *arg ) {
	word	val;
	
	if( analyse_value( arg, false, 1, &val ) == 1 ) {
		//
		//	Have value, set label
		//
		if( set_symbol( label, val )) return( true );
		if( assembler_pass == GATHER_PHASE ) {
			log_error( "Duplicate label asignment in EQU" );
		}
		else {
			log_error( "Inconsistent labelling in EQU" );
		}
		return( false );
	}
	log_error( "Error calculating label value in EQU" );
	return( false );
}

static bool asm_setdp( int line, char *label, char *opcode, char *arg ) {
	word	val;
	bool	ret;

	ret = true;
	if( label ) {
		if( !set_symbol( label, this_address )) {
			if( assembler_pass == GATHER_PHASE ) {
				log_error( "Duplicate label for SETDP" );
			}
			else {
				log_error( "Inconsistent labelling for SETDP" );
			}
			ret = false;
		}
	}
	if( analyse_value( arg, false, 1, &val ) == 1 ) {
		//
		//	Have value, set virtual DP.
		//
		//	Eight bit values are assumed to be
		//	the exact value that the direct
		//	pointer is to be set to.
		//
		if(( val >= 0 )&&( val < 256 )) {
			direct_page = val;
			return( ret );
		}
		//
		//	Sixteen bit values are OK too, so
		//	long as the bottom eight bits are zero.
		//	In this case the top eight bits are
		//	placed into DP.
		//
		if( L( val ) == 0 ) {
			direct_page = H( val );
			return( ret );
		}
		log_error( "Invalid page number in SETDP" );
		return( false );
	}
	log_error( "Error calculating Direct Page number" );
	return( false );
}

static bool asm_org( int line, char *label, char *opcode, char *arg ) {
	word	val;

	if( analyse_value( arg, false, 1, &val ) == 1 ) {
		//
		//	Have value, set this address
		//
		this_address = set_pc( val );
		if( label ) {
			if( !set_symbol( label, this_address )) {
				if( assembler_pass == GATHER_PHASE ) {
					log_error( "Duplicate label for ORG" );
				}
				else {
					log_error( "Inconsistent labelling for ORG" );
				}
				return( false );
			}
		}
		return( true );
	}
	log_error( "Error calculating address in ORG" );
	return( false );
}

static bool _asm_db( int line, char *label, char *opcode, char *arg, bool n_flag, bool s_flag ) {
	word	constants[ MAX_CONSTANTS ];
	int	l;
	bool	ret;

	ret = true;
	if( label ) {
		if( !set_symbol( label, this_address )) {
			if( assembler_pass == GATHER_PHASE ) {
				log_error( "Duplicate label for DB" );
			}
			else {
				log_error( "Inconsistent labelling for DB" );
			}
			ret = false;
		}
	}
	if(( l = analyse_value( arg, true, MAX_CONSTANTS, constants )) <= 0 ) {
		log_error( "Error in DB constants" );
		return( false );
	}
	if( assembler_pass == GENERATOR_PHASE ) {
		set_address( this_address );
		for( int i = 0; i < l; i++ ) {
			if(( H( constants[ i ]) != 0 )&&( H( constants[ i ]) != 0xff )) log_error( "Byte contant in DB too big" );
			if( s_flag ) {
				if( L( constants[ i ]) & 0x80 ) log_error( "Invalid DB value in top bit terminated data" );
				add_byte( L( constants[ i ])|(( i == l-1 )? 0x80: 0 ));
			}
			else {
				add_byte( L( constants[ i ]));
			}
		}
		if( n_flag ) add_byte( 0 );
	}
	this_address = advance_pc( this_address, l + ( n_flag? 1: 0 ));
	return( ret );
}

static bool asm_db( int line, char *label, char *opcode, char *arg ) {
	return( _asm_db( line, label, opcode, arg, false, false ));
}

static bool asm_db_n( int line, char *label, char *opcode, char *arg ) {
	return( _asm_db( line, label, opcode, arg, true, false ));
}

static bool asm_db_s( int line, char *label, char *opcode, char *arg ) {
	return( _asm_db( line, label, opcode, arg, false, true ));
}


static bool asm_dw( int line, char *label, char *opcode, char *arg ) {
	word	constants[ MAX_CONSTANTS ];
	int	len;
	bool	ret;

	ret = true;
	if( label ) {
		if( !set_symbol( label, this_address )) {
			if( assembler_pass == GATHER_PHASE ) {
				log_error( "Duplicate label for DW" );
			}
			else {
				log_error( "Inconsistent labelling for DW" );
			}
			ret = false;
		}
	}
	if(( len = analyse_value( arg, true, MAX_CONSTANTS, constants )) <= 0 ) {
		log_error( "Error in constants" );
		return( false );
	}
	if( assembler_pass == GENERATOR_PHASE ) {
		set_address( this_address );
		for( int i = 0; i < len; i++ ) {
			add_byte( H( constants[ i ]));
			add_byte( L( constants[ i ]));
		}
	}
	this_address = advance_pc( this_address, ( len << 1 ));
	return( ret );
}

static bool asm_ds( int line, char *label, char *opcode, char *arg ) {
	word	val;
	bool	ret;

	ret = true;
	if( label ) {
		if( !set_symbol( label, this_address )) {
			if( assembler_pass == GATHER_PHASE ) {
				log_error( "Duplicate label for DS" );
			}
			else {
				log_error( "Inconsistent labelling for DS" );
			}
			ret = false;
		}
	}
	if( analyse_value( arg, false, 1, &val ) == 1 ) {
		this_address = advance_pc( this_address, val );
		return( ret );
	}
	log_error( "Error calculating space required in DS" );
	return( false );
}

static bool asm_align( int line, char *label, char *opcode, char *arg ) {
	word	val;
	int	s;
	
	if( analyse_value( arg, false, 1, &val ) == 1 ) {
		if( val == 0 ) {
			log_error( "Zero alignment invalid" );
			return( false );
		}
		s = 0;
		while(( val & 1 ) == 0 ) {
			val >>= 1;
			s++;
		}
		if( val != 1 ) {
			log_error( "Alignment must be power of 2" );
			return( false );
		}
		val = ( val << s )-1;
		val = (( this_address + val )& ~val )- this_address;
		this_address = advance_pc( this_address, val );
		if( label ) {
			if( !set_symbol( label, this_address )) {
				if( assembler_pass == GATHER_PHASE ) {
					log_error( "Duplicate label for ALIGN" );
				}
				else {
					log_error( "Inconsistent labelling for ALIGN" );
				}
				return( false );
			}
		}
		return( true );
	}
	log_error( "Error calculating alignment in ALIGN" );
	return( false );
}




static bool _asm_fill( int line, char *label, char *opcode, char *arg, bool is_fill ) {
	word	constants[ 2 ],
		fill,
		count;
	int	l;
	bool	ret;

	ret = true;
	if( label ) {
		if( !set_symbol( label, this_address )) {
			if( assembler_pass == GATHER_PHASE ) {
				log_error( "Duplicate label for FILL/RZB" );
			}
			else {
				log_error( "Inconsistent labelling for FILL/RZB" );
			}
			ret = false;
		}
	}
	if(( l = analyse_value( arg, false, 2, constants )) < 1 ) {
		log_error( "Error in FILL/RZB constants" );
		return( false );
	}
	if( is_fill ) {
		// FILL value, count	Fill memory space
		if( l != 2 ) {
			log_error( "FILL argument missing" );
			return( false );
		}
		fill = constants[ 0 ];
		count = constants[ 1 ];
	}
	else {
		// RZB count{,value}	Reserve zero bytes
		count = constants[ 0 ];
		if( l > 1 ) {
			fill = constants[ 1 ];
		}
		else {
			fill = 0x00;
		}
	}
	if(( assembler_pass != GENERATOR_PHASE )&&( count > MAX_FILL )) {
		log_warning( "FILL/RZB too big, resetting" );
		count = 0;
	}
	if(( assembler_pass == GENERATOR_PHASE )&&( count > 0 )) {
		if(( H( fill ) != 0 )&&( H( fill ) != 0xff )) log_error( "Filler in FILL/RZB not 8 bit value" );
		set_address( this_address );
		for( int i = 0; i < count; i++ ) add_byte( L( fill ));
	}
	this_address = advance_pc( this_address, count );
	return( ret );
}




static bool asm_fill( int line, char *label, char *opcode, char *arg ) {
	return( _asm_fill( line, label, opcode, arg, true ) );
}

static bool asm_rzb( int line, char *label, char *opcode, char *arg ) {
	return( _asm_fill( line, label, opcode, arg, false ) );
}

static bool asm_unsupported( int line, char *label, char *opcode, char *arg ) {
	return( false );
}

//
//	This is a flag used to "end" a source file before
//	the actual end of the source code.
//
static bool end_not_reached = true;

static bool asm_end( int line, char *label, char *opcode, char *arg ) {
	word	val;
	bool	ret;

	end_not_reached = false;
	ret = true;
	if( label ) {
		if( !set_symbol( label, this_address )) {
			if( assembler_pass == GATHER_PHASE ) {
				log_error( "Duplicate label for END" );
			}
			else {
				log_error( "Inconsistent labelling for END" );
			}
			ret = false;
		}
	}
	if( analyse_value( arg, false, 1, &val ) == 1 ) {
		//
		//	Have value, set this address as the start address
		//	of the executable.
		//
		set_start( val );
	}
	else {
		log_error( "Error calculating START address" );
		ret = false;
	}
	return( ret );
}

//
//	Simple pointer to handle function type.
//
typedef bool FUNC( directive )( int line, char *label, char *opcode, char *arg );

//
//	Define table of directive and handler functions
//
typedef struct {
	char		*command;
	directive	handler;
} asm_command;

//
//	Command table and lookup routine
//
static asm_command directives[] = {
	//
	//	Symbolic equivalence.
	//
	{ "equ",	asm_equ		},
	//
	//	Setting the "virtual" Direct Page number
	//
	{ "setdp",	asm_setdp	},
	//
	//	Setting the target address for the next
	//	data item or machine instruction
	//
	{ "org",	asm_org		},
	//
	//	Define 8-bit data byte (or series of bytes)
	//
	//	FCB: Form Constant Byte. Each value is evaluated
	//	either to a number or a string. Numbers are
	//	truncated to 8 bits and stored directly as bytes.
	//
	//	FCC: Form Constant Characters. For strings, the ASCII
	//	value of each character is stored in sequential bytes.
	//
	//	FCN Identical to FCC, but a terminating zero byte is
	//	stored after the data.
	//
	//	FCS Identical to FCC, but the final byte has its top
	//	bit set to mark the end of the data. Other bytes are
	//	checked to ensure their top bits are not set.
	//
	{ "db",		asm_db		},
	{ "fcb",	asm_db		},
	{ "fcc",	asm_db		},
	{ "fcn",	asm_db_n	},
	{ "fcs",	asm_db_s	},
	//
	//	Define 16-bit data word (or series of words)
	//
	{ "dw",		asm_dw		},
	{ "fdb",	asm_dw		},
	//
	//	Define "space" (addresses with effectively
	//	zero bytes).
	//
	{ "ds",		asm_ds		},	// Force skip in target address
	{ "rmb",	asm_ds		},
	//
	{ "fill",	asm_fill	},	// FILL value, count	Fill memory space
	{ "rzb",	asm_rzb		},	// RZB count{,value}	Reserve zero bytes
	//
	//	Move the current target address forward until
	//	supports an alignment of "value" bytes.  This
	//	might means ther address does not change if the
	//	target address already meets this requirement.
	//
	{ "align",	asm_align	},
	//
	//	Unsuported assembler directives (so far)
	//
	{ "set",	asm_unsupported	},	// Like EQU except a symbol *can* be reset
	{ "reg",	asm_unsupported	},	// Create an alias for a list of registers
	{ "rpt",	asm_unsupported	},	// Repeat following assembler instruction
	//
	//	End of source code directive.
	//
	{ "end",	asm_end		},
	{ NULL }
};

//
//	Lookup asm directive
//
static directive find_directive( char *command ) {
	asm_command	*look;

	for( look = directives; look->command != NULL; look++ ) {
		if( strcasecmp( command, look->command ) == 0 ) {
			return( look->handler );
		}
	}
	return( NULL );
}

////////////////////////////////////////////////////////////////////////
//
//	parse_input - read the supplied stream end-to-end
//	and (depending on assembler_pass) gather data on the source
//	or emit machine code information
//
////////////////////////////////////////////////////////////////////////

//
//	Look forward through a string, but never past EOS
//	and skip over strings as a single "character".
//
//	Returns address of the next byte to be examined.
//
//	NOTES:
//
//	1/	This routine is broken, it does not skip over
//		strings containing escaped quote(s)
//		properly.
//
//	2/	Legacy syntax support also has an impact with
//		respect to support of character constants and
//		alternative string delimiting (/).
//
static char *look_forward( char *from ) {
	char	q;

	switch(( q = *from )) {
		case EOS: {
			return( from );
		}
		case SLASH:
		case QUOTE:
		case QUOTES: {
			char	*peek, c;
			
			peek = from + 1;
			if( option_flags & LEGACY_SYNTAX ) {
				//
				//	Legacy and a QUOTE means there
				//	is no matching quote to search for.
				//
				if( q == QUOTE ) {
					if( *peek != EOS ) peek++;
					return( peek );
				}
			}
			else {
				//
				//	Non-legacy and a SLASH is just
				//	another character.
				//
				if( q == SLASH ) return( peek );
			}
			while((( c = *peek ) != EOS )&&( c != q )) {
				peek++;
				if(( c == ESCAPE )&&( *peek != EOS )) peek++;
			}
			if( c == EOS ) {
				//
				//	If we were matching a slash (legacy string)
				//	and we find no matching pair we assume its
				//	a divide symbol, so break out of here.
				//
				if( q == SLASH ) break;
				//
				//	otherwise there an error.
				//
				log_error( "Unmatched delimiter on constant" );
			}
			else {
				peek++;
			}
			return( peek );
		}
		default: {
			break;
		}
	}
	return( from + 1 );
}
			
static bool break_line( char *line, char **label, char **opcode, char **arg, char **comment ) {	
	//
	//	Label at start of line?
	//
	if( is_ident_1( *line )) {
		*label = line++;
		while( is_ident_2( *line )) line++;
		if(( *line != EOS )&&( !isspace( *line ))&&( *line != COLON )) {
			log_error( "Label incorrectly terminated" );
			return( false );
		}
		*line++ = EOS;
	}
	else {
		*label = NULL;
	}
	//
	//	skip space then capture the opcode
	//
	while( isspace( *line )) line++;
	if( isalpha( *line )) {
		*opcode = line;
		while( isalnum( *line )) line++;
		if( *line != EOS ) { 
			if( !isspace( *line )) {
				log_error( "Opcode incorrectly terminated" );
				return( false );
			}
			*line++ = EOS;
		}
	}
	else {
		*opcode = NULL;
	}
	//
	//	skip space then capture the argument, if present.
	//
	while( isspace( *line )) line++;
	if(( *line != EOS )&&( *line != ';' )) {
		int	l;
		char	*a;

		l = 0;
		*arg = a = line;
		while(( *line != EOS )&&( *line != ';' )) {
			line = look_forward( line );
			l++;
		}
		if( *line == ';' ) *line++ = EOS;
		while(( l > 0 )&&( isspace( a[ l-1 ]))) a[ --l ] = EOS;
	}
	else {
		*arg = NULL;
	}
	//
	//	skip space then comment or end of line.
	//
	while( isspace ( *line )) line++;
	if( *line == ';' ) {
		line++;
		while( isspace ( *line )) line++;
	}
	*comment = line;
	//
	//	Done!
	//
	return( true );
}

static int parse_input( FILE *input ) {
	char		line[ BUFFER ],
			*label,
			*opcode,
			*arg,
			*comment,
			*err;
	int		count,
			errors;

	directive	dptr;

	count = 0;
	errors = 0;
	end_not_reached = true;
	while( end_not_reached && fgets( line, BUFFER, input )) {
		line[ strlen( line ) -1 ] = EOS;
		//
		//	Add one to the line count and
		//	pass the line to the output system
		//	if we are generating code.
		//
		count++;
		if( assembler_pass == GENERATOR_PHASE ) next_line( count, line );
		//
		//	What does it contain?
		//
		if( break_line( line, &label, &opcode, &arg, &comment )) {
			PRINT_DEBUG(( "Input:%d|%s|%s|%s|%s\n", count, label, opcode, arg, comment ));
			//
			//	Anything to do?
			//
			if( opcode ) {
				//
				//	Is this an assembler command?
				//
				if(( dptr = find_directive( opcode )) != NULL ) {
					if( !FUNC( dptr )( count, label, opcode, arg )) log_error( "Assembler directive error" );
				}
				else {
					//
					//	If there is a label, note its address
					//
					if( label != NULL ) set_symbol( label, this_address );
					//
					//	Now process the machine instruction
					//
					if( !process_machine_inst( count, opcode, arg )) log_error( "Machine instruction error" );
				}
			}
			else {
				//
				//	No opcode, so only need to do anything if there is a label.
				//
				if( label ) {
					if( !set_symbol( label, this_address )) log_error( "Inconsistent label value" );
				}
			}
		}
		else {
			log_error( "Source line incorrectly formed" );
		}
		//
		//	Errors or Warnings?
		//
		if(( err = error_text( 0 )) != NULL ) {
			int	n = 0;

			do {
				fprintf( stderr, "Error Line %d: %s.\n", count, err );
				n++;
			} while (( err = error_text( n )) != NULL );
			//
			//	Count line with errors.
			//
			errors++;
		}
		if(( err = warning_text( 0 )) != NULL ) {
			int	n = 0;

			do {
				fprintf( stderr, "Warning Line %d: %s.\n", count, err );
				n++;
			} while (( err = warning_text( n )) != NULL );
		}
		reset_error_cache();
		reset_warning_cache();
	}
	return( errors == 0 );
}

//
//	Implement a simplistic argument processing system
//
typedef struct {
	char		*name,
			*help;
	program_options	set,
			reset;
	inst_options	iso;
} option;
static option options[] = {
	//
	//	Output related options.
	//
	{	"--hexadecimal",	"\tOutput text hexadecimal values",	DISPLAY_TEXT,		DISPLAY_MASK,		ISO_NONE	},
	{	"--intel",		"\t\tOutput Intel Hex format data",	DISPLAY_INTEL,		DISPLAY_MASK,		ISO_NONE	},
	{	"--motorola",		"\tOutput Motorola S records",		DISPLAY_MOTOROLA,	DISPLAY_MASK,		ISO_NONE	},
	{	"--listing",		"\tOutput an assembly listing",		DISPLAY_LISTING,	DISPLAY_MASK,		ISO_NONE	},
	{	"--no-output",		"\tDo not output any code",		DISPLAY_NOTHING,	DISPLAY_MASK,		ISO_NONE	},
	{	"--stdout",		"\tSend output to console",		DISPLAY_STDOUT,		OPTIONS_NONE,		ISO_NONE	},
	//
	//	Option to enable relocation data in output.
	//
	{	"--relocate",		"\tInclude relocation data in output",	RELOCATION_DATA,	OPTIONS_NONE,		ISO_NONE	},
	//
	//	Syntax options.
	//
	{	"--legacy-syntax",	"\tAccept legacy Motorola syntax",	LEGACY_SYNTAX,		OPTIONS_NONE,		ISO_NONE	},
	//
	//	Assembly information.
	//
	{	"--symbols",		"\tOutput Symbol table",		DISPLAY_SYMBOLS,	OPTIONS_NONE,		ISO_NONE	},
	{	"--dump-opcodes",	"\tDisplay op-codes table",		DISPLAY_OPCODES,	OPTIONS_NONE,		ISO_NONE	},
	//
	//	Instruction Set options.
	//
	{	"--mc6809",		"\tSelect MC6809 instructions",		OPTIONS_NONE,		OPTIONS_NONE,		ISO_MC6809		},
	{	"--hd6309",		"\tSelect MC6809 and HD6309 instructions", OPTIONS_NONE,	OPTIONS_NONE,		ISO_MC6809|ISO_HD6309	},
	{	"--undocumented",	"\tInclude undocumented instructions",	OPTIONS_NONE,		OPTIONS_NONE,		ISO_UNDOCUMENTED	},
	{	"--aliases",		"\tInclude instruction aliases",	OPTIONS_NONE,		OPTIONS_NONE,		ISO_ALIASES		},
	//
	//	Command line help and debugging (if selected at compile time).
	//
	{	"--help",		"\t\tDisplay this help information",	DISPLAY_HELP,		OPTIONS_NONE,		ISO_NONE	},
#ifdef ENABLE_DEBUG
	{	"--debug",		"\t\tEnable debugging output",		DISPLAY_DEBUG,		OPTIONS_NONE,		ISO_NONE	},
	{	"--debug-verbose",	"\tAdditional debugging output",	DISPLAY_DEBUG|DISPLAY_VEBOSE,	OPTIONS_NONE,	ISO_NONE	},
#endif
	{	NULL																}
};

//
//	Handed the whole argument list so look for the filename
//	and return its index (while gathering the arguments on the way).
//
//	Return 0 or -ve number if the program should exit as a result of
//	this processing.
//
static int parse_arguments( int argc, char **argv ) {
	int	a;
	option	*o;

	//
	//	Step through arguments..
	//
	for( a = 1; a < argc; a++ ) {
		if( strncmp( argv[ a ], "--", 2 ) == 0 ) {
			for( o = options; o->name != NULL; o++ ) {
				if( strcmp( argv[ a ], o->name ) == 0 ) break;
			}
			if( o->name != NULL ) {
				option_flags = ( option_flags & ~o->reset ) | o->set;
				iso_options |= o->iso;
			}
			else {
				fprintf( stderr, "Unrecognised option '%s'.\n", argv[ a ]);
				return( -1 );
			}
		}
		else break;
	}
	//
	//	Analyse what we found..
	//
	if( option_flags & DISPLAY_HELP ) {
		printf( "Usage: %s [ {options} ] {filename}\nOptions:-\n", argv[ 0 ]);
		for( o = options; o->name != NULL; o++ ) printf( "\t%s%s\n", o->name, o->help );
		return( 0 );
	}
	if( option_flags & DISPLAY_OPCODES ) {
		printf( "Op-codes dump:\n" );
		dump_opcodes();
		return( 0 );
	}
	if(( option_flags & DISPLAY_MASK ) == 0 ) {
		printf( "Output format not specified.\n" );
		return( -2 );
	}
	if(( iso_options & (ISO_MC6809|ISO_HD6309)) == 0 ) {
		printf( "Target CPU not specified.\n" );
		return( -3 );
	}		
	return(( a >= argc )? 0: a );
}

////////////////////////////////////////////////////////////////////////
//
//	Entry point to assembler.
//
//	Single argument is name of file to assemble.
//
////////////////////////////////////////////////////////////////////////

int main( int argc, char *argv[] ) {
	FILE	*source;
	int	file, last, count;

	//
	//	Start with the arguments
	//
	if(( file = parse_arguments( argc, argv )) <= 0 ) return( -file );
	if(( source = fopen( argv[ file ], "r" )) == NULL ) {
		fprintf( stderr, "Unable to open file '%s', error '%m'.\n", argv[ file ]);
		return( 4 );
	}
	//
	//	Symbol scanning pass
	//
	reset_conditions( GATHER_PHASE );
	if( !parse_input( source )) {
		//
		//	Display error message and exit
		//
		fprintf( stderr, "Errors detected in gather pass.\n" );
		return( 5 );
	}
	//
	//	Normalisation pass
	//
	sym_sort = sort_symbols( sym_root, NULL );
	last = INT_MAX;
	while( true ) {
		//
		//	Restart for a Normalisation pass.
		//
		rewind( source );
		reset_conditions( NORMALISE_PHASE );
		if( option_flags & DISPLAY_DEBUG ) show_symbols( sym_sort );
		if( !parse_input( source )) {
			//
			//	Display error message and exit
			//
			fprintf( stderr, "Errors detected in normalisation pass.\n" );
			return( 6 );
		}	
		//
		//	Have things improved?
		//
		if(( count = normalisation_count()) >= last ) {
			//
			//	Normalisation failed, the same number
			//	of tweaks (or more!) has occured.  There is an
			//	unresolved co-dependency.
			//
			fprintf( stderr, "Symbol normalisation cannot be resolved.\n" );
			return( 7 );
		}
		if( count == 0 ) {
			//
			//	No adjustments means we are normalised!
			//
			break;
		}
		//
		//	Round again.
		//
		last = count;
	}
	//
	//	Code generation pass
	//
	if( init_output( argv[ file ])) {
		rewind( source );
		reset_conditions( GENERATOR_PHASE );
		if( !parse_input( source )) {
			end_output();
			fprintf( stderr, "Code generation pass failed.\n" );
			return( 8 );
		}
		end_output();
	}
	else {
		fprintf( stderr, "Initialisation of output target failed.\n" );
		return( 9 );
	}
	//
	//	Output complete!
	//
	if( option_flags & DISPLAY_SYMBOLS ) show_symbols( sym_sort );
	//
	//	Done.
	//
	fclose( source );
	return( 0 );
}

//
//	EOF
//
