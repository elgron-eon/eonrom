#
# jonesforth.S translated to eon cpu
# (c) JCGV, agosto del 2022
#

#
#	A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
#	By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth
#	This is PUBLIC DOMAIN (see public domain release statement below).
#	$Id: jonesforth.S,v 1.47 2009-09-11 08:33:13 rich Exp $
#
#	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -Wl,--build-id=none -o jonesforth jonesforth.S
#
JONES_VERSION	.EQU	47

#	INTRODUCTION ----------------------------------------------------------------------
#
#	FORTH is one of those alien languages which most working programmers regard in the same
#	way as Haskell, LISP, and so on.  Something so strange that they'd rather any thoughts
#	of it just go away so they can get on with writing this paying code.  But that's wrong
#	and if you care at all about programming then you should at least understand all these
#	languages, even if you will never use them.

#	LISP is the ultimate high-level language, and features from LISP are being added every
#	decade to the more common languages.  But FORTH is in some ways the ultimate in low level
#	programming.  Out of the box it lacks features like dynamic memory management and even
#	strings.  In fact, at its primitive level it lacks even basic concepts like IF-statements
#	and loops.

#	Why then would you want to learn FORTH?  There are several very good reasons.  First
#	and foremost, FORTH is minimal.  You really can write a complete FORTH in, say, 2000
#	lines of code.	I don't just mean a FORTH program, I mean a complete FORTH operating
#	system, environment and language.  You could boot such a FORTH on a bare PC and it would
#	come up with a prompt where you could start doing useful work.	The FORTH you have here
#	isn't minimal and uses a Linux process as its 'base PC' (both for the purposes of making
#	it a good tutorial). It's possible to completely understand the system.  Who can say they
#	completely understand how Linux works, or gcc?

#	Secondly FORTH has a peculiar bootstrapping property.  By that I mean that after writing
#	a little bit of assembly to talk to the hardware and implement a few primitives, all the
#	rest of the language and compiler is written in FORTH itself.  Remember I said before
#	that FORTH lacked IF-statements and loops?  Well of course it doesn't really because
#	such a lanuage would be useless, but my point was rather that IF-statements and loops are
#	written in FORTH itself.

#	Now of course this is common in other languages as well, and in those languages we call
#	them 'libraries'.  For example in C, 'printf' is a library function written in C.  But
#	in FORTH this goes way beyond mere libraries.  Can you imagine writing C's 'if' in C?
#	And that brings me to my third reason: If you can write 'if' in FORTH, then why restrict
#	yourself to the usual if/while/for/switch constructs?  You want a construct that iterates
#	over every other element in a list of numbers?	You can add it to the language.  What
#	about an operator which pulls in variables directly from a configuration file and makes
#	them available as FORTH variables?  Or how about adding Makefile-like dependencies to
#	the language?  No problem in FORTH.  How about modifying the FORTH compiler to allow
#	complex inlining strategies -- simple.	This concept isn't common in programming languages,
#	but it has a name (in fact two names): "macros" (by which I mean LISP-style macros, not
#	the lame C preprocessor) and "domain specific languages" (DSLs).

#	This tutorial isn't about learning FORTH as the language.  I'll point you to some references
#	you should read if you're not familiar with using FORTH.  This tutorial is about how to
#	write FORTH.  In fact, until you understand how FORTH is written, you'll have only a very
#	superficial understanding of how to use it.

#	So if you're not familiar with FORTH or want to refresh your memory here are some online
#	references to read:

#	http://en.wikipedia.org/wiki/Forth_%28programming_language%29
#	http://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm
#	http://wiki.laptop.org/go/Forth_Lessons
#	http://www.albany.net/~hello/simple.htm
#	Here is another "Why FORTH?" essay: http://www.jwdt.com/~paysan/why-forth.html
#	Discussion and criticism of this FORTH here: http://lambda-the-ultimate.org/node/2452

#	ACKNOWLEDGEMENTS ----------------------------------------------------------------------

#	This code draws heavily on the design of LINA FORTH (http://home.hccnet.nl/a.w.m.van.der.horst/lina.html)
#	by Albert van der Horst.  Any similarities in the code are probably not accidental.

#	Some parts of this FORTH are also based on this IOCCC entry from 1992:
#	http://ftp.funet.fi/pub/doc/IOCCC/1992/buzzard.2.design.
#	I was very proud when Sean Barrett, the original author of the IOCCC entry, commented in the LtU thread
#	http://lambda-the-ultimate.org/node/2452#comment-36818 about this FORTH.

#	And finally I'd like to acknowledge the (possibly forgotten?) authors of ARTIC FORTH because their
#	original program which I still have on original cassette tape kept nagging away at me all these years.
#	http://en.wikipedia.org/wiki/Artic_Software

#	PUBLIC DOMAIN ----------------------------------------------------------------------

#	I, the copyright holder of this work, hereby release it into the public domain. This applies worldwide.

#	In case this is not legally possible, I grant any entity the right to use this work for any purpose,
#	without any conditions, unless such conditions are required by law.

#	SETTING UP ----------------------------------------------------------------------

#	Let's get a few housekeeping things out of the way.  Firstly because I need to draw lots of
#	ASCII-art diagrams to explain concepts, the best way to look at this is using a window which
#	uses a fixed width font and is at least this wide:

#<------------------------------------------------------------------------------------------------------------------------>

#	Secondly make sure TABS are set to 8 characters.  The following should be a vertical
#	line.  If not, sort out your tabs.

#		|
#		|
#		|

#	Thirdly I assume that your screen is at least 50 characters high.

#	ASSEMBLING ----------------------------------------------------------------------

#	If you want to actually run this FORTH, rather than just read it, you will need Linux on an
#	i386.  Linux because instead of programming directly to the hardware on a bare PC which I
#	could have done, I went for a simpler tutorial by assuming that the 'hardware' is a Linux
#	process with a few basic system calls (read, write and exit and that's about all).  i386
#	is needed because I had to write the assembly for a processor, and i386 is by far the most
#	common.  (Of course when I say 'i386', any 32- or 64-bit x86 processor will do.  I'm compiling
#	this on a 64 bit AMD Opteron).

#	Again, to assemble this you will need gcc and gas (the GNU assembler).	The commands to
#	assemble and run the code (save this file as 'jonesforth.S') are:

#	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -Wl,--build-id=none -o jonesforth jonesforth.S
#	cat jonesforth.f - | ./jonesforth

#	If you want to run your own FORTH programs you can do:

#	cat jonesforth.f myprog.f | ./jonesforth

#	If you want to load your own FORTH code and then continue reading user commands, you can do:

#	cat jonesforth.f myfunctions.f - | ./jonesforth

#	ASSEMBLER ----------------------------------------------------------------------

#	(You can just skip to the next section -- you don't need to be able to read assembler to
#	follow this tutorial).

#	However if you do want to read the assembly code here are a few notes about gas (the GNU assembler):

#	(1) Register names are prefixed with '%', so %eax is the 32 bit i386 accumulator.  The registers
#	    available on i386 are: %eax, %ebx, %ecx, %edx, %esi, %edi, %ebp and %esp, and most of them
#	    have special purposes.

#	(2) Add, mov, etc. take arguments in the form SRC,DEST.  So mov %eax,%ecx moves %eax -> %ecx

#	(3) Constants are prefixed with '$', and you mustn't forget it!  If you forget it then it
#	    causes a read from memory instead, so:
#	    mov $2,%eax 	moves number 2 into %eax
#	    mov 2,%eax		reads the 32 bit word from address 2 into %eax (ie. most likely a mistake)

#	(4) gas has a funky syntax for local labels, where '1f' (etc.) means label '1:' "forwards"
#	    and '1b' (etc.) means label '1:' "backwards".  Notice that these labels might be mistaken
#	    for hex numbers (eg. you might confuse 1b with $0x1b).

#	(5) 'ja' is "jump if above", 'jb' for "jump if below", 'je' "jump if equal" etc.

#	(6) gas has a reasonably nice .macro syntax, and I use them a lot to make the code shorter and
#	    less repetitive.

#	For more help reading the assembler, do "info gas" at the Linux prompt.

#	Now the tutorial starts in earnest.

#	THE DICTIONARY ----------------------------------------------------------------------

#	In FORTH as you will know, functions are called "words", and just as in other languages they
#	have a name and a definition.  Here are two FORTH words:

#	: DOUBLE DUP + ;		\ name is "DOUBLE", definition is "DUP +"
#	: QUADRUPLE DOUBLE DOUBLE ;	\ name is "QUADRUPLE", definition is "DOUBLE DOUBLE"

#	Words, both built-in ones and ones which the programmer defines later, are stored in a dictionary
#	which is just a linked list of dictionary entries.

#	<--- DICTIONARY ENTRY (HEADER) ----------------------->
#	+------------------------+--------+---------- - - - - +----------- - - - -
#	| LINK POINTER		 | LENGTH/| NAME	      | DEFINITION
#	|			 | FLAGS  |		      |
#	+--- (4 bytes) ----------+- byte -+- n bytes  - - - - +----------- - - - -

#	I'll come to the definition of the word later.	For now just look at the header.  The first
#	4 bytes are the link pointer.  This points back to the previous word in the dictionary, or, for
#	the first word in the dictionary it is just a NULL pointer.  Then comes a length/flags byte.
#	The length of the word can be up to 31 characters (5 bits used) and the top three bits are used
#	for various flags which I'll come to later.  This is followed by the name itself, and in this
#	implementation the name is rounded up to a multiple of 4 bytes by padding it with zero bytes.
#	That's just to ensure that the definition starts on a 32 bit boundary.

#	A FORTH variable called LATEST contains a pointer to the most recently defined word, in
#	other words, the head of this linked list.

#	DOUBLE and QUADRUPLE might look like this:

#	  pointer to previous word
#	   ^
#	   |
#	+--|------+---+---+---+---+---+---+---+---+------------- - - - -
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | (definition ...)
#	+---------+---+---+---+---+---+---+---+---+------------- - - - -
#	   ^	   len			       padding
#	   |
#	+--|------+---+---+---+---+---+---+---+---+---+---+---+---+------------- - - - -
#	| LINK	  | 9 | Q | U | A | D | R | U | P | L | E | 0 | 0 | (definition ...)
#	+---------+---+---+---+---+---+---+---+---+---+---+---+---+------------- - - - -
#	   ^	   len					   padding
#	   |
#	   |
#	  LATEST

#	You should be able to see from this how you might implement functions to find a word in
#	the dictionary (just walk along the dictionary entries starting at LATEST and matching
#	the names until you either find a match or hit the NULL pointer at the end of the dictionary);
#	and add a word to the dictionary (create a new definition, set its LINK to LATEST, and set
#	LATEST to point to the new word).  We'll see precisely these functions implemented in
#	assembly code later on.

#	One interesting consequence of using a linked list is that you can redefine words, and
#	a newer definition of a word overrides an older one.  This is an important concept in
#	FORTH because it means that any word (even "built-in" or "standard" words) can be
#	overridden with a new definition, either to enhance it, to make it faster or even to
#	disable it.  However because of the way that FORTH words get compiled, which you'll
#	understand below, words defined using the old definition of a word continue to use
#	the old definition.  Only words defined after the new definition use the new definition.

#	DIRECT THREADED CODE ----------------------------------------------------------------------

#	Now we'll get to the really crucial bit in understanding FORTH, so go and get a cup of tea
#	or coffee and settle down.  It's fair to say that if you don't understand this section, then you
#	won't "get" how FORTH works, and that would be a failure on my part for not explaining it well.
#	So if after reading this section a few times you don't understand it, please email me
#	(rich@annexia.org).

#	Let's talk first about what "threaded code" means.  Imagine a peculiar version of C where
#	you are only allowed to call functions without arguments.  (Don't worry for now that such a
#	language would be completely useless!)	So in our peculiar C, code would look like this:

#	f ()
#	{
#	  a ();
#	  b ();
#	  c ();
#	}

#	and so on.  How would a function, say 'f' above, be compiled by a standard C compiler?
#	Probably into assembly code like this.	On the right hand side I've written the actual
#	i386 machine code.

#	f:
#	  CALL a			E8 08 00 00 00
#	  CALL b			E8 1C 00 00 00
#	  CALL c			E8 2C 00 00 00
#	  ; ignore the return from the function for now

#	"E8" is the x86 machine code to "CALL" a function.  In the first 20 years of computing
#	memory was hideously expensive and we might have worried about the wasted space being used
#	by the repeated "E8" bytes.  We can save 20% in code size (and therefore, in expensive memory)
#	by compressing this into just:

#	08 00 00 00		Just the function addresses, without
#	1C 00 00 00		the CALL prefix.
#	2C 00 00 00

#	On a 16-bit machine like the ones which originally ran FORTH the savings are even greater - 33%.

#	[Historical note: If the execution model that FORTH uses looks strange from the following
#	paragraphs, then it was motivated entirely by the need to save memory on early computers.
#	This code compression isn't so important now when our machines have more memory in their L1
#	caches than those early computers had in total, but the execution model still has some
#	useful properties].

#	Of course this code won't run directly on the CPU any more.  Instead we need to write an
#	interpreter which takes each set of bytes and calls it.

#	On an i386 machine it turns out that we can write this interpreter rather easily, in just
#	two assembly instructions which turn into just 3 bytes of machine code.  Let's store the
#	pointer to the next word to execute in the %esi register:

#		08 00 00 00	<- We're executing this one now.  %esi is the _next_ one to execute.
#	%esi -> 1C 00 00 00
#		2C 00 00 00

#	The all-important i386 instruction is called LODSL (or in Intel manuals, LODSW).  It does
#	two things.  Firstly it reads the memory at %esi into the accumulator (%eax).  Secondly it
#	increments %esi by 4 bytes.  So after LODSL, the situation now looks like this:

#		08 00 00 00	<- We're still executing this one
#		1C 00 00 00	<- %eax now contains this address (0x0000001C)
#	%esi -> 2C 00 00 00

#	Now we just need to jump to the address in %eax.  This is again just a single x86 instruction
#	written JMP *(%eax).  And after doing the jump, the situation looks like:

#		08 00 00 00
#		1C 00 00 00	<- Now we're executing this subroutine.
#	%esi -> 2C 00 00 00

#	To make this work, each subroutine is followed by the two instructions 'LODSL; JMP *(%eax)'
#	which literally make the jump to the next subroutine.

#	And that brings us to our first piece of actual code!  Well, it's a macro.
#

#	.macro NEXT
#	lodsl
#	jmp *(%eax)
#	.endm
#EON#	R12 = ESI, R0 = EAX
#EON#	ld4	r0, [r12]
#EON#	add	4, r12
#EON#	ld4	r1, [r0]
#EON#	jmp	r1

#	The macro is called NEXT.  That's a FORTH-ism.	It expands to those two instructions.

#	Every FORTH primitive that we write has to be ended by NEXT.  Think of it kind of like
#	a return.

#	The above describes what is known as direct threaded code.

#	To sum up: We compress our function calls down to a list of addresses and use a somewhat
#	magical macro to act as a "jump to next function in the list".	We also use one register (%esi)
#	to act as a kind of instruction pointer, pointing to the next function in the list.

#	I'll just give you a hint of what is to come by saying that a FORTH definition such as:

#	: QUADRUPLE DOUBLE DOUBLE ;

#	actually compiles (almost, not precisely but we'll see why in a moment) to a list of
#	function addresses for DOUBLE, DOUBLE and a special function called EXIT to finish off.

#	At this point, REALLY EAGLE-EYED ASSEMBLY EXPERTS are saying "JONES, YOU'VE MADE A MISTAKE!".

#	I lied about JMP *(%eax).

#	INDIRECT THREADED CODE ----------------------------------------------------------------------

#	It turns out that direct threaded code is interesting but only if you want to just execute
#	a list of functions written in assembly language.  So QUADRUPLE would work only if DOUBLE
#	was an assembly language function.  In the direct threaded code, QUADRUPLE would look like:

#		+------------------+
#		| addr of DOUBLE  --------------------> (assembly code to do the double)
#		+------------------+			NEXT
#	%esi -> | addr of DOUBLE   |
#		+------------------+

#	We can add an extra indirection to allow us to run both words written in assembly language
#	(primitives written for speed) and words written in FORTH themselves as lists of addresses.

#	The extra indirection is the reason for the brackets in JMP *(%eax).

#	Let's have a look at how QUADRUPLE and DOUBLE really look in FORTH:

#		: QUADRUPLE DOUBLE DOUBLE ;

#		+------------------+
#		| codeword	   |		   : DOUBLE DUP + ;
#		+------------------+
#		| addr of DOUBLE  ---------------> +------------------+
#		+------------------+		   | codeword	      |
#		| addr of DOUBLE   |		   +------------------+
#		+------------------+		   | addr of DUP   --------------> +------------------+
#		| addr of EXIT	   |		   +------------------+ 	   | codeword	   -------+
#		+------------------+	   %esi -> | addr of +	   --------+	   +------------------+   |
#						   +------------------+    |	   | assembly to    <-----+
#						   | addr of EXIT     |    |	   | implement DUP    |
#						   +------------------+    |	   |	..	      |
#									   |	   |	..	      |
#									   |	   | NEXT	      |
#									   |	   +------------------+
#									   |
#									   +-----> +------------------+
#										   | codeword	   -------+
#										   +------------------+   |
#										   | assembly to   <------+
#										   | implement +      |
#										   |	..	      |
#										   |	..	      |
#										   | NEXT	      |
#										   +------------------+

#	This is the part where you may need an extra cup of tea/coffee/favourite caffeinated
#	beverage.  What has changed is that I've added an extra pointer to the beginning of
#	the definitions.  In FORTH this is sometimes called the "codeword".  The codeword is
#	a pointer to the interpreter to run the function.  For primitives written in
#	assembly language, the "interpreter" just points to the actual assembly code itself.
#	They don't need interpreting, they just run.

#	In words written in FORTH (like QUADRUPLE and DOUBLE), the codeword points to an interpreter
#	function.

#	I'll show you the interpreter function shortly, but let's recall our indirect
#	JMP *(%eax) with the "extra" brackets.	Take the case where we're executing DOUBLE
#	as shown, and DUP has been called.  Note that %esi is pointing to the address of +

#	The assembly code for DUP eventually does a NEXT.  That:

#	(1) reads the address of + into %eax		%eax points to the codeword of +
#	(2) increments %esi by 4
#	(3) jumps to the indirect %eax			jumps to the address in the codeword of +,
#							ie. the assembly code to implement +

#		+------------------+
#		| codeword	   |
#		+------------------+
#		| addr of DOUBLE  ---------------> +------------------+
#		+------------------+		   | codeword	      |
#		| addr of DOUBLE   |		   +------------------+
#		+------------------+		   | addr of DUP   --------------> +------------------+
#		| addr of EXIT	   |		   +------------------+ 	   | codeword	   -------+
#		+------------------+		   | addr of +	   --------+	   +------------------+   |
#						   +------------------+    |	   | assembly to    <-----+
#					   %esi -> | addr of EXIT     |    |	   | implement DUP    |
#						   +------------------+    |	   |	..	      |
#									   |	   |	..	      |
#									   |	   | NEXT	      |
#									   |	   +------------------+
#									   |
#									   +-----> +------------------+
#										   | codeword	   -------+
#										   +------------------+   |
#									now we're  | assembly to    <-----+
#									executing  | implement +      |
#									this	   |	..	      |
#									function   |	..	      |
#										   | NEXT	      |
#										   +------------------+

#	So I hope that I've convinced you that NEXT does roughly what you'd expect.  This is
#	indirect threaded code.

#	I've glossed over four things.	I wonder if you can guess without reading on what they are?

#	.
#	.
#	.

#	My list of four things are: (1) What does "EXIT" do?  (2) which is related to (1) is how do
#	you call into a function, ie. how does %esi start off pointing at part of QUADRUPLE, but
#	then point at part of DOUBLE.  (3) What goes in the codeword for the words which are written
#	in FORTH?  (4) How do you compile a function which does anything except call other functions
#	ie. a function which contains a number like : DOUBLE 2 * ; ?

#	THE INTERPRETER AND RETURN STACK ------------------------------------------------------------

#	Going at these in no particular order, let's talk about issues (3) and (2), the interpreter
#	and the return stack.

#	Words which are defined in FORTH need a codeword which points to a little bit of code to
#	give them a "helping hand" in life.  They don't need much, but they do need what is known
#	as an "interpreter", although it doesn't really "interpret" in the same way that, say,
#	Java bytecode used to be interpreted (ie. slowly).  This interpreter just sets up a few
#	machine registers so that the word can then execute at full speed using the indirect
#	threaded model above.

#	One of the things that needs to happen when QUADRUPLE calls DOUBLE is that we save the old
#	%esi ("instruction pointer") and create a new one pointing to the first word in DOUBLE.
#	Because we will need to restore the old %esi at the end of DOUBLE (this is, after all, like
#	a function call), we will need a stack to store these "return addresses" (old values of %esi).

#	As you will have seen in the background documentation, FORTH has two stacks, an ordinary
#	stack for parameters, and a return stack which is a bit more mysterious.  But our return
#	stack is just the stack I talked about in the previous paragraph, used to save %esi when
#	calling from a FORTH word into another FORTH word.

#	In this FORTH, we are using the normal stack pointer (%esp) for the parameter stack.
#	We will use the i386's "other" stack pointer (%ebp, usually called the "frame pointer")
#	for our return stack.
#EON#	SP = parameter stack, R13 = return stack, R12 = ESI, R0 = EAX

#	I've got two macros which just wrap up the details of using %ebp for the return stack.
#	You use them as for example "PUSHRSP %eax" (push %eax on the return stack) or "POPRSP %ebx"
#	(pop top of return stack into %ebx).


# Macros to deal with the return stack.
#	.macro PUSHRSP reg
#	lea -4(%ebp),%ebp	// push reg on to return stack
#	movl \reg,(%ebp)
#	.endm
#EON#	sub	r13, 4
#EON#	st4	[r13], r0

#	.macro POPRSP reg
#	mov (%ebp),\reg 	// pop top of return stack to reg
#	lea 4(%ebp),%ebp
#	.endm
#EON#	ld4	reg, [r13]
#EON#	add	r13, 4

#	And with that we can now talk about the interpreter.

#	In FORTH the interpreter function is often called DOCOL (I think it means "DO COLON" because
#	all FORTH definitions start with a colon, as in : DOUBLE DUP + ;

#	The "interpreter" (it's not really "interpreting") just needs to push the old %esi on the
#	stack and set %esi to the first word in the definition.  Remember that we jumped to the
#	function using JMP *(%eax)?  Well a consequence of that is that conveniently %eax contains
#	the address of this codeword, so just by adding 4 to it we get the address of the first
#	data word.  Finally after setting up %esi, it just does NEXT which causes that first word
#	to run.

# DOCOL - the interpreter!
DOCOL		SUB	R13, 4
		ST4	[R13], R12	; push %esi on to the return stack
		ADD	R12, R0, 4	; %eax points to codeword, make %esi point to first data word
		BRA	NEXT

#	Just to make this absolutely clear, let's see how DOCOL works when jumping from QUADRUPLE
#	into DOUBLE:

#		QUADRUPLE:
#		+------------------+
#		| codeword	   |
#		+------------------+		   DOUBLE:
#		| addr of DOUBLE  ---------------> +------------------+
#		+------------------+	   %eax -> | addr of DOCOL    |
#	%esi -> | addr of DOUBLE   |		   +------------------+
#		+------------------+		   | addr of DUP      |
#		| addr of EXIT	   |		   +------------------+
#		+------------------+		   | etc.	      |

#	First, the call to DOUBLE calls DOCOL (the codeword of DOUBLE).  DOCOL does this:  It
#	pushes the old %esi on the return stack.  %eax points to the codeword of DOUBLE, so we
#	just add 4 on to it to get our new %esi:

#		 QUADRUPLE:
#		 +------------------+
#		 | codeword	    |
#		 +------------------+		    DOUBLE:
#		 | addr of DOUBLE  ---------------> +------------------+
#top of return	 +------------------+	    %eax -> | addr of DOCOL    |
#stack points -> | addr of DOUBLE   |	    + 4 =   +------------------+
#		 +------------------+	    %esi -> | addr of DUP      |
#		 | addr of EXIT     |		    +------------------+
#		 +------------------+		    | etc.	       |

#	Then we do NEXT, and because of the magic of threaded code that increments %esi again
#	and calls DUP.

#	Well, it seems to work.

#	One minor point here.  Because DOCOL is the first bit of assembly actually to be defined
#	in this file (the others were just macros), and because I usually compile this code with the
#	text segment starting at address 0, DOCOL has address 0.  So if you are disassembling the
#	code and see a word with a codeword of 0, you will immediately know that the word is
#	written in FORTH (it's not an assembler primitive) and so uses DOCOL as the interpreter.
#EON#	not true DOCOL has address 0 ! (it resides in ROM)

#	STARTING UP ----------------------------------------------------------------------

#	Now let's get down to nuts and bolts.  When we start the program we need to set up
#	a few things like the return stack.  But as soon as we can, we want to jump into FORTH
#	code (albeit much of the "early" FORTH code will still need to be written as
#	assembly language primitives).

#	This is what the set up code does.  Does a tiny bit of house-keeping, sets up the
#	separate return stack (NB: Linux gives us the ordinary parameter stack already), then
#	immediately jumps to a FORTH word called QUIT.	Despite its name, QUIT doesn't quit
#	anything.  It resets some internal state and starts reading and interpreting commands.
#	(The reason it is called QUIT is because you can call QUIT from your own FORTH code
#	to "quit" your program and go back to interpreting).

JFORTH		LI	R0, 0		; setup forth vars
		ST4	[R0 + FORTH_STATE / 4], R0
		LI	R1, 10
		ST4	[R0 + FORTH_BASE / 4], R1
		LI	R1, F_INTERPRET
		ST4	[R0 + FORTH_LATEST / 4], R1
		LI	R13, RSTACK
		ST4	[R0 + FORTH_HERE / 4], R13
		LI	R0, FORTH_HELLO ; say hello
		JAL	CONSTR
		LI	R12, W_QUIT	; jump interpreter
		BRA	NEXT

C_BYE		LI	R0, SYS_STACK
		MV	SP, R0
		JMP	MAINMENU


#	BUILT-IN WORDS ----------------------------------------------------------------------

#	Remember our dictionary entries (headers)?  Let's bring those together with the codeword
#	and data words to see how : DOUBLE DUP + ; really looks in memory.

#	  pointer to previous word
#	   ^
#	   |
#	+--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    | + 	 | EXIT       |
#	+---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
#	   ^	   len			       pad  codeword	  |
#	   |							  V
#	  LINK in next word				points to codeword of DUP

#	Initially we can't just write ": DOUBLE DUP + ;" (ie. that literal string) here because we
#	don't yet have anything to read the string, break it up at spaces, parse each word, etc. etc.
#	So instead we will have to define built-in words using the GNU assembler data constructors
#	(like .int, .byte, .string, .ascii and so on -- look them up in the gas info page if you are
#	unsure of them).

#	The long way would be:

#	.int <link to previous word>
#	.byte 6 		// len
#	.ascii "DOUBLE" 	// string
#	.byte 0 		// padding
#DOUBLE:.int DOCOL		// codeword
#	.int DUP		// pointer to codeword of DUP
#	.int PLUS		// pointer to codeword of +
#	.int EXIT		// pointer to codeword of EXIT

#	That's going to get quite tedious rather quickly, so here I define an assembler macro
#	so that I can just write:

#	defword "DOUBLE",6,,DOUBLE
#	.int DUP,PLUS,EXIT

#	and I'll get exactly the same effect.

#	Don't worry too much about the exact implementation details of this macro - it's complicated!

# Flags - these are discussed later.
#	.set F_IMMED,0x80
#	.set F_HIDDEN,0x20
#	.set F_LENMASK,0x1f	// length mask
F_IMMED 	.EQU	$80
F_HIDDEN	.EQU	$20
F_LENMASK	.EQU	$1F

#	// Store the chain of links.
#	.set link,0

#	.macro defword name, namelen, flags=0, label
#	.section .rodata
#	.align 4
#	.globl name_\label
#name_\label :
#	.int link		// link
#	.set link,name_\label
#	.byte \flags+\namelen	// flags + length byte
#	.ascii "\name"		// the name
#	.align 4		// padding to next 4 byte boundary
#	.globl \label
#\label :
#	.int DOCOL		// codeword - the interpreter
#	// list of word pointers follow
#	.endm

#	Similarly I want a way to write words written in assembly language.  There will quite a few
#	of these to start with because, well, everything has to start in assembly before there's
#	enough "infrastructure" to be able to start writing FORTH words, but also I want to define
#	some common FORTH words in assembly language for speed, even though I could write them in FORTH.

#	This is what DUP looks like in memory:

#	  pointer to previous word
#	   ^
#	   |
#	+--|------+---+---+---+---+------------+
#	| LINK	  | 3 | D | U | P | code_DUP ---------------------> points to the assembly
#	+---------+---+---+---+---+------------+		    code used to write DUP,
#	   ^	   len		    codeword			    which ends with NEXT.
#	   |
#	  LINK in next word

#	Again, for brevity in writing the header I'm going to write an assembler macro called defcode.
#	As with defword above, don't worry about the complicated details of the macro.

#	.macro defcode name, namelen, flags=0, label
#	.section .rodata
#	.align 4
#	.globl name_\label
#name_\label :
#	.int link		// link
#	.set link,name_\label
#	.byte \flags+\namelen	// flags + length byte
#	.ascii "\name"		// the name
#	.align 4		// padding to next 4 byte boundary
#	.globl \label
#\label :
#	.int code_\label	// codeword
#	.text
#	//.align 4
#	.globl code_\label
#code_\label :			 // assembler code follows
#	.endm

# next code
NEXT		LD4	R0, [R12]	; R12 = ESI
		ADD	R12, 4		; INC4 ESI
		LD4	R1, [R0]	; GET CODEWORD
		JMP	R1		; JUMP CODEWORD

# drop top of stack
C_DROP		ENTER	1		; drop top of stack
		BRA	NEXT

# read disk block, put addr to top
C_BLOCK 	LD4	R0, [SP]
		LI	R1, DISK_CMD_READ
		JAL	DISK_IO
		ST4	[SP], R0
		BRA	NEXT

# write disk block, put addr to top
C_WRITE 	LD4	R0, [SP]
		LI	R1, DISK_CMD_WRITE
		JAL	DISK_IO
		ST4	[SP], R0
		BRA	NEXT

# swap top two elements on stack
C_SWAP		LD4	R0, [SP]
		LD4	R1, [SP + 2]
		ST4	[SP + 2], R0
		ST4	[SP], R1
		BRA	NEXT

# duplicate top of stack
C_DUP		LD4	R0, [SP]
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

# get the second element of stack and push it on top
C_OVER		LD4	R0, [SP + 2]
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

# rotate elements on stack
C_ROT		LD4	R0, [SP]
		LD4	R1, [SP + 2]
		LD4	R2, [SP + 4]
		ST4	[SP + 4], R1
		ST4	[SP + 2], R0
		ST4	[SP], R2
		BRA	NEXT

C_NROT		LD4	R0, [SP]
		LD4	R1, [SP + 2]
		LD4	R2, [SP + 4]
		ST4	[SP + 4], R0
		ST4	[SP + 2], R2
		ST4	[SP], R1
		BRA	NEXT

#	 defcode "2DROP",5,,TWODROP // drop top two elements of stack
#	 pop %eax
#	 pop %eax
#	 NEXT

#	 defcode "2DUP",4,,TWODUP // duplicate top two elements of stack
#	 mov (%esp),%eax
#	 mov 4(%esp),%ebx
#	 push %ebx
#	 push %eax
#	 NEXT

#	 defcode "2SWAP",5,,TWOSWAP // swap top two pairs of elements of stack
#	 pop %eax
#	 pop %ebx
#	 pop %ecx
#	 pop %edx
#	 push %ebx
#	 push %eax
#	 push %edx
#	 push %ecx
#	 NEXT

# duplicate top of stack if non-zero
C_QDUP		LD4	R0, [SP]
		BZ	R0, .DONE
		ENTER	-1
		ST4	[SP], R0
.DONE		BRA	NEXT

# aritmethic
C_INCR		LD4	R0, [SP]
		ADD	R0, 1
		ST4	[SP], R0
		BRA	NEXT

C_DECR		LD4	R0, [SP]
		SUB	R0, 1
		ST4	[SP], R0
		BRA	NEXT

C_INCR4 	LD4	R0, [SP]
		ADD	R0, 4
		ST4	[SP], R0
		BRA	NEXT

C_DECR4 	LD4	R0, [SP]
		SUB	R0, 4
		ST4	[SP], R0
		BRA	NEXT

C_ADD		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		ADD	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_SUB		LD4	R1, [SP]
		ENTER	1
		LD4	R0, [SP]
		SUB	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_CRC32 	LD4	R1, [SP]
		ENTER	1
		LD4	R0, [SP]
		JAL	CRC32
		ST4	[SP], R0
		BRA	NEXT

C_MUL		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		MUL	R0, R1
		ST4	[SP], R0
		BRA	NEXT

#	 defcode "*",1,,MUL
#	 pop %eax
#	 pop %ebx
#	 imull %ebx,%eax
#	 push %eax		 // ignore overflow
#	 NEXT

#	In this FORTH, only /MOD is primitive.	Later we will define the / and MOD words in
#	terms of the primitive /MOD.  The design of the i386 assembly instruction idiv which
#	leaves both quotient and remainder makes this the obvious choice.

C_DIVMOD	LD4	R1, [SP]
		LD4	R0, [SP + 2]
		DIV	R0, R1
		GET	R1, REG_MOD
		ST4	[SP + 2], R1	; remainder
		ST4	[SP], R0	; quotient
		BRA	NEXT

#	Lots of comparison operations like =, <, >, etc..
#	ANS FORTH says that the comparison words should return all (binary) 1's for
#	TRUE and all 0's for FALSE.  However this is a bit of a strange convention
#	so this FORTH breaks it and returns the more normal (for C programmers ...)
#	1 meaning TRUE and 0 meaning FALSE.

C_EQU		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		SUB	R1, R0
		CSETZ	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_NEQU		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		SUB	R1, R0
		CSETNZ	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_LT		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		SUB	R1, R0
		CSETN	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_GT		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		SUB	R1, R0
		CSETP	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_LE		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		SUB	R1, R0
		CSETNP	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_GE		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		SUB	R1, R0
		CSETNN	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_ZEQU		LD4	R0, [SP]
		CSETZ	R0, R0
		ST4	[SP], R0
		BRA	NEXT

C_ZNEQU 	LD4	R0, [SP]
		CSETNZ	R0, R0
		ST4	[SP], R0
		BRA	NEXT

C_ZLT		LD4	R0, [SP]
		CSETN	R0, R0
		ST4	[SP], R0
		BRA	NEXT

C_ZGT		LD4	R0, [SP]
		CSETP	R0, R0
		ST4	[SP], R0
		BRA	NEXT

C_ZLE		LD4	R0, [SP]
		CSETNP	R0, R0
		ST4	[SP], R0
		BRA	NEXT

C_ZGE		LD4	R0, [SP]
		CSETNN	R0, R0
		ST4	[SP], R0
		BRA	NEXT

C_AND		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		AND	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_OR		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		OR	R0, R1
		ST4	[SP], R0
		BRA	NEXT

C_XOR		LD4	R0, [SP]
		ENTER	1
		LD4	R1, [SP]
		XOR	R0, R1
		ST4	[SP], R0
		BRA	NEXT

#	 defcode "INVERT",6,,INVERT // this is the FORTH bitwise "NOT" function (cf. NEGATE and NOT)
#	 notl (%esp)
#	 NEXT

#	RETURNING FROM FORTH WORDS ----------------------------------------------------------------------

#	Time to talk about what happens when we EXIT a function.  In this diagram QUADRUPLE has called
#	DOUBLE, and DOUBLE is about to exit (look at where %esi is pointing):

#		QUADRUPLE
#		+------------------+
#		| codeword	   |
#		+------------------+		   DOUBLE
#		| addr of DOUBLE  ---------------> +------------------+
#		+------------------+		   | codeword	      |
#		| addr of DOUBLE   |		   +------------------+
#		+------------------+		   | addr of DUP      |
#		| addr of EXIT	   |		   +------------------+
#		+------------------+		   | addr of +	      |
#						   +------------------+
#					   %esi -> | addr of EXIT     |
#						   +------------------+

#	What happens when the + function does NEXT?  Well, the following code is executed.
C_EXIT		LD4	R12, [R13]	; R12 = ESI, R13 = RSP
		ADD	R13, 4
		BRA	NEXT

#	EXIT gets the old %esi which we saved from before on the return stack, and puts it in %esi.
#	So after this (but just before NEXT) we get:

#		QUADRUPLE
#		+------------------+
#		| codeword	   |
#		+------------------+		   DOUBLE
#		| addr of DOUBLE  ---------------> +------------------+
#		+------------------+		   | codeword	      |
#	%esi -> | addr of DOUBLE   |		   +------------------+
#		+------------------+		   | addr of DUP      |
#		| addr of EXIT	   |		   +------------------+
#		+------------------+		   | addr of +	      |
#						   +------------------+
#						   | addr of EXIT     |
#						   +------------------+

#	And NEXT just completes the job by, well, in this case just by calling DOUBLE again :-)

#	LITERALS ----------------------------------------------------------------------

#	The final point I "glossed over" before was how to deal with functions that do anything
#	apart from calling other functions.  For example, suppose that DOUBLE was defined like this:

#	: DOUBLE 2 * ;

#	It does the same thing, but how do we compile it since it contains the literal 2?  One way
#	would be to have a function called "2" (which you'd have to write in assembler), but you'd need
#	a function for every single literal that you wanted to use.

#	FORTH solves this by compiling the function using a special word called LIT:

#	+---------------------------+-------+-------+-------+-------+-------+
#	| (usual header of DOUBLE)  | DOCOL | LIT   | 2     | *     | EXIT  |
#	+---------------------------+-------+-------+-------+-------+-------+

#	LIT is executed in the normal way, but what it does next is definitely not normal.  It
#	looks at %esi (which now points to the number 2), grabs it, pushes it on the stack, then
#	manipulates %esi in order to skip the number as if it had never been there.

#	What's neat is that the whole grab/manipulate can be done using a single byte single
#	i386 instruction, our old friend LODSL.  Rather than me drawing more ASCII-art diagrams,
#	see if you can find out how LIT works:
C_LIT		ENTER	-1
		LD4I	R0, [R12]
		ADD	R12, 4
		ST4	[SP], R0
		BRA	NEXT

#	MEMORY ----------------------------------------------------------------------

#	As important point about FORTH is that it gives you direct access to the lowest levels
#	of the machine.  Manipulating memory directly is done frequently in FORTH, and these are
#	the primitive words for doing it.
C_STORE 	LD4I	R0, [SP]
		LD4	R1, [SP + 2]
		ENTER	2
		ST4	[R0], R1
		BRA	NEXT

C_FETCH 	LD4I	R0, [SP]
		LD4I	R1, [R0]
		ST4	[SP], R1
		BRA	NEXT

C_ADDSTORE	LD4I	R0, [SP]
		LD4	R1, [SP + 2]
		ENTER	2
		LD4	R2, [R0]
		ADD	R2, R1
		ST4	[R0], R2
		BRA	NEXT

C_SUBSTORE	LD4I	R0, [SP]
		LD4	R1, [SP + 2]
		ENTER	2
		LD4	R2, [R0]
		SUB	R2, R1
		ST4	[R0], R2
		BRA	NEXT

#	! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes
#	so we also define standard words C@ and C!.
#	Byte-oriented operations only work on architectures which permit them (i386 is one of those).
C_STOREBYTE	LD4I	R0, [SP]
		LD1	R1, [SP + 2]
		ENTER	2
		ST1	[R0], R1
		BRA	NEXT

C_FETCHBYTE	LD4I	R0, [SP]
		LD1	R1, [R0]
		ST4	[SP], R1
		BRA	NEXT

# C@C! is a useful byte copy primitive
#	 defcode "C@C!",4,,CCOPY
#	 movl 4(%esp),%ebx	 // source address
#	 movb (%ebx),%al	 // get source character
#	 pop %edi		 // destination address
#	 stosb			 // copy to destination
#	 push %edi		 // increment destination address
#	 incl 4(%esp)		 // increment source address
#	 NEXT

# and CMOVE is a block copy operation.
#	 defcode "CMOVE",5,,CMOVE
#	 mov %esi,%edx		 // preserve %esi
#	 pop %ecx		 // length
#	 pop %edi		 // destination address
#	 pop %esi		 // source address
#	 rep movsb		 // copy source to destination
#	 mov %edx,%esi		 // restore %esi
#	 NEXT

#	BUILT-IN VARIABLES ----------------------------------------------------------------------

#	These are some built-in variables and related standard FORTH words.  Of these, the only one that we
#	have discussed so far was LATEST, which points to the last (most recently defined) word in the
#	FORTH dictionary.  LATEST is also a FORTH word which pushes the address of LATEST (the variable)
#	on to the stack, so you can read or write it using @ and ! operators.  For example, to print
#	the current value of LATEST (and this can apply to any FORTH variable) you would do:

#	LATEST @ . CR

#	To make defining variables shorter, I'm using a macro called defvar, similar to defword and
#	defcode above.	(In fact the defvar macro uses defcode to do the dictionary header).

#	.macro defvar name, namelen, flags=0, label, initial=0
#	defcode \name,\namelen,\flags,\label
#	push $var_\name
#	NEXT
#	.data
#	.align 4
#v ar_\name :
#	.int \initial
#	.endm

#	The built-in variables are:
#	STATE		Is the interpreter executing code (0) or compiling a word (non-zero)?
#	LATEST		Points to the latest (most recently defined) word in the dictionary.
#	HERE		Points to the next free byte of memory.  When compiling, compiled words go here.
#	S0		Stores the address of the top of the parameter stack.
#	BASE		The current base for printing and reading numbers.

C_STATE 	LI	R0, FORTH_STATE
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_HERE		LI	R0, FORTH_HERE
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_LATEST	LI	R0, FORTH_LATEST
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_S0		LI	R0, SYS_STACK
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_BASE		LI	R0, FORTH_BASE
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

#	BUILT-IN CONSTANTS ----------------------------------------------------------------------

#	It's also useful to expose a few constants to FORTH.  When the word is executed it pushes a
#	constant value on the stack.

#	The built-in constants are:

#	VERSION 	Is the current version of this FORTH.
#	R0		The address of the top of the return stack.
#	DOCOL		Pointer to DOCOL.
#	F_IMMED 	The IMMEDIATE flag's actual value.
#	F_HIDDEN	The HIDDEN flag's actual value.
#	F_LENMASK	The length mask in the flags/len byte.

#	SYS_*		and the numeric codes of various Linux syscalls (from <asm/unistd.h>)

##include <asm-i386/unistd.h>  // you might need this instead
##include <asm/unistd.h>

#	 .macro defconst name, namelen, flags=0, label, value
#	 defcode \name,\namelen,\flags,\label
#	 push $\value
#	 NEXT
#	 .endm

C_VERSION	LI	R0, JONES_VERSION
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_R0		LI	R0, RSTACK
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_DOCOL 	LI	R0, DOCOL
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_F_IMMED	LI	R0, F_IMMED
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_F_HIDDEN	LI	R0, F_HIDDEN
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_F_LENMASK	LI	R0, F_LENMASK
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

#	RETURN STACK ----------------------------------------------------------------------
#	These words allow you to access the return stack.  Recall that the register %ebp always points to
#	the top of the return stack.
C_TOR		LD4	R0, [SP]
		ENTER	1
		SUB	R13, 4
		ST4	[R13], R0
		BRA	NEXT

C_FROMR 	LD4	R0, [R13]
		ENTER	-1
		ADD	R13, 4
		ST4	[SP], R0
		BRA	NEXT

C_RSPFETCH	ENTER	-1
		ST4	[SP], R13
		BRA	NEXT

C_RSPSTORE	LD4	R13, [SP]
		ENTER	1
		BRA	NEXT

C_RDROP 	ADD	R13, 4
		BRA	NEXT


#	PARAMETER (DATA) STACK ----------------------------------------------------------------------
#	These functions allow you to manipulate the parameter stack.  Recall that Linux sets up the parameter
#	stack for us, and it is accessed through %esp.

C_DSPFETCH	MV	R0, SP
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

C_DSPSTORE	LD4	SP, [SP]
		BRA	NEXT

#	INPUT AND OUTPUT ----------------------------------------------------------------------

#	These are our first really meaty/complicated FORTH primitives.	I have chosen to write them in
#	assembler, but surprisingly in "real" FORTH implementations these are often written in terms
#	of more fundamental FORTH primitives.  I chose to avoid that because I think that just obscures
#	the implementation.  After all, you may not understand assembler but you can just think of it
#	as an opaque block of code that does what it says.

#	Let's discuss input first.

#	The FORTH word KEY reads the next byte from stdin (and pushes it on the parameter stack).
#	So if KEY is called and someone hits the space key, then the number 32 (ASCII code of space)
#	is pushed on the stack.

#	In FORTH there is no distinction between reading code and reading input.  We might be reading
#	and compiling code, we might be reading words to execute, we might be asking for the user
#	to type their name -- ultimately it all comes in through KEY.

#	The implementation of KEY uses an input buffer of a certain size (defined at the end of this
#	file).	It calls the Linux read(2) system call to fill this buffer and tracks its position
#	in the buffer using a couple of variables, and if it runs out of input buffer then it refills
#	it automatically.  The other thing that KEY does is if it detects that stdin has closed, it
#	exits the program, which is why when you hit ^D the FORTH system cleanly exits.

#    buffer			      bufftop
#	|				 |
#	V				 V
#	+-------------------------------+--------------------------------------+
#	| INPUT READ FROM STDIN ....... | unused part of the buffer	       |
#	+-------------------------------+--------------------------------------+
#			  ^
#			  |
#		       currkey (next character to read)

#	<---------------------- BUFFER_SIZE (4096 bytes) ---------------------->

C_KEY		JAL	CONREADECHO
		ENTER	-1
		ST4	[SP], R0
		BRA	NEXT

#	By contrast, output is much simpler.  The FORTH word EMIT writes out a single byte to stdout.
#	This implementation just uses the write system call.  No attempt is made to buffer output, but
#	it would be a good exercise to add it.

C_EMIT		LD4	R0, [SP]
		ENTER	1
		JAL	CONOUT
		BRA	NEXT

#	Back to input, WORD is a FORTH word which reads the next full word of input.

#	What it does in detail is that it first skips any blanks (spaces, tabs, newlines and so on).
#	Then it calls KEY to read characters into an internal buffer until it hits a blank.  Then it
#	calculates the length of the word it read and returns the address and the length as
#	two words on the stack (with the length at the top of stack).

#	Notice that WORD has a single internal buffer which it overwrites each time (rather like
#	a static C string).  Also notice that WORD's internal buffer is just 32 bytes long and
#	there is NO checking for overflow.  31 bytes happens to be the maximum length of a
#	FORTH word that we support, and that is what WORD is used for: to read FORTH words when
#	we are compiling and executing code.  The returned strings are not NUL-terminated.

#	Start address+length is the normal way to represent strings in FORTH (not ending in an
#	ASCII NUL character as in C), and so FORTH strings can contain any character including NULs
#	and can be any length.

#	WORD is not suitable for just reading strings (eg. user input) because of all the above
#	peculiarities and limitations.

#	Note that when executing, you'll see:
#	WORD FOO
#	which puts "FOO" and length 3 on the stack, but when compiling:
#	: BAR WORD FOO ;
#	is an error (or at least it doesn't do what you might expect).	Later we'll talk about compiling
#	and immediate mode, and you'll understand why.
C_WORD		JAL	WORD
		ENTER	-2
		ST4	[SP + 2], R0	; base address
		ST4	[SP], R1	; length
		BRA	NEXT

WORD		ENTER	-1
		ST4	[SP], R14
		ST4	[SP + 1], R10

.READ		; search for first non-blank character.  Also skip \ comments
		JAL	CONREADECHO	; get next key, returned in R0
		SUB	R1, R0, '\'
		BZ	R1, .COMMENT
		LI	R1, ' '
		BLE	R0, R1, .READ

		; search for the end of the word, storing chars as we go
		LI	R10, WORD_BUFFER
.STORE		ST1	[R10], R0
		ADD	R10, 1
		JAL	CONREADECHO
		LI	R1, ' '
		BLT	R1, R0, .STORE

		; return result
		LI	R0, WORD_BUFFER
		SUB	R1, R10, R0
		LD4	R10, [SP + 1]
		LD4	R14, [SP]
		ENTER	1
		RET

.COMMENT	; skip comments to end of the current line
		JAL	CONREADECHO
		LI	R1, CR
		BNE	R0, R1, .COMMENT
		BRA	.READ

#	As well as reading in words we'll need to read in numbers and for that we are using a function
#	called NUMBER.	This parses a numeric string such as one returned by WORD and pushes the
#	number on the parameter stack.

#	The function uses the variable BASE as the base (radix) for conversion, so for example if
#	BASE is 2 then we expect a binary number.  Normally BASE is 10.

#	If the word starts with a '-' character then the returned value is negative.

#	If the string can't be parsed as a number (or contains characters outside the current BASE)
#	then we need to return an error indication.  So NUMBER actually returns two items on the stack.
#	At the top of stack we return the number of unconverted characters (ie. if 0 then all characters
#	were converted, so there is no error).	Second from top of stack is the parsed number or a
#	partial value if there was an error.

C_NUMBER	LD4	R0,[SP + 2]	; begin
		LD4	R1,[SP] 	; len
		JAL	NUMBER
		ST4	[SP + 2], R0	; parsed number
		ST4	[SP], R1	; number of unparsed characters (0 = no error)
		BRA	NEXT

NUMBER: 	LI	R3, 0
		BZ	R1, .DONE

		LI	R5, FORTH_BASE
		LD4	R5, [R5]

		LD1	R4, [R0]	; fetch first char
		SUB	R6, R4, '$'	; hex prefix ?
		BNZ	R6, .NOHEX
		LI	R5, 16
		LI	R6, 1
		BRA	.CONSUME
.NOHEX		SUB	R6, R4, '-'
		BNZ	R6, .CONTINUE	; non negative
.CONSUME	ADD	R0, 1
		SUB	R1, 1
		BNZ	R1, .CONTINUE
		LI	R1, 1		; only -
		BRA	.DONE

.CONTINUE	; loop reading digits
		MUL	R3, R5		; R3 *= BASE
		LD1	R4, [R0]
		ADD	R0, 1

		; have a digit ?
		SUB	R4, '0'
		BLTI	R4, SP, .DONEG
		LI	R2, 10
		BLT	R4, R2, .CHECK
		SUB	R4, 17	; 17 == 'A' - '0'
		BLTI	R4, SP, .DONEG
		ADD	R4, 10

.CHECK		; digit fits ?
		BLE	R5, R4, .DONEG

		; add digit and loop
		ADD	R3, R4
		SUB	R1, 1
		BNZ	R1, .CONTINUE

.DONEG		BNZ	R6, .DONE
		SUB	R3, SP, R3	; negate if '-'

.DONE		MV	R0, R3
		RET

#	DICTIONARY LOOK UPS ----------------------------------------------------------------------
#	We're building up to our prelude on how FORTH code is compiled, but first we need yet more infrastructure.
#	The FORTH word FIND takes a string (a word as parsed by WORD -- see above) and looks it up in the
#	dictionary.  What it actually returns is the address of the dictionary header, if it finds it,
#	or 0 if it didn't.
#	So if DOUBLE is defined in the dictionary, then WORD DOUBLE FIND returns the following pointer:
#   pointer to this
#	|
#	|
#	V
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    | + 	 | EXIT       |
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+

#	See also >CFA and >DFA.
#	FIND doesn't find dictionary entries which are flagged as HIDDEN.  See below for why.

C_FIND		LD4	R0,[SP + 2]	; begin
		LD4	R1,[SP] 	; len
		ENTER	1
		JAL	FIND
		ST4	[SP], R0
		BRA	NEXT

FIND		LI	R2, FORTH_LATEST
		LD4	R2, [R2]
.LOOP		BZ	R2, .DONE
		LD1	R3, [R2 + 4]	; load len + flags
		AND	R3, F_LENMASK | F_HIDDEN
		BNE	R1, R3, .NEXT

		# compare strings in detail
		ADD	R4, R2, 5
		MV	R5, R0
.CMP		LD1	R6, [R4]
		LD1	R7, [R5]
		BNE	R6, R7, .NEXT
		ADD	R4, 1
		ADD	R5, 1
		SUB	R3, 1
		BNZ	R3, .CMP

		# found
		MV	R0, R2
		RET

.NEXT		LD4	R2, [R2]	; load previous
		BRA	.LOOP

.DONE		LI	R0, 0		; not found
		RET

#	FIND returns the dictionary pointer, but when compiling we need the codeword pointer (recall
#	that FORTH definitions are compiled into lists of codeword pointers).  The standard FORTH
#	word >CFA turns a dictionary pointer into a codeword pointer.
#	The example below shows the result of:
#		WORD DOUBLE FIND >CFA

#	FIND returns a pointer to this
#	|				>CFA converts it to a pointer to this
#	|					   |
#	V					   V
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    | + 	 | EXIT       |
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#						   codeword

#	Notes:

#	Because names vary in length, this isn't just a simple increment.

#	In this FORTH you cannot easily turn a codeword pointer back into a dictionary entry pointer, but
#	that is not true in most FORTH implementations where they store a back pointer in the definition
#	(with an obvious memory/complexity cost).  The reason they do this is that it is useful to be
#	able to go backwards (codeword -> dictionary entry) in order to decompile FORTH definitions
#	quickly.

#	What does CFA stand for?  My best guess is "Code Field Address".

C_TCFA		LD4	R0, [SP]
		JAL	TCFA
		ST4	[SP], R0
		BRA	NEXT

TCFA		ADD	R0, 4		; skip link pointer
		LD1	R1, [R0]	; load flags + len
		AND	R1, F_LENMASK	; drop flags
		ADD	R1, 4		; align to 4
		AND	R1, $fc
		ADD	R0, R1
		RET

#	Related to >CFA is >DFA which takes a dictionary entry address as returned by FIND and
#	returns a pointer to the first data field.
#	FIND returns a pointer to this
#	|				>CFA converts it to a pointer to this
#	|					   |
#	|					   |	>DFA converts it to a pointer to this
#	|					   |		 |
#	V					   V		 V
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    | + 	 | EXIT       |
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#						   codeword
#	(Note to those following the source of FIG-FORTH / ciforth: My >DFA definition is
#	different from theirs, because they have an extra indirection).
#	You can see that >DFA is easily defined in FORTH just by adding 4 to the result of >CFA.

#	defword ">DFA",4,,TDFA
#	.int TCFA		// >CFA 	(get code field address)
#	.int INCR4		// 4+		(add 4 to it to get to next word)
#	.int EXIT		// EXIT 	(return from FORTH word)

#	COMPILING ----------------------------------------------------------------------
#	Now we'll talk about how FORTH compiles words.	Recall that a word definition looks like this:
#		: DOUBLE DUP + ;
#	and we have to turn this into:
#	  pointer to previous word
#	   ^
#	   |
#	+--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    | + 	 | EXIT       |
#	+---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
#	   ^	   len			       pad  codeword	  |
#	   |							  V
#	  LATEST points here				points to codeword of DUP

#	There are several problems to solve.  Where to put the new word?  How do we read words?  How
#	do we define the words : (COLON) and ; (SEMICOLON)?

#	FORTH solves this rather elegantly and as you might expect in a very low-level way which
#	allows you to change how the compiler works on your own code.

#	FORTH has an INTERPRET function (a true interpreter this time, not DOCOL) which runs in a
#	loop, reading words (using WORD), looking them up (using FIND), turning them into codeword
#	pointers (using >CFA) and deciding what to do with them.

#	What it does depends on the mode of the interpreter (in variable STATE).

#	When STATE is zero, the interpreter just runs each word as it looks them up.  This is known as
#	immediate mode.

#	The interesting stuff happens when STATE is non-zero -- compiling mode.  In this mode the
#	interpreter appends the codeword pointer to user memory (the HERE variable points to the next
#	free byte of user memory -- see DATA SEGMENT section below).

#	So you may be able to see how we could define : (COLON).  The general plan is:

#	(1) Use WORD to read the name of the function being defined.
#	(2) Construct the dictionary entry -- just the header part -- in user memory:

#   pointer to previous word (from LATEST)			+-- Afterwards, HERE points here, where
#	   ^							|   the interpreter will start appending
#	   |							V   codewords.
#	+--|------+---+---+---+---+---+---+---+---+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      |
#	+---------+---+---+---+---+---+---+---+---+------------+
#		   len			       pad  codeword

#	(3) Set LATEST to point to the newly defined word, ...
#	(4) .. and most importantly leave HERE pointing just after the new codeword.  This is where
#	    the interpreter will append codewords.
#	(5) Set STATE to 1.  This goes into compile mode so the interpreter starts appending codewords to
#	    our partially-formed header.

#	After : has run, our input is here:

#	: DOUBLE DUP + ;
#		 ^
#		 |
#		Next byte returned by KEY will be the 'D' character of DUP

#	so the interpreter (now it's in compile mode, so I guess it's really the compiler) reads "DUP",
#	looks it up in the dictionary, gets its codeword pointer, and appends it:

#									     +-- HERE updated to point here.
#									     |
#									     V
#	+---------+---+---+---+---+---+---+---+---+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    |
#	+---------+---+---+---+---+---+---+---+---+------------+------------+
#		   len			       pad  codeword

#	Next we read +, get the codeword pointer, and append it:

#											  +-- HERE updated to point here.
#											  |
#											  V
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    | + 	 |
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+
#		   len			       pad  codeword

#	The issue is what happens next.  Obviously what we _don't_ want to happen is that we
#	read ";" and compile it and go on compiling everything afterwards.

#	At this point, FORTH uses a trick.  Remember the length byte in the dictionary definition
#	isn't just a plain length byte, but can also contain flags.  One flag is called the
#	IMMEDIATE flag (F_IMMED in this code).	If a word in the dictionary is flagged as
#	IMMEDIATE then the interpreter runs it immediately _even if it's in compile mode_.

#	This is how the word ; (SEMICOLON) works -- as a word flagged in the dictionary as IMMEDIATE.

#	And all it does is append the codeword for EXIT on to the current definition and switch
#	back to immediate mode (set STATE back to 0).  Shortly we'll see the actual definition
#	of ; and we'll see that it's really a very simple definition, declared IMMEDIATE.

#	After the interpreter reads ; and executes it 'immediately', we get this:

#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP	    | + 	 | EXIT       |
#	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
#		   len			       pad  codeword					       ^
#												       |
#												      HERE
#	STATE is set to 0.

#	And that's it, job done, our new definition is compiled, and we're back in immediate mode
#	just reading and executing words, perhaps including a call to test our new word DOUBLE.

#	The only last wrinkle in this is that while our word was being compiled, it was in a
#	half-finished state.  We certainly wouldn't want DOUBLE to be called somehow during
#	this time.  There are several ways to stop this from happening, but in FORTH what we
#	do is flag the word with the HIDDEN flag (F_HIDDEN in this code) just while it is
#	being compiled.  This prevents FIND from finding it, and thus in theory stops any
#	chance of it being called.

#	The above explains how compiling, : (COLON) and ; (SEMICOLON) works and in a moment I'm
#	going to define them.  The : (COLON) function can be made a little bit more general by writing
#	it in two parts.  The first part, called CREATE, makes just the header:

#						   +-- Afterwards, HERE points here.
#						   |
#						   V
#	+---------+---+---+---+---+---+---+---+---+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 |
#	+---------+---+---+---+---+---+---+---+---+
#		   len			       pad

#	and the second part, the actual definition of : (COLON), calls CREATE and appends the
#	DOCOL codeword, so leaving:

#								+-- Afterwards, HERE points here.
#								|
#								V
#	+---------+---+---+---+---+---+---+---+---+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 | DOCOL      |
#	+---------+---+---+---+---+---+---+---+---+------------+
#		   len			       pad  codeword

#	CREATE is a standard FORTH word and the advantage of this split is that we can reuse it to
#	create other types of words (not just ones which contain code, but words which contain variables,
#	constants and other data).

C_CREATE	LD4	R0, [SP + 2]
		LD4	R1, [SP]
		ENTER	2
		LI	R2, 0
		LD4	R3, [R2 + FORTH_HERE / 4]
		LD4	R4, [R2 + FORTH_LATEST / 4]
		ST4	[R3], R4	; store link pointer

		# update LATEST
		ST4	[R2 + FORTH_LATEST / 4], R3
		ADD	R3, 4

		# store length & flags
		ST1	[R3], R1

		# store name
.COPY		LD1	R4, [R0]
		ADD	R3, 1
		ADD	R0, 1
		SUB	R1, 1
		ST1	[R3], R4
		BNZ	R1, .COPY

		# align to 4
		ADD	R3, 4
		AND	R3, -4	; #~3

		# update HERE
		ST4	[R2 + FORTH_HERE / 4], R3
		BRA	NEXT

#	Because I want to define : (COLON) in FORTH, not assembler, we need a few more FORTH words
#	to use.

#	The first is , (COMMA) which is a standard FORTH word which appends a 32 bit integer to the user
#	memory pointed to by HERE, and adds 4 to HERE.	So the action of , (COMMA) is:

#							previous value of HERE
#								 |
#								 V
#	+---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
#	| LINK	  | 6 | D | O | U | B | L | E | 0 |		|  <data>    |
#	+---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
#		   len			       pad			      ^
#									      |
#									new value of HERE

#	and <data> is whatever 32 bit integer was at the top of the stack.

#	, (COMMA) is quite a fundamental operation when compiling.  It is used to append codewords
#	to the current word that is being compiled.

C_COMMA 	LD4	R0, [SP]
		ENTER	1
		JAL	COMMA
		BRA	NEXT

COMMA:		LI	R1, 0
		LD4	R2, [R1 + FORTH_HERE / 4]
		ST4	[R2], R0
		ADD	R2, 4
		ST4	[R1 + FORTH_HERE / 4], R2
		RET

#	Our definitions of : (COLON) and ; (SEMICOLON) will need to switch to and from compile mode.
#	Immediate mode vs. compile mode is stored in the global variable STATE, and by updating this
#	variable we can switch between the two modes.

#	For various reasons which may become apparent later, FORTH defines two standard words called
#	[ and ] (LBRAC and RBRAC) which switch between modes:

#	Word	Assembler	Action		Effect
#	[	LBRAC		STATE := 0	Switch to immediate mode.
#	]	RBRAC		STATE := 1	Switch to compile mode.

#	[ (LBRAC) is an IMMEDIATE word.  The reason is as follows: If we are in compile mode and the
#	interpreter saw [ then it would compile it rather than running it.  We would never be able to
#	switch back to immediate mode!	So we flag the word as IMMEDIATE so that even in compile mode
#	the word runs immediately, switching us back to immediate mode.

C_LBRAC 	LI	R0, 0
		ST4	[R0 + FORTH_STATE / 4], R0
		BRA	NEXT

C_RBRAC 	LI	R0, 0
		LI	R1, 1
		ST4	[R0 + FORTH_STATE / 4], R1
		BRA	NEXT

#	Now we can define : (COLON) using CREATE.  It just calls CREATE, appends DOCOL (the codeword), sets
#	the word HIDDEN and goes into compile mode.

#	defword ":",1,,COLON
#	.int WORD		// Get the name of the new word
#	.int CREATE		// CREATE the dictionary entry / header
#	.int LIT, DOCOL, COMMA	// Append DOCOL  (the codeword).
#	.int LATEST, FETCH, HIDDEN // Make the word hidden (see below for definition).
#	.int RBRAC		// Go into compile mode.
#	.int EXIT		// Return from the function.

# (SEMICOLON) is also elegantly simple.  Notice the F_IMMED flag.

#	defword ";",1,F_IMMED,SEMICOLON
#	.int LIT, EXIT, COMMA	// Append EXIT (so the word will return).
#	.int LATEST, FETCH, HIDDEN // Toggle hidden flag -- unhide the word (see below for definition).
#	.int LBRAC		// Go back to IMMEDIATE mode.
#	.int EXIT		// Return from the function.

#	EXTENDING THE COMPILER ----------------------------------------------------------------------

#	Words flagged with IMMEDIATE (F_IMMED) aren't just for the FORTH compiler to use.  You can define
#	your own IMMEDIATE words too, and this is a crucial aspect when extending basic FORTH, because
#	it allows you in effect to extend the compiler itself.	Does gcc let you do that?

#	Standard FORTH words like IF, WHILE, ." and so on are all written as extensions to the basic
#	compiler, and are all IMMEDIATE words.

#	The IMMEDIATE word toggles the F_IMMED (IMMEDIATE flag) on the most recently defined word,
#	or on the current word if you call it in the middle of a definition.

#	Typical usage is:
#	: MYIMMEDWORD IMMEDIATE
#		...definition...
#	;

#	but some FORTH programmers write this instead:
#	: MYIMMEDWORD
#		...definition...
#	; IMMEDIATE

#	The two usages are equivalent, to a first approximation.

C_IMMEDIATE	LI	R0, 0
		LD4	R1, [R0 + FORTH_LATEST]
		LD1	R2, [R1 + 4]
		XOR	R2, F_IMMED
		ST1	[R1 + 4], R2
		BRA	NEXT

#	'addr HIDDEN' toggles the hidden flag (F_HIDDEN) of the word defined at addr.  To hide the
#	most recently defined word (used above in : and ; definitions) you would do:
#		LATEST @ HIDDEN

#	'HIDE word' toggles the flag on a named 'word'.

#	Setting this flag stops the word from being found by FIND, and so can be used to make 'private'
#	words.	For example, to break up a large word into smaller parts you might do:
#		: SUB1 ... subword ... ;
#		: SUB2 ... subword ... ;
#		: SUB3 ... subword ... ;
#		: MAIN ... defined in terms of SUB1, SUB2, SUB3 ... ;
#		HIDE SUB1
#		HIDE SUB2
#		HIDE SUB3

#	After this, only MAIN is 'exported' or seen by the rest of the program.

C_HIDDEN	LD4	R0, [SP]
		ENTER	1
		LD1	R1, [R0 + 4]
		XOR	R1, F_HIDDEN
		ST1	[R0 + 4], R1
		BRA	NEXT

#	defword "HIDE",4,,HIDE
#	.int WORD		// Get the word (after HIDE).
#	.int FIND		// Look up in the dictionary.
#	.int HIDDEN		// Set F_HIDDEN flag.
#	.int EXIT		// Return.

#	' (TICK) is a standard FORTH word which returns the codeword pointer of the next word.
#	The common usage is:
#	' FOO ,

#	which appends the codeword of FOO to the current word we are defining (this only works in compiled code).
#	You tend to use ' in IMMEDIATE words.  For example an alternate (and rather useless) way to define
#	a literal 2 might be:

#	: LIT2 IMMEDIATE
#		' LIT , 	\ Appends LIT to the currently-being-defined word
#		2 ,		\ Appends the number 2 to the currently-being-defined word
#	;

#	So you could do:

#	: DOUBLE LIT2 * ;

#	(If you don't understand how LIT2 works, then you should review the material about compiling words
#	and immediate mode).

#	This definition of ' uses a cheat which I copied from buzzard92.  As a result it only works in
#	compiled code.	It is possible to write a version of ' based on WORD, FIND, >CFA which works in
#	immediate mode too.
C_TICK		LD4	R0, [R12]
		ENTER	-1
		ADD	R12, 4
		ST4	[SP], R0
		BRA	NEXT

#	BRANCHING ----------------------------------------------------------------------

#	It turns out that all you need in order to define looping constructs, IF-statements, etc.
#	are two primitives.

#	BRANCH is an unconditional branch. 0BRANCH is a conditional branch (it only branches if the
#	top of stack is zero).

#	The diagram below shows how BRANCH works in some imaginary compiled word.  When BRANCH executes,
#	%esi starts by pointing to the offset field (compare to LIT above):

#	+---------------------+-------+---- - - ---+------------+------------+---- - - - ----+------------+
#	| (Dictionary header) | DOCOL | 	   | BRANCH	| offset     | (skipped)     | word	  |
#	+---------------------+-------+---- - - ---+------------+-----|------+---- - - - ----+------------+
#								   ^  | 		      ^
#								   |  | 		      |
#								   |  +-----------------------+
#								  %esi added to offset

#	The offset is added to %esi to make the new %esi, and the result is that when NEXT runs, execution
#	continues at the branch target.  Negative offsets work as expected.

#	0BRANCH is the same except the branch happens conditionally.

#	Now standard FORTH words such as IF, THEN, ELSE, WHILE, REPEAT, etc. can be implemented entirely
#	in FORTH.  They are IMMEDIATE words which append various combinations of BRANCH or 0BRANCH
#	into the word currently being compiled.

#	As an example, code written like this:

#		condition-code IF true-part THEN rest-code

#	compiles to:

#		condition-code 0BRANCH OFFSET true-part rest-code
#					  |		^
#					  |		|
#					  +-------------+
C_BRANCH	LD4	R0, [R12]
		ADD	R12, R0
		BRA	NEXT

C_ZBRANCH	LD4	R0, [SP]
		ENTER	1
		BZ	R0, C_BRANCH	; branch if zero
		ADD	R12, 4		; skip offset
		BRA	NEXT

#	LITERAL STRINGS ----------------------------------------------------------------------

#	LITSTRING is a primitive used to implement the ." and S" operators (which are written in
#	FORTH).  See the definition of those operators later.

#	TELL just prints a string.  It's more efficient to define this in assembly because we
#	can make it a single Linux syscall.

C_LITSTRING	LD4	R0, [R12]
		ENTER	-2
		ADD	R12, 4
		ST4	[SP - 2], R12	; push string start addr
		ST4	[SP], R0	; push len
		ADD	R12, R0 	; skip past the string
		ADD	R12, 3
		AND	R12, -4 	; align to 4
		BRA	NEXT

C_TELL		LD4	R3, [SP + 2]	; string addr
		LD4	R4, [SP]	; len
.LOOP		BZ	R4, NEXT
		LD1	R0, [R3]
		ADD	R3, 1
		JAL	CONOUT
		SUB	R4, 1
		BRA	.LOOP


#	CHAR puts the ASCII code of the first character of the following word on the stack.  For example
#	CHAR A puts 65 on the stack.

#	EXECUTE is used to run execution tokens.  See the discussion of execution tokens in the
#	FORTH code for more details.
C_CHAR		JAL	WORD	; R0 = word addr, R1 = len
		ENTER	-1
		LD1	R0, [R0]
		ST4	[SP], R0
		BRA	NEXT

C_EXECUTE	LD4	R0, [SP]
		ENTER	1
		LD4	R0, [R0]
		JMP	R0

#	QUIT AND INTERPRET ----------------------------------------------------------------------

#	QUIT is the first FORTH function called, almost immediately after the FORTH system "boots".
#	As explained before, QUIT doesn't "quit" anything.  It does some initialisation (in particular
#	it clears the return stack) and it calls INTERPRET in a loop to interpret commands.  The
#	reason it is called QUIT is because you can call it from your own FORTH words in order to
#	"quit" your program and start again at the user prompt.

#	INTERPRET is the FORTH interpreter ("toploop", "toplevel" or "REPL" might be a more accurate
#	description -- see: http://en.wikipedia.org/wiki/REPL).

#	defword "QUIT",4,,QUIT
#	.int RZ,RSPSTORE	// R0 RSP!, clear the return stack
#	.int INTERPRET		// interpret the next word
#	.int BRANCH,-8		// and loop (indefinitely)

#	This interpreter is pretty simple, but remember that in FORTH you can always override
#	it later with a more powerful one!
C_INTERPRET	JAL	WORD		; R0 = addr, R1 = len
		LI	R10, 0		; literal flag
		MV	R11, R0 	; save R0
		JAL	FIND
		BZ	R0, .NOTFOUND
		MV	R2, R0
		LD1	R3, [R2 + 4]	; get len + flags
		JAL	TCFA		; get codeword
		AND	R3, F_IMMED
		BNZ	R3, .DOEXEC
		BRA	.CHOICE

.NOTFOUND	; not a word, assume a literal
		LI	R10, 1
		MV	R0, R11
		JAL	NUMBER
		BNZ	R1, .ERROR
		MV	R3, R0
		LI	R0, B_LIT

.CHOICE 	; codeword in R0: compiling or executing ?
		LI	R1, FORTH_STATE
		LD4	R1, [R1]
		BZ	R1, .DOEXEC

		; compiling: just append the word
		JAL	COMMA
		BZ	R10, .NOLIT
		MV	R0, R3
		JAL	COMMA
.NOLIT		BRA	NEXT

.DOEXEC 	; executing: run it
		BNZ	R10, .ISLIT
		LD4	R1, [R0]
		JMP	R1
.ISLIT		ENTER	-1
		ST4	[SP], R3
		BRA	NEXT

.ERROR		; parser error
		LI	R0, PARSER_ERROR
		JAL	CONSTR
		BRA	NEXT
