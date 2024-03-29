------------------------------
. * ELEVEN v0.4.6 -- README *
------------------------------

(Note: this document was optimized for viewing with the EE editor. If you're
reading this using EE, you can press 'f7' for the table of contents. You may
then use the cursor keys to jump to individual sections directly)


. 1. INTRODUCTION

. 1.1. What is ELEVEN?

ELEVEN is a combined editor and preprocessor package which enhances BASIC65 
by adding the following features:

- no more line numbers
- named labels
- variable declaration
- variable names of arbitrary length
- $ and % prefixes for hexadecimal and binary integers
- easy to read comments
- easy to use 'EE' full screen text editor with preprocessor integration
- automatic backup facility  

ELEVEN consists of two main parts: The editor and the preprocessor itself. 
Both are working hand in hand in order to provide a simple and integrated
development workflow.


. 1.2. Installation / Quick start

To use ELEVEN, simply make a copy of the ELEVEN disc image and boot from it
(either by resetting your MEGA65 or by typing 'BOOT' at the READY prompt). 
Booting launches EE, ELEVEN's source code editor, along with the current
file.

You may press 'p' during the boot process to access the preferences program
(see "4. PREFERENCES" for more info).

Once in the editor, by pressing the 'f1' key you can load one of the
various examples. You can compile and run them by hitting the 'f5' key. Or,
when you've seen enough, you can press 'f2' to begin your very own adventures
in ELEVEN.


. 1.3. Examples

There are some example programs on the ELEVEN disc showing how to put the
precompiler features to good use. These are:

"hello.el"
A slightly advanced hello world

"baseDemo.el"
Simple base conversion demo

"hopalong.el"
Hopalong fractal

"ffire.el"
Forest fire simulation

"fieni"
Snake game

"dizzy"
Palette shift demo


. 2. THE EDITOR

. 2.1. Overview

ELEVEN comes with EE, an easy to use text editor with a few tricks up its
sleeve to make editing BASIC sources for ELEVEN easy and uncomplicated.


. 2.2. The status bar

EE displays a status bar with the following information:
- the file name (prefixed with '*' if the file has unsaved changes)
- the current cursor position (in columns:row format)
- the most important function keys
- the current control character mode (flashing 'ctrl' when in ctrl mode)

After file operations, the status bar displays the result of the last file
operation. It then changes to its normal appearance after the next keypress.
 

. 2.3. Editor functions

<f1>   LOAD      load a file from the default drive
<f2>   NEW       erase the current text in memory and start over

<f3>   SAVE      save the current text (with the current filename)
<f4>   SAVE AS   save the current text under a new name ('save as')

<f5>   COMPILE   compile the current file (see below)

<f7>   LABELS    display label chooser (see below)

<f9>   GOTO      jump to line number (see below)

<f11>  CMODE     toggle control character mode on/off (see below)

<f14>  SVCONF    save current configuration as default (see below)

<alt+f>     search
<alt+r>     search & replace

<alt+del>   delete current line
<home>      jump to beginning of text
<alt+home>  jump to end of text
<alt+up>    page up
<alt+down>  page down
<alt+left>  scroll line left (only with lines>80 columns)
<alt+right> scroll line right (only with lines>80 columns)
<help>      show command help

<shift+run/stop> exit editor


. 2.4. The label chooser (f7)

One of the main features of the precompiler is its usage of labels instead
of line numbers. A label is defined with a "dot" prefix (see "3.2 Labels")

To make working with labels easier, EE keeps track of the labels in your
program and lets you navigate directly to label positions. To bring up the
label chooser, press 'f7' and then choose the desired label with the 
cursor keys.

Note: You can also use this feature to mark certain points in your source
code without actually using them as labels. You may define as many labels
as you like.

(If you are reading this document with EE, you can try this feature by
pressing 'f7' now. All sections are defined as labels, so that you can
easiliy jump sections) 


. 2.5. Goto line (f9)

Press 'f9' to directly specify a line number to jump to.


. 2.6. Compile (f5)

Press 'f5' to directly invoke the precompiler from inside the editor. If the
compiler encounters an error while compiling, you are automatically returned
to the editor, and (if possible) the line where the error occured.


. 2.7. Control Character Mode (f11)

To include control characters (like CURSOR UP, HOME, etc.) in your source code,
you can enter "Control Character Mode" by pressing 'f11'. 
While in Control Chracter Mode, "ctrl" flashes at the lower right corner
of the screen, and all characer codes generated by cursor and screen editing 
keys are directly inserted into the source code rather than interpreted by EE, 
just like "quote mode" in Commodore Basic.
To exit Control Character Mode, simply press 'f11' again.


. 2.8. Saving the current configuration (f14)

When booting ELEVEN, certain options are loaded from the configuration file if
present. These options are:

- the screen colours
- whether to automatically backup files when saving
- the current file name

That way, you can automatically have your current file loaded when starting 
a new session with ELEVEN.

To store the current configuration (and the current filename along with it),
press 'f14'. 

To edit other aspects of the configuration, you may use the 'preferences'
program by pressing 'p' while booting ELEVEN.


. 2.9. Automatic backups

EE automatically creates backups of the current file each time you save
something to disc. This behaviour can be changed in the prefs application.
Please note that disabling backups is highly discouraged, because the C65's
DOS is highly unstable and you can't do too much backups!



. 3. THE PRECOMPILER

The precompiler is what is at the heart of ELEVEN. It transforms bland old
BASIC10 into a reasonably readable and comfortable programming language by
adding the following new features.


. 3.1. Comments

Comments are defined with the single quote ("'") sign. Everyting after the
single quote sign is ignored in the final program.
(you can still use the REM command for comments, but its use is discouraged
because the quote sign provides much more readable program code)

Example:

print "hello, ";userName$         ' greet user

Of course, you may continue using the "'" sign in string constants:

myString$="may I 'quote' this?"   ' yes, you may



. 3.2. Labels

Instead of line numbers, ELEVEN-enabled programs use named labels as jump 
destinations. Labels contain numbers, letters and graphics characters. When
labels are defined, a dot character immediately precedes the label:

   .initProgram
   .errorHandler
   
When they're referenced by a TRAP, GOTO or GOSUB, the dot is omitted:

trap errorHandler
gosub initProgram

Eleven's editor has a nice feature which lists all the labels in a program
when pressing the 'f7' (lbls) key.  You can then select which label to jump 
to by using the cursor keys (or escape to return from the label list). 



. 3.3. The #declare directive

Like labels, ELEVEN supports descriptive variable names. Variables start
with a letter and may contain numbers. As in BASIC10, string variables end
with $ and integer variables end with %.

With #declare, you declare a variable for usage on your program. You can only
use variables you have declared beforehand. Unlike BASIC10, variable names 
can be of arbitrary length.

Examples:
#declare floatingPointVariable
#declare stringVariable$
#declare integerVariable%

floatingPointVariable=5.3
integerVariable=42
stringVariable$="value is "+str$(floatingPointVariable)

Your program can use as many #declare directives as is neccessary to declare
all the variables used.

For array variables, you may either #declare and DIM the array in one go:

#declare anArray(100)

or declare only the array's base name and DIMension it later on:

#declare anArray
dim anArray(100)

Last but not least, it's also possible to assign an initial value to 
a variable when declaring it:

#declare eleven=11

Note: Although ELEVEN makes programming BASIC10 much more concise and 
coprehensive, nothing is changed in BASIC10 itself. For example, all 
calculations are still performed in slow floating point arithmetic, 
integers are converted internally before any calculations are done. 


. 3.4. The #output directive

The #output directive lets you define the name of the finished program.

Example:
#output "myProg.prg"



. 3.5. Base conversions

You can use the "%" and "$" prefixes to write integers in binary or 
hexadecimal notation, respectively.

Examples:

#declare myBinaryNumber = %10110
#declare anotherNumber
anotherNumber = $ba12
print "Your lucky number is ",anotherNumber+%101011


. 4. THE PREFERENCES PROGRAM

(description forthcoming)



. 5. VARIOUS TIPS & TRICKS


. 5.1. Auto indent

EE automatically indents new lines when RETURN is pressed inside an indented 
line, thus making it much easier writing pretty formatted code.


. 5.2. Long lines

Lines with more than 80 characters are displayed truncated with an "$" at the
right screen border. You can scroll those lines by pressing ALT and the
CURSOR LEFT/RIGHT keys.


. 5.3. Back to EE

After running your compiled program you can enter the editor again by simply
pressing the 'F1' key (that is, unless your program redefines the function
keys).
 
 


. 6. ELEVEN SAQ (Seldom Asked Questions)

>> "Ok, so tell us: Why did you do it?"

After holding the first prototype MEGA65 in my hands and using that wonderful
keyboard, the first thing coming to my mind was: "I want to write programs
on this thing!!"

Yep, that's right. *On* this thing. Not cross-developing on some shiny PC in
some shiny IDE but *on* the MEGA65 directly. After all, why build such a 
gorgeous keyboard if no one's programming on it?

Unfortunately, one of the most severe problems of the MEGA65 platform is its 
bundled programming environment. BASIC10 was already hopelessly antiquated in 
1991 (there's a quite extensive internal document from Commodore listing
its many shortcomings), and it certainly hasn't aged well. 
BASIC10 is an inherently slow and overcome implementation of the BASIC language,
and its reliance on archaic concepts like two-letter variable names and line 
numbers make it a positively hazardous programming environment which is 
engaging only from an archaeological point of view or to people with a 
distinct fondness for masochism. 

But, alas, it's the only one we have, and it's bundled with each MEGA65.


>> "Hey, but there have been made big enhancements to BASIC10. We can even do
TETRIS with it now! Finally, we have a really good BASIC!"

Sorry to burst your bubble but no, we don't. Not even a little.  

It's true that BitShifter has made tremendous improvements to BASIC10, and he 
deserves heaps of respect and praise for it (matter of fact, the newer versions
of ELEVEN would not exist without his efforts!)

But a good implementation of BASIC it is not. It lacks such a frustratingly
large number of things that were already perfectly natural in 1991 (named 
procedures and functions, fast integer arithmetic, inline assembly, 
indirection operators, structures...) that it's almost painful to argue about 
it again and again in online forums in the year 2021. 


>> "Ok, fair enough. But why just a preprocessor then? Why not a beautiful 
new implementation of BASIC with all the little and big things that BASIC10 
lacks?" 

I would have liked that very much. Regrettably I lack both the skills
and the time for such an undertaking. 

I'm still hopeful that such a language might eventually see the light of day. 
But countless discussions on forum64 and the MEGA65 discord have shown that
it's still a long, long, long way to go. As of early 2021, there are many 
different opinions about what such a language should be -- and even whether it
is needed at all.

And that's how ELEVEN came into existence: Rather than waiting for folks to 
come to their senses, I decided to tackle the problem by writing an editor 
and preprocessor in order to alleviate at least some of the most glaring 
maladies inherent in BASIC10.


>> "Named multiline procedures and functions would be nice. Oh and please,
we want LOCAL variables!"

Indeed they would. As a matter of fact, this feature was implemented in a
very early version of ELEVEN already. But there are two problems: 
a) the amount of parsing needed slows down preprocessing to a crawl and 
b) in order to support recursion, we'd need a wrapper around parameter 
passing, which would make the resulting program much slower. There's only 
so much you can do without resorting to machine language... and once you 
start coding in ML, you might as well implement a sane programming language 
for the M65 altogehter (see above ;-))


>> "Yo. What gives? We don't need your shiny 'modern' concepts on the MEGA65! 
We like this here BASIC10 with them pretty ol' line numbers and spaghetti code 
and them two letter variable names and the quirkiness and the bugses! Matter
of fact, even BASIC2.0 would be enough! BRING BACK THE POKES! Spare us your 
modern big city hocus pocus and MAKE MEGA65 GREAT AGAIN!11!"

Uh... relax. You're welcome to use your MEGA65 in any way you want, as long 
as you leave me out of it.


>> "What's next for ELEVEN?"  

As of May 2022, development of ELEVEN has halted. ELEVEN was intended as a
stopgap measure to give people something to code with until a better BASIC
is on the horizon. It's high time to implement that better BASIC. The dead 
horse which is BASIC65 has been flogged long enough. 


>> "Why call it ELEVEN?"

The inspiration for the name of course comes from Spinal Tap... if you don't 
know the film, I heartily recommend it :)


. 7. Technical information

. 7.1 Overlay mailbox RAM

In order to communicate with each other, editor and precopiler use
a specially designated area in RAM bank 4. To this end, locations 
$04ff00-$04ff79 are used as follows:

$04,ff00 's'    eleven id; each module checks for the existence of these
$04,ff01 'k'    identifier bytes. 

$04,ff02        border colour
$04,ff03        background colour
$04,ff04        foreground colour
$04,ff05        highlight colour
$04,ff06        status bar colour

$04,ff07        misc. flags
               bit 0  : autoload source enable flag
               bit 1  : autojump to line flag
               bit 2  : autobackup flag
               bit 3  : verbose flag
               bit 4-7: reserved

$04,ff08        error number to be displayed (>128 are preprocessor errors)

$04,ff09/0a     line number for autojump

$04,ff10-
$04,ff1f        current file name

$04,ff20-
$04,ff2f        output file name

$04,ff30-        
$04,ff7f        reserved


. 7.2 Attic RAM cache usage

Eleven uses some parts of attic RAM to cache its various modules.

I believe Ubik's intent by this is so that eleven can re-load its modules
straight from the cache (and not need to always access the drive).

As some of these modules have grown over time, the amount of attic RAM
allocated to them has incrementally grown.

The diagram below represents current attic ram usage:


$800,0000 +---------------+
          ! 11.EDIT       ! (max 53kb)
$801,0000 +---------------+
          ! 11.PARSE      ! (max 53kb)
$802,0000 +---------------+
          ! 11.SETTINGS   ! (8kb)
$802,2000 +---------------+
          ! 11.TOKENIZE   ! (8kb)
$802,4000 +---------------+
          ! 11.POST       ! (8kb)
$802,6000 +---------------+

$803,0000 +---------------+
          ! ELEVEN SOURCE !
      ... +---------------+
