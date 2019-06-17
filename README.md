# hsp
hsp is a command line text processor that provides most of the functionality of grep, sed, awk, and much more using standard haskell text and list functions as well as custom functions.  hsp uses a haskell interpreter (from the hint package) that makes available any function or operator defined in the Prelude, Data.Text (qualified as T), Data.List, and several other modules.  The interface is largely based on the Python Pyed Piper project, first developed by Toby Rosen at Sony Imageworks.  However, hsp is much faster.
## Why use hsp?
The unix utilities are powerful and fast, but the syntax can be difficult to remember.  On the other hand, if one is a haskell user on a regular basis, hsp provides an intuitive and easy to master syntax, with acceptable performance even with large amounts of text.  Another advantage of hsp compared with its unix cousins is hsp's easy-to-use macro facility, which offers a convenient way to save, list, search, and recall complex, frequently used, text manipulations without the need for collecting and managing executable scripts. 
## Installation
### Step 1
Install stack if not already installed.  See https://docs.haskellstack.org/en/stable/install_and_upgrade/ for instructions.
### Step 2
Download the source from github, build the executable, and install the hsp script, using the following commands.
```bash
git clone https://github.com/bawolk/hsp
cd hsp
make && make install
```
This will create an executable script `hsp` and copy it to `~/.local/bin`.  If that directory does not exist, or that directory is not in your $PATH, simply copy the hsp file from the hsp source directory to a directory in your $PATH. The hsp script runs stack in the downloaded source directory, so the source directory should not be deleted.  
## A simple example
Suppose you have a large list of words in a file (say the ~102K words in the /usr/share/dict/words or equivalent file provided by most linux distributions). You wish to find all of the words that have five or more characters and are palindromes.  Futhermore, when you output the result, you wish to reverse the order of the words and prefix each word with the string "Palindrome: ".  We can accomplish this with hsp as follows:
```bash
$ cat /usr/share/dict/words | hsp 'T.length p > 4 && p == T.reverse p | reverse pp | "Palindrome: " <> p'
Palindrome: tenet
Palindrome: stats
(+ 15 more)
```
This contrived example illustrates several important features of hsp.  The command string provided to hsp is contained within single quotes.  Double quotes are reserved for string literals.  The command string can be divided into pipes using the "|" character, quite analagous to unix pipes.  The output of each pipe is the input to the next.  `p` and `pp` are the workhorse variables of hsp.  `p` represents the line-by-line output from the previous pipe.  `pp` represents a list of all the lines of the previous pipe.  If the statement within a given pipe yields a Bool (i.e., True or False), input lines that yield True are kept and lines that yield False are discarded.  As a general rule, any Haskell function that takes a Text argument can be used with `p` and any Haskell function that takes a list argument (technically, Ord a => [a], which is why a function such as `sort` will work) can be used with `pp`.
  * **Why the "T." prefix?**  For efficiency reasons, `p` has type Text.  This means that the usual string manipulation functions in the prelude cannot be used.  Instead, hsp imports the Data.Text module from the text package, qualified as T.  Whenever you need to use a function from the Data.Text module, prefix it with "T."
  * **Why "<>"?**  Strings can be joined by "++", but hsp uses the Text type.  Text must be joined by "<>".
  * **Performance**.  hsp is fast.  On a three-year-old modest desktop this example took about .6 second, with .3 second of that the time it takes to load the program.
## Splits, joins, and lists
In the simple example above, the output of each of the three pipes in the pipeline is a list of Text.  But there is a second acceptable type of output: a list of lists of Text.  This allows a user to use a haskell expression to divide each line into various fields which are then input into the next pipe to be further manipulated.  To this end, hsp provides a number of special built-in splits and joins that make it easy to accomplish most of the common ways lines are split and then joined.
  * **Built-ins.**
    + `s` or `slash`: p split/joined on "/"        
    + `d`  or `dot`: p split/joined on "."        
    + `w`  or `whitespace`: p split/joined on whitespace (on spaces,tabs,etc)
    + `u`  or `underscore`: p split/joined on '_'       
    + `c`  or `colon`: p split/joined on ':'       
    + `mm` or `comma`: p split/joined on ','        
    + `m`  or `minus`: p split/joined on '-'        
    + `a`  or `all`: p split on non-alphanumeric characters/joined on spaces
  * **Example:** The hsp function `c` (or `colon`) splits every input line at colons.
```bash
  $ echo $'a:1:cat\nb:3:dog\nd:7:fish' | hsp 'c'
  [0][[0]a[1]1[2]cat]
  [1][[0]b[1]3[2]dog]
  [2][[0]d[1]7[2]fish]
```
  * **Interpreting the output.** The output of a split is a numbered list of numbered fields.  Unless the `--no-color` (or `-c`) flag is present, the display uses colors to distinguish the field numbers from the field data, making the output more readable.  This assists the user in constructing the next pipe to further manipulate the data.
  * **Arbitrary splits: the `splitOn` function.** In addition to the built-ins noted above, hsp imports unqualified (so no need for the "T." prefix) the function `splitOn` from Data.Text, which splits each line based on any given string.
```bash
  $ echo $'1@@cat\n3@@dog\n1@@fish' |hsp 'splitOn "@@" p'
  [0][[0]1[1]cat]
  [1][[0]3[1]dog]
  [2][[0]1[1]fish]
```
  * **Joining fields.** The built-in split functions also operate as join functions when they follow a split.  For example, the function "u" (or "underscore") splits on underscores, but it also joins with underscores as well.  Of course, appropriate haskell functions, such as `T.intercalate`, can be used to join as well.
 ```bash
    $ echo $'a:1:cat\nb:3:dog\nd:7:fish' | hsp 'colon | u'
    a_1_cat
    b_3_dog
    d_7_fish
    $ echo $'a:1:cat\nb:3:dog\nd:7:fish' | hsp 'c | T.intercalate "%%" p'
    a%%1%%cat
    b%%3%%dog
    d%%7%%fish
 ```
  * **Selecting fields.**  Following a split, haskell functions that operate on a list to produce a new list are available to modify the list. For example `drop` could be used to delete fields.  Note that `p`, when used in a pipe following a split, represents the list of fields.  
```bash
  $ echo $'a:1:cat\nb:3:dog\nd:7:fish' | hsp 'c | drop 1 p'
  [0][[0]1[1]cat]
  [1][[0]3[1]dog]
  [2][[0]7[1]fish]
```
  * **The hsp selection operator: (!!!).**  hsp provides a useful operator, `!!!`, to select and reorder fields. The right operand is a haskell list of integers representing the field numbers to be selected in the desired order.  Field numbers can be repeated.  `!!!` is actually useable with any haskell list, not just the list of fields following a split.
```bash
  $ echo $'a:1:cat\nb:3:dog\nd:7:fish' | hsp 'c | p !!! [2, 1, 2]'
  [0][[0]cat[1]1[2]cat]
  [1][[0]dog[1]3[2]dog]
  [2][[0]fish[1]7[2]fish]
```
  * **Constructing lines following a split.**  Joining the fields in the pipe following a split is just one way to construct a new line.  One can also select individual fields of interest using haskell's list index operator `!!` and manipulate them as Text.
```bash
  echo $'a:1:cat\nb:3:dog\nd:7:fish' | hsp 'c |  p!!2 <> "*" <> p!!0 <> "-" <> p!!1'
  cat*a-1
  dog*b-3
  fish*d-7
```
## Operations on the entire input list
The entire input list can be manipulated using the `pp` variable.  In the simple example above, `sort pp` sorted the input.  Duplicate lines can be deleted using `nub pp` or the equivalent hsp function `uniq pp`.  Other haskell functions, such as `drop`, `take`, `tail`, and `init` can be used as well.
  * **General rule.**  Although it is helpful to think of `pp` as a list of Text, its underlying type is more complex.  Only functions that operate on `Ord a => [a]` are acceptable.  A function that attempts to operate on the underlying type `a` will generally fail.  When `pp` is used, the output is displayed as a numbered list.
  * **Adding lines using string literals.**  Through the magic of the OverloadedStrings GHC extension, a list of string literals can be used to add lines to the input.  Any attempt to directly use any other object of type Text in this manner will fail.  Note that `pp` is not constant throughout the pipeline.  In each pipe it represents the output from the preceeding pipe.
```bash
  $ echo $'cat\ndog' | hsp '["Animals", "======="] ++ pp ++ ["bird"]'
  [0]Animals
  [1]=======
  [2]cat
  [3]dog
  [4]bird
```
  * **Adding lines using the hsp `text` function.**  An object of type Text can be converted to a line that can be added to `pp` using the hsp function `text`.  The following example adds the current directory as the first line of the output.  Note that `pwd` is a special hsp function that returns the current directory as a Text.
```bash
  $ echo $'bird\ndog' | hsp '[text ("pwd: " <> pwd)] ++ pp| p'
  pwd: /home/user/hsp
  bird
  dog
```
  * **Special operations on `pp`.**  hsp provides a number of specialized functions (see below) that can operate on `pp`. For example, `oneline pp` converts all the lines of input Text into a single line of Text.
```bash
  $ echo $'cat\ndog' | hsp '["Animals", "======="] ++ pp ++ ["bird"] | oneline pp'
  Animals ======= cat dog bird
```
  * **Using `pp` following a split: the `expand` function.**  When `pp` is used following a split, it represents the list of all the split lines. hsp provides a special function, `expand`, that takes a list of split lines and converts it to a list of all the fields contained in the split lines.  
```bash
  $ echo $'1:cat\n3:dog' | hsp 'c | expand pp'
  [0]1
  [1]cat
  [2]3
  [3]dog
```
  * **Returning to text manipulation following the use of `pp`.**  Since `pp` refers to the entire input, it is not possible to manipulate individual lines.  Thus any attempt to use `pp` and `p` in the same pipe will fail.  To manipulate individual lines simply pipe the output into another pipe and use `p` in that pipe.
```bash
  $ echo $'1:cat\n3:dog' | hsp 'c | expand pp | p'
  1
  cat
  3
  dog
```
## Filtering lines
If the expression in a pipe results in a Bool (i.e., True or False), the pipe acts as a filter.  Lines that result in True are passed to the output; lines that result in False are dropped.  Standard haskell logic operators such as `&&`, `||`, and `not` are available. 
  * **Example.**
```bash
  $ echo  $'1:cat\n3:dog\n1:fish' | hsp  'T.isPrefixOf "1" p && T.length p > 5'
  1:fish
```
  * **Special hsp filtering functions.** hsp provides a few easy-to-use functions for filtering, which operate implicitly on unsplit lines.  Using them on split lines will fail.
    + `k` (or `keep`): takes a list of strings and returns True if any string in the list is contained in the line.
    + `l` (or `lose`): takes a list of strings and returns False if any string in the list is contained in the line.
    + `rek` (or `rekeep`): takes a regular expression string and returns True if the line matches the regular expression.  POSIX extended regular expressions are used.
    + `rel` (or `relose`): takes a regular expression string and returns False if the line matches the regular expression. POSIX extended regular expressions are used. 
```bash
    $ echo  $'1:cat\n3:dog\n1:fish' | hsp 'k ["og", "is"]'
    3:dog
    1:fish
    $ echo  $'1:cat\n3:dog\n1:fish' | hsp 'l ["og", "is"]'
    1:cat
    $ echo  $'1:cat\n3:dog\n1:fish' | hsp 'rek "^1.*i"'
    1:fish
    $ echo  $'1:cat\n3:dog\n1:fish' | hsp 'rel "h$"'
    1:cat
    3:dog
```
  * **Filtering after a split.** Filtering is available after a split, but it must be kept in mind that `p` after a split refers to a list of Text.  Filtering based on a given field's content is easily accomplished using the haskell list index operator,`!!`.
```bash
  $ echo  $'1:cat\n3:dog\n1:fish' | hsp  'c | p !! 1 == "dog" | c'
  3:dog
```
  * **Retaining dropped lines as blank lines: the `--keep-false` (or `-k`) flag.**  By default, all lines that test False are eliminated from the output completely.  If the `--keep-false` flag is added to the hsp command line, lines that test False will be kept as blank lines (or empty lists in the case of split lines). This can be useful if one needs placeholders to keep lists in sync.
```bash
  $ echo  $'1:cat\n3:dog\n1:fish' | hsp --keep-false 'c | p !! 0 == "1" | c'
  1:cat

  1:fish
```
## Line numbers
hsp provides a special variable, `n`, that represents the zero-based line number of each line.  The type of `n` is `Int` to facilitate arithmetic operations.  Typically one uses the special hsp function `tshow` to display `n`.  The usual haskell `show` function yields a String, but hsp needs a Text.
```bash
$ echo $'cat\ndog\nfish' | hsp 'tshow (n + 1)  <> " " <> p'
1 cat
2 dog
3 fish
```
## Math
hsp provides two functions to facilitate basic mathematical operations: `integer` and `double`.  Currently, if the string cannot be parsed into the appropriate type, the hsp command will fail.  The second example below illustrates how hsp can be used to sum a list of numbers.  Recall that after a split, `p` represents a list of Text.  Also note the use of `tshow` to convert from a number to Text.
  * `integer`: Converts a Text to an `Integer`.
  * `double`: Converts a Text to a `Double`.
```bash
$ echo $'12\n73\n7' | hsp 'tshow (integer p + 1)'
13
74
8
$ echo $'12\n73\n7' | hsp 'oneline pp | w | (tshow . sum) $ integer <$> p'
92
```
## History
hsp caches the output of each pipe in the hsp command pipeline for possible use later in the pipeline.  Two functions, `hp` and `hpp`, provide access to this cache.  Each takes as its only argument a literal positive or negative integer.  A positive integer, n, refers to the nth (zero-based pipe), with n = 0 refering to the original standard input to the hsp command.  A negative integer, -n, counts backward from the current pipe.  Thus, -1 refers to the immediately preceeding pipe.  To limit unnecessary memory use, no cacheing occurs if history is not used in the pipeline.
  * `hp n`.  This variable operates similar to `p`.  For example, `hp 2` refers to the line-by-line output of the second pipe.
  * `hpp n`.  This variable operates similar to `pp`.  For example, `hpp 1` refers to the entire output of the first pipe.
```bash
  echo  $'1:cat\n3:dog\n1:fish' | hsp 'c|u|hp 1'
  [0][[0]1[1]cat]
  [1][[0]3[1]dog]
  [2][[0]1[1]fish]
```
  * **Limitations.**
    + If `hp` and `p` are used in the same pipe, they must have the same type.  For example, if the preceeding pipe was a split, the history pipe must also be a split.
    + If `hp` is used in a given pipe, the lengths of the output of the preceeding pipeline and the history pipeline are made equal by expanding the shorter output with blank lines.
    + `hp` may be used more than once in a given pipe, but only the first one determines which preceeding pipe is used.  
  * **The hsp variable `o` (or `original`) and `hp 0`.**  `hp 0` represents the original line-by-line standard input to hsp.  The special hsp variable `o` (or `original`) also refers to the original input of each line, but it is the original of each line of the current pipe's input.  Thus, even if the lines are reordered or filtered, `o` will remain attached to its original line.  Also, if lines are added to `pp` using `++`, `o` will refer to those lines in future pipes. The following example should make this clear.
```bash
  $ echo $'cat\ndog\nbird\nfish' | hsp 'sort pp | lose ["do"] | pp ++ ["rat"] | upper p <> "-" <> hp 0 <> "-" <> o'
  BIRD-cat-bird
  CAT-dog-cat
  FISH-bird-fish
  RAT-fish-rat
```
## Other inputs
The normal input to the hsp command is via standard input, usually by piping into it via a shell command, often `cat`.  But sometimes it may be necessary to combine two streams of inputs, such as combining the output of two shell commands (or two files) line by line.  hsp provides several ways to accomplish this.
  * **Second stream input.**  Any strings on the hsp command line that follow the hsp command pipeline string and are not part of one of the hsp command line option flags are treated as lines of second stream input.  This input can be referenced separately from the primary stream by using the variables `sp` and `spp`, in essentially the same fashion as `p` and `pp`.  Apart from simply tacking on strings at the end of the command, one can execute a shell command that outputs such strings, using backticks.  For example, `` `ls` `` placed at the end of the hsp command line would be equivalent to listing all the file names in the directory on the command line.
```bash
  $ echo $'cat\nfish' | hsp 'p <> " " <> sp' dog bird
  cat dog
  fish bird
  $ echo $'cat\nfish' | hsp 'p <> " " <> sp' `ls`
  cat app
  fish ChangeLog.md
```
  * **File input: the `--text-file` (or `-t`) flag.**  In a similar way, text can be input from a text file using the `--text-file` flag. This input can be referenced separately using the variables `fp` and `fpp`, in the same fashion as `sp` and `spp`.  This option is intended to allow data from standard input to be combined with data in a text file.  If the only input you need is a text file, you should `cat` it, and pipe the output into hsp.
```bash
  $ cat f.txt | hsp -t g.txt `p <> " " <> fp`
  fline1 gline1
  fline2 gline2
  ...
```
  * **Input length adjustment.**  When a second stream or file input is used line-by-line in any pipe, i.e., via `sp` or `fp`, the length of that input is made equal to the length of standard input to that pipe, i.e., the length of `pp`.  If it is longer than `pp`, it is truncated, if it is shorter, it is expanded with blank lines. Note that this adjustment does not apply to operations on the entire list using `pp`, `spp`, or `fpp` directly.  For example, `pp ++ spp ++ fpp` would combine all three inputs without any length adjustment. Also, the adjustment applies only to the current pipe and does not affect the use of the inputs in other pipes.  If you need to use more of the line-by-line input than the length of `pp` will allow, `pp` can be expanded by a specified number of blank lines using the hsp `blanklines` function.
```bash
  $ ls short_directory | hsp 'pp ++ blanklines 10 | p <> "-" <> sp' `ls long_directory`
  short1-long1
  short2-long2
  -long3
  -long4
  ...
```
  * **Blank line input: the `--blank-inputs` (or `-b`) flag.**  It is possible to generate output from hsp with no input using the hsp `--blank-inputs` (or `-b`) flag.  For example, one could generate a list of numbers by using the `n` variable.
```bash
  $ hsp -b 4 'tshow(n+1)'
  1
  2
  3
  4
```
  * **The `--rerun` (or `-r`) flag.**  If you need to process output from a long-running script, you can first buffer it to a file by piping the output into hsp without any hsp command:  `long_running_script | hsp`.  You can then experiment with various hsp commands by running the hsp command with the `--rerun` flag as many times as you wish.  If hsp is run without the `--rerun` flag, any previous buffered input is lost.
  * **Cut and paste input.**  You can also apply hsp to lines you have cut. First execute hsp. Since there is no input it will wait.  Now paste in your data.  It should be displayed in the terminal.  Now hit Ctrl-d twice. This will put the data into the disk buffer. Finally, run hsp with the `--rerun` flag and provide whatever hsp command line you need.
## Macros
Macros are a way to permanently store useful commands for future use. Macros can become quite complex, and
provide a useful intermediate between shell commands and scripts, especially for solving one-time problems.  
  * **Macro creation: the `--macro-save` (or `-s`) flag.** The `--macro-save` flag causes the current hsp command to be saved as a macro.  The macro name is specified by the string following the flag.  If the string contains "#", the macro name is the part of the string preceeding the "#" and the part after is treated as a comment that is viewable if the macro is displayed.  If an existing macro has the same name it will be overwritten.
    + **Example.**  The hsp pipeline in the simple example at the beginning of this README could be saved as a macro named "palindrome" as follows:
```bash
    $ hsp -s "palindrome# palindrome finder" 'T.length p > 4 && p == T.reverse p | reverse pp | "Palindrome: " <> p'
```
  * **Using a macro.**  To use a macro, simply insert it at an appropriate location in the hsp command. You can hsp to and from macros just like any normal hsp command, e.g., `hsp 'a| my_favorite_macro | p'`.  Note that additional items can follow the macro name in the pipe, as long as they are consistent with the contents of the final pipe of the macro.  For example, if the macro returns a list, you can access individual elements using the list index operator `!!`: `hsp 'my_list_macro !! 2'`  
    + **Macros with arguments.** If the macro ends with a function that needs arguments, these can be provided
    after the macro name.  Even lambda functions are acceptable.  For example, suppose 
    my_macro is `'c | u | p <> (\x y -> x <> "--" <> y)'`.  One could write: `hsp 'my_macro "first" "second"'` 
    + **Limitations**
      - hsp currently does not allow a macro to contain another macro.
      - Avoid macro names that are the same as a haskell function, hsp function, or variable name.  The macro will replace any identically named identifiers in the pipeline.
  * **Deleting a macro: the `--macro-delete` (or `-d`) flag.**  `hsp -d my_macro` will delete the my_macro macro.
  * **Listing the macros: the `--macro-list` (or `-l`) flag.** `hsp -l` will list all available macros, sorted by macro name.  The list displays the name, the username of the creator, the date of creation (UTC time), the macro itself, and any comment.
  * **Finding a macro: the `--macro-find` (or `-f`) flag.**  `hsp -f search_term` will list all macros that contain search_term in the name, user, or comments fields.  For example, `hsp -f pal` will display the palindrome macro and perhaps others as well.
  * **Group macros: the `--macro-group` (or `-g`) flag.**  Macros can be shared among a team by making them group macros.  When the macro-group flag is present, the macro-save and macro-delete commands operate on the group macro database.
    + **CAUTION!:** Group macros represent a huge security hole.  Since hsp permits the execution of shell scripts, a malicious attacker could modify the group database and wreak havoc with your system.  For this reason, hsp will only use group macros if the macro-group flag is present on the command line.
    + **User macros have priority.**  User macros override group macros with the same name.
  * **Macro files.**  The user macro file, hsp_user_macros.json, and the group macro file, hsp_group_macros.json, are stored in your home directory by default. This location can be changed to another directory by setting the HSP_MACRO_DIR and HSP_GROUP_MACRO_DIR environmental variables in the hsp script.
## Custom functions
hsp provides a number of special functions and imports all the haskell functions in the Data.Text (qualified by "T") and Data.List modules.  You can make new functions available to hsp by placing them in a module named HspCustom.hs.  The default directory is your home directory, but this can be changed by setting the HSP_CUSTOM variable in the hsp executable script.  The source directory already has an HspCustom.hs.example file with a sample function to illustrate what is needed.  To experiment with the sample function, just copy the example file to HspCustom.hs in your home directory and run hsp.  Note that you must import any modules that you need for your function.
  * **Available modules.**  Currently only modules in the following packages are available:  base, text, hsp, hsphint (subpackage of hsp), directory, regex-compat, filepath, time, unordered-containers, process, and filemanip.
## Bash quirks
Because the hsp command line string must first be parsed by bash, there are some issues to keep in mind.
  * **Single quotes: `'` and `apost`.** The hsp command string is enclosed with single quotes.  This allows double quotes to be used in the command string without escaping.  However, a single quote may not occur between single quotes, even when preceded by a backslash.  If a single quote is needed, one can prefix the command string with a `$` and escape the single quote.  For example, `hsp $'lose ["\'"]'` will successfully filter out all lines containing a single quote.  As an alternative, hsp provides the `apost` constant, which is defined as `"'"`.  Thus, `hsp lose[apost]` will work as well.
  * **Double quotes: `"` and `quote`.** As noted, in bash everything between single quotes is taken literally.  Escape sequences have no special meaning.  This actually works well with haskell, since to include a `"` in a literal string, one must escape it.  For example `a = "\""` sets `a` to be the one character string `"`.  For example, `hsp 'lose ["\""]'` (note the absence of the `$`) will successfully filter out all lines containing a double quote.  Trying `hsp $'lose ["\""]'` will produce a syntax error since the haskell interpreter would see `lose ["""]`.  If you need to use the `$` prefix, you must escape the `\`:  `hsp $'lose ["\\""]'`. As an alternative, hsp provides the `quote` constant, which is defined as `"\""`.  Thus, `hsp lose[quote]` will work as well.
  * **Unicode characters.**  hsp can be used with UTF-8 input.  For example, `echo $'☺\nhello' | hsp 'keep ["☺"]'` will drop the "hello" line and keep the `☺`.  You can also use the hexadecimal or decimal version of the unicode character.  For example, `☺` is the same as `\x263a` and `\9786`.  However, as with quotes, care must be taken because of the `\`.  With single quotes, `'keep ["\x263a"]'` and `'keep ["\9786"]'` work fine, but if you are using the `$' '`-type command string, the `\` must generally be escaped: `$'keep ["\\x263a"]'`.  The reason is that `\xnn` is an escape sequence recognized by `$' '`-type strings and represents the hexadecimal value `nn`.  Similarly `\nnn` is an escape sequence representing the octal value `nnn`.  In our example, bash would replace the `\x26` piece with `&` and you would be keeping all lines containing `&3a`, not the desired unicode character.  Note that in our particular example, the decimal version does not require the extra `\`: `$'keep ["\9786"]'` works just fine.  The reason is that bash only escapes `\nnn` when `nnn` is an octal number.  Nevertheless, it is probably wise to use `\\` with both versions when using `$' '`-type strings.
## Additional variables and functions
In addition to the variables and functions already discussed, hsp provides other useful special variables and functions.
### Variables
  * **`date`:**  This returns the current UTCTime object from the Data.Time module. It must be 
  converted to a Text using `tshow` or more usefully, formatTime. Example: 'pack $ formatTime defaultTimeLocale "%D" date' will produce output like this: 05/03/19.  See documentation for the Data.Time module.
  * **`pwd`:**  This returns the present working directory as a Text.
  * **`envXXX`:**  This returns the value of evironment variable XXX as a Text.  For example if the username is "iris", envUSER will return the Text "iris".
  * **`digits`:**  The Text "0123456789".
  * **`letters`:** The Text "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".  This can be useful when combined with the variable `n`: 'letters !! n' will print out "a" on the first line, "b" on the second line, etc.
  * **`punctuation`:**  The Text "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
### Text functions
These functions take a Text argument, often p, sp, fp, or hp.  
  * **`cdigits`:**  Returns a list of contiguous numbers.  Example: 'cdigits "ab 77 ! 34b "' returns the list ["77", "34"].
  * **`cletters`:**  Returns a list of contiguous letters.
  * **`cpunctuation`:**  Returns a list of contiguous punctuation
  * **`lower`:**  Converts a Text to lower case.
  * **`upper`:**  Converts a Text to upper case.
  * **`trim d`:**  Removes the last occurrence of text d and all that follows.
  * **`kill [STR1,STR2...]`:**  Removes the listed strings.  Example: 'kill ["##", "b"] "1:b:signal##"' returns "1::signal".
  * **`clean d`:**  Splits Text at all metacharacters (except for slashes, dots and d), then rejoins with delimeter d.  d is a one-character string. Example: 'clean ":" "1:&7:b:signal##5"' returns "1::7:b:signal:5".
  * **`replace OLD NEW`:** Replaces all occurrences of text OLD with text NEW.  Actually this is just the `replace` function from the Data.Text module, but can be used without the "T." prefix.
  * **`dir`:**  Returns the directory of a path text.  Example: 'dir "/home/user/project/package.yaml"' returns "/home/user/project/".
  * **`file`:**  Returns the file name of a path text.  Example: 'file "/home/user/project/package.yaml"' returns "package.yaml".
  * **`ext`:**  Returns the file extension of a path text.  Example: 'ext "/home/user/project/package.yaml"' returns "yaml".
#### Regular expression functions: `re` and `sub`
hsp provides two text functions that take a regular expression as their first argument. Note that hsp uses "POSIX extended regular expressions", which is the same standard used by egrep. This differs from the Perl and python standards, but there is considerable overlap.
  * **`re REGEX`:**  Returns the portion of Text that matches the first group of the REGEX regular expression.  Example: 're "(x.\*l)" "example"' returns "xampl".  The parentheses are important, since without them no group is specified.  Thus 're "x.\*l" "example"' would return "".
  * **`sub REGEX NEW`:** Replaces each occurrence of the text that matches the REGEX regular expression with the text NEW.  If the REGEX is a simple string (e.g., "foo"), use the `replace` function noted above instead of `sub`.  It is faster.
### Functions on pp and hpp (when they are not split), spp and fpp.
  * **`delimit DELIM`:**  Split input on delimiter string instead of newlines.  Newlines are replaced by spaces.
  * **`divide N`:**  Consolidates every N consecutive lines to a single split line with N fields.  Final output line may contain fewer fields.  
  * **`before STRING N`:**  Searches for STRING, then consolidates N lines *before* it to a single line. 
  * **`after STRING N`:**  Searches for STRING, then consolidates N lines *after* it to a single line.
  * **`matrix STRING N`:**  Searches for STRING, then consolidates N lines before it and N lines after it to a single line.
  * **`onelined DELIM`:**  Combines input to one line with delimiter DELIM.
  * **`oneline`:**  Combines input to one line with delimiter single space, i.e, oneline = onelined " ".
### hsp system functions
  * **`shell COMMAND`:** Returns a list of the lines of standard output from the shell command COMMAND as a split line.  COMMAND is a literal string.  E.g., 'shell "ls /etc"'.
  * **`glob PATTERN`:**  Returns a list of file names matching the glob PATTERN as a split line.  PATTERN is a literal string.  E.g., 'glob "src/*"'. Currently only one glob can appear in a given pipe.
## Testing
 The testing is fairly primitive currently.  There is a series of system tests that assure that hsp's basic functionality is correct.  These can be run using shelltestrunner, a command-line tool for testing command-line programs, which is available in most distributions.  Once shelltestrunner is installed, just run `shelltest test` in the hsp package directory.
