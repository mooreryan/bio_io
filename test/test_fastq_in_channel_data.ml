(* For good stuff: header line ends with [#], seq line end with [N], extra line
   end with [%] qual line end with [?]. *)

let empty = ""

let seqs =
  {eof|@s1 apple#
ACTGN
+ a a   a%
!!!!?
@s2 pie#
a c  t    gN
+
! !! !              ?
|eof}

let all_ampersand = {eof|@
@@
@@@
@
|eof}

let all_ampersand2 = {eof|@
@@
@@@
@
@
|eof}

let one_line = {eof|@apple pie|eof}

let two_lines = {eof|@apple pie
actgN
|eof}

let three_lines = {eof|@apple pie
actgN
+
|eof}

let four_lines = {eof|@apple pie
actgN
+
....?
|eof}

let four_lines_with_starting_blank = {eof|
@apple pie
actgN
+
....?
|eof}

let random_stuff = {eof|a
b
c
d
e
f
|eof}

let no_extra_marker = {eof|@a
N
missing
!
|eof}

let tricky_seqs =
  {eof|@ empty seq at beginning#

+

@seq1 is fun#
AAC TGG AA N
+
!!! !!! !! ?
@seq2#
     AAT      CCTGGGN
+
     !!!      !!!!!!?
@ empty seq 1#

+

@ empty seq 2#

+

@seq3#
yyyyyyyyyy     yyyyy yyN
+seq3%
!!!!!!!!!!     !!!!! !!?
@seq 4 @ has many '@' in header#
ACTGactN
+ h h eeee%
1234!@#?
@seq 5#
          a      c     t     N
+
123?
@empty seq at end

+

|eof}

let bad_file = "\r"

let weird_blank_lines = {eof|

@s1

A

|eof}

let empty_header_lines = {eof|@
ACTG
+
!!!!
@
actg
+
!!!!

|eof}
