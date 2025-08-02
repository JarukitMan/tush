The frontend part of the Turtle Shell. This is the part that will be using `vty` and `Grr`.

# Configurables (The text below is the default configuration file.) (Some keybinds are left unimplemented because I don't want to.)
# pattern
* glob <!-- Either `glob` or `regex` --!>
* insensitive <!-- Either `insensitive` or `sensitive` --!>
# colors
* language
  | type    | color     |
  |---------|-----------|
  | int     | red       |
  | flt     | orange    |
  | bln     | green     |
  | chr     | cyan      |
  | tup     | blue      |
  | arr     | purple    |
  | typ     | pink      |
  | str     | **black** |
  | wrd     | black     |
  | pth     | **green** |
  | comment | gray      |
* window
  | part                  | color |
  |-----------------------|-------|
  | background            | none  |
  | selection             | blue  |
  | seperator background  | black |
  | seperator text        | white |
  | current               | none  |
  | dim                   | none  |
  | suggestion background | blue  |
# styles
* text
  | part      | text                                                                                       |
  |-----------|--------------------------------------------------------------------------------------------|
  | prompt    | `\<\|<directory depth=1>\|<orange>*</orange>> -> `                                         |
  | seperator | `<blue><name></blue><pink>@</pink><red><host></red> in <mode> mode at <directory depth=0>` |
* height
  | part       | lines |
  |------------|-------|
  | main       | 3     |
  | seperator  | 1     |
  | suggestion | 2     |
# features
* autocomplete
* monochrome dimming <!-- as opposed to "colored dimming" or not including dimming at all --!>
* history suggestion
# keybinds
* normal
  * motion
    | motion                | key |
    |-----------------------|-----|
    | left                  |  h  |
    | right                 |  j  |
    | up                    |  k  |
    | down                  |  l  |
    | next word             |  w  |
    | strict next word      |  W  |
    | end of word           |  e  |
    | strict end of word    |  E  |
    | start of word         |  b  |
    | strict start of word  |  B  |
    | start of line         |  0  |
    | first char of line    |  ^  |
    | end of line           |  $  |
    | end of file           |  G  |
    | top of screen         |  H  |
    | middle of screen      |  M  |
    | bottom of screen      |  L  |
    | matching              |  %  |
    | repeat jump to        |  ;  |
    | rollback jump to      |  ,  |
    | next paragraph        |  }  |
    | previous paragraph    |  {  |
    | next search match     |  n  |
    | previous search match |  N  |
  * action
    | action              | key |
    |---------------------|-----|
    | join line           |  J  |
    | delete              |  x  |
    | undo                |  u  |
    | undo line           |  U  |
    | redo                | C-r |
    | repeat              |  .  |
    | copy to line end    |  Y  |
    | delete to line end  |  D  |
    | paste after cursor  |  p  |
    | paste before cursor |  P  |
  * view
    | location       | key |
    |----------------|-----|
    | down one line  | C-e |
    | up one line    | C-y |
    | down one page  | C-f |
    | up one page    | C-b |
    | down half page | C-d |
    | up half page   | C-u |
  * switch
    | mode               | key    | <!-- Window, Mark mode unimplemented. --!>
    |--------------------|--------| <!-- Repeat mode doesn't need keybinds --!>
    | return             | return | <!-- This is sending to the outside-most function. Yet another signal. --!>
    | insert             |  i     |
    | insert to line     |  I     |
    | append             |  a     |
    | append to line     |  A     |
    | new line under     |  o     | <!-- Internally, these modes that are "do action and go into another mode" modes aren't really modes. --!>
    | new line above     |  O     | <!-- This one is just "add line above and go to insert mode". --!>
    | visual             |  v     |
    | visual line        |  V     |
    | visual block       | C-v    |
    | search             |  /     |
    | jump to next       |  f     |
    | jump to previous   |  F     |
    | jump forward until |  t     |
    | jump back to after |  T     |
    | goto               |  g     |
    | fold               |  z     |
    | cursor             |  Z     | <!-- This is different from Vim, but come on, they're different actions. --!>
    | replace            |  r     | <!-- These two modes use --!>
    | overwrite          |  R     | <!-- insert mode keybinds --!>
    | change             |  c     |
    | change line        | C S    |
    | substitute         |  s     |
    | copy               |  y     | <!-- These five modes use the normal mode navigation keybinds. --!>
    | delete             |  d     | <!-- But with the extra bind of repeating themselves to do line. --!>
    | indent             |  >     | <!-- --!>
    | de-indent          |  <     | <!-- --!>
    | re-indent          |  =     | <!-- --!>
    | repeat 1           |  1     | <!-- I know it looks stupid. It IS stupid. --!>
    | repeat 2           |  2     | <!-- This basically sends it to the same repeat mode with that number already in the buffer. --!>
    | repeat 3           |  3     | <!-- What else do I do, make it a new key altogether? I took enough liberties with the cursor. --!>
    | repeat 4           |  4     |
    | repeat 5           |  5     |
    | repeat 6           |  6     |
    | repeat 7           |  7     |
    | repeat 8           |  8     |
    | repeat 9           |  9     |
    | repeat             |        | <!-- The user can define a no-num repeat key, though. Wink. --!>
* insert
  * motion
    | motion                  | key | <!-- Yeah this is all I'm doing for emacs since I don't use it. --!>
    |-------------------------|-----|
    | left                    | C-b |
    | right                   | C-f |
    | up                      | C-p |
    | down                    | C-n |
    | next word               | M-f |
    | start of word           | M-b |
    | start of line           | C-a |
    | end of line             | C-r |
    | end of file             | M-> |
    | start of file           | M-< |
    | top of screen           |  H  |
    | middle of screen        |  M  |
    | bottom of screen        |  L  |
    | matching                |  %  |
    | next paragraph          | M-} |
    | previous paragraph      | M-{ |
  * action
    | action                    | key      |
    |---------------------------|----------|
    | uppercase word            | M-u      |
    | lowercase word            | M-l      |
    | capitalize word           | M-c      |
    | new line                  | S-return |
    | accept history suggestion | C-l      |
  * view
  * switch
    | mode       | key    |
    |------------|--------|
    | normal     | ESC    |
    | return     | return |
    | search     | C-s    |
    | suggestion | tab    |
* suggestion
  * motion
    | motion              | key   |
    |---------------------|-------|
    | previous suggestion | S-tab |
    | next suggestion     | tab   |
  * action
    | action | key    |
    |--------|--------|
    | accept | return |
  * switch
    | mode     | key    |
    |----------|--------|
    | previous | return |
* visual
  * motion
    | motion                  | key |
    |-------------------------|-----|
    | left                    |  h  |
    | right                   |  j  |
    | up                      |  k  |
    | down                    |  l  |
    | next word               |  w  |
    | strict next word        |  W  |
    | end of word             |  e  |
    | strict end of word      |  E  |
    | start of word           |  b  |
    | strict start of word    |  B  |
    | start of line           |  0  |
    | first char of line      |  ^  |
    | end of line             |  $  |
    | end of file             |  G  |
    | top of screen           |  H  |
    | middle of screen        |  M  |
    | bottom of screen        |  L  |
    | matching                |  %  |
    | repeat jump to          |  ;  |
    | rollback jump to        |  ,  |
    | next paragraph          |  }  |
    | previous paragraph      |  {  |
    | next search match       |  n  |
    | previous search match   |  N  |
  * action
    | action                | key |
    |-----------------------|-----|
    | flip selection cursor |  o  |
    | copy                  |  y  |
    | delete                |  d  |
    | un-indent             |  <  |
    | indent                |  >  |
    | re-indent             |  =  |
    | switch case           |  ~  |
    | to lowercase          |  u  |
    | to uppercase          |  U  |
  * switch
    | mode               |  key  |
    |--------------------|-------|
    | insert             |   i   |
    | insert to line     |   I   |
    | append             |   a   |
    | append to line     |   A   |
    | normal             | v ESC y d < > = ~ u U |
    | visual line        |   V   |
    | visual block       |  C-v  |
    | search             |   /   |
    | jump to next       |   f   |
    | jump to previous   |   F   |
    | jump forward until |   t   |
    | jump back to after |   T   |
    | goto               |   g   |
    | fold               |   z   | <!-- Different from normal mode fold since you don't need to do motion to define folds. --!>
    | cursor             |   Z   |
    | replace            |   r   |
    | overwrite          |   R   |
    | change             |  c s  |
    | change line        |  C S  |
    | window             |  C-w  |
    | repeat 1           |   1   |
    | repeat 2           |   2   |
    | repeat 3           |   3   |
    | repeat 4           |   4   |
    | repeat 5           |   5   |
    | repeat 6           |   6   |
    | repeat 7           |   7   |
    | repeat 8           |   8   |
    | repeat 9           |   9   |
* goto
  * motion
    | motion            | key |
    |-------------------|-----|
    | one command down  |  j  |
    | one command up    |  k  |
    | end               |  e  |
    | strict end        |  E  |
    | last char of line |  _  |
    | first line        |  g  |
    | declaration       |  d  | <!-- tabs are left out because I don't have that right now --!>
  * switch
    | mode     | key                   |
    |----------|-----------------------|
    | previous | j k e E _ g d t T ESC |
* fold
  * action
    | action                        | key |
    |-------------------------------|-----|
    | delete fold under cursor      |  d  |
    | delete all folds under cursor |  D  |
    | toggle fold under cursor      |  a  |
    | toggle all folds under cursor |  A  |
    | open fold under cursor        |  o  |
    | open all folds under cursor   |  O  |
    | close fold under cursor       |  c  |
    | close all folds under cursor  |  C  |
    | open all folds by one level   |  r  |
    | open all folds                |  R  |
    | close all folds by one level  |  m  |
    | close all folds               |  M  |
    | toggle folding functionality  |  i  |
  * switch
    | mode        | key |
    |-------------|-----|
    | define fold |  f  | <!-- This uses the normal mode internally. The position returned is used to define the fold. --!>
    | normal      | d D a A o O c C r R m M i |
* cursor
  * view
    | location | key |
    |----------|-----|
    | center   |  z  |
    | top      |  t  |
    | bottom   |  b  |
  * switch
    | mode     | key   |
    |----------|-------|
    | previous | z t b |
