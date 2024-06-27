# https://ziglang.org/ theme

# Code
face global attribute keyword
face global comment rgb:aaaa77+i
face global documentation comment
face global error rgb:c3bf9f+b
face global function rgb:ee3333+b
face global identifier Default
face global keyword rgb:eeeeee+b
face global module string
face global operator Default
face global string rgb:22ee55
face global type rgb:6688ff+b
face global value rgb:ff8080
face global variable Default

# #include <...>
face global meta rgb:ff894c

# TODO: Markup
face global title blue
face global header cyan
face global mono green
face global block magenta
face global link cyan
face global bullet cyan
face global list yellow

# Builtin
face global Default rgb:cccccc,default
face global PrimarySelection default,default,default+r
face global SecondarySelection default,default,default+r
face global PrimaryCursor black,white+fg
face global SecondaryCursor black,white+fg
face global PrimaryCursorEol black,rgb:22ee55+fg
face global SecondaryCursorEol black,rgb:22ee55+fg

face global LineNumbers rgb:777777
face global LineNumberCursor rgb:eeeeee+b

# Bottom menu:
face global MenuBackground white,rgb:222222,default
face global MenuForeground black,white,default

# completion menu info
face global MenuInfo black,rgb:6688ff

# assistant, [+]
face global Information MenuBackground

face global Error rgb:ee3333,default,default+b
face global DiagnosticError rgb:ee3333
face global DiagnosticWarning rgb:ff894c
face global StatusLine rgb:aaaa77,default

# Status line modes and prompts:
# insert, prompt, enter key...
face global StatusLineMode string

# 1 sel
face global StatusLineInfo type

# param=value, reg=value. ex: "ey
face global StatusLineValue string

face global StatusCursor PrimaryCursorEol

# :
face global Prompt rgb:aaaa77

# (), {}
face global MatchingChar rgb:6688ff+b

# EOF tildas (~)
face global BufferPadding rgb:777777,default

# Whitespace characters
face global Whitespace default+f

