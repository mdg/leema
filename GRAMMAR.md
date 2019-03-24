
## Statements

leemafile = stmts

stmts = stmts stmt

stmt = defconst
     | deffunc
     | defmacro
     | deftype
     | let
     | expr

defconst = "const" localid := expr
deftype = defstruct | defenum

defstruct = "type" structdef "--"
structdef = genericid idtypes

defenum = "type" genericid enumvariants "--"
enumvariants = enumvariants enumvariant
enumvariant = "|" genericid
            | "|" structdef
enumfields = enumfields enumfield
enumfield = "|" localid "(" lri

deffunc = "func" genericid idtypes blockx

idtypes = idtypes idtype
idtype = "." localid ":" lri
       | "." localid
       | ":" lri

genericid = localid "[" klist "]"

let = "let" pattern optype ":=" expr

## Expressions

expr = blockx
     | ifx
     | matchx
     | prefixop(op, precedence)
     | binaryop(op, precedence, association)
     | lessthan3
     | term

blockx = arrowblock "--"

ifx = "if" cases "--"
matchx = "match" expr cases "--"
       | "match" cases "--"
cases = cases ifcase
case = "|" expr arrowblock

prefixop(op) = op term

binary(op) = expr op expr

lessthan3 = expr "<" expr "<" expr

## Terms

term = localid
     | call
     | list
     | tuple
     | typecall
     | strx

call = term "(" xlist ")"

list = "[" xlist "]"

tuple = "(" xlist ")"

typecall = term "[" xlist "]"

strx = DoubleQuoteL DoubleQuoteR
     | DoubleQuoteL strlist DoubleQuoteR
strlist = strlist stritem
stritem = strlit | expr

## Extras

arrowblock = ">>" stmts

klist = klist "," klistitem
klistitem = localid | localid ":" expr

xlist = xlist "," xlistitem
xlistitem = expr | localid ":" expr

kxlist = kxlist "," kxlistitem
kxlistitem = localid ":" expr

