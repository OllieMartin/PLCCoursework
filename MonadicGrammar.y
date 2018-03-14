{ 
module MonadicGrammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token
    rel { TokenRel _ $$ } 
    var { TokenVar _ $$ } 
    '=' { TokenEq _ } 
    '(' { TokenLParen _ } 
    ')' { TokenRParen _ } 
    '{' { TokenOpenBrace _ }
    '}' { TokenCloseBrace _ }
    ',' { TokenComma _ }
    ';' { TokenSemiColon _ }
    '_' { TokenUnderscore _ }
    '[' { TokenOpenSqr _ }
    ']' { TokenCloseSqr _ }
    num { TokenNum _ $$ }

%monad { E } { thenE } { returnE }

%% 
Prog : StrictVarList '{' ConstraintList '}'  {Query $1 $3}
| StrictVarList '{' ConstraintList '}' Prog {ProgLink (Query $1 $3) $5}

VarList : VarItem {[$1]}
| VarItem ',' VarList {$1 : $3}

StrictVarList : var {[$1]}
| var ',' StrictVarList {$1 : $3}

VarItem : var {VarItemVar $1}
| '_' {VarItemBlank}

ConstraintList : Constraint ';' {[$1]}
| Constraint ';' ConstraintList {$1 : $3}

Constraint : rel '(' VarList ')' {ConstraintRel $1 $3}
| rel '[' NumList ']' '(' VarList ')' {ConstraintRelEnhanced $1 $3 $6}
| var '=' var {ConstraintEq $1 $3}

NumList : num {[$1]}
| num ',' NumList {$1 : $3}


{ 
parseError :: [Token] -> E a
parseError _ = failE "Parse Error"

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e
  
data Prog = Query [String] [Constraint]
         | ProgLink Prog Prog
         deriving Show

data VarItem = VarItemVar String
  | VarItemBlank
  deriving Show

data Constraint = ConstraintRel String [VarItem]
  | ConstraintRelEnhanced String [Int] [VarItem]
  | ConstraintEq String String
  deriving Show
} 
