{ 
module Grammar where 
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

%% 
Prog : StrictVarList '{' ConstraintList '}'  {Query $1 $3}
| StrictVarList '{' ConstraintList '}' Prog {ProgLink (Query $1 $3) $5}

VarList : VarItem {VarListSingleton $1}
| VarItem ',' VarList {VarListLink $1 $3}

StrictVarList : var {StrictVarListSingleton $1}
| var ',' StrictVarList {StrictVarListLink $1 $3}

VarItem : var {VarItemVar $1}
| '_' {VarItemBlank}

ConstraintList : Constraint ';' {ConstraintListSingleton $1}
| Constraint ';' ConstraintList {ConstraintListLink $1 $3}

Constraint : rel '(' VarList ')' {ConstraintRel $1 $3}
| rel '[' NumList ']' '(' VarList ')' {ConstraintRelEnhanced $1 $3 $6}
| var '=' var {ConstraintEq $1 $3}

NumList : num {NumListSingleton $1}
| num ',' NumList {NumListLink $1 $3}


{ 
parseError :: [Token] -> a
parseError _ = error "Parse error"
  
data Prog = Query StrictVarList ConstraintList
         | ProgLink Prog Prog
         deriving Show

data VarList = VarListSingleton VarItem
  | VarListLink VarItem VarList
  deriving Show
  
data StrictVarList = StrictVarListSingleton String
  | StrictVarListLink String StrictVarList
  deriving Show

data VarItem = VarItemVar String
  | VarItemBlank
  deriving Show

data ConstraintList = ConstraintListSingleton Constraint
  | ConstraintListLink Constraint ConstraintList
  deriving Show

data Constraint = ConstraintRel String VarList
  | ConstraintRelEnhanced String NumList VarList
  | ConstraintEq String String
  deriving Show

data NumList = NumListSingleton Int
  | NumListLink Int NumList
  deriving Show
} 
