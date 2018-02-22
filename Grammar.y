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
Exp : let var '=' Exp in Exp { Let $2 $4 $6 } 
    | Exp '+' Exp            { Plus $1 $3 } 
    | Exp '-' Exp            { Minus $1 $3 } 
    | Exp '*' Exp            { Times $1 $3 } 
    | Exp '/' Exp            { Div $1 $3 } 
    | '(' Exp ')'            { $2 } 
    | '-' Exp %prec NEG      { Negate $2 } 
    | var                    { Var $1 } 
    | num                    { Num $1 }
    | rel 
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 
data Prog = Num Int 
         | Var String 
         | 
         deriving Show 
} 