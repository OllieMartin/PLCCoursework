{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9  
-- numbers 
$alpha = [a-zA-Z]    
-- alphabetic characters
$upper = [A-Z]
-- uppercase alphabetic characters
$lower = [a-z]
-- lowercase alphabetic characters

tokens :-
$white+         ; 
  "//".*        ; 
  "{"           { \p s -> TokenOpenBrace p} 
  "}"           { \p s -> TokenCloseBrace p}
  "["           { \p s -> TokenOpenSqr p} 
  "]"           { \p s -> TokenCloseSqr p}
  \=          { \p s -> TokenEq p}
  \(          { \p s -> TokenLParen p}
  \)          { \p s -> TokenRParen p}
  ";"         { \p s -> TokenSemiColon p}
  ","         { \p s -> TokenComma p}
  "_"         { \p s -> TokenUnderscore p }
  $lower [$alpha $digit \_ \’]*   { \p s -> TokenVar p s } 
  $upper [$alpha $digit \_ \’]*   { \p s -> TokenRel p s }
  [$digit]+                       { \p s -> TokenNum p (read s) }


{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenOpenBrace AlexPosn   |
  TokenCloseBrace AlexPosn  | 
  TokenOpenSqr AlexPosn     |
  TokenCloseSqr AlexPosn    | 
  TokenVar AlexPosn String  |
  TokenRel AlexPosn String  | 
  TokenEq AlexPosn          |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenSemiColon AlexPosn   |
  TokenNum AlexPosn Int     |
  TokenUnderscore AlexPosn  |
  TokenComma AlexPosn
  deriving (Eq,Show) 

}
