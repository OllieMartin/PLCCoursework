myinterpreter: Grammar.hs Interpreter2.hs Tokens.hs
	ghc -o myinterpreter main.hs

Grammar.hs: Grammar.y
	happy -o Grammar.hs Grammar.y

Tokens.hs: Tokens.x
	alex -o Tokens.hs Tokens.x
