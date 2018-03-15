myinterpreter: Parser.hs Interpreter.hs Lexer.hs
	ghc -o myinterpreter Main.hs

Parser.hs: Grammar.y
	happy -o Parser.hs Grammar.y

Lexer.hs: Tokens.x
	alex -o Lexer.hs Tokens.x
