myinterpreter: Parser.hs Interpreter.hs Lexer.hs
	ghc -o myinterpreter Main.hs

Parser.hs: Parser.y
	happy -o Parser.hs Parser.y

Lexer.hs: Lexer.x
	alex -o Lexer.hs Lexer.x
