all: ast.sml tail.lex tail.yacc load-tail.sml loader.mlb
	mllex tail.lex
	ml-yacc tail.yacc
	mlton -output transform loader.mlb
clean:
	rm tail.lex.sml
	rm tail.yacc.desc
	rm tail.yacc.sig
	rm tail.yacc.sml
	rm transform