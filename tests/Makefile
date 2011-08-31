
PKG = run_test

$(PKG): Main.uo Makefile
	$(LINK) $@ Main.uo

Main.uo: Main.sml Parser.ui Lexer.ui
	$(COMPILE) $@ Parser.ui Lexer.ui Main.sml

Lexer.ui Lexer.uo: Lexer.sml
	$(COMPILE) Lexer.uo Lexer.sml

Parser.ui Parser.uo: Parser.sig Parser.sml
	$(COMPILE) Parser.ui Parser.sig
	$(COMPILE) Parser.uo Parser.sml

Parser.sig Parser.sml: Parser.grm Makefile
	$(YACC) Parser.grm

Lexer.sml: Lexer.lex Makefile
	$(LEX) Lexer.lex

clean:
	make clean-common
	$(RM) $(PKG) Lexer.sml Parser.sig Parser.sml

include ../Makefile.common

# vim: tabstop=8 shiftwidth=8 noexpandtab
