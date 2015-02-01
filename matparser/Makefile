all:
	flex lex_input.c
	yacc -d yacc_input.c
	gcc -c lex.yy.c
	gcc -c y.tab.c
	g++ -c MatElements.cpp
	g++ -o test lex.yy.o y.tab.o MatElements.o
clean:
	rm -f lex.yy.c
	rm -f lex.yy.h
	rm -f y.tab.c
	rm -f y.tab.h
	rm -f *.o
	rm -f test
