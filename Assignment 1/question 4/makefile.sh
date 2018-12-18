bison -d assign_4.y
flex assign_4.l
g++ lex.yy.c assign_4.tab.c
./a.out