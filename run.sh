#!bin/include
bison -d -v cfg.y
flex lex.l
mv cfg.tab.c cfg.tab.cpp
mv lex.yy.c lex.yy.cpp
g++ cfg.tab.cpp lex.yy.cpp
rm *.*.*
./a.out <input >output.s
cat output.s
rm a.out
