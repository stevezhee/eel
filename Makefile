.PHONY: test all lint doc clean t.exe

UNAME = $(shell uname)

ifeq ($(UNAME), Linux)
CFLAGS = 
else
CFLAGS = -lcygwin -lSDL2main # order of cflags matters
endif

all : t.exe # test
  # cat t.ll
	./t.exe

t.ll : src/Eel.hs test/Spec.hs
	stack runghc test/Spec.hs

%.s : %.ll
	llc -fatal-assembler-warnings $<

t.exe : t.s eel.c
	clang -I/usr/include/SDL2 -o $@ $^ ${CFLAGS} -lSDL2

#all : test doc # lint
	# stack install
	# ./t.exe
	# ./t.exe

lint :
	hlint app src test

doc :
	stack haddock

test :
	stack test

clean :
	stack clean
