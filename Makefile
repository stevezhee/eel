.PHONY: test all lint doc clean t.exe

UNAME = $(shell uname)

ifneq ($(UNAME), Windows_NT)
CFLAGS = 
else
CFLAGS = -lcygwin -lSDL2main # order of cflags matters
endif

ifeq ($(UNAME), Darwin)
SDL2_PATH = -I/usr/local/include/SDL2
else
SDL2_PATH = -I/usr/include/SDL2
endif

all : t.exe # test
  # cat t.ll
	./t.exe

t.ll : src/Eel.hs test/Spec.hs
	stack runghc test/Spec.hs

%.s : %.ll
	llc $<

t.exe : t.s eel.c
	clang -I/usr/include/SDL2 -I/usr/local/include/SDL2 -o $@ $^ ${CFLAGS} -lSDL2

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
