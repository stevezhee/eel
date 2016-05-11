.PHONY: test all lint doc clean t.exe

CFLAGS = 

all : t.exe
  # cmd "cat t.ll"
	./t.exe

%.s : %.ll
	llc -fatal-assembler-warnings $<

t.exe : t.s eel.c
  # let cflags = if os == "linux" then "" else "-lcygwin -lSDL2main"
	clang -I/usr/include/SDL2 -o t.exe t.s eel.c ${CFLAGS} -lSDL2
  # order of cflags matters

#all : test doc # lint
	# stack install
	# ./t.exe
	# ./t.exe

lint :
	hlint app src test

doc :
	stack haddock

test :
	rm -f t.exe
	stack test

clean :
	stack clean
