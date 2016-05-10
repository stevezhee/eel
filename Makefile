.PHONY: test all lint doc clean

all : test doc # lint
	# stack install
	# ./t.exe
	./t.exe

lint :
	hlint app src test

doc :
	stack haddock

test :
	rm -f t.exe
	stack test

clean :
	stack clean
