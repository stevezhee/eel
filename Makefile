.PHONY: test all lint doc clean

all : test # doc lint
	# stack install

lint :
	hlint app src test

doc :
	stack haddock

test :
	stack test

clean :
	stack clean
