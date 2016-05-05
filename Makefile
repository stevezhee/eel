.PHONY: test

all : doc lint
	stack install

lint :
	hlint app src test

doc :
	stack haddock

test :
	stack test
