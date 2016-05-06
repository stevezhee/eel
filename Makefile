.PHONY: test

all : doc lint test
	stack install

lint :
	hlint app src test

doc :
	stack haddock

test :
	stack test
