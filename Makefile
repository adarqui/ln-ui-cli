build:
	stack build --fast

test:
	stack test --fast

clean:
	stack clean

build-watch:
	stack build --fast --file-watch

test-watch:
	stack test --fast --file-watch

docs:
	cabal haddock --hyperlink-source

ghci:
	stack ghci ln-api
