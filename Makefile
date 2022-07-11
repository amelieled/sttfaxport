DUNE = dune

all: lib

lib:
	${DUNE} build lib/

tests:
	${DUNE} runtest

.PHONY: all lib tests
