TOOLS=

STACKPATH=$(shell if [ -d "dist" ]; then echo ""; else stack path | grep local-install-root | sed 's/local-install-root: //'; fi)
BLDTOOL=$(shell if [ -d "dist" ]; then echo "cabal"; else echo "stack"; fi)

default:
	${BLDTOOL} build
	@for i in ${TOOLS}; do if [ -d "dist" ]; then cp ./dist/build/$${i}/$${i} $${i}; else cp ${STACKPATH}/bin/$${i} $${i}; fi; done

install:
	${BLDTOOL} install

test:
	${BLDTOOL} test

doc:
	${BLDTOOL} haddock --open

format:
	stylish-haskell  -c .stylish-haskell.yaml  -i  -r src/

clean:
	${BLDTOOL} clean
	@for i in ${TOOLS}; do rm -f $${i}; done

.PHONY: install test doc format clean
.SILENT:
