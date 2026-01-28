STACKPATH=$(shell stack path | grep local-install-root | sed 's/local-install-root: //')
DOCPATH=$(shell stack path | grep local-doc-root | sed 's/local-doc-root: //')
LINTING=hlint -i "Use tuple-section"

default:
	stack build

clean:
	stack clean

format:
	hindent --line-length 100 src/lib/*.hs
	hindent --line-length 100 src/lib/*/*.hs
	hindent --line-length 100 src/lib/*/*/*.hs

lint:
	${LINTING} src/lib/*.hs
	${LINTING} src/lib/*/*.hs
	${LINTING} src/libi/*/*/*.hs

doc-gen:
	stack haddock --haddock-internal --only-locals

doc-open:
	@echo "Open ${DOCPATH}/index.html"
	@xdg-open ${DOCPATH}/index.html

