# Not warning about unused do bind because of how the lineInOut hack works
GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -fno-warn-unused-do-bind -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all clean doc install shell

all: report.html doc dist/build/smtp2mta/smtp2mta dist/smtp2mta-$(VERSION).tar.gz

install: dist/build/smtp2mta/smtp2mta
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: smtp2mta.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/smtp2mta/index.html README

README: smtp2mta.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/smtp2mta/index.html: dist/setup-config smtp2mta.hs
	cabal haddock --hyperlink-source --executables

dist/setup-config: smtp2mta.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/smtp2mta/smtp2mta: smtp2mta.cabal dist/setup-config smtp2mta.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/smtp2mta-$(VERSION).tar.gz: smtp2mta.cabal dist/setup-config smtp2mta.hs README
	cabal check
	cabal sdist
