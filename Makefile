PREFIX=/usr/local
TARGET=puzq
BINDIR=$(PREFIX)/bin


.PHONY: build install uninstall clean

build: src
	-mkdir _build
	cp src/lex.mll _build
	cp src/parse.mly _build
	cp -r src/*.ml src/*.mli src/lib _build
	cd _build && ocamllex lex.mll
	cd _build && menhir parse.mly
	cd _build && ocamlfind ocamlopt  \
		-I lib                         \
		-o puzq                        \
		-linkpkg -package str          \
		range.ml                       \
		tmType.ml                      \
		parse.mli parse.ml             \
		lex.ml                         \
		puzzleType.ml                  \
		msg.ml                         \
		lib/yajirin.mli lib/yajirin.ml \
		lib/puzzleLib.ml               \
		optionState.mli optionState.ml \
		main.ml
	cp _build/puzq ./

install:
	mkdir -p $(BINDIR)
	install $(TARGET) $(BINDIR)

uninstall:
	rm -rf $(BINDIR)/$(TARGET)




clean:
	@rm -rf *.cmi *.cmx *.cmo *.o *.out _build puzq