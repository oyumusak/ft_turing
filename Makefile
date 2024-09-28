# Program adı
PROGRAM = main

# Kaynak dosyalar
SRC = types.ml utils.ml machineone.ml machinetwo.ml main.ml

# Gerekli kütüphaneler
LIBS = yojson

# Derleme komutları
OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamlfind ocamldep

# Kütüphane dosyaları
OCAMLLIBS = -package yojson -linkpkg
OCAMLOPTLIBS = -package yojson -linkpkg

# Derlenen bytecode ve native kod dosyaları
OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

# OPAM kontrol fonksiyonu

.PHONY: check-tools
check-tools:
	@echo "Checking for ocamlfind..."
	@if ! command -v ocamlfind &> /dev/null; then \
		echo "ocamlfind is missing. Installing..."; \
		opam install ocamlfind; \
	else \
		echo "ocamlfind is already installed."; \
	fi
	@echo "Installing yojson..."
	opam install yojson || echo "yojson installation failed. Continuing without it..."



# OPAM kurulumu kontrol ve yükleme fonksiyonu
.PHONY: check-libs
check-libs: check-tools
	@echo "Checking for missing libraries..."
	@for lib in $(LIBS); do \
		if ! opam list --installed $$lib > /dev/null 2>&1; then \
			echo "$$lib is missing. Installing..."; \
			opam install $$lib; \
		else \
			echo "$$lib is already installed."; \
		fi \
	done

# .ml dosyalarını ayrı ayrı derleme kuralı
%.cmo: %.ml
	$(OCAMLC) $(OCAMLLIBS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTLIBS) -c $<

# Bytecode ile çalıştırılabilir dosya oluşturma
bytecode: $(OBJS)
	$(OCAMLC) $(OCAMLLIBS) -o $(PROGRAM).byte $(OBJS)

# Native kod ile çalıştırılabilir dosya oluşturma
native: $(OPTOBJS)
	$(OCAMLOPT) $(OCAMLOPTLIBS) -o $(PROGRAM) $(OPTOBJS)

# Derleme komutları
all: check-libs bytecode native

# Bağımlılıkların otomatik oluşturulması
depend:
	$(OCAMLDEP) $(SRC) > .depend

# Temizlik
clean:
	rm -f *.cm[iox] *.o $(PROGRAM) $(PROGRAM).byte .depend

# Bağımlılıkları include etme
-include .depend
