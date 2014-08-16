.PHONY: default
default: all

ATDGEN_SOURCES = src/serial.atd
ATDGEN_FLAGS := -j-std

-include Atdgen.mk

SOURCES := \
	src/main.ml src/gallery.ml src/plain.ml src/log.ml src/serial_t.mli src/serial_t.ml src/serial_j.mli src/serial_j.ml src/file.ml

DOC_FILES := \
	src/main.mli

RESULT := main

THREADS := "yes"

INCDIRS :=
EXTLIBDIRS :=
CLIBS :=
LIBS :=
CC := gcc
PACKS := core atdgen cryptokit
OCAMLFLAGS := -g -w +a-4-6-7-9-27-29-32..39-41..42-44-45-8
# default warning flags are -w +a-4-6-7-9-27-29-32..39-41..42-44-45
OCAMLLDFLAGS := -g
ANNOTATE := yes

api: all htdoc
	cp -rf doc/symdroid/html/ docs/api/
	rm -rf doc

define PROJ_logappend
	SOURCES = $(SOURCES) src/logappend.ml
	RESULT = logappend
endef
export PROJ_logappend

define PROJ_logread
	SOURCES = $(SOURCES) src/logread.ml
	RESULT = logread
endef
export PROJ_logread

ifndef SUBPROJS
		export SUBPROJS = logappend logread
endif

include OCamlMakefile

cleanall: clean
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=clean

.PHONY: sources opt all
sources: $(SOURCES)
nc: sources
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=native-code
bc: sources
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=byte-code
all: sources
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=byte-code
