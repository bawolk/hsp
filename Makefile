# hsp package makefile

default: build

build:
	stack build --ghc-options="-O2"

install:
	./install.sh

dev:
	stack build --fast


