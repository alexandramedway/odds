# Odds: Makefile
#  - main entrypoint for building compiler and running tests

default: build

all: clean build

build:
	cd compiler; make

test: build
	cd test; make

.PHONY: clean
clean:
	cd compiler; make clean
	cd test; make clean
	cd test; cd compiler; make clean
