.PHONY: all install test clean

CC=gcc
CFLAGS=-Wall -pedantic

all: gswrapper closefds

fdcloser.o: fdcloser.c fdcloser.h
	$(CC) $(CFLAGS) -c $< -o $@

gswrapper.o: gswrapper.c gswrapper.h common.h fdcloser.h
	$(CC) $(CFLAGS) -c $< -o $@

closefds.o: closefds.c common.h fdcloser.h
	$(CC) $(CFLAGS) -c $< -o $@

gswrapper: gswrapper.o fdcloser.o
	gcc $(CFLAGS) $^ -o $@

closefds: closefds.o fdcloser.o
	gcc $(CFLAGS) $^ -o $@

install: all
	cp gswrapper /usr/local/bin/gswrapper
	chown root:root /usr/local/bin/gswrapper
	chmod +s /usr/local/bin/gswrapper
	cp closefds /usr/local/bin/closefds
	chown root:root /usr/local/bin/gswrapper

test:
	./tests/test.sh

clean:
	rm -rf gswrapper closefds *.o
