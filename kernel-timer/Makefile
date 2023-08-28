CC = gcc
OBJS = lib/timer.o lib/LPerm.o lib/Phi.o lib/Gamma.o lib/NTLib.o lib/Util.o lib/Monty.o lib/Multiply.o lib/Generic.o
H_FILES = src/timer.h src/LPerm.h src/Phi.h src/Gamma.h src/Util.h src/NTLib.h src/Mutliply.h
FLAGS = -O3 -fopenmp
SOURCES = $(wildcard src/gen/*.c) # match anything that looks like gen/*.c
TARGETS = $(patsubst src/gen%.c,bin%,$(SOURCES))

all: $(TARGETS) 

$(TARGETS): $(OBJS) $(H_FILE)
	$(CC) $(FLAGS) -o $@  $^  $(patsubst bin%,src/gen%.c,$@)

lib/timer.o: src/timer.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/Phi.o: src/Phi.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/Gamma.o: src/Gamma.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/LPerm.o: src/LPerm.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/Util.o: src/Util.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/NTLib.o: src/NTLib.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/Monty.o: src/Monty.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/Multiply.o: src/Multiply.c
	$(CC) $(FLAGS) -o $@ -c $^
lib/Generic.o: src/Generic.c
	$(CC) $(FLAGS) -o $@ -c $^




clean:
	rm bin/*

super-clean:
	rm lib/*
