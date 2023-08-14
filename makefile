argPrinter: argPrinter.o
	gcc -m32 argPrinter.o -o argPrinter

argPrinter.o: argPrinter.s	
	nasm -f elf32 argPrinter.s -o argPrinter.o

multi: multi.o
	gcc -m32 multi.o -o multi

multi.o: multi.s
	nasm -f elf32 multi.s -o multi.o