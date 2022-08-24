all: eonrom.bin

eonrom.hex: rom.asm jforth.asm dict.asm string.asm
	eonasm -u -l $@ $^ > eonrom.lst && tail -1 eonrom.lst

eonrom.bin: eonrom.hex
	binfmt $^ ihex $@ bin && truncate -s 8192 $@

clean:
	rm -f eonrom.hex eonrom.bin eonrom.lst
