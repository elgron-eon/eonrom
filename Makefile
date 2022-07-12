all: eonrom.bin

eonrom.hex: rom.asm
	eonasm -u -l $@ $^ > eonrom.lst

eonrom.bin: eonrom.hex
	binfmt $^ ihex $@ bin && truncate -s 4096 $@
