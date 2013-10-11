all: loader/load.img kernel/kernel.bin


loader/load.img: loader/loader.asm
	nasm loader/loader.asm -o loader/load.img -l loader/load.lst
	rawrite2 -f loader/load.img -d A -n

kernel/kernel.bin: kernel/kernel.asm
	nasm kernel/kernel.asm -o kernel/kernel.bin -l kernel/kernel.lst
	copy.bat

write:
	rawrite2 -f loader/load.img -d A -n
	copy.bat


