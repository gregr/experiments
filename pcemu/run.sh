set -eufo pipefail

nasm -o boot.img boot.asm
nasm -o repl.img repl.asm
cat boot.img repl.img > disk.img

qemu-system-i386 -hda disk.img
