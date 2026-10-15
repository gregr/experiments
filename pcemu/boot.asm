bits 16
org 0x7c00
  jmp zero_cs
times 0x3e -( $ - $$ ) db 0  ; FAT12/BPB padding that a bad BIOS may overwrite
zero_cs:
  jmp 0:start
start:
  mov ax, 0
  mov ds, ax
  mov es, ax
  cli            ; prevent ss/sp initialization race in a bad 8088
  mov ss, ax
  mov sp, ax     ; intended to wrap around to 0xffff
  sti
  cld
  mov ax, 0x0003 ; int 0x10 video mode 80x25
  int 0x10
  mov di, 5
disk_read:
  mov ah, 0x02   ; int 0x13 disk read
  mov bx, 0x600  ; destination
  mov dh, 0      ; head
  mov ch, 0      ; cylinder
  mov cl, 2      ; sector
  mov al, 2      ; number of sectors (it's an error if disk has fewer sectors)
  ; dl=drive was automatically set by the BIOS
  int 0x13
  jnc finish     ; CF=1 if the disk read failed
  dec di
  jz error
  mov ah, 0      ; int 0x13 reset disk system
  int 0x13
  jmp disk_read
finish:
  jmp bx         ; jump to the loaded code
error:
  mov bp, disk_read_error_msg
  mov bl, 0x0c   ; red
  mov dx, 0
  call print_str
  mov bp, bx     ; examine what was read from disk
  mov cx, 1024   ; 2 sectors worth
  mov bl, 0x3e   ; yellow on cyan
  mov dl, 0
  mov dh, 1
  call print_str_n
  cli
  hlt
print_str_n:  ; cx=string length, bl=color, dl=col, dh=row
  push ax
  push bx
  mov ah, 0x13  ; int 0x10 write string
  mov al, 0     ; don't move cursor
  mov bh, 0     ; page
  int 0x10
  pop bx
  pop ax
  ret
print_str:  ; bp=null-terminated string, bl=color, dl=col, dh=row
  push cx
  push si
  mov si, bp
  dec si
strlen:
  inc si
  mov al, [si]  ; 16-bit x86 does not support [cx], only bx, bp, si, di
  test al, al
  jnz strlen
  sub si, bp
  mov cx, si
  call print_str_n
  pop si
  pop cx
  ret
disk_read_error_msg db "Disk read failed.  Contents read:", 0
times 510 -( $ - $$ ) db 0
db 0x55, 0xaa
