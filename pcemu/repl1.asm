bits 16
org 0x600
start:
  mov ah, 0x01    ; int 0x10 set cursor shape
  mov ch, 6  ; row start
  mov cl, 5  ; row end  (invisible if end < start)
  int 0x10
  mov bp, welcome_msg
  mov bl, 0x0f  ; white
  mov dx, 0
  call print_str
  mov dh, 1
loop:
  call read_word
  inc dh
  test cx, cx
  jnz loop
exit:
  ;mov ax, 0x4f02
  ;mov bx, 256
  ;int 0x10
  ;mov ah, 0x0c
  ;mov bh, 0
  ;mov al, 0x3f
  ;mov cx, 10
  ;mov dx, 10
  ;int 0x10
  ;mov cx, 11
  ;mov dx, 10
  ;int 0x10
  ;mov cx, 12
  ;mov dx, 10
  ;int 0x10
  ;mov cx, 10
  ;mov dx, 11
  ;int 0x10
  ;mov cx, 11
  ;mov dx, 11
  ;int 0x10
  ;mov cx, 12
  ;mov dx, 11
  ;int 0x10
  ;mov cx, 10
  ;mov dx, 12
  ;int 0x10
  ;mov cx, 11
  ;mov dx, 12
  ;int 0x10
  ;mov cx, 12
  ;mov dx, 12
  ;int 0x10
  cli
  hlt
read_word:  ; dh=echo-row, cx:=length
  push ax
  push bx
  push bp
  push di
  mov bp, prompt_msg
  mov bl, 0x2f  ; white
  mov dl, 0     ; col
  call print_str
  mov bp, word_buffer
  mov di, bp
  dec di
read_word_keys:
  inc di
  mov bl, 0x2f  ; white
  mov dl, 2     ; col
  mov cx, di
  sub cx, word_buffer
  call print_str_n
  call read_key
  mov [di], al
  cmp al, 0x20  ; ascii space
  ja read_word_keys
  pop di
  pop bp
  pop bx
  pop ax
  ret
read_key:
  mov ah, 0  ; int 0x16 read key: ah:=scan, al:=ascii
  int 0x16
  ret
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
welcome_msg db "Welcome", 0
prompt_msg db "> ", 0
times 1024 -( $ - $$ ) db 0  ; make sure we fill up 2 sectors
word_buffer:
