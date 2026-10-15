bits 16
org 0x600
start:
  mov ax, 0x0003 ; int 0x10 video mode 80x25
  int 0x10
  mov bh, 0x5b
  call set_video_color
  call clear_page
  mov ch, 3  ; row start
  mov cl, 5  ; row end  (invisible if end < start)
  call set_cursor_shape
  ;mov al, 1
  ;call set_video_page
  mov bh, 0
  mov dl, 5
  mov dh, 10
  call set_cursor
  ;mov al, 0
  ;call set_video_page
  mov bh, 0x0f
  call set_video_color
  mov bp, welcome_msg
  mov dx, 0
  call print_str
  mov bh, 0x5b
  call set_video_color
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
  mov bh, 0x3e
  call set_video_color
  call clear_page
  cli
  hlt
read_word:  ; dh=echo-row, cx:=length
  push ax
  push bx
  push bp
  push di
  mov bh, 0x2f
  call set_video_color
  mov bp, prompt_msg
  mov dl, 0     ; col
  call print_str
  mov bh, 0x5b
  call set_video_color
  mov bp, word_buffer
  mov di, bp
  dec di

  mov bh, 0x4d
  call set_video_color

  read_word_keys:
    inc di
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
print_str:  ; bp=null-terminated string, dl=col, dh=row
  push cx
  push si
  call strlen
  mov cx, si
  call print_str_n
  pop si
  pop cx
  ret
strlen:  ; bp=null-terminated string, si:=length
  push ax
  mov si, bp
  dec si
  strlen_loop:
    inc si
    mov al, [si]  ; 16-bit x86 does not support [cx], only bx, bp, si, di
    test al, al
    jnz strlen_loop
  sub si, bp
  pop ax
  ret
print_str_n:  ; bp=string, cx=string length, dl=col, dh=row
  test cx, cx
  jz print_str_n_ret
  call set_cursor
  push bp
  push cx
  print_str_n_loop:
    mov al, [bp]
    call print_ch_color
    inc bp
    dec cx
    jnz print_str_n_loop
  pop cx
  pop bp
  print_str_n_ret:
    ret
print_ch:  ; al=ascii
  push bx
  call get_video_page
  mov ah, 0x0e
  int 0x10
  pop bx
  ret
print_ch_color:  ; al=ascii
  push bx
  push cx
  call get_video_color
  mov bl, bh
  call get_cursor
  call get_video_page
  mov cx, 1
  mov ah, 0x09
  int 0x10
  inc dl
  call set_cursor
  pop cx
  pop bx
  ret
read_key:  ; ah:=scan, al:=ascii
  mov ah, 0
  int 0x16
  ret
get_cursor:  ; dh:=row, dl:=col, cx:=row-start-end
  push ax
  push bx
  call get_video_page
  mov ah, 0x03
  int 0x10
  pop bx
  pop ax
  ret
set_cursor:  ; dh=row, dl=col
  push ax
  push bx
  call get_video_page
  mov ah, 0x02  ; int 0x10 set cursor pos
  int 0x10
  pop bx
  pop ax
  ret
set_cursor_shape:  ; ch=row-start, cl=row-end
  push ax
  mov ah, 0x01
  int 0x10
  pop ax
  ret
clear_page:
  push ax
  mov al, 0
  call scroll_page_up
  pop ax
  ret
scroll_page_up: ; al=lines
  mov ah, 0x06
  call int10h_page
  ret
scroll_page_down: ; al=lines
  mov ah, 0x07
  call int10h_page
  ret
int10h_page: ; ah=code, al=lines
  push bx
  push cx
  push dx
  push si
  call get_video_color
  mov ch, 0
  mov cl, 0
  mov si, video_height
  mov dh, [si]
  dec dh
  mov si, video_width
  mov dl, [si]
  dec dl
  int 0x10
  pop si
  pop dx
  pop cx
  pop bx
  ret
clear_rect:       ; ch=top, cl=left, dh=bottom, dl=right
  push ax
  push bx
  call get_video_color
  mov al, 0
  call scroll_rect_up
  pop bx
  pop ax
  ret
scroll_rect_up:   ; al=lines, ch=top, cl=left, dh=bottom, dl=right
  push bx
  call get_video_color
  mov ah, 0x06
  int 0x10
  pop bx
  ret
scroll_rect_down: ; al=lines, ch=top, cl=left, dh=bottom, dl=right
  push bx
  call get_video_color
  mov ah, 0x07
  int 0x10
  pop bx
  ret
get_video_color:  ; bh:=color
  push si
  mov si, video_color
  mov bh, [si]
  pop si
  ret
set_video_color:  ; bh=color
  push di
  mov di, video_color
  mov [di], bh
  pop di
  ret
get_video_page:  ; bh:=page
  push si
  mov si, video_page
  mov bh, [si]
  pop si
  ret
set_video_page:  ; al=page
  push di
  mov di, video_page
  mov [di], al
  pop di
  mov ah, 0x05
  int 0x10
  ret
welcome_msg db "Welcome", 0
prompt_msg db "> ", 0
video_color:  db 0x4d
video_page:   db 0
video_height: db 25
video_width:  db 80
times 1024 -( $ - $$ ) db 0  ; make sure we fill up 2 sectors
word_buffer:
