bits 16
org 0x600
VIDEO_MODE_40x25_16  equ 0x00  ; 8 pages
VIDEO_MODE_80x25_16  equ 0x03  ; 8 pages
VIDEO_MODE_40x25_256 equ 0x13  ; 1 page, 320x200 pixels
VIDEO_WIDTH  equ 80
VIDEO_HEIGHT equ 25
VIDEO_LAST_COL equ VIDEO_WIDTH - 1
VIDEO_LAST_ROW equ VIDEO_HEIGHT - 1
WRITE_STR_MODE_COLOR        equ 0
WRITE_STR_MODE_COLOR_CURSOR equ 1
WRITE_STR_MODE_ATTR         equ 2
WRITE_STR_MODE_ATTR_CURSOR  equ 3
SCROLL_MODE_UP   equ 0x06
SCROLL_MODE_DOWN equ 0x07
COLOR_BLACK         equ 0
COLOR_BLUE          equ 1
COLOR_GREEN         equ 2
COLOR_CYAN          equ 3
COLOR_RED           equ 4
COLOR_MAGENTA       equ 5
COLOR_BROWN         equ 6
COLOR_LIGHT_GRAY    equ 7
COLOR_DARK_GRAY     equ 8
COLOR_LIGHT_BLUE    equ 9
COLOR_LIGHT_GREEN   equ 10
COLOR_LIGHT_CYAN    equ 11
COLOR_LIGHT_RED     equ 12
COLOR_LIGHT_MAGENTA equ 13
COLOR_YELLOW        equ 14
COLOR_WHITE         equ 15
start:
  mov al, VIDEO_MODE_80x25_16
  ;mov al, VIDEO_MODE_40x25_256
  call set_video_mode

  mov bl, 0x5b
  call clear_page

  mov ch, 3  ; row start
  mov cl, 5  ; row end  (invisible if end < start)
  call set_cursor_shape

  mov bp, welcome_msg
  call strlen
  mov al, WRITE_STR_MODE_COLOR_CURSOR
  mov bl, 0x0f
  mov bh, 0
  mov dh, 0
  mov dl, 0
  call write_str_mode
  call newline
loop:
  mov bp, prompt_msg
  call strlen
  call get_cursor
  mov al, WRITE_STR_MODE_COLOR_CURSOR
  mov bl, 0x0f
  call write_str_mode

  call read_word_echo
  call newline
  mov bp, word_buffer
  call strlen
  call get_cursor
  mov al, WRITE_STR_MODE_COLOR_CURSOR
  mov bl, 0x0f
  call write_str_mode
  call newline

  mov ax, cx
  add al, 48
  call write_ch_cursor_color
  call newline

  jmp loop
exit:
  ;mov ax, 0x4f02
  ;mov bx, 256
  ;int 0x10

  ;mov ah, 0x0c
  ;mov bh, 0
  ;mov al, 0x3f
  ;;mov al, 0x22
  ;;mov al, 0x33
  ;;mov al, 0x55
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

  call read_key
  mov bl, 0x3e
  call clear_page
  cli
  hlt

read_word_echo:
  push di
  mov di, word_buffer
  read_word_echo_start_loop:
    call read_key
    call write_ch_cursor
    cmp al, 32
    jbe read_word_echo_start_loop
  read_word_echo_loop:
    mov [di], al
    call read_key
    call write_ch_cursor
    inc di
    cmp al, 32
    ja read_word_echo_loop
  mov [di], 0
  pop di
  ret

newline:
  push ax
  mov al, 13
  call write_ch_cursor
  mov al, 10
  call write_ch_cursor
  pop ax
  ret
write_ch_cursor:       ; al=ascii          bh=page
  push ax
  mov ah, 0x0e
  int 0x10
  pop ax
  ret
write_ch_cursor_color: ; al=ascii          bh=page bl=color
  cmp al, 32
  jae write_ch_cursor_color_main
  call write_ch_cursor
  ret
  write_ch_cursor_color_main:
    call write_ch_color
    push dx
    call get_cursor
    inc dl
    call set_cursor
    pop dx
    ret
write_ch:              ; al=ascii          bh=page
  push cx
  mov cx, 1
  call write_ch_repeat
  pop cx
  ret
write_ch_color:        ; al=ascii          bh=page bl=color
  push cx
  mov cx, 1
  call write_ch_repeat_color
  pop cx
  ret
write_ch_repeat:       ; al=ascii cx=count bh=page
  push ax
  mov ah, 0x0a
  int 0x10
  pop ax
  ret
write_ch_repeat_color: ; al=ascii cx=count bh=page bl=color
  push ax
  mov ah, 0x09
  int 0x10
  pop ax
  ret
write_str_mode: ; al=mode bp=string cx=length dh=row dl=col bh=page bl=color
  push ax
  mov ah, 0x13
  int 0x10
  pop ax
  ret
get_cursor: ; bh=page dh:=row dl:=col
  push ax
  push cx
  mov ah, 0x03  ; cx:=shape-row-start-end, which we throw away
  int 0x10
  pop cx
  pop ax
  ret
set_cursor: ; bh=page dh=row dl=col
  push ax
  mov ah, 0x02  ; int 0x10 set cursor pos
  int 0x10
  pop ax
  ret
set_cursor_shape: ; ch=row-start cl=row-end
  push ax
  mov ah, 0x01
  int 0x10
  pop ax
  ret
clear_page:                   ; bl=color
  push ax
  mov ah, SCROLL_MODE_UP
  mov al, 0
  call scroll_page
  pop ax
  ret
scroll_page: ; ah=mode al=lines bl=color
  push cx
  push dx
  mov ch, 0
  mov cl, 0
  mov dh, VIDEO_LAST_ROW
  mov dl, VIDEO_LAST_COL
  call scroll_rect
  pop dx
  pop cx
  ret
clear_rect:                   ; ch=top cl=left dh=bottom dl=right bl=color
  push ax
  mov ah, SCROLL_MODE_UP
  mov al, 0
  call scroll_rect
  pop ax
  ret
scroll_rect: ; ah=mode al=lines ch=top cl=left dh=bottom dl=right bl=color
  push bx
  mov bh, bl
  int 0x10
  pop bx
  ret
write_pixel: ; al=color bh=page cx=x dx=y
  push ax
  mov ah, 0x0c
  int 0x10
  pop ax
  ret
get_video_page: ; bh:=page
  push si
  mov si, video_page
  mov bh, [si]
  pop si
  ret
set_video_page: ; bh=page
  push ax
  push di
  mov di, video_page
  mov [di], bh
  pop di
  mov ah, 0x05
  mov al, bh
  int 0x10
  pop ax
  ret
set_video_mode: ; al=mode
  push ax
  mov ah, 0
  int 0x10
  pop ax
  ret
read_key: ; ah:=scan al:=ascii
  mov ah, 0
  int 0x16
  ret
strlen: ; bp=null-terminated-string cx:=length
  push ax
  push si
  mov si, bp
  dec si
  strlen_loop:
    inc si
    mov al, [si]  ; 16-bit x86 does not support [cx], only bx, bp, si, di
    test al, al
    jnz strlen_loop
  sub si, bp
  mov cx, si
  pop si
  pop ax
  ret
welcome_msg db "Welcome", 0
;welcome_msg db "Welcome", 10, "one", 13, 10, "two", 8, 10, "three", 0
prompt_msg db "> ", 0
video_page db 0
times 1024 -( $ - $$ ) db 0  ; make sure we fill up 2 sectors
word_buffer:
