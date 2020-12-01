;* xwjoy.asm - A simple memory-resident program that simulates keyboard
;* input when throttle movement occurs.  After installing, it terminates
;* through the Terminate-and-Stay-Resident function (function 31h).

.MODEL tiny
.486P
.STACK

.CODE

.STARTUP

PORT_JOYSTICK EQU 0201h

DOS_PRINTSTR  EQU 09h
DOS_TSR       EQU 031h
DOS_ALLOCMEM  EQU 048h
DOS_FREEMEM   EQU 049h


    jmp Install ; Jump over data and resident code

; Data must be in code segment so it won't be thrown away with Install code.

Down_msg BYTE 'Move dial to down position, and press button.$'
Up_msg BYTE 'Move dial to up position, and press button.$'
Crlf_msg BYTE 0dh,0ah,'$'

Range_Bottom WORD ?
Range_25 WORD ?
Range_50 WORD ?
Range_75 WORD ?

ISRInstalled BYTE 00h

OldISR WORD 0000h
OldISRSeg WORD 0000h
Var_Mystery BYTE 08h,0eh,05dh,01bh,05bh,01ah,05ch,02bh


Proc0175 PROC
    push dx
    mov dx, PORT_JOYSTICK
    xor cx, cx
    out dx, al ; start read
Proc0175_loop:
    in al, dx
    and al, 08h
    or al, al
    jz Proc0175_ret
    loop Proc0175_loop
Proc0175_ret:
    neg cx
    pop dx
    ret
Proc0175 ENDP


Proc0189 PROC
    mov dx, PORT_JOYSTICK
Proc0189_loop:
    in al, dx
    and al, 30h
    cmp al, 30h
    jnz Proc0189_ret
    jmp Proc0189_loop
Proc0189_ret:
    ret
Proc0189 ENDP

Proc0196 PROC
    mov dx, PORT_JOYSTICK
Proc0196_loop:
    in al, dx
    and al, 30h
    cmp al, 30h
    jz Proc0196_ret
    jmp Proc0196_loop
Proc0196_ret:
    ret
Proc0196 ENDP


IntHdlr PROC FAR
    push ax
    push bx
    push cx
    push dx
    push di
    pushf
    call Proc0175
    call Proc01de
    cmp al,cs:[ISRInstalled]
    jz IntHdlr_exit

    mov bl, al
    shl al, 1
    xor ah, ah
    add ax, offset Var_Mystery
    mov di, ax
    mov cx, cs:[di]
    mov ah, 05h
    int 16h
    or al, al
    jnz IntHdlr_exit

    mov cs:[ISRInstalled], bl

IntHdlr_exit:
    popf
    pop di
    pop dx
    pop cx
    pop bx
    pop ax

IntHdlr__callprev:
    pushf

    ;call far ptr cs:OldISR
    byte 2eh,0ffh,1eh
    word offset OldISR

    iret
IntHdlr ENDP


Proc01de PROC
    call Proc0175
    xor al, al

    cmp cx, cs:[Range_25]
    jl Proc01de_ret
    inc al

    cmp cx, cs:[Range_50]
    jl Proc01de_ret
    inc al

    cmp cx, cs:[Range_75]
    jl Proc01de_ret
    inc al

Proc01de_ret:
    ret
Proc01de ENDP

;* Install
;* This procedure marks the end of the TSR's resident section and the
;* beginning of the installation section.  When xwjoy terminates through
;* function 31h, the above code and data remain resident in memory.  The
;* memory occupied by the following code is returned to DOS.


Install PROC
    mov ah, DOS_PRINTSTR
    mov dx, 0103h ; dx = "Move dial to down..."
    int 21h
    mov dx, 015dh ; 5d = "\r\n"
    int 21h

    call Proc0189
    call Proc0175

    mov cs:[Range_Bottom], cx

    mov dx, 0131h ; "Move dial to up..."
    int 21h
    mov dx, 015dh ; 5d = "\r\n"
    int 21h

    call Proc0196

    call Proc0189
    call Proc0175

    mov ax, cs:[Range_Bottom]
    sub ax, cx
    shr ax, 1 ; divide difference between top and bottom by four
    shr ax, 1
    add cx, ax
    mov cs:[Range_25], cx
    add cx, ax
    mov cs:[Range_50], cx
    add cx, ax
    mov cs:[Range_75], cx
    call Proc01de

    mov cs:[ISRInstalled], al

    ; save old interrupt vector
    mov ax, 3508h ; 35 = get interrupt vector, vector = 08
    int 21h ; ES:BX -> current interrupt handler
    mov cs:[OldISR], bx
    mov cs:[OldISRSeg], es

    ; set new interrupt vector
    mov dx, IntHdlr ; 01a3 = interrupt handler
    mov ax, 2508h ; 25 = set interrupt vector, vector = 08
    int 21h ; DS:DX -> new interrupt handler

FreeAddr EQU 002ch
    mov ax, word ptr ds:[FreeAddr]
    mov es, ax
    mov ah, DOS_FREEMEM
    int 21h

    mov dx, 30h ; 30 paragraphs long (0x300h bytes?)
    mov ax, 3100h
    int 21h
Install ENDP

END
