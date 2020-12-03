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

KEYBD_WRITE   EQU 05h


    jmp Install ; Jump over data and resident code

; Data must be in code segment so it won't be thrown away with Install code.

Down_msg BYTE 'Move dial to down position, and press button.$'
Up_msg BYTE 'Move dial to up position, and press button.$'
Crlf_msg BYTE 0dh,0ah,'$'

Range_0   WORD ?
Range_75  WORD ?
Range_50  WORD ?
Range_25 WORD ?

;LastPosition  WORD ?
LastRange     BYTE ?
;LastPosOffset WORD ?

OldISR WORD 0000h
OldISRSeg WORD 0000h
KeyDataTable:
KeyData_100   BYTE 08h,0eh ; backspace (100%)
KeyData_66    BYTE 5dh,1bh ; ']' (66%)
KeyData_33    BYTE 5bh,1ah ; '[' (33%)
KeyData_0     BYTE 5ch,2bh ; '\' (0%)
;KeyData_Plus  BYTE 3dh,0dh ; '+'
;KeyData_Minus BYTE 2dh,0ch ; '-'


JoystickReadPosition PROC USES dx
    mov dx, PORT_JOYSTICK
    xor cx, cx
    out dx, al ; start read
ReadJS:
    in al, dx
    and al, 08h
    .IF al
        loop ReadJS
    .ENDIF
    neg cx
    ret
JoystickReadPosition ENDP


JoystickWaitButton PROC
    mov dx, PORT_JOYSTICK
JoystickWaitButton_loop:
    in al, dx
    and al, 30h
    cmp al, 30h
    .IF zero?
        jmp JoystickWaitButton_loop
    .ENDIF
    ret
JoystickWaitButton ENDP


JoystickWaitNotButton PROC
    mov dx, PORT_JOYSTICK
JoystickWaitNotButton_loop:
    in al, dx
    and al, 30h
    cmp al, 30h
    .IF !zero?
        jmp JoystickWaitNotButton_loop
    .ENDIF
    ret
JoystickWaitNotButton ENDP


IntHdlr PROC FAR
    push ax
    push bx
    push cx
    push dx
    push di
    pushf
    ; original did a seemingly-useless "call JoystickReadPosition", here, as ReadBand() does one for us
    call ReadBand

    .IF al != cs:[LastRange]
        mov bl, al ; save range before converting to keycodes

        shl al, 1 ; di = KeyDataTable[range * 2]
        xor ah, ah
        add ax, offset KeyDataTable
        mov di, ax

        mov cx, cs:[di]
        mov ah, KEYBD_WRITE
        int 16h

        .IF !al ; if successfully wrote to keyboard
            mov cs:[LastRange], bl
        .ENDIF
    .ENDIF

    popf
    pop di
    pop dx
    pop cx
    pop bx
    pop ax

IntHdlr_callprev:
    pushf

    ;call far ptr cs:OldISR
    byte 2eh,0ffh,1eh
    word offset OldISR

    iret
IntHdlr ENDP


;;;;;;;;;;;;;;;;;;;;
; ReadBand() returns band in al, and total offset in cx
;;;;;;;;;;;;;;;;;;;;
ReadBand PROC
    call JoystickReadPosition
    xor al, al

    cmp cx, cs:[Range_75]
    jl ReadBand_ret
    inc al

    cmp cx, cs:[Range_50]
    jl ReadBand_ret
    inc al

    cmp cx, cs:[Range_25]
    jl ReadBand_ret
    inc al

ReadBand_ret:
    ret
ReadBand ENDP


;* Install
;* This procedure marks the end of the TSR's resident section and the
;* beginning of the installation section.  When xwjoy terminates through
;* function 31h, the above code and data remain resident in memory.  The
;* memory occupied by the following code is returned to DOS.


Install PROC
    mov ah, DOS_PRINTSTR
    mov dx, offset Down_msg
    int 21h
    mov dx, offset Crlf_msg
    int 21h

    call JoystickWaitButton
    call JoystickReadPosition

    mov cs:[Range_0], cx

    mov dx, offset Up_msg
    int 21h
    mov dx, offset Crlf_msg
    int 21h

    call JoystickWaitNotButton

    call JoystickWaitButton
    call JoystickReadPosition

    mov ax, cs:[Range_0]
    sub ax, cx
    shr ax, 1 ; divide difference between top and bottom by four
    shr ax, 1
    add cx, ax
    mov cs:[Range_75], cx
    add cx, ax
    mov cs:[Range_50], cx
    add cx, ax
    mov cs:[Range_25], cx
    call ReadBand

    ;mov cs:[LastPosition], cx
    mov cs:[LastRange], al

    ; save old interrupt vector
    mov ax, 3508h ; 35 = get interrupt vector, vector = 08 (clock)
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

ResidentLen EQU 30h ; 30 paragraphs long (0x300h bytes?)
    mov dx, ResidentLen
    mov ax, 3100h
    int 21h
Install ENDP

END
