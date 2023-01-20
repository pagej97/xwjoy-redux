;* xwjoy.asm - A simple memory-resident program that simulates keyboard
;* input when throttle movement occurs.  After installing, it terminates
;* through the Terminate-and-Stay-Resident function (function 31h).

.MODEL tiny
.486P
.STACK

.CODE

.STARTUP

PORT_JOYSTICK EQU 0201h
JOY_BUTTON_1  EQU 20h
JOY_BUTTON_2  EQU 10h

DOS_PRINTCHR  EQU 02h
DOS_PRINTSTR  EQU 09h

DOS_INTVECT_SET EQU 025h
DOS_KEEPPRGM    EQU 031h
DOS_INTVECT_GET EQU 035h

DOS_FILE_CREATE EQU 03ch
DOS_FILE_OPEN   EQU 03dh
DOS_FILE_CLOSE  EQU 03eh
DOS_FILE_READ   EQU 03fh
DOS_FILE_WRITE  EQU 040h

DOS_ALLOCMEM  EQU 048h
DOS_FREEMEM   EQU 049h


FILE_MODE_READ      EQU 00h
FILE_MODE_WRITE     EQU 01h
FILE_MODE_READWRITE EQU 02h


PSP_ENVBLOCK   EQU 02ch


LOADWORD MACRO reg, hi, lo
    mov reg, ( (hi SHL 8) OR lo )
ENDM


KEYBD_WRITE   EQU 05h

IFNDEF DEBUGMODE
DEBUGMODE EQU 0
ENDIF

    jmp Install ; Jump over data and resident code

; Data must be in code segment so it won't be thrown away with Install code.

Down_msg BYTE 'Move dial to down position, and press button.$'
Up_msg BYTE 'Move dial to up position, and press button.$'
Crlf_msg BYTE 0dh,0ah,'$'
CalibrationFile db "xwjoy.cal", 0

KeyDataTable:
KeyData_0     BYTE 5ch,2bh ; '\' (0%)
KeyData_33    BYTE 5bh,1ah ; '[' (33%)
KeyData_66    BYTE 5dh,1bh ; ']' (66%)
KeyData_100   BYTE 08h,0eh ; backspace (100%)
KeyData_Plus  BYTE 3dh,0dh ; '+'
KeyData_Minus BYTE 2dh,0ch ; '-'


BANDS equ 3
FINE_BANDS_PER_BAND equ 10

; calibration info:
Calibration_Persist:
Range_0   WORD ?
Range_100 WORD ?
Calibration_Persist_End:

Range_Inverted BYTE 0
BandWidthCoarse WORD ?
BandWidthFine WORD ?

; state:
LastRangeAbsolute BYTE ?
LastRangeRelative BYTE ?
LastOffset WORD ?
LastRising BYTE 1 ; 1 if rising, -1 if falling

; install:
OldISR WORD 0000h
OldISRSeg WORD 0000h


JoystickReadPosition PROC USES dx ; returns position in cx
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


JoystickWaitButton PROC ; returns the button (s) pressed in al
    mov dx, PORT_JOYSTICK
JoystickWaitButton_loop:
    in al, dx
    and al, JOY_BUTTON_1 OR JOY_BUTTON_2
    cmp al, JOY_BUTTON_1 OR JOY_BUTTON_2
    .IF zero?
        jmp JoystickWaitButton_loop
    .ENDIF
    ret
JoystickWaitButton ENDP


JoystickWaitNotButton PROC
    mov dx, PORT_JOYSTICK
JoystickWaitNotButton_loop:
    in al, dx
    and al, JOY_BUTTON_1 OR JOY_BUTTON_2
    cmp al, JOY_BUTTON_1 OR JOY_BUTTON_2
    .IF !zero?
        jmp JoystickWaitNotButton_loop
    .ENDIF
    ret
JoystickWaitNotButton ENDP


HandlePosition MACRO SendKey ; expects absolute band in ah, relative band in al, and fine-band offset within greater band in cx, possibly negative
    LOCAL CheckOffset, Decr, Incr, OffsetKey, SkipRest

    ; write new range (if needed)
    .IF al != cs:[LastRangeRelative]
        mov bx, ax ; save range before converting to keycodes

        shl al, 1 ; di = KeyDataTable[range * 2]
        xor ah, ah
        add ax, offset KeyDataTable
        mov di, ax

        IF SendKey
            push cx
            mov cx, cs:[di]
            mov ah, KEYBD_WRITE
            int 16h
            pop cx
            .IF al ; if failed to write to keyboard
                jmp SkipRest
            .ENDIF
        ENDIF

        mov cs:[LastRangeAbsolute], bh
        mov cs:[LastRangeRelative], bl ; we're now in this new range
        xor ax, ax ; at offset zero ; at offset zero
        mov cs:[LastOffset], ax
    .ENDIF

    ; write new offsets (while needed)
    mov ah, KEYBD_WRITE

CheckOffset:
    cmp cx, cs:[LastOffset]
    je SkipRest
    push cx ; save offset
    jg Incr

Decr:
    ; else less-than
    mov cx, offset KeyData_Minus
    xor bx, bx
    dec bx ; bl = -1
    jmp OffsetKey

Incr:
    mov cx, offset KeyData_Plus
    ; greater-than
    xor bx, bx
    inc bl ; bl = 1

OffsetKey:
    mov di, cx
    mov cx, cs:[di]
    IF SendKey
        int 16h
    ENDIF
    pop cx ; restore offset
    IF SendKey
        .IF al ; if failed to write to keyboard
            jmp SkipRest
        .ENDIF
    ENDIF
    add cs:[LastOffset], bx ; success!
    jmp CheckOffset

SkipRest:
ENDM



IntHdlr PROC FAR
    pusha
;    call JoystickReadPosition
    call JoystickReadPosition
    call ConvertPosition

    HandlePosition 1
    popa

IntHdlr_callprev:
    pushf

    ;call far ptr cs:OldISR
    byte 2eh,0ffh,1eh
    word offset OldISR

    ;.IF cs:[LastRangeAbsolute] == 2
    ;    TryUninstall
    ;.ENDIF

    iret
IntHdlr ENDP


;;;;;;;;;;;;;;;;;;;;
; ConvertPosition(cx position) returns absolute band in ah, relative band in al, and fine-band offset within greater band in cx, possibly negative
;;;;;;;;;;;;;;;;;;;;
ConvertPosition PROC
    ; get the range
    mov ax, cs:[Range_100]
    mov bx, cs:[Range_0]

    ; possible early return, before MustCompute
    .IF cs:[BandWidthCoarse] == 0
        xor al, al
    .ELSEIF cs:[Range_Inverted]
        .IF cx <= bx ; read a value beyond 100%
            mov al, BANDS ; top band
            mov dx, cs:[Range_100]
            sub dx, cs:[Range_0]
        .ELSEIF cx >= ax ; read a value beyond 0%
            xor al, al
            xor dx, dx
            mov cs:[LastRising], 01h ; nowhere to go but up
        .ELSE
            ; gotta subtract reading from range top
            mov dx, cx ; save reading in dx, then reading = top - reading
            mov cx, ax ; ax == cs:[Range_100]
            sub cx, dx
            jmp MustCompute
        .ENDIF
    .ELSE
        .IF cx >= ax ; read a value beyond 100%
            mov al, BANDS ; top band
            mov dx, cs:[Range_100]
            sub dx, cs:[Range_0]
        .ELSEIF cx <= bx ; read a value beyond 0%
            xor al, al
            xor dx, dx
            mov cs:[LastRising], 01h ; nowhere to go but up
        .ELSE
            sub cx, bx ; normalized_position = position - range_bottom
            jmp MustCompute
        .ENDIF
    .ENDIF

    mov ah, al ; absolute band = relative band
    xor cx, cx ; no offset
    ret

MustCompute:
    push bp
    mov bp, sp
    sub sp, 8

    ; 6 bytes before sp will be stack area for new band, relative band, offset

    mov [bp - 8], cx

    mov ax, cx ; load dividend
    xor dx, dx
    mov bx, cs:[BandWidthCoarse] ; divisor
    div bx ; ax is quotient, which is band
    ; dx is remainder

    mov [bp - 6], ax ; save band


;    .IF cs:[LastRangeAbsolute] > al || cs:[LastRangeAbsolute] == al && cs:[LastOffset] < 0
;        inc ax ; moving to lower band || staying at same band, and offset negative, falling = true, inc band
;        sub dx, cs:[BandWidthCoarse] ; and reduce the remainder by that amount, so we go negative with offset
;    .ENDIF
    xor cl, cl
    inc cl ; positive offsets, unless we determine otherwise

    cmp cs:[LastRangeAbsolute], al
    jl RelativeComputed ; moving to a higher band; will use positive offsets
    jg NegOffsets; moving to a lower band; will use negative offsets
    xor bx, bx ; can be bl, now
    cmp cs:[LastRising], bl
;    cmp cs:[LastOffset], bx
    jge RelativeComputed  ; if same band, but last offset < 0, use negative offsets
NegOffsets:
    inc ax ; moving to lower band || staying at same band, and offset negative, falling = true, inc band
    sub dx, cs:[BandWidthCoarse] ; and reduce the remainder by that amount, so we go negative with offset
    neg cl
RelativeComputed:
    mov cs:[LastRising], cl
    mov [bp - 4], ax ; save relative band

    mov ax, dx ; load remainder as dividend
    cwd
    mov bx, cs:[BandWidthFine]
    idiv bx ; ax is offset band

    mov [bp - 2], ax ; save offset

    mov ah, [bp - 6] ; absolute band
    mov al, [bp - 4] ; relative band
    mov cx, [bp - 2] ; offset
    mov dx, [bp - 8] ; normalized offset

    mov sp, bp
    pop bp
ConvertPosition_ret:
    ret
ConvertPosition ENDP


IncludeUninstallSupport MACRO

Uninstall PROC USES ax dx
    ; restore original interrupt vector
    push ds
    mov ds, cs:[OldISRSeg]
    mov dx, cs:[OldISR]
    LOADWORD ax, DOS_INTVECT_SET, 08h ; vector = 08
    int 21h ; DS:DX -> new interrupt handler
    pop ds

    push es
    push cs
    pop es ; Now free the program's memory
    mov ah, DOS_FREEMEM
    int 21h
    pop es

    ret
Uninstall ENDP


CanUninstall PROC USES es bx cx
    ; (returns ax = true, if can uninstall)

    ; query current interrupt vector
    LOADWORD ax, DOS_INTVECT_GET, 08h ; vector = 08 (clock)
    int 21h ; ES:BX -> current interrupt handler

    mov ax, es
    mov cx, cs
    .IF ax == cx && bx == IntHdlr
        mov ax, 1
    .ELSE
        xor ax, ax
    .ENDIF

    ret
CanUninstall ENDP

ENDM

TryUninstall MACRO ; interrupts should be disabled whenever we call this!
    push ax
    call CanUninstall
    .IF ax
        call Uninstall
    .ENDIF
    pop ax
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


InstallSection:

;* InstallSection
;* This label marks the end of the TSR's resident section and the
;* beginning of the installation section.  When xwjoy terminates through
;* function 31h, the above code and data remain resident in memory.  The
;* memory occupied by the following code is returned to DOS.

IF DEBUGMODE

MsgUpper db "Upper: ", '$'
MsgLower db "Lower: ", '$'
MsgRange db "Range: ", '$'
MsgBandWidthCoarse db "Coarse Band: ", '$'
MsgBandWidthFine db "Fine Band: ", '$'
Inverted_msg db "(Inverted)", 0dh, 0ah, '$'
MsgColon db ": ", '$'


PrintUnsignedCX PROC USES ax bx cx dx
    mov ah, DOS_PRINTCHR
    mov bx, cx

    mov cx, 4
PrintLoop:
    rol bx, 4
    mov dl, bl
    and dl, 0fh
    .IF dl > 9
    add dl, 'a' - 10
    .ELSE
    or dl, '0'
    .ENDIF
    int 21h
    loop PrintLoop
    ret
PrintUnsignedCX ENDP


PrintCalibration MACRO
    ; display joystick calibration values
    mov ah, DOS_PRINTSTR

    mov dx, offset MsgUpper
    int 21h
    mov cx, cs:[Range_100]
    call PrintUnsignedCX
    mov dx, offset Crlf_msg
    int 21h

    mov dx, offset MsgLower
    int 21h
    mov cx, cs:[Range_0]
    call PrintUnsignedCX
    mov dx, offset Crlf_msg
    int 21h

    mov dx, offset MsgRange
    int 21h
    mov cx, cs:[Range_100]
    sub cx, cs:[Range_0]
    call PrintUnsignedCX
    mov dx, offset Crlf_msg
    int 21h

    mov dx, offset MsgBandWidthCoarse
    int 21h
    mov cx, cs:[BandWidthCoarse]
    call PrintUnsignedCX
    mov dx, offset Crlf_msg
    int 21h

    mov dx, offset MsgBandWidthFine
    int 21h
    mov cx, cs:[BandWidthFine]
    call PrintUnsignedCX
    mov dx, offset Crlf_msg
    int 21h

    .IF cs:[Range_Inverted]
        mov dx, offset Inverted_msg
        int 21h
    .ENDIF
ENDM

PrintPosition MACRO ; clobbers some registers
    push cx
    push ax

    mov cx, dx
    call PrintUnsignedCX
    mov dx, ax

    mov ah, DOS_PRINTCHR
    mov dl, ':'
    int 21h

    pop ax
    push ax
    xor cx, cx
    mov cl, ah
    call PrintUnsignedCX

    mov ah, DOS_PRINTCHR
    mov dl, ':'
    int 21h

    pop ax
    xor cx, cx
    mov cl, al
    call PrintUnsignedCX

    mov ah, DOS_PRINTCHR
    mov dl, ':'
    int 21h

    pop cx
    call PrintUnsignedCX
    mov ah, DOS_PRINTSTR
    mov dx, offset Crlf_msg
    int 21h
ENDM


ENDIF


ComputeCalibration MACRO
    mov cx, cs:[Range_100]

    .IF cx < cs:[Range_0] ; inverted, swap top and bottom
        mov cs:[Range_Inverted], 1
        xchg cx, cs:[Range_0]
    .ENDIF
    mov cs:[Range_100], cx

    ; compute usable range
    ;mov cx, cs:[Range_100]
    sub cx, cs:[Range_0]

    xor dx, dx
    mov ax, cx
    mov bx, BANDS
    div bx
    mov cs:[BandWidthCoarse], ax

    xor dx, dx
    mov bx, FINE_BANDS_PER_BAND
    div bx
    .IF ax == 0
        inc ax ; fine shouldn't be 0
    .ENDIF
    mov cs:[BandWidthFine], ax
ENDM


Calibrate PROC
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

    mov cs:[Range_100], cx

    ret
Calibrate ENDP


SaveCalibration PROC
    LOCAL res:WORD

    mov [res], -1 ; failure, by default

    mov ah, DOS_FILE_CREATE
    xor cx, cx ; attr_normal
    lea dx, CalibrationFile
    int 21h

    .IF !carry? ; success
        mov bx, ax ; bx = file handle
        mov ah, DOS_FILE_WRITE
        mov cx, Calibration_Persist_End - Calibration_Persist
        lea dx, Calibration_Persist
        int 21h

        .IF !carry? && ax == (Calibration_Persist_End - Calibration_Persist) ; succeeded
            mov [res], 0
        .ENDIF

        ; close file
        mov ah, DOS_FILE_CLOSE
        ; bx = file handle
        int 21h
    .ENDIF

    mov ax, [res]
    ret
SaveCalibration ENDP


LoadCalibration PROC
    LOCAL res:WORD

    mov [res], -1 ; failure, by default

    LOADWORD ax, DOS_FILE_OPEN, FILE_MODE_READ
    lea dx, CalibrationFile
    int 21h

    .IF !carry? ; success
        mov bx, ax ; bx = file handle
        mov ah, DOS_FILE_READ
        mov cx, Calibration_Persist_End - Calibration_Persist
        lea dx, Calibration_Persist
        int 21h

        .IF !carry? && ax == (Calibration_Persist_End - Calibration_Persist) ; succeeded
            mov [res], 0
        .ENDIF

        ; close file
        mov ah, DOS_FILE_CLOSE
        ; bx = file handle
        int 21h
    .ENDIF

    mov ax, [res]
    ret
LoadCalibration ENDP


Install PROC
    call LoadCalibration
    .IF ax
        call Calibrate
        call SaveCalibration
    .ENDIF

    ComputeCalibration

    ; read initial position
    call JoystickReadPosition
    call ConvertPosition
    mov cs:[LastRangeAbsolute], ah
    mov cs:[LastRangeRelative], al
    mov cs:[LastOffset], cx

IF DEBUGMODE
    PrintCalibration

printloop:
    call JoystickWaitNotButton
    call JoystickWaitButton
    push ax ; save button

    call JoystickReadPosition
    call JoystickReadPosition
    call ConvertPosition

    pusha
    HandlePosition 0 ; called for side-effects only
    popa
    PrintPosition

    pop ax ; restore button
    test al, JOY_BUTTON_2
    jz printloop

    ; don't actually install, just test
    ; exit to DOS
    mov ax, 4c00h
    int 21h
ENDIF

    ; save old interrupt vector
    LOADWORD ax, DOS_INTVECT_GET, 08h ; vector = 08 (clock)
    int 21h ; ES:BX -> current interrupt handler
    mov cs:[OldISR], bx
    mov cs:[OldISRSeg], es

    ; set new interrupt vector
    mov dx, IntHdlr ; 01a3 = interrupt handler
    LOADWORD ax, DOS_INTVECT_SET, 08h ; vector = 08
    int 21h ; DS:DX -> new interrupt handler

    mov es, cs:[PSP_ENVBLOCK] ; free program environment block.
    mov ah, DOS_FREEMEM
    int 21h

    mov dx, offset InstallSection ; everything before InstallSection remains resident
    shr dx, 4
    inc dx
    LOADWORD ax, DOS_KEEPPRGM, 0 ; al:0 is return value
    int 21h
Install ENDP


; Uninstall:
;https://www.plantation-productions.com/Webster/www.artofasm.com/DOS/ch18/CH18-4.html

END
