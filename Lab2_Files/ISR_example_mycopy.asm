; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.1 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; My adaptations
; 1. Changed the TIMER2_Rate to 500Hz, so that we trigger a timer2 intrupt every 2ms
;    this allows for a 1 second delay, since the count1ms counter triggers a display 
;	 update every 500 interupts.

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 500   ; 500Hz, for a timer tick of 2ms (timer overflows and triggers an interupt every 2ms -> goes to ISR)
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

HOURS_BUTTON   equ P0.6
SECONDS_BUTTON equ P0.0
MINUTES_BUTTON equ P0.3
AMPM_BUTTON    equ P4.5
SOUND_OUT     equ P1.1

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count2ms:     ds 2 ; Used to determine when half second has passed (2 bytes)
seconds:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop (1 byte -> 8 bits)
minutes:  ds 1 ; counter to keep track of minutes
hours:    ds 1 ; counter to keep track of hours

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
one_second_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
AMPM_flag: dbit 1 ; flag for if we are in AM or PM time

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Time_Message:  db    'Time  xx:xx:xx ', 0
Alarm_Message: db    'Alarm  xx:xx ',0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P1.1 -> complements this pin on and off every 
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count2ms+0, a
	mov Count2ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

change_AMPM:
	Set_Cursor(1, 16)
	jnb AMPM_flag, write_AM
    Display_char(#'P')
    clr AMPM_flag
    ret
write_AM:
	Set_Cursor(1, 16)
    Display_char(#'A')
    setb AMPM_flag
    ret
;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
; In this subroutine, we can check our "alarm flag (to be created)" if our alarm is off or on
; If alarm is on, we need to somehow compare the current time with the alarm time, and trigger a 
; call to another subroutine that will activate the alarm and use the speaker 
; until we push the button that turns the alarm off. 
Timer2_ISR: ;triggers when timer has overflowed (occurs after 2ms)
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc ; push accumulator to stack
	push psw ; push status flag to stack
	
	; Increment the 16-bit one mili second counter
	inc Count2ms+0    ; Increment the low 8-bits first
	mov a, Count2ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count2ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count2ms+0
	cjne a, #low(500), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count2ms+1
	cjne a, #high(500), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb one_second_flag ; Let the main program know one second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	;reset the 2ms counter
	mov Count2ms+0, a 
	mov Count2ms+1, a
	; Increment the seconds counter by 1
	lcall Update_Seconds
	cjne a, #0x60, Timer2_ISR_done ; check if seconds has reached 60 (if not 60, jump to Timer2_ISR_done)
	
Update_Times: ;update all times subroutine (will trigger if we have reached a 60 seconds mark
	clr a
	mov seconds, a ;reset seconds to 0
	lcall Update_Minutes ; update minutes call
	cjne a, #0x60, Timer2_ISR_done ; check if minutes has reached 60 (if not 60, skip to Timer2_ISR_done)
	clr a
	mov minutes, a ; reset minutes to 0
	lcall Update_Hours ; update hours call
	cjne a, #0x13, Timer2_ISR_done ; check if hours was at 12:00 when we reached 60 minutes(if not, goto Timer2_ISR_done)
	mov hours, #0x01 ; reset hours back to 1:00 if it was at 12:00 when we reached 60 minutes
	lcall change_AMPM
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti ;return from the interupt
	
Update_Seconds:
	mov a, seconds
	add a, #0x01
	da a ;value in a -> BCD
	mov seconds, a ; seconds = BCD value
	ret
	
Update_Minutes:	
	mov a, minutes ; check minutes
	add a, #0x01 ; increment minutes by 1
	da a 
	mov minutes, a	
	ret
Update_Hours:
	mov a, hours
	add a, #0x01 ; increment hours by 1
	da a
	mov hours, a
	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Time_Message)
    Set_Cursor(1, 16)
    Display_char(#'A')
    setb one_second_flag
    setb AMPM_flag
	mov seconds, #0x00 ;initialize seconds to zero
	mov minutes, #0x00 ;initialize minutes to zero
	mov hours, #0x00 ;initialize hours to zero
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb SECONDS_BUTTON, check_hours_push  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SECONDS_BUTTON, check_hours_push  ; if the 'BOOT' button is not pressed, goto loop_a 
					   ; if there is a "bounce" then there was not a real button press
					   ; if the button is still pressed, continue with the next instruction
	jnb SECONDS_BUTTON, $	   ; Wait for button release.  The '$' means: jump to same instruction
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
second_push:
	lcall Update_Seconds ; increments seconds by 1
	cjne a, #0x60, loop_b ; if seconds != 60, display all time values
	mov seconds, #0x00
	lcall Update_Minutes ; else, reset seconds to 0 and update minutes
	cjne a, #0x60, loop_b ; if minutes != 60, display all time values
	lcall Update_Hours ; else, update the hours and reset minutes to 0
	cjne a, #0x13, loop_b ; check if hours was at 12:00 when we reached 60 minutes(if not, goto Timer2_ISR_done)
	mov hours, #0x01 ; reset hours back to 1:00 if it was at 12:00 when we reached 60 minutes
	lcall change_AMPM
	sjmp loop_b             ; Display the new value

check_hours_push:
	jb HOURS_BUTTON, check_minutes_push  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb HOURS_BUTTON, check_minutes_push  ; if the 'BOOT' button is not pressed, goto loop_a 
					   ; if there is a "bounce" then there was not a real button press
					   ; if the button is still pressed, continue with the next instruction
	jnb HOURS_BUTTON, $	   ; Wait for button release.  The '$' means: jump to same instruction
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
hour_push:
	lcall Update_Hours
	cjne a, #0x13, loop_b
	mov hours, #0x01
	lcall change_AMPM
	sjmp loop_b
	
check_minutes_push:
	jb MINUTES_BUTTON, loop_a  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MINUTES_BUTTON, loop_a  ; if the 'BOOT' button is not pressed, goto loop_a 
					   ; if there is a "bounce" then there was not a real button press
					   ; if the button is still pressed, continue with the next instruction
	jnb MINUTES_BUTTON, $	   ; Wait for button release.  The '$' means: jump to same instruction
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
minute_push:
	lcall Update_Minutes
	cjne a, #0x60, loop_b
	mov minutes, #0x00
	lcall Update_Hours
	cjne a, #0x13, loop_b
	mov hours, #0x01
	lcall change_AMPM
	sjmp loop_b

loop_a:
	jnb one_second_flag, loop
	
; we should control whether or not we change the clock vs. alarm digits in this loop
; probably need to have an alarm flag (on/off) that will control which line we edit (clock/alarm)
; we also probably need seperate counters for minutes and hours
loop_b: ;if one_second flag is high or we have reset the clock values, we enter this loop
    clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 13)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(seconds) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 10)
	Display_BCD(minutes)
	Set_Cursor(1, 7)
	Display_BCD(hours)
    ljmp loop
    
END
