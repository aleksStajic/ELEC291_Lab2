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
CLOCK_MODE     equ p2.1 ; controls between clock and alarm
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
minutes_A:  ds 1 ; counter to keep track of minutes on alarm
hours_A:    ds 1 ; counter to keep track of hours on alarm

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
one_second_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
AMPM_flag: dbit 1 ; flag for if we are in AM or PM time
Alarm_Clock_flag: dbit 1 ;flag for modifying alarm or clock values (Alarm_Clock_flag = 1 means we can set an alarm)
AMPM_flag_A: dbit 1
AMPM_flag_A_set: dbit 1
Alarm_mode: dbit 1
Alarm_active: dbit 1

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
Alarm_Message: db    'Alarm xx:xx ',0
Alarm_On:      db    'on ', 0
Alarm_Off:     db    'off', 0

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
    clr ET0  ; disable timer 0 interrupt (we will do turn this on when the alarm triggers)
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
change_AMPM_A: ; label to update AMPM flag for the alarm row
	Set_Cursor(2, 12)
	jnb AMPM_flag_A_set, write_AM_A ; if current flag is PM, update to AM
	Display_char(#'P') ; else, curruent flag was AM, update to PM
	clr AMPM_flag_A_set
	ret
	write_AM_A:
		Display_char(#'A')
		setb AMPM_flag_A_set
		ret
change_AMPM:
	jnb AMPM_flag_A, change_AMPM_A ; if we are in "ALARM" mode, we should change the AM/PM type for the second row (jump to AMPM_A label)
	Set_Cursor(1, 16) ; otherwise, change the AM/PM type for the first row
	jnb AMPM_flag, write_AM ; if current flag is PM, update to AM
    Display_char(#'P') ; else, current flag was AM, update to PM
    clr AMPM_flag
    ret
	write_AM:
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
	mov a, minutes
	cjne a, #0x60, Timer2_ISR_done ; check if minutes has reached 60 (if not 60, skip to Timer2_ISR_done)
	clr a
	mov minutes, a ; reset minutes to 0
	lcall Update_Hours ; update hours call
	mov a, hours
	cjne a, #0x13, Timer2_ISR_done ; check if hours was at 12:00 when we reached 60 minutes(if not, goto Timer2_ISR_done)
	mov hours, #0x01 ; else, reset hours back to 1:00 if it was at 12:00 when we reached 60 minutes
	
Timer2_ISR_done:
	; after all our relevant times have updated, we should compare the current time with the alarm time if the alarm is active
	jnb Alarm_mode, real_done ; if Alarm_mode is off, we skip to Timer2_ISR_done
	mov a, minutes
	cjne a, minutes_A, real_done ; if minutes_clock != minutes_A skip to Timer2_ISR_done, else check hours next
	mov a, hours
	cjne a, hours_A, real_done ; if hours_clock != hours_A skips to TImer2_ISR_done, else check if AM/PM matches
	jb AMPM_flag, check1 ; if AMPM_flag = 1, check if AMPM_flag_A_set also is 1 (that means bboth alarm and clock are in AM)
	jnb AMPM_flag, check2 ; if AMPM_flag = 0, check if AMPM_flag_A_set also is 0 (that means bboth alarm and clock are in PM)
	; if AM/PM on the clock != AM/PM on the alarm, times are not equal so skip to done
	; if we made it here, the clock time equals the alarm time, so that means we need to sound the alarm
	setb Alarm_active ; turn on the alarm active flag
	setb ET0 ; enable Timer_0 interrupt -> this will cause our alarm to go off
	sjmp real_done
	check1:
		jnb AMPM_flag_A_set, real_done ; if AMPM_flag_A_set = 0, that means clock is in AM, alarm is in PM, so goto real_done
		setb Alarm_active ; else, trigger alarm
		setb ET0
		sjmp real_done
	check2: 
		jb AMPM_flag_A_set, real_done ;if AMPM_flag_A_set = 1, that means clock is in PM, alarm is in AM, so goto real_done
		setb Alarm_active ; else, trigger alarm
		setb ET0
	real_done:
		pop psw
		pop acc
		reti ;return from the interupt
	
Update_Seconds:
	mov a, seconds
	add a, #0x01
	da a ;value in a -> BCD
	mov seconds, a ; seconds = BCD value
	ret
	
Update_Minutes:	; updates either clock or alarm minute value
	jnb Alarm_Clock_flag, Minutes_Clock ; if not in alarm mode, update the clock time values
	mov a, minutes_A ; else, update the alarm values
	add a, #0x01
	da a ;value in a -> BCD
	mov minutes_A, a ; seconds = BCD value
	cjne a, #0x60, minute_A_done ; check if our alarm_minutes value has gone over 60 (if not, skip to minute_A_done)
	mov minutes_A, #0x00 ; else, reset minute_A, and update the hours
	lcall Update_Hours
	minute_A_done:
		ret
	Minutes_Clock:
		mov a, minutes ; check minutes
		add a, #0x01 ; increment minutes by 1
		da a 
		mov minutes, a	
		ret
Update_Hours: ; updates either clock or alarm hour value
	jnb Alarm_Clock_flag, Hours_Clock ; if not in alarm mode, update the clock hour values
	mov a, hours_A ; else, we should change the alarm values
	add a, #0x01 ; increment alarm hour value
	da a ;value in a -> BCD
	mov hours_A, a ; seconds = BCD value
	cjne a, #0x12, hours_A_done0 ; if we roll over from 11 to 12, we need to change AM/PM
	lcall change_AMPM
	hours_A_done0:
	cjne a, #0x13, hours_A_done ; else, we check if we should roll over from 12:00 to 1:00
	mov hours_A, #0x01
	
	hours_A_done:
		ret
	Hours_Clock:
		mov a, hours
		add a, #0x01 ; increment hours by 1
		da a
		mov hours, a
		cjne a, #0x12, done_hours ; if hours is not equal to 12, we can go to done
		lcall change_AMPM ; else, we should change AMPM
		done_hours:
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
    setb p4.5
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Time_Message)
    Set_Cursor(2,1)
    Send_Constant_String(#Alarm_Message)
    Set_Cursor(1, 16)
    Display_char(#'A')
    Set_Cursor(2,12)
    Display_char(#'A')
    Set_Cursor(2, 14)
    Send_Constant_String(#Alarm_off) ; initially display that the alarm is turned off
    setb one_second_flag
    setb AMPM_flag
    setb AMPM_flag_A
    setb AMPM_flag_A_set
    clr Alarm_Clock_flag ; alarm clock flag initially set to zero
    clr Alarm_mode ; alarm should be automatically set to off
    clr Alarm_active ; make sure alarm is not active
	mov seconds, #0x00 ;initialize seconds to zero
	mov minutes, #0x00 ;initialize minutes to zero
	mov hours, #0x01 ;initialize hours to 1
	mov minutes_A, #0x00
	mov hours_A, #0x01

Alarm_OnOff: ; defaults to alarm OFF after passing through the main initialization loop
	jnb Alarm_Clock_flag, loop ; if we enter here after main, makes sure alarm is kept off and skip to loop
	jnb Alarm_mode, turn_A_on ; if Alarm_mode is zero (off) we want to turn it on
		cpl Alarm_mode ; else, we turn off Alarm_mode
		Set_Cursor(2, 14)
		Send_Constant_String(#Alarm_Off) ; write 'off' in the alarm field
		jb Alarm_active, turn_off_alarm ; if the alarm is actively sounding, we want to turn it off once we turn off Alarm_mode
		sjmp check_hours_push ; jump back to the next stage in our button daisy chain
	turn_A_on:
		cpl Alarm_mode ; set Alarm_mode to 1
		Set_Cursor(2, 14)
		Send_Constant_String(#Alarm_On) ; write 'on' in the alarm field
		sjmp check_hours_push ; jump back to the next stage in our button daisy chain
turn_off_alarm: ; sub-routine that turns off the active alarm by complementing the ET0 (timer 0 interrupt)
	clr ET0 ; turn off timer_0 interrupt, which will stop the noise
	clr Alarm_active ; turn off the active alarm flag
	sjmp check_hours_push ; resume button checking
loop: ; After initialization the program stays in this 'forever' loop
	jb SECONDS_BUTTON, check_hours_push  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SECONDS_BUTTON, check_hours_push  ; if the 'BOOT' button is not pressed, goto loop_a 
	jnb SECONDS_BUTTON, $	  
	jb Alarm_Clock_flag, Alarm_OnOff ; if we are in alarm mode and the seconds button is pressed, toggle alarm on/off
second_push: ; change second value from p0.0 pushbutton
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
check_hours_push: ; change hour value from p0.6 pushbutton
	jb HOURS_BUTTON, check_minutes_push  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb HOURS_BUTTON, check_minutes_push  ; if the 'BOOT' button is not pressed, goto loop_a 
	jnb HOURS_BUTTON, $	   
hour_push:
	lcall Update_Hours
	mov a, hours
	cjne a, #0x13, loop_b
	mov hours, #0x01
	sjmp loop_b
loop_a: ; maximum distance from loop label for jnb to work
	jnb one_second_flag, loop
	ljmp loop_b
check_minutes_push: ; change minute values from p0.3 pushbutton
	jb MINUTES_BUTTON, check_AMPM_push  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MINUTES_BUTTON, check_AMPM_push  ; if the 'BOOT' button is not pressed, goto loop_a 
	jnb MINUTES_BUTTON, $	   
minute_push:
	lcall Update_Minutes
	mov a, minutes
	cjne a, #0x60, loop_b
	mov minutes, #0x00
	lcall Update_Hours
	cjne a, #0x13, loop_b
	mov hours, #0x01
	lcall change_AMPM
	sjmp loop_b
	
; we should control whether or not we change the clock vs. alarm digits in this loop
; probably need to have an alarm flag (on/off) that will control which line we edit (clock/alarm)
; we also probably need seperate counters for minutes and hours
loop_b: ;if one_second flag is high or we have reset the clock values, we enter this loop
    clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    lcall write_times ; prints all times to the display
    ljmp loop
   
check_AMPM_push: ; change between AM/PM using p4.5 pushbutton
	jb AMPM_BUTTON, check_mode_push  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb AMPM_BUTTON, check_mode_push  ; if the 'BOOT' button is not pressed, goto loop_a 
	jnb AMPM_BUTTON, $	   ; Wait for button release.  The '$' means: jump to same instruction
AMPM_push:
	lcall change_AMPM
	sjmp loop_b
check_mode_push: ; change between clock/alarm value editing mode using p2.1 pushbutton
	jb CLOCK_MODE, loop_a  ; if the 'BOOT' button is not pressed goto loop_a
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb CLOCK_MODE, loop_a  ; if the 'BOOT' button is not pressed, goto loop_a 
	jnb CLOCK_MODE, $	   ; Wait for button release.  The '$' means: jump to same instruction
mode_push:
	cpl Alarm_Clock_flag ; triggers if we switch to "alarm" or "clock" editing mode
	cpl AMPM_flag_A ; flag to allow us to control the AM and PM setting in the ALARM row
	;cpl ET2  ; Disable/Enable timer 2 interrupt
	sjmp loop_b
write_times: ; label to write all relevant time values to the LCD
	Set_Cursor(1, 13)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(seconds) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 10)
	Display_BCD(minutes)
	Set_Cursor(1, 7)
	Display_BCD(hours)
	Set_Cursor(2, 7)
	Display_BCD(hours_A)
	Set_Cursor(2, 10)
	Display_BCD(minutes_A)
	ret
END

