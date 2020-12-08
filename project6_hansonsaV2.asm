TITLE String Primitives & Macros     (project6_hansonsa.asm)

; Author:					Savanna Hanson
; Last Modified:			4 December 2020
; OSU email address:		hansonsa@oregonstate.edu
; Course number/section:	CS271 Section 400
; Project Number:			6			
; Due Date:					6 December 2020
; Description:				Does some stuff with string primitives.
;							*****write more stuff here obvs*****

INCLUDE Irvine32.inc

;-----------------------------------------------------------------
; MACROS
;-----------------------------------------------------------------

mGetString MACRO prompt,inputString,maxsize,stringLength
  ; preserve registers
  push	EDX
  push  ECX
  push  EAX
  ; display prompt 
  mov   EDX, prompt				; prompt for a number (input parameter, by reference) - should this be my other macro? within a macro??
  call  WriteString
  ; get user input 
  mov   EDX, inputString		; buffer (output parameter, by reference)
  mov   ECX, maxsize        	; maxsize (input parameter, by value)
  call  ReadString
  mov	EBX, stringLength
  mov   [EBX], EAX		        ; bytes read - (output parameter, by reference)
  call  CrLf
  pop	EAX
  pop	ECX
  pop	EDX
ENDM

mDisplayString MACRO userString
  ; print string - (input parameter, by reference)
  push  EDX
  mov	EDX, userString
  call	WriteString
  pop	EDX

ENDM

MAXSIZE = 21
ARRAYSIZE = 10

.data
intro			BYTE	 " String Primitive Manipulation with Macros, by Savanna Hanson",13,10,0
intro2			BYTE	 "Please enter 10 signed decimal integers."
 				BYTE	 "Each number needs to be small enough to fit inside a 32-bit register. After you have"
				BYTE	 " finished inputting the raw numbers I will display a list of the integers, their sum, and their average value.",13,10,0
inString		BYTE	 MAXSIZE DUP(0)	 ;User String
prompt			BYTE	 "Please enter a signed decimal integer: ",0
invalid			BYTE     "That is not a valid integer, try again.",0
intLabel		BYTE     "The integers you supplied are: ",0
sumLabel		BYTE     "Their sum is:                  ",0
avgLabel		BYTE     "Their average is:              ",0
sLen			DWORD	 ?
numList			SDWORD	 ARRAYSIZE DUP(?)
typeList		DWORD    TYPE numList      ; 4 (type SDWORD = 4 bytes)
countList		DWORD    LENGTHOF numList  ; 
validInt		DWORD	 ?				   ; "returns" from readVal, to be added to numList in loop
comma			BYTE	 ", ",0
revString		BYTE     MAXSIZE DUP(0)
properString	BYTE     MAXSIZE DUP(0)

.code
main PROC
  mov	ECX, ARRAYSIZE							
  mDisplayString OFFSET intro
  call	CrLf
  mDisplayString OFFSET intro2
  call  CrLf
  mov	EDI, OFFSET numList			
_untilTen:							; Get 10 valid integers from the user.
  push	OFFSET validInt
  push  OFFSET invalid
  push	OFFSET sLen
  push	OFFSET inString
  push  OFFSET prompt
  call  readVal
  mov	ESI, validInt				; this is a SDWORD integer value!
  mov	[EDI], ESI					; Stores these numeric values in an array.
  add	EDI, typeList
  LOOP	_untilTen

  ; do math

  mov   ECX, ARRAYSIZE
  mDisplayString OFFSET intLabel 
  mov	ESI, OFFSET numList
_displayLoop:
  cld								; moving forward
  lodsd								; move first value in numList to EAX for processing
  push  OFFSET properString
  push  OFFSET revString
  push	EAX
  call  writeVal					; Display the integers, ****EVENTUALLY**** their sum, and their average.
  mDisplayString OFFSET comma
  LOOP	_displayLoop
  call  CrLf
  call  CrLf

  Invoke ExitProcess,0	; exit to operating system
main ENDP


; ---------------------------------------------------------------------------------
; Name: readVal
;	A method to read user input of ascii number characters and turn them into their acutal signed integer value (as a SDWORD)
;   Validation occurs to ensure input is a number and not some other character. 
; Preconditions:
; Postconditions: 
; Receives:	[EBP+24] = validInt - offset address for data label
;			[EBP+20] = invalid - offset address
;           [EBP+16] = sLen - offset address
;			[EBP+12] = inString (the buffer) - offset address
;	    	[EBP+8] = prompt - offset address
; Returns: 
; ---------------------------------------------------------------------------------
readVal PROC
; preserve EBP
  push	EBP				
  mov	EBP, ESP
; preserve registers
  pushad

; Invoke the mGetSring macro to get user input in the form of a string of digits.
_getInput:
  mGetString [EBP+8],[EBP+12],MAXSIZE,[EBP+16]

; Convert (using string primitives) the string of ascii digits to its numeric value representation (SDWORD),
; and validate the user’s input is a valid number (no letters, symbols(other than negataive), etc).
  mov	ESI, [EBP+16]	; 
  mov   ECX, [ESI]		; set string length as counter
;  dec   ECX
  mov	ESI, [EBP+12]	
  mov   EBX, 0          ; numInt starts at 0

  cld
  lodsb					; bytes in inString into AL

  cmp   AL, 45			; check if char is negative sign
  je	_negLoop

  cmp	AL, 48			; make sure it is a number char
  jl	_notNum			
  cmp	AL, 57
  jg	_notNum
  sub	AL, 48			; it IS a number digit, perform algorithm
  movzx EAX, AL
  push  EAX              
  mov   EAX, 10         
  imul	EBX             ; 10 * numInt
  mov	EBX, EAX        
  pop   EAX             ; (numChar-48)
  movzx EAX, AL
  add   EBX, EAX        ; numInt = 10 * numInt + (numChar - 48)
  dec	ECX
  jmp	_posLoop

; If it is negative:
_negLoop:
  dec   ECX
  lodsb					; next char
  cmp	AL, 48			; make sure it is a number char
  jl	_notNum			
  cmp	AL, 57
  jg	_notNum
  sub	AL, 48			; it IS a number digit, perform algorithm
  movzx EAX, AL
  push  EAX              
  mov   EAX, 10         
  imul	EBX             ; 10 * numInt
  mov	EBX, EAX        
  pop   EAX             ; (numChar-48)
  movzx EAX, AL
  add   EBX, EAX        ; numInt = 10 * numInt + (numChar - 48)
  LOOP  _negLoop
  neg   EBX
  jmp	_store

_posLoop:
  cmp   ECX, 0
  jz	_store
  cld
  lodsb	
  cmp	AL, 48			; make sure it is a number char
  jl	_notNum			
  cmp	AL, 57
  jg	_notNum

  sub	AL, 48			; it IS a number digit
  movzx EAX, AL
  push  EAX              
  mov   EAX, 10         
  imul	EBX             ; 10 * numInt
  mov	EBX, EAX        
  pop   EAX             ; (numChar-48)
  movzx EAX, AL
  add   EBX, EAX        ; numInt = 10 * numInt + (numChar - 48)
  LOOP	_posloop
  jmp	_store
_notNum:
  mDisplayString [EBP+20]
  jmp	_getInput
_store: 
; Store this value in a memory variable (output parameter, by reference). 
  mov	EDI, [EBP+24]	; validInt address into EDI  ;
  mov	[EDI], EBX		; numInt is the value at that address
  
  popad
  pop	EBP 

  ret   20
readVal ENDP

; ---------------------------------------------------------------------------------
; Name: writeVal
;	Convert a numeric SDWORD value (input parameter, by value) to a string of ascii digits
; Postconditions:
; Receives: [EBP+16] = properString - by reference - output pararmeter
;			[EBP+12] = revString - by reference   	
;			[EBP+8] = EAX (nth element of numList) - input parameter, by value - SDWORD
; Returns: 
; ---------------------------------------------------------------------------------
writeVal PROC
; preserve EBP
  push	EBP				
  mov	EBP, ESP
; preserve registers
  pushad

; to check if the SDWORD is negative, check the hex MSB, if it is 8 or higher the number is negative. or in decimal it would be 2147483648 or greater

; If the SDWORD is negative, first add the ascii character for -, whichi is 45 (2Dh) ***********DO NEG PART*********

; then for each digit, divide by 10. the remainder plus 48 is the ascii code for the digit.
  mov   EBX, 10
  mov   EAX, [EBP+8]		 ; move element of numList into dividend
  mov   ECX, 0				 ; counter
  mov   EDI, [EBP+12]		 ; EDI points to address of outString I CAN DO THE STORSB thing now because i have a byte array. and use redfield's reversal algo.
_divLoop:  
  cdq
  idiv  EBX					 ; EAX/EBX = q: EAX r: EDX
  add   EDX, 48
  push  EAX					 ; to save the quotient
  mov   EAX, EDX			 ; move the num we want to EAX so we can use STOSD
  stosb						 ; puts EAX val into [EDI] and increments EDI ****should this be stosD?????
  pop   EAX
;  push EDX
;  mov  [EDI], EDX	
  inc   ECX
  cmp   EAX, 0				 ; if quotient is 0 we can stop
  jnz  _divLoop				 ; then do it again. divide the quotient by 10, and the remainder plus 48 is the second to last digit. and so on.

  push  ECX					 ; save the string's length!

;reverse so it's proper:
  mov   ESI, [EBP+12]
  add   ESI, ECX
  dec   ESI
  mov   EDI, [EBP+16]
 _revLoop:
  std
  lodsb
  cld
  stosb
  LOOP   _revLoop
;add the null terminator:
  pop   ECX
  mov   EDI, [EBP+16]
  add   EDI, ECX			 ; add the length to get to the end of the string
  mov	EAX, 0
  mov	[EDI], EAX			 ; add the null terminator 0

;write
  mDisplayString [EBP+16]	 ; Invoke the mDisplayString macro to print the ascii representation of the SDWORD value to the output.

  popad						 ; restore registers
  pop   EBP
  ret   12
writeVal ENDP

END main
