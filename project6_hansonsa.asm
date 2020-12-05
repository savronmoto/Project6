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

mGetString MACRO prompt,maxsize,inputString,stringLength
  ; preserve registers
  push	EDX
  push  ECX
  push  EAX
  ; display prompt 
  mov   EDX, prompt				; (input parameter, by reference)
  call  WriteString
  ; get user input 
  mov   EDX, inputString		; (output parameter, by reference)
  mov   ECX, maxsize			; (input parameter, by value)
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
  call  CrLf
  pop	EDX

ENDM

MAXSIZE = 101
ARRAYSIZE = 10
.data
intro		BYTE	 " String Primitive Manipulation with Macros, by Savanna Hanson",13,10,0
intro2		BYTE	 "Please enter 10 signed decimal integers."
 			BYTE	 "Each number needs to be small enough to fit inside a 32-bit register. After you have"
			BYTE	 " finished inputting the raw numbers I will display a list of the integers, their sum, and their average value.",13,10,0
inString	BYTE	 MAXSIZE DUP(?)	 ;User String
outString	BYTE	 MAXSIZE DUP(?)	 ;User String - DO I NEED THIS??
prompt		BYTE	 "Please enter a signed decimal integer: ",0
invalid     BYTE     "That is not a valid integer, try again.",0
startLabel  BYTE     "The starting string:    ",0
dupLabel    BYTE     "The duplicate string:   ",0
capLabel    BYTE     "The capitalized string: ",0
revLabel    BYTE     "The reversed string:    ",0
sLen		DWORD	 ?
numList		SDWORD	 ARRAYSIZE DUP(?)
typeList    DWORD    TYPE numList      ; 4 (type SDWORD = 4 bytes)
countList   DWORD    LENGTHOF numList  ; 
numBytes    DWORD    SIZEOF numList    ; 
validInt	SDWORD	 ?				   ; "returns" from readVal, to be added to numList in loop

.code
main PROC
  mov	ECX, ARRAYSIZE
_untilTen:
  mDisplayString OFFSET intro
  call	CrLf
  mDisplayString OFFSET intro2
  push	OFFSET validInt
  push  OFFSET invalid
  push	sLen
  push	OFFSET inString
  push  OFFSET prompt
  call  readVal
  push  OFFSET validInt
  push  validInt
  call  writeVal
  mov	ESI, validInt				; this is a SDWORD value. Should I change it back to ascii before adding to the array? Probably...
  mov	EDI, OFFSET numList
  mov	[EDI], ESI				
  add	EDI, typeList
  LOOP	_untilTen
; Get 10 valid integers from the user.
; Stores these numeric values in an array.
; Display the integers, their sum, and their average.

  Invoke ExitProcess,0	; exit to operating system
main ENDP


; ---------------------------------------------------------------------------------
; Name: readVal
;	
; Preconditions:
; Postconditions: 
; Receives:	[EBP+24] = validInt - offset address for data label
;			[EBP+20] = invalid - offset address
;           [EBP+16] = sLen - value: ?
;			[EBP+12] = inString - offset address
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
  mGetString [EBP+8],MAXSIZE,[EBP+12],[EBP+16]

; Convert (using string primitives) the string of ascii digits to its numeric value representation (SDWORD),
; and validate the user’s input is a valid number (no letters, symbols, etc).

  mov	ECX, [EBP+16]
  dec   ECX
  mov	ESI, [EBP+12]
_loop:
  cld					; incrementing
  lodsb					; bytes in inString into AL
  mov   EBX, 0          ; numInt starts at 0
  cmp	AL, 48
  jl	_notNum			; validate
  cmp	AL, 57
  jg	_notNum
  sub	AL, 48			; it IS a number digit
  push  EAX              
  mov   EAX, 10         
  imul	EBX             ; 10 * numInt
  mov	EBX, EAX        
  pop   EAX             ; (numChar-48)
  movzx EAX, AL
  add   EBX, EAX        ; numInt = 10 * numInt + (numChar - 48)
  LOOP	_loop
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
readVal ENDP

; ---------------------------------------------------------------------------------
; Name: writeVal
;	
; Postconditions:
; Receives: [EBP+12] = address of validInt - input parameter, by reference
;	    	[EBP+8] = validInt - input parameter, by value - SDWORD
; Returns: 
; ---------------------------------------------------------------------------------
writeVal PROC
; Convert a numeric SDWORD value (input parameter, by value) to a string of ascii digits
  

; Invoke the mDisplayString macro to print the ascii representation of the SDWORD value to the output.
writeVal ENDP

END main
