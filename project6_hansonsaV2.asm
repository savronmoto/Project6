TITLE String Primitives & Macros     (project6_hansonsa.asm)

; Author:					Savanna Hanson
; Last Modified:			7 December 2020
; OSU email address:		hansonsa@oregonstate.edu
; Course number/section:	CS271 Section 400
; Project Number:			6			
; Due Date:					6 December 2020
; Description:				A program that creates versions of the Irvine procedures ReadInt and WriteInt.
;							Gets user input of signed integers in string form, then calls the conversion procedure to 
;							translate ascii into a SDWORD value. The program will perform sum and average calculations 
;							on the integer values, then convert the number back to ascii characters, and display all
;							the input numbers as well as their sum and average.

INCLUDE Irvine32.inc

;----------------------------------------------------------------------------------
; MACROS
;----------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------
; Name: mGetString
;	Stores a user input string
; Preconditions: do not use eax, edx, ecx, ebx as arguments
; Receives:
;	prompt = string for user input prompt - reference
;	inputString = buffer for string to be read into - reference
;	maxsize = maxsize of string - value
;	stringLength = the number of bytes read - reference
; Returns: 
;	inputString = generated string address
;	stringLength = length of the generated string
; ---------------------------------------------------------------------------------

mGetString MACRO prompt,inputString,maxsize,stringLength
  ; preserve registers
  push	EDX
  push  ECX
  push  EAX
  ; display prompt 
  mov   EDX, prompt				; prompt for a number (input parameter, by reference)
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

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;	Displays a passed string to output.
; Preconditions: do not use edx as argument
; Receives:
;	userString = string to be printed - input paramete by reference
; ---------------------------------------------------------------------------------

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
invalid			BYTE     "That is not a valid integer, try again. ",0
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
sum				SDWORD   ?
average			SDWORD   ?

.code
main PROC
; Display intoduction
  mov	ECX, ARRAYSIZE							
  mDisplayString OFFSET intro
  call	CrLf
  mDisplayString OFFSET intro2
  call  CrLf

; Get 10 valid integers from the user
  mov	EDI, OFFSET numList			
_untilTen:							
  push	OFFSET validInt
  push  OFFSET invalid
  push	OFFSET sLen
  push	OFFSET inString
  push  OFFSET prompt
  call  readVal
  mov	ESI, validInt				; This is a SDWORD integer value!
  mov	[EDI], ESI					; Stores these numeric values in an array.
  add	EDI, typeList
  LOOP	_untilTen

; Call sum procedure
  push  OFFSET sum
  push  ARRAYSIZE
  push  OFFSET numList
  call  sumVal

; Call average procedure
  push  sum
  push  OFFSET average
  push  ARRAYSIZE
  push  OFFSET numList
  call  averageVal

; Call writeVal procedure
  mov   ECX, ARRAYSIZE
  mDisplayString OFFSET intLabel 
  mov	ESI, OFFSET numList
_displayLoop:
  cld							
  lodsd								; Move first value in numList to EAX for processing
  push  OFFSET properString
  push  OFFSET revString
  push	EAX
  call  writeVal					; Display the integers
  mDisplayString OFFSET comma
  LOOP	_displayLoop
  call  CrLf
  call  CrLf

; Display sum & average
  mDisplayString OFFSET sumLabel
  push  OFFSET properString
  push  OFFSET revString
  push  sum
  call  writeVal					; Display the sum
  call  CrLf
  mDisplayString OFFSET avgLabel
  push  OFFSET properString
  push  OFFSET revString
  push  average						; Display the average
  call  writeVal
  call  CrLf

  Invoke ExitProcess,0	; exit to operating system
main ENDP


; ---------------------------------------------------------------------------------
; Name: readVal
;	A method to read user input of ascii number characters and turn them into their acutal signed integer value (as a SDWORD)
;   Validation occurs to ensure input is a number and not some other character. 
; Preconditions: 
;	validInt, invalid, sLen, inString, and prompt must all be intialized in the date section.
; Receives:	[EBP+24] = validInt - offset address for data label
;			[EBP+20] = invalid - offset address
;           [EBP+16] = sLen - offset address
;			[EBP+12] = inString (the buffer) - offset address
;	    	[EBP+8] = prompt - offset address
; Returns: validInt generated by procedure and stored in global variable.
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
  je	_decCount

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
_decCount:
  dec   ECX
_negLoop:
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
  cmp   EBX, 2147483648
  jns    _notNum
  jmp	_store
_notNum:
  mDisplayString [EBP+20]
  jmp	_getInput
_store: 
; Store this value in a memory variable (output parameter, by reference)
  mov	EDI, [EBP+24]	; validInt address into EDI 
  mov	[EDI], EBX		; numInt becomes the value at that address
  
  popad
  pop	EBP 

  ret   20
readVal ENDP

; ---------------------------------------------------------------------------------
; Name: writeVal
;	Convert a numeric SDWORD value to a string of ascii digits, and display to output.
; Preconditions: [EBP+8] must be a SDWORD. 
; Postconditions:
; Receives: [EBP+16] = properString - output parameter by reference 
;			[EBP+12] = revString - output parameter by reference  	
;			[EBP+8] = an SWORD value - input parameter by value
; Returns:  properString and revString are saved as global variables, but will be
;			overwritten the next time the procedure runs.
; ---------------------------------------------------------------------------------
writeVal PROC
; preserve EBP
  push	EBP				
  mov	EBP, ESP
; preserve registers
  pushad

; To check if the SDWORD is negative, check the hex MSB, if it is 8 or higher the number is negative.
; In decimal it would be 2147483648 or greater

  mov   ESI, [EBP+8]		 ; input param into ESI	
  mov   EDI, [EBP+12]		 ; EDI points to address of outString for stosb.
  push  ESI					 ; SAVE THIS TO COMPARE AGAIN LATER?	
  mov	EBX, 2147483648
  cmp   EBX, ESI
  js   _negative
  mov   EAX, [EBP+8]		 ; move element of numList into dividend
  jmp   _skip
_negative:
  neg   ESI
  mov   EAX, ESI

; Then for each digit, divide by 10. the remainder plus 48 is the ascii code for the digit.
_skip:
  mov   EBX, 10
  mov   ECX, 0				 ; counter
  
_divLoop:  
  cdq
  idiv  EBX					 ; EAX/EBX = q: EAX r: EDX
  add   EDX, 48
  push  EAX					 ; to save the quotient
  mov   EAX, EDX			 ; move the num we want to EAX so we can use STOSD
  stosb						 ; puts EAX val into [EDI] and increments EDI ****should this be stosD?????
  pop   EAX
  inc   ECX
  cmp   EAX, 0				 ; if quotient is 0 we can stop
  jnz  _divLoop				 ; then do it again. divide the quotient by 10, and the remainder plus 48 is the second to last digit. and so on.

  pop   ESI					 ; get that SDWORD back
  mov	EBX, 2147483648
  cmp   EBX, ESI    		 ; check if negative again, lol, there's got to be a better way.	
  js	_addSign
  jmp   _setIndices

_addSign:
  mov   EAX, 45				 ; put the ascii sign for negative in EAX for processing
  stosb
  inc   ECX

_setIndices:
  push  ECX					 ; save the string's length!
; Reverse so it's proper:
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
; Add the null terminator:
  pop   ECX
  mov   EDI, [EBP+16]
  add   EDI, ECX			 ; add the length to get to the end of the string
  mov	EAX, 0
  mov	[EDI], EAX			 ; add the null terminator 0

; Rrite
  mDisplayString [EBP+16]	 ; Invoke the mDisplayString macro to print the ascii representation of the SDWORD value to the output.

  popad						 ; restore registers
  pop   EBP
  ret   12
writeVal ENDP


; ---------------------------------------------------------------------------------
; Name: sumVal
;	sums the SDWORD integers in an array
; Preconditions: sum must be defined in data section. Array is of type SDWORDS.
; Postconditions: 
; Receives:	[EBP+16] = sum - output parameter by reference
;			[EBP+12] = ARRAYSIZE - input parameter by value
;	    	[EBP+8] = numList - input parameter by reference - offset address
; Returns: sum - an SDWORD
; ---------------------------------------------------------------------------------
sumVal PROC
; preserve EBP
  push	EBP				
  mov	EBP, ESP
; preserve registers
  pushad

  mov	ECX, [EBP+12]
  xor   EBX, EBX
  mov   ESI, [EBP+8]
  mov   EDI, [EBP+16]
_sum:
  lodsd 
  add 	EBX, EAX
  LOOP	_sum

  mov	[EDI], EBX			; save sum

  popad
  pop   EBP
  ret   12
sumVal ENDP


; ---------------------------------------------------------------------------------
; Name: averageVal
;	calculates the rounded down average of the SDWORD integers in an array
; Preconditions: sum must be defined in data section and already calculated as a SDWORD. 
;				 averge must be defined in data section. Array is of type SDWORDS.
; Postconditions: 
; Receives:	[EBP+20] = sum - input parameter by value
;			[EBP+16] = average - output parameter by reference
;			[EBP+12] = ARRAYSIZE - input parameter by value
;	    	[EBP+8] = numList - input parameter by reference - offset address
; Returns: Rounded down average (remainder is discarded) value saved to global variable.
; ---------------------------------------------------------------------------------
averageVal PROC
; preserve EBP
  push	EBP				
  mov	EBP, ESP
; preserve registers
  pushad

  mov   EAX, [EBP+20]
  mov   EBX, [EBP+12]
  mov   EDI, [EBP+16]
  
  cdq   
  idiv  EBX					 ; EAX/EBX = q: EAX r: EDX

  mov	[EDI], EAX			; save average

  popad
  pop   EBP
  ret   16
averageVal ENDP 

END main
