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
  mov   [EBX], EAX		; bytes read - (output parameter, by reference)
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

.data
intro		BYTE	 " String Primitive Manipulation with Macros, by Savanna Hanson",13,10,0
intro2		BYTE	 "Please enter 10 signed decimal integers."
 			BYTE	 "Each number needs to be small enough to fit inside a 32-bit register. After you have"
			BYTE	 " finished inputting the raw numbers I will display a list of the integers, their sum, and their average value.",13,10,0
inString	BYTE	 MAXSIZE DUP(?)	 ;User String
outString	BYTE	 MAXSIZE DUP(?)	 ;User String
prompt		BYTE	 "Please enter a signed decimal integer: ",0
startLabel  BYTE     "The starting string:    ",0
dupLabel    BYTE     "The duplicate string:   ",0
capLabel    BYTE     "The capitalized string: ",0
revLabel    BYTE     "The reversed string:    ",0
sLen		DWORD	 ?


.code
main PROC
  mDisplayString OFFSET intro
  call	CrLf
  mDisplayString OFFSET intro2
  push	sLen
  push	OFFSET inString
  push  OFFSET prompt
  call  readVal
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
; Receives: [EBP+16] = sLen - value: ?
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
  mGetString [EBP+8],MAXSIZE,[EBP+12],[EBP+16]

; Convert (using string primitives) the string of ascii digits to its numeric value representation (SDWORD)
;  mov   ESI, 
; validate the user’s input is a valid number (no letters, symbols, etc).

; Store this value in a memory variable (output parameter, by reference). 

  popad
  pop	EBP 
readVal ENDP

; ---------------------------------------------------------------------------------
; Name: writeVal
;	
; Postconditions:
; Receives: [EBP+12] = 
;	    	[EBP+8] = 
; Returns: 
; ---------------------------------------------------------------------------------
writeVal PROC
; Convert a numeric SDWORD value (input parameter, by value) to a string of ascii digits

; Invoke the mDisplayString macro to print the ascii representation of the SDWORD value to the output.
writeVal ENDP

END main
