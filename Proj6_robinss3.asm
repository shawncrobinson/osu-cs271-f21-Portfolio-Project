TITLE PROJECT 6 - Low-level I/O procedures     (Proj6_robinss3.asm)

; Author: Shawn Robinson
; Last Modified: 2021/12/05
; OSU email address: robinss3@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6           Due Date: 2021/12/05
; Description: This program defines two procedures: 1) Takes ASCII user input and stores it as an integer value, and 2) Takes an integer value and writes its ASCII representation to the console.

INCLUDE Irvine32.inc

; ---------------------------------------------------------------------------------
; Name: mGetString
;
; Displays a prompt for and gets user input.
;
; Preconditions: do not use eax, ecx, edx as arguments
;
; Receives:
; promptAddr = Address of prompt displayed to the user before reading input
; outputSize = Size of buffer in bytes for storing user input 
;
; Returns: 
; outputAddr = Address of buffer to store user input
; bytesRead = Length in bytes of user input
; -----------------------------------------------------------------------------
mGetString MACRO promptAddr, outputAddr, outputSize, bytesRead
	; Preservation
	PUSH EDX
	PUSH ECX
	PUSH EAX

	; Display prompt
	MOV EDX, promptAddr
	CALL WriteString

	; Read user input
	MOV EDX, outputAddr
	MOV ECX, outputSize
	CALL ReadString
	
	; Move ReadString outputs to the macro's parameters
	MOV bytesRead, EAX

	; Preservation
	POP EAX
	POP ECX
	POP EDX
ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; Writes a string to the console.
;
; Preconditions: do not use edx as arguments
;
; Receives:
; stringAddr = Address of string to write
; -----------------------------------------------------------------------------
mDisplayString MACRO stringAddr
	; Preservation
	PUSH EDX

	; Display string
	MOV EDX, stringAddr
	CALL WriteString

	; Preservation
	POP EDX
ENDM

BUFFERSIZE = 40	; Size of any local arrays created in procedures.	
BASE = 10		; Base that WriteVal displays numbers in.

.data
	prompt				BYTE "Please enter an integer: ", 0
	userInput			SDWORD ?
	invalidInputMsg		BYTE "ERROR: You did not enter a signed number or your number was too big. Please try again.", 0 ; String used by the ReadVal procedure when the user input is invalid.
	programIntro		BYTE "Computer Architecture and Assembly Project #6 : Low Level I/O",10,13
						BYTE "Written by: Shawn Robinson",10,13,10,13
						BYTE "Please provide 10 signed 32-bit decimal integers (Range: [-2147483648, 2147483647]).",10,13,10,13,0
	intArray			SDWORD 11 DUP(0)
	arrayMsg			BYTE "The numbers you provided: ",0
	intSum				SDWORD 0
	sumMsg				BYTE "The sum of your numbers (assuming it didn't overflow, 'cause I'm not handling that): ",0
	intAvg				SDWORD 0
	AvgMsg				BYTE "The (truncated) average of your numbers (assuming the sum didn't overflow): ",0
	goodbye				BYTE "Thanks for running the IO procedures test.",10,13, "I hope your day is as wonderful for you as the moment I fixed all the bugs in this thing was for me.",0

.code

main PROC

	MOV EDX, OFFSET programIntro
	CALL WriteString

	; Get 10 numbers for input
	MOV ECX, 10
	MOV EDI, OFFSET intArray
_getInput:
	PUSH OFFSET invalidInputMsg
	PUSH OFFSET userInput
	PUSH OFFSET prompt
	CALL ReadVal
	MOV EAX, userInput
	STOSD
	LOOP _getInput
	CALL CRLF

	; Display the 10 input numbers
	MOV EDX, OFFSET arrayMsg
	CALL WriteString

	MOV ECX, 10
	MOV ESI, OFFSET intArray
_displayInput:
	LODSD
	PUSH EAX
	CALL WriteVal
	MOV AL, ','
	CALL WriteChar
	MOV AL, ' '
	CALL WriteChar
	LOOP _displayInput
	CALL CRLF
	CALL CRLF

	; Calc sum
	MOV ECX, 10
	MOV ESI, OFFSET intArray
_addNext:
	LODSD
	ADD intSum, EAX
	LOOP _addNext

	; Display sum
	MOV EDX, OFFSET sumMsg
	CALL WriteString
	PUSH intSum
	CALL WriteVal
	CALL CRLF
	CALL CRLF

	; Sign extend intSum into EDX so IDIV works properly
	CMP intSum, 0
	JS _negSignExtend
_zeroExtend:
	MOV EDX, 0
	JMP _divide
_negSignExtend:
	MOV EDX, -1
_divide:
	MOV EAX, intSum
	MOV EBX, 10
	IDIV EBX
	MOV intAvg, EAX
	
	; Display sum and average
	MOV EDX, OFFSET avgMsg
	CALL WriteString
	PUSH intAvg
	CALL WriteVal
	CALL CRLF
	CALL CRLF

	; Display goodbye message :)
	MOV EDX, OFFSET goodbye
	CALL WriteString
	CALL CRLF
	CALL CRLF

	Invoke ExitProcess,0	; exit to operating system
main ENDP

; ---------------------------------------------------------------------------------
; Name: ReadVal
;
; Prompt the user for a signed 32-bit integer, retries if the user input isn't valid, and returns the 32-bit integer.
;
; Preconditions: none
;
; Postconditions: none
;
; Receives:
; [ebp+16] = address of invalidInputMsg
; [ebp+12] = address of SDWORD to store user input
; [ebp+8] = address of string to prompt user for input
;
; Returns:
; [ebp+12] = address of stored user input
; ---------------------------------------------------------------------------------

ReadVal PROC
	; Preservation + Set base pointer
	PUSH EBP
	MOV EBP, ESP
	PUSHAD
	PUSHFD

	; Create buffer for mGetString output
	SUB ESP, BUFFERSIZE
	MOV EDI, ESP

_getUserInput:
	; Get user input via mGetString
	mGetString [EBP+8], EDI, BUFFERSIZE, EBX

	; Check for empty input
	CMP EBX, 0
	JE _invalidInputError

	;	Convert and validate
	; Move ESI to final index of string, set direction flag to move backwards
	MOV ESI, EDI
	ADD ESI, EBX
	SUB ESI, 1
	STD

	MOV EAX, 0		; Will hold value after its pulled from the string and converted
	MOV ECX, EBX	; Will hold array length
	MOV EBX, 0		; Will hold sums of (each digit in string * the digit's place value)
	MOV EDX, 1		; Will hold place value multiplier
_nextChar:
	MOV EAX, 0		; Empty EAX out entirely as we'll be doing 32bit multiplication with it eventually
	LODSB

	; Check if current char is a sign (43d or 45d -- '+' or '-')
	CMP AL, 43
	JE _signChar
	CMP AL, 45
	JE _signChar

	; Check if current char is a number ([48d, 57d])
	CMP AL, 57
	JA _invalidInputError
	CMP AL, 48
	JB _invalidInputError

_numberChar:
	SUB AL, 48		; Convert ASCII digit to int
	PUSH EDX
	MUL EDX			; Multiply by its place value -- Save and restore EDX since its overwritten 
	POP EDX
	SUB EBX, EAX	; Add calc'd value to running sum (Sub instead of add so min_int can be valid, since otherwise we'd have issues with abs(min_int) > abs(max_int))
	JNS _invalidInputError
	; Increase place value multiplier
	MOV EAX, 10
	MUL EDX
	MOV EDX, EAX
	CMP ECX, 1
	JE _invert		; If at end (start) of string, make sure to invert back to positive since there was no sign char
	JMP _endOfCycle

_signChar:
	; Check that the sign is at the start of the string -- If there's stuff before it, the input is invalid
	CMP ECX, 1
	JNZ _invalidInputError
	
	; If '-', ignore since EBX is already negative
	CMP AL, 45
	JZ _endOfCycle
_invert:
	; If '+', mult by -1 to make positive (and check for overflow since abs(min_int) > abs(max_int))
	NEG EBX
	JO _invalidInputError

_endOfCycle:
	LOOP _nextChar

	JMP _storeOutput

_invalidInputError:
	; Display error message for invalid input
	MOV EDX, [EBP+16]
	CALL WriteString
	CALL CRLF
	JMP _getUserInput

_storeOutput:
	; Store in output variable
	MOV EAX, [EBP+12]
	MOV [EAX], EBX

	; Preservation + Dereference local variables
	ADD ESP, BUFFERSIZE
	POPFD
	POPAD
	POP EBP
	RET 8
ReadVal ENDP

; ---------------------------------------------------------------------------------
; Name: WriteVal
;
; Takes an SDWORD value and writes its ASCII symbols to the output.
;
; Preconditions: none
;
; Postconditions: A message will be written to the console.
;
; Receives:
; [ebp+8] = address of the SDWORD to write
;
; Returns:
; none
; ---------------------------------------------------------------------------------
WriteVal PROC
	; Preservation + Set EBP
	PUSH EBP
	MOV EBP, ESP
	PUSHAD
	PUSHFD

	; Repeatedly divide the int value by 10 and store the remainders.
	
	MOV EAX, [EBP+8]; Int value to repeatedly divide
	MOV EBX, BASE	; Divisor
	MOV ECX, 0		; Current digit

	SUB ESP, BUFFERSIZE
	MOV EDI, ESP	; Set EDI to start of array for reading back later 
	CLD

	; Check for negative
	CMP EAX, 0
	JNS	_divide		; If positive, skip writing a '-'
	NEG EAX			
	PUSH EAX
	MOV AL, '-'
	CALL WriteChar
	POP EAX

	MOV EDI, ESP
_divide:
	MOV EDX, 0		; Zero out the high 32 bits of division
	DIV EBX			
	MOV EBX, EAX	; Preserve EAX -- EBX was a constant so it doesn't matter
	MOV EAX, EDX	; Prep to store remainders
	STOSB			; Store the remainders
	MOV EAX, EBX	; Restore EAX
	MOV EBX, BASE	; Restore EBX
	INC ECX			; Increment current digit count
	CMP EAX, 0
	JNZ _divide

	; Read back the ints, add 48 (since 0d is 48 ascii), write that as char
	MOV ESI, EDI	; Set up to read from the array backwards. Using the stack would have made more sense here, but the assignment requires STO/LOD
	DEC ESI
	STD
	MOV EBX, 48 ; Ascii offset
_writeNextChar:
	
	LODSB
	ADD EAX, EBX
	CALL WriteChar
	LOOP _writeNextChar

	; Preservation + Dereference local variables
 	ADD ESP, BUFFERSIZE
	POPFD
	POPAD
	POP EBP
	RET 4
WriteVal ENDP


END main
