
INCLUDE Irvine32.inc
.DATA

maxSize equ 400 ;won't be changed
actualSize BYTE ? ;actual number of bytes/coordinates (X's + Y's), will be determined during run-time (the converting PROC)
inputLines BYTE maxSize DUP(?)	; input buffer containing the characters in the file
fileName BYTE "lines.txt", 0 ;doesn't have to be a path if it's in the project's folder
	
pointsList BYTE maxSize DUP(?)	; contains the converted integers

xList BYTE 400 DUP(?)	; x coordinates {startL1, endL1, startL2, endL2, startL3, ...}
xNumberElement BYTE ?	; number of x-coordinates x2 (start + end)
yList BYTE 400 DUP(?)	; y coordinates {startL1, endL1, startL2, endL2, startL3, ...}
yNumberElement BYTE ?	; number of y-coordinates x2 (start + end)

.code
main PROC
	CALL ReadLines

	mov ESI, OFFSET inputLines	; list of characters
	mov EDI, OFFSET pointsList	; list to be filled with numbers
	CALL convertCharsToNumbers
	;call writeDec
	mov actualSize, AL ; EAX has the number of converted integers

	mov EDX, OFFSET pointsList
	movsx ECX, actualSize
	mov ESI, OFFSET xList
	mov EDI, OFFSET yList
	mov xNumberElement, 0
	mov yNumberElement, 0
	CALL divideIntoXY

exit
main ENDP

ReadLines PROC
	mov EDX, offset fileName 
	CALL OpenFile ;returns fileHandle in EAX
	
	mov ECX, maxSize
	mov EDX, offset inputLines ;the buffer
	CALL ReadFromFile
	;CALL WriteInt ;view number of bytes read (EAX)
	;CALL CRLF

	;mov EDX, offset inputLines
	;mov ECX, EAX ;EAX = number of bytes read
	;LoopWriteLines:
		;mov AL, [EDX]
		;CALL WriteChar
		;;CALL CRLF
		;add EDX, type inputLines
	;loop LoopWriteLines
ret
ReadLines ENDP

OpenFile PROC
;
; Opens a new text file and opens for input.
; Receives: EDX points to the filename.
; Returns: If the file was opened successfully, EAX
; contains a valid file handle. Otherwise, EAX equals
; INVALID_HANDLE_VALUE.
;------------------------------------------------------
INVOKE CreateFile,
	edx, GENERIC_READ, DO_NOT_SHARE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
ret
OpenFile ENDP

convertCharsToNumbers PROC USES EBX ECX EDX ESI EDI
;
; Converts a list of characters representing integer into their integer values.
; Receives: ESI points to the array of characters, EDI points to
; the array of numbers to fill.
; Returns: EAX containing the total number of the converted numbers.
; Requirements: All the lists must be lists of BYTES.
;------------------------------------------------------
	mov ECX, 0	; temporary counter of the integer elements
	mov BL, 0	; temporary value to update with the integer number
	mov EDX, 0	; counter spaces
	while_convertingChars:
		mov AL, [ESI]
		cmp AL, '*'
		je endwhile_convertingChars	; EOF character
		call IsDigit
		jz if_updateESIvalue ; contains a valid number
		cmp EDX, 0	; first space after a number
		jne endif_updateESIvalue
		
		; AL has the first space or enter, then move the previous integer value in the list
		mov [EDI], BL
		mov BL, 0 ; reset the temporary value
		inc ECX ; count an integer
		inc EDI	; go to the next index
		inc EDX ; count a space
		jmp endif_updateESIvalue
	
		; BL = (BL * 10) + AL = (result * 10) + digit
		if_updateESIvalue:
			sub AL, '0' ; convert the ASCII character to its digit value
			mov BH, AL	; save the  digit
			mov AL, BL	; needed for MUL 
			mov BL, 10	; needed for MUL
			mul BL	; (BL * 10) = (result * 10)
			add AL, BH ; (BL * 10) + BH --having value of AL--
			mov BL, AL ; update the original container
			mov EDX, 0 ; always update the spaces counter to zero
		endif_updateESIvalue:
		inc ESI ; go to the next character
	jmp while_convertingChars
	endwhile_convertingChars:
	mov EAX, ECX
ret
convertCharsToNumbers ENDP

divideIntoXY PROC USES EAX EDX ECX ESI EDI
;
; Divides a list of ponits stored as {x, y,x , y, ...} into 2 lists
; Receives: EDX points to array of points, ESI points to
;	array of x-coordinates, ESI points to array of y-coordinates
;	ECX containing number of elements in the array of points
; Returns: EAX containing the total number of the converted numbers.
; Requirements: All the lists must be lists of BYTES.
;------------------------------------------------------
	loop_xyDivision:
		; Line_i Start_x
		mov AL, [EDX]
		mov [ESI], AL
		inc ESI
		inc EDX
		; Line_i Start_y
		mov AL, [EDX]
		mov [EDI], AL
		inc EDI
		inc EDX
		; Line_i End_x
		mov AL, [EDX]
		mov [ESI], AL
		inc ESI
		inc EDX
		; Line_i End_y
		mov AL, [EDX]
		mov [EDI], AL
		inc EDI
		inc EDX

		add xNumberElement, 2
		add yNumberElement, 2
		sub ECX, 3
	LOOP loop_xyDivision
ret
divideIntoXY ENDP

END main