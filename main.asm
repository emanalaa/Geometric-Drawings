
INCLUDE Irvine32.inc

.DATA

maxSize equ 400 ;won't be changed
inputLines BYTE maxSize DUP(?)
fileName BYTE "lines.txt", 0 ;doesn't have to be a path if it's in the project's folder

pointsList BYTE maxSize DUP(?)
XList BYTE maxSize DUP(?)
YList BYTE maxSize DUP(?)


.CODE

main PROC
	CALL readLines
	exit
main ENDP

readLines PROC
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
readLines ENDP

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

END main