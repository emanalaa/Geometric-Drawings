
INCLUDE Irvine32.inc
.DATA
	inputLines BYTE 400 DUP(?)
	fileName BYTE "C:\Users\Mirna\Desktop\Assembly\Project\Sample Case\lines.txt", 0 ; based on the place pof the file on your pc. Should be changed later.
.code
main PROC
	CALL readFileWtv
	exit
main ENDP

readFileWtv PROC
	mov EDX, offset fileName ; file path
	CALL OpenFile ; returns filehandle in eax
	
	mov ECX, 400 ; maxmium number of bytes
	mov EDX, offset inputLines ; the buffer
	CALL ReadFromFile
	CALL WriteInt ; view number of bytes read (EAX)
	CALL CRLF

	mov EDX, offset inputLines
	mov ECX, EAX ; EAX = number of bytes read
	LoopWriteLines:
		mov AL, [EDX]
		CALL WriteChar
		; CALL CRLF
		add EDX, type inputLines
		loop LoopWriteLines

	ret
readFileWtv ENDP

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