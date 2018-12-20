INCLUDE Irvine32.inc

.DATA

fileName BYTE "lines.txt", 0 ;doesn't have to be a path if it's in the project's folder

maxSize equ 400 ;won't be changed
actualSize BYTE ? ;actual number of bytes/coordinates (X's + Y's), will be determined during run-time (the converting PROC)
numOfPoints BYTE ? ;actualSize / 2
numOfLines BYTE ? ;numOfPoints / 2

inputLines BYTE maxSize DUP(?) ; input buffer containing the characters in the file
pointsList BYTE maxSize DUP(?) ; contains the converted integers
xList BYTE maxSize DUP(?) ; x coordinates {startL1, endL1, startL2, endL2, startL3, ...}
yList BYTE maxSize DUP(?) ; y coordinates {startL1, endL1, startL2, endL2, startL3, ...}

numOfBytesTriangles DWORD ?  ;used as an index with the offset of pointsTrianglesList
							 ;to know where to insert the next element
numOfPointsTriangles DWORD ? ;actual number of points
numOfTriangles DWORD ?
pointsTrianglesList DWORD maxSize DUP(?) ;indices of the points of the detected triangles in xList/yList


.CODE

main PROC
	CALL ReadLines

	mov ESI, OFFSET inputLines	; list of characters
	mov EDI, OFFSET pointsList	; list to be filled with numbers
	CALL ConvertCharsToNumbers
	;call writeDec
	mov actualSize, AL ; EAX has the number of converted integers

	mov EDX, OFFSET pointsList
	movsx ECX, actualSize
	mov ESI, OFFSET xList
	mov EDI, OFFSET yList
	;mov xNumberElement, 0
	;mov yNumberElement, 0
	CALL DivideIntoXY

	movzx EAX, actualSize
	CALL DivideByTwo      ;getting # points = # of bytes (actualSize) / 2
	mov numOfPoints, AL   ;EAX contains actualSize / 2
	CALL DivideByTwo      ;getting # of lines = # of points (EAX) / 2
	mov numOfLines, AL	  ;EAX contains numOfPoints / 2

	;movzx ECX, numOfPoints
	;mov EDX, offset xList
	;coutXs:
		;movzx EAX, BYTE ptr [EDX]
		;CALL WriteInt
		;CALL CRLF
		;inc EDX
	;LOOP coutXs

	;triangles + example of the output (to understand what my PROCs do)
	CALL FindTriangles
	mov EAX, numOfTriangles
	CALL WriteDec ;number of triangles
	CALL CRLF
	cmp numOfTriangles, 0
	JE noTrianglesFound
	mov ECX, numOfPointsTriangles
	mov EDX, offset pointsTrianglesList
	mov ESI, 0 ;points counter
	outputTriangles:
		mov AL, '('
		CALL WriteChar

		mov EBX, [EDX] ;index of the point in xList and yList
		mov EDI, offset xList
		add EDI, EBX
		movzx EAX, BYTE ptr [EDI]
		CALL WriteInt  ;x coordinate of the point
		
		mov AL, ','
		CALL WriteChar
		mov AL, ' '
		CALL WriteChar

		mov EDI, offset yList
		add EDI, EBX
		movzx EAX, BYTE ptr [EDI] ;y coordinate of the point
		CALL WriteInt

		mov AL, ')'
		CALL WriteChar

		CALL CRLF
		add EDX, type pointsTrianglesList

		inc ESI
		cmp ESI, 3
		JNE triangleNotDone
		mov ESI, 0
		CALL CRLF

		triangleNotDone:
	loop outputTriangles
	noTrianglesFound:

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

ConvertCharsToNumbers PROC USES EBX ECX EDX ESI EDI
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
		JE endwhile_convertingChars	; EOF character
		call IsDigit
		JZ if_updateESIvalue ; contains a valid number
		cmp EDX, 0	; first space after a number
		JNE endif_updateESIvalue

		; AL has the first space or enter, then move the previous integer value in the list
		mov [EDI], BL
		mov BL, 0 ; reset the temporary value
		inc ECX ; count an integer
		inc EDI	; go to the next index
		inc EDX ; count a space
		JMP endif_updateESIvalue

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
	JMP while_convertingChars
	endwhile_convertingChars:
	mov EAX, ECX
ret
ConvertCharsToNumbers ENDP

DivideIntoXY PROC USES EAX EDX ECX ESI EDI
;
; Divides a list of points stored as {x, y,x , y, ...} into 2 lists
; Receives: EDX points to array of points, ESI points to
;	array of x-coordinates, EDI points to array of y-coordinates
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

		;add xNumberElement, 2
		;add yNumberElement, 2
		sub ECX, 3
	LOOP loop_xyDivision
ret
DivideIntoXY ENDP

;
;Divides a number by two
;Receives: EAX: the number to divide (N)
;Returns: EAX: N/2
DivideByTwo PROC USES EBX EDX
	mov EDX, 0
	mov EBX, 2
	div EBX
ret
DivideByTwo ENDP

FindTriangles PROC
	mov numOfTriangles, 0
	mov numOfBytesTriangles, 0
	mov numOfPointsTriangles, 0
	movzx ECX, numOfLines ;iterating over all lines,
	sub ECX, 2			  ;except last 2 lines (because we need 3 lines to form a trianlge)
	;1st iteration: EAX (index of line 1) = 0
	;2nd iteration: EAX = 2 (because each line us represented with start and end, so 2 bytes)
	;so for each iteration: EAX += 2
	mov EAX, 0	;index of line1 (1st iteration of 1st loop, gets incremented by 2 after each iteration)
	loop_firstLineTri:
		push ECX

		mov EBX, EAX
		add EBX, 2 ;index of line2 = index of line1 + 2
		loop_secondLineTri:
			;check start1 and start2
			;check x's
			mov EDI, offset xList
			add EDI, EAX ;EDI = beginning + index of line1 (EAX)
			mov ESI, offset xList
			add ESI, EBX ;ESI = beginning + index of line2 (EBX)
			cmpsb ;check if both lines have the same start
			JNE checkEnds_FindTri
			;same x, check y's
			mov EDI, offset yList
			add EDI, EAX ;y coordinate of the start of line1, EAX = index of line1
			mov ESI, offset yList
			add ESI, EBX ;y coordiante of the start of line2, EBX = index of line2
			cmpsb
			JNE checkEnds_FindTri
			;the 2 lines have the same start point, so we need to find a line that has the other two end points
			;for the FindThirdLine PROC:
			;EDX = index of line to start checking from
			mov EDX, EBX
			add EDX, 2
			;EAX = index of pt1 (end pt. of line1)
			push EAX ;start pt. of line1
			inc EAX
			;EBX = index of pt2 (end pt. of line2)
			push EBX ;start pt. of line2
			inc EBX ;index of end of pt2
			CALL FindThirdLine_FindTri
			pop EBX
			pop EAX
			;result is returned in EDX
			cmp EDX, -1
			JE nextLine_FindTri
			mov EDI, offset pointsTrianglesList
			add EDI, numOfBytesTriangles
			mov [EDI], EAX ;start of line1 (intersection pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EAX
			add DWORD ptr [EDI], 1 ;end of line1 (2nd pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EBX
			add DWORD ptr [EDI], 1 ;end of line2 (3rd pt.)
			add numOfBytesTriangles, type pointsTrianglesList * 3
			add numOfPointsTriangles, 3
			inc numOfTriangles
			JMP nextLine_FindTri

			checkEnds_FindTri:
			mov EDI, offset xList
			add EDI, EAX ;x coordinate of the start point of line1
			inc EDI ;x coordinate of end point of line1
			mov ESI, offset xList
			add ESI, EBX ;x coordinate of the start point of line2
			inc ESI ;x coordinate of the end point of line2
			cmpsb
			JNE checkCrossStartEnd_FindTri
			mov EDI, offset yList
			add EDI, EAX
			inc EDI ;y coordinate of the end pt. of line1
			mov ESI, offset yList
			add ESI, EBX
			inc ESI ;y coordinate of the end pt. of line2
			cmpsb
			JNE checkCrossStartEnd_FindTri
			;EAX = line1 index
			;EBX = line2 index
			;EDX = EBX + 2 (line3 index, start searching from there)
			push EAX
			push EBX
			mov EDX, EBX
			add EDX, 2
			CALL FindThirdLine_FindTri
			pop EBX
			pop EAX
			cmp EDX, -1
			JE nextLine_FindTri
			mov EDI, offset pointsTrianglesList
			add EDI, numOfBytesTriangles
			mov [EDI], EAX ;start of line1 (1st pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EAX
			add DWORD ptr [EDI], 1 ;end of line1 (intersection pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EBX ;start of line2 (3rd pt.)
			add numOfBytesTriangles, type pointsTrianglesList * 3
			add numOfPointsTriangles, 3
			inc numOfTriangles
			JMP nextLine_FindTri

			checkCrossStartEnd_FindTri: ;check if start of line1 = end of line2
			mov EDI, offset xList
			add EDI, EAX ;x coordinate of the start point of line1
			mov ESI, offset xList
			add ESI, EBX
			inc ESI ;x coordinate of the end point of line2
			cmpsb
			JNE checkCrossEndStart_FindTri
			;same x's, check y's
			mov EDI, offset yList
			add EDI, EAX ;y coordinate of the start point of line1
			mov ESI, offset yList
			add ESI, EBX
			inc ESI ;y coordinate of the end point of line2
			cmpsb
			JNE checkCrossEndStart_FindTri
			;same x, same y (start of line1 = end of line2)
			;for FindThirdLine PROC:
			;EAX = end of line1
			;EBX = start of line2
			;EDX = start of line to start checking from (start of line2 + 2)
			push EAX
			push EBX
			inc EAX
			mov EDX, EBX
			add EDX, 2
			CALL FindThirdLine_FindTri
			pop EBX
			pop EAX
			cmp EDX, -1
			JE nextLine_FindTri
			mov EDI, offset pointsTrianglesList
			add EDI, numOfBytesTriangles
			mov [EDI], EAX ;start of line1 (intersection pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EAX
			add DWORD ptr [EDI], 1 ;end pt. of line1 (2nd pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EBX ;start of line2 (3rd pt.)
			add numOfBytesTriangles, type pointsTrianglesList * 3
			add numOfPointsTriangles, 3
			inc numOfTriangles
			JMP nextLine_FindTri

			checkCrossEndStart_FindTri:
			mov EDI, offset xList
			add EDI, EAX ;x coordinate of the start point of line1
			inc EDI ;x coordinate of the end point of line1
			mov ESI, offset xList
			add ESI, EBX ;x coordinate of the start point of line2
			cmpsb
			JNE nextLine_FindTri
			mov EDI, offset yList
			add EDI, EAX ;y coordinate of the start point of line1
			inc EDI ;y coordinate of the end point of line1
			mov ESI, offset yList
			add ESI, EBX ;y coordinate of the start point of line2
			cmpsb
			JNE nextLine_FindTri
			;end of line1 = start of line2
			;for FindThirdLine PROC:
			;EAX = start of line1
			;EBX = end of line2
			;EDX = start of line to start checking from (starts of line2 + 2)
			push EAX
			push EBX
			mov EDX, EBX
			add EDX, 2
			inc EBX
			CALL FindThirdLine_FindTri
			pop EBX
			pop EAX
			cmp EDX, -1
			JE nextLine_FindTri
			mov EDI, offset pointsTrianglesList
			add EDI, numOfBytesTriangles
			mov [EDI], EBX ;start of line2 (intersection pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EAX ;start of line1 (2nd pt.)
			add EDI, type pointsTrianglesList
			mov [EDI], EBX
			add DWORD ptr [EDI], 1 ;end of line2 (3rd pt.)
			add numOfBytesTriangles, type pointsTrianglesList * 3
			add numOfPointsTriangles, 3
			inc numOfTriangles

			nextLine_FindTri:
			add EBX, 2 ;x coordinate of the start of the next line in 2nd loop

		;LOOP loop_secondLineTri: JUMP TOO FAR, HAS TO BE WRITTEN EXPLICITLY
		dec ECX
		JNZ loop_secondLineTri

		add EAX, 2 ;next line in 1st loop
		pop ECX ;retrieve the original counter of 1st loop from stack

	;LOOP loop_firstLineTri: JUMP TOO FAR, HAS TO BE WRITTEN EXPLICITLY
	dec ECX
	JNZ loop_firstLineTri

	done_FindTri:
ret
FindTriangles ENDP

;
;After finding two lines intersecting at one point,
;this function searches for the third line that completes the triangle, if it exists,
;i.e. a line whose 2 points are equal to the 2 points in the parameters, order doesn't matter.
;Receives:  EAX contains the index of the 1st point
;			EBX contains the index of the 2nd point
;			EDX contains the index of the line to start checking from
;Returns:	EDX contains the index of the third line if found, else it contains -1
FindThirdLine_FindTri PROC USES ECX
		push EAX ;EAX is an input parameter, we need to keep its value
		mov EAX, EDX ;we need to divide the index of the line by 2
		CALL DivideByTwo ;EAX = EDX/2 = index/2
		movzx ECX, numOfLines
		sub ECX, EAX ;loopCounter = numOfLines - # of line to start checking from
					 ;# of the line = index of the line divided by two,
					 ;if index = 8, then it's the 4th line (because each line takes up 2 indices (start and end)
		pop EAX
		loop_FindThirdLine:
			;check start of current line with 1st pt (EDI)
			mov EDI, offset xList
			add EDI, EAX ;x coordinate of 1st pt.
			mov ESI, offset xList
			add ESI, EDX ;x coordinate of current line
			cmpsb
			JNE checkCurrEndWithFirstPt_FindThirdLine
			;same x, check y's
			mov EDI, offset yList
			add EDI, EAX ;y coordinate of 1st pt.
			mov ESI, offset yList
			add ESI, EDX ;y coordinate of current line
			cmpsb
			JNE checkCurrEndWithFirstPt_FindThirdLine
			;same x, same y, check end pt. of current line with 2nd pt.
			JMP checkCurrEndWithSecPt_FindThirdLine

			checkCurrEndWithFirstPt_FindThirdLine:
			;check x's
			mov EDI, offset xList
			add EDI, EAX ;x of 1st pt.
			mov ESI, offset xList
			add ESI, EDX ;x of curr start
			inc ESI ;x of curr end
			cmpsb
			JNE nextLine_FindThirdLine
			;same x, check y's
			mov EDI, offset yList
			add EDI, EAX ;y of 1st pt.
			mov ESI, offset yList
			add ESI, EDX ;y of curr start
			inc ESI ;y of curr end
			cmpsb
			JNE nextLine_FindThirdLine
			;current end = 1st point, then we need to check if current start = 2nd point
			JMP checkCurrStartWithSecPt_FindThirdLine

			checkCurrEndWithSecPt_FindThirdLine:
			mov ESI, offset xList
			add ESI, EBX ;x of 2nd pt.
			mov EDI, offset xList
			add EDI, EDX ;x of curr start
			inc EDI ;x of curr end
			cmpsb
			JNE nextLine_FindThirdLine
			;same x, check y's
			mov ESI, offset yList
			add ESI, EBX ;y of 2nd pt.
			mov EDI, offset yList
			add EDI, EDX ;y of curr start
			inc EDI ;y of curr end
			cmpsb
			JNE nextLine_FindThirdLine
			;FOUND THE LINE (curr start = 1st pt, curr end = 2nd pt)
			JMP foundThirdLine_FindThirdLine

			checkCurrStartWithSecPt_FindThirdLine:
			;check x's
			mov ESI, offset xList
			add ESI, EBX ;x of 2nd pt.
			mov EDI, offset xList
			add EDI, EDX ;x of curr start
			cmpsb
			JNE nextLine_FindThirdLine
			;same x, check y's
			mov ESI, offset yList
			add ESI, EBX ;y of 2nd pt.
			mov EDI, offset yList
			add EDI, EDX ;y of curr start
			cmpsb
			JNE nextLine_FindThirdLine
			;FOUND THE LINE (curr start = 2nd pt, curr end = 1st pt)
			JMP foundThirdLine_FindThirdLine

			nextLine_FindThirdLine:
			add EDX, 2
		;LOOP loop_FindThirdLine: JUMP TOO FAR, HAS TO BE WRITTEN EXPLICITLY
		dec ECX
		JNZ loop_FindThirdLine

		mov EDX, -1 ;third line not found
		
		foundThirdLine_FindThirdLine:
		;return, EDX contains the index of the found third line (hopefully :'D)
ret
FindThirdLine_FindTri ENDP

END main
