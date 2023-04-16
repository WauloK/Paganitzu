	CPU Z80
	 org $7b00
StartBlock7b00:
	jp block1
Screen_Loc:	dw	0
Screen_Data: dw  0
Screen_c:	db	0
Screen_ga:	db	0
Screen_u:	db	0
Screen_i:	db	0
Screen_h:	db	0
Screen_x:	db	$00
Screen_y:	db	$00
Screen_tab32:	dw $00, $20, $40, $60, $80, $a0, $c0, $e0
	dw $100, $120, $140, $160, $180, $1a0, $1c0, $1e0
	dw $200, $220, $240, $260, $280, $2a0, $2c0, $2e0
	dw $300, $320, $340, $360, $380, $3a0, $3c0, $3e0
	dw $400, $420, $440, $460, $480, $4a0, $4c0, $4e0
	dw $500, $520, $540, $560, $580, $5a0, $5c0, $5e0
	dw $600, $620, $640, $660, $680, $6a0, $6c0, $6e0
	dw $700, $720, $740, $760, $780, $7a0, $7c0, $7e0
Compression_in: dw  0
Compression_out: dw  0
Compression_temp:	dw	0
Font_text: dw  0
Font_currentFont: dw  0
Font_tx:	db	0
Font_ty:	db	0
Font_tpos:	dw	0
Font_tran:	db	0
Font_font1:	db $0, $0, $0, $0, $0, $10, $10, $10
	db $0, $10, $44, $44, $0, $0, $0, $44
	db $54, $44, $54, $44, $10, $54, $50, $14
	db $54, $44, $4, $10, $40, $44, $10, $44
	db $11, $44, $11, $10, $10, $0, $0, $0
	db $4, $10, $10, $10, $4, $40, $10, $10
	db $10, $40, $44, $10, $54, $10, $44, $0
	db $10, $54, $10, $0, $0, $0, $0, $10
	db $40, $0, $0, $54, $0, $0, $0, $0
	db $0, $0, $40, $4, $4, $10, $40, $40
	db $54, $44, $44, $44, $54, $10, $50, $10
	db $10, $54, $54, $4, $54, $40, $54, $54
	db $4, $14, $4, $54, $44, $44, $54, $4
	db $4, $54, $40, $54, $4, $54, $54, $40
	db $54, $44, $54, $54, $4, $4, $4, $4
	db $54, $44, $54, $44, $54, $54, $44, $54
	db $4, $4, $0, $40, $0, $40, $0, $0
	db $10, $0, $10, $40, $4, $10, $40, $10
	db $4, $0, $54, $0, $54, $0, $40, $10
	db $4, $10, $40, $54, $4, $10, $0, $10
	db $10, $44, $44, $40, $14, $10, $44, $54
	db $44, $44, $50, $44, $50, $44, $50, $14
	db $40, $40, $40, $14, $50, $44, $44, $44
	db $50, $54, $40, $50, $40, $54, $54, $40
	db $50, $40, $40, $14, $40, $44, $44, $14
	db $44, $44, $54, $44, $44, $54, $10, $10
	db $10, $54, $54, $4, $4, $44, $10, $44
	db $44, $50, $44, $44, $40, $40, $40, $40
	db $54, $44, $54, $44, $44, $44, $10, $44
	db $44, $44, $44, $10, $44, $44, $44, $10
	db $50, $44, $50, $40, $40, $10, $44, $44
	db $44, $14, $50, $44, $50, $44, $44, $14
	db $40, $10, $4, $50, $54, $10, $10, $10
	db $10, $44, $44, $44, $44, $14, $44, $44
	db $44, $44, $10, $44, $44, $44, $54, $44
	db $44, $44, $10, $44, $44, $44, $44, $44
	db $10, $10, $54, $4, $10, $40, $54, $54
	db $40, $40, $40, $54, $40, $40, $10, $4
	db $4, $54, $4, $4, $4, $54, $10, $44
	db $0, $0, $0, $0, $0, $0, $0, $54
	db $40, $10, $0, $0, $0, $0, $14, $44
	db $44, $14, $40, $50, $44, $44, $50, $0
	db $14, $40, $40, $14, $4, $14, $44, $44
	db $14, $10, $44, $50, $40, $14, $14, $10
	db $54, $10, $10, $10, $44, $14, $4, $50
	db $40, $40, $50, $44, $44, $10, $0, $10
	db $10, $10, $4, $0, $4, $44, $10, $40
	db $44, $50, $44, $44, $10, $10, $10, $10
	db $10, $0, $44, $54, $44, $44, $0, $10
	db $44, $44, $44, $0, $10, $44, $44, $10
	db $0, $10, $44, $50, $40, $0, $10, $44
	db $14, $4, $0, $10, $44, $40, $40, $0
	db $14, $50, $14, $50, $40, $50, $40, $44
	db $10, $0, $44, $44, $44, $14, $0, $44
	db $44, $44, $10, $0, $44, $44, $54, $44
	db $0, $44, $10, $10, $44, $0, $44, $14
	db $4, $10, $0, $54, $14, $50, $54, $14
	db $10, $40, $10, $14, $10, $10, $10, $10
	db $10, $50, $10, $4, $10, $50, $11, $44
	db $0, $0, $0
Input_c:	db	$00
Sound_freq:	dw	0
Sound_dur:	dw	0
Functions_i:	dw	0
Functions_j:	db	0
Functions_s: dw  0
Sprite_spritex:	db	0
Sprite_spritey:	db	0
Sprite_no:	db	0
Sprite_spritedatasize:	db	0
Sprite_spritewidth:	db	0
Sprite_spriteheight:	db	0
Sprite_x:	db	0
Sprite_y:	db	0
Sprite_loc:	dw	0
Sprite_spritedata: dw  0
Sprite_tran:	db	0
	; Declaring string asmz80
mychar:		db	"A"
	db	0
myp: dw  0
mym: dw  0
strpos:	db	0
varPrefixed_p:	db	0
q:	db	0
u:	db	0
v:	db	0
	; Declaring string asmz80
message:		db	"Press 'S' or Fire to start! Welcome to Paganitzu! Use Joysticks or Keys: QA = Up/Down M, = Left/Right. Check out BlueBilby.com ... "
	db	0
strlen:	db	0
titledata:
	incbin	 "D:/VZ/TRSE_Mysrc/Paganitzu///trse.bin_c"
end_incbin_titledata:
gameStats_gameStats_record_gameStats_record_lives	db	0
gameStats_gameStats_record_gameStats_record_bonus	dw	0
gameStats_gameStats_record_gameStats_record_keys	db	0
gameStats_gameStats_record_gameStats_record_room	db	0
gameStats_gameStats_record_gameStats_record_score	dw	0
gameStats_gameStats_record_gameStats_record_hiScore	dw	0
gameStats_gameStats_record_gameStats_record_gameRunning	db	0
gameStats_gameStats_record_gameStats_record_titlePlayed	db	0
HUDData:	db $3f, $ff, $ff, $ff, $ff, $ff, $3f, $5f
	db $df, $df, $7f, $7f, $3f, $77, $77, $77
	db $5d, $7f, $3f, $5f, $77, $77, $77, $7f
	db $3f, $77, $77, $77, $7f, $7f, $3f, $77
	db $df, $df, $7f, $7f, $3f, $ff, $ff, $ff
	db $ff, $ff, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $3f, $ff
	db $ff, $ff, $ff, $ff, $3f, $5f, $7f, $7d
	db $7d, $5f, $3d, $fd, $dd, $dd, $dd, $ff
	db $3f, $7d, $fd, $dd, $7d, $7f, $3f, $dd
	db $dd, $dd, $dd, $ff, $3d, $7f, $7f, $7d
	db $dd, $5f, $3f, $ff, $ff, $ff, $ff, $ff
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $3f, $ff, $ff, $ff
	db $ff, $ff, $3d, $fd, $5d, $dd, $5f, $5f
	db $3d, $ff, $7d, $dd, $fd, $ff, $3d, $ff
	db $7d, $dd, $7f, $7f, $3d, $ff, $7d, $dd
	db $ff, $df, $3d, $5d, $5f, $7d, $5d, $7f
	db $3f, $ff, $ff, $ff, $ff, $ff, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $3f, $ff, $ff, $ff, $ff, $ff
	db $3f, $dd, $d5, $dd, $f5, $ff, $3f, $dd
	db $df, $dd, $df, $ff, $3f, $d7, $d7, $d5
	db $f7, $ff, $3f, $dd, $df, $f7, $fd, $ff
	db $3f, $dd, $d5, $f7, $d7, $ff, $3f, $ff
	db $ff, $ff, $ff, $ff, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $3f, $ff, $ff, $ff, $ff, $ff, $3d, $7f
	db $7f, $7d, $df, $5f, $3d, $dd, $dd, $dd
	db $dd, $ff, $3d, $7d, $dd, $dd, $df, $7f
	db $3d, $dd, $dd, $dd, $df, $df, $3d, $7f
	db $7d, $df, $7d, $7f, $3f, $ff, $ff, $ff
	db $ff, $ff, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	db $0, $0, $0, $0, $0, $0, $0, $0
	
; //	pixelTab : array[4] of byte = (0,1,2,3); 
; //	Clears the screen using mode parameter 0 or 1
; //	
	; ***********  Defining procedure : Screen_Cls
	;    Procedure type : User-defined procedure
 ; Temp vars section
 ; Temp vars section ends
Screen_Cls_block2:
Screen_Cls:
	; ****** Inline assembler section
		ld a, [Screen_u]
		cp #00
		jp z,mode0
		xor a
		ld bc,#0800
		jr cls
mode0
		ld a,#20
		ld bc,#0200
cls
		ld hl,#7000
		ld de,#7001
		ld (hl),a
		ldir
		jr clsend
clsend
	
	ret
end_procedure_Screen_Cls:
	;*
; //Sets the VZ200 resolution mode.
; //<ul>
; //<li>0: Text mode</li>
; //<li>1: Graphics mode 128x64</li>
; //</ul>
; 

	; ***********  Defining procedure : Screen_SetMode
	;    Procedure type : User-defined procedure
Screen_SetMode_block3:
Screen_SetMode:
	; ****** Inline assembler section
		ld a,[Screen_ga]
		cp #00
		jr nz,md1
		ld a,($783B)
		res 3,a
		ld (#6800),a
		ld (#783B),a
		ret
md1		
		ld a,(#783B)
		set 3,a
		ld (#6800),a
		ld (#783B),a
	
	ret
end_procedure_Screen_SetMode:
	;*
; //	Set pen colour (0-3) for plotting pixels in mode(1)
; //	*
	; ***********  Defining procedure : Screen_SetPen
	;    Procedure type : User-defined procedure
Screen_SetPen_block4:
Screen_SetPen:
	; ****** Inline assembler section
		ld a,[Screen_c]
		ld (#7846),a
		
	ret
end_procedure_Screen_SetPen:
	;*
; //	Set paper (background) color 0 or 1
; //	*
	; ***********  Defining procedure : Screen_SetPaper
	;    Procedure type : User-defined procedure
Screen_SetPaper_block5:
Screen_SetPaper:
	; ****** Inline assembler section
		ld a,[Screen_c]
		cp #00
		jr z,bg1
		ld a,(#783B)
		set 4,a
		ld (#6800),a
		ld (#783B),a
		ret
bg1		
		ld a,(#783B)
		res 4,a
		ld (#6800),a
		ld (#783B),a
	
	ret
end_procedure_Screen_SetPaper:
	; ***********  Defining procedure : Screen_PutPixel
	;    Procedure type : User-defined procedure
Screen_PutPixel_block6:
Screen_PutPixel:
	; ****** Inline assembler section
	ld a,(#7846)
	ld c,a
	ld      a, [Screen_y]            ; get y
	ld 			h,a
	cp      #40             ; >= 64 ?
	jr      nc, psetx       ; nah, wont pset there
	ld      a, [Screen_x]            ; get x
	ld      l,a
	cp      #80            ; >= 128 ?
	jr      nc, psetx
	sla     l               ; calculate screen offset
	srl     h
	rr      l
	srl     h
	rr      l
	srl     h
	rr      l
	and     #03              ; pixel offset
	inc     a
	ld      b, #fc
pset1   rrc     b
	rrc     b
	rrc     c
	rrc     c
	dec     a
	jr      nz, pset1
	ld	de, #7000
	add	hl, de
	ld      a, (hl)
	and     b
	or      c
	ld      (hl), a
psetx
	
	ret
end_procedure_Screen_PutPixel:
	;*
; //	1 pixel smooth scroll in mode(1)
; //	First parameter is right-most char onscreen of starting location
; //	Second parameter is the height of the scroll
; //	*
	; ***********  Defining procedure : Screen_doSmoothScroll
	;    Procedure type : User-defined procedure
Screen_doSmoothScroll_block7:
Screen_doSmoothScroll:
	; ****** Inline assembler section
  ld hl,[Screen_Loc]
	ld a,[Screen_h]
	ld c,a
nextline	
	xor a
  ld b,#20
doscroll
	rl (hl)
	rla
	rl (hl)
	rra
	dec hl
  djnz doscroll
	ld de,#0040
	add hl,de
	dec c
	jr nz,nextline
	ret
end_procedure_Screen_doSmoothScroll:
	;*
; //Turns off interrupts. Same as "di"
; 

	; ***********  Defining procedure : Compression_Decompress
	;    Procedure type : User-defined procedure
Compression_Decompress_block8:
Compression_Decompress:
	; ****** Inline assembler section
 
; HL - pointer to the buffer with compressed source data
; DE - pointer to the destination buffer for decompressed data
; BC - size of the compressed data
	ld hl,[Compression_in]
	ld de,[Compression_out]
	
	; ****** Inline assembler section
 
	
;
; LZ4 decompression algorithm - Copyright (c) 2011-2015, Yann Collet
; All rights reserved. 
; LZ4 implementation for z80 and compatible processors - Copyright (c) 2013-2015 Piotr Drapich
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, 
; are permitted provided that the following conditions are met: 
; 
; * Redistributions of source code must retain the above copyright notice, this 
;   list of conditions and the following disclaimer. 
; 
; * Redistributions in binary form must reproduce the above copyright notice, this 
;   list of conditions and the following disclaimer in the documentation and/or 
;   other materials provided with the distribution. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND 
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
;
; The latest version always available from http://www.union.org.pl/download/z80
; Questions, ideas, optimization suggestions, consulting requests? Send email to union@union.pl
; 
; Only 219 bytes of fully relocatable code with raw data decompression routine taking only 109 bytes.
; Supports both legacy and latest LZ4 data format without framing support.
; History:
; version 1.0 (18.09.2013)
; - initial implementation for legacy compression formats, generated with lz4 1.3.3 or earlier
; version 1.1 (28.02.2015)
; - added support for files, compressed with the lz4 version 1.41
; Available functions:
; LZ4_decompress
; - decompresses files, packed with lz4 command line tool, preferably with options -Sx -B4
; input parameters:
; HL - pointer to the buffer with compressed source data
; DE - pointer to the destination buffer for decompressed data
; on exit:
; A  - contains exit code: 
; 	0 - decompression successful
;	1 - compressed size is bigger than 64kb
;	2 - unsupported version of lz4 compression format
; HL - the number of decompressed bytes
; Contents of AF,BC,DE,HL are not preserved.
; LZ4_decompress_raw
; - decompresses raw LZ4 compressed data 
; input parameters:
; HL - pointer to the buffer with compressed source data
; DE - pointer to the destination buffer for decompressed data
; BC - size of the compressed data
; on exit:
; A  - exit code: 
; 	0 - decompression successful
; HL - the number of decompressed bytes
LZ4_decompress:
; check the magic number
	ld 		bc,0
	ld		a,(hl)
	cp 		#4
	jr 		z, LZ4_Version4
	cp 		#3
	jr 		z, LZ4_LegacyVersion3
	cp 		#2
	jr 		z, LZ4_LegacyVersion2
LZ4_version_not_supported:
	ld 		a,2
	jr		LZ4_decompress_finished
LZ4_decompress_error:
	ld 		a,1
LZ4_decompress_finished:
	ret
LZ4_Version4:
; check version 1.41 magic 
	inc		hl
	ld 		a,(hl)
	inc		hl
	cp		#$22
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$4D
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$18
	jr		nz, LZ4_version_not_supported
; parse version 1.41 spec header
	ld		a,(hl)
	inc		hl
; check version bits for version 01
	bit		7,a
	jr		nz, LZ4_version_not_supported
	bit		6,a
	jr		z, LZ4_version_not_supported
; is content size set?
	bit		3,a
	jr		z, LZ4_no_content_size
; skip content size
	ld		c,8
LZ4_no_content_size:
	bit		0,a
	jr		z, LZ4_no_preset_dictionary
; skip dictionary id
	ld		a,c
	add		a,4
	ld		c,a
LZ4_no_preset_dictionary:
	ld		a,(hl)
	inc		hl
; strip reserved bits (and #70) and check if block max size is set to 64kb (4)
	and		#$40
	jr		z, LZ4_version_not_supported
; skip header checksum
	inc		bc
	jr		LZ4_start_decompression
LZ4_LegacyVersion3:
	ld		c,8
LZ4_LegacyVersion2:
	inc		hl
	ld		a,(hl)
	inc		hl
	cp		#$21
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$4c
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$18
	jr		nz, LZ4_version_not_supported
LZ4_start_decompression:
	add		hl,bc
; load low 16 bit of compreesed block size to bc
	ld		c,(hl)
	inc		hl
	ld		b,(hl)
	inc		hl
; check if compressed size <64kb - high 16 bits of compressed size must be 0
	ld		a,(hl)
	cp		0
	jr		nz,LZ4_decompress_error
	inc		hl
	ld		a,(hl)
	cp		0
	jr		nz,LZ4_decompress_error
	inc		hl
; decompress raw lz4 data packet
; on entry hl - start of packed buffer, de - destination buffer, bc - size of packed data
LZ4_decompress_raw:
	push	de							; store original destination pointer
	push	hl							; store start of compressed data source
	add		hl,bc       				; calculate end address of compressed block
	ld		b,h							; move end address of compressed data to bc
	ld		c,l	
	pop		hl							; restore start of compressed data source
	push	bc							; store end address of compessed data
; now hl - start of packed buffer, de - destination, bc - end of packed buffer
	ld		b,0		         			; clear b, c is set later
; get decompression token
LZ4_GetToken:
	xor		a 							; reset c flag for sbc later
	ld		a,(hl)						; read token
	inc		hl
	push	af							; store token
; unpack 4 high bits to get the length of literal
	rlca
	rlca
	rlca
	rlca
; copy literals
	and		#$f							; token can be max 15 - mask out unimportant bits
	jr		z,LZ4_skipcalc   			; there is no literals, skip calculation of literal size
	ld		c,a							; set the count for calculation
	cp		#$f							; if literal size <15
	jr		nz, LZ4_copyliterals		; copy literal, else
; calculate total literal size by adding contents of following bytes
	push	de							; store destination
	ex		de,hl
; a = size of literal to copy, de=pointer to data to be added
	ld		h,0         				; set hl with size of literal to copy 
	ld		l,a
LZ4_calcloop:
	ld		a,(de)						; get additional literal size to add 
	inc		de
	ld		c,a							; set bc to the length of literal
	add		hl,bc						; add it to the total literal length
	cp		#$ff							; if literal=255
	jr		z,LZ4_calcloop				; continue calculating the total literal size
	ld		b,h							; store total literal size to copy in bc
	ld		c,l
	ex		de,hl						; hl now contains current compressed data pointer  
	pop		de							; restore destination to de 
LZ4_copyliterals:
	ldir								; copy literal to destination
LZ4_skipcalc:
; check for end of compressed data
	pop		af							; restore token, carry is cleared because of xor a at the beginning of GetToken
	pop		bc							; restore end address of compressed data 
	push	hl							; store current compressed data pointer 
	sbc		hl, bc						; check if we reached the end of compressed data buffer
	pop		hl							; restore current compressed data pointer
	jr		z,LZ4_decompress_success	; decompression finished
	push	bc							; store end address of compressed data
; Copy Matches
	and		#$f							; token can be max 15 - mask out unimportant bits. resets also c flag for sbc later
; get the offset
	ld		c,(hl)
	inc		hl
	ld		b,(hl)						; bc now contains the offset
	inc		hl
	push	hl							; store current compressed data pointer
	push	de							; store destination pointer
	ex		de,hl
	sbc		hl,bc   					; calculate from the offset the new decompressed data source to copy from
; hl contains new copy source, de source ptr
	ld		b,0     					; load bc with the token
	ld		c,a
	cp		#$f							; if matchlength <15
	jr nz, LZ4_copymatches				; copy matches. else 
; calculate total matchlength by adding additional bytes
	push	hl							; store current decompressed data source
; a = size of match to copy, de= pointer to data to be added
	ld		h,0     					; set hl with initial matchlength to copy
	ld		l,a
LZ4_calcloop2:
	ld		a,(de)						; get additional matchlength to add
	inc		de
	ld		c,a							; set bc to the matchlength
	add		hl,bc						; add it to the total match length
	cp		#$ff							; if matchlength=255
	jr		z,LZ4_calcloop2				; continue calculating the total match length		
	ld		b,h							; store total matchlength to copy in bc
	ld		c,l			 
	pop		hl							; restore current decompressed data source
	pop		af							; set stack to proper position by restoring destination pointer temporarily into af  
	ex		de,hl
	ex		(sp),hl						; update current compressed data pointer on the stack to the new value from de
	ex		de,hl 
	push	af							; restore stack
LZ4_copymatches:
	pop		de							; restore destination pointer
	inc		bc							; add base length of 4 to get the correct size of matchlength 
	inc		bc
	inc		bc
	inc		bc
	ldir								; copy match
	pop		hl							; restore current compressed data source
	jr		LZ4_GetToken				; continue decompression
LZ4_decompress_success:
	pop		hl							; store destination pointer 
	sbc		hl,de						; calculate the number of decompressed bytes 
	xor		a							; clear exit code
	ret
	 
	ret
end_procedure_Compression_Decompress:
	;*
; //	Sets the current font 
; //
; 

	; ***********  Defining procedure : Font_SetFont
	;    Procedure type : User-defined procedure
Font_SetFont_block9:
Font_SetFont:
	ret
end_procedure_Font_SetFont:
	; ***********  Defining procedure : Font_DrawTextAt
	;    Procedure type : User-defined procedure
Font_DrawTextAt_block10:
Font_DrawTextAt:
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[Font_tx]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[Font_ty]
	ld e,a ; variable is 8-bit
	ld d,0 ;keep
	ld hl,Screen_tab32
	add hl,de
	; TYPETEST : INTEGER  WriteType : NADA
	add hl,de
	ld a,[hl]
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Loading pointer
	ld [Font_tpos],hl
	; ****** Inline assembler section
    ld de,[Font_text]
    ld hl,[Font_tpos]
    push de         ; String to display, first char
    push hl         ; Location onscreen to display char
strngloop    
    ld a,(de)       ; Get string char
endloop    
    cp #00          ; End of string?
    jp z,enddraw    ; Yes, stop drawing.
    sub #20         ; No, get index of fontdata for char
    ld d,#00
    ld e,a
    ld h,#00
    ld l,a   
    add hl,de
    add hl,de
    add hl,de
    add hl,de
    push hl
    pop bc
    ld hl,[Font_currentFont]  ; Start of fontdata to draw
    add hl,bc       ; hl points to correct letter to draw
    push hl
    ld a,#05        ; Number of lines down to draw font letter
drawloop 
    ld (loopcnt),a
    ld a,(hl)
    push af         ; Save first byte to draw
    ld a,(#7846) 
    cp #00
    jp z,enddraw
    cp #01
    jr z,drawletter
    cp #02
    jr z,addblue
    pop af
    ld d,a
    sla a
    add a,d
    jr drawletter2  ; Font byte is coloured red
addblue
    xor a
    pop af
    rl a
    jr drawletter2
drawletter
    pop af
drawletter2    
    pop bc          ; Font data pointer          
    pop hl          ; Coords to display fontbyte
    ld d,a
    ld a,[Font_tran]
    cp #01
    jr z,trans      ; Draw transparent
    ld a,d
    jr drawtext
trans
    ld a,(hl)
    xor d
drawtext
    ld (hl),a       ; Put it there
    ld de,#0020
    add hl,de       ; Move screen coords pointer down 1 line
    inc bc
    push hl         ; Screen pointer
    push bc         ; Font data pointer
    ld l,c          ; hl = font pointer
    ld h,b
    ld a,(loopcnt)
    dec a
    cp #00
    jr nz,drawloop  ; Works to here
    pop bc
    pop hl
    ld bc,#009f
    xor a
    sbc hl,bc
    pop de          ; String pointer
    inc de
    push de         ; Next char
    push hl         ; Screen pointer   
    jp strngloop
loopcnt db $00
enddraw  
  pop hl
  pop hl  ; Clear the stack!
  
	ret
end_procedure_Font_DrawTextAt:
	;*
; //  Get keyboard key pressed
; //  *
	; ***********  Defining procedure : Input_GetPressedKey
	;    Procedure type : User-defined procedure
Input_GetPressedKey:
	; ****** Inline assembler section
        
; Taken from MPAGD by Jonathan Cauldwell; VZ keyboard routine by Kees van Oss.
; Detect keypress.
; Note that each key causes a logic 0 to appear at the bit position shown, when its row address is read.
;       I/O Address -----------------------------------------------
;       (Selector)  bit 7 bit 6 bit 5  bit 4  bit 3   bit 2  bit 1  bit 0
;row 0  0x68FE  	N/A   N/A   R      Q      E              W      T	1111 1110
;row 1  0x68FD  	N/A   N/A   F      A      D       CTRL   S      G	1111 1101
;row 2  0x68FB  	N/A   N/A   V      Z      C       SHIFT  X      B	1111 1011
;row 3  0x68F7  	N/A   N/A   4      1      3              2      5	1111 0111
;row 4  0x68EF  	N/A   N/A   M      SPACE  ,              .      N	1110 1111
;row 5  0x68DF  	N/A   N/A   7      0      8       -      9      6	1101 1111
;row 6  0x68BF  	N/A   N/A   U      P      I       RETURN O      Y	1011 1111
;row 7  0x687F  	N/A   N/A   J      ;      K       :      L      H	0111 1111
;
; If the '2' key were pressed, it would cause bit 1 at address 68F7H to drop to 0.
; The data retrieved by reading that address, neglecting the 2 most significant bits which are not driven by the keyboard, would be 3DH (binary 111101).
; Wait for keypress.
prskey	
	ld b,#01		    ; reset row
	ld hl,#68fe	    ; high byte of port to read.
; Check every row
prskey0
  ld a,l		      ; low byte
	rrca		        ; Adjust lb port address
	ld l,a
	ld a,(hl)	      ; read key
	and #3f
	cp #3f		      ; Key pressed?
	jr nz,prskey1	  ; Yes, exit
	inc b		        ; increment row counter
	ld a,b
	cp #09		      ; last row checked?
	jr nz,prskey0	  ; no, repeat
	ret     	      ; yes, no key pressed, check again
; Determine column
prskey1
  ld d,a
	ld c,1		      ; reset column
prskey2
  sra d		        ; rotate bit out
	jr nc,prskey4	  ; key pressed, exit
	inc c		        ; increment column counter
	ld a,c
	cp 7		        ; last column checked?
	jr nz,prskey2	  ; no, repeat
prskey3
  jr prskey	      ; yes, no key pressed, exit
; Key pressed, create keycode
prskey4	ld a,c		; high nibble=row
	sla a
	sla a
	sla a
	sla a
	add a,b		      ; low nibble=column
	push af
	pop af
  ld [Input_c],a
keyend
  
	ld a,[Input_c]
	ret
end_procedure_Input_GetPressedKey:
	;*
; //  Get joystick direction/fire pressed
; //  *
	; ***********  Defining procedure : Input_GetJoystick
	;    Procedure type : User-defined procedure
Input_GetJoystick_block12:
Input_GetJoystick:
	; ****** Inline assembler section
   
;--------------------------------------------------------
; Joystick
;
; Out: joyval=xx5FUDLR (bit cleared if key pressed)
;             ||||||||
;             |||||||+> Right      
;             ||||||+-> Left       
;             |||||+--> Down      
;             ||||+---> Up        
;             |||+----> Fire1     
;             ||+-----> Fire2    
;             |+------> Not used
;             +-------> Not used
;--------------------------------------------------------
; Joystick reading routines.
; The two Joystick units are connected to a plug-in module that
; contains I/O address decoding and switch matrix encoding.
; IC U2 (74LS138) enables I/O reads between 20 - 2F Hex.
; Address lines AO - A3 are used separately to generate active LOW signals
; on the joystick or switch to be read.
; Switch state is then read at the resultant address from Data bits DO - D4.
; When a switch is ON it provides an active-low Data bit. 
;
; JOY1 0x2E    JOY2 0x2B
; U    0xFE    U    0xFE   1111 1110	
; D    0xFD    D    0xFD   1111 1101   
; L    0xFB    L    0xFB   1111 1011   
; R    0xF7    R    0xF7   1111 0111   
; FIRE 0xEF    FIRE 0xEF   1110 1111   
; 'Arm'0x2D (joy1 button 2)
; FIRE 0xEF                1110 1111   
; 'Arm'0x27 (joy2 button 2)
;              FIRE 0xEF   1110 1111 
  ld a,[Input_c]      ; control flag.
  dec a               ; is it joystick 1?
  jr z,joy1           ; yes, read it.
  dec a               ; is it joystick 2?
  jr z,joy2           ; yes, read it.
  jr joyend
; Joystick 1.
joy1
  in a,(#2e)	             ; read joystick1
	call readjoy	           ; convert to joyval
	in a,(#2d)	             ; Read arm button joystick1
	jr readarm
; Joystick 2.
joy2
  in a,(#2b)	             ; read joystick2
	call readjoy	           ; convert to joyval
	in a,(#27)	             ; Read arm button joystick1
	jr readarm
readjoy
  ld b,#05		             ; read 5 bits from joystick
read0
  sra a	
	ccf		                   ; complement the result (0=not pressed,1=pressed).
	rl e
	djnz read0
	rrc e		                 ; convert VZ values to Kempston
	jr nc,rstfire
	set 4,e
	jr joyend
rstfire
  res 4,e
  jr joyend
readarm
  and #10		              ; read arm button
	jr z, joy1a
	res 5,e		              ; Not pressed, carry clear
	jr joy1b
joy1a
  set 5,e		              ; Pressed, carry set
joy1b
	set 6,e
	jr joy1d
joy1c
  res 6,e
joy1d
  ld a,e                   ; copy e register to accumulator.
  ld [Input_c],a            ; remember value.
joyend
	ld a,[Input_c]
	ret
end_procedure_Input_GetJoystick:
	; ***********  Defining procedure : Sound_Play
	;    Procedure type : User-defined procedure
Sound_Play_block13:
Sound_Play:
	; ****** Inline assembler section
  ld hl,[Sound_freq]
  ld bc,[Sound_dur]
  call #345c
  
	ret
end_procedure_Sound_Play:
	;* 
; //  Returns the size of a string in byte format
; //  *
	; ***********  Defining procedure : Functions_SizeOfString
	;    Procedure type : User-defined procedure
Functions_SizeOfString_block14:
Functions_SizeOfString:
	ld a, $0
	ld [Functions_j], a
Functions_SizeOfString_while15:
Functions_SizeOfString_loopstart19:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; Optimization : zp[0]
	ld hl,[Functions_s]
	ld a,[hl]
	or a
	jr z, Functions_SizeOfString_elsedoneblock18
Functions_SizeOfString_ConditionalTrueBlock16: ;Main true block ;keep :
	; ;generic pointer/integer P:=P+(expr) add expression
	; RHS is pure 
	ld de,$1
	ld hl,[Functions_s]
	add  hl,de
	ld [Functions_s],hl
	; 'a:=a + const'  optimization 
	ld a,[Functions_j]
	add  a,$1
	ld [Functions_j], a
	jr Functions_SizeOfString_while15
Functions_SizeOfString_elsedoneblock18:
Functions_SizeOfString_loopend20:
	ld a,[Functions_j]
	ret
end_procedure_Functions_SizeOfString:
	; ***********  Defining procedure : Sprite_SetSize
	;    Procedure type : User-defined procedure
Sprite_SetSize_block23:
Sprite_SetSize:
	; generic assign 
	; Swapping nodes
	; Generic mul
	ld a,[Sprite_spritewidth]
	ld e,a
	ld d,0
	ld a,[Sprite_spriteheight]
	ld h,a
	ld l,0
	call mul_8x8
	ld a,l
	ld [Sprite_spritedatasize], a
	ret
end_procedure_Sprite_SetSize:
	;*
; //	Sets the sprite data
; //
; 

	; ***********  Defining procedure : Sprite_SetData
	;    Procedure type : User-defined procedure
Sprite_SetData_block25:
Sprite_SetData:
	ret
end_procedure_Sprite_SetData:
	; ***********  Defining procedure : Sprite_DrawAt
	;    Procedure type : User-defined procedure
Sprite_DrawAt_block26:
Sprite_DrawAt:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[Sprite_x]
	cp $1f
	jr c, Sprite_DrawAt_localfailed32
	jr z, Sprite_DrawAt_localfailed32
	jr Sprite_DrawAt_ConditionalTrueBlock28
Sprite_DrawAt_localfailed32: ;keep:
	; ; logical OR, second chance
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[Sprite_y]
	cp $3f
	jr c, Sprite_DrawAt_elsedoneblock30
	jr z, Sprite_DrawAt_elsedoneblock30
Sprite_DrawAt_ConditionalTrueBlock28: ;Main true block ;keep :
	
; // Exit if out of bounds
	ret
Sprite_DrawAt_elsedoneblock30:
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[Sprite_spritex]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[Sprite_spritey]
	ld e,a ; variable is 8-bit
	ld d,0 ;keep
	ld hl,Screen_tab32
	add hl,de
	; TYPETEST : INTEGER  WriteType : NADA
	add hl,de
	ld a,[hl]
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Loading pointer
	ld [Sprite_loc],hl
	; ****** Inline assembler section
  ld a,[Sprite_no]        ; a = spriteno
  ld c,a                  ; c = spriteno  **
  ld a,[Sprite_loc+1]
  ld d,a 
  ld a,[Sprite_loc]
  ld e,a                  ; de = sprite screen location
  push de
  ld a,[Sprite_spriteheight]
  ld [hcount],a            
  ld hl,[Sprite_spritedata]
  ld a,#00
  cp c
  jr z,displaysprite      ; if spriteno = 0, just display it
  ld a,[Sprite_spritedatasize]  ; sprite data amount per sprite                
  ld e,a 
  ld b,#00
sprite_mult:                
  add hl,bc
  dec e
  jr nz,sprite_mult       ; hl = start of spriteno spritedata 
displaysprite:
  pop bc                  ; bc = sprite screen location
iterate_height:
  ld a,[Sprite_spritewidth]     ; sprite width
  ld d,a                  ; d = sprite width
iterate_width:
  ld a,(hl)
  ld e,a
  ld a,[Sprite_tran]
  cp #01
  jr z,transp
  ld a,e
  jr drawsprite
transp 
  ld a,(bc)
  xor e
drawsprite  
  ld (bc),a
  inc hl
  inc bc
  dec d
  jr nz,iterate_width     ; display all sprite data in x dimension
  push hl                 ; save hl (sprite data position) cos only hl can be used in 16bit adding
  ld hl,#0000
  add hl,bc               ; put screen location into hl
  ld de,#0020            
  add hl,de               ; add one screen line down
  ld a,[Sprite_spritewidth]     ; sprite width
  ld d,a                  ; d = sprite width
subtract_width:
  dec hl                  ; move hl=bc to start of new line
  dec d
  jr nz,subtract_width    
  push hl                 ; save screen location
  pop bc                  ; restore sprite screen location
  pop hl                  ; restore sprite data position
  ld a,[hcount]           ; sprite height counter
  dec a 
  ld [hcount],a
  jr nz,iterate_height
  ret
hcount
  db #00  
	ret
end_procedure_Sprite_DrawAt:
	
; // ( 0 = left, 1 = right, 2 = up, 3 = down )
; // ( false = left, true = right )
; // Levels data
; // Store current level live data
; // Draw and manage Title Screen
	; ***********  Defining procedure : TitleScreen
	;    Procedure type : User-defined procedure
TitleScreen:
	ld hl,mychar
	ld [myp],hl
	ld hl,message
	ld [mym],hl
	; generic assign 
	ld hl,message
	ld [Functions_s],hl
	call Functions_SizeOfString
	ld [strlen], a
	ld a, $0
	ld [v], a
	ld [gameStats_gameStats_record_gameStats_record_titlePlayed], a
	ld a, $1
	ld [Screen_u], a
	call Screen_Cls
	
; // Decompress the image to the screen
	ld hl,titledata
	ld [Compression_in],hl
	ld hl,$7000
	ld [Compression_out],hl
	call Compression_Decompress
	
; // Set up font
; // Set pen colour
	ld a, $2
	ld [Screen_c], a
	call Screen_SetPen
	ld a, $0
	ld [Screen_c], a
	call Screen_SetPaper
	ld hl,Font_font1
	ld [Font_currentFont],hl
	call Font_SetFont
TitleScreen_while35:
TitleScreen_loopstart39:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[gameStats_gameStats_record_gameStats_record_gameRunning]
	or a
	jp nz,TitleScreen_elsedoneblock38
TitleScreen_ConditionalTrueBlock36: ;Main true block ;keep :
	
; // Keep titlescreen going until S pressed
	; generic assign 
	ld a, $1
	ld [Input_c], a
	call Input_GetJoystick
	ld [varPrefixed_p], a
	; generic assign 
	ld a, $2
	ld [Input_c], a
	call Input_GetJoystick
	ld [q], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[varPrefixed_p]
	cp $d0
	jr nz,TitleScreen_localfailed85
	jr TitleScreen_ConditionalTrueBlock81
TitleScreen_localfailed85: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[q]
	cp $d0
	jr nz,TitleScreen_localfailed86
	jr TitleScreen_ConditionalTrueBlock81
TitleScreen_localfailed86: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	call Input_GetPressedKey
	cp $27
	jr nz,TitleScreen_elsedoneblock83
TitleScreen_ConditionalTrueBlock81: ;Main true block ;keep :
	ld a, $1
	ld [gameStats_gameStats_record_gameStats_record_gameRunning], a
TitleScreen_elsedoneblock83:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	or a
	jr nz,TitleScreen_elsedoneblock91
TitleScreen_ConditionalTrueBlock89: ;Main true block ;keep :
	
; // Draw new char in the onscreen scroll every 4 pixels
	ld a, $4
	ld [v], a
	ld a,[strpos]
	ld e,a ; variable is 8-bit
	ld d,0 ;keep
	ld hl,[mym]
	add hl,de
	; TYPETEST : BYTE  WriteType : NADA
	ld a,[hl]
	push af
	ld hl,[myp]
	; Writetype: NADA
	pop af
	ld [hl],a
	ld hl,mychar
	ld [Font_text],hl
	ld a, $1f
	ld [Font_tx], a
	ld a, $3b
	ld [Font_ty], a
	ld a, $0
	ld [Font_tran], a
	call Font_DrawTextAt
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[strlen]
	ld b,a
	ld a,[strpos]
	cp b
	jr nc,TitleScreen_elseblock104
TitleScreen_ConditionalTrueBlock103: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[strpos]
	add  a,$1
	ld [strpos], a
	jr TitleScreen_elsedoneblock105
TitleScreen_elseblock104:
	ld a, $0
	ld [strpos], a
TitleScreen_elsedoneblock105:
TitleScreen_elsedoneblock91:
	ld a, $0
	ld [u], a
TitleScreen_forloop110:
	; Wait
	ld a,$32
TitleScreen_wait116:
	sub 1
	jr nz,TitleScreen_wait116
TitleScreen_loopstart111:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c8
	jr nz,TitleScreen_forloop110
TitleScreen_loopend112:
	
; // Smooth scroll the message 1 pixel at a time
	; generic assign 
	ld hl,$777f
	; Loading pointer
	ld [Screen_Loc],hl
	ld a, $5
	ld [Screen_h], a
	call Screen_doSmoothScroll
	; 'a:=a + const'  optimization 
	ld a,[v]
	sub $1
	ld [v], a
	jp TitleScreen_while35
TitleScreen_elsedoneblock38:
TitleScreen_loopend40:
	ret
end_procedure_TitleScreen:
	
; // In-game HUD
	; ***********  Defining procedure : DrawHUD
	;    Procedure type : User-defined procedure
DrawHUD:
	ld a, $1
	ld [Screen_u], a
	call Screen_Cls
	ld a, $6
	ld [Sprite_spritewidth], a
	ld a, $40
	ld [Sprite_spriteheight], a
	call Sprite_SetSize
	ld hl,HUDData
	ld [Sprite_spritedata],hl
	call Sprite_SetData
	ld a, $1a
	ld [Sprite_spritex], a
	ld a, $0
	ld [Sprite_spritey], a
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ret
end_procedure_DrawHUD:
block1:
main_block_begin_:
	; ****** Inline assembler section
 di
	
; // Set Graphics mode
	ld a, $1
	ld [Screen_ga], a
	call Screen_SetMode
	
; // Display Titlescreen
	call TitleScreen
	
; // Draw Game HUD
	call DrawHUD
MainProgram_end119:
	;nop
	jr MainProgram_end119
main_block_end_:
; Copy BC bytes from HL to DE.
z80_copy_mem:
    ld      a,b
    or      c
    ret     z
    ld      a,16
    sub     c
    and     15
    add     a,a
    ld      (z80_copy_lp-1),a
    jr      z80_copy_lp ; will be overwritten
z80_copy_lp:
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    jp      pe,z80_copy_lp
    ret
mul_8x8:
    sla h  ; optimised 1st iteration
    jr nc,mul_8x8_s1
    ld l,e
mul_8x8_s1:
    add hl,hl
    jr nc,mul_8x8_s2
    add hl,de
mul_8x8_s2:
    add hl,hl
    jr nc,mul_8x8_s3
    add hl,de
mul_8x8_s3:
    add hl,hl
    jr nc,mul_8x8_s4
    add hl,de
mul_8x8_s4:
    add hl,hl
    jr nc,mul_8x8_s5
    add hl,de
mul_8x8_s5:
    add hl,hl
    jr nc,mul_8x8_s6
    add hl,de
mul_8x8_s6:
    add hl,hl
    jr nc,mul_8x8_s7
    add hl,de
mul_8x8_s7:
    add hl,hl
    jr nc,mul_8x8_s8
    add hl,de
mul_8x8_s8:
    ret
mul_16x8:
    add a,a  ; optimised 1st iteration
    jr nc,mul_16x8_s1
    ld h,d
    ld l,e
mul_16x8_s1:
    add hl,hl
    rla
    jr nc,mul_16x8_s2
    add hl,de
    adc a,c
mul_16x8_s2:
    add hl,hl
    rla
    jr nc,mul_16x8_s3
    add hl,de
    adc a,c
mul_16x8_s3:
    add hl,hl
    rla
    jr nc,mul_16x8_s4
    add hl,de
    adc a,c
mul_16x8_s4:
    add hl,hl
    rla
    jr nc,mul_16x8_s5
    add hl,de
    adc a,c
mul_16x8_s5:
    add hl,hl
    rla
    jr nc,mul_16x8_s6
    add hl,de
    adc a,c
mul_16x8_s6:
    add hl,hl
    rla
    jr nc,mul_16x8_s7
    add hl,de
    adc a,c
mul_16x8_s7:
    add hl,hl
    rla
    jr nc,mul_16x8_s8
    add hl,de
    adc a,c
mul_16x8_s8:
    ret
div_16x8
;div_hl_c:
   xor	a
   ld	b, 16
_loop:
   add	hl, hl
   rla
   jr	c, $+5
   cp	c
   jr	c, $+4
   sub	c
   inc	l
   djnz	_loop
   ret
div_16x16:
   ld	hl, 0
   ld	b, 16
_loop16:
   sll	c
   rla
   adc	hl, hl
   sbc	hl, de
   jr	nc, $+4
   add	hl, de
   dec	c
   djnz	_loop16
   ret
_Mod_16:
    ld hl,0
    ld a,b
    ld b,8
_Mod_Loop1:
    rla
    adc hl,hl
    sbc hl,de
    jr nc,_Mod_NoAdd1
    add hl,de
_Mod_NoAdd1:
    djnz _Mod_Loop1
    rla
    cpl
    ld b,a
    ld a,c
    ld c,b
    ld b,8
_Mod_Loop2:
    rla
    adc hl,hl
    sbc hl,de
    jr nc,_Mod_NoAdd2
    add hl,de
_Mod_NoAdd2:
    djnz _Mod_Loop2
    rla
    cpl
    ld b,c
    ld c,a
    ret
EndBlock7b00:
	end
