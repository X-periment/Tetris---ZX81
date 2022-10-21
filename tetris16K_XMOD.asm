;; Modified by Xperiment 10/2022, based on the code by ByteForever https://www.youtube.com/watch?v=yyCUVOmWhSU

;; Xperiment BUGS and TODO
;; if the piece hits a piece below, there is no time for it to slide to left or right if that key is pressed - look at the order the checks are made so it can slide just as it does the last drop

;; so what is happening is that when we reach bc == 0, we do not pass through the key press if there is one -

;;;;;;;;;;;;;;;;;;;;;
;;; zx81 16K code (gave up for now with getting tetris working in 1K!!)
;;; It's a clone of tetris (in case that wasn't clear from filename;)
;;; The code heavily!! dependant on the definition of the screen memory in screenTetris.asm
;;;;;;;;;;;;;;;;;;;;;

; TODO  / bugs
;   when shape next to edge no logic to prevent rotation, so sticks to wall or worse goes through

; all the includes came from  https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
#include "zx81defs.asm"
#include "zx81rom.asm"
#include "charcodes.asm"
#include "zx81sys.asm"                ;; removed some of unneeded definitions
#include "line1.asm"

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE

#define SHAPE_CHAR_0   128        ; black square
#define SHAPE_CHAR_1 136        ; grey square
#define BOTTOM 22
#define KEY_DELAY $ff
#define REDUCED_KEY_DELAY $00
#define GO_LEFT 1
#define GO_RIGHT 2
#define GO_ROTATE 3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp intro_title		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the underscore characters here are mapped onto real zx81
;; characters in charcodes.asm, they are more human readble
;; shortcuts, than decimal equivalent
game_over_txt1
	DEFB	_G,_A,_M,_E,$ff
game_over_txt2
    DEFB	_O,_V,_E,_R,$ff
currentShape
    DEFB 0
shapes      ; Shapes are known as Tetromino (see wikipedia), use 8 bits per shape
            ; base shape 2 column * 4 rows to make logic easier, interpreted as such in the code and definition
            ;  "square"   "L"    "straight"   "T"  "skew left" "skew right"
            ;       00     00       10        00     00          00
            ;       00     10       10        01     10          01
            ;       11     10       10        11     11          11
            ;       11     11       10        01     01          10

; shape definition (bit packed)
;        square       L R/L   straight     T L/R   skew L   skew R
;        11223344
   DEFB %00111100,  %00101011,%10101010,%00011101,%00101101, %00011110   ; should be drawn vertically
;        ttttbbbb
   DEFB %11001100,  %00101110,%00001111,%11100100,%01101100, %11000110   ; should be drawn horiz
   DEFB %00111100,  %11010100,%10101010,%10111000,%10110100, %00011110   ; should be drawn vertically
   DEFB %11001100,  %11101000,%00001111,%01001110,%01101100, %11000110   ; should be drawn horiz

;   DEFB             %00010111,           %00101110,

screen_area_blank_txt
	DEFB	__,__,__,__,__,__,__,$ff

waitLoopDropFasterFlag
    DEFB 0
shape_row_index     ; the current row of the top of the falling shape
    DEFB 0
shape_col_index     ; the current column of the top left of the falling shape
    DEFB 0
outerCount
    DEFB 0,0
currentShapeOffset
    DEFB 0,0
shapeTrackLeftRight
    DEFB 0,0
shape_row
    DEFB 0
initScreenIndex
    DEFB 0,0
flagForBottomHit
    DEFB 0
checkColOffsetStartRow
    DEFB 0,0
checkRowIndex
    DEFB 0
checkColIndex
    DEFB 0
lineCompleteFlag
    DEFB 0
lineRemoved
    DEFB 0,0
lineToSuffleFrom
    DEFB 0,0
copyOfCheckColOffsetStartRow
    DEFB 0,0
rotationCount           ; zero means not rotated!
    DEFB 0
innerDrawLoopInit
    DEFB 0
displayLineIncrement
    DEFB 0,0
displayOuterIncrement
    DEFB 0,0
score_mem_hund
	DEFB 0
score_mem_tens
	DEFB 0
deleteShapeFlag
    DEFB 0
;XMOD start ---------------------------------
testShapeFlag
    DEFB 0
collisionDetectedFlag
    DEFB 0
oldCurrentShapeOffset
    DEFB 0
oldShapeRow
    DEFB 0
oldRotationCount
    DEFB 0
oldShapeRowIndex
    DEFB 0
oldShapeTrackLeftRight
    DEFB 0
rotateKeyPressed
    DEFB 0
rotateDelay
    DEFB 0
userInputDelay
    DEFB 0
leftKeyPressed
    DEFB 0
rightKeyPressed
    DEFB 0
timeLeftC
    DEFB 0
timeLeftB
    DEFB 1
timeToDrop
    DEFB 0
pendingMove
    DEFB 0
blockedDirection
    DEFB 0
;XMOD end ----------------------------------
speedUp
    DEFB 0
;; intro screen
intro_title
    ; screenTetris16K.asm has already set everything including the title
    ; clear the play area (is need for all after first game as play area will be filled with previous blocks
    ld b, BOTTOM
    ld a, 11
    ld (initScreenIndex),a
initPlayAreaLoop
    push bc
    ld bc, (initScreenIndex)
    ld de,screen_area_blank_txt
    call printstring
    ld a,(initScreenIndex)
    add a, 10
    ld (initScreenIndex),a
    pop bc
    djnz initPlayAreaLoop

    ld a, $ff  ;$df   XMOD
    ld (speedUp),a


;; main game loop
main
    ld a, 1
    ld (shape_row),a
    ld a, 5
    ld (shapeTrackLeftRight),a
    ld a, 13
    ld (shape_row_index),a
    xor a
    ld (flagForBottomHit), a
    ld (rotationCount), a


tryAnotherR                             ; generate random number to index shape memory
    ld a, r                             ; we want a number 0 to 4 inclusive
    and %00000111
    cp 6
    jp nc, tryAnotherR                  ; loop when nc flag set ie not less than 5 try again
    ld (currentShapeOffset), a

    xor a
    ld (waitLoopDropFasterFlag),a

; read keyboard input, delete old shape move current shape down one
dropLoop
    ld a, 1
    ld (deleteShapeFlag),a          ;  drawShape checks this flag to see if is deleting
    call drawShape

; XMOD begin ---------------------------------------
    ld a, (shape_row_index)
    ld (oldShapeRowIndex), a
    ld a, (shapeTrackLeftRight)
    ld (oldShapeTrackLeftRight), a
    ld a, (currentShapeOffset)
    ld (oldCurrentShapeOffset), a
    ld a, (rotationCount)
    ld (oldRotationCount), a
    ld a, (shape_row)
    ld (oldShapeRow), a
; XMOD end -----------------------------------------

    xor a                           ;  xor a is always zero and saves 1 byte compared to ld a, 0
    ld (deleteShapeFlag),a          ;  clear the flag

    ; read the keyboard input and adust the offset
; XMOD start ----------------------------------------------------------------
    ld a, (pendingMove)
    ld (blockedDirection), a
    cp GO_LEFT
    jr nz, skip1
    ld a, 0
    ld (pendingMove), a
    jp shapeLeft
skip1
    cp GO_RIGHT
    jr nz, skip2
    ld a, 0
    ld (pendingMove), a
    jp shapeRight
skip2

; XMOD end ----------------------------------------------------------------



    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard space to B
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 3, a									; check bit set for key press right (C)
    jr z, dropShapeAllTheWay                    ; causes wait loop to be zero when C pressed
    jp noShapeMove								; dropped through to no move

dropShapeAllTheWay
; XMOD start ---------------------------------------
; check if we are lower than first row
    ld a, (shape_row)
    cp 1
    jp z, noShapeMove
; XMOD end ----------------------------------------
    ld a, 1
    ld (waitLoopDropFasterFlag), a
    jp noShapeMove								; dropped through to no move

shapeRight


    ;; need to account for rotation when checking if shape can go further to right
    ; and also special case for vertical drawn straight shape to go fully to right

    ld a, (rotationCount)
    and %00000001       ;; work out if rotation count is odd or even
    jr nz, handleShapeRightForHorizontal             ;; if odd then treat as a horizontal shape

    ;; ok so check if shape is straight, if so gets less width.
    ;straight shape offsets are 2, 8, 14, 20
    ld a, (currentShapeOffset)
    cp 2
    jp z, handleShapeRight_StrVert
    ld a, (currentShapeOffset)
    cp 8
    jp z, handleShapeRight_StrVert
    ld a, (currentShapeOffset)
    cp 14
    jp z, handleShapeRight_StrVert
    ld a, (currentShapeOffset)
    cp 20
    jp z, handleShapeRight_StrVert

    ld a, (shapeTrackLeftRight)
    dec a
    cp 1
    jp z, noShapeMove
    ld (shapeTrackLeftRight),a
    ld a, (shape_row_index)
    inc a
    ld (shape_row_index), a
    jp noShapeMove

handleShapeRight_StrVert
    ld a, (shapeTrackLeftRight)
    dec a
    cp 0
    jp z, noShapeMove
    ld (shapeTrackLeftRight),a
    ld a, (shape_row_index)
    inc a
    ld (shape_row_index), a
    jp noShapeMove

handleShapeRightForHorizontal

    ;; ok so check if shape is straight, if so gets less width.
    ;straight shape offsets are 2, 8, 14, 20
    ld a, (currentShapeOffset)
    cp 2
    jp z, handleShapeRight_StrHoriz
    ld a, (currentShapeOffset)
    cp 8
    jp z, handleShapeRight_StrHoriz
    ld a, (currentShapeOffset)
    cp 14
    jp z, handleShapeRight_StrHoriz
    ld a, (currentShapeOffset)
    cp 20
    jp z, handleShapeRight_StrHoriz

    ld a, (shapeTrackLeftRight)
    dec a
    cp 2
    jp z, noShapeMove
    ld (shapeTrackLeftRight),a
    ld a, (shape_row_index)
    inc a
    ld (shape_row_index), a
    jp noShapeMove

handleShapeRight_StrHoriz
    ld a, (shapeTrackLeftRight)
    dec a
    cp 3
    jp z, noShapeMove
    ld (shapeTrackLeftRight),a
    ld a, (shape_row_index)
    inc a
    ld (shape_row_index), a
    jp noShapeMove
shapeLeft
    ld a, (shapeTrackLeftRight)
    inc a
    cp 8
    jp z, noShapeMove
    ld (shapeTrackLeftRight),a

    ld a, (shape_row_index)
    dec a
    ld (shape_row_index), a

noShapeMove

    ;;; read the rotate shape after the left right is done,
    ;; we draw the shape then next time the delete shape code runs will delete rotated

; XMOD start ----------------------------------------------------

    ld a, (rotateKeyPressed)
    cp 1
    ; check bit set for key press rotate  use X key
    jp nz, pass3  ; XMOD  drawShapeHook ---------------------------------------
    ld a, 0
    ld (rotateKeyPressed), a
; XMOD end ------------------------------------------------------
    ld a, (rotationCount)
    inc a
    cp 4
    jp nz, storeIncrementedRotation
    ld a, 0
    ld (rotationCount), a
    ; need to subract 18 from shape offset to get back to original rotation
    ld a, (currentShapeOffset)
    sub 18
    ld (currentShapeOffset),a
    ;; need to take 2 off shape row when it's rotated, as no longer printing vertically
    ld a, (shape_row)
    sub 2
    ld (shape_row),a

    jp pass3 ; XMOD ---------------------------------------------------------------
    call drawShape

    jp  preWaitloop

storeIncrementedRotation
    ld (rotationCount), a
    ld a, (currentShapeOffset)
    ld (oldCurrentShapeOffset), a  ; XMOD +++++++++++++++++++++++++++
    add a, 6
    ld (currentShapeOffset),a

;XMOD start -----------------------------------------------------

pass3
    ld a, 1
    ld (testShapeFlag), a
    call drawShape              ; this call just tests for a collision
    ; restore the shape row index if it was 'timeToDrop'
    ld a, (timeToDrop)
    cp 1
    jr nz, afterRestore
    ld a, (shape_row_index)
    sub 10
    ld (shape_row_index), a
afterRestore

    ld a, (collisionDetectedFlag)
    cp 1
    jp z, collisionDetected

    ld a, 0
    ld (blockedDirection), a
    jp pass1
collisionDetected
    ; reset the flag
    ld a, 0
    ld (collisionDetectedFlag), a

; now undo the rotation (or move)
    ld a, (oldRotationCount)
    ld (rotationCount), a
    ld a, (oldCurrentShapeOffset)
    ld (currentShapeOffset), a

    ld a, (oldShapeRow)
    ld (shape_row), a

    ld a, (oldShapeRowIndex)
    ld (shape_row_index), a
    ld a, (oldShapeTrackLeftRight)
    ld (shapeTrackLeftRight), a

pass1
    ld a, 0
    ld (testShapeFlag), a
;XMOD end -------------------------------------------------

    call drawShape
    jp  preWaitloop

drawShapeHook
    call drawShape
preWaitloop
    ld a, (score_mem_tens)
    cp 153
    jr z, addOneToHund
    jr skipAddHund
addOneToHund
    ld a, 0
    ld (score_mem_tens), a
    ld a, (score_mem_hund)
    add a, 1
    daa
    ld (score_mem_hund), a
skipAddHund

printScoreInGame
    ld bc, 1
    ld de, score_mem_tens
    call printNumber

    ld bc, 4            ; print the current difficulty level (lower number is harder)
    ld de, speedUp
    call printNumber

; XMOD start -------------------

loopBack
    ld a, (timeLeftC)
    ld c, a
    ld a, (timeLeftB)
    ld b, a
; XMOD end ---------------------
    ld a, (waitLoopDropFasterFlag)
    cp 0
    jp z,dropNormalSpeed

    ld bc, $0002      ; set to zero no wait, drop fast
    ld hl, $0002

dropNormalSpeed
   ; adc hl, bc
    ;push hl
   ; pop bc

waitloop
    ld a, (userInputDelay)
    cp 0
    jp z, checkKeyboard
    dec a
    ld (userInputDelay), a
    ld a, $10
innerLoop
    dec a
    jr nz, innerLoop
    jp waitpass4


checkKeyboard
; XMOD start ---------check for C key press--------------------------
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			; read keyboard
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 2, a									; check bit set for M key press
    jr nz, waitpass
    ld a, 1
    ld (waitLoopDropFasterFlag), a
    ld a, KEY_DELAY
    ld (userInputDelay), a
    jr waitOver
waitpass
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			; read keyboard
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 3, a								; check for N press
    ; check bit set for key press rotate  use X key
    jp nz, waitpass2
    ld a, 1
    ld (rotateKeyPressed), a
    ld a, GO_ROTATE
    ld (pendingMove), a
    ld a, KEY_DELAY
    ld (userInputDelay), a
    jr waitOver
waitpass2
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard shift to v
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 1, a                               ; check for Z key
    jp nz, waitpass3
    ld a, 1
    ld (leftKeyPressed), a
    ld a, GO_LEFT
    ld (pendingMove), a
    ld a, KEY_DELAY
    ld (userInputDelay), a
    jr waitOver
waitpass3
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard space to B
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 2, a							; X press
    jp nz, waitpass4
    ld a, 1
    ld (rightKeyPressed), a
    ld a, GO_RIGHT
    ld (pendingMove), a
    ld a, KEY_DELAY
    ld (userInputDelay), a
    jr waitOver
waitpass4


; XMOD end ------------------------------------------
    dec bc
    ld a,b
    or c
    jr nz, waitloop

; XMOD start ----
; set drop flag and reset bc
    ld a, 1
    ld (timeToDrop), a
    ld a, (speedUp) ; XMOD
    ld c, a ; XMOD
    ld b, 0
    ld hl, $0200  ; XMOD
    add hl, bc
    push hl
    pop bc
; if there is a blocked direction, then execute that now
    ld a, (blockedDirection)
    cp 0
    ;jp nz, gameOver
    ld (pendingMove), a

waitOver
    ld a, c ;   ;-- save bc here?
    ld (timeLeftC), a
    ld a, b
    ld (timeLeftB), a

; check if the direction is a) not zero and b) the same as the blocked direction
    ;ld a, (blockedDirection)
    ;cp 0
    ;jp nz, gameOver
    ;ld b, a
    ;ld a, (pendingMove)
    ;cp 0
    ;jr z, noMovePass
    ;sub b
    ;jp z, gameOver
    ;jp z, loopBack
noMovePass
; XMOD end -----------------------

    ld a,(flagForBottomHit)         ; on current shape draw we detected that if the shape dropped one
                                    ; more line it would hit the something
    cp 1                            ; if flagForBottomHit is set then this will set zero flag
                                    ; so we need to check if rows are complete first

    jp z, checkForCompleteLinesInit

; XMOD start -------------- don't increment row if we just rotated or moved L/R---------
    ;really we only want to move when the counter reaches zero, so save the counter?

; only increment if flag says to
    ld a, (timeToDrop)
    cp 1
    jr nz, dontDrop
    ld a, (shape_row)
    inc a
    ld (shape_row),a
dontDrop
; XMOD end ------------------------------------------------------------------------------
    cp BOTTOM                            ; only gets here if no shapes at bottom
    jp nz, dropLoop
    jp main

checkForCompleteLinesInit
    ld a, 11                        ; offset to first block in screen play area
    ld (checkColOffsetStartRow), a
    ld a, 1
    ld (checkRowIndex), a
checkLoopSetup
    ld a, 1
    ld (lineCompleteFlag),a
    ld hl,(DF_CC)
    ld a, (checkColOffsetStartRow)
    add a, 10
    ld (checkColOffsetStartRow), a
    ld bc, (checkColOffsetStartRow)
    add hl,bc
    ld a, 0
    ld (checkColIndex), a
checkLine
    ld a, (hl)
    and SHAPE_CHAR_0
    inc hl
    cp SHAPE_CHAR_0
    jp nz, setlineNOTComplete
afterSetlineNOTComplete

    ld a, (checkColIndex)
    inc a
    ld (checkColIndex), a
    cp 7
    jp nz, checkLine                ; always complete check loop fully

    ld a, (lineCompleteFlag)
    cp 1
    jp z,removelineIsComplete

    jp checkCompleteLoopInc

setlineNOTComplete
    ld a, 0
    ld (lineCompleteFlag),a
    jp afterSetlineNOTComplete

removelineIsComplete
    push hl ; preserve for after printstring
    push de
    push bc

    ld a, (speedUp)     ;; increase difficulty with each line removed
    dec a
    dec a   ; XMOD speed up the difficulty increase
    ld (speedUp),a

    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
    add a,1
    daa									; z80 daa instruction realigns for BCD after add or subtract
    ld (score_mem_tens),a				; add one to score, scoring is binary coded decimal (BCD)
    ; move all lives about this down by one
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ld a, (checkColOffsetStartRow)
    ld (copyOfCheckColOffsetStartRow), a

playAreaShuffle
    ld hl, (DF_CC)
    ld bc, (copyOfCheckColOffsetStartRow)    	; checkColOffsetStartRow is an offset from DF_CC,
                                                ; not address of screen memory
    add hl,bc
    ld (lineRemoved), hl                        ; lineRemoved is now the 16bit copy of address the
                                                ; start of play area for the line romoved
    ld a, (copyOfCheckColOffsetStartRow)        ; subtract 10 from checkColOffsetStartRow
    sub 10                                      ; this gets us the offset to the previous line...
    ld (copyOfCheckColOffsetStartRow),a         ; subtract 10 from checkColOffsetStartRow
    ld hl, $00                                  ; have to zero this here otherwise left overs in lineToSuffleFrom top 8bits
    ld (lineToSuffleFrom), hl
    ld (lineToSuffleFrom) , a                   ; ...the line we're shuffling down from
    ld bc,(lineToSuffleFrom)
    ld hl, (DF_CC)
    add hl,bc
    ld (lineToSuffleFrom), hl                   ; lineToSuffleFrom is a 16 bit value now the offset
                                                ; from start of  screen memory
    ld hl, (lineRemoved)
    ld bc, (lineToSuffleFrom)
    ld e, 0
loopFor_7_Shuffle
    ld a,(bc)
    ld (hl), a   ;; this instruction crashes!!       ; load screen position at hl with a
    inc hl                              ; move position in screen memory we're writing to on one
    inc bc                              ; move the position we're moving from on one
                                    ; loop count down to zero
    inc e
    ld a, e
    cp 7
    jp nz, loopFor_7_Shuffle

    ; need to loop until reached top with copy of checkColOffsetStartRow
    ld a,(copyOfCheckColOffsetStartRow)
    cp 21
    jp nz, playAreaShuffle

    pop bc
    pop de
    pop hl

checkCompleteLoopInc
    ld a, (checkRowIndex)
    inc a
    ld (checkRowIndex), a
    cp BOTTOM
    jp nz, checkLoopSetup

checkIfTopWillBeHit                     ; call if bottom was hit and if this means no space at top
                                        ; check the if the top is reached then game over
    ld a, (shape_row)
    cp 2                                ; depends on shape so need multiple compares
    jp z, gameOver
    cp 1                                ; depends on shape so need multiple compares
    jp z, gameOver
    jp main


gameOver
    ld bc,22
    ld de,game_over_txt1
    call printstring
    ld bc,32
    ld de,game_over_txt2
    call printstring
    ld bc, $ffff
    ld a, 0
    ld (score_mem_tens), a
    ld (score_mem_hund), a
waitloopRetryGame
    dec bc
    ld a,b
    or c
    jr nz, waitloopRetryGame
    jp intro_title

; this prints at top any offset (stored in bc) from the top of the screen D_FILE
printstring
    ld hl,(DF_CC)
    add hl,bc
printstring_loop
    ld a,(de)
    cp $ff
    jp z,printstring_end
    ld (hl),a
    inc hl
    inc de
    jr printstring_loop
printstring_end
    ret

printNumber
    ld hl,(DF_CC)
    add hl,bc
printNumber_loop
    ld a,(de)
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a
    ret

drawShape
    ld a,(deleteShapeFlag)
    cp 1
    jp z, dontIncrementShapeRowIndex    ;; if we're deleting shape then skip increment shape_row_index
; XMOD start ----------------------------------------

    ld a, (timeToDrop)

    cp 1
    jr nz, dontIncrementShapeRowIndex
    ld a, (testShapeFlag)
    cp 1
    jr z,  dontClearFlag
    ld a, 0
    ld (timeToDrop), a
dontClearFlag
; XMOD end --------------------------------
    ld a, (shape_row_index)
    add a, 10                  ; always need ten as the offset, the left right just adds bit to this
    ld (shape_row_index), a

dontIncrementShapeRowIndex

    ld a, (currentShapeOffset)
    ld hl, shapes
    ld d, 0
    ld e, a                            ; add the random (0 to 3) offset to hl to get value of shape
    add hl, de
    ld a, (hl)
    ld (currentShape), a
    ; draw shape at next row
    ld hl, (DF_CC)
    ld de, (shape_row_index)            ; add offset to top of screen memory to skip title

    ;; this will only draw shape at top need to add current position offset
    add hl, de                          ; to where we want to draw shape
    ld c, %10000000                     ; mask for shape (initialised, but will be rotated  )

    ;; alter loop counts when rotating so draw horizontal or vertical, not just vertical                                    l
    ld a, (rotationCount)
    and %00000001       ;; work out if rotation count is odd or even
    jr nz, drawHorizLoopCountSetup
    ld e, 4
    ld a, 2
    ld (innerDrawLoopInit), a
    ld a,10
    ld (displayLineIncrement), a
    sub 2
    ld (displayOuterIncrement),a
    jr drawShapeOuter
drawHorizLoopCountSetup
    ld e, 2
    ld a, 4         ; drawing horizontally
    ld (innerDrawLoopInit), a
    ld a,8
    ld (displayLineIncrement), a
    sub 2
    ld (displayOuterIncrement),a
drawShapeOuter
    ld a, (innerDrawLoopInit)
    ld b, a             ; directly loading into b from memory fails?? MS byte not used error??
drawShapeInner
    ld a, (currentShape)
    and c                               ; set to block or no block based on (shapes)
    jp z, drawNothing

; XMOD start -------------------------------------------------------------
    ld a, (testShapeFlag)
    cp 1
    jr z,  testTheSquare
; XMOD end --------------------------------------------------

    ; detect if hl is already drawn on (ie a block already in that location, if so stop
    ;; we also need to draw the shape from the bottom upwards, because we want to detect the collision earlier
    ;; and actually we should to a "trial draw of shape then if no collisions actually draw it!!

    push hl
    ;ld de, (displayLineIncrement)
    ld de, 10
    add hl, de
    ld a, (hl)
    and SHAPE_CHAR_0                      ; this will result in "true" if block exists already in that position
    ;cp 0                                ; don't need cp 0, as the and sets the flags (saved 2bytes wooo!)
    pop hl

    jp z, drawTheDamnSquare             ; set a flag to say if move shape one more down will be collision

    ld a, (deleteShapeFlag)     ;; if we're deleting the old shape then don't trigger collision
    cp 1
    jp z, drawTheDamnSquare

    ld a, 1
    ld (flagForBottomHit), a

drawTheDamnSquare
    ld a,(deleteShapeFlag)     ;; if we're deleting the old shape then don't draw anything
    cp 1
    jp z, loadBlank



    ld a, (currentShapeOffset)
    and %00000011
    ;cp 0                       ; don't need cp 0, as the and sets the flags (saved 2bytes wooo!)
    jp z, loadAlternateShape1
    ld (hl), SHAPE_CHAR_0
    jr drawNothing

loadAlternateShape1
    ld (hl), SHAPE_CHAR_1
    jr drawNothing

;XMOD start ---------------------------------------------------------
testTheSquare
    ld a, (hl)
    cp 0
    jr z, drawNothing
    ld a, 1
    ld (collisionDetectedFlag), a
    ret
;XMOD end ---------------------------------------------------------

loadBlank
    ld (hl), 0      ; this clears the block with space
drawNothing
    inc hl
    xor a
    ld a, c
    rra                                 ; rotate mask to right by one bit
    ld c, a
    djnz drawShapeInner                 ; dnjz decrements b and jumps if not zero
    ld (outerCount), de                 ; store loop count temp
    ld de, (displayOuterIncrement)
    add hl, de                          ; gets current screen position to next row
    ld de, (outerCount)                 ; retreive  loop count temp
    dec e
    ld a, e
    cp 0
    jp nz, drawShapeOuter

    ret


#include "line2.asm"
#include "screenTetris16K.asm"      			; definition of the screen memory, in colapsed version for 1K
#include "endbasic.asm"
