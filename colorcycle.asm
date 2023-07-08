;program starts at address $0801
* = $0801

CharacterMemory = $0400
ColorMemory = $d800
ROW = 5
FirstCharacterMemory = CharacterMemory+ROW*40
FirstColorMemory = ColorMemory+ROW*40
TextOffset = 13
NextTextOffset = 10
ROW2 = 15
NextCharacterMemory = $0400+ROW2*40
NextColorMemory = ColorMemory+ROW2*40
TIMER_RUNS = 1
KeyPressed = 00
ScrollLeftFlag = 1

;used to display the next string in the sequence on the second row.
!macro textDisplayMacro stringIndex, nextRoutine, textStringIn
  ldx #00
.macroDisplayNextString
  lda Row2Index       ;this value is incremented on the first and every subsequent change of text
  cmp #stringIndex    ;if index is higher than the current call label, move
  bne nextRoutine     ;to the next call label/routine
  lda textStringIn, x ;next 5 lines: display the string
  beq .returnFromMacroDisplayNextString
  sta NextCharacterMemory+NextTextOffset-1, x
  inx
  jmp .macroDisplayNextString
.returnFromMacroDisplayNextString
  rts
!end 

;this is the final statement, so no comparisons
!macro textDisplayMacroFinal textStringIn
  ldx #00
.macroDisplayFinalString
  lda textStringIn, x
  beq .returnFromMacroDisplayFinal
  sta NextCharacterMemory+NextTextOffset-1, x
  inx
  jmp .macroDisplayFinalString
.returnFromMacroDisplayFinal
  rts
!end

;create a basic line for running the SYS value
!basic

  ;set lowercase mode
  lda #23
  sta $d018
  ;change screen to black
  lda #00
  sta $d020
  sta $d021
  jsr .clearScreen

  ;Display the text
  jsr .initializeText
  ;shift the colors
  jsr .colorShift
;end demo
  lda #$08   ;b00001000 (3rd-order bit is columns)
  sta $d016  ;reset scrolling to normal, columns to 40
  lda #00    ;reset indices in case program is re-run
  sta Row2Index
  sta CycleCount
  rts

.initializeText
  ldx #00
.displayText
  lda .textString, x
  beq .nextDisplay
  sta FirstCharacterMemory+TextOffset, x
  lda .colorCharacter, x
  sta FirstColorMemory+TextOffset, x
  inx
  jmp .displayText
.nextDisplay
  ldx #00
.displayNextText
  lda .nextTextString, x
  beq .returnFromInit
  sta NextCharacterMemory+NextTextOffset, x
  lda .colorCharacterNext, x
  sta NextColorMemory+NextTextOffset, x
  inx
  jmp .displayNextText
.returnFromInit
  rts

.textString
  !scr "REAL ONE PROPS", $00
.nextTextString
  !scr "Nothing to say really", $00
.nextTextString2
  !scr "I'm just fiddling around", $00
.nextTextString3
  !scr "  Havin' a good time   ", $00
.nextTextString4
  !scr "Traveling at the speed ", $00
.nextTextString5
  !scr "of Light, 2000 degrees ", $00
.nextTextString6
  !scr "That's why they call me", $00
.nextTextStringFinal
  !scr "    Mr. Fahrenheit     ", $00

.colorCharacter
  ;!byte 6, 14, 3, 1, 1, 3, 14, 6, 6, 14, 3, 1, 1, 3, 14, 0  
  !byte 6, 5, 3, 3, 13, 13, 1, 1, 13, 13, 3, 3, 5, 6, 0  
.colorCharacterNext
  ;!byte 6, 14, 3, 1, 1, 3, 14, 6, 6, 14, 3, 1, 1, 3, 14, 6, 6, 14, 3, 1, 1, 3, 14, 0  
  !byte 16, 11, 11, 11, 12, 12, 12, 15, 15, 15, 1, 1, 1, 1, 1, 15, 15, 15, 12, 12, 12, 11, 11, 11, 0  
.timerCounter  
  !byte TIMER_RUNS
LeftScrollFlag
  !byte ScrollLeftFlag
CycleCount
  !byte 0
Row2Index
  !byte 0
DelaySlowness
  !byte $ff
  
.colorShift
  lda .timerCounter
  ;cmp #$00
  beq .colorShiftRight
  lda .colorCharacter
  pha                    ;PUSH first color onto the stack
  ldx #01
.colorShiftLoop
  lda .colorCharacter, x ;take the next color and save it in accum
  beq .finishColorShift
  sta .colorCharacter-1, x ;store the next color in the previous spot
  inx
  jmp .colorShiftLoop
.finishColorShift
  dex                    ;move back to the spot before the 0 (end)
  pla                    ;POP first color from the stack
  sta .colorCharacter, x ;put the first color in the last spot

.colorShiftRight
  ldx #00
.initShiftLoopRight      ;first start at the rightmost byte
  lda .colorCharacterNext, x
  beq .startShiftLoopRight
  inx
  jmp .initShiftLoopRight
.startShiftLoopRight
  dex
  lda .colorCharacterNext, x
  pha
.colorShiftLoopRight
  lda .colorCharacterNext-1, x
  sta .colorCharacterNext, x
  dex
  beq .finishColorShiftRight
  jmp .colorShiftLoopRight
.finishColorShiftRight
  pla
  sta .colorCharacterNext, x
  cmp #16
  bne .changeColors ;carry on with normal business
  ;bump cycle count
  inc CycleCount    ;bump the color cycle count
  lda CycleCount
  cmp #10
  bne .changeColors ;carry on with normal business
  lda #$00
  sta CycleCount    ;reset color cycle count
  jsr .displayText2 ;display new text
  
;  jsr .delayRoutine  
;  jsr .delayRoutine
  
.changeColors
  ldx #00
.displayColors
  lda .colorCharacter, x
  beq .initDisplayColorsRow2
  sta FirstColorMemory+TextOffset, x
  inx
  jsr .checkForSpace
  lda KeyPressed
  cmp #$ef
  bne .displayColors
.initDisplayColorsRow2
  lda #20
  sta DelaySlowness
  jsr .delayWithTimerCounter
;call scroll-shift
  jsr .scroll
;next row  
  ldx #00
.displayColorsRow2
  lda .colorCharacterNext, x
  beq .colorShift
  sta NextColorMemory+NextTextOffset-1, x
  inx
  jsr .checkForSpace  
  lda KeyPressed
  cmp #$ef
  bne .displayColorsRow2
  rts

.scroll
  lda LeftScrollFlag
  beq .scrollRight
  lda $d016  
  sbc #$01
  and #$07
  sta $d016
  beq .setScrollRight
  ;debug
  ;adc #$30
  ;sta $0700
  ;end debug
  rts
.setScrollRight
  ;debug
  ;adc #$30
  ;sta $0700
  ;end debug
  lda #$00
  sta LeftScrollFlag
  rts
.scrollRight
  lda $d016
  ;debug
  ;adc #$30
  ;sta $0700
  ;sbc #$30
  ;end debug
  clc
  adc #$01
  and #$07
  sta $d016
  cmp #$07
  beq .setScrollLeft
  rts
.setScrollLeft
  lda #$01
  sta LeftScrollFlag
  rts
  
  
.checkForSpace
  lda $dc01
  sta KeyPressed
  rts

;delay and decrement timer count
.delayWithTimerCounter
  jsr .delayRoutine
  dec .timerCounter
  bne .dWTCReturn
  lda TIMER_RUNS
  sta .timerCounter
.dWTCReturn
  rts
  
;start delay routine
.delayRoutine  
  tya     ;save the X and Y registers in case used for indices
  pha
  txa
  pha
  ldy DelaySlowness   ;configurable delay
.cycleDelayRoutine
  ldx #255
.delayRoutineNested
  dex
  bne .delayRoutineNested
  dey
  bne .cycleDelayRoutine
  pla     ;restore the X and Y registers to their values before the call to .delayRoutine
  tax
  pla
  tay
  rts

.displayText2
  inc Row2Index
  ldx #00
.clearLine
  lda #$20
  sta CharacterMemory+ROW2*40, x
  inx
  cpx #40
  bne .clearLine
;now display it
  jsr .fadeAndDelayOut
  +textDisplayMacro $01, .displayText3, .nextTextString2

.displayText3
  ;jsr .fadeAndDelayOut
  +textDisplayMacro $02, .displayText4, .nextTextString3

.displayText4
  ;jsr .fadeAndDelayOut
  +textDisplayMacro $03, .displayText5, .nextTextString4

.displayText5
  ;jsr .fadeAndDelayOut
  +textDisplayMacro $04, .displayText6, .nextTextString5
  
.displayText6
  ;jsr .fadeAndDelayOut
  +textDisplayMacro $05, .displayTextFinal, .nextTextString6

.displayTextFinal
  ;jsr .fadeAndDelayOut
  +textDisplayMacroFinal .nextTextStringFinal
  
.clearScreen
    ldx #00
    lda #$20
.clearScreenLoop
    sta CharacterMemory,X ; Store the space character in the screen memory
    sta CharacterMemory+$fa, X
    sta CharacterMemory+2*$fa, X
    sta CharacterMemory+3*$fa, X
    inx         ; Increment the X register
    cpx #$fa    ; Check if X reached 250
    bcc .clearScreenLoop    ; If not, continue looping
    rts


;fade code could be good for something, but doesn't currently work quite right.  
BRIGHTNESS
  !byte 1, 15, 12, 11, 0, 0    ;ordered by dimmer greys
CURRENT_DIMMER_INDEX
  !byte 0
TEMP_TIMER_RUNS
  !byte 0
COLOR_MAP ;24 character colors mapped in normal memory
  !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

.fadeAndDelayOut
  jsr .copyColorMap
  ldx #00
.fADOFadeLoop
  stx CURRENT_DIMMER_INDEX
  jsr .fadeColorMap
  jsr .fADODelay
  jsr .fADODisplayColorMap
  ldx CURRENT_DIMMER_INDEX
  inx
  cpx #4  ;there are only 4 brightness increments
  bne .fADOFadeLoop
  rts

.fADODisplayColorMap
  ldx #00
.fADONextColor
  lda COLOR_MAP, x
  sta NextColorMemory+NextTextOffset-1, x
  inx
  cpx #24
  bne .fADONextColor
  rts
    
.fADODelay
  lda .timerCounter     ;store the current timerCounter value from .delayRoutine so
  sta TEMP_TIMER_RUNS   ;we can call it without affecting its global timer count
  lda #$40
  sta DelaySlowness
  jsr .delayRoutine
  lda TEMP_TIMER_RUNS
  sta .timerCounter
  rts
  
.copyColorMap
  ldx #00
.cCMNextColor
  lda NextColorMemory+NextTextOffset-1, x
  sta COLOR_MAP, x
  inx
  cpx #24             ;no more then 24 characters in 2nd row
  bne .cCMNextColor
  rts

.fadeColorMap
  ldx #00
.fCMNextColor
  lda COLOR_MAP, x
  beq .fCMNextX        ;leave this one alone. it's already 0
  ldy #00
.fCMBrightnessCheck
  lda COLOR_MAP, x
  and #$0f
  sta COLOR_MAP, x
  lda BRIGHTNESS, y
  cmp COLOR_MAP, X
  bne .fCMONextY       ;increment Y and go to the next color brightness byte
  lda BRIGHTNESS+1, y  ;get the next dimmer color in BRIGHTNESS
  sta COLOR_MAP, X
  jmp .fCMNextX
.fCMONextY
  iny
  jmp .fCMBrightnessCheck
.fCMNextX
  inx
  cpx #24             ;no more then 24 characters in 2nd row
  bne .fCMNextColor
  rts
;end fade code