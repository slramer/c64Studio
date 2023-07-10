;program starts at address $0801
* = $0801

;create a basic line for running the SYS value
!basic .start

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
TempSave = $FB
BitField = $FC
IRQVectorLow = $0314
IRQVectorHigh = $0315

;used to display the next string in the sequence on the second row.
!macro textDisplayMacro stringIndexM, nextRoutineM, textStringInM
  ldx #00
.macroDisplayNextString
  lda Row2Index       ;this value is incremented on the first and every subsequent change of text
  cmp #stringIndexM    ;if index is higher than the current call label, move
  bne nextRoutineM     ;to the next call label/routine
  lda textStringInM, x ;next 5 lines: display the string
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

!macro displayBits AddressOfByte, ScreenStart, Label
    txa     ;push X and Y registers
    pha
    tya
    pha
;display label
    ldy #0    
.labelNextCharacter
    lda Label, y
    cmp #" " ;(space character)
    beq .finalLabelCharacter
    sta ScreenStart, y
    iny
    jmp .labelNextCharacter
.finalLabelCharacter
    sta ScreenStart, y
    iny
    ldx #0
    lda #%10000000
    sta BitField
.nextBit
    lda AddressOfByte
    and BitField
    beq .displayZero  ;show a zero if bit is 0, otherwise show a one
;displayOne
    lda #$31
    sta ScreenStart, y
    jmp .nextX
.displayZero
    lda #$30
    sta ScreenStart, y
.nextX
    clc
    lsr BitField
    iny
    inx
    cpx #8
    bne .nextBit
    pla     ;pop Y and X registers
    tay
    pla
    tax
    ;rts ;don't return. Continue processing.
!end  
    
.start
;.setTimerInterrupt
  ;sei           ;stop all interrupts while setting up
  ;lda #$00      ;set low byte and high byte of timer B - start value is 8192 ($2000)
  ;sta $dc04
  ;lda #$21      ;high-order byte
  ;sta $dc05
  ;lda #<.scroll       ;store the scroll routine in the interrupt-processing address ($0314 and $0315)
  ;sta IRQVectorLow    ;store low-order byte of routine in $0314
  ;lda #>.scroll
  ;sta IRQVectorHigh   ;store high-order byte of routine in $0315
  ;;lda $dc0e
  ;;ora #%00010001      ;bit-4 tells the system timer A to cycle continuously. bit-0 tells the sytem timer A to start.
  ;lda #$11
  ;sta $dc0e           ;control register A. Controls Timer A (and other things but we did not set those bits
  ;cli
  
;normalProcessing
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
  beq .row2Display
  sta FirstCharacterMemory+TextOffset, x
  lda .colorCharacter, x
  sta FirstColorMemory+TextOffset, x
  inx
  jmp .displayText
.row2Display
  ldx #00
.row2NextCharacter
  lda .nextTextString, x
  beq .returnFromInit
  sta NextCharacterMemory+NextTextOffset, x
  lda .colorCharacterNext, x
  sta NextColorMemory+NextTextOffset, x
  inx
  jmp .row2NextCharacter
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
  jsr .DEBUGShowTimer
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
  inc Row2Index     ;bump the index for the next text line
  jsr .displayText2 ;display new text
  jsr .fadeAndDelayIn ;fade In?
  
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
  ;dummy trying the SYSTEM TIMER IRQ uncomment the 4 opcode lines if giving up
  lda #20
  sta DelaySlowness
  jsr .delayWithTimerCounter
;;call scroll-shift
  jsr .scroll
;next row  
  ldx #00
.displayColorsRow2
  lda .colorCharacterNext, x  
  beq .jumpBackToColorShift
  sta NextColorMemory+NextTextOffset-1, x
  inx
  jsr .checkForSpace  
  lda KeyPressed
  cmp #$ef
  bne .displayColorsRow2
  rts

.jumpBackToColorShift
  jmp .colorShift     ;the branch was too far away (129 bytes)
  
TmrL
    !scr "TmrL "
TmrH
    !scr "TmrH "
CR
    !scr "CR "
    
.DEBUGShowTimer
    ;DEBUG - show Timer A
    stx TempSave
    ldx #0
  .DSTloop
    lda #1
    sta $d800,x
    inx
    cpx #80
    bne .DSTloop
    +displayBits $dc05, $0400, TmrH
    +displayBits $DC04, $0414, TmrL
    +displayBits $DC0e, $0428, CR
    ldx TempSave
    rts
    ;end DEBUG

!macro displayShiftAmountFromAccum
  adc #$30
  sta $0700
!end

.scroll
  lda LeftScrollFlag
  beq .scrollRight
  lda $d016  
  sbc #$01
  and #$07
  sta $d016
  beq .setScrollRight
  ;debug
  ;+displayShiftAmountFromAccum
  ;end debug
  ;rts
  jmp .returnFromInterrupt
.setScrollRight
  ;debug
  ;+displayShiftAmountFromAccum
  ;end debug
  lda #$00
  sta LeftScrollFlag
  ;rts
  jmp .returnFromInterrupt
.scrollRight
  lda $d016
  ;debug
  ;+displayShiftAmountFromAccum
  ;sbc #$30
  ;end debug
  clc
  adc #$01
  and #$07
  sta $d016
  cmp #$07
  beq .setScrollLeft
  ;rts
  jmp .returnFromInterrupt
.setScrollLeft
  lda #$01
  sta LeftScrollFlag
.returnFromInterrupt
;Acknowledge and allow all interrupts again
  ;lda #%01111111      ;set all interrupt flags
  ;sta $dc0d           ;store in the IRQ control register
  ;lda $dc0d           ;address has to be READ to clear the register
  ;jmp $ea31           ;hardware interrupt service routine address. same as RTI
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
  txa      ;save the X and Y registers in case used for indices
  pha
  tya
  pha
  ldy DelaySlowness   ;configurable delay
.cycleDelayRoutine
  ldx #255
.delayRoutineNested
  dex
  bne .delayRoutineNested
  dey
  bne .cycleDelayRoutine
  pla      ;restore the Y and X registers to their values before the call to .delayRoutine
  tay
  pla
  tax
  rts

.clearLineRoutine
  ldx #00
.clearLine
  lda #$20
  sta CharacterMemory+ROW2*40, x
  inx
  cpx #40
  bne .clearLine
  rts
  
;now display it
.displayText2
  jsr .fadeAndDelayOut
  +textDisplayMacro $01, .displayText3, .nextTextString2

.displayText3
  +textDisplayMacro $02, .displayText4, .nextTextString3
  
.displayText4
  +textDisplayMacro $03, .displayText5, .nextTextString4

.displayText5
  +textDisplayMacro $04, .displayText6, .nextTextString5

.displayText6
  +textDisplayMacro $05, .displayTextFinal, .nextTextString6

.displayTextFinal
  +textDisplayMacroFinal .nextTextStringFinal

  
.clearScreen
    sei         ;disable interrupts while clearing
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
    cli         ;enable interrupts
    rts    

BRIGHTNESS_END_INDEX=3
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
  jsr .clearLineRoutine
  rts

.fadeAndDelayIn
  ldx #BRIGHTNESS_END_INDEX
.fADIFadeLoop
  stx CURRENT_DIMMER_INDEX
  jsr .fadeInColorMap
  jsr .fADODelay
  jsr .fADODisplayColorMap
  ldx CURRENT_DIMMER_INDEX
  dex
  bne .fADIFadeLoop
  rts

.fADODisplayColorMap
  ldx #00
.fADONextColor
  lda COLOR_MAP, x
  sta NextColorMemory+NextTextOffset-1, x
  inx
  cpx #28             ;no more then 28 characters in 2nd row
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
  cpx #28             ;no more then 28 characters in 2nd row
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
  cpx #28             ;no more then 28 characters in 2nd row
  bne .fCMNextColor
  rts

;just blast all BRIGHTNESS_BYTE_COUNT increments of color to the line. It's more efficient 
;than figuring out the color cycle values per character
.fadeInColorMap
  ldx #00
  ldy CURRENT_DIMMER_INDEX
  lda BRIGHTNESS, y   ;get the  next (decremented) color from the BRIGHTNESS table
.fICMNextColor
  sta COLOR_MAP, x    ;apply BRIGHTNESS color to next character
  inx
  cpx #28             ;no more then 28 characters in 2nd row
  bne .fICMNextColor
  rts
;end fade code