DECLARE SUB kodieren ()
DECLARE SUB dekodieren ()
CLEAR
Startbildschirm:
DO
  CLS
  LOCATE 1, 1
  PRINT "Willkommen zur (De) Kodiersoftware!"
  LOCATE 2, 1
  PRINT "Version 1.00"
  LOCATE 4, 1
  PRINT "1 - Text kodieren"
  LOCATE 5, 1
  PRINT "2 - Text dekodieren"
  LOCATE 6, 1
  PRINT "3 - Programm beenden"
  LOCATE 8, 1
  PRINT "(C)Copyright 2001 Daniel Marschall."
  LOCATE 9, 1
  PRINT "Alle Rechte vorbehalten."
  DO
    ir$ = UCASE$(INKEY$)
  LOOP UNTIL ir$ = "1" OR ir$ = "2" OR ir$ = "3"
  IF ir$ = "1" THEN
    PLAY "P4"
    CALL kodieren
  END IF
  IF ir$ = "2" THEN
    PLAY "P4"
    CALL dekodieren
  END IF
  IF ir$ = "3" THEN
    PLAY "P4"
    CLS
    SYSTEM
  END IF
LOOP
Fehler:
CLS
CLOSE #1
PRINT "Datei nicht gefunden!"
PLAY "P4"
CALL dekodieren
GOTO Startbildschirm
END

SUB dekodieren
CLS
i = 0
a% = 1
b% = 1
DIM let$(27)
let$(1) = "A"
let$(2) = "B"
let$(3) = "C"
let$(4) = "D"
let$(5) = "E"
let$(6) = "F"
let$(7) = "G"
let$(8) = "H"
let$(9) = "I"
let$(10) = "J"
let$(11) = "K"
let$(12) = "L"
let$(13) = "M"
let$(14) = "N"
let$(15) = "O"
let$(16) = "P"
let$(17) = "Q"
let$(18) = "R"
let$(19) = "S"
let$(20) = "T"
let$(21) = "U"
let$(22) = "V"
let$(23) = "W"
let$(24) = "X"
let$(25) = "Y"
let$(26) = "Z"
DIM inl$(27)
inl$(1) = CHR$(200)
inl$(2) = CHR$(201)
inl$(3) = CHR$(202)
inl$(4) = CHR$(203)
inl$(5) = CHR$(204)
inl$(6) = CHR$(205)
inl$(7) = CHR$(206)
inl$(8) = CHR$(207)
inl$(9) = CHR$(208)
inl$(10) = CHR$(209)
inl$(11) = CHR$(210)
inl$(12) = CHR$(211)
inl$(13) = CHR$(212)
inl$(14) = CHR$(213)
inl$(15) = CHR$(214)
inl$(16) = CHR$(215)
inl$(17) = CHR$(216)
inl$(18) = CHR$(217)
inl$(19) = CHR$(218)
inl$(20) = CHR$(219)
inl$(21) = CHR$(220)
inl$(22) = CHR$(221)
inl$(23) = CHR$(222)
inl$(24) = CHR$(223)
inl$(25) = CHR$(224)
inl$(26) = CHR$(225)
inl$(27) = CHR$(226)
100
datei$ = ""
DO
  CLS
  INPUT "Welche Datei soll dekodiert werden"; datei$
  IF datei$ = "" THEN
    GOTO Ende
  END IF
LOOP UNTIL datei$ <> ""
ON ERROR GOTO Fehler
OPEN datei$ FOR INPUT AS #1
FOR y = 1 TO 10
  IF EOF(1) = 0 THEN
    INPUT #1, line$
    code$ = code$ + line$
  END IF
NEXT y
CLOSE #1
CLS
IF MID$(code$, 1, 5) <> CHR$(67) + CHR$(79) + CHR$(68) + CHR$(1) + CHR$(1) OR MID$(code$, LEN(code$) - 2, 3) <> CHR$(1) + CHR$(1) + CHR$(1) THEN
  CLS
  PLAY "P4"
  PRINT "Diese Datei wurde nicht mit dieser Kodiersoftware kodiert!"
  PLAY "P4"
  EXIT SUB
END IF
FOR h = 6 TO LEN(code$) - 3 STEP 3
99
  IF MID$(code$, h, 3) = CHR$(36) + inl$(1) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(1)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(2) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(2)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(3) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(3)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(4) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(4)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(5) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(5)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(6) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(6)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(7) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(7)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(8) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(8)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(9) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(9)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(10) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(10)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(11) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(11)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(12) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(12)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(13) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(13)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(14) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(14)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(15) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(15)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(16) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(16)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(17) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(17)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(18) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(18)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(19) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(19)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(20) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(20)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(21) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(21)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(22) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(22)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(23) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(23)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(24) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(24)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(25) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(25)
  ELSEIF MID$(code$, h, 3) = CHR$(36) + inl$(26) + CHR$(16) THEN
    LOCATE a% + 1, i + 1
    PRINT let$(26)
  ELSE
    FOR a = 1 TO 27
      FOR b = 1 TO 255
        IF MID$(code$, h, 3) = CHR$(36) + inl$(a) + CHR$(b) THEN
          LOCATE a% + 1, i + 1
          PRINT CHR$(b)
        END IF
      NEXT b
    NEXT a
  END IF
  PLAY "T255O3L45AP45"
200
  FOR C = 27 TO 2 STEP -1
    let$(C) = let$(C - 1)
  NEXT C
  let$(1) = let$(27)
300
  i = i + 1
  IF i = 78 THEN
    i = 0
    a% = a% + 1
  END IF
NEXT h
PRINT
PRINT "Beliebige Taste drÅcken, um fortzusetzen..."
SLEEP
Ende:
END SUB

SUB kodieren
CLS
i = -1
a% = 1
b% = 1
DIM let$(27)
let$(1) = CHR$(200)
let$(2) = CHR$(201)
let$(3) = CHR$(202)
let$(4) = CHR$(203)
let$(5) = CHR$(204)
let$(6) = CHR$(205)
let$(7) = CHR$(206)
let$(8) = CHR$(207)
let$(9) = CHR$(208)
let$(10) = CHR$(209)
let$(11) = CHR$(210)
let$(12) = CHR$(211)
let$(13) = CHR$(212)
let$(14) = CHR$(213)
let$(15) = CHR$(214)
let$(16) = CHR$(215)
let$(17) = CHR$(216)
let$(18) = CHR$(217)
let$(19) = CHR$(218)
let$(20) = CHR$(219)
let$(21) = CHR$(220)
let$(22) = CHR$(221)
let$(23) = CHR$(222)
let$(24) = CHR$(223)
let$(25) = CHR$(224)
let$(26) = CHR$(225)
let$(27) = CHR$(226)
PRINT "Geben Sie den zu kodierenden Text ein:"
i = 39
1
w$ = "n"
zeichen$ = UCASE$(INKEY$)
IF LEN(code$) = 0 AND zeichen$ = CHR$(8) THEN
  zeichen$ = ""
  GOTO 1
END IF
IF zeichen$ = "" THEN
  GOTO 1
END IF
LOCATE a%, i + 1
PRINT zeichen$
IF zeichen$ = "A" THEN
  code$ = code$ + CHR$(36) + let$(1) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "B" THEN
  code$ = code$ + CHR$(36) + let$(2) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "C" THEN
  code$ = code$ + CHR$(36) + let$(3) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "D" THEN
  code$ = code$ + CHR$(36) + let$(4) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "E" THEN
  code$ = code$ + CHR$(36) + let$(5) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "F" THEN
  code$ = code$ + CHR$(36) + let$(6) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "G" THEN
  code$ = code$ + CHR$(36) + let$(7) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "H" THEN
  code$ = code$ + CHR$(36) + let$(8) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "I" THEN
  code$ = code$ + CHR$(36) + let$(9) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "J" THEN
  code$ = code$ + CHR$(36) + let$(10) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "K" THEN
  code$ = code$ + CHR$(36) + let$(11) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "L" THEN
  code$ = code$ + CHR$(36) + let$(12) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "M" THEN
  code$ = code$ + CHR$(36) + let$(13) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "N" THEN
  code$ = code$ + CHR$(36) + let$(14) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "O" THEN
  code$ = code$ + CHR$(36) + let$(15) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "P" THEN
  code$ = code$ + CHR$(36) + let$(16) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "Q" THEN
  code$ = code$ + CHR$(36) + let$(17) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "R" THEN
  code$ = code$ + CHR$(36) + let$(18) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "S" THEN
  code$ = code$ + CHR$(36) + let$(19) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "T" THEN
  code$ = code$ + CHR$(36) + let$(20) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "U" THEN
  code$ = code$ + CHR$(36) + let$(21) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "V" THEN
  code$ = code$ + CHR$(36) + let$(22) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "W" THEN
  code$ = code$ + CHR$(36) + let$(23) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "X" THEN
  code$ = code$ + CHR$(36) + let$(24) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "Y" THEN
  code$ = code$ + CHR$(36) + let$(25) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = "Z" THEN
  code$ = code$ + CHR$(36) + let$(26) + CHR$(16)
  w$ = "y"
ELSEIF zeichen$ = CHR$(8) THEN
  code$ = MID$(code$, 1, (LEN(code$)) - 3)
  LOCATE a% + 1, i + 1
  PRINT " "
  LOCATE a%, i + 1
  PRINT " "
  IF i = 0 THEN
    i = 78
    a% = a% - 1
  END IF
  i = i - 1
  LOCATE a% + 1, i + 1
  PRINT " "
  LOCATE a%, i + 1
  PRINT " "
  FOR C = 26 TO 2 STEP -1
    let$(C) = let$(C - 1)
  NEXT C
  let$(1) = let$(27)
  let$(27) = let$(26)
  i = i - 1
  w$ = "y"
  i = i + 1
  IF i = 78 THEN
    i = 0
  END IF
  IF w$ <> "y" THEN
    code$ = code$ + CHR$(36) + let$(27) + zeichen$
  END IF
  PLAY "T255O3L45AP45"
  GOTO 1
ELSEIF zeichen$ = CHR$(13) THEN
  CLS
  IF LEN(code$) = 0 THEN
    PLAY "P4"
    PLAY "P2"
    EXIT SUB
  END IF
  DO
    INPUT "Mîchten Sie den Text speichern (J/N)"; speichern$
    IF speichern$ = "J" OR speichern$ = "j" THEN
      CLS
      INPUT "Speichern als"; datei$
      IF datei$ = "" THEN
        datei$ = "code.txt"
      END IF
      OPEN datei$ FOR OUTPUT AS #1
      PRINT #1, CHR$(67);
      PRINT #1, CHR$(79);
      PRINT #1, CHR$(68);
      PRINT #1, CHR$(1);
      PRINT #1, CHR$(1);
      PRINT #1, MID$(code$, 1, LEN(code$));
      PRINT #1, CHR$(1);
      PRINT #1, CHR$(1);
      PRINT #1, CHR$(1);
      CLOSE #1
      CLS
      PLAY "P4"
      PRINT "Speicherung erfolgreich!"
      PRINT "Gespeicherte Datei:"; SPACE$(1); datei$
      PLAY "P1P1P2"
      EXIT SUB
    ELSEIF speichern$ = "N" OR speichern$ = "n" THEN
      PLAY "T32O2L16E"
      PLAY "P2"
      EXIT SUB
    END IF
  LOOP
END IF
let$(27) = let$(1)
FOR C = 1 TO 26
  let$(C) = let$(C + 1)
NEXT C
i = i + 1
IF i = 78 THEN
  i = 0
  a% = a% + 1
END IF
IF w$ <> "y" THEN
  code$ = code$ + CHR$(36) + let$(27) + zeichen$
END IF
PLAY "T255O3L45AP45"
GOTO 1
END SUB

