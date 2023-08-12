SPLASHSCREEN:
Cls
_FullScreen
Screen 12 'SETTING UP A DEFAULT TRIG-EXPRESSION WINDOW

Dim DEBUG$
Dim CUSTOMF$
CUSTOMF$ = "C(X * 2) + 23 + S(X) + (5 ^ 2)"
CUSTOMF$ = "C(X)"
CUSTOMF$ = "S(X)"
CUSTOMF$ = "T(X)"

Dim num As Long ' Round number
Dim numdec As Long ' Round Decimal

PI = 3.141592
XMIN = -2 * PI
XMAX = 2 * PI
YMIN = -2
YMAX = 2
XSTEP = .025

R = 1 ' THIS WILL HELP US TO SHRINK THE SIZE OF THE GRAPHS AS THEY REPEAT THEMSELVES
Window (XMIN, YMIN)-(XMAX, YMAX)
Locate 29, 17: Print "GRAPHY - BY ROB RICE - PRESS ANY KEY TO CONTINUE"
For C1 = 1 To 14
    For X = XMIN To XMAX Step XSTEP
        A$ = InKey$: If A$ <> "" Then GoSub DEFAULTVARIABLES
        Let Y = Cos(X)
        PSet (X / R, Y / R), C1
    Next X
    Let R = R * .97 'AGAIN, THIS IS GOING TO SHRINK THE SIZE OF THE REPEATING GRAPHS
Next C1
For C2 = 1 To 14
    For X = XMIN To XMAX Step XSTEP
        Let Y = Sin(X ^ -2)
        PSet (X / R, Y / R), C2
        PSet (X / R, -Y / R), C2
        A$ = InKey$: If A$ <> "" Then GoSub DEFAULTVARIABLES
    Next X
    Let R = R / .99
Next C2
Do Until A$ <> ""
    A$ = InKey$
    For TC = 0 To 15
        Color TC: Locate 3, 36: Print "G R A P H Y"
        Let A$ = InKey$
        If A$ <> "" Then GoSub DEFAULTVARIABLES
        For D = 1 To 3000: Next D

        Randomize Timer
        Let X = (Rnd * (2 * XMAX)) - XMAX
        Let Y = (Rnd * (2 * YMAX)) + YMIN
        Let STARCOLOR = (Rnd * 15) + 1 ' PLOTS A RANDOMLY PLACED/COLORED STAR
        PSet (X, Y), STARCOLOR
        Let B1 = B1 + 1
        If B1 = 200 Then Let B1 = 0: GoSub DEFAULTVARIABLES ' ENDS AFTER 100 STARS AND STARTS THE MAIN PROGRAM WITH FRESH VARIABLES.
    Next TC

Loop
GoSub DEFAULTVARIABLES
    
DEFAULTVARIABLES: ' SETS VARIABLES AT THE START OF THE GRAPHING PROGRAM.  RESETS VARIABLES AT ANY POINT.
Let PI = 3.141593 ' HELPS SET TRIGONOMETRIC VALUES FOR WINDOWS.
Let XMIN = -2 * PI ' DEFAULT X-AXIS MINIMUM
Let XMAX = 2 * PI ' DEFAULT X-AXIS MAXIMUM
Let YMIN = -2 ' DEFAULT Y-AXIS MINIMUM
Let YMAX = 2 ' DEFAULT Y-AXIS MAXIMUM
Let XSTEP = .005 ' DEFINES RESOLUTION
'Let XSTEP = 2.025 'HACK
Let GWIN = .85 ' REDUCES THE INNER GRAPHING WINDOW AS A PERCENTAGE OF THE FULL WINDOW.
Let WC = 8 ' COLOR FOR THE INNER GRAPHING WINDOW
Let BC = 15 ' COLOR FOR THE INNER GRAPHING WINDOW BORDER
Let AC = 4 ' COLOR FOR AXES
Let GC = 7 ' COLOR FOR THE GRID
Let YC1 = 11 ' COLOR FOR THE GRAPH Y FUNCTION
Let TC = 15 ' COLOR FOR TEXT

SETWINDOW: ' SETS THE WINDOW SIZE AND THEN SETS UP INNER GRAPHING WINDOW.
Screen 12
Window (XMIN, YMIN)-(XMAX, YMAX)
Let XMIN = XMIN * GWIN ' SETTING THE INNER GRAPHING WINDOW
Let XMAX = XMAX * GWIN
Let YMIN = YMIN * GWIN
Let YMAX = YMAX * GWIN

STARTUP: ' START THE GRAPHY SEQUENCE WITHOUT RESETTING VARIABLES.
Cls
Screen 12
GoSub GRAPHY

GRAPHY: ' MAIN [GRAPH Y= ...] HANDLING SUBROUTINE
GoSub CHOOSEGRAPH ' ALLOWS THE USER TO CHOOSE FROM PRE-PROGRAMMED GRAPHING FUNCTIONS
GoSub DRAWAXES ' CALLS THE AXES TO BE DRAWN ON SCREEN
Color TC
'For X = XMIN / GWIN To XMAX / GWIN Step XSTEP
For X = XMAX / GWIN To XMIN / GWIN Step -XSTEP
    Let C = YC1
    If G$ = "1" Then Let Y = Cos(X): Let Y$ = "COS(X)" ' IF CHANGING THESE IN "CHOOSEGRAPH", CHANGE HERE TOO.
    If G$ = "2" Then Let Y = Sin(X): Let Y$ = "SIN(X)"
    If G$ = "3" Then Let Y = Tan(X): Let Y$ = "TAN(X)"
    If G$ = "4" Then Let Y = X ^ 2: Let Y$ = "X^2"
    If G$ = "5" Then Let Y = 2 * (Cos(X ^ 2)): Let Y$ = "2(COS(X^2))"
    If G$ = "6" Then Let Y = (X ^ 3) - (19 * X) + (30): Let Y$ = "X^3-19X+30"
    If G$ = "7" Then Let Y = (3 ^ (X - 1)) + (3 * (X ^ 2)) - (7 * X): Let Y$ = "(3^(X-1))+(3*(X^2))-(7*X)"

    If G$ = "9" Then

        Rem MICRO-PARSER BY CHANG C. DIARIOTEC.COM INPUT CUSTOMF$
        PARSER$ = ""

        Rem REPLACE X
        For XPARSER = 1 To Len(CUSTOMF$)
            IPARSER = Asc(Mid$(CUSTOMF$, XPARSER, 1))
            If IPARSER = Asc("X") Then
                TEMPX$ = Str$(X)
                If Left$(TEMPX$, 1) = " " Then TEMPX$ = Mid$(TEMPX$, 2)

                PARSER$ = PARSER$ + TEMPX$
            Else
                PARSER$ = PARSER$ + Chr$(IPARSER)
            End If
        Next

        INITPARSER:
        Locate 1, 1
        Print "INIT VERBOSE="; PARSER$; " X="; X; " <<<<<<<<<                             "

        'Input DEBUG$

        Rem replace ( ) segment

        PARSER_OP_A = 1 ' POSITION INIT
        PARSER_OP_B = Len(PARSER$) ' POSITION END
        PARSER_OP_C = Len(PARSER$) ' POSITION END
        VALUE_A# = 0 ' VALUE OPERATOR A
        VALUE_B# = 0 ' VALUE OPERATOR B
        VALUE_C# = 0 ' VALUE OPERATOR C
        PARSE_STAGE = 0 ' STAGE PARSE SEGMENT () / * + -

        VALUE_ACTIVE = 0
        PARSER_SIMBOL$ = ""
        TMP_A$ = ""
        TMP_B$ = ""



        For XPARSER = 1 To Len(PARSER$)
            IPARSER = Asc(Mid$(PARSER$, XPARSER, 1))
            'Print "VERBOSE: ";XPARSER; "="; Mid$(PARSER$, XPARSER, 1)
            Rem INIT SEGMENT
            If IPARSER = Asc("(") Then
                PARSER_OP_A = XPARSER
                PARSER_SIMBOL$ = "("
            End If
            Rem END SEGMENT
            If IPARSER = Asc(")") Then
                PARSER_OP_B = XPARSER
                PARSE_STAGE = 0
            End If
        Next

        Rem NOW MAGIC FOR / *

        Rem ALWAYY TEST FORMULAS LEVEL 0 COS SIN
        If VALUE_ACTIVE = 0 Then
            VALUE_A# = 0
            VALUE_B# = 0
            TMP_A$ = ""
            TMP_B$ = ""


            For XPARSER = PARSER_OP_A To PARSER_OP_B - 1

                IPARSER = Asc(Mid$(PARSER$, XPARSER, 1))
                IPARSER2 = Asc(Mid$(PARSER$, XPARSER + 1, 1))
                If IPARSER = Asc("C") Or IPARSER = Asc("S") Or IPARSER = Asc("T") Then
                    ' DONT PROCESS COS SIN TAN WITH PENDING FORMULAS
                    If IPARSER2 = Asc("(") Then Exit For

                    VALUE_ACTIVE = 1
                    PARSER_SIMBOL$ = Chr$(IPARSER)
                    VALUE_A# = 0
                    VALUE_B# = 0
                    TMP_A$ = ""
                    TMP_B$ = ""
                    IPARSER = Asc("|") 'Clean current letter from stack


                End If

                If VALUE_ACTIVE = 0 Then
                    'If IPARSER <> Asc("(") Then
                    'TMP_A$ = TMP_A$ + Chr$(IPARSER)
                    'End If
                Else


                    If VALUE_ACTIVE > 0 Then
                        If IPARSER = Asc(" ") Then Exit For
                    End If

                    If VALUE_ACTIVE = 2 And (IPARSER) <> Asc("|") Then
                        TMP_B$ = TMP_B$ + Chr$(IPARSER)
                    End If

                    If VALUE_ACTIVE = 1 And (IPARSER) <> Asc("|") Then
                        TMP_A$ = TMP_A$ + Chr$(IPARSER)
                    End If

                End If
                PARSER_OP_C = XPARSER
            Next

        End If

        Rem FORMULAS LEVEL 1 MULTI DIV POWER
        If VALUE_ACTIVE = 0 Then
            For XPARSER = PARSER_OP_A + 1 To PARSER_OP_B

                IPARSER = Asc(Mid$(PARSER$, XPARSER, 1))
                If IPARSER = Asc("/") Or IPARSER = Asc("*") Or IPARSER = Asc("^") Then
                    VALUE_ACTIVE = 1
                    PARSER_SIMBOL$ = Chr$(IPARSER)
                End If

                If VALUE_ACTIVE = 0 Then
                    If IPARSER <> Asc("(") Then
                        TMP_A$ = TMP_A$ + Chr$(IPARSER)
                    End If
                Else

                    If VALUE_ACTIVE = 2 Then
                        TMP_B$ = TMP_B$ + Chr$(IPARSER)
                    End If

                    If VALUE_ACTIVE = 1 Then
                        VALUE_ACTIVE = 2
                        PARSER_SIMBOL$ = Chr$(IPARSER)
                    End If

                End If
                PARSER_OP_C = XPARSER
            Next
        End If


        Rem FORMULAS LEVEL 2 SUM MINUS
        If VALUE_ACTIVE = 0 Then
            VALUE_A# = 0
            VALUE_B# = 0
            TMP_A$ = ""
            TMP_B$ = ""

            For XPARSER = PARSER_OP_A + 1 To PARSER_OP_B

                IPARSER = Asc(Mid$(PARSER$, XPARSER, 1))
                If IPARSER = Asc("-") Or IPARSER = Asc("+") Then
                    VALUE_ACTIVE = 1
                    PARSER_SIMBOL$ = Chr$(IPARSER)
                End If

                If VALUE_ACTIVE = 0 Then
                    If IPARSER <> Asc("(") Then
                        TMP_A$ = TMP_A$ + Chr$(IPARSER)
                    End If
                Else

                    If VALUE_ACTIVE = 2 Then
                        TMP_B$ = TMP_B$ + Chr$(IPARSER)
                    End If

                    If VALUE_ACTIVE = 1 Then
                        VALUE_ACTIVE = 2
                        PARSER_SIMBOL$ = Chr$(IPARSER)
                    End If

                End If
                PARSER_OP_C = XPARSER
            Next

        End If

        VALUE_A# = Val(TMP_A$)
        VALUE_B# = Val(TMP_B$)

        'VERBOSE
        'Print "VERBOSE VAL A="; VALUE_A#; " B="; VALUE_B#; " a$ "; TMP_A$; " B$ "; TMP_B$; "                    "
        'Print "VERBOSE POS A "; PARSER_OP_A, "B:"; PARSER_OP_B; "C:", PARSER_OP_C; "                    "
        'Print "VERBOSE SYM "; PARSER_SIMBOL$; "              "
        'Input "******************"; DEBUG$

        Rem OPERATION 1
        If (VALUE_ACTIVE = 2 And PARSER_SIMBOL$ = "/") Then
            VALUE_C# = VALUE_A# / VALUE_B#

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + Str$(VALUE_C#)
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            'Print "VERBOSE C:"; VALUE_C#; "                    "
            'Print "VERBOSE PARSER: "; PARSER$; "                    "

            GoTo INITPARSER
        End If

        Rem OPERATION 2
        If (VALUE_ACTIVE = 2 And PARSER_SIMBOL$ = "*") Then
            VALUE_C# = VALUE_A# * VALUE_B#

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + Str$(VALUE_C#)
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            'Print "VERBOSE C=:"; VALUE_C#; "C POS"; PARSER_OP_C; "= "; ""
            'Print "VERBOSE PARSER: "; PARSER$; "                    "

            GoTo INITPARSER
        End If

        Rem OPERATION 3
        If (VALUE_ACTIVE = 2 And PARSER_SIMBOL$ = "^") Then
            VALUE_C# = VALUE_A# ^ VALUE_B#

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + Str$(VALUE_C#)
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            'Print "VERBOSE C=:"; VALUE_C#; "C POS"; PARSER_OP_C; "= "; ""
            'Print "VERBOSE PARSER: "; PARSER$; "                    "

            GoTo INITPARSER
        End If

        Rem OPERATION 4
        If (VALUE_ACTIVE = 2 And PARSER_SIMBOL$ = "+") Then
            VALUE_C# = VALUE_A# + VALUE_B#

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + Str$(VALUE_C#)
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            'Print "VERBOSE C=:"; VALUE_C#; "C POS"; PARSER_OP_C; "= "; ""
            'Print "VERBOSE PARSER: "; PARSER$; "                    "

            GoTo INITPARSER
        End If

        Rem OPERATION 5
        If (VALUE_ACTIVE = 2 And PARSER_SIMBOL$ = "-") Then
            VALUE_C# = VALUE_A# - VALUE_B#

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + Str$(VALUE_C#)
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            'Print "VERBOSE C=:"; VALUE_C#; "C POS"; PARSER_OP_C; "= "; ""
            'Print "VERBOSE PARSER: "; PARSER$; "                    "

            GoTo INITPARSER
        End If

        Rem OPERATION 5
        If (PARSER_SIMBOL$ = "C") Then
            VALUE_C# = Cos(VALUE_A#)
            VALUE_STR$ = Str$(VALUE_C#)

            Rem HACK
            HACK1 = InStr(VALUE_STR$, " ")
            If HACK1 = 1 Then VALUE_STR$ = Mid$(VALUE_STR$, 2)

            HACK1 = InStr(VALUE_STR$, "-")
            If HACK1 > 1 Then
                VALUE_STR$ = Mid$(VALUE_STR$, 1, HACK1 - 1)
            End If

            HACK1 = InStr(VALUE_STR$, "D")

            If HACK1 > 1 Then
                VALUE_STR$ = Mid$(VALUE_STR$, 1, HACK1 - 1)
            End If

            Rem HACK COMPLETED

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + VALUE_STR$
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            GoTo INITPARSER
        End If

        Rem OPERATION 6
        If (PARSER_SIMBOL$ = "S") Then
            VALUE_C# = Sin(VALUE_A#)
            VALUE_STR$ = Str$(VALUE_C#)

            Rem HACK
            HACK1 = InStr(VALUE_STR$, " ")
            If HACK1 = 1 Then VALUE_STR$ = Mid$(VALUE_STR$, 2)

            HACK1 = InStr(VALUE_STR$, "-")
            If HACK1 > 1 Then
                VALUE_STR$ = Mid$(VALUE_STR$, 1, HACK1 - 1)
            End If

            HACK1 = InStr(VALUE_STR$, "D")

            If HACK1 > 1 Then
                VALUE_STR$ = Mid$(VALUE_STR$, 1, HACK1 - 1)
            End If

            Rem HACK COMPLETED

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + VALUE_STR$
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            GoTo INITPARSER
        End If

        Rem OPERATION 7
        If (PARSER_SIMBOL$ = "T") Then
            VALUE_C# = Tan(VALUE_A#)
            VALUE_STR$ = Str$(VALUE_C#)

            Rem HACK
            HACK1 = InStr(VALUE_STR$, " ")
            If HACK1 = 1 Then VALUE_STR$ = Mid$(VALUE_STR$, 2)

            HACK1 = InStr(VALUE_STR$, "-")
            If HACK1 > 1 Then
                VALUE_STR$ = Mid$(VALUE_STR$, 1, HACK1 - 1)
            End If

            HACK1 = InStr(VALUE_STR$, "D")

            If HACK1 > 1 Then
                VALUE_STR$ = Mid$(VALUE_STR$, 1, HACK1 - 1)
            End If

            Rem HACK COMPLETED

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + VALUE_STR$
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_C + 1, Len(PARSER$) - PARSER_OP_C)
            PARSER$ = TEMP1$

            GoTo INITPARSER
        End If


        Rem REMOVE PARANTESIS
        If (PARSER_SIMBOL$ = "(") Then
            SIGN = 0

            Rem HACK C
            VALUE_STR$ = Str$(VALUE_A#)
            HACK1 = InStr(VALUE_STR$, " ")
            If (HACK1 = 1) Then VALUE_STR$ = Mid$(VALUE_STR$, 2)
            Rem HACK COMPLETE

            TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + VALUE_STR$
            TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_B, Len(PARSER$) - PARSER_OP_B)
            PARSER$ = TEMP1$
            GoTo INITPARSER
        End If



        Rem NO OPERATION
        If (PARSER_SIMBOL$ = "") Then
            SIGN = 0

            'TEMP1$ = Mid$(PARSER$, 1, PARSER_OP_A - 1) + Str$(VALUE_A#)
            'TEMP1$ = TEMP1$ + Mid$(PARSER$, PARSER_OP_B, Len(PARSER$) - PARSER_OP_B)
            PARSER$ = TEMP1$
            '           Print "VERBOSE non-PARSER: "; PARSER$; "     "; Cos(X); " <<"
            '           Input DEBUG$
            Let Y = Val(PARSER$)

        End If



    End If

    PSet (X * GWIN, 0), AC ' CHANGES X-AXIS FROM GRAPH COLOR TO DEFAULT AXIS.  SHOWS X-PROGRESSION ON SLOW-MOVING GRAPHING FUNCTIONS.
    ' BELOW PLOTS (X,Y) UNLESS Y IS OUT OF BOUNDS.
    If Y < (YMAX / GWIN) And Y > (YMIN / GWIN) Then PSet (X * GWIN, Y * GWIN), C
    A$ = InKey$: If A$ <> "" Then GoSub ABORT
Next X
Locate 2, 2: Color TC: Print "      GRAPH Y = "; Y$;
Locate 29, 30: Color TC: Print "PRESS ANY KEY TO CONTINUE. ";
Do Until A$ <> ""
    A$ = InKey$
Loop
GoSub STARTUP

CHOOSEGRAPH: ' ALLOWS USER TO SELECT GRAPH FUNCTION OR TO CHANGE SETTINGS
Cls
GoSub PRINTPARAMETERS
Locate 2, 5: Print "WELCOME TO BASIC GRAPHING CALCULATOR."
Print
Locate 3, 5: Print "GRAPH: Y = "
Locate 5, 5: Print "1: COS(X)" ' IF CHANGING THESE, YOU MUST ALSO CHANGE IN GRAPHY:
Locate 6, 5: Print "2: SIN(X)"
Locate 7, 5: Print "3: TAN(X)"
Locate 8, 5: Print "4: X^2"
Locate 9, 5: Print "5: 2(COS(X^2))"
Locate 5, 23: Print "6: X^3-19X+30"
Locate 6, 23: Print "7: (3^(X-1))+(3*(X^2))-(7*X)"
Locate 7, 23: Print "8: Custom Formula"
Locate 8, 23: Print CUSTOMF$
Locate 9, 23: Print "9: RUN Custom Formula"

Locate 5, 55: Print "W: WINDOW LIMITS"
Locate 6, 55: Print "C: CHANGE COLORS"
Locate 7, 55: Print "R: RESET"
Locate 8, 55: Print "I: INSTRUCTIONS"
Locate 9, 55: Print "X: EXIT"
Locate 10, 5: For P = 5 To 70: Print "_";: Next P
Locate 11, 5: Input "PLEASE CHOOSE: ", G$
G$ = UCase$(G$)
If G$ = "1" Then Return ' IF ADDING GRAPHS, ADD VALID OPTIONS HERE.
If G$ = "2" Then Return
If G$ = "3" Then Return
If G$ = "4" Then Return
If G$ = "5" Then Return
If G$ = "6" Then Return
If G$ = "7" Then Return
If G$ = "9" Then Return
If G$ = "8" Then GoSub SETCUSTOMFORMULA:
If G$ = "W" Then GoSub SETWINLIMITS: GoSub SETWINDOW: GoSub GRAPHY
If G$ = "C" Then GoSub CHANGECOLORS: GoSub GRAPHY
If G$ = "R" Then GoSub DEFAULTVARIABLES
If G$ = "I" Then GoSub INSTRUCTIONS
If G$ = "X" Then GoSub EXITPROGRAM
GoSub PROGRAMERROR: GoSub GRAPHY
Return

DRAWAXES: ' DRAWS AXES INSIDE THE INNER GRAPHING WINDOW
Cls
For X = XMIN To XMAX Step XSTEP * 2 ' USES 1/2 THE RESOLUTION AS THE ACTUAL GRAPH.
    A$ = InKey$: If A$ <> "" Then GoSub ABORT
    Line (X, YMIN)-(X, YMAX), WC ' FILLS THE INNER WINDOW BY DRAWING VERTICAL LINES FROM L TO R.
Next X
Line (XMIN, YMAX)-(XMAX, YMAX), BC ' DRAWS THE OUTTER BORDER
Line (XMAX, YMAX)-(XMAX, YMIN), BC
Line (XMAX, YMIN)-(XMIN, YMIN), BC
Line (XMIN, YMIN)-(XMIN, YMAX), BC
Line (XMIN, 0)-(XMAX, 0), YC1 ' DRAWS THE X-AXIS. NOTE, IT STARTS THE SAME COLOR AS THE GRAPH BUT CHANGES TO THE AXIS COLOR IN "GRAPHY"
Line (0, YMIN)-(0, YMAX), AC ' DRAWS TTHE Y-AXIS.
Line (XMIN, (YMIN * .25))-(XMAX, (YMIN * .25)), GC ' DRAWS GRIDS TO THE LEFT/RIGHT/TOP/BOTTOM OF THE AXIS IN 25% INCREMENTS.
Line (XMIN, (YMIN * .5))-(XMAX, (YMIN * .5)), GC
Line (XMIN, (YMIN * .75))-(XMAX, (YMIN * .75)), GC
Line (XMIN, (YMAX * .25))-(XMAX, (YMAX * .25)), GC
Line (XMIN, (YMAX * .5))-(XMAX, (YMAX * .5)), GC
Line (XMIN, (YMAX * .75))-(XMAX, (YMAX * .75)), GC
Line ((XMIN * .25), YMIN)-((XMIN * .25), YMAX), GC
Line ((XMIN * .5), YMIN)-((XMIN * .5), YMAX), GC
Line ((XMIN * .75), YMIN)-((XMIN * .75), YMAX), GC
Line ((XMAX * .25), YMIN)-((XMAX * .25), YMAX), GC
Line ((XMAX * .5), YMIN)-((XMAX * .5), YMAX), GC
Line ((XMAX * .75), YMIN)-((XMAX * .75), YMAX), GC
Return

ABORT: ' ABORTS ANY SEQUENCE WHICH MAY TAKE TOO LONG.  THIS COULD BE DUE  TO LARGE WINDOW DIMENSIONS AND/OR HIGH RESOLUTION.
Cls
Let WINNAME$ = "--- PROCEDURE ABORTED ----"
Let WINDESC$ = ""
Let WINL1$ = "THE PROCEDURE HAS BEEN STOPPED      "
Let WINL2$ = "VIA USER COMMAND.                   "
Let WINL3$ = "                                    "
Let WINL4$ = "PRESS ANY KEY TO CONTINUE.          "
Let WINL5$ = "                                    "
GoSub DRAWWINDOW
A$ = InKey$
Do Until A$ <> ""
    A$ = InKey$
Loop
GoSub STARTUP
    
SETWINLIMITS:
Cls
Locate 5, 10: Print "SETTING WINDOW LIMITS."
Locate 6, 10: Print "WHAT WOULD YOU LIKE YOUR X-AXIS MINIMUM TO BE?"
Locate 7, 10: Print "DEFAULTS: TRIG = -6.2832.  GENERAL = -10"
Locate 15, 10: Input "XMIN? ", XMIN
Cls
Locate 5, 10: Print "SETTING WINDOW LIMITS."
Locate 6, 10: Print "WHAT WOULD YOU LIKE YOUR X-AXIS MAXIMUM TO BE?"
Locate 7, 10: Print "DEFAULTS: TRIG = 6.2832.  GENERAL = 10"
Locate 15, 10: Input "XMAX? ", XMAX
Let XSPREAD = XMAX - XMIN
If XSPREAD > 40 Then Locate 17, 10: Print "XMAX - XMIN CANNOT BE > 40.  TRY AGAIN.": Input "PRESS ENTER TO CONTINUE. ", ENTER$: GoTo SETWINLIMITS
Cls
Locate 5, 10: Print "SETTING WINDOW LIMITS."
Locate 6, 10: Print "WHAT WOULD YOU LIKE YOUR X-STEP (RESOLUTION FACTOR)"
Locate 7, 10: Print "TO BE?  0.005 IS STANDARD. THE LOWER THIS NUMBER,"
Locate 8, 10: Print "THE HIGHER THE RESOLUTION AND THE SLOWER THE GRAPH."
Locate 15, 10: Input "X-STEP? ", XSTEP
Let XRES = XSPREAD / XSTEP
If XRES > 40000 Then Locate 17, 10: Print "YOUR INPUT RESULTS IN A GRAPH THAT IS TOO SLOW.  TRY A HIGHER XSTEP VALUE.": Input "PRESS ENTER TO CONTINUE. ", ENTER$: GoTo SETWINLIMITS
Cls
Locate 5, 10: Print "SETTING WINDOW LIMITS."
Locate 6, 10: Print "WHAT WOULD YOU LIKE YOUR Y-AXIS MINIMUM TO BE?"
Locate 7, 10: Print "DEFAULTS: TRIG = -2.  GENERAL = -10"
Locate 15, 10: Input "YMIN? ", YMIN
Cls
Locate 5, 10: Print "SETTING WINDOW LIMITS."
Locate 6, 10: Print "WHAT WOULD YOU LIKE YOUR Y-AXIS MAXIMUM TO BE?"
Locate 7, 10: Print "DEFAULTS: TRIG = 2.  GENERAL = 10"
Locate 15, 10: Input "YMAX? ", YMAX
Let YSPREAD = YMAX - YMIN
If YSPREAD > 40 Then Locate 17, 10: Print "YMAX - YMIN CANNOT BE > 40.  TRY AGAIN.": Input "PRESS ENTER TO CONTINUE. ", ENTER$: GoTo SETWINLIMITS:
Return

CHANGECOLORS:
Cls
Print "COLOR CODES:"
Print "NOTE: 0 = BLACK"
For X = 0 To 15
    Color X
    Print X;
Next X
Print
Color 7: Locate 5, 5: Input "WINDOW COLOR (DEFAULT IS 8) ", WC: Locate 5, 40: Color WC: Print "GRAPHING WINDOW COLOR "
Color 7: Locate 6, 5: Input "BORDER COLOR? (DEFAULT IS 4) ", BC: Locate 6, 40: Color BC: Print "BORDER COLOR "
Color 7: Locate 7, 5: Input "AXIS COLOR? (DEFAULT 15) ", AC: Locate 7, 40: Color AC: Print "AXIS COLOR "
Color 7: Locate 8, 5: Input "GRID COLOR? (DEFAULT IS 7) ", GC: Locate 8, 40: Color GC: Print "GRID COLOR "
Color 7: Locate 9, 5: Input "Y COLOR? (DEFAULT IS 11) ", YC1: Locate 9, 40: Color YC1: Print "GRAPH FUNCTION COLOR "
Color 7: Locate 10, 5: Input "TEXT COLOR (DEFAULT IS 15) ", TC: Locate 10, 40: Color TC: Print "TEXT COLOR "
Color 7: Locate 12, 5: Print "PLEASE MAKE A SELECTION:"
Color 7: Locate 13, 5: Print "R > REDO COLOR SELECTIONS."
Color 7: Locate 14, 5: Print "C > CONTINUE BACK TO GRAPH SELECTION. [DEFAULT]"
Color 7: Locate 15, 5: Input "YOUR SELECTION: ", SEL$
If SEL$ = "R" Then GoSub CHANGECOLORS
If SEL$ = "C" Then GoSub STARTUP
GoSub STARTUP
Return

PROGRAMERROR:
Sound 500, 1
Sound 300, .75
Cls
Print
Print
Print "PROGRAM ERROR.  PRESS ENTER TO CONTINUE."
Input IGNORE$
Return

EXITPROGRAM:
Cls
Print "THANK YOU FOR GRAPHING WITH BASIC."
Print "PRESS ENTER TO END."
End

PRINTPARAMETERS:
Color 15
Locate 16, 8: Print Chr$(218)
For X = 9 To 74: Locate 16, X: Print Chr$(196): Next X
Locate 16, 75: Print Chr$(191)
For Y = 17 To 21: Locate Y, 75: Print Chr$(179): Next Y
Locate 22, 75: Print Chr$(217)
For X = 9 To 74: Locate 22, X: Print Chr$(196): Next X
Locate 22, 8: Print Chr$(192)
For Y = 17 To 21: Locate Y, 8: Print Chr$(179): Next Y
Locate 17, 10: Print "WINDOW PARAMETERS:"
For X = 9 To 74: Locate 19, X: Print Chr$(177);: Next X
Locate 20, 10: Print "XMIN": Locate 21, 10: Print XMIN / GWIN
Locate 20, 23: Print "XMAX": Locate 21, 23: Print XMAX / GWIN
Locate 20, 36: Print "YMIN": Locate 21, 36: Print YMIN / GWIN
Locate 20, 49: Print "YMAX": Locate 21, 49: Print YMAX / GWIN
Locate 20, 62: Print "X-STEP": Locate 21, 62: Print XSTEP
GoSub PRINTCOLORS
Return

PRINTCOLORS:
Locate 23, 9: Color 15: Print "COLORS:"
Locate 23, 20: Color WC: Print "WINDOW"
Locate 23, 30: Color BC: Print "BORDER"
Locate 23, 40: Color AC: Print "AXIS"
Locate 23, 50: Color GC: Print "GRID"
Locate 23, 60: Color YC1: Print "GRAPH"
Locate 23, 70: Color TC: Print "TEXT"
Color 15
Return

DRAWWINDOW:
Screen 12
Cls
Color 11
Locate 4, 10: Print Chr$(218)
Locate 4, 60: Print Chr$(191)
Locate 25, 10: Print Chr$(192)
Locate 25, 60: Print Chr$(217)
For LL = 5 To 24: Locate LL, 10: Print Chr$(179): Next LL
For RL = 5 To 24: Locate RL, 60: Print Chr$(179): Next RL
For UL = 11 To 59: Locate 4, UL: Print Chr$(196): Next UL
For BL = 11 To 59: Locate 25, BL: Print Chr$(196): Next BL
For UB = 11 To 59: Locate 5, UB: Print Chr$(178): Next UB
Color 11: Locate 5, 22: Print WINNAME$
For BB = 11 To 59: Locate 7, BB: Print Chr$(176): Next BB
Color 11: For UL = 11 To 59: Locate 6, UL: Print Chr$(196): Next UL
Color 14: Locate 7, 22: Print WINDESC$
Color 11: For UL = 11 To 59: Locate 8, UL: Print Chr$(196): Next UL
Color 15
' BEST TO USE INSERT FUNCTION WHEN TYPING BELOW
Locate 9, 12: Print WINL1$
Locate 10, 12: Print WINL2$
Locate 11, 12: Print WINL3$
Locate 12, 12: Print WINL4$
Locate 13, 12: Print WINL5$
Let WINNAME$ = "----- GRAPH SELECTION ----" ' COPY THIS AND INSERT INTO ANYWHERE CALLING A WINDOW TO BE DRAWN.
Let WINDESC$ = "-- CHOOSE A GRAPH BELOW --" ' THE SUBROUTINE CALLING FOR THE WINDOW TO BE DRAWN WILL NEED THESE LINES.
Let WINL1$ = "                                    "
Let WINL2$ = "                                    "
Let WINL3$ = "                                    "
Let WINL4$ = "                                    "
Let WINL5$ = "                                    "
Return

INSTRUCTIONS:
Cls
Locate 2, 5: Print "---GRAPHY PROGRAM INSTRUCTIONS---"
Locate 3, 5: Print "USE THIS PROGRAM TO PLOT VARIOUS GRAPHS."
Locate 4, 5: Print "KEEP YOUR CAPSLOCK ON."
Locate 5, 5: Print "MAKE A SELECTION FROM BELOW:"
Locate 6, 5: Print "1. RETURN TO SPLASH SCREEN"
Locate 7, 5: Print "2. RESTART PROGRAM (RESET VARIABLES AND COLORS)"
Locate 8, 5: Print "PRESS ANY OTHER KEY TO GO BACK TO WHERE YOU LEFT OFF."
Locate 10, 5: Input "MAKE A SELECTION: ", SELECTION$
If SELECTION$ = "1" Then GoSub SPLASHSCREEN
If SELECTION$ = "2" Then GoSub DEFAULTVARIABLES
GoSub STARTUP

SETCUSTOMFORMULA:
Color 12
Locate 13, 5: Print "PLEASE INPUT YOUR CUSTOM FORMULA: (EX. X * 3) "
Locate 14, 5: Input "FORMULA: ", CUSTOMF$

CUSTOMF$ = UCase$(CUSTOMF$)

CLEANUPFORMULA:
Rem CLEAN UP FORMULA
TEMP$ = ""
For X = 1 To Len(CUSTOMF$)
    I = Asc(Mid$(CUSTOMF$, X, 1))
    Rem REMOVE INVALID LETTER
    If I = Asc("X") Or I = Asc("C") Or I = Asc("S") Or I = Asc("T") Or I = Asc("(") Or I = Asc(")") Or (I >= Asc("0") And I <= Asc("9")) Then
        TEMP$ = TEMP$ + Chr$(I)
    End If

    If I = Asc("+") Or I = Asc("-") Or I = Asc("*") Or I = Asc("^") Or I = Asc("/") Then
        TEMP$ = TEMP$ + " " + Chr$(I) + " "
    End If


Next
CUSTOMF$ = TEMP$

Color 7


GoSub STARTUP

Return
    
End

