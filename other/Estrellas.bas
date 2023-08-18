Screen 13: Cls
' Init variables
Randomize Timer
pi = 3.141592654#
s = .0001
Grav! = -455
Stars% = 50
Dim x!(Stars%), y!(Stars%)
Dim Oldx%(Stars%), Oldy%(Stars%)
Dim xvel!(Stars%), yvel!(Stars%)
Dim Xs!(Stars%), Ys!(Stars%)
x!(0) = 159
y!(0) = 70
xvel!(0) = .05
yvel!(0) = -.001
For N% = 1 To Stars% - 1
    x!(N%) = Rnd * 319
    y!(N%) = Rnd * 199
    ang = ang + 6
    a! = ang * (pi / 180)
    s = s + .005#
    Xs!(N%) = Sin(a!) * s * 1.6
    Ys!(N%) = Cos(a!) * s
    '   x!(N%) = x!(N% - 1) + Xs!(N%)
    '   y!(N%) = y!(N% - 1) + Ys!(N%)
Next N%
' Main loop
Do
    a$ = InKey$
    For N% = 0 To Stars% - 1
        ' Do Calc's
        Xdist! = 159 - x!(N%)
        Ydist! = 99 - y!(N%)
        dist! = Sqr(Xdist! * Xdist! + Ydist! * Ydist!)
        Accn! = Grav! / (dist! * dist!)
        Xaccn! = Xdist! * Accn! / dist!
        Yaccn! = Ydist! * Accn! / dist!
        xvel!(N%) = xvel!(N%) + Xaccn!
        yvel!(N%) = yvel!(N%) + Yaccn!
        x!(N%) = x!(N%) + xvel!(N%)
        y!(N%) = y!(N%) + yvel!(N%)
        ' Make sure star does not go out of screen
        If x!(N%) < 0 Then x!(N%) = 319
        If x!(N%) > 319 Then x!(N%) = 0
        If y!(N%) < 0 Then y!(N%) = 199
        If y!(N%) > 199 Then y!(N%) = 0
        ' Plot star on screen
        PSet (Oldx%(N%), Oldy%(N%)), 0
        PSet (x!(N%), y!(N%)), dist!
        Oldx%(N%) = x!(N%)
        Oldy%(N%) = y!(N%)
    Next N%
    For wr = 1 To 9000: Next
Loop Until a$ = " "

