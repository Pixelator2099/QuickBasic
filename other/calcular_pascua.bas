' **********************************************************************
' **********************************************************************
' ** **
' ** P R O G R A M A D O M I N G O D E P A S C U A **
' ** Version 1.0 **
' ** (C) Alberteotl Tzompancoatzin, 2023 **
' ** **
' ** Calcula el dia y el mes en los que caera el Domingo de Pascua **
' ** a partir de cualquier ano posterior a 1583 suministrado por el **
' ** usuario, con base en las formulas del Algoritmo de Butcher. **
' ** **
' **********************************************************************
' **********************************************************************
DECLARE SUB IntroduceDatos ()
DECLARE SUB Calcula ()
DECLARE SUB ImprimeFecha ()
DECLARE SUB ImprimeAlgoritmo ()
DECLARE SUB ImprimeMenu (RES AS STRING)
Dim Shared A, B, C, D, E, F, G, H, I, K, L, M, N, ANO, MES, DIA, ME$
IntroduceDatos
10 If ANO = 999 Then End
Calcula
Print: Print: Print: Print
ImprimeFecha
Print: Print: Print: Print: Print
ImprimeMenu (RESPUESTA$)
Print "Tu opcion fue "; RESPUESTA$
Select Case RESPUESTA$
    Case "F"
        IntroduceDatos
        GoTo 10
    Case "A": ImprimeAlgoritmo
    Case "X": End
End Select
Sub Calcula
    A = ANO Mod 19
    B = Fix(ANO / 100)
    C = ANO Mod 100
    D = Fix(B / 4)
    E = B Mod 4
    F = Fix((B + 8) / 25)
    G = Fix((B - F + 1) / 3)
    H = (19 * A + B - D - G + 15) Mod 30
    I = Fix(C / 4)
    K = C Mod 4
    L = (32 + 2 * E + 2 * I - H - K) Mod 7
    M = Fix((A + 11 * H + 22 * L) / 451)
    N = H + L - 7 * M + 114
    MES = Fix(N / 31)
    DIA = 1 + (N - (MES * 31))
    If MES = 4 Then ME$ = "Abril"
End Sub



Sub ImprimeAlgoritmo
    Cls
    Color 14, 0
    Print "A = Residuo de A"; Chr$(164); "o/19 = "; A
    Print "B = Cociente de A"; Chr$(164); "o/100 = "; B
    Print "C = Residuo de A"; Chr$(164); "o/100 = "; C
    Print "D = Cociente de B/4 = "; D
    Print "E = Residuo de B/4 = "; E
    Print "F = Cociente de (B+8)/25 = "; F
    Print "G = Cociente DE (B-F+1)/3 = "; G
    Print "H = Residuo de (19*A+B-D-G+15)/30 = "; H
    Print "I = Cociente C/4 = "; I
    Print "K = Residuo de C/4 = "; K
    Print "L = Residuo de (32+2*E+2*I-H-K)/7 = "; L
    Print "M = Cociente (a+11*H+22*L)/451 = "; M
    Print "N = H+L-7*M+114 = "; N
    Print "NUmero de Mes = Cociente de N/31 = "; MES
    Print "Dia = 1+(N-(MES*31))"
    Print: Print: Print
End Sub
Sub ImprimeFecha
    Print
    Color 14, 0
    Print "En el a"; Chr$(164); "o"; ANO; ", el Domingo de Pascua cae en "; DIA; " de "; ME$
End Sub
Sub ImprimeMenu (RES As String)
    Color 12, 0
    Print "MENU"
    Print "[F] Probar otra fecha"
    Print "[A] Mostrar resultados de algoritmo"
    Print "[X] Salir"
    Do
        RES = UCase$(InKey$)
    Loop While RES = ""
End Sub
Sub IntroduceDatos
    ANO = 0
    ME$ = "Marzo"
    Cls
    Print " ** (C) Alberteotl Tzompancoatzin, 2023 **"
    Print " ** **"
    Print " ** Calcula el dia y el mes en los que caera el Domingo de Pascua **"
    Print " ** a partir de cualquier ano posterior a 1583 suministrado por el **"
    print " ** usuario, con base en las formulas del Algoritmo de Butcher. **


    Print
    Print "Introduce, a cuatro cifras,"
    Print "un a"; Chr$(164); "o mayor a 1583"
    Print "en el que deseas calcular el"
    Print "domingo de Pascua. Escribe 999"
    Print "para salir: ";
    Do
        Input ANO
    Loop While ANO <= 1586
End Sub

