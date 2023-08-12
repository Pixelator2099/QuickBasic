$Debug
Rem FORM
Rem Created by Chang C. (1995)
Rem www.diariotec.com


Cls
Call Rectangle(2, 2, 78, 20, 14, 1, "Title Window")

itemh = 4
Call Rectangle(3, 3 + itemh * 0, 55, itemh - 1, 14, 1, "Item 1")
Call RectangleText(3, 3 + itemh * 0, 55, itemh - 1, 14, 1, "John William Cummings (Queens, Nueva York, 8 de octubre de 1948-Los Ángeles, California, 15 de septiembre de 2004), más conocido por su nombre artístico Johnny Ramone, fue un músico estadounidense, popular por haber sido el guitarrista de la banda de punk Ramones, en la que permaneció desde su formación en 1974 hasta su concierto final en 1996. ")

Call Rectangle(3, 3 + itemh * 1, 55, itemh - 1, 14, 1, "Item 2")
Call RectangleText(3, 3 + itemh * 1, 55, itemh - 1, 14, 1, "John William Cummings (Queens, Nueva York, 8 de octubre de 1948-Los Ángeles, California, 15 de septiembre de 2004), más conocido por su nombre artístico Johnny Ramone, fue un músico estadounidense, popular por haber sido el guitarrista de la banda de punk Ramones, en la que permaneció desde su formación en 1974 hasta su concierto final en 1996. ")

Call Rectangle(3, 3 + itemh * 2, 55, itemh - 1, 14, 1, "Item 3")
Call RectangleText(3, 3 + itemh * 2, 55, itemh - 1, 14, 1, "John William Cummings (Queens, Nueva York, 8 de octubre de 1948-Los Ángeles, California, 15 de septiembre de 2004), más conocido por su nombre artístico Johnny Ramone, fue un músico estadounidense, popular por haber sido el guitarrista de la banda de punk Ramones, en la que permaneció desde su formación en 1974 hasta su concierto final en 1996. ")

Call Rectangle(3, 3 + itemh * 3, 55, itemh - 1, 14, 1, "Item 4")
Call RectangleText(3, 3 + itemh * 3, 55, itemh - 1, 14, 1, "John William Cummings (Queens, Nueva York, 8 de octubre de 1948-Los Ángeles, California, 15 de septiembre de 2004), más conocido por su nombre artístico Johnny Ramone, fue un músico estadounidense, popular por haber sido el guitarrista de la banda de punk Ramones, en la que permaneció desde su formación en 1974 hasta su concierto final en 1996. ")

Call Rectangle(3, 3 + itemh * 4, 55, itemh - 1, 14, 1, "Item 5")
Call RectangleText(3, 3 + itemh * 4, 55, itemh - 1, 14, 1, "John William Cummings (Queens, Nueva York, 8 de octubre de 1948-Los Ángeles, California, 15 de septiembre de 2004), más conocido por su nombre artístico Johnny Ramone, fue un músico estadounidense, popular por haber sido el guitarrista de la banda de punk Ramones, en la que permaneció desde su formación en 1974 hasta su concierto final en 1996. ")

Call Rectangle(60, 3, 17, 18, 10, 2, "Detail")
Call RectangleLeftAlign(60, 3, 17, 18, 10, 2, "Detail John William Cummings (Queens, Nueva York, 8 de octubre de 1948-Los Ángeles, California, 15 de septiembre de 2004), más conocido por su nombre artístico Johnny Ramone, fue un músico estadounidense.")

Sub RectangleLeftAlign (x As Integer, y As Integer, w As Integer, h As Integer, pcolor As Integer, pbgcolor As Integer, text As String)

    If (x > 80) Then x = 79
    If (y >= 24) Then y = 23

    If (w + x) >= 80 Then
        w = 79 - x
    End If

    If (h + y) >= 24 Then
        h = 23 - y
    End If


    For f = y To y + h
        Locate f + 1, x + 1
        Dim tmpW As Integer
        tmpW = 0

        For fx = 1 To w - 1
            If Mid$(text, fx, 1) = " " Then tmpW = fx
        Next

        If (tmpW = 0) Then tmpW = w

        Print Left$(text, tmpW)

        text = Mid$(text, tmpW + 1, Len(text) - tmpW)

        If f > y + h - 3 Then Exit Sub

    Next
End Sub



Sub RectangleText (x As Integer, y As Integer, w As Integer, h As Integer, pcolor As Integer, pbgcolor As Integer, text As String)
    For f = y To y + h
        Locate f + 1, x + 1
        Print Left$(text, w - 1)
        text = Mid$(text, w, Len(text) - w)

        If f > y + h - 3 Then Exit Sub

    Next
End Sub

Sub Rectangle (x As Integer, y As Integer, w As Integer, h As Integer, pcolor As Integer, pbgcolor As Integer, title As String)

    If (x > 80) Then x = 79
    If (y >= 24) Then y = 23

    If (w + x) >= 80 Then
        w = 79 - x
    End If

    If (h + y) >= 24 Then
        h = 23 - y
    End If


    Color pcolor, pbgcolor

    Rem Fill
    For fx = x To x + w
        For fy = y To y + h
            Locate fy, fx: Print Chr$(32)
        Next
    Next

    Rem Horizontal
    For f = x To x + w
        Rem TOP
        Locate y, f: Print Chr$(196)
        Rem BOTTOM
        Locate y + h, f: Print Chr$(196)
    Next

    Rem Vertical
    For f = y To y + h
        Rem TOP
        Locate f, x: Print Chr$(179)
        Rem BOTTOM
        Locate f, x + w: Print Chr$(179)

    Next

    Rem top left
    Locate y, x
    Print Chr$(218)

    Rem top right
    Locate y, x + w
    Print Chr$(191)

    Rem bottom left
    Locate y + h, x
    Print Chr$(192)

    Rem bottom rigth
    Locate y + h, x + w
    Print Chr$(217)

    If title <> "" Then
        Dim t As Integer
        t = Len(title) / 2
        Locate y, x + w / 2 - t
        Print title
    End If

End Sub
