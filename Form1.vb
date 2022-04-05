Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim i, j As Integer
        Do While (i < 1000)
            If i Mod 3 = 0 Or i Mod 5 = 0 Then
                j += i
                i += 1
            End If
        Loop
        MsgBox(j.ToString())
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim sayi1, sayi2, toplam, toplam2 As Integer
        sayi1 = 1
        Do While (toplam < 4000000)
            toplam = sayi1 + sayi2 ' toplam = 1 + 0 = 1
            sayi2 = sayi1          ' sayi2 = 1
            sayi1 = toplam         ' sayi1 = 1
            If toplam Mod 2 = 0 Then
                toplam2 += toplam
            End If
        Loop
        MsgBox(toplam2.ToString())
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim sayi As Int64 = 600851475143
        Dim i As Integer = 2
        Dim sonuc As Integer
        Do While (sayi > 1)
            If sayi Mod i = 0 Then
                sayi = sayi / i
                If sonuc < i Then
                    sonuc = i
                End If
            End If
            i += 1
        Loop
        MsgBox(sonuc.ToString())
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim sayi1, sayi2, sonuc1, sonuc2 As Integer
        sayi1 = 1
        Do While (sayi1 < 1000)
            sayi2 = 1
            Do While (sayi2 < 1000)
                sonuc1 = sayi1 * sayi2
                If sonuc1.ToString() = StrReverse(sonuc1.ToString()) Then
                    If sonuc2 < sonuc1 Then
                        sonuc2 = sonuc1
                    End If
                End If
                sayi2 += 1
            Loop
            sayi1 += 1
        Loop
        MsgBox(sonuc2.ToString())
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim sayi As Integer = 20
        Dim bol As Integer = 2
        Dim bolundu As Boolean = False
        Dim bolundu2 As Boolean = False
        Do While (bolundu = False)
            bolundu2 = False
            bol = 2
            Do While (bol <= 20 And bolundu2 = False)
                If sayi Mod bol = 0 Then
                    bol += 1
                Else
                    bolundu2 = True
                    sayi += 1
                End If
            Loop
            If bolundu2 = False Then
                MsgBox(sayi.ToString())
                bolundu = True
            End If
        Loop
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        Dim toplam, toplam2 As Integer
        For sayi = 1 To 100
            toplam += sayi ^ 2
        Next
        For sayi = 1 To 100
            toplam2 += sayi
        Next
        MsgBox(((toplam2 ^ 2) - toplam).ToString())
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        Dim sayac, toplam As Integer
        Dim sayi As Integer = 2
        Do While (sayac < 10001)
            toplam = 0
            For i = 2 To sayi
                If sayi Mod i = 0 Then
                    toplam += sayi
                End If
            Next
            If toplam = sayi Then
                sayac += 1
            End If
            sayi += 1
        Loop
        MsgBox((sayi - 1).ToString())
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        Dim sayi As String = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
        Dim yenisayi As String
        Dim i As Integer
        Dim sonuc As Int64
        Dim carpim As Int64 = 1
        'Do While (i < sayi.Length - 12)
        '    yenisayi = sayi.Substring(i, 13)
        '    carpim = (Convert.ToInt64(yenisayi.Substring(0, 1)) * Convert.ToInt64(yenisayi.Substring(1, 1)) * Convert.ToInt64(yenisayi.Substring(2, 1)) * Convert.ToInt64(yenisayi.Substring(3, 1)) * Convert.ToInt64(yenisayi.Substring(4, 1)) * Convert.ToInt64(yenisayi.Substring(5, 1)) * Convert.ToInt64(yenisayi.Substring(6, 1)) * Convert.ToInt64(yenisayi.Substring(7, 1)) * Convert.ToInt64(yenisayi.Substring(8, 1)) * Convert.ToInt64(yenisayi.Substring(9, 1)) * Convert.ToInt64(yenisayi.Substring(10, 1)) * Convert.ToInt64(yenisayi.Substring(11, 1)) * Convert.ToInt64(yenisayi.Substring(12, 1)))
        '    If carpim > sonuc Then
        '        sonuc = carpim
        '    End If
        '    i += 1
        'Loop

        Do While (i < sayi.Length - 12)
            yenisayi = sayi.Substring(i, 13)
            carpim = 1
            For j = 0 To 12
                carpim *= Convert.ToInt64(yenisayi.Substring(j, 1))
            Next
            If carpim > sonuc Then
                sonuc = carpim
            End If
            i += 1
        Loop
        MsgBox(sonuc.ToString())
    End Sub

    Private Sub Button15_Click(sender As Object, e As EventArgs) Handles Button15.Click
        Dim a, b, c As Integer
        a = 2
        Do While (a < 1000)
            b = a + 1
            Do While (b < 1000)
                c = b + 1
                Do While (c < 1000)
                    If c > b And b > a And a + b + c = 1000 And (c ^ 2) = (b ^ 2) + (a ^ 2) Then
                        GoTo git
                    End If
                    c += 1
                Loop
                b += 1
            Loop
            a += 1
        Loop
git:
        MsgBox("a = " + a.ToString() + " b = " + b.ToString() + " c = " + c.ToString())
    End Sub

    Private Sub Button14_Click(sender As Object, e As EventArgs) Handles Button14.Click
        Dim toplam, sayi, sayi2 As Int64
        sayi = 3
        Dim asal As Boolean = False
        Do While (sayi < 2000000)
            asal = False
            sayi2 = 2
            Do While (sayi2 < sayi And asal = False)
                If sayi Mod sayi2 = 0 Then
                    asal = True
                End If
                sayi2 += 1
            Loop
            If asal = False Then
                toplam += sayi
            End If
            sayi += 2
        Loop
        MsgBox((toplam + 2).ToString())
    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs) Handles Button13.Click
        Dim sayi As Long = 100
        For i = sayi To 2 Step -1
            sayi *= (i - 1)
        Next
        Dim sayi1 As String = sayi.ToString()
        Dim toplam As Integer = 0
        For i = 0 To sayi1.Length - 1
            toplam += Convert.ToInt32(sayi1.Substring(i, 1))
        Next
        MsgBox(toplam.ToString())
    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles Button12.Click

    End Sub
End Class
