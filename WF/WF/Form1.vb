Public Class Form1
    Dim R(8), Tsi(8) As Integer
    Dim gamma, Tgamma, Treq1, Treq2 As Double
    Private Sub RadioButton2_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton2.CheckedChanged
        R(0) = 3
        R(1) = 0
        R(2) = 2
        R(3) = 4
        R(4) = 2
        R(5) = 0
        R(6) = 1
        R(7) = 1

        Tsi(0) = 180
        Tsi(1) = 144
        Tsi(2) = 169
        Tsi(3) = 143
        Tsi(4) = 162
        Tsi(5) = 167
        Tsi(6) = 143
        Tsi(7) = 176


    End Sub

    Private Sub RadioButton3_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton3.CheckedChanged
        R(0) = 3
        R(1) = 1
        R(2) = 0
        R(3) = 0
        R(4) = 0
        R(5) = 1
        R(6) = 3
        R(7) = 2

        Tsi(0) = 160
        Tsi(1) = 187
        Tsi(2) = 143
        Tsi(3) = 129
        Tsi(4) = 126
        Tsi(5) = 121
        Tsi(6) = 200
        Tsi(7) = 157


    End Sub

    Private Sub inputBoxTextChanged(sender As Object, e As EventArgs) Handles input_box_for_E.TextChanged, input_box_for_m.TextChanged
        If RadioButton1.Checked Or RadioButton2.Checked Or RadioButton3.Checked Then
            If input_box_for_E.Text <> "" And input_box_for_m.Text <> "" Then

                btn_execute.Enabled = True
            End If
        End If
    End Sub
    Private Sub RadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton1.CheckedChanged
        R(0) = 4
        R(1) = 3
        R(2) = 0
        R(3) = 0
        R(4) = 1
        R(5) = 0
        R(6) = 0
        R(7) = 1

        Tsi(0) = 377
        Tsi(1) = 381
        Tsi(2) = 320
        Tsi(3) = 327
        Tsi(4) = 336
        Tsi(5) = 387
        Tsi(6) = 375
        Tsi(7) = 329


    End Sub
    Private Sub btn_execute_Handler(sender As Object, e As EventArgs) Handles btn_execute.Click
        Dim j, I, s As Integer
        Dim Eps, Ez, T, Tij(8), Ti(8) As Double
        Dim Tsumj(40000), Fs(10), currForTij As Double
        Ez = Val(input_box_for_E.Text)
        Eps = 0.0
        j = 0
        T = 0
        gamma = 0.85
        Tgamma = 1.4
        Treq1 = 200
        Treq2 = 400
        REM Ez - заданное значение точности моделирования
        REM Tij -  массив значений 
        REM Tsumj - 
        REM T -
        fill_initial_forms()
        For I = 0 To 7
            If R(I) > 0 Then
                Ti(I) = Tsi(I) / R(I)
            Else
                Ti(I) = Tsi(I) * 1.44
            End If
            T = T + 1 / Ti(I)
        Next
        T = 1 / T
        Tsumj(0) = T
        fill_forms_after_first_interation(Ti, T)
        Do While Eps < Ez
            For I = 0 To 7
                If R(I) = 0 Then
                    currForTij = Ti(I)

                    I = I + 1

                Else
                    s = 1
                    Do While s < R(I)

                        Randomize()
                        Fs(s) = CDbl(Rnd())
                        s = s + 1

                    Loop
                    For s = 1 To R(I)
                        currForTij = Math.Log(Fs(s))
                    Next
                    currForTij = -1 * Ti(I) / currForTij
                End If
                Tij(j) += currForTij
            Next
            Tsumj(j) = 1 / Tij(j)
            If j Mod 100 = 0 Then
                Eps += 1
            End If
        Loop
        Dim m As Integer
        m = j
        Array.Sort(Tsumj)
        Dim gamma1, gamma2 As Double
        gamma1 = (1 - gamma) / 2
        gamma2 = (1 + gamma) / 2
        Dim bottom As Integer = Math.Floor(m * gamma1)
        Dim top As Integer = Math.Floor(m * gamma2)
        Dim Tb, Tt As Double
        Tb = Tij(bottom)
        Tt = Tij(top)
    End Sub



    Private Sub fill_initial_forms()
        box_for_n.Text = 8
        box_for_Treq1.Text = Treq1
        box_for_Treq2.Text = Treq2
        box_for_y.Text = gamma
        box_for_Ty.Text = Tgamma

        box_for_R1.Text = R(0)
        box_for_R2.Text = R(1)
        box_for_R3.Text = R(2)
        box_for_R4.Text = R(3)
        box_for_R5.Text = R(4)
        box_for_R6.Text = R(5)
        box_for_R7.Text = R(6)
        box_for_R8.Text = R(7)

        box_for_T1.Text = Tsi(0)
        box_for_T2.Text = Tsi(1)
        box_for_T3.Text = Tsi(2)
        box_for_T4.Text = Tsi(3)
        box_for_T5.Text = Tsi(4)
        box_for_T6.Text = Tsi(5)
        box_for_T7.Text = Tsi(6)
        box_for_T8.Text = Tsi(7)
    End Sub
    Private Sub fill_forms_after_first_interation(Ti() As Double, T As Double)
        box_for_Ti1.Text = Ti(0)
        box_for_Ti2.Text = Ti(1)
        box_for_Ti3.Text = Ti(2)
        box_for_Ti4.Text = Ti(3)
        box_for_Ti5.Text = Ti(4)
        box_for_Ti6.Text = Ti(5)
        box_for_Ti7.Text = Ti(6)
        box_for_Ti8.Text = Ti(7)

        box_for_T.Text = T
        Dim Ts As Double
        For I As Integer = 0 To 7
            Ts += Ti(I)
        Next
        box_for_Ts.Text = Ts / 8
    End Sub
End Class
