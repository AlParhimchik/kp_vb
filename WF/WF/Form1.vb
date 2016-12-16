Public Class Form1
    Dim R(8), Tsumi(8) As Integer
    Dim gamma, Tgamma, Treq1, Treq2 As Double
    Dim n As Integer = 8
    Private Sub RadioButton2_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton2.CheckedChanged
        R(0) = 3
        R(1) = 0
        R(2) = 2
        R(3) = 4
        R(4) = 2
        R(5) = 0
        R(6) = 1
        R(7) = 1

        Tsumi(0) = 180
        Tsumi(1) = 144
        Tsumi(2) = 169
        Tsumi(3) = 143
        Tsumi(4) = 162
        Tsumi(5) = 167
        Tsumi(6) = 143
        Tsumi(7) = 176

        If input_box_for_E.Text <> "" Or input_box_for_m.Text <> "" Then

            btn_execute.Enabled = True
        End If
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

        Tsumi(0) = 160
        Tsumi(1) = 187
        Tsumi(2) = 143
        Tsumi(3) = 129
        Tsumi(4) = 126
        Tsumi(5) = 121
        Tsumi(6) = 200
        Tsumi(7) = 157
        If input_box_for_E.Text <> "" Or input_box_for_m.Text <> "" Then

            btn_execute.Enabled = True
        End If

    End Sub

    Private Sub inputBoxTextChanged(sender As Object, e As EventArgs) Handles input_box_for_E.TextChanged, input_box_for_m.TextChanged
        If RadioButton1.Checked Or RadioButton2.Checked Or RadioButton3.Checked Then
            If input_box_for_E.Text <> "" Or input_box_for_m.Text <> "" Then

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

        Tsumi(0) = 377
        Tsumi(1) = 381
        Tsumi(2) = 320
        Tsumi(3) = 327
        Tsumi(4) = 336
        Tsumi(5) = 387
        Tsumi(6) = 375
        Tsumi(7) = 329
        If input_box_for_E.Text <> "" Or input_box_for_m.Text <> "" Then

            btn_execute.Enabled = True
        End If
    End Sub
    Private Sub btn_execute_Handler(sender As Object, e As EventArgs) Handles btn_execute.Click
        Dim T, Tsum(8), Ti(8), Tlast_sumi(8), Tsumj(40000), Eps, F(8), Ez As Double
        Dim i, j, s, m, m_max As Int64
        j = 1
        If input_box_for_E.Text <> "" Then
            Ez = Val(input_box_for_E.Text)
        End If
        If input_box_for_m.Text <> "" Then
            m_max = Val(input_box_for_m.Text)
        End If
        Eps = 0.0
        T = 0
        gamma = 0.85
        Tgamma = 1.4
        Treq1 = 200
        Treq2 = 400
        fill_initial_forms()
        'Dim j, I, s As Integer
        'Dim Eps, Ez, T, Ti(8) As Double
        'Dim Tsumj(40000), Fs(10), currForTij As Double

        'REM Ez - заданное значение точности моделирования
        'REM Tij -  массив значений 
        'REM Tsi - входной массив T 

        For i = 0 To n - 1
            If R(i) > 0 Then
                Ti(i) = Tsumi(i) / R(i)
            Else
                Ti(i) = Tsumi(i) * 1.44
            End If
            T = T + 1 / Ti(i)
        Next
        T = 1 / T
        Tsumj(0) = T
        fill_forms_after_first_interation(Ti, T)
        Do While Eps <= Ez And j < m_max
            For i = 0 To n - 1
                If R(i) = 0 Then
                    Tlast_sumi(i) = Ti(i)

                Else
                    s = 1
                    Do While s < R(i)
                        Randomize()
                        F(s) += CDbl(Rnd())
                        s = s + 1
                    Loop
                    For s = 1 To R(i)
                        Tlast_sumi(i) += Math.Log(F(s))
                    Next
                End If
                Tlast_sumi(i) += -1 * Ti(i) / Tlast_sumi(i)
            Next
            Tsumj(j) += 1 / Tlast_sumi(i)
            Tsumj(j) = 1 / Tsumj(j)
            If j Mod 100 = 0 Then
                Dim Temp1, Temp2 As Double
                Temp1 = Temp2 = 0
                For i = 0 To j
                    Temp1 += Tsumj(i)
                    Temp2 += Tsumj(i) * Tsumj(i)
                Next

                Eps = Tgamma * Math.Sqrt((Math.Pow(Temp1, -2) * Temp2 * 1 / j) * (j / (j - 1)))

            End If
            j += 1
        Loop
        m = j
        T = 0
        For s = 0 To n - 1
            T += 1 / Tlast_sumi(s)
        Next
        T = 1 / T
        fill_forms_after_last_iteration(Tlast_sumi, T)
        Array.Sort(Tsumj)
        Dim gamma1, gamma2 As Double
        gamma1 = (1 - gamma) / 2
        gamma2 = (1 + gamma) / 2
        Dim bottom As Integer = Math.Floor(m * gamma1)
        Dim top As Integer = Math.Floor(m * gamma2)
        Dim Tb, Tt As Double
        Tb = Tsumj(bottom)
        Tt = Tsumj(top)
        REM  заполнение оставшихся форм
        box_for_gamma1.Text = gamma1
        box_for_gamma2.Text = gamma2
        box_for_Tmin.Text = Tsumj(0)
        box_for_Tmax.Text = Tsumj(m)
        box_for_Tb.Text = Tb
        box_for_Ttop.Text = Tt


    End Sub


    Private Sub fill_forms_after_last_iteration(Tlast() As Double, T As Double)
        box_for_Ts1_after_ex.Text = Tlast(0)
        box_for_Ts2_after_ex.Text = Tlast(1)
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

        box_for_T1.Text = Tsumi(0)
        box_for_T2.Text = Tsumi(1)
        box_for_T3.Text = Tsumi(2)
        box_for_T4.Text = Tsumi(3)
        box_for_T5.Text = Tsumi(4)
        box_for_T6.Text = Tsumi(5)
        box_for_T7.Text = Tsumi(6)
        box_for_T8.Text = Tsumi(7)
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
