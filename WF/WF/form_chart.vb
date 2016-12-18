Public Class form_chart
    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        Dim Graph1 As Graphics
        Dim Pen1 As New Pen(Color.Black, 2)
        Dim Pen2 As New Pen(Color.Red, 2)
        Dim brush1 As New SolidBrush(Color.Black)
        Dim font1 As New Font("Arial", 10)
        Dim X, Y As Single
        Graph1 = Me.PictureBox1.CreateGraphics()
        'Рисуем шкалу координат
        'По оcи X
        For X = -180 To 180 Step 90
            Graph1.DrawString(X, font1, brush1, X + 180, 100)
        Next X
        'По оcи Y
        For Y = -100 To 100 Step 50
            Graph1.DrawString(Y / 50, font1, brush1, 180, 100 - Y)
        Next Y
        'Преобразование компьютерной системы координат в математическую
        'Поворот оси Y
        Graph1.ScaleTransform(1, -1)
        'Сдвиг по осям X и Y
        Graph1.TranslateTransform(180, -100)
        'Рисование осей математической системы координат
        'Ось X
        Graph1.DrawLine(Pen1, -180, 0, 180, 0)
        'Ось Y
        Graph1.DrawLine(Pen1, 0, -100, 0, 100)
        'Делаем засечки по осям координат
        'По оси X
        For X = -180 To 180 Step 90
            Graph1.DrawLine(Pen1, X, -5, X, 5)
        Next X
        'По оси Y
        For Y = -100 To 100 Step 50
            Graph1.DrawLine(Pen1, -5, Y, 5, Y)
        Next Y
        'Рисуем график функции
        For X = -180 To 180 Step 1
            Y = Math.Sin(X * 3.14 / 180)
            Graph1.DrawEllipse(Pen1, X, Y * 50, 1, 1)
        Next X
    End Sub
End Class