! 計算誤差を考えて重解の判定に幅を持たせる。
! 今回は誤差を重解の方に丸めたが、虚数解や実数解２つの場合の判定にも誤差があるため
! 場合によって使い分けをする。

program ex1
   implicit none
   real(8) :: a, b, c, x1, x2, d
   complex(8) :: ix1, ix2
   real(8), parameter :: epsilon = 1d-14  !重解に丸める誤差の値

   ! 係数a, b, cの入力
   write(*,'(a)', advance='no') 'input a, b, c :'
   read(*,*) a, b, c

   ! 二次方程式でない場合の例外処理
   if (a == 0.0d0) then
      if (b == 0.0d0) then
         if (c == 0.0d0) then
            write(*,*) 'x is indeterminate'  ! a = b = c = 0 のとき不定解
            stop
         else
            write(*,*) 'No solution'  ! a = b = 0 かつ c /= 0 のとき解がない
            stop
         endif
      else
         x1 = -c / b  ! a = 0のとき一次方程式 b*x + c = 0
         write(*,'("x1 = " f24.12)') x1
         stop
      endif
   endif

   ! 以下二次方程式として解く
   ! 判別式の計算
   d = b * b - 4.0d0 * a * c

   if (d > epsilon) then  ! 実数解
      x1 = (-b + sign(sqrt(d), -b)) / (2.0d0 * a)  ! 桁落ちを防ぐため同符号の解を求める
      x2 = c / (a * x1)  ! もう一つの解は、解と係数の関係より求める
      write(*,'("x1 = " f24.12)') x1
      write(*,'("x2 = " f24.12)') x2

   else if (abs(d) <= epsilon) then  ! 重解
      x1 = -b / (2.0d0 * a)
      write(*,'("x1 = " f24.12 " (Repeated Solution)")') x1

   else  ! 虚数解
      ix1 = cmplx(-b / (2.0d0 * a), sqrt(-d) / (2.0d0 * a))
      ix2 = cmplx(-b / (2.0d0 * a), -sqrt(-d) / (2.0d0 * a))
      write(*,'(SP, "x1 = " f24.12 " " f24.12 " i")') real(ix1), aimag(ix1)
      write(*,'(SP, "x2 = " f24.12 " " f24.12 " i")') real(ix2), aimag(ix2)
   endif

end program ex1
