program ex1
   implicit none
   real(8) :: a, b, c, x1, x2, d
   complex(8) :: ix1, ix2

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
         x1 = -c / b  ! a = 0 のとき一次方程式 b * x + c = 0
         write(*,'("x1 = " f24.12)') x1
         stop
      endif
   endif

   ! 以下二次方程式として解く
   ! 判別式の計算
   d = b * b - 4.0d0 * a * c

   if (d > 0.0d0) then  ! 実数解
      x1 = (-b + sign(sqrt(d), -b)) / (2.0d0 * a)  ! 桁落ちを防ぐため同符号の解を求める
      x2 = c / (a * x1)  ! もう一つの解は、解と係数の関係より求める
      write(*,'("x1 = " f24.12)') x1
      write(*,'("x2 = " f24.12)') x2

   else if (d == 0.0d0) then  ! 重解
      x1 = -b / (2.0d0 * a)
      write(*,'("x1 = " f24.12 " (Repeated Solution)")') x1

   else  ! 虚数解
      ix1 = cmplx(-b / (2.0d0 * a), sqrt(-d) / (2.0d0 * a))
      ix2 = cmplx(-b / (2.0d0 * a), -sqrt(-d) / (2.0d0 * a))
      write(*,'(SP, "x1 = " f24.12 " " f24.12 " i")') real(ix1), aimag(ix1)
      write(*,'(SP, "x2 = " f24.12 " " f24.12 " i")') real(ix2), aimag(ix2)
   endif

end program ex1

! b**2.0d0の計算は　x^y=exp(y*ln(x))　で計算される　→　重くなる

! b**2　と整数にしてやる。もしくはb*bとする。

! ifと()の間は開ける！！！！！！！！！！！！！！！！

! 表記法"d24.16"とする！！！！！！！！！！！！！！！
! 0.324252D+01と表示される
! 24-16=8の残りの部分は先頭の空白、符号、0.、D+01の計8でピッタリになる。
! f → 小数表示(倍精度になるはず)
! e → 指数表示(単精度になることがある)
! i → 整数, a → 文字列 