! メインルーチンは、program [プログラム名] という行で始まり、endで終わる
program example
! 中身は二つのブロック（宣言部と実行部）から成る
! 宣言部：記号の定義、実行部：本体

! implicit noneは、バグを発見しやすくするために常に記述
! 書かないと、宣言していない変数が現れてもコンパイラがエラーを検出しないことがある
  implicit none
! 型宣言
  real x, y
  integer i
! 定数は、parameter文で値を宣言しておく
  integer, parameter::IMAX=5

! do 変数 = 初期値, 終値(, 増分)　~ enddo
  do i=1, IMAX
!-- print文でカンマ後の文字列、変数をディスプレイに表示する
    print *, 'x=?'
!-- read文で、キーボオードからの入力待ちとなる（xに入力値が代入される）
    read *, x
!-- call文で、サブルーチンが呼び出されてyにf(x)の値が代入される
    call func(x, y)
    print *, 'f(x)=', y
!-- if(~ endif)文で条件を記述
    if (y > 0) then
      print *, 'f(x) is positive'
    elseif (y == 0) then
      print *, 'f(x) is equal to zero'
    else
      print *, 'f(x) is negative'
    endif
  enddo

! stop文でプログラムは実行を停止する
  stop
end

! サブルーチンではreturn文で処理を終えてメインルーチンに戻る（注：stop文ではない）
subroutine func(t, ft)
  implicit none
  real t, ft
  ft= t**2 - 1
  return
end
