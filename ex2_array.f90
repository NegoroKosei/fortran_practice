! すべてのデータを2列ずつ表示させる。入力:ex2_file/input.d , 出力:ex2_file/output.d

module Add_sub
   implicit none
contains

   ! 任意の四則演算をする関数
   function func(x) result (y)
      real(8), intent(in) :: x
      real(8) y
      real(8),parameter::k=50.0d0     ! ばね定数 k の設定

      y = k * x * x / 2   ! 任意の四則演算、ここを変更する。今回はバネのエネルギーを求める

   end function func

   ! 一次元配列を受け取り、すべての要素についてfuncを実行するサブルーチン
   subroutine array_modi(x)
      real(8), intent(inout) :: x(:)  ! 形状引継ぎ配列
      integer :: i, j

      i = size(x, 1)  ! 引き継いだxのsizeを求める

      do j = 1, i
         x(j) = func(x(j))     ! すべての要素に対してfuncを実行
      end do
   end subroutine array_modi

end module Add_sub

program ex2
   use Add_sub
   implicit none

   real(8), allocatable :: x(:)  ! 割付け配列 後で大きさを指定する。
   integer, allocatable :: n(:)
   integer :: i, is, iost, max_n
   integer, parameter :: input_file_n = 10, output_file_n = 11

   max_n = 100  ! 最大配列数(行数)の指定

   ! 最大の配列数(行数)で割付け   
   allocate(x(max_n), n(max_n), stat = is)
   if (is /= 0) stop 'cannot allocate (max_n is too large)'  ! 割り付け不可なら停止

   ! 読み取り用ファイルの確認
   open(input_file_n, file = 'ex2_file/input.d', status = 'old', action = 'read', iostat = is)
   if (is /= 0) stop 'cannot open ''ex2_file/input.d'''  ! 読み取り用ファイルの確認

   ! ファイルの全要素の読み取り　1行ずつ読み取る
   do i = 1, max_n
      read(input_file_n, *, iostat = iost) n(i), x(i)    ! iost → ファイルの終了条件が検出されたとき負になる。
      if (iost < 0) exit
   end do

   max_n = i - 1                                         ! 配列の最大要素数(行)を更新

   close(input_file_n)

   call array_modi(x)  ! 1次元配列に対して任意の四則演算を実行するarray_modiを実行

   open(output_file_n, file = 'ex2_file/output.d', status = 'replace', action = 'write', iostat = is)
   if (is /= 0) stop 'cannot open output file'  ! 書き込み用ファイルの確認
   write(*, *) 'output_file : ex2_file/output.d'

   do i = 1, max_n
      write(output_file_n, '(i3, f10.5)') n(i), x(i)   !データを2列ずつ書き込み
   end do

   close(output_file_n)

end program ex2
