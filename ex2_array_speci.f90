! データを行ごとにファイルに分割してすべて出力する。
! 最大値をとるデータを別のファイルに出力する。
! 入力ファイル:'ex2_speci_file/input.d'
! 出力ファイル:"ex2_speci_file/outout'ファイル番号'.d"
! 最大値出力ファイル:"ex2_speci_file/outout_max'ファイル番号'.d"

module Add_sub
   implicit none
contains

   ! 任意の四則演算をする関数
   function func(x) result (y)
      real(8), intent(in) :: x
      real(8) :: y
      real(8), parameter :: k = 50.0d0     ! ばね定数 k の設定

      y = k * x * x / 2   ! 任意の四則演算、ここを変更する。今回はバネのエネルギーを求める

   end function func

   ! 一次元配列を受け取り、すべての要素についてfuncを実行するサブルーチン
   subroutine array_modi(x)
      real(8), intent(inout) :: x(:)  ! 形状引継ぎ配列
      integer :: i, j

      i = size(x, 1)  ! 引き継いだxのsizeを求める

      do j = 1, i
         x(j) = func(x(j))            ! すべての要素に対してfuncを実行
      end do
   end subroutine array_modi
end module Add_sub

program ex2
   use Add_sub
   implicit none

   real(8), allocatable :: x(:)  ! 割付け配列 後で大きさを指定する。
   integer, allocatable :: n(:)
   integer :: i, is, iost, max_n, max_array_n
   integer, parameter :: input_file_n = 10, output_file_n = 11
   character(len=32) :: file_name
   integer :: file_n  ! ファイル番号

   max_n = 100  ! 最大配列数(行数)の指定

   ! 最大の配列数(行数)で割付け
   allocate(x(max_n), n(max_n), stat = is)
   if (is /= 0) stop 'cannot allocate (max_n is too large)'  ! 割り付け不可なら停止

   ! 読み取り用ファイルの確認
   open(input_file_n, file = 'ex2_speci_file/input.d', status = 'old', action = 'read', iostat = is)
   if (is /= 0) stop 'cannot open ''ex2_speci_file/input.d'''

   ! ファイルの全要素の読み取り　1行ずつ読み取る
   do i = 1, max_n
      read(input_file_n, *, iostat = iost) n(i), x(i)  ! iost → ファイルの終了条件が検出されたとき負になる。
      if (iost < 0) exit
   end do

   max_n = i - 1  ! 配列の最大要素数(行)を更新

   close(input_file_n)

   call array_modi(x)           ! 1次元配列に対して任意の四則演算を実行するarray_modiを実行

   do i = 1, max_n
      file_n = i

      ! "ex2_speci_file/outout'ファイル番号'.d"を文字列 filename に代入
      write(file_name, '("ex2_speci_file/output"i3.3".d")') file_n

      ! 書き込み用ファイル(filename)の確認
      open(output_file_n, file = file_name, action = 'write', iostat = is)
      if (is /= 0) stop 'cannot open output file'
      write(*, *) 'output_file : ',file_name
      write(output_file_n, '(""i3" "f24.12"")') n(file_n), x(file_n)     !file_n番目行の値を書き込み
      close(output_file_n)
   end do

   max_array_n = maxloc(x, 1)   ! 配列x(2列目)のfunc実行後の値が最大となる行番号を指定

   file_n = max_array_n         ! 最大値の行番号をファイル番号に代入

   ! "ex2_speci_file/outout_max'ファイル番号'.d"を文字列filenameに代入
   write(file_name, '("ex2_speci_file/output_max"i3.3".d")') file_n

   ! 書き込み用ファイルの確認
   open(output_file_n, file = file_name, action = 'write', iostat = is)
   if (is /= 0) stop 'cannot open output file'
   write(*, *) 'output_file : ',file_name
   write(output_file_n, '(""i3" "f24.12"")') n(file_n), x(file_n)       ! 最大値をもつ行のみを出力
   close(output_file_n)

end program ex2

! fortranは大文字、小文字を区別しない
! intent属性(入出力特性)は付けると最適化により、速度が向上することもある。バグも減らせる。
! read文を用いてread(A, 'I2', iostat = iost) str    → Aを'I2'として(整数2桁)としてstrに代入
! write文を用いてwrite(str,'I2') A とも書ける
