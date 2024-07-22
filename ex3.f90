! ニュートン法による解の計算
! 実行時にオプション'-v'を入力することでそれぞれのステップごとの出力も得られる
! 解の付近の様子がある程度わかっているときに使う
! 微分が計算できなかったり、極値があるせいで値が飛んでしまったりするため。

module modu
   implicit none
contains
   subroutine newton(x1, i, v, fo, fmt)
      real(8), intent(inout) :: x1              ! 前のstepの解
      integer, intent(out) :: i                 ! 反復回数
      ! verboseモード用の変数
      integer, intent(in) :: v, fo
      character(len=*), intent(in) :: fmt

      real(8) :: x2, y, delta
      real(8), parameter :: epsilon = 1.0d-15   ! 絶対誤差の大きさを指定
      integer, parameter :: max_i = 1000        ! 最大反復回数max_iを指定

      i = 0
      do
         i = i + 1                                    ! 反復回数の増加
         if (i > max_i) stop 'err: did not converge'  ! 最大反復回数まで繰り返し

         ! 微分が0ならば解x2 が求まらないため終了
         if (diff(x1) == 0.0d0) stop 'err: diff==0!'

         ! ニュートン法により新たな解x2 を求める
         x2 = -func(x1) / diff(x1) + x1

         ! 前の解x1 と新たな解x2 の差deltaを求める
         delta = x1 - x2

         ! 前のstepの解x1の更新
         x1 = x2

         ! 代入して0なら解とする
         if (func(x2) == 0.0d0) then
            exit

         ! x2 - x1 の絶対値が誤差範囲に収まっているなら解とする
         elseif (abs(delta) <  epsilon) then
            exit
         endif
         if (v == 1) write(fo, fmt) x2, i          ! verboseモード、解と反復回数を出力
      end do
   end subroutine newton

   function func(x) result(y)       ! f(x)
      real(8), intent(in) :: x
      real(8) :: y
      y = x ** 4 - 1.0d0*x ** 2
   end function func

   function diff(x) result(y)       ! f'(x)
      real(8), intent(in) :: x
      real(8) :: y
      y = 4.0d0 * x ** 3 - 2.0d0 * x
   end function diff
end module modu

program ex3
   use modu
   implicit none
   real(8) :: x
   integer :: i = 0, is
   ! ファイル出力用
   integer, parameter :: fo = 11
   character(len=32), parameter :: fname = "ex3_file/output001.dat"
   character(len=32), parameter :: fmt = '(d24.16, i3)'

   ! verboseモードの設定 実行時に-vを入力するとすべてのstepでの結果を出力できる
   integer :: argn = 0, k = 1, v = 0
   character(len=64) :: argc
   argn = command_argument_count()
   do k = 1, argn
      call get_command_argument(k, argc)
      if (argc == '-v') v = 1                  ! -vの入力時、v = 1とする
   end do

   ! 初期値xの読み込み
   write(*, '(a)', advance='no') 'input x1 : '
   read(*, *) x

   ! 書き込みファイルの準備
   open(fo, file=fname, status='replace', action='write', iostat=is)
   if (is /= 0) stop 'cannot open output file'

   if (func(x) == 0) then       ! 初期値xが解かどうか確認
      write(fo, *) x, i
      stop
   endif
   if (v == 1) write(fo, fmt) x, i  ! verboseモードあり

   ! ニュートン法による計算。初期値、反復回数に加えverboseモード用のvとfoも引数とする
   call newton(x, i, v, fo, fmt)
   write(fo, fmt) x, i
   close(fo)
end program ex3









! 文字列型　character
! charactor(len=*)のように指定すると関数内で形状引き継ぎ配列のように扱える
