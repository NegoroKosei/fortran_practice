module modu
   implicit none
contains
   subroutine newton(x1, i, v, fo, fmt)
      real(8), intent(inout) :: x1              ! 前のstepの解
      integer, intent(out) :: i                 ! 反復回数
      integer, intent(in) :: v, fo              ! verboseモード用の変数
      character(len=*), intent(in) :: fmt

      real(8) :: x2, y, delta
      real(8), parameter :: epsilon = 1.0d-15   ! 絶対誤差の大きさを指定
      integer, parameter :: max_i = 100         ! 最大反復回数max_iを指定

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
         elseif (abs(delta) < epsilon) then
               exit
         endif

         if (v == 1) write(fo, fmt) x2, i          ! verboseモード、解と反復回数を出力
      end do
   end subroutine newton

   function func(x) result(y)
      real(8), intent(in) :: x
      real(8) :: y
      y = x**3 - x
   end function func

   function diff(x) result(y)
      real(8), intent(in) :: x
      real(8) :: y
      y = 3.0d0 * x**2 - 1
   end function diff
end module modu

program ex6_addi
   use modu
   implicit none
   real(8) :: xfrom, xto, x, x0
   integer :: xidx, is, xsteps, i
   integer, parameter :: fo = 11
   character(len=32), parameter :: fname = "ex6_addi_file/output001.dat"
   character(len=32), parameter :: fmt = '(2d24.16, i3)'
   integer :: argn = 0, k = 1, v = 0
   character(len=64) :: argc

   argn = command_argument_count()
   do k = 1, argn
      call get_command_argument(k, argc)
      if (argc == '-v') v = 1                  ! -vの入力時、v = 1とする
   end do

   ! 改行無し出力
   write(*, '(a)', advance='no') 'xfrom xto xsteps : '

   ! 初期値の読み込み、xfrom以上xto以下の範囲をxstep分割する
   read(*, *) xfrom, xto, xsteps

   ! 書き込みファイルの準備
   open(fo, file=fname, status='replace', action='write', iostat=is)
   if (is /= 0) stop 'cannot open output file'    ! 書き込み用ファイルの確認

   do xidx = 0, xsteps
      ! 内分点を求める式、小さい値からxid番目の値をxに代入
      x = real((xfrom * (xsteps - xidx) + xto * xidx)) / real(xsteps)
      ! 初期値をx0に代入
      x0 = x

      if (func(x) == 0) then       ! 初期値xが解かどうか確認
         write(fo, fmt) x0, x, i
         cycle                     ! 次のループへ
      endif
      if (v == 1) write(fo, fmt) x, i  ! verboseモードあり

      ! ニュートン法の実行
      call newton(x, i, v, fo, fmt)

      ! 初期値、解、回数を出力
      write(fo, fmt) x0, x, i
   end do
   close(fo)
end program ex6_addi

