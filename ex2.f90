module Func
   implicit none
contains
   function add_sub(x) result (a)!四則演算を指定する関数
      real(8),intent(in):: x !xが読み取るだけの引数であることを明示
      real(8) a !intent(out)としてはダメ
      a=x-0.03d0
   end function add_sub
end module Func

!subroutineを用いた解法
! module sub
!    implicit none
! contains
   ! subroutine add_sub(x)
   !    real(8),intent(inout):: x !xが読み取るかつ変更される引数であることを明示
   !    x=x-0.05d0
   ! end subroutine add_sub
! end module sub

program ex2
   use Func
   !use sub
   implicit none
   real(8) x
   integer i,n,is,iost
   open (10,file='ex2_file/input.d',action='read',iostat=is)
   if(is/=0) stop 'cannot open input file'    !読み取り用ファイルの確認
   open (11,file='ex2_file/output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   do i=1,100
      read(10,*,iostat=iost)n,x  !ファイルの終了条件が検出されたとき負になる。
      if(iost < 0) exit
      !call add_sub(x)
      x=add_sub(x)
      write(11,'(i3,a,f10.3)')n,' ',x
   enddo
   close(10)
   close(11)
end program ex2