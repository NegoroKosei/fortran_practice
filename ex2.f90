! function add_sub(x) !四則演算を指定する関数
!    implicit none
!    real(8),intent(in):: x !xが読み取るだけの引数であることを明示
!    real(8) add_sub !関数(の戻り値)がreal(8)型に指定
!    add_sub=x-0.01d0
! end function add_sub
! program ex2
!    implicit none
!    real(8) x
!    real(8) add_sub !関数をreal(8)型に指定(プログラム側でも指定が必要)
!    integer i,n,is,iost
!    open (10,file='ex2_input.d',action='read',iostat=is)
!    if(is/=0) stop 'cannot open input file'    !読み取り用ファイルの確認
!    open (11,file='ex2_output.d',action='write',iostat=is)
!    if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
!    do i=1,100
!       read(10,*,iostat=iost)n,x  !ファイルの終了条件が検出されたとき負になる。
!       if(iost < 0) exit
!       x=add_sub(x)
!       write(11,'(i3,a,f10.3)')n,' ',x
!    enddo
!    close(10)
!    close(11)
! end program ex2

! function add_sub(x) !四則演算を指定する関数(配列ver)
!    implicit none
!    real(8),intent(in):: x !xが読み取るだけの引数であることを明示
!    real(8) add_sub !関数(の戻り値)がreal(8)型に指定
!    add_sub=x-0.01d0
! end function add_sub
program ex2
   implicit none
   real(8) , allocatable::x(:)
   real(8) add_sub !関数をreal(8)型に指定(プログラム側でも指定が必要)
   integer ,allocatable ::n(:)
   integer i,is,iost,nn
   nn=100
   allocate(x(nn),n(nn),stat=is)
   if(is/=0) stop 'cannot allocate (nn is too large)' !割り付け不可なら停止
   open (10,file='ex2_input.d',action='read',iostat=is)
   if(is/=0) stop 'cannot open input file'    !読み取り用ファイルの確認
   do i=1,nn
      read(10,*,iostat=iost)n(i),x(i)  !ファイルの終了条件が検出されたとき負になる。
      if(iost < 0) exit
   enddo
   nn=i-1
   close(10)

   x(:)=x(:)+0.2 !任意の四則演算

   open (11,file='ex2_output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   do i=1,nn
      write(11,'(i3,a,f10.3)')n(i),' ',x(i)
   enddo
   close(11)
end program ex2