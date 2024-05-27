module add_sub
   implicit none
contains
   subroutine ab(x)
      real(8),intent(inout)::x(:) !形状引継ぎ配列
      integer i
      i=size(x,1)  !引き継いだxのsizeを求める
      x(1:i)=x(1:i)+0.2
   end subroutine ab
end module add_sub

program ex2
   use add_sub
   implicit none
   real(8) , allocatable::x(:) !動的にファイルを更新
   integer ,allocatable ::n(:)
   integer i,is,iost,nn
   nn=100
   allocate(x(nn),n(nn),stat=is)
   if(is/=0) stop 'cannot allocate (nn is too large)' !割り付け不可なら停止
   open (10,file='ex2_file/input.d',status='old',action='read',iostat=is)
   if(is/=0) stop 'cannot open ''ex2_file/input.d'' '    !読み取り用ファイルの確認
   do i=1,nn
      read(10,*,iostat=iost)n(i),x(i)  !iost → ファイルの終了条件が検出されたとき負になる。
      if(iost < 0) exit
   enddo
   nn=i-1
   close(10)

   call ab(x) !任意の四則演算

   open (11,file='ex2_file/output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   do i=1,nn
      write(11,'(i3,a,f10.3)')n(i),' ',x(i)
   enddo
   close(11)
end program ex2