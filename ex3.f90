module modu
   implicit none
contains
   subroutine newton(x1,i)
      real(8):: x1,x2,y,delta,epsilon=1.0d-12 !相対誤差の大きさを指定
      integer,intent(out)::i
      i=0
      do
         i=i+1
         if(i>100) stop 'err :did not converge' !最大反復回数100
         if((diff(x1))==0.0d0) stop 'err : diff==0!'
         x2=-func(x1)/diff(x1)+x1
         delta=x1-x2
         x1=x2
         if((func(x2))==0.0d0) then 
            exit
         else if(abs(delta)<(abs(x2)*epsilon)) then 
            exit
         endif
      end do
   end subroutine newton

   function func(x) result (y)
   real(8),intent(in)::x
   real(8) y
      y=x**2-1
   end function func

   function diff(x) result (y)
   real(8),intent(in)::x
   real(8) y
      y=2*x
   end function diff
end module modu

program ex3
   use modu
   implicit none
   real(8) x
   integer ::fo=11,i,is
   write(*,'(a)',advance='no')'input x1 : '
   read(*,*)x    !初期値xの読み込み
   open (fo,file='ex3_output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   call newton(x,i)
   write(fo,*)x,i
   close(fo)
end program ex3

!解２つ見つける必要ある？