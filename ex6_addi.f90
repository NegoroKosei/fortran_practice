module modu
   implicit none
contains
   subroutine newton(x1,i)
      real(8):: x1,x2,y,delta,epsilon=1.0d-15 !相対誤差の大きさを指定
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
         elseif(abs(delta)<(abs(x2)*epsilon)) then 
            exit
         endif
      end do
   end subroutine newton

   function func(x) result (y)
      real(8),intent(in)::x
      real(8) y
      y=x**3-x
   end function func

   function diff(x) result (y)
   real(8),intent(in)::x
   real(8) y
      y=3.0d0*x**2-1
   end function diff
end module modu

program ex6
   use modu
   implicit none
   real(8) xfrom,xto,x,x0
   integer ::fo=11,xidx,is,xsteps,i
   write(*,'(a)',advance='no')'xfrom xto xsteps : '

   !初期値の読み込み
   read(*,*)xfrom,xto,xsteps                  

   open (fo,file='ex6_addi_file/output.d',status='replace',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認

   do xidx=0,xsteps
      x=real((xfrom*(xsteps-xidx)+xto*xidx))/real(xsteps) 
      x0=x
      call newton(x,i)
      write(fo,*)x0,x,i
   enddo
   close(fo)
end program ex6
