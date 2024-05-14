module modu
   implicit none
contains
   function simpson(xfrom,xto,xsteps) result (sum)
      real(8),intent(in):: xfrom,xto
      real(8) sum,x,hh
      integer,intent(in):: xsteps
      integer xidx
      hh=(xto-xfrom)/xsteps
      do xidx=0,xsteps-1,2
         x=real((xfrom*(xsteps-xidx)+xto*xidx))/real(xsteps)
         sum=sum + func(x)+2.0d0*func(x+hh)
         write(*,*)xidx
      enddo
      sum=sum*2.0d0
      sum=sum-func(xfrom)+func(xto)
      sum=sum*2.0d0*(xto-xfrom)/(6.0d0*xsteps)
   end function simpson

   function func(x) result (y)
   real(8),intent(in)::x
   real(8) y
      y=sin(x)
   end function func
end module modu

program ex7
   use modu
   implicit none
   real(8) ::xfrom,xto,sum=0.0d0,x
   integer ::fo=11,is,xsteps
   write(*,'(a)',advance='no')'input x1 : from to steps : '
   read(*,*)xfrom,xto,xsteps !初期値の読み込み
   if(modulo(xsteps,2)/=0) stop 'Bad steps'
   sum=simpson(xfrom,xto,xsteps)
   open (fo,file='ex7_output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   write(fo,*)sum
   close(fo)
end program ex7