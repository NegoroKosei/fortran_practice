module modu
   implicit none
contains
   function two(a,b,i) result (c)
      real(8):: a,b,c,ya,yb,yc,delta,epsilon=1.0d-15 !相対誤差の大きさを指定
      integer,intent(out)::i
      do
         i=i+1
         if(i>100) stop 'err :did not converge' !最大反復回数100
         c=(a+b)/2.0d0
         ya=func(a)
         yb=func(b)
         yc=func(c)
         delta=a-b
         if(yc==0.0d0) then 
            exit
         elseif(abs(delta)<(abs(c)*epsilon)) then 
            exit
         elseif((a==c).or.(c==b)) then
            exit
         elseif (ya*yb<0.0d0) then
            b=c
         else
            a=c
         endif
      end do
   end function two

   function func(x) result (y)
   real(8),intent(in)::x
   real(8) y
      y=x**2-1
   end function func
end module modu

program ex4
   use modu
   implicit none
   real(8) xfrom,xto,a,b,ya,yb,c
   integer ::fo=11,i=0,xidx,is,xsteps
   write(*,'(a)',advance='no')'input x1 : from to steps : '
   read(*,*)xfrom,xto,xsteps !初期値の読み込み
   open (fo,file='ex4_output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   a=xfrom
   ya=func(a)
   if(ya==0.0d0) write(fo,*)a,i
   do xidx=1,xsteps
      b=real((xfrom*(xsteps-xidx)+xto*xidx))/real(xsteps)
      yb=func(b)
      if(yb==0.0d0) then
         write(fo,*)b,i
      elseif(ya*yb<0) then
         c=two(a,b,i)
         write(fo,*)c,i
      endif
      a=b
      ya=yb
      i=0
   enddo
   close(fo)
end program ex4

!解２つ見つける必要ある？