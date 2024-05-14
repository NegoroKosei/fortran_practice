module modu
   implicit none
contains
   subroutine newton(x1,i)
      real(8):: x1,x2,y,delta,epsilon=1.0d-15 !絶対誤差の大きさを指定
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
         elseif(abs(delta)<(epsilon)) then  !収束判定
            exit
         endif
      end do
   end subroutine newton

   function two(a,b,i) result (c)
      real(8):: a,b,c,ya,yb,yc,delta,epsilon=1.0d-15 !絶対誤差の大きさを指定
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
         elseif(abs(delta)<(epsilon)) then !収束判定
            exit
         elseif((a==c).or.(c==b)) then
            exit
         elseif (ya*yc<0.0d0) then
            b=c
         else
            a=c
         endif
      end do
   end function two

   function func(x) result (y) !解を求める関数
   real(8),intent(in)::x
   real(8) y
      y=x**2-1.0d0
   end function func

   function diff(x) result (y) !一次導関数
   real(8),intent(in)::x
   real(8) y
      y=2*x
   end function diff
end module modu

program ex5
   use modu
   implicit none
   real(8) x,xfrom,xto,a,b,ya,yb,c
   integer ::fo=11,i1=0,i2=0,xidx,is
   write(*,'(a)',advance='no')'input Newton x1 : '
   read(*,*)x    !newton初期値xの読み込み
   write(*,'(a)',advance='no')'input Two x1 : from to: '
   read(*,*)xfrom,xto !two初期値の読み込み
   open (fo,file='ex5_output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   !ニュートン法
   call newton(x,i1)
   write(fo,*)'Newton i :',i1
   !二分法
   a=xfrom
   ya=func(a)
   b=xto
   yb=func(b)
   if(ya==0.0d0) then
      write(fo,*)' i :',i2
   elseif(yb==0.0d0) then
      write(fo,*)'Two i :',i2
   elseif(ya*yb<0) then
      c=two(a,b,i2)
      write(fo,*)'Two i :',i2
   else
      write(fo,*)'Two i : No solution'
   endif
   close(fo)
end program ex5