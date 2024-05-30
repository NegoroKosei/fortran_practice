module modu
   implicit none
contains
   subroutine newton(x1,i,v,fo)
      real(8):: x2,y,delta
      real(8),parameter::epsilon=1.0d-15           !相対誤差の大きさを指定
      real(8),intent(out)::x1
      integer,intent(out)::i
      integer,intent(in)::v,fo                     !verboseモード用の変数
      integer,parameter::max_i=100                 !最大反復回数max_iを指定
      
      i=0
      do
         i=i+1
         if(i>max_i) stop 'err :did not converge'  !最大反復回数まで繰り返し
         if((diff(x1))==0.0d0) stop 'err : diff==0!'
         x2=-func(x1)/diff(x1)+x1
         delta=x1-x2
         x1=x2
         if((func(x2))==0.0d0) then 
            exit
         elseif(abs(delta)<(abs(x2)*epsilon)) then  !収束判定
            exit
         endif
         if(v==1) write(fo,*)x2,i                   !verboseモード
      end do
   end subroutine newton

   function two(a,b,i,v,fo) result (c)
      real(8):: a,b,c,ya,yb,yc,delta
      real(8),parameter::epsilon=1.0d-15 !相対誤差の大きさを指定
      integer,intent(out)::i
      integer,intent(in)::v,fo           !verboseモード用の変数
      integer,parameter::max_i=100       !最大反復回数100
      
      i=0
      do
         i=i+1
         if(i>max_i) stop 'err :did not converge' 
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
         elseif (ya*yc<0.0d0) then
            b=c
         else
            a=c
         endif
         if(v==1) write(fo,*)a,b,i                   !verboseモード
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

   !verboseモードの設定　実行時に-vを入力するとすべてのstepでの結果を出力できる
   integer::argn=0,k=1,v=0
   character(len=100)::argc
   argn=command_argument_count()
   do k =1,argn
      call get_command_argument(k,argc)
      if(argc=='-v') v=1
   enddo

   write(*,'(a)',advance='no')'input Newton x0 > '
   read(*,*)x    !newton初期値xの読み込み
   write(*,'(a)',advance='no')'input Two x : from to > '
   read(*,*)xfrom,xto !two初期値の読み込み
   open (fo,file='ex5_file/output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認

   !ニュートン法
   write(fo,*)'Newton(x0=',x,')'
   if(v==1) write(fo,*)x,i1  !verboseモードあり
   call newton(x,i1,v,fo)
   write(fo,*)x,i1
   !二分法
   a=xfrom
   ya=func(a)
   b=xto
   yb=func(b)
   write(fo,*)'Two(xfrom=',a,'xto=',b,')'

   if(ya==0.0d0) then
      write(fo,*)a,i2
   elseif(yb==0.0d0) then
      write(fo,*)b,i2
   elseif(ya*yb<0) then
      if(v==1) write(fo,*)a,b,i2  !verboseモードあり
      c=two(a,b,i2,v,fo)
      write(fo,*)c,i2
   else
      write(fo,*)'Two i : No solution'
   endif
   close(fo)
end program ex5