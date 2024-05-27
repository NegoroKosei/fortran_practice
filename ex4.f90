module modu
   implicit none
contains
   function two(a,b,i,v,fo) result (c)
      real(8):: a,b,c,ya,yb,yc,delta
      real(8),parameter::epsilon=1.0d-15 !相対誤差の大きさを指定
      integer,intent(out)::i
      integer,intent(in)::v,fo           !verboseモード用の変数
      integer,parameter::max_i=100       !最大反復回数100
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

   function func(x) result (y)
   real(8),intent(in)::x
   real(8) y
      y=x**2-1.0d0
   end function func
end module modu

program ex4
   use modu
   implicit none
   real(8) xfrom,xto,a,b,ya,yb,c
   integer ::fo=11,i=0,xidx,is

   !verboseモードの設定　実行時に-vを入力するとすべてのstepでの結果を出力できる
   integer::argn=0,k=1,v=0
   character(len=100)::argc
   argn=command_argument_count()
   do k =1,argn
      call get_command_argument(k,argc)
      if(argc=='-v') v=1
   enddo

   write(*,'(a)',advance='no')'input x1 : from to: '
   read(*,*)xfrom,xto                          !初期値の読み込み
   open (fo,file='ex4_file/output.d',status='replace',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   a=xfrom
   ya=func(a)
   b=xto
   yb=func(b)
   if(ya==0.0d0) then
      write(fo,*)a,i
   elseif(yb==0.0d0) then
      write(fo,*)b,i
   elseif(ya*yb<0) then
      if(v==1) write(fo,*)a,b,i  !verboseモードあり
      c=two(a,b,i,v,fo)
      write(fo,*)c,i
   else
      write(fo,*)'No solution' !初期値の解が同符号で二分法を使って求められない
   endif
   close(fo)
end program ex4