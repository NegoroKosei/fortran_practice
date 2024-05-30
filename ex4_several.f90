module modu
   implicit none
contains
   function two(a,b,i) result (c)
      real(8):: a,b,c,ya,yb,yc,delta
      real(8),parameter::epsilon=1.0d-15           !相対誤差の大きさの定義
      integer,intent(out)::i
      integer,parameter::max_i=100                 !最大反復回数の定義
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
      end do
   end function two

   function func(x) result (y)                       !f(x)の定義
   real(8),intent(in)::x
   real(8) y
      y=x**2-1.0d0                                   !この行を変更することで様々な関数に対応できる
   end function func
end module modu


program ex4
   use modu
   implicit none
   real(8) xfrom,xto,a,b,ya,yb,c
   integer ::fo=11,i=0,soln_n=0,xidx,is,xsteps
   character(len=32)filename
   write(*,'(a)',advance='no')'xfrom xto xsteps : '

   !初期値の読み込み
   read(*,*)xfrom,xto,xsteps                  
   a=xfrom
   ya=func(a)

   !初期解のチェック
   if(ya==0.0d0) then
      soln_n=soln_n+1
      write(filename,'("ex4_several_file/output"i3.3".d")')soln_n
      open (fo,file=filename,status='replace',action='write',iostat=is)
      if(is/=0) stop 'cannot open output file'      
      write(fo,*)a,i
      close(fo)
   endif
   do xidx=1,xsteps

      !分割数でxfromからxtoまでを分割し、左から0,1...と数えてxid番目をbとする
      b=real((xfrom*(xsteps-xidx)+xto*xidx))/real(xsteps) 
      yb=func(b)

      !解の存在を確認
      if(yb==0.0d0) then
         soln_n=soln_n+1
         write(filename,'("ex4_several_file/output"i3.3".d")')soln_n
         open (fo,file=filename,status='replace',action='write',iostat=is)
         if(is/=0) stop 'cannot open output file'              
         write(fo,*)b,i
         close(fo)

      elseif(ya*yb<0) then
         soln_n=soln_n+1
         write(filename,'("ex4_several_file/output"i3.3".d")')soln_n
         open (fo,file=filename,status='replace',action='write',iostat=is)
         if(is/=0) stop 'cannot open output file'          

         !2分法を実行する
         c=two(a,b,i)
         write(fo,*)c,i
         close(fo)
      endif

      !次の分割に移行するための準備
      a=b
      ya=yb
      i=0
   enddo
end program ex4