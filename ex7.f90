module modu
   implicit none
contains

   !シンプソン測を実行する関数 xfromからxtoまでxsteps分割で値を求めて実行する。
   function simpson(xfrom,xto,xsteps) result (sum)
      real(8),intent(in):: xfrom,xto
      real(8) sum,x,hh
      integer,intent(in):: xsteps
      integer xidx
      hh=(xto-xfrom)/xsteps            !分割した幅の大きさhh

      !計算する3点の内、左と真ん中のx座標の2ペアで計算する
      !f(x)を1,2,1,2,...,1,2,0で重みづけする。
      do xidx=0,xsteps-1,2
         x=real((xfrom*(xsteps-xidx)+xto*xidx))/real(xsteps)    !左のx座標を求める
         sum=sum + func(x)+2.0d0*func(x+hh)
      enddo

      !まずf(x)を 1,4,2,4,....,2,4,1 で重みづけをするように調整する。(2倍した後両端を減らす)
      !最後に3点の幅2.0*hhを掛け、6.0で割る
      sum=sum*2.0d0
      sum=sum-func(xfrom)+func(xto)
      sum=sum*2.0d0*hh/6.0d0
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
   ! write(*,'(a)',advance='no')'input x1 : from to steps : '
   ! read(*,*)xfrom,xto,xsteps !初期値の読み込み
   ! if(modulo(xsteps,2)/=0) stop 'Bad steps'
   xfrom=0.0d0
   xto=2.0d0*acos(0.0d0)
   xsteps=20000
   sum=simpson(xfrom,xto,xsteps)
   open (fo,file='ex7_output.d',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   write(fo,*)sum
   close(fo)
end program ex7