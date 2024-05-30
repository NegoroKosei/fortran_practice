!a*x^2+b*x+c=0の二次方程式を解く。

program ex1
   implicit none
   real(8) a,b,c,x1,x2,d
   complex(8) ix1,ix2

   !係数a,b,cの入力
   write(*,'(a)',advance='no') 'input a, b, c :' 
   read(*,*)a,b,c
   
   !二次方程式でない方程式に対して例外の処理をする
   if(a==0.0d0) then
      if(b==0.0d0) then
         stop 'it is not equation'         !a=0かつb=0のとき方程式でない。
      endif
      x1=-c/b                              !a=0のときはb*x+c=0の一次方程式
      write(*,'("x1="f10.5"")')x1
      stop
   endif
   !以下二次方程式として解く。

   !判別式
   d=b*b-4.0d0*a*c

   if(d>0.0d0) then   !実数解
      x1=(-b+sign(sqrt(d),-b))/(2.0d0*a)  !|b|~=sqrt(d)のとき桁落ちが生まれ誤差が生じやすいので同符号のみ求める
      x2=c/(a*x1)                         !もう一つの解は、解と係数の関係より求める
      write(*,'("x1="f10.5"")')x1
      write(*,'("x2="f10.5"")')x2

   else if(d==0.0d0) then  !重解
      x1=-b/(2.0d0*a)
      write(*,'("x1="f10.5" (Heavy Solution)")')x1

   else   !虚数解
      ix1=cmplx(-b/(2.0d0*a),sqrt(-d)/(2.0d0*a))
      ix2=cmplx(-b/(2.0d0*a),sqrt(-d)/(-2.0d0*a))
      write(*,'(SP,"x1="f10.5" "F10.5" i")')real(ix1),aimag(ix1)
      write(*,'(SP,"x2="f10.5" "f10.5" i")')real(ix2),aimag(ix2)
   endif

end program ex1
