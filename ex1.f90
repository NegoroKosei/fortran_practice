program ex1
   implicit none
   real(8) a,b,c,x1,x2,o
   complex(8) ix1,ix2
   write(*,'(a)',advance='no') 'input a, b, c :'
   read(*,*)a,b,c
   if(a==0.0d0) then
      if(b==0.0d0) then
         write(*,*)'it is not equation'
         stop
      endif
      x1=-c/b
      write(*,'("x1="f10.5"")')x1
      stop
   endif
   
   o=b*b-4.0d0*a*c !判別式

   if(o>0.0d0) then   !実数解
      x1=(-b+sign(sqrt(o),-b))/(2.0d0*a)  !|b|~=sqrt(o)のとき桁落ちが生まれ誤差が生じやすいので同符号のみ求める
      x2=c/(a*x1)                         !解と係数の関係より求める
      write(*,'("x1="f10.5"")')x1
      write(*,'("x2="f10.5"")')x2

   else if(o==0.0d0) then  !重解
      x1=-b/(2.0d0*a)
      write(*,'("x1="f10.5" (Heavy Solution)")')x1

   else   !虚数解
      ix1=cmplx(-b/(2.0d0*a),sqrt(-o)/(2.0d0*a))
      ix2=cmplx(-b/(2.0d0*a),sqrt(-o)/(-2.0d0*a))
      write(*,'(SP,"x1="f10.5" "F10.5" i")')real(ix1),aimag(ix1)
      write(*,'(SP,"x2="f10.5" "f10.5" i")')real(ix2),aimag(ix2)
   endif

end program ex1
