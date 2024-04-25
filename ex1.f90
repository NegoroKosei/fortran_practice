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
      x1=c/b
      write(*,'(a,f10.5)')'x1 = ',x1
      stop
   endif
   o=b*b-4.0d0*a*c
   if(o>0.0d0) then   !実数解
      x1=(-b+sign(sqrt(o),-b))/(2.0d0*a)  !|b|~=sqrt(o)のとき桁落ちが生まれ誤差が生じやすい
      x2=c/(a*x1)                         !解と係数の関係より求める
      write(*,'(a,f10.5)')'x1 = ',x1
      write(*,'(a,f10.5)')'x2 = ',x2
      stop
   else if(o==0.0d0) then  !重解
      x1=-b/(2.0d0*a)
      write(*,'(a,f10.5,a)')'x1 = ',x1,'(Heavy Solution)'
      stop
   else   !虚数解
      ix1=cmplx(-b/(2.0d0*a),sqrt(-o)/(2.0d0*a),kind=8)
      ix2=cmplx(-b/(2.0d0*a),sqrt(-o)/(-2.0d0*a),kind=8)
      write(*,'(a,2e11.3)')'x1 = ',ix1  !2個の値を処理する(虚数だと値が2つになる)
      write(*,'(a,2e11.3)')'x2 = ',ix2 
      stop
   endif
end program ex1
