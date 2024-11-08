program loop_odd_even
   implicit none
   integer i,wa0,wa1
   wa0=0;wa1=0
   do i=1,100
      if(mod(i,2)==0) then
         wa0=wa0+i
      else if (mod(i,2)==1) then
         wa1=wa1+i
      else
         stop 'something is wrong !!'
      endif
   enddo
   write(*,*) 'wa0, wa1, wa =', wa0,    wa1, wa0+wa1    
end program loop_odd_even