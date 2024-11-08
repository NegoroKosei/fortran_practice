program file_io
   implicit none
   real(8) d,x,y,z
   integer :: n,i,j,fi=10,fo=11 !初期設定するには::をつける
   open(fi,file='file_io_input.d')
   open(fo,file='file_io_output.d')
   read(fi,*)n
   close(fi)
   if(n<3)stop 'stop, n < 3'
   d=10.0d0 / dble(n-1)
   do j=1, n
      y=-5.0d0+dble(j-1)*d
      do i=1, n
         x=-5.0d0 + dble(i-1)*d
         z=sin(x)*cos(y)
         write(fo,'(3e12.4)')x,y,z
      enddo
      write(fo,*) ' ' !gnuplotの際に直線がひかれないように
   enddo
   close(fo)
end program file_io