program prime_numbers
   implicit none
   integer n,j,i
   do
      write(*,'(a)',advance='no') ' input n ((if n <= 0 then stop).and.(n<=10000)) : '
      read(*,*) n
      if (n<=0) then
         write(*,*)'0以下の数が入力されたので終了します。'
         exit
      endif
      write(*,'(i10,a)',advance='no')n,' ='
      do j=1,10000
         if(n==1) then 
            exit
         endif
         if(.not.(j==1)) write(*,'(a)',advance='no')' *'
         if(modulo(n,2)==0) then
            n=n/2
            write(*,'(a)',advance='no') '    2' 
            cycle     
         endif
         do i=3,97,2                  !ループ終了後i=99となる
            if(modulo(n,i)==0) then
               n=n/i
               write(*,'(i5)',advance='no') i
               exit                   !iを99として処理している
            endif
         enddo
         if(i==99) then              !i=99の場合に対応している
            write(*,'(i10)',advance='no') n
            exit
         endif
      enddo
      write(*,*)
   enddo
end program prime_numbers