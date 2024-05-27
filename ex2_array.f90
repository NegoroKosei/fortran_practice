module Add_sub
   implicit none
contains
   function func(x) result (y) !任意の四則演算をする関数
      real(8),intent(in)::x
      real(8) y

      y=x+0.3          !任意の四則演算

   end function func
   
   subroutine array_modi(x)
      real(8),intent(inout)::x(:) !形状引継ぎ配列
      integer i,j
      i=size(x,1)  !引き継いだxのsizeを求める
      do j=1,i
         x(j)=func(x(j))
      enddo
   end subroutine array_modi
end module Add_sub

program ex2
   use Add_sub
   implicit none
   real(8) , allocatable::x(:) !配列の動的な確保
   integer ,allocatable ::n(:)
   integer i,is,iost,max_n
   integer,parameter::input_file_n=10,output_file_n=11

   max_n=100
   allocate(x(max_n),n(max_n),stat=is)
   if(is/=0) stop 'cannot allocate (max_n is too large)' !割り付け不可なら停止
   open (input_file_n,file='ex2_file/input.d',status='old',action='read',iostat=is)
   if(is/=0) stop 'cannot open ''ex2_file/input.d'' '    !読み取り用ファイルの確認
   do i=1,max_n
      read(input_file_n,*,iostat=iost)n(i),x(i)  !iost → ファイルの終了条件が検出されたとき負になる。
      if(iost < 0) exit
   enddo
   max_n=i-1
   close(input_file_n)

   call Array_modi(x) !1次元配列に対して任意の四則演算を実行するArray_modiを実行

   open (output_file_n,file='ex2_file/output.d',status='replace',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   do i=1,max_n
      write(output_file_n,'(""i3" "f10.5"")')n(i),x(i)
   enddo
   close(output_file_n)
end program ex2