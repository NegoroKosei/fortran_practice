!特定の行にのみ操作を加えるバージョン
!それぞれの行番号の000.dを作る

module Add_sub
   implicit none
contains
   function func(x) result (y) !任意の四則演算をする関数
      real(8),intent(in)::x
      real(8) y

      y=x+0.3          !任意の四則演算

   end function func
   
   subroutine array_modi(x,modify_n)
      real(8),intent(inout)::x(:) !形状引継ぎ配列
      integer,intent(in)::modify_n
      x(modify_n)=func(x(modify_n))
   end subroutine array_modi
end module Add_sub

program ex2
   use Add_sub
   implicit none
   real(8) , allocatable::x(:) !配列の動的な確保
   integer ,allocatable ::n(:)
   integer i,is,iost,max_n
   integer,parameter::input_file_n=10,output_file_n=11
   character(len=32) file_name
   integer,parameter::modify_n=3 !適応するファイル番号を指定
   integer::file_n=0

   max_n=100

   allocate(x(max_n),n(max_n),stat=is)
   if(is/=0) stop 'cannot allocate (max_n is too large)' !割り付け不可なら停止
   open (input_file_n,file='ex2_speci_file/input.d',status='old',action='read',iostat=is)
   if(is/=0) stop 'cannot open ''ex2_speci_file/input.d'' '    !読み取り用ファイルの確認
   do i=1,max_n
      read(input_file_n,*,iostat=iost)n(i),x(i)  !iost → ファイルの終了条件が検出されたとき負になる。
      if(iost < 0) exit
   enddo
   max_n=i-1
   close(input_file_n)

   call Array_modi(x,modify_n) !特定の配列に対して任意の四則演算を実行するArray_modiを実行
   file_n=modify_n
   write(file_name,'("ex2_speci_file/output"i3.3".d")')file_n

   open (output_file_n,file=file_name,status='replace',action='write',iostat=is)
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
   write(output_file_n,'(""i3" "f10.5"")')n(file_n),x(file_n)
   close(output_file_n)
end program ex2