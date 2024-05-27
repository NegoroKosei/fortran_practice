!特定の範囲の行にのみ操作を加え、その範囲の行のみを別々のファイルに出力するバージョン
!それぞれの行番号の000.dを作る

module Add_sub
   implicit none
contains
   function func(x) result (y)     !任意の四則演算をする関数
      real(8),intent(in)::x
      real(8) y

      y=x+99.0d0                   !任意の四則演算

   end function func
   
   subroutine array_modi(x)
      real(8),intent(inout)::x(:)  !形状引継ぎ配列
      integer i,j
      i=size(x,1)                  !引き継いだxのsizeを求める
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
   character(len=32) file_name
   integer modify_nfrom,modify_nto !適応するファイル番号の始めと終わり
   integer file_n      !ファイル番号

   max_n=100           !最大配列数(行数)の指定

   allocate(x(max_n),n(max_n),stat=is)                    !最大の配列数(行数)で割付け
   if(is/=0) stop 'cannot allocate (max_n is too large)'  !割り付け不可なら停止

   write(*,'("modify_row_number, from to : ")',advance='no')
   read(*,*)modify_nfrom,modify_nto
   open (input_file_n,file='ex2_speci_file/input.d',status='old',action='read',iostat=is)
   if(is/=0) stop 'cannot open ''ex2_speci_file/input.d'' '    !読み取り用ファイルの確認
   do i=1,max_n
      read(input_file_n,*,iostat=iost)n(i),x(i)   !iost → ファイルの終了条件が検出されたとき負になる。
      if(iost < 0) exit
   enddo
   max_n=i-1                                      !最大行数の変更
   close(input_file_n)

   call Array_modi(x(modify_nfrom:modify_nto))        !特定の範囲の行に対して任意の四則演算を実行するArray_modiを実行

   do file_n=modify_nfrom,modify_nto
      write(file_name,'("ex2_speci_file/output"i3.3".d")')file_n
      open (output_file_n,file=file_name,status='replace',action='write',iostat=is)
      if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認
      write(output_file_n,'(""i3" "f10.5"")')n(file_n),x(file_n)
      close(output_file_n)
   enddo
end program ex2