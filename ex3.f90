module modu
   implicit none
contains
   subroutine newton(x1,i,v,fo)
      real(8),intent(inout)::x1
      integer,intent(out)::i
      integer,intent(in)::v,fo                     !verboseモード用の変数
      real(8):: x2,y,delta
      real(8),parameter::epsilon=1.0d-15           !相対誤差の大きさを指定
      integer,parameter::max_i=100                 !最大反復回数max_iを指定
      
      i=0
      do
         i=i+1
         if(i>max_i) stop 'err :did not converge'  !最大反復回数まで繰り返し
         if((diff(x1))==0.0d0) stop 'err : diff==0!'
         x2=-func(x1)/diff(x1)+x1
         delta=x1-x2
         x1=x2
         if((func(x2))==0.0d0) then
            exit
         elseif(abs(delta)<(abs(x2)*epsilon)) then  !収束判定
            exit
         endif
         if(v==1) write(fo,*)x2,i                   !verboseモード
      end do
   end subroutine newton

   function func(x) result (y)             !f(x)
   real(8),intent(in)::x
   real(8) y
      y=x**2-1.0d0
   end function func

   function diff(x) result (y)             !f'(x)
   real(8),intent(in)::x
   real(8) y
      y=2*x
   end function diff
end module modu

program ex3
   use modu
   implicit none
   real(8) x
   integer:: i=0,is
   integer,parameter::fi=10,fo=11

   

   !verboseモードの設定　実行時に-vを入力するとすべてのstepでの結果を出力できる
   integer::argn=0,k=1,v=0
   character(len=64)::argc
   argn=command_argument_count()
   do k =1,argn
      call get_command_argument(k,argc)
      if(argc=='-v') v=1
   enddo

   write(*,'(a)',advance='no')'input x1 : '
   read(*,*)x                                  !初期値xの読み込み

   open (fo,file='ex3_file/output001.d',status='replace',action='write',iostat=is)   !ファイル名をここで変更できる
   if(is/=0) stop 'cannot open output file'    !書き込み用ファイルの確認

   if(func(x)==0) then                         !初期値xが解かどうか確認
      write(fo,*)x,i
      stop
   endif
   if(v==1) write(fo,*)x,i  !verboseモードあり

   call newton(x,i,v,fo)    !ニュートン法による計算。初期値、反復回数に加えverboseモード用のvとfoも引数とする

   write(fo,*)x,i
   close(fo)
end program ex3

