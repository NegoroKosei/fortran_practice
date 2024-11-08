program ray
   implicit none
   integer i
   integer, parameter ::n=2 !パラメータ(定数)で指定(parameter属性をつける)
   real(8) u(n),v(n),dotp
   real(8), allocatable :: s(:),t(:) !allocatableを用いる。（後からメモリ確保）
   integer ::m=2
   dotp =0.0d0
   u(:)=2.0d0
   v(:)=3.0d0
   do i=1,n
      dotp =dotp +u(i)*v(i)
   enddo
   write(*,*) 'dot product = ',dotp
   !allocateを用いて可変長配列をつくる
   dotp=0.0d0
   allocate(s(n),t(n))
   s(:)=2.0d0
   t(:)=3.0d0
   do i=1,m
      dotp =dotp +s(i)*t(i)
   enddo
   deallocate(s,t) !解放を忘れずに
   write(*,*) 'dot product = ',dotp
end program ray

module sub
   implicit none
contains
   subroutine add_A(a(:))
      real(8),intent(inout)::a


!配列がいっぱいになったときにメモリを10ずつ増やしていくサブルーチンをつくる


!引数はポインタ渡し（？）同じ値を共有している。
!save属性をつけると前回の値をそのまま用いる
!変更したくない値はintent(in),必ず変更する値はintent(out)
!function a(引数,引数) result (返す変数)
          !配列の添え字は一番左を内側のループにする。

!配列の受け渡し形状明示仮配列a(n,m)(仮引数で形状を明示して宣言)
!配列の受け渡し形状引継ぎ配列a(:,:)(明示せずとも勝手に形状が決定)(モジュールやインターフェイスが必須)、見割付けの配列も引き継げる(allocatable属性必要)
!△大きさ引継ぎ配列a(*)またはa(:,:,*)　最後の次元が実引数配列の大きさに合わせて変わる。ただしa(:)のような部分配列は使えない。
!部分配列を用いて配列の引き渡しができる