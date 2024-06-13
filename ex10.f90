!#############################################################################!
! 02_io/ex10.f90
! 2024/06/12
!=============================================================================!
! グラフェンシートの出力例
! gnuplot で出力を確認する際は 'fmt' を '(3e24.16)' に書き換えること.
!
! [コンパイル例]
! gfortran ex10.f90 -o ex10 -O
!
! [gnuplot コマンド]
! set size ratio -1
! plot "graphene.dat"
!#############################################################################!

program ex10
    implicit none
    ! 出力用情報
    integer, parameter :: fo = 10
    character(32), parameter :: fname = "graphene.dat"
    character(32), parameter :: fmt = '(3d24.16)'
    ! xy 座標 (A単位)
    real(8) :: xy(1:2)
    ! z 座標 (A単位)
    real(8), parameter :: z = 0d0
    ! 結合長 (A単位)
    real(8), parameter :: bond_length = 1.46d0
    ! 基本並進ベクトル (結合長単位)
    real(8), parameter :: a1(1:2) = [1.5d0, -sqrt(0.75d0)]
    real(8), parameter :: a2(1:2) = [1.5d0,  sqrt(0.75d0)]
    ! ユニットセル内の原子位置 (結合長単位)
    real(8), parameter :: m1(1:2) = [1d0, 0d0]
    real(8), parameter :: m2(1:2) = [2d0, 0d0]
    ! 整数の組
    integer :: i1, i2
    ! シートサイズ (整数の組)
    integer :: n1, n2

    ! シートサイズの指定
    n1 = 8
    n2 = 8
    ! write(*, *) "Input n1 and n2."
    ! read *, n1, n2

    ! グラフェンシートの各原子位置をファイルに出力
    open(fo, file=fname, status="replace")
    do i2 = 0, n2 - 1
        do i1 = 0, n1 - 1
            xy = bond_length * (i1*a1 + i2*a2 + m1)
            write(fo, fmt) xy, z
            xy = bond_length * (i1*a1 + i2*a2 + m2)
            write(fo, fmt) xy, z
        end do
    end do
    close(fo)

end program ex10
