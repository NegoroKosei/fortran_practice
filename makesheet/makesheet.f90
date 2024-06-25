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

program makesheet
    implicit none
    ! 出力用情報
    integer, parameter :: fo = 10
    character(32), parameter :: fname = "graphene.dat"
    character(32), parameter :: fmt = '(3e24.16)'
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
    ! n3 は端の原子を除外するための数
    real(8) :: n3, n4
    ! 拡大倍率
    real(8) :: scale = 3d0
    ! シートサイズの指定
    n1 = 8 * scale
    n2 = 8 * scale 
    !　除外するx方向並進ベクトルの数
    n3 = 4d0 * scale
    !　除外するy方向並進ベクトルの数
    n4 = 4d0 * scale
    ! write(*, *) "Input n1 and n2."
    ! read *, n1, n2

    ! グラフェンシートの各原子位置をファイルに出力
    open(fo, file=fname, status="replace")
    !並進ベクトルを平行移動、長方形は負の方向にも拡張
    do i2 = -n2 + 1, n2 - 1
        do i1 = -n2 +1, n1 - 1

!#############################################################################!

            ! x座標の条件で除外

            ! xy = bond_length * (i1*a1 + i2*a2 + m1)
            ! if (xy(1) > n3*a1(1) * bond_length .and. xy(1) < (n2*2 - n3)*a1(1)*bond_length) then
            !     write(fo, fmt) xy, z
            ! end if
            ! xy = bond_length * (i1*a1 + i2*a2 + m2)

            ! ! 端の原子を除外,x座標がを並進ベクトルの数分だけ端から離れている場合
            ! if (xy(1) > n3*a1(1) * bond_length .and. xy(1) < (n2*2 - n3)*a1(1) * bond_length) then
            !     write(fo, fmt) xy, z
            ! end if

!#############################################################################!

            ! 長方形ver
            ! n1とn2を負の方まで拡張,a1がy方向負であることに注意
            ! y方向は並進ベクトル上に原子が並ぶため、並進ベクトルの0.5倍を加える(n4 + 0.5 する)
            xy = bond_length * (i1*a1 + i2*a2 + m1)
            if (xy(1) > n3*a2(1) * bond_length .and. xy(1) < (n2*2 - n3)*a2(1)*bond_length) then
                if (xy(2) > (-n2 + n4 + 0.5)*a2(2) * bond_length .and. xy(2) < (n2 - n4 - 0.5)*a2(2)*bond_length) then
                    write(fo, fmt) xy, z
                end if
            end if
            xy = bond_length * (i1*a1 + i2*a2 + m2)
            if (xy(1) > n3*a2(1) * bond_length .and. xy(1) < (n2*2 - n3)*a2(1) * bond_length) then
                if (xy(2) > (-n2 + n4 + 0.5)*a2(2) * bond_length .and. xy(2) < (n2 - n4  -0.5)*a2(2) * bond_length) then
                    write(fo, fmt) xy, z
                end if
            end if

!#############################################################################!

            ! 並進ベクトルの数の条件で除外(左右が非対称なので難しい)
            ! if (i1 + i2 > (n3 - 2) .and. i1 + i2 < 2*n1 - n3 ) then
            !     if (i1 + i2 /= (n3 - 1) ) then
            !         xy = bond_length * (i1*a1 + i2*a2 + m1)
            !         write(fo, fmt) xy, z
            !     end if
            !     if (i1 + i2 /= 2*n1 - (n3 + 1)) then
            !         xy = bond_length * (i1*a1 + i2*a2 + m2)
            !         write(fo, fmt) xy, z
            !     end if
            ! end if

!#############################################################################!

        end do
    end do
    close(fo)

end program makesheet