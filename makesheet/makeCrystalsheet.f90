module fractal_module
   implicit none
   integer :: i
   real(8),parameter :: pi = acos(-1.0d0)
   real(8), parameter :: rot90(1:2, 1:2) = reshape([0.0d0, -1.0d0, 1.0d0, 0.0d0], [2, 2])
   real(8), parameter :: rot30(1:2, 1:2) = reshape([sqrt(0.75d0), -0.5d0, 0.5d0, sqrt(0.75d0)], [2, 2])
   ! 結合長(A単位)
   real(8) ,parameter :: bond_length = 1.46d0

contains
! fractal......中心位置から六員環を描写し、次の六員環の位置を再帰的に決定する
! xy......六員環中心位置ベクトル(A単位)
! theta_n......y方向を0度とし、反時計回りに60度ずつ回転させたときの回転回数n
! n......再帰回数
   recursive subroutine fractal(xy, theta_n, n, fo, fmt, z)
    real(8),intent(in) :: xy(1:2), z
    integer,intent(in) :: fo
    character(32),intent(in) :: fmt
    integer,intent(in) :: theta_n, n
    ! 結晶の進展方向ベクトル(結合長単位)
    real(8) :: xy_v(1:2), xy2(1:2)
    integer :: theta_n2, i2
    xy_v = matmul([0.0d0, 1.0d0], reshape([cos(pi * theta_n / 3.0), -sin(pi * theta_n / 3.0), &
                                           sin(pi * theta_n / 3.0), cos(pi * theta_n / 3.0)], [2, 2]))
    if (n > 20) return
    xy_v = matmul(-xy_v, rot90)
    xy2 = xy + xy_v * bond_length
    write(fo, fmt) xy2, z
    do i2 = 1, 3
        xy_v = matmul(xy_v, rot30)
        if (modulo(n, 6) == 5)  then
            xy2 = xy + sqrt(3.0d0) * xy_v * bond_length
            theta_n2 = modulo(theta_n - 2 + i2, 6)
            call fractal(xy2, theta_n2, n + 1 , fo, fmt, z)
        else if(i2 == 2) then
            xy2 = xy + sqrt(3.0d0) * xy_v * bond_length
            theta_n2 = modulo(theta_n - 2 + i2, 6)
            call fractal(xy2, theta_n2, n + 1 , fo, fmt, z)
        end if
        xy_v = matmul(xy_v, rot30)
        xy2 = xy + xy_v * bond_length
        write(fo, fmt) xy2, z
    end do
   end subroutine fractal
end module fractal_module

program makeCrystalsheet
    use fractal_module
    implicit none
    ! 出力情報
    integer :: fo = 10
    character(32), parameter :: filename = 'Crystalsheet.dat'
    character(32), parameter :: fmt = '(3e24.16)'

    ! xy座標(A単位)
    real(8) :: xy(1:2), xy2(1:2), xy_v(1:2)
    ! z座標(A単位)
    real(8) :: z
    ! ループ用変数
    ! integer :: i, j
    ! ファイルオープンのステータス
    integer :: is

    ! z座標の設定
    z = 0.0d0
    ! 出力ファイルのオープン
    open(fo, file=filename, status='replace', action='write', iostat=is)
    if (is /= 0) then
        print*, 'Error: file open'
        stop
    end if

    xy = (/0.0d0, 0.0d0/)
    do i = 0, 5
        xy_v = matmul([1.0d0, 0.0d0], reshape([cos(pi * i / 3.0), -sin(pi * i / 3.0), &
                                           sin(pi * i / 3.0), cos(pi * i / 3.0)], [2, 2]))
        xy2 = xy + bond_length * xy_v
        write(fo, fmt) xy2, z
    end do
    xy = xy + bond_length * sqrt(3.0d0) * [0.0d0, 1.0d0]
    call fractal(xy, 0, 2, fo, fmt, z)
    xy = bond_length * sqrt(3.0d0) * [0.0d0, -1.0d0]
    call fractal(xy, 3, 2, fo, fmt, z)

    close(fo)

end program makeCrystalsheet