! y軸がTubeの軸方向
program makeTube
    implicit none
    
    ! 出力情報
    integer :: fo = 10
    character(32), parameter :: filename = 'Tube(7,7).dat'
    character(32), parameter :: fmt = '(3e24.16)'

    ! xy座標(A単位)
    real(8) :: xy(1:2)
    ! z座標(A単位)
    real(8) :: z
    ! 結合長(A単位)
    real(8) :: bond_length = 1.46d0
    ! 基本並進ベクトル(結合長単位)
    real(8),parameter :: a1(1:2) = [3.0d0, 0.0d0], a2(1:2) = [0.0d0, sqrt(3.0d0)]
    ! ユニットセルの原子位置(結合長単位)
!###################六員環中心が原点の場合###################
    real(8),parameter :: m1(1:2) = [0.5d0, sqrt(0.75d0)]
    real(8),parameter :: m2(1:2) = [1.0d0, 0.0d0]
    real(8),parameter :: m3(1:2) = [2.0d0, 0.0d0]
    real(8),parameter :: m4(1:2) = [2.5d0, sqrt(0.75d0)]

!###################原子が原点の場合########################
    ! real(8),parameter :: m1(1:2) = [0.0d0, 0.0d0]
    ! real(8),parameter :: m2(1:2) = [1.0d0, 0.0d0]
    ! real(8),parameter :: m3(1:2) = [1.5d0, sqrt(0.75d0)]
    ! real(8),parameter :: m4(1:2) = [2.5d0, sqrt(0.75d0)]

!##########################################################
    ! Tubeの半径(A単位), 円周(A単位), 位置角度(ラジアン)
    real(8) :: r, l, ltheta
    ! ループ用変数
    integer :: i, j
    ! ファイルオープンのステータス
    integer :: is
    ! 拡大倍率の設定
    integer, parameter :: scale = 1
    ! シートの大きさ(ユニットセル単位),のちにscale倍する
    integer :: n1, n2
    ! シートの大きさの設定
    n1 = 7 * scale
    n2 = 10 * scale
    ! Tubeの円周と半径の設定
    l = bond_length * a1(1) * n1
    r = l / (2.0d0 * acos(-1.0d0))
    
    ! 出力ファイルのオープン
    open(unit=fo, file=filename, status='replace', action='write', iostat=is)
    if (is /= 0) then
        print*, 'Error: file open'
        stop
    end if

    ! 長方形のシートを作る
    do j = 0, n2 - 1
        do i = 0, n1 - 1
            xy = bond_length * (i * a1 + j * a2 + m1)
            ltheta = xy(1) / r
            write(fo, fmt) r * cos(ltheta), xy(2), r * sin(ltheta)

            xy = bond_length * (i * a1 + j * a2 + m2)
            ltheta = xy(1) / r 
            write(fo, fmt) r * cos(ltheta), xy(2), r * sin(ltheta)

            xy = bond_length * (i * a1 + j * a2 + m3)
            ltheta = xy(1) / r
            write(fo, fmt) r * cos(ltheta), xy(2), r * sin(ltheta)

            xy = bond_length * (i * a1 + j * a2 + m4)
            ltheta = xy(1) / r
            write(fo, fmt) r * cos(ltheta), xy(2), r * sin(ltheta)

        end do
    end do
    close(fo)

end program makeTube