! 平行移動 →　回転

program makeParallelogramsheet
    implicit none
    
    ! 出力情報
    integer :: fo = 10
    character(32), parameter :: filename = 'Parallelogramsheet.dat'
    character(32), parameter :: fmt = '(3e24.16)'

    ! xy座標(A単位)
    real(8) :: xy(1:2)
    ! z座標(A単位)
    real(8) :: z
    ! 結合長(A単位)
    real(8) :: bond_length = 1.46d0
    ! 基本並進ベクトル(結合長単位)
    real(8),parameter :: a1(1:2) = [3.0d0, 0.0d0], a2(1:2) = [-1.5d0, sqrt(6.75d0)]
    ! ユニットセルの原子個数
    integer, parameter :: sheet_num = 6

    ! ユニットセルの原子位置(結合長単位), m(座標, 原子番号)
!###################六員環中心が原点の場合###################
    ! real(8), parameter :: m( 1:2, 1:sheet_num ) = reshape( [  -1.0d0, sqrt(3.0d0),    &
    !                                                           -0.5d0, sqrt(0.75d0),   &
    !                                                           0.5d0, sqrt(0.75d0),    &
    !                                                           1.0d0, 0.0d0,           &
    !                                                           1.0d0, sqrt(3.0d0),     &
    !                                                           2.0d0, 0.0d0 ],         &
    !                                                           [ 2, 6 ] ) 
!###################原子が原点の場合########################
    real(8), parameter :: m( 1:2, 1:sheet_num ) = reshape( [  -0.5d0, sqrt(0.75d0),   &
                                                              0.0d0, 0.0d0,           &
                                                              0.0d0, sqrt(3.0d0),     &
                                                              1.0d0, 0.0d0,           &
                                                              1.0d0, sqrt(3.0d0),     &
                                                              1.5d0, sqrt(0.75d0) ],  &
                                                              [ 2, 6 ] ) 

!##########################################################
    ! ループ用変数
    integer :: i, j, k
    ! ファイルオープンのステータス
    integer :: is
    ! 拡大倍率の設定
    integer, parameter :: scale = 2
    ! シートの大きさ(ユニットセル単位),のちにscale倍する
    integer :: n1, n2
    ! 平行移動ベクトル(ユニットセル単位), のちにscale倍する
    real(8) :: trans(1:2)
    ! 回転角(ラジアン)
    real(8) :: theta
    ! 回転行列
    real(8) :: rot(1:2, 1:2)

    ! シートの大きさの設定
    n1 = 5 * scale
    n2 = 5 * scale
    ! 平行移動ベクトルの設定
    trans = [-3.5d0, -3.5d0] * scale
    ! 回転角の設定
    theta = acos(0.5d0)
    ! 回転行列の設定
    rot = reshape([cos(theta), -sin(theta), sin(theta), cos(theta)], [2, 2])
    ! z座標の設定
    z = 0.0d0
    ! 出力ファイルのオープン
    open(unit=fo, file=filename, status='replace', action='write', iostat=is)
    if (is /= 0) then
        print*, 'Error: file open'
        stop
    end if

    ! 長方形のシートを作る
    do j = 0, n1 - 1
        do i = 0, n2 - 1
            do k = 1, sheet_num
                xy = matmul(bond_length * ((i + trans(1)) * a1 + (j + trans(2)) * a2 + m(:,k)), rot)
                write(fo, fmt) xy, z
            end do
        end do
    end do
    close(fo)

end program makeParallelogramsheet