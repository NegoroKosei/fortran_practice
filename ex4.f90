! 2分法
module modu
    implicit none
contains
    function two(a, b, i, v, fo, fmt) result (c)
        real(8), intent(inout) :: a, b
        integer, intent(out) :: i             ! 反復回数
        ! verboseモード用の変数
        character(len=*), intent(in) :: fmt
        integer, intent(in) :: v, fo
        real(8) :: c, ya, yb, yc, delta
        real(8), parameter :: epsilon = 1.0d-15 ! 絶対誤差の大きさを指定
        integer, parameter :: max_i = 1000      ! 最大反復回数1000

        ! 誤差を比べる際にニュートン法と比較しやすいように1回目の中点での計算を0回目とする
        i = -1
        do
            i = i + 1             ! 反復回数の増加
            if (i > max_i) stop 'err :did not converge' ! 最大反復回数max_iを越えると発散とする

            ! 中点を求める
            c = (a + b) / 2.0d0
            ! 関数の値を求める
            ya = func(a)
            yb = func(b)
            yc = func(c)
            delta = b - a
            ! 代入して0なら解とする
            if (yc == 0.0d0) then
                exit
            ! a-bの絶対値が誤差範囲に収まっているなら解とする
            elseif (abs(delta) < epsilon) then
                exit
            ! yaとycの符号が異なるならば解はa, cの間にある
            elseif (ya * yc < 0.0d0) then
                b = c
            ! ybとycの符号が同じならば解はc, bの間にある
            else
                a = c
            endif
            if (v == 1) write(fo, fmt) c, i                   ! verboseモード
        end do
    end function two

    function func(x) result (y)      ! f(x)
        real(8), intent(in) :: x
        real(8) :: y
        y = x**2 - 1.0d0
    end function func
end module modu

program ex4
    use modu
    implicit none
    real(8) :: xfrom, xto, a, b, ya, yb, c
    integer :: xidx, is, i = 0
    ! ファイル出力用
    integer, parameter :: fo = 11
    character(len=32) :: fname = "ex4_file/output001.dat"        ! 書き込みファイル名
    character(len=32), parameter :: fmt = '(d24.16, i3)'

    ! verboseモードの設定 実行時に-vを入力するとすべてのstepでの結果を出力できる
    integer :: argn = 0, k = 1, v = 0
    character(len=100) :: argc
    argn = command_argument_count()
    do k = 1, argn
        call get_command_argument(k, argc)
        if (argc == '-v') v = 1                     ! -vの入力時、v = 1とする
    end do

    write(*, '(a)', advance='no') 'input xfrom xto : '
    read(*, *) xfrom, xto                          ! 初期値の読み込み

    ! 書き込みファイルの準備
    open(fo, file=fname, status='replace', action='write', iostat=is)
    if (is /= 0) stop 'cannot open output file'    ! 書き込み用ファイルの確認

    a = xfrom
    ya = func(a)
    b = xto
    yb = func(b)

    if (ya == 0.0d0) then   ! 初期値aが解かどうか確認
        write(fo, fmt) a, i
    elseif (yb == 0.0d0) then ! 初期値bが解かどうか確認
        write(fo, fmt) b, i
    ! ya とyb の符号が異なるならば解はa, bの間にある
    elseif (ya * yb < 0) then
        c = two(a, b, i, v, fo, fmt)
        write(fo, fmt) c, i
    else
        write(fo, *) 'No solution' ! 初期値の解が同符号で二分法を使って求められない
    endif

    close(fo)
end program ex4
