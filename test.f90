module modu
    implicit none
contains
    function two(a, b, i, v, fo) result (c)
        real(8), intent(inout) :: a, b
        integer, intent(out) :: i             ! 反復回数
        integer, intent(in) :: v, fo          ! verboseモード用の変数
        real(8) :: c, ya, yb, yc, delta
        real(8), parameter :: epsilon = 1.0d-15 ! 絶対誤差の大きさを指定
        integer, parameter :: max_i = 100       ! 最大反復回数100
        
        i = 0
        do
            i = i + 1             ! 反復回数の増加
            if (i > max_i) stop 'err :did not converge' ! 最大反復回数max_iを指定
            
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
            ! a, b, cの中で同じ値があれば解とする
            elseif ((a == c) .or. (c == b)) then
                exit
            ! yaとybの符号が異なるならば解はa, bの間にある
            elseif (ya * yc < 0.0d0) then
                b = c
            ! ybとycの符号が同じならば解はc, bの間にある
            else
                a = c
            endif
            if (v == 1) write(fo, *) c, i                   ! verboseモード
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
    integer, parameter :: fo = 11

    ! verboseモードの設定 実行時に-vを入力するとすべてのstepでの結果を出力できる
    integer :: argn = 0, k = 1, v = 0
    character(len=100) :: argc
    argn = command_argument_count()
    do k = 1, argn
        call get_command_argument(k, argc)
        if (argc == '-v') v = 1                      ! -vの入力時、v = 1とする
    end do

    write(*, '(a)', advance='no') 'input xfrom xto : '
    read(*, *) xfrom, xto                          ! 初期値の読み込み
    
    ! 書き込みファイル名、ここで変更する
    open(fo, file='ex4_file/output001.d', status='replace', action='write', iostat=is)
    if (is /= 0) stop 'cannot open output file'    ! 書き込み用ファイルの確認
    
    a = xfrom
    ya = func(a)
    b = xto
    yb = func(b)
    
    if (ya == 0.0d0) then   ! 初期値aが解かどうか確認
        write(fo, *) a, i
    elseif (yb == 0.0d0) then ! 初期値bが解かどうか確認
        write(fo, *) b, i
    elseif (ya * yb < 0) then
        c = two(a, b, i, v, fo)
        write(fo, *) c, i
    else
        write(fo, *) 'No solution' ! 初期値の解が同符号で二分法を使って求められない
    endif
    
    close(fo)
end program ex4
