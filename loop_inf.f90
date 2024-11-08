program loop_inf        !　入力したnまでの値の和を求める,0以下の値を入れると終了
    implicit none
    integer wa,n,i
    do
        write(*,'(a)',advance='no') ' input n (if  n <= 0, stop) : ' !　aは文字列(複数ある場合は後ろからさしていく)を示す。それに対してadvance=no(改行なし)を与える
        read (*,*) n
        if (n==0) then
            exit   !ループ終了
        elseif(n<0) then
            write(*,*) ' good bye ...'
            cycle   !ループの頭へジャンプ
        endif
        wa =0
        do i=1,n
            wa=wa+i
        enddo
        write(*,*) 'wa = ',wa
    enddo
    write(*,*) 'a'
end program loop_inf

