! barrierによる同期、private変数とshared変数の違い
program hello
    !$ use omp_lib
    implicit none
    integer :: nth = 2, p, s
    !$ call omp_set_num_threads(nth)    ! スレッド数を2に設定
    !$OMP PARALLEL private(p)        ! 並列領域開始　pはプライベート変数
        !$ p = omp_get_thread_num()  ! スレッド番号をプライベート変数pに代入
        !$ s = omp_get_thread_num()  ! スレッド番号を共有変数sに代入
        !$OMP BARRIER             ! 同期
        !$ write(* ,*) 'my thread no., p, s, =',omp_get_thread_num(), p, s
    !$OMP END PARALLEL          ! 並列領域終了
end program hello
