! single文による単一処理の実行とbroadcastで変数を他のスレッドにコピーする
program single_broadcast
    !$ use omp_lib
    !$ implicit none
    !$ integer p
    !$ call omp_set_num_threads(4)  ! スレッド数を4に設定
    !$OMP parallel private(p)       ! 並列領域開始　pはプライベート変数
        !$ p = -999
        !$OMP single    ! single文　ここから先は一つのスレッドのみが実行
            !$ write(*,*) 'input p ='   ! pの値を入力
            !$ read(*,*) p
        !$OMP end single copyprivate(p) ! ここでend single 同期が取れる　pの値が他のスレッドにコピーされる
        !$ write(*,*) 'my thread no., p =',omp_get_thread_num(), p
    !$OMP end parallel            ! 並列領域終了
end program single_broadcast
