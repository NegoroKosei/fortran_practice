! parallel do文によるdo文の並列化、reductionによる結果の集計
program parallel_dotp
    !$ use omp_lib
    implicit none
    integer :: i, n = 1000
    real(8) :: dp = 0.0d0
    real(8), allocatable :: x(:), y(:)
    allocate (x(n), y(n))
    call random_number(x)
    call random_number(y)
    !$ call omp_set_num_threads(4)
    !$OMP parallel do private(i) reduction(+ : dp)
        ! do文はiの大きさによって処理がスレッドに割り当てられる、private(i)でiをスレッドごとに確保、reduction(+ : dp) でdpをスレッドごとに集計
        ! OMP　do文はparallel内に記述することで並列化される
        do i = 1, n
            dp = dp + x(i) * y(i)
        end do
    !$OMP end parallel do   ! 並列do文の終了
    write (* ,*) 'parallel dp =', dp
    write (* ,*) 'serial dp =', dot_product(x, y)
end program parallel_dotp