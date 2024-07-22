program fibonacci_example
    implicit none
    integer :: n, result

    ! ユーザーに入力を求める
    print *, "Enter a number:"
    read *, n

    ! フィボナッチ数列を計算する再帰関数を呼び出す
    result = fibonacci(n)

    ! 結果を表示
    print *, "Fibonacci number", n, "is", result

contains
    ! 再帰関数 fibonacci を定義
    recursive function fibonacci(n) result(res)
        implicit none
        integer, intent(in) :: n
        integer :: res
        ! 再帰終了条件
        if (n == 0) then
            res = 0
        elseif (n == 1) then
            res = 1
        else
            res = fibonacci(n - 1) + fibonacci(n - 2)
        end if
    end function fibonacci

end program fibonacci_example
