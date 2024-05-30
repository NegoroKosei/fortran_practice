program test
    implicit none
    integer i
    do 10 i = 1, 10
        if (i > 7)  cycle
        write(*,*) i
        read(file_num, '(I2)', iostat=io) max_data_file_number
10  continue
end program test

! continue 文　の使い方
! １．goto文と合わせて指定行にジャンプする
! ex) goto 10
! 10 continue 
!同じ文番号のcontinueにジャンプ
! ２．do文と合わせてループ終了を示す
! ex) do 20 i = 1, 10
! 10 continue
! で文番号10のdo文すべてを終わらせる

! cycle 文　はdoループを次のループに doにタグ付けすることで、指定したdo文に飛ぶ
! exit 文　はdoループを抜け出す