program character
    implicit none
    ! 文字列の宣言
    character(8) A
    ! 文字列の宣言と初期化
    character(8) :: B = "Good"
    ! 違うタイプの文字列の宣言
    character C*8
    ! 動的な文字列の宣言
    character(:),allocatable :: d

    A="Hello!"
    C="Goodbey!!!"
    b=trim(B)//C
    ! A= (B//"3000")
    print *, len(A), len(trim(A)), len(C)
    print*, A, "   ", B, C


    ! 動的な割付け、代入は自動で再割り付けされる
    allocate(D,source="Baddddddd")
    deallocate(D)
    allocate(character(len=10) :: D)
    D = "Hello"
    D="Good"
    print *, len(D)
end program character

! 文字列は文字の配列とは違って直接文字列を代入できる
! 終端文字はないので、文字列の長さを指定する必要がある。残りは空白になっている
! 配列と異なりにA(n:n)で要素番号にアクセスできる
! 文字列の結合は//で行う
! trimは文字列の後ろの空白を取り除く
! lenは文字列の長さを返す
! index(検索文字列, 検索対象文字列)は検索対象文字列の中で検索文字列が最初に現れる位置を返す(整数)
! scan(検索文字列,文字集合)検索対象文字列の中で検索文字集合のひとつが最初に現れる位置を返す(整数)
! write(str,'(I3)') i で整数を文字列に変換できる。ただし、動的な文字列はallocateで長さを確保してから