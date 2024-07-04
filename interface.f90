module use_interface
    implicit none
    type :: vector2
        real(8) ::x,y
        character(8) :: name
    end type vector2
    interface operator(+)
        module procedure add
    end interface
    contains
    function add(a,b) result(ret)
        type(vector2), intent(in) :: a,b
        type(vector2) :: ret
        ret%x = a%x + b%x
        ret%y = a%y + b%y
        ret%name = trim(a%name) // trim(b%name)
    end function add
end module use_interface

program ex7
    use use_interface
    implicit none
    type(vector2) :: a,b = vector2(3.0,4.0,'b'),c
    a%x = 1.0d0
    a%y = 2.0d0
    a%name = "a"
    c = a + b
    print*, c%x, c%y, c%name
end program ex7

! interfaceを使うことで同じoperatorに対して入力の型が異なる場合でも処理を行うことができる
! 構造体はtype...end typeで宣言する
! コンストラクタはtype(vector2) :: a,b,cのように宣言する
! 要素へのアクセスは%を使う
! まとめてa = vector2(3.0,4.0,'b')のように代入することができる