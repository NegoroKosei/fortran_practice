module use_interface
    implicit none
    ! 構造体の宣言
    type :: vector2
        real(8) ::x,y
        character(8) :: name
    end type vector2
    ! 構造体の加算を行うためのinterface宣言(vector2 + vector2, vector2 + real)
    ! operator(+)やoperator(.name.)などの演算子を用いることで、前後の変数を関数の引数として受け取る
    interface operator(+)
        module procedure add, add2
    end interface
    interface operator(.cName.)
        module procedure changeName
    end interface
    contains
    function add(a,b) result(ret)
        type(vector2), intent(in) :: a,b
        type(vector2) :: ret
        ret%x = a%x + b%x
        ret%y = a%y + b%y
        ret%name = trim(a%name) // trim(b%name)
    end function add
    function add2(a,b) result(ret)
        type(vector2), intent(in) :: a
        real(8), intent(in) :: b
        type(vector2) :: ret
        ret%x = a%x + b
        ret%y = a%y + b
        ret%name = a%name
    end function add2
    function changeName(a,b) result(ret)
        type(vector2), intent(in) :: a
        character(*), intent(in) :: b
        type(vector2) :: ret
        ret%x = a%x
        ret%y = a%y
        ret%name = trim(a%name) // b
    end function changeName
end module use_interface

program ex7
    use use_interface
    implicit none
    real(8) :: x = 3.0d0
    type(vector2) :: a,b = vector2(3.0,4.0,'b'), c, d
    a%x = 1.0d0
    a%y = 2.0d0
    a%name = "a"
    c = a + b
    d = a + x
    d = d.cName."c"
    print*, c%x, c%y, c%name
    print*, d%x, d%y, d%name
end program ex7

! interfaceを使うことで同じoperatorに対して入力の型が異なる場合でも処理を行うことができる
! interface 総省英　[ret]module procedure 関数名1, 関数名2,...で関数を指定することで、同じ名前の関数を複数作成することができる
! 構造体はtype...end typeで宣言する
! コンストラクタはtype(構造体名) :: a,b,cのように宣言する
! 要素へのアクセスは%を使う
! まとめてa = vector2(3.0,4.0,'b')のように代入することができる