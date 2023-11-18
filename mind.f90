module mind
    ! Declarations
    use mpath

    public :: makeI, pathI, xposI, yposI, zposI, distI, ind_eqI

    type, public :: ind
        private
        type(path) :: w
        real :: x
        real :: y
        real :: z
    end type ind

contains

    ! Subroutine to create an individual
    subroutine makeI(w, x, y, z, i)
        type(path), intent(in) :: w
        real, intent(in) :: x, y, z
        type(ind), intent(out) :: i

        i%w = w
        i%x = x
        i%y = y
        i%z = z
    end subroutine makeI

    ! Subroutine to obtain the path of an individual
    subroutine pathI(i, w)
        type(ind), intent(in) :: i
        type(path), intent(out) :: w

        w = i%w
    end subroutine pathI

    ! Function to get the x-coordinate of an individual
    function xposI(i) result(x)
        type(ind), intent(in) :: i
        real :: x

        x = i%x
    end function xposI

    ! Function to get the y-coordinate of an individual
    function yposI(i) result(y)
        type(ind), intent(in) :: i
        real :: y

        y = i%y
    end function yposI

    ! Function to get the z-coordinate of an individual
    function zposI(i) result(z)
        type(ind), intent(in) :: i
        real :: z

        z = i%z
    end function zposI

    ! Function to calculate the distance between two individuals
    function distI(i1, i2) result(r)
        type(ind) :: i1, i2
        real :: a, b, c, d, e, f, r

        a = i1%x
        b = i1%y
        c = i1%z
        d = i2%x
        e = i2%y
        f = i2%z

        r = sqrt(((a - d) ** 2) + ((b - e) ** 2) + ((c - f) ** 2))
    end function distI

    ! Function to check equality between two individuals
    function ind_eqI(i1, i2) result(b)
        type(ind), intent(in) :: i1, i2
        logical :: b

        if ((xposI(i1) == xposI(i2)) .and. (yposI(i1) == yposI(i2)) .and. (zposI(i1) == zposI(i2))) then
            b = .true.
        else
            b = .false.
        end if
    end function ind_eqI

end module mind
