module mpath
    ! This module defines operations for manipulating paths in a graph

    public :: makeP, firstP, lastP, lengthP, compP, glueP, crossesP, copyP, deleteP, showP
    private :: newP, enterP, shortenP, revP

    ! Define the 'path' and 'node' types
    type, public :: path
        private
        type(node), pointer :: fst ! Points to first element of path
        type(node), pointer :: lst ! Points to last element of path
        integer :: ln ! Number of vertices (length) of path
    end type path

    type, private :: node ! Represents a vertex stored as a node
        integer :: value
        type(node), pointer :: next
    end type node

contains

    ! Function to create an empty path
    function newP() result(r)
        type(path) :: r

        r%fst => null()
        r%lst => null()
        r%ln = 0
    end function newP

    ! Subroutine to add an element to the back of a path
    subroutine enterP(x, w)
        integer, intent(in) :: x
        type(path), intent(inout) :: w
        type(node), pointer :: aux

        allocate(aux)
        aux%value = x
        if (w%ln > 0) then
            w%lst%next => aux
        else
            w%fst => aux
        end if
        w%lst => aux
        w%ln = w%ln + 1
    end subroutine enterP

    ! Subroutine to copy elements from one path to another up to 'd' elements
    subroutine revP(s, r, d)
        type(path), intent(in) :: s
        type(path), intent(inout) :: r
        integer, intent(in) :: d
        type(node), pointer :: aux

        if (d > 0) then
            aux => s%fst
            do i = 1, d
                call enterP(aux%value, r)
                aux => aux%next
            end do
        end if
    end subroutine revP

    ! Subroutine to delete the first element of a path
    subroutine shortenP(w)
        type(path), intent(inout) :: w
        type(node), pointer :: aux

        if (w%ln > 1) then
            aux => w%fst
            w%fst => w%fst%next
            deallocate(aux)
            w%ln = w%ln - 1
        else
            if (w%ln == 1) then
                aux => w%fst
                w%fst => null()
                w%lst => null()
                deallocate(aux)
            end if
            if (w%ln == 0) then
                print *, "Error: Cannot shorten empty path"
            end if
        end if
    end subroutine shortenP

    ! Function to create a path from two vertices
    function makeP(v1, v2) result(w)
        integer, intent(in) :: v1, v2
        type(path) :: w, t
        t = newP()
        call enterP(v1, t)
        call enterP(v2, t)
        w = t
    end function makeP

    ! Function to get the first element of a path
    function firstP(w) result(v)
        integer :: v
        type(path), intent(in) :: w

        if (w%ln > 0) then
            v = w%fst%value
        else
            print *, "Error: Cannot display first element of an empty path"
        end if
    end function firstP

    ! Function to get the last element of a path
    function lastP(w) result(v)
        integer :: v
        type(path), intent(in) :: w

        if (w%ln > 0) then
            v = w%lst%value
        else
            print *, "Error: Cannot display last element of an empty path"
        end if
    end function lastP

    ! Function to get the length of a path
    function lengthP(w) result(l)
        integer :: l
        type(path), intent(in) :: w
        l = w%ln
    end function lengthP

    ! Function to compare two paths for compatibility
    function compP(w1, w2) result(a)
        type(path), intent(in) :: w1, w2
        logical :: a

        if (w2%fst%value == w1%lst%value) then
            a = .true.
        else
            a = .false.
        end if
    end function compP

    ! Subroutine to glue two compatible paths together into a new path
    subroutine glueP(w1, w2, w3)
        type(path), intent(inout) :: w1, w2
        type(path), intent(out) :: w3
        type(path) :: aux1, aux2

        if (compP(w1, w2)) then
            aux1 = newP()
            aux2 = newP()
            w3 = newP()
            call copyP(w1, aux1)
            call copyP(w2, aux2)
            call shortenP(aux2)
            aux1%lst%next => aux2%fst
            aux1%lst => aux2%lst
            call revP(aux1, w3, aux1%ln + aux2%ln)
        else if (.not. compP(w1, w2)) then
            print *, "Error: Input paths are not compatible"
        end if
    end subroutine glueP

    ! Subroutine to check if a vertex crosses a path
    subroutine crossesP(w, v, b)
        type(path), intent(inout) :: w
        type(node), pointer :: aux
        integer, intent(in) :: v
        integer :: i
        logical, intent(out) :: b

        b = .false.
        if (w%ln == 0) then
            b = .false.
        else
            aux => w%fst
            do i = 1, w%ln
                if (aux%value == v) then
                    b = .true.
                end if
                aux => aux%next
            end do
        end if
    end subroutine crossesP

    ! Subroutine to copy a path to another path
    subroutine copyP(w1, w2)
        type(path), intent(inout) :: w1
        type(path), intent(out) :: w2
        w2 = newP()
        call revP(w1, w2, w1%ln)
    end subroutine copyP

    ! Recursive subroutine to delete elements of a path
    recursive subroutine deleteP(w)
        type(path), intent(inout) :: w
        type(node), pointer :: aux

        if (w%ln == 0) then
            print *, "Error: Cannot delete elements of an empty path"
        end if
        if (w%ln == 1) then
            aux => w%fst
            w%fst => null()
            w%lst => null()
            deallocate(aux)
            w%ln = 0
        else
            if (w%ln > 1) then
                aux => w%fst
                w%fst => w%fst%next
                deallocate(aux)
                w%ln = w%ln - 1
                call deleteP(w)
            end if
        end if
    end subroutine deleteP

    ! Subroutine to display elements of a path
    subroutine showP(w)
        type(path), intent(in) :: w
        type(path) :: r
        integer :: i
        integer, dimension(w%ln) :: aux

        r = newP()
        call revP(w, r, w%ln)
        do i = 1, w%ln
            aux(i) = firstP(r)
            if (r%ln > 1) then
                call shortenP(r)
            end if
        end do
        print *, aux
    end subroutine showP

end module mpath
