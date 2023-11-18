module msoup
    ! Declarations
    use mpath
    use mind

    public :: newS, insertS, nextS, takeS, an_indS, emptyS, sizeS, showS

    type, public :: soup
        private
        type(node), pointer :: fst ! Points to first individual of path
        type(node), pointer :: lst ! Points to last individual of path
        integer :: ln ! Number of vertices (length) of path
    end type soup

    type, private :: node  ! Every vertice is stored as a node
        type(ind) :: ind ! Individual
        type(node), pointer :: next ! Next
    end type node

contains

    ! Function to create an empty soup
    function newS() result(s)
        type(soup) :: s

        s%fst => null()
        s%ln = 0
    end function newS

    ! Subroutine to add an individual to the soup
    subroutine insertS(i, s)
        type(ind), intent(in) :: i
        type(soup), intent(inout) :: s
        type(node), pointer :: aux

        allocate(aux)
        aux%ind = i
        if (s%ln > 0) then
            s%lst%next => aux
        else
            s%fst => aux
        end if
        s%lst => aux
        s%ln = s%ln + 1
    end subroutine insertS

    ! Subroutine to find 2 individuals with the shortest distance between their compatible paths
    subroutine nextS(s, i1, i2, b)
        type(soup), intent(inout) :: s
        type(ind), intent(out) :: i1, i2
        logical, intent(out) :: b
        type(node), pointer :: aux1, aux2
        type(path) :: p1, p2
        integer :: k, l
        real :: d

        ! Maximum distance in the cube is sqrt(3) < 2
        d = 2.0
        b = .false.
        aux1 => s%fst
        do k = 1, s%ln
            aux2 => aux1%next
            do l = k + 1, s%ln
                call pathI(aux1%ind, p1)
                call pathI(aux2%ind, p2)
                ! Check path compatibility
                if (compP(p1, p2) .or. compP(p2, p1)) then
                    b = .true.
                    ! Check minimum distance criterion
                    if (distI(aux1%ind, aux2%ind) < d) then
                        d = distI(aux1%ind, aux2%ind)
                        if (compP(p1, p2)) then
                            i1 = aux1%ind
                            i2 = aux2%ind
                        else
                            i2 = aux1%ind
                            i1 = aux2%ind
                        end if
                    end if
                end if
                aux2 => aux2%next
            end do
            aux1 => aux1%next
        end do
    end subroutine nextS

    ! Subroutine to remove an individual from the soup
    subroutine takeS(i, s)
        type(soup), intent(inout) :: s
        type(ind), intent(in) :: i
        type(node), pointer :: del, aux
        integer :: k

        ! Deletes a single instance of i
        if (.not. emptyS(s)) then
            ! Test first node separately to avoid bad references with s%start
            del => s%fst
            if (ind_eqI(del%ind, i)) then
                s%fst => s%fst%next
                s%ln = s%ln - 1
                deallocate(del)
            ! Test all other nodes
            else
                aux => s%fst
                do k = 2, sizeS(s)
                    del => del%next
                    if (ind_eqI(del%ind, i)) then
                        aux%next => del%next
                        s%ln = s%ln - 1
                        deallocate(del)
                        exit
                    end if
                    aux => aux%next
                end do
            end if
        end if
    end subroutine takeS

    ! Subroutine to return a random individual from the soup
    subroutine an_indS(s, i)
        type(soup), intent(inout) :: s
        type(ind), intent(out) :: i

        i = s%fst%ind
    end subroutine an_indS

    ! Function to check whether the soup is empty or not
    function emptyS(s) result(b)
        type(soup), intent(in) :: s
        logical :: b

        if (associated(s%fst)) then
            b = .false.
        else
            b = .true.
        end if
    end function emptyS

    ! Function to display the number of elements in a soup
    function sizeS(s) result(r)
        type(soup) :: s
        integer :: r

        r = s%ln
    end function sizeS

    ! Subroutine to display all the individuals of a soup
    subroutine showS(s)
        type(soup), intent(inout) :: s
        type(node), pointer :: aux
        type(path) :: path_print
        integer :: k

        aux => s%fst
        do k = 1, s%ln
            call pathI(aux%ind, path_print)
            print *,"Individual", k, "at coordinates", xposI(aux%ind), yposI(aux%ind), zposI(aux%ind), "has path:"
            call showP(path_print)
            aux => aux%next
        end do
    end subroutine showS

end module msoup
