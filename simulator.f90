!SIMULADOR
program sim
        use mpath
        use mind
        use msoup
        
        ! Declarations
        ! Declare variables and types here

        logical:: a, b, c, d, e, sb, ib1, ib2, ib3
        integer ::  c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, n1, n2
        integer :: beg, end, ka, nu, no, s, lengp, eln
        real :: randx, randy, randz
        type(soup) :: sp, sp2, sp3, sp4
        type(path) :: tp, pp, w1, w2, w3, w4, w5, w6, w7
        type(ind) :: tind, i1, i2, i3, i4, i5, i6, i7

        !Step 0

        print *,"Step 0: Preparation. Print intermediate results? (T/F)"
        read *, a
        print *,"Step 1: Hybridization. Print intermediate results? (T/F)"
        read *, b
        print *,"Step 2: Selection of paths with wanted origin and end nodes.  Print intermediate results? (T/F)"
        read *, c
        print *,"Step 3: Selection of paths with wanted length. Print intermediate results? (T/F)"
        read *, d
        print *,"Step 4: Selection of paths that pass through all the wanted nodes. Print intermediate results? (T/F)"
        read *, e
        print *,"Step 5: Presentation of final results. Will be printed (you get no choice! >:) )"
        print *,"Input sigma, that is, the origin node for the hamiltonian path."
        read *, beg
        print *,"Input tau, that is, the end node for the hamiltonian path."
        read *, end
        print *,"Input kappa, that is, the number of copies of edges to be placed into the soup."
        read *, ka
        print *,"Input nu, that is, the number of path concatenations to be done."
        read *, nu
        print *,"Input d, that is, the number of nodes of the graph. They will be numbered from 1 to n."
        read *, no
        print *,"Input s, that is, the number of edges of the graph."
        read *, s

        ! Random seed initialization
        call random_seed()
        c2=1
        sp=newS()

        ! Step 0: Generating edges with random coordinates
        do c3=1, s
                c12=1
                do c5=1, ka
                        if(c12==1)then
                                print *,"Input edge number",c3,": (Node1,Node2)"
                                read *,n1,n2
                                tp=makeP(n1,n2)
                                call random_number(randx)
                                call random_number(randy)
                                call random_number(randz)
                                call makeI(tp,randx,randy,randz,tind)
                                call insertS(tind,sp)
                        else
                                call random_number(randx)
                                call random_number(randy)
                                call random_number(randz)
                                call makeI(tp,randx,randy,randz,tind)
                                call insertS(tind,sp)
                        end if
                        if (a) then
                                print *, "Individual No.", c2
                                call showP(tp)
                                print *, "Coordinates:", " x =",randx, "y =",randy, "z =",randz
                                print *, "" !println
                        end if
                        c2=c2+1
                        c12=c12+1
                end do
        end do

        print *, "STEP 0 SUCESSFUL "

        !Step 1
        do c6=1, nu

                call nextS(sp,i1,i2,sb)
                call pathI(i1,w1)
                call pathI(i2,w2)
                call glueP(w1,w2,w3)
                call random_number(randx)
                call random_number(randy)
                call random_number(randz)
                call makeI(w3,randx,randy,randz,i3)
                call insertS(i3,sp)
                call takeS(i1,sp)
                call takeS(i2,sp)

        end do
        if (b) then
                print *,"At step 1, the soup contains the individuals:"
                call showS(sp)
        end if

        print *, "STEP 1 SUCESSFUL"

        !Step 2
        sp2=newS()

        do c7=1, sizeS(sp) !Do length?
                call an_indS(sp,i4)
                call pathI(i4,w4)
                call crossesP(w4,beg,ib1)
                call crossesP(w4,end,ib2)
                if(.not. ib1 .or. .not. ib2) then
                        call takeS(i4,sp)
                else
                        call insertS(i4,sp2)
                        call takeS(i4,sp)
                end if
        end do

        if (c) then
                print *,"At step 2, the soup contains the individuals:"
                call showS(sp2)
        end if

        print *, "..."

        !Step 3
        sp3=newS()
        do c8=1, sizeS(sp2)
                call an_indS(sp2,i5)
                call pathI(i5,w5)
                lengp=lengthP(w5)
                if (lengp/=no) then
                        call takeS(i5,sp2)
                else
                        call insertS(i5,sp3)
                        call takeS(i5,sp2)
                end if
        end do
        if (c) then
                print *,"At step 3, the soup contains the individuals:"
                call showS(sp3)
        end if

        !Step 4
        sp4=newS()
        eln=0
        do c9=1, no
                eln=eln+1
                do c10=1, sizeS(sp3)
                        call an_indS(sp3,i6)
                        call pathI(i6,w6)
                        call crossesP(w6,eln,ib1)
                        if(.not. ib1) then
                                call takeS(i6,sp3)
                        else
                                call insertS(i6,sp4)
                                call takeS(i6,sp3)
                        end if
                end do
        end do
        if (d) then
                print *,"At step 4, the soup contains the individuals:"
                call showS(sp4)
        end if

        !Step 5

        print *, ""
        print *, "These are hamiltonian paths of the graph:"
        print *, ""
        do c11=1, sizeS(sp4)
                call an_indS(sp4,i7)
                call pathI(i7,w7)
                call showP(w7)
                print *, "-------------"
                call takeS(i7,sp4)
        end do

        print *, ""
        print *, "Join the graph side!"

        print *, ""

        pause

end program sim

