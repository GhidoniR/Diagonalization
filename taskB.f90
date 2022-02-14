program taskB
        use controlli
        use diagonale
        use prints

        implicit none

        integer :: i, j, coeff, N, num_autoval, num_trovati, CF, lwork, info
        integer, dimension(:), allocatable :: iwork, ifail, twork

        real (kind = 8) :: L, tol, dx, T1, T2, pi = 4.d0*datan(1.d0)
        real (kind = 8), dimension(:), allocatable :: x, k, autoval, energie, errori, work
        real (kind = 8), dimension(:,:), allocatable :: H, autovett

        complex (kind = 8) :: i_unit = (0, 1)
        complex (kind = 8), dimension(:,:), allocatable :: u, autostati

        character (len = 13) :: PARAMETRI = "parametri.txt", FOURIER = "cffourier.txt"
        character (len = 5 ) :: outen

        ! Lettura dati

        open (unit = 11, file = PARAMETRI)
        read (11, *) L, N, num_autoval, tol
        close (unit = 11)

        open (unit = 11, file = FOURIER)
        read (11, *) CF
        close (unit = 11)

        CF = 2*CF + 1


        ! Costruzione componenti di Fourier
        
        allocate ( k(CF) )                      ! allocazione in memoria di k, con dimensione CF
        
        do coeff = 1, CF
                k(coeff) = (coeff - 1 - (CF -1)/2) * pi/L
        end do


        call dimensionale(CF, num_autoval)
        
        ! Costruzione dell'Hamiltoniana

        allocate ( H(CF, CF) )                  ! allocazione in memoria di H, matrice simmetrica di dimensione CF

        call simmetrica(k, CF, L, H)
        
        ! Calcolo degli autovalori e degli autovettori

        allocate ( autoval(num_autoval) )       ! allocazione in memoria dell'array degli autovalori, dimensione M
        allocate ( autovett(CF, num_autoval)  ) ! allocazione della matrice degli autovettori, dimensione CFxM
        allocate ( ifail(num_autoval) )
        lwork = 8*CF 
        allocate ( work(lwork) )
        allocate( twork(5*CF) )
        
        call cpu_time( T1 )
        
        call dsyevx('V', 'I', 'L', CF, H, CF, 0, 0, 1, num_autoval, tol, num_trovati, &
                & autoval, autovett, CF, work, lwork, twork, ifail, info)

        call cpu_time( T2 )
        
        open( unit = 11, file = "tempoB.txt" )
        write( 11, * ) T2- T1
        close( unit = 11 )

        call esito(num_autoval, num_trovati, ifail, info)

        ! calcolo delle energie
        allocate ( energie(num_autoval) )
        energie = autoval / 2

        ! calcolo degli errori
        allocate ( errori(num_autoval) )
        call errdiff(energie, num_autoval, errori)

        ! costruzuione vettore posizione
        dx = L / (N - 1) 

        allocate ( x( N ) )
        
        do i = 1, N
                x(i) = (-(N - 1)/2 + i -1) * dx
        end do

        ! costruzione vettore u
        allocate ( u(N, CF) )

        do j = 1, N
                do i = 1, CF 
                        u(j,i) = exp( -i_unit*k(i)*x(j) ) / sqrt(L)
                end do
        end do


        ! costruzione autostati
        allocate ( autostati(N, num_autoval) )
        autostati(:,:) = 0

        do i = 1, num_autoval
                !do j = 1, N
                        do coeff = 1, CF
                        do j = 1, N
                                autostati(j, i) = autostati(j, i) + autovett(coeff, i)*u(j, coeff)
                        end do
                end do
        end do


        call printdata( x, dble(autostati), "autoB", num_autoval, N)
        call printdata( x, aimag(autostati), "immgB", num_autoval, N)

        write( outen, "(a2,i3)" ) "EB", CF
        call printdata( energie, errori, outen, 1, num_autoval)

end program taskB
