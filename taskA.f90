program taskA
        use controlli
        use diagonale
        use normalizzazione
        use prints

        implicit none
        character( len = 13 ) :: DATI = 'parametri.txt'
        integer :: punti, num_autovalori, N_autovalori, i, j, info
        integer, dimension(:), allocatable :: twork, ifail
        real( kind = 8 ), dimension(:), allocatable :: work, x, diag, subdiag, autoval_trovati, energie, errori
        real( kind = 8 ) :: L, dx, tol, T1, T2
        real( kind = 8 ), dimension(:,:), allocatable :: autovettori

        character (len = 5 ) :: outen
        
        ! Lettura dati

        open( unit = 11, file=DATI )
        read( 11, fmt=* ) L, punti, num_autovalori, tol
        close( unit = 11 )


        ! Se il numero di punti è pari, lo si rende dispari, così da includere lo zero nel vettore x
        punti = 2*(punti/2) + 1

        ! Controllo dimensionale

        call dimensionale( punti, num_autovalori )

        allocate( autoval_trovati( num_autovalori ) )
        allocate( diag( punti ) )
        allocate( x( punti ) )
        allocate( autovettori( punti, num_autovalori ) )
        allocate( subdiag( punti - 1 ) )

        ! Costruzione intervallo

        dx = L / (punti - 1)
        
        ! Costruzione vettore x

        do i = 1 , punti
                x(i) = (-(punti -1)/2 + i - 1 )*dx
        end do

        ! Costruzione matrice tridiagonale

        call banda(dx, x, punti, diag, subdiag)
        
        allocate( work( 5*punti  ) )
        allocate( twork( 5*punti ) )
        allocate( ifail( punti ) )

        ! Diagonalizzazione

        call cpu_time(T1)
        call dstevx('V', 'I', punti ,diag ,subdiag, 0, 0, 1, num_autovalori, tol, N_autovalori, &
                & autoval_trovati, autovettori, punti, work, twork, ifail, info)
        call cpu_time(T2)
        open( unit = 11, file = "tempoA.txt" )
        write( 11, * ) T2 - T1
        close( unit = 11 )

        ! Controllo esito

        call esito(num_autovalori, N_autovalori, ifail, info)

        ! Le energie sono valgono la metà degli autovalori

        allocate( energie( num_autovalori ) )
        energie = autoval_trovati / 2

        ! Chiamata della funzione di normalizzazione

        call unitaria(autovettori,x, punti, num_autovalori)

        ! Stampa autovettori

        call printdata( x, autovettori, "autof", num_autovalori, punti )  

        ! Calcolo errori

        allocate( errori( num_autovalori ) )
        call errdiff(energie, num_autovalori, errori)

        ! Stampa energie ed errori
        write( outen, "(a2,i3)" ) "EA", punti

        call printdata( energie, errori, outen, 1, num_autovalori )

end program taskA
