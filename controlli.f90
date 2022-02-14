module controlli
        implicit none

        contains

        !!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Controllo dimensionale !
        !!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine dimensionale( N_punti, N_autoval )
                integer, intent( in ) :: N_punti, N_autoval

                if ( N_autoval > N_punti ) then
                        print *, "!!! ERRORE !!!"
                        print *, ""
                        print *, "::::   Il numero di autovalori da trovare è"
                        print *, ":::: maggiore della dimensione della matrice"

                        stop
                else
                        print*, "... OK ..."
                end if

        end subroutine dimensionale


        !!!!!!!!!!!!!!!!!!!
        ! Controllo esito !
        !!!!!!!!!!!!!!!!!!!

        subroutine esito(N_richiesti, N_trovati, ifail, info )
                integer, intent( in ) :: N_richiesti, N_trovati, info
                integer, dimension( N_richiesti ), intent( in ) :: ifail

                if ( N_richiesti /= N_trovati .or. info /= 0 ) then
                        print *, "!!! ERRORE !!!"
                        print *, ""
                        print *, "::::   La ricerca degli autovalori non è andata a buon fine."
                        print *, "::::   ifail = ", ifail

                        stop
                else
                        print *, "... OK ..."
                end if
        end subroutine esito


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Errore rispetto al risultato esatto !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine errdiff(energie, num_autoval, errori)
                integer, intent( in ) :: num_autoval
                real( kind = 8 ), dimension( num_autoval ), intent( in ) :: energie
                real( kind = 8 ), dimension( num_autoval ), intent( out ) :: errori

                integer :: i
                do i=1, num_autoval
                        errori(i) =  abs( energie(i) - (0.5 + i - 1) )
                        errori(i) = (errori(i)*100) / energie(i)
                end do
        end subroutine errdiff

end module controlli
