module prints
        implicit none
        contains
                subroutine printdata( vett1, vett2, prefix, R, C )
                        integer, intent( in ) :: R, C
                        real( kind = 8 ), dimension( C ), intent( in ) :: vett1
                        real( kind = 8 ), dimension( C, R ), intent( in ) :: vett2
                        character( len = 5 ), intent( in ) :: prefix
        
                        integer :: i, j
                        character( len = 20 ) :: SALVATAGGIO

                        do i = 1, R
                                write( SALVATAGGIO, "(a5,i2,a4)" ) prefix, (i - 1), ".txt"
                                print *, SALVATAGGIO
                                open( unit = 11, file = SALVATAGGIO )
                                do j=1, C
                                        write( 11, fmt='(e25.17,e25.17)' ) vett1(j),  vett2(j,i)
                                end do
                                close( unit = 11 )
                        end do
                end subroutine printdata
end module prints

