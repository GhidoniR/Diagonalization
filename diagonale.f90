module diagonale
        implicit none

        contains

        subroutine banda(dx, x, N, diag, subdiag)
                integer, intent( in ) :: N
                real( kind = 8 ), intent( in ) :: dx
                real( kind = 8 ), dimension( N ), intent( in ) :: x
                real( kind = 8 ), dimension( N ), intent( out ) :: diag
                real( kind = 8 ), dimension( N-1 ), intent( out ) :: subdiag
                integer :: i
                do i = 1, N
                        diag( i ) = 2/(dx**2) + x(i)**2
                end do
                subdiag = -1/(dx**2)
        end subroutine banda

        subroutine simmetrica(K, CF, L, H)
                integer, intent( in ) :: CF
                real( kind = 8 ), dimension( CF ), intent( in ) :: K
                real( kind = 8 ), intent( in ) :: L
                real( kind = 8 ), dimension( CF, CF ), intent( out ) :: H
        
                integer :: i, j
                real( kind = 8 ) :: dK

                do i = 1, CF
                        do j = 1, i
                                if (i == j) then
                                        H(i,j) = K(i)*K(j) + (L**2)/3
                                else
                                        dK = K(i) - K(j)

                                        H(i,j) =  ( ((dK**2) * (L**2) - 2) * sin(dK*L) + &
                                                & 2*dK*L*cos(dK*L) ) / (dK**3 * L)
                                end if
                        end do
                end do
        end subroutine simmetrica

end module diagonale
