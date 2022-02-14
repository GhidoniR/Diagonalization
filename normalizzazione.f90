module normalizzazione
        implicit none

        contains

        subroutine unitaria(funzione, vettore, n, m)
                integer, intent( in ) :: n, m
                real( kind = 8 ), dimension(n), intent( in ) :: vettore
                real( kind = 8 ), dimension(n,m), intent( out ) :: funzione

                integer :: i, j
                real( kind = 8 ) :: area, h
                
                do j = 1, m - 1
                        area = 0
                        do i = 1, n - 1
                                area = area + (funzione(i, j)**2 + funzione(i + 1,j + 1)**2) * (vettore(i + 1) - vettore(i)) / 2
                        end do
                        funzione(:, j) = funzione(:, j) / sqrt(area)
                end do
                                
        end subroutine
end module
