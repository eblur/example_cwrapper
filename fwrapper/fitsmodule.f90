SUBROUTINE multiply(x, const, results)

IMPLICIT NONE
REAL, INTENT(IN) :: x
REAL, INTENT(IN) :: const
REAL, DIMENSION(nx), INTENT(OUT) :: results
INTEGER :: i, nx

nx = SIZE(x)

DO i = 1, nx
   results(i) = x(i) * const
END DO

END SUBROUTINE multiply

