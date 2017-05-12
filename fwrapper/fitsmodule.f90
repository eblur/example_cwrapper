SUBROUTINE multiply(x, const, results)

IMPLICIT NONE
REAL, DIMENSION(3), INTENT(IN) :: x
REAL, INTENT(IN) :: const
REAL, DIMENSION(3), INTENT(OUT) :: results
INTEGER :: i, nx

nx = SIZE(x)

DO i = 1, nx
   results(i) = x(i) * const
END DO

END SUBROUTINE multiply

