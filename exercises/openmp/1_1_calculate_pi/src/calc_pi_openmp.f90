FUNCTION calculate_pi(n) result(pi)

   !!**************************************************!!
   !!     Function Information                         !!
   !!     Name of function: calc_pi                    !!
   !!     Type: REAL FUNCTION                          !!
   !!     Dummy Arguments:                             !!
   !!              n - INTENT(IN)                      !!
   !!     Function Description:                        !!
   !!              Estimates pi through a simple sum,  !!
   !               with the upper bounds given by "n". !!
   !!**************************************************!!

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: n
   INTEGER             :: i
   REAL                :: pi, temp

   pi = 0.0
   !$OMP PARALLEL
   !$OMP DO REDUCTION(+:pi)
   DO i = 1, n
      pi = pi + 4.0 / &
         ( 1.0 + ((i - 0.5) / n) ** 2.0 )
   ENDDO
   !$OMP END DO
   !$OMP END PARALLEL
   pi = pi / n

END FUNCTION calculate_pi

PROGRAM calc_pi_openmp

   USE m_timer
   USE OMP_LIB

   IMPLICIT NONE

   INTEGER :: n, maxn, freq
   REAL :: pi, calculate_pi, cpu_t1, cpu_t2, wall_t1, wall_t2

   freq = 1000

   !PRINT *, TRIM('>> Enter maxn:'), TRIM('     (Note: huge(n) = '), huge(n)
   !READ *, maxn

   maxn = 10000

   CALL start_timer(cpu_t1, wall_t1)
   DO n = 1, maxn
      pi = calculate_pi(n)
      !IF (MOD(n,freq).EQ.0) THEN
      !  PRINT*, TRIM("pi = "), pi, TRIM("(n = "), n, TRIM(")")
      !ENDIF
   ENDDO
   PRINT*, TRIM("pi = "), pi, TRIM("(n = "), n, TRIM(")")
   CALL stop_timer(cpu_t1, cpu_t2, wall_t1, wall_t2)


END PROGRAM calc_pi_openmp
