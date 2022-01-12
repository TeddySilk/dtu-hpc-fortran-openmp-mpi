MODULE m_solver

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global
   USE m_init
   USE OMP_LIB

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE solver
      MODULE PROCEDURE solver3
   END INTERFACE

CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE SOLVER                                 !
   ! ------------------------------------------------- !
   SUBROUTINE solver3(field, previous_field, source)

      DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(INOUT) :: field, previous_field
      DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(IN)    :: source

      INTEGER  :: i, j, k, step
      DOUBLE PRECISION     :: eps

      ! ------------------------------------------------- !
      ! JACOBI METHOD                                     !
      ! ------------------------------------------------- !
      step = 0
      eps = 1E8
      DO WHILE (eps.GT.eps_min.AND.&
         step.LE.nsteps)
         eps = 0
         !$OMP PARALLEL
         !$OMP DO COLLAPSE(2) PRIVATE(k, j, i)
         DO k = 2, nz - 1
            DO j = 2, ny - 1
               DO i = 2, nx - 1
                  field(i, j, k) = (1.0/6.0) * (&
                     previous_field(i-1, j  , k  ) &
                     + previous_field(i+1, j  , k  ) &
                     + previous_field(i  , j-1, k  ) &
                     + previous_field(i  , j+1, k  ) &
                     + previous_field(i  , j  , k+1) &
                     + previous_field(i  , j  , k-1) &
                     + dx * dx * source(i, j, k) )
               ENDDO
            ENDDO
         ENDDO
         !$OMP END DO
         !$OMP END PARALLEL
         step = step + 1
         eps = NORM2(field - previous_field)
         IF (MOD(step, 100).EQ.0) THEN
            PRINT "(A, 1E14.6, A, 1I6)", "    eps = ", eps, "    step = ", step
            IF (source_type.EQ."sinusoidal") THEN
               PRINT "(A, 1E14.6)", "    geps = ", &
                  NORM2(field - source/(3.0 * 4.0 * ATAN(1.0) * 4.0 * ATAN(1.0)))/&
                  NORM2(source/(3.0 * 4.0 * ATAN(1.0) * 4.0 * ATAN(1.0)))
            ENDIF
         ENDIF


         ! replace the old iteration with the new iteration
         CALL copy_arrays(field, previous_field)

      ENDDO

      PRINT "(A, 1E14.6, A, 1I6)", "    eps = ", eps, "    step = ", step
      IF (source_type.EQ."sinusoidal") THEN
         PRINT "(A, 1E14.6)", "    geps = ", &
            NORM2(field - source/(3.0 * 4.0 * ATAN(1.0) * 4.0 * ATAN(1.0)))/&
            NORM2(source/(3.0 * 4.0 * ATAN(1.0) * 4.0 * ATAN(1.0)))
      ENDIF

   END SUBROUTINE solver3

END MODULE m_solver
