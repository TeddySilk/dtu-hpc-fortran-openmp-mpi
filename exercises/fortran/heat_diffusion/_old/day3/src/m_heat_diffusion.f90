MODULE m_heat_diffusion

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global
   USE m_arrays
   USE m_io

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE init_tfield
      MODULE PROCEDURE init_tfield_single, init_tfield_double
   END INTERFACE

   INTERFACE simulate_diffusion
      MODULE PROCEDURE simulate_diffusion_single, simulate_diffusion_double
   END INTERFACE


CONTAINS


   ! ------------------------------------------------- !
   ! SUBROUTINE: INIT_TFIELD                           !
   ! ------------------------------------------------- !
   SUBROUTINE init_tfield_single(tfield, initial_temperature)

      ! input variables
      REAL, DIMENSION(:,:), INTENT(INOUT) :: tfield

      ! local variables
      INTEGER, DIMENSION(2)   :: NxNy
      INTEGER                 :: i, j

      ! optional variables
      REAL, OPTIONAL                :: initial_temperature

      NxNy = SHAPE(tfield)

      ! apply Dirilect boundary condition
      DO i = 1, NxNy(1)
         tfield(i, 1)         = 1.0
         tfield(i, NxNy(2))   = 1.0
      ENDDO

      DO j = 1, NxNy(2)
         tfield(1, j)         = 1.0
         tfield(NxNy(1), j)   = 1.0
      ENDDO

      ! initialize field to T = 0, unless other value given
      IF (.NOT.PRESENT(initial_temperature)) THEN
         DO j = 2, NxNy(2) - 1
            DO i = 2, NxNy(1) - 1
               tfield(i, j)       = 0.0
            ENDDO
         ENDDO
      ELSE
         DO j = 2, NxNy(2) - 1
            DO i = 2, NxNy(1) - 1
               tfield(i, j)       = initial_temperature
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE init_tfield_single

   SUBROUTINE init_tfield_double(tfield, initial_temperature)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:,:), INTENT(INOUT) :: tfield

      ! local variables
      INTEGER, DIMENSION(2)   :: NxNy
      INTEGER                 :: i, j

      ! optional variables
      DOUBLE PRECISION, OPTIONAL                :: initial_temperature

      NxNy = SHAPE(tfield)

      ! apply Dirilect boundary condition
      DO i = 1, NxNy(1)
         tfield(i, 1)         = 1.0
         tfield(i, NxNy(2))   = 1.0
      ENDDO

      DO j = 1, NxNy(2)
         tfield(1, j)         = 1.0
         tfield(NxNy(1), j)   = 1.0
      ENDDO

      ! initialize field to T = 0, unless other value given
      IF (.NOT.PRESENT(initial_temperature)) THEN
         DO j = 2, NxNy(2) - 1
            DO i = 2, NxNy(1) - 1
               tfield(i, j)       = 0.0
            ENDDO
         ENDDO
      ELSE
         DO j = 2, NxNy(2) - 1
            DO i = 2, NxNy(1) - 1
               tfield(i, j)       = initial_temperature
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE init_tfield_double


   ! ------------------------------------------------- !
   ! SUBROUTINE: SIMULATE_DIFFUSION                    !
   ! ------------------------------------------------- !
   SUBROUTINE simulate_diffusion_single(tfield, nsteps, verbose)

      ! input variables
      REAL, DIMENSION(:, :), INTENT(INOUT) :: tfield
      INTEGER, INTENT(IN) :: nsteps
      INTEGER, INTENT(IN) :: verbose

      ! local variables
      REAL, DIMENSION(:, :), ALLOCATABLE  :: w
      INTEGER, DIMENSION(2)               :: s
      INTEGER                             :: i, j, istep, info

      ! generate work array w
      s = SHAPE(tfield)
      CALL alloc(w, s(1), s(2), info)
      CALL copy_arrays(tfield, w)

      ! perform time-stepping
      DO istep = 1, nsteps

         ! compute time-advanced field
         DO j = 2, Ny - 1
            DO i = 2, Nx - 1
               w(i, j) = tfield(i, j) &
                  + dt * (tfield(i + 1, j) - 2 * tfield(i, j) + tfield(i - 1, j)) * rdx2 &
                  + dt * (tfield(i, j + 1) - 2 * tfield(i, j) + tfield(i, j - 1)) * rdy2
            ENDDO
         ENDDO

         ! copy temporary work array w (time-advanced) into tfield (old)
         !CALL copy_arrays(w, tfield)
         CALL swap(w, tfield)

         IF (verbose.EQ.1) THEN
            CALL extract_field(tfield, output_file, istep)
         ENDIF

      ENDDO

   END SUBROUTINE simulate_diffusion_single

   SUBROUTINE simulate_diffusion_double(tfield, n, verbose)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:, :), INTENT(INOUT) :: tfield
      INTEGER, INTENT(IN) :: n
      INTEGER, INTENT(IN) :: verbose

      ! local variables
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE  :: w
      INTEGER, DIMENSION(2)               :: s
      INTEGER                             :: i, j, istep, info

      s = SHAPE(tfield)
      CALL alloc(w, s(1), s(2), info)

      ! perform time-stepping
      DO istep = 1, n

         ! compute time-advanced field
         DO j = 2, Ny - 1
            DO i = 2, Nx - 1
               w(i, j) = tfield(i, j) &
                  + dt * (tfield(i + 1, j) - 2 * tfield(i, j) + tfield(i - 1, j)) * rdx2 &
                  + dt * (tfield(i, j + 1) - 2 * tfield(i, j) + tfield(i, j - 1)) * rdy2
            ENDDO
         ENDDO

         ! copy temporary work array w (time-advanced) into tfield (old)
         CALL copy_arrays(w, tfield)

         IF (verbose.EQ.1) THEN
            CALL extract_field(tfield, output_file, istep)
         ENDIF

      ENDDO

   END SUBROUTINE simulate_diffusion_double

END MODULE m_heat_diffusion
