MODULE m_init_tfield

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE init_tfield
      MODULE PROCEDURE init_tfield_single, init_tfield_double
   END INTERFACE


CONTAINS


   ! ------------------------------------------------- !
   ! SUBROUTINE: INIT_TFIELD                           !
   ! ------------------------------------------------- !
   SUBROUTINE init_tfield_single(tfield, boundary_temperature, initial_temperature)

      ! input variables
      REAL, DIMENSION(:,:), INTENT(INOUT) :: tfield

      ! local variables
      INTEGER, DIMENSION(2)   :: NxNy
      INTEGER                 :: i, j

      ! optional variables
      REAL, OPTIONAL                :: boundary_temperature
      REAL, OPTIONAL                :: initial_temperature

      ! ------------------------------------------------ !

      NxNy = SHAPE(tfield)

      ! apply Dirilect boundary condition
      IF (.NOT.PRESENT(boundary_temperature)) THEN
         DO i = 1, NxNy(1)
            tfield(i, 1)         = 1.0
            tfield(i, NxNy(2))   = 1.0
         ENDDO

         DO j = 1, NxNy(2)
            tfield(1, j)         = 1.0
            tfield(NxNy(1), j)   = 1.0
         ENDDO
      ELSE
         DO i = 1, NxNy(1)
            tfield(i, 1)         = boundary_temperature
            tfield(i, NxNy(2))   = boundary_temperature
         ENDDO

         DO j = 1, NxNy(2)
            tfield(1, j)         = boundary_temperature
            tfield(NxNy(1), j)   = boundary_temperature
         ENDDO
      ENDIF

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

      ! ------------------------------------------------ !

   END SUBROUTINE init_tfield_single

   SUBROUTINE init_tfield_double(tfield, boundary_temperature, initial_temperature)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:,:), INTENT(INOUT) :: tfield

      ! local variables
      INTEGER, DIMENSION(2)   :: NxNy
      INTEGER                 :: i, j

      ! optional variables
      DOUBLE PRECISION, OPTIONAL                :: boundary_temperature
      DOUBLE PRECISION, OPTIONAL                :: initial_temperature

      ! ------------------------------------------------ !

      NxNy = SHAPE(tfield)

      ! apply Dirilect boundary condition
      IF (.NOT.PRESENT(boundary_temperature)) THEN
         DO i = 1, NxNy(1)
            tfield(i, 1)         = 1.0
            tfield(i, NxNy(2))   = 1.0
         ENDDO

         DO j = 1, NxNy(2)
            tfield(1, j)         = 1.0
            tfield(NxNy(1), j)   = 1.0
         ENDDO
      ELSE
         DO i = 1, NxNy(1)
            tfield(i, 1)         = boundary_temperature
            tfield(i, NxNy(2))   = boundary_temperature
         ENDDO

         DO j = 1, NxNy(2)
            tfield(1, j)         = boundary_temperature
            tfield(NxNy(1), j)   = boundary_temperature
         ENDDO
      ENDIF

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

      ! ------------------------------------------------ !

   END SUBROUTINE init_tfield_double

END MODULE m_init_tfield

