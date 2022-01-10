MODULE m_init
   IMPLICIT none

CONTAINS
   SUBROUTINE init(field)
      REAL, DIMENSION(:,:) :: field
      INTEGER, DIMENSION(2) :: NxNy
      INTEGER :: i, j

      NxNy = SHAPE(field)

      ! apply Dirilect boundary condition (i.e. boundaries set to and kept at 1)
      DO i = 1, NxNy(1)
         field(i, 1)         = 1
         field(i, NxNy(2))   = 1
      ENDDO

      DO j = 1, NxNy(2)
         field(1, j)         = 1
         field(NxNy(1), j)   = 1
      ENDDO

      ! initialize the rest to 0
      DO j = 2, NxNy(2) - 1
         DO i = 2, NxNy(1) - 1
            field(i, j)       = 0
         ENDDO
      ENDDO

   END SUBROUTINE init
END MODULE m_init
