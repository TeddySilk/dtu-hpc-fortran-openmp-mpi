MODULE m_copy
   IMPLICIT none

CONTAINS
   SUBROUTINE cp_field(field1, field2)
      REAL, DIMENSION(:,:) :: field1, field2
      INTEGER :: i, j
      INTEGER, DIMENSION(2) :: array_size

      array_size = shape(field1)

      DO j = 1, array_size(2)
         DO i = 1, array_size(1)
            field2(i, j) = field1(i, j)
         ENDDO
      ENDDO

   END SUBROUTINE cp_field
END MODULE m_copy
