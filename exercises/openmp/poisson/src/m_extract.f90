MODULE m_extract

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE extract_field
      MODULE PROCEDURE extract_field3
   END INTERFACE

CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE EXTRACT_FIELD                          !
   ! ------------------------------------------------- !
   SUBROUTINE extract_field3(field, output_name, step)

      ! input variables
      CHARACTER(LEN = *), INTENT(IN)                     :: output_name
      DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(IN)   :: field
      INTEGER, OPTIONAL                                  :: step

      ! local variables
      CHARACTER(LEN = 48)  :: string
      INTEGER              :: i, j, k

      IF (PRESENT(step)) THEN
         WRITE(string, '(A,A,A,(I6.6),A)') './res/', trim(output_name), '@', step, '.dat'
         PRINT *, "Saving temperature field data to file: ", string
         OPEN(10, FILE = trim(string))
         DO k = 1, nz
            DO j = 1, ny
               DO i = 1, nx
                  WRITE(10, '(4E12.4)') (-0.5*Lx) + REAL(i - 1) * dx, &
                                        (-0.5*Ly) + REAL(j - 1) * dy, &
                                        (-0.5*Lz) + REAL(k - 1) * dz, &
                                        field(i, j, k)
               ENDDO
            ENDDO
            WRITE(10, '(A)')
         ENDDO
         CLOSE(10)
      ELSE
         WRITE(string, '(A,A,A)') './res/', trim(output_name), '.dat'
         PRINT *, "Saving temperature field data to file: ", string
         OPEN(10, FILE = trim(string))
         DO k = 1, nz
            DO j = 1, ny
               DO i = 1, nx
                  WRITE(10, '(4E12.4)') (-0.5*Lx) + REAL(i - 1) * dx, &
                                        (-0.5*Ly) + REAL(j - 1) * dy, &
                                        (-0.5*Lz) + REAL(k - 1) * dz, &
                                        field(i, j, k)
               ENDDO
            ENDDO
            WRITE(10, '(A)')
         ENDDO
         CLOSE(10)
      ENDIF

   END SUBROUTINE extract_field3

END MODULE m_extract

