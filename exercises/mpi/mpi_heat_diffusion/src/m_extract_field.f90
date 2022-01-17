MODULE m_extract_field

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE extract_field
      MODULE PROCEDURE extract_field_single, extract_field_double
   END INTERFACE

CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE EXTRACT_FIELD                          !
   ! ------------------------------------------------- !
   SUBROUTINE extract_field_single(field, output_name, timestep)

      ! input variables
      CHARACTER(LEN = *), INTENT(IN)   :: output_name
      REAL, DIMENSION(:,:), INTENT(IN) :: field
      INTEGER, OPTIONAL                :: timestep

      ! local variables
      CHARACTER(LEN = 48)  :: string
      INTEGER              :: i, j

      IF (PRESENT(timestep)) THEN
         WRITE(string, '(A,A,A,(I6.6),A)') './res/', trim(output_name), '@', timestep, '.dat'
         print *, "Saving temperature field data to file: ", string
         OPEN(10, FILE = trim(string))
         DO j = 1, Ny
            DO i = 1, Nx
               WRITE(10, '(3E12.4)') REAL(i - 1) * dx, REAL(j - 1) * dy, field(i, j)
            ENDDO
            WRITE(10, '(A)')
         ENDDO
         CLOSE(10)
      ELSE
         WRITE(string, '(A,A,A)') './res/', trim(output_name), '.dat'
         print *, "Saving temperature field data to file: ", string
         OPEN(10, FILE = trim(string))
         DO j = 1, Ny
            DO i = 1, Nx
               WRITE(10, '(3E12.4)') REAL(i - 1) * dx, REAL(j - 1) * dy, field(i, j)
            ENDDO
            WRITE(10, '(A)')
         ENDDO
         CLOSE(10)
      ENDIF

   END SUBROUTINE extract_field_single

   SUBROUTINE extract_field_double(field, output_name, timestep)

      ! input variables
      CHARACTER(LEN = *), INTENT(IN)               :: output_name
      DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN) :: field
      INTEGER, OPTIONAL                            :: timestep

      ! local variables
      CHARACTER(LEN = 48)  :: string
      INTEGER              :: i, j

      IF (PRESENT(timestep)) THEN
         WRITE(string, '(A,A,A,(I6.6),A)') './res/', trim(output_name), '@', timestep, '.dat'
         print *, "Saving temperature field data to file: ", string
         OPEN(10, FILE = trim(string))
         DO j = 1, Ny
            DO i = 1, Nx
               WRITE(10, '(3E12.4)') REAL(i - 1) * dx, REAL(j - 1) * dy, field(i, j)
            ENDDO
            WRITE(10, '(A)')
         ENDDO
         CLOSE(10)
      ELSE
         WRITE(string, '(A,A,A)') './res/', trim(output_name), '.dat'
         print *, "Saving temperature field data to file: ", string
         OPEN(10, FILE = trim(string))
         DO j = 1, Ny
            DO i = 1, Nx
               WRITE(10, '(3E12.4)') REAL(i - 1) * dx, REAL(j - 1) * dy, field(i, j)
            ENDDO
            WRITE(10, '(A)')
         ENDDO
         CLOSE(10)
      ENDIF

   END SUBROUTINE extract_field_double


END MODULE m_extract_field

