MODULE m_out
   USE m_constants

CONTAINS
   SUBROUTINE extract(field, output_name, timestep)

      IMPLICIT none

      CHARACTER(LEN = 24), INTENT(IN) :: output_name
      REAL, DIMENSION(:,:), INTENT(IN) :: field
      INTEGER, OPTIONAL :: timestep

      CHARACTER(LEN = 48) :: string
      INTEGER :: i, j

      IF (PRESENT(timestep)) THEN
         WRITE(string, '(A,A,A,(I6.6),A)') './res/', trim(output_name), '@', timestep, '.dat'
         print *, string
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
         OPEN(10, FILE = trim(string))
         DO j = 1, Ny
            DO i = 1, Nx
               WRITE(10, '(3E12.4)') REAL(i - 1) * dx, REAL(j - 1) * dy, field(i, j)
            ENDDO
            WRITE(10, '(A)')
         ENDDO
         CLOSE(10)
      ENDIF

   END SUBROUTINE extract
END MODULE m_out
