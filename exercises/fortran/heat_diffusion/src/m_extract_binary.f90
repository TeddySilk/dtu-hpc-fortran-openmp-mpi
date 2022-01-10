MODULE m_extract_binary

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE extract_binary
      MODULE PROCEDURE extract_binary_single, extract_binary_double
   END INTERFACE

CONTAINS

! ------------------------------------------------- !
! SUBROUTINE EXTRACT_BINARY                         !
! ------------------------------------------------- !
   SUBROUTINE extract_binary_single(field, output_name, timestep)

      ! input variables
      CHARACTER(LEN = *), INTENT(IN)   :: output_name
      REAL, DIMENSION(:,:), INTENT(IN) :: field
      INTEGER, OPTIONAL                :: timestep

      ! local variables
      CHARACTER(LEN = 48)  :: string

      ! ----------------------------------------------------- !

      IF (.NOT.PRESENT(timestep)) THEN
         timestep = 0
      ENDIF

      WRITE(string, '(A,A,A,(I6.6),A)') './res/', trim(output_name), '@', timestep, '.bin'
      print *, "Saving binary field data to file: ", string
      OPEN(10, FILE = trim(string), FORM='UNFORMATTED')
      WRITE(10) timestep, field
      CLOSE(10)

      ! ----------------------------------------------------- !

   END SUBROUTINE extract_binary_single

   SUBROUTINE extract_binary_double(field, output_name, timestep)

      ! input variables
      CHARACTER(LEN = *), INTENT(IN)               :: output_name
      DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN) :: field
      INTEGER, OPTIONAL                            :: timestep

      ! local variables
      CHARACTER(LEN = 48)  :: string

      ! ----------------------------------------------------- !

      IF (.NOT.PRESENT(timestep)) THEN
         timestep = 0
      ENDIF

      WRITE(string, '(A,A,A,(I6.6),A)') './res/', trim(output_name), '@', timestep, '.bin'
      print *, "Saving binary field data to file: ", string
      OPEN(10, FILE = trim(string), FORM='UNFORMATTED')
      WRITE(10) timestep, field
      CLOSE(10)

      ! ----------------------------------------------------- !

   END SUBROUTINE extract_binary_double


END MODULE m_extract_binary
