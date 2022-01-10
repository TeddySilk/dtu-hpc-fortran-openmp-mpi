MODULE m_diagnostics

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE diagnostics
      MODULE PROCEDURE diagnostics_single, diagnostics_double
   END INTERFACE

CONTAINS


   ! ------------------------------------------------- !
   ! SUBROUTINE DIAGNOSTICS                            !
   ! ------------------------------------------------- !
   SUBROUTINE diagnostics_single(field, time, funit, fname)

      ! input variables
      REAL, DIMENSION(:, :), INTENT(IN)   :: field
      REAL, INTENT(IN)                    :: time
      INTEGER, INTENT(IN)                 :: funit
      CHARACTER(LEN = *), INTENT(IN)      :: fname

      ! local variables
      REAL    :: min_val
      LOGICAL :: isopen

      ! --------------------------------------------------------- !

      ! check if file already exists
      INQUIRE(UNIT=funit, OPENED=isopen)
      IF (isopen) THEN
         OPEN(funit, FILE="res/"//TRIM(fname)//".dat", STATUS="replace", ACTION="write")
      ELSE
         OPEN(funit, FILE="res/"//TRIM(fname)//".dat", POSITION="append", ACTION="write")
      END IF

      min_val = MINVAL(field)

      PRINT '(A, F16.8, A, F16.8)', "(t = ", time, ") minimum temperature:", min_val
      WRITE(funit, '(3E12.4)') time, min_val
      CLOSE(funit)

      ! --------------------------------------------------------- !

   END SUBROUTINE diagnostics_single

   SUBROUTINE diagnostics_double(field, time, funit, fname)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN)   :: field
      REAL, INTENT(IN)                                :: time
      INTEGER, INTENT(IN)                             :: funit
      CHARACTER(LEN = *), INTENT(IN)                  :: fname

      ! local variables
      DOUBLE PRECISIOn     :: min_val
      LOGICAL              :: isopen

      ! --------------------------------------------------------- !

      ! check if file already exists
      INQUIRE(UNIT=funit, OPENED=isopen)
      IF (isopen) THEN
         OPEN(funit, FILE="res/"//TRIM(fname)//".dat", STATUS="replace", ACTION="write")
      ELSE
         OPEN(funit, FILE="res/"//TRIM(fname)//".dat", POSITION="append", ACTION="write")
      END IF

      min_val = MINVAL(field)

      PRINT '(A, F16.8, A, F16.8)', "(t = ", time, ") minimum temperature:", min_val
      WRITE(funit, '(3E12.4)') time, min_val
      CLOSE(funit)

      ! --------------------------------------------------------- !

   END SUBROUTINE diagnostics_double

END MODULE m_diagnostics


