MODULE m_io
   ! This module contains the following subroutines:
   !  - continue(): gets terminal argument and imports field data if binary data file
   !  - read_input(): reads "input.txt" for inputs, or generates it with default values
   !  - extract_field(field, formatted, step, output): exports field data to output file
   !  - diagnostics(field, time, filename): prints minimum field value to terminal and to file

   USE m_precision
   USE m_global
   USE m_arrays

   IMPLICIT NONE

CONTAINS

   SUBROUTINE continue()
      ! continue() checks terminal arguments for a given binary input file
      ! which will allow continuation of previous simulation.

      INTEGER :: nargc, iostat, i
      CHARACTER(LEN = 24) :: arg_string, cfield, cstep
      LOGICAL :: arg_exists

      nargc = iargc()
      IF (nargc.GE.1) THEN

         ! get first argument (file-path)
         CALL getarg(1, arg_string)
         INQUIRE(FILE=arg_string, EXIST=arg_exists)

         ! check if the file exists
         IF (arg_exists) THEN
            PRINT "(A)", TRIM("CONTINUING FROM GIVEN FIELD INPUT:")
            i = INDEX(arg_string, "@")
            cfield = arg_string

            ! read binary field file
            PRINT "(A, A)", TRIM("    NAME: "), TRIM(cfield)
            OPEN(80, FILE=TRIM(arg_string), FORM="UNFORMATTED")
            READ(80, IOSTAT=iostat) fstep, field
            PRINT "(A, I8)", TRIM("    STEP: "), fstep
            IF (iostat.EQ.0) THEN
               PRINT "(A)", TRIM("    IMPORT SUCCESSFUL")
            ELSE
               PRINT "(A, I8)", TRIM("    IMPORT FAILED. ERROR CODE: "), iostat
               PRINT "(A)"
               PRINT*, "# ___________ PROGRAM EXITED ___________ #"
               STOP
            ENDIF

            ! if the file doens't exist, exit the program and complain
         ELSE
            PRINT "(A)", TRIM("GIVEN FIELD PATH IS INVALID!")
            PRINT "(A)"
            PRINT*, "# ___________ PROGRAM EXITED ___________ #"
            STOP

         ENDIF

      ENDIF
   END SUBROUTINE continue

   SUBROUTINE read_input()
      ! read_input() checks if "inputs.txt" exists; 
      ! if it does, read it. Otherwise, generate
      ! and exit.

      INTEGER  :: iostat
      LOGICAL  :: file_exists

      ! namelists
      NAMELIST /SIMULATION/ nx, ny, lx, ly, dt, diff, fstep, nsteps, temp_init, temp_boundary
      NAMELIST /DATA_EXTRACTION/ diag_freq, bin_freq
      NAMELIST /LOGICALS/ save_field, save_bin, save_diag, benchmark
      NAMELIST /FILENAMES/ field_out, bin_out, diag_out, bench_out

      ! check if file exists
      INQUIRE(FILE=input_file, EXIST=file_exists)

      ! if it does, read it
      IF (file_exists) THEN

         PRINT "(A)", TRIM("Input file detected.")

         ! read input file
         OPEN(UNIT=99, FILE=input_file, IOSTAT=iostat)
         CALL check_iostat(iostat)
         IF (iostat.EQ.0) THEN
            READ(UNIT=99, NML=SIMULATION)
            READ(UNIT=99, NML=DATA_EXTRACTION)
            READ(UNIT=99, NML=LOGICALS)
            READ(UNIT=99, NML=FILENAMES)
         ELSE
            PRINT*, TRIM("ERROR OPENING INPUT FILE | CODE: "), iostat
            CLOSE(99)
         ENDIF

         CLOSE(99)
         ! if not, generate "input.txt" with default values
      ELSE

         PRINT "(A)", TRIM("No input file detected. Generating default input file.")

         ! generate input file
         OPEN(UNIT=99, FILE=input_file, IOSTAT=iostat)
         CALL check_iostat(iostat)
         IF (iostat.EQ.0) THEN
            WRITE(UNIT=99, NML=SIMULATION)
            WRITE(UNIT=99, NML=DATA_EXTRACTION)
            WRITE(UNIT=99, NML=LOGICALS)
            WRITE(UNIT=99, NML=FILENAMES)
            PRINT "(A)", TRIM("Check input file, and re-run the executable.")
            PRINT*, "# ___________ PROGRAM EXITED ___________ #"
            STOP
         ELSE
            PRINT*, TRIM("ERROR OPENING INPUT FILE | CODE: "), iostat
         ENDIF
         CLOSE(99)

      ENDIF

      ! compute dependent parameters
      dx = lx / REAL(nx - 1)
      dy = ly / REAL(ny - 1)
      rdx2 = 1/(dx ** 2)
      rdy2 = 1/(dy ** 2)

      ! check if dt is lower than the Fourier limit
      IF (dt.GT.MIN(dx, dy) ** 2.0_wp / (4.0_wp*diff).OR.dt.EQ.0.0_wp) THEN
         dt = MIN(dx, dy) ** 2.0_wp / (4.0_wp*diff)
         PRINT "(A, F10.8)", &
            TRIM("WARNING! Input time-step size (dt) is lower than Fourier limit or equal zero!&
         &Time-step size forced to the Fourier limit: dt = "), dt
      ENDIF

   END SUBROUTINE read_input


   SUBROUTINE extract_field(field, formatted, step, output)
      ! extract_field(field, formatted, step, output) takes
      ! a given field, and outputs it either as a formatted
      ! or unformatted (ascii or bin), depending on the
      ! Boolean parameter "formatted". The filename is given
      ! by "step" and "output" parameters as
      ! output@step.{dat/bin}, depending on formatting.

      REAL(wp), DIMENSION(:,:), INTENT(IN)       :: field
      LOGICAL                                    :: formatted
      INTEGER, OPTIONAL                          :: step
      CHARACTER(LEN = 48), INTENT(IN), OPTIONAL  :: output

      ! local variables
      CHARACTER(LEN = 48)  :: string, filename
      INTEGER              :: i, j, iostat

      ! if output name given, set as filename.
      ! else, set default "field"
      IF (PRESENT(output)) THEN
         filename = output
      ELSE
         filename = "field"
      ENDIF

      ! generate full filename string
      IF (PRESENT(step)) THEN
         WRITE(string, '(A,A,A,(I6.6))') "./res/", TRIM(filename), '@', step
      ELSE
         WRITE(string, '(A,A)') "./res/", TRIM(filename)
      ENDIF

      ! open file
      IF (formatted) THEN
         ! create formatted file
         PRINT *, "Saving temperature field data to file: ", TRIM(string)//".dat"
         OPEN(10, FILE=TRIM(string)//".dat", IOSTAT=iostat)
         CALL check_iostat(iostat)

         ! write 3 columns to file (xpos, ypos, fieldval)
         DO j = 1, ny
            DO i = 1, nx
               WRITE(10, '(3E12.4)') REAL(i - 1) * dx, REAL(j - 1) * dy, field(i, j)
            ENDDO
            WRITE(10, '(A)')
         ENDDO
      ELSE
         ! create unformatted file
         PRINT *, "Saving temperature field data to file: ", TRIM(string)//".bin"
         OPEN(10, FILE=TRIM(string)//".bin", FORM='UNFORMATTED', IOSTAT=iostat)
         CALL check_iostat(iostat)

         ! write binary file
         WRITE(10) step, field
      ENDIF
      CLOSE(10)

   END SUBROUTINE extract_field


   SUBROUTINE diagnostics(field, time, filename)
      ! diagnostics(field, time, filename) prints a
      ! statement with the field minimum value to track
      ! the diffusion progress, and saves it to
      ! a diagnostic ascii file.

      ! input variables
      REAL(wp), DIMENSION(:,:), INTENT(IN)   :: field
      REAL(wp), INTENT(IN)                   :: time
      CHARACTER(LEN = *), INTENT(IN)         :: filename

      ! local variables
      REAL    :: min_val
      LOGICAL :: isopen

      ! check if file already exists
      INQUIRE(UNIT=450, OPENED=isopen)
      IF (isopen) THEN
         OPEN(450, FILE="res/"//TRIM(filename)//".dat", STATUS="replace", ACTION="write")
      ELSE
         OPEN(450, FILE="res/"//TRIM(filename)//".dat", POSITION="append", ACTION="write")
      END IF

      min_val = MINVAL(field)

      PRINT '(A, F16.8, A, F16.8)', TRIM("| t = "), time, TRIM(") minimum value:"), min_val
      WRITE(450, '(3E12.4)') time, min_val
      CLOSE(450)

   END SUBROUTINE diagnostics

END MODULE m_io
