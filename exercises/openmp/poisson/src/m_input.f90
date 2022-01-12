MODULE m_input

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global

   IMPLICIT NONE

CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE: READ_INPUT                            !
   ! ------------------------------------------------- !
   SUBROUTINE read_input(input_file, iostate)
      ! input variables
      CHARACTER(LEN = *), INTENT(IN) :: input_file
      INTEGER, INTENT(INOUT) :: iostate

      ! local variables
      LOGICAL            :: input_exists

      ! list of inputs
      NAMELIST /GLOBAL/ nx, ny, nz, lx, ly, lz, nsteps, source_type, output_file

      INQUIRE(FILE=input_file, EXIST=input_exists)
      IF (input_exists) THEN
         ! set default values
         nx = 21
         ny = 21
         nz = 21
         lx = 2
         ly = 2
         lz = 2
         !dt = 1.0
         !diff_const = 1
         nsteps = 200
         !diagfreq = 10
         !binfreq = 100
         !Tinit = 0.0
         !Tboundary = 1.0
         source_type = "radiator"

         !save_bin = .TRUE.

         output_file = 'Tfield'
         !binary_file = output_file
         !diagnostic_file = 'diag'
         !diagnostic_unit = 20

         ! read input file
         OPEN(UNIT=99, FILE=input_file)
         READ(UNIT=99, NML=GLOBAL)
         CLOSE(99)

         iostate = 1
      ELSE
         ! set default values
         nx = 21
         ny = 21
         nz = 21
         lx = 2
         ly = 2
         lz = 2
         !dt = 1.0
         !diff_const = 1
         nsteps = 200
         !diagfreq = 10
         !binfreq = 100
         !Tinit = 0.0
         !Tboundary = 1.0
         source_type = "radiator"

         !save_bin = .TRUE.

         output_file = 'Tfield'
         !binary_file = output_file
         !diagnostic_file = 'diag'
         !diagnostic_unit = 20

         ! write input file
         OPEN(UNIT=99, FILE=input_file)
         WRITE(UNIT=99, NML=GLOBAL)
         CLOSE(99)

         iostate = 0

      ENDIF

      ! set dependent parameters
      dx = lx / REAL(nx - 1)
      dy = ly / REAL(ny - 1)
      dz = lz / REAL(nz - 1)

   END SUBROUTINE

END MODULE m_input
