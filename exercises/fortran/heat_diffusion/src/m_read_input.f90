MODULE m_read_input

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
      NAMELIST /GLOBAL/ Nx, Ny, Lx, Ly, diff_const, dt, nsteps, diagfreq, binfreq, Tinit, Tboundary,&
         save_bin, output_file, binary_file, diagnostic_file, diagnostic_unit

      INQUIRE(FILE=input_file, EXIST=input_exists)
      IF (input_exists) THEN
         ! set default values
         Nx = 21
         Ny = 21
         Lx = 1
         Ly = 1
         dt = 1.0
         diff_const = 1
         nsteps = 200
         diagfreq = 10
         binfreq = 100
         Tinit = 0.0
         Tboundary = 1.0

         save_bin = .TRUE.

         output_file = 'Tfield'
         binary_file = output_file
         diagnostic_file = 'diag'
         diagnostic_unit = 20

         ! read input file
         OPEN(UNIT=99, FILE=input_file)
         READ(UNIT=99, NML=GLOBAL)
         CLOSE(99)

         iostate = 1
      ELSE
         ! set default values
         Nx = 21
         Ny = 21
         Lx = 1
         Ly = 1
         dt = 1.0
         diff_const = 1
         nsteps = 200
         diagfreq = 10
         binfreq = 100
         Tinit = 0.0
         Tboundary = 1.0

         save_bin = .TRUE.

         output_file = 'Tfield'
         binary_file = output_file
         diagnostic_file = 'diag'
         diagnostic_unit = 20

         ! generate input file
         OPEN(UNIT=99, FILE=input_file)
         WRITE(UNIT=99, NML=GLOBAL)
         CLOSE(99)

         iostate = 0

      ENDIF

      ! set dependent parameters
      dx = Lx / REAL(Nx - 1)
      dy = Ly / REAL(Ny - 1)
      rdx2 = 1/(dx ** 2)
      rdy2 = 1/(dy ** 2)
      IF (dt.GT.MIN(dx, dy) ** 2 / (4*diff_const)) THEN
         dt = MIN(dx, dy) ** 2 / (4*diff_const)
         PRINT "(A, F10.8)", TRIM("WARNING! Input time-step size (dt) is lower than Fourier limit!&
                             & Time-step size forced to the Fourier limit: dt= "), dt
      ENDIF

   END SUBROUTINE

END MODULE m_read_input
