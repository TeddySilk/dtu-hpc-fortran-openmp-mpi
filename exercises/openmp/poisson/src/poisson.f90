PROGRAM poisson

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_init
   USE m_timer
   USE m_global
   USE m_input
   USE m_extract
   USE m_solver

   ! ------------------------------------------------- !
   ! VARIABLES                                         !
   ! ------------------------------------------------- !
   IMPLICIT NONE

   REAL, DIMENSION(:, :, :), ALLOCATABLE :: field, previous_field
   REAL, DIMENSION(:, :, :), ALLOCATABLE :: source

   INTEGER :: info
   REAL :: cpu_t1, cpu_t2, wall_t1, wall_t2

   ! ------------------------------------------------- !
   ! INITIALIZATION                                    !
   ! ------------------------------------------------- !
   CALL start_timer(cpu_t1, wall_t1)

   ! read input file
   CALL read_input(input_file, info)
   IF (info.EQ.1) THEN
      PRINT "(A)", TRIM("Input file detected.")
   ELSE
      PRINT "(A)", TRIM("No input file detected. Generating default input file.")
      PRINT "(A)", TRIM("Check input file, and re-run the executable.")
      PRINT *, "# ___________ PROGRAM EXITED ___________ #"
      STOP
   ENDIF

   ! allocate two fields for the Jacobi iteration
   CALL alloc(field, nx, ny, nz, info)
   CALL alloc(previous_field, nx, ny, nz, info)

   ! initialize the temperature field with Dirilect BCs
   CALL init(previous_field, type=source_type, source=.FALSE.)
   CALL init(field, type=source_type, source=.FALSE.)

   ! allocate and initialize source field
   CALL alloc(source, nx, ny, nz, info)
   CALL init(source, type=source_type, source=.TRUE.)

   ! ------------------------------------------------- !
   ! SOLVE                                             !
   ! ------------------------------------------------- !
   CALL solver(field, previous_field, source)

   CALL stop_timer(cpu_t1, cpu_t2, wall_t1, wall_t2)
   !CALL extract_field(field, output_file)

END PROGRAM poisson
