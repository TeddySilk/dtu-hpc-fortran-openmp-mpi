PROGRAM main
   ! ------------------------------------------------- !
   !  Filename : heat_diffusion_2d                     !
   !  Version  : 0.5                                   !
   !  Author   : Kristian Ebstrup Jacobsen             !
   !  Created  : January 8, 2022                       !
   ! ------------------------------------------------- !

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global
   USE m_read_input
   USE m_alloc
   USE m_extract_field
   USE m_extract_binary
   USE m_init_tfield
   USE m_simulate_diffusion

   ! ------------------------------------------------- !
   ! INITIALIZATION                                    !
   ! ------------------------------------------------- !
   IMPLICIT NONE

   ! initialize relevant variables
   INTEGER              :: info, nargc, i, step = 0
   CHARACTER(LEN = 24)  :: arg_string, cfield, cstep
   LOGICAL              :: arg_exists

   ! empty array to contain the temperature field
   REAL, DIMENSION(:,:), ALLOCATABLE :: tfield

   PRINT*, "# ___________ PROGRAM STARTED ___________ #"
   PRINT "(A)"

   ! read input file
   CALL read_input(input_file, info)
   IF (info.EQ.1) THEN
      PRINT "(A)", TRIM("Input file detected.")
   ELSE
      PRINT "(A)", TRIM("No input file detected. Generating default input file.")
      PRINT "(A)", TRIM("Check input file, and re-run the executable.")
      PRINT*, "# ___________ PROGRAM EXITED ___________ #"
      STOP
   ENDIF

   ! allocate memory to the global field
   CALL alloc(tfield, Nx, Ny, info)

   ! initialize the temperature field according to BC
   CALL init_tfield(tfield, boundary_temperature = Tboundary, initial_temperature = Tinit)

   ! ------------------------------------------------- !
   ! INPUT FIELD                                       !
   ! ------------------------------------------------- !
   ! if the path to a binary file for a field is given as a command argument,
   ! continue from that field.
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
         READ(80, IOSTAT=info) step, tfield
         PRINT "(A, I8)", TRIM("    STEP: "), step
         IF (info.EQ.0) THEN
            PRINT "(A)", TRIM("    IMPORT SUCCESSFUL")
         ELSE
            PRINT "(A, I8)", TRIM("    IMPORT FAILED. ERROR CODE: "), info
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

   ! ------------------------------------------------- !
   ! HEAT DIFFUSION                                    !
   ! ------------------------------------------------- !
   CALL simulate_diffusion(tfield, nsteps, input_step = step, verbose = 0, binary_backup = save_bin)

   ! ------------------------------------------------- !
   ! EXTRACT FIELD(S)                                  !
   ! ------------------------------------------------- !
   CALL extract_field(tfield, output_file, nsteps)
   IF (save_bin) THEN
      CALL extract_binary(tfield, binary_file, nsteps)
   ENDIF

   PRINT "(A)"
   PRINT*, "# ___________ PROGRAM SUCCESSFUL ___________ #"

END PROGRAM main
