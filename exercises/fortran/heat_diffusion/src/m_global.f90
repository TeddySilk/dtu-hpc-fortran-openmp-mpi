MODULE m_global

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! INPUT FILE                                        !
   ! ------------------------------------------------- !
   CHARACTER(LEN = 24)   :: input_file = 'inputs.txt'

   ! ------------------------------------------------- !
   ! INPUT PARAMETERS                                  !
   ! ------------------------------------------------- !
   INTEGER   :: Nx, Ny                          ! nx: amount of cells in x-dir, ny: amount of cells in y-dir
   INTEGER   :: Lx, Ly                          ! box dimensions (Lx: length in x-dir, Ly: length in y-dir)
   INTEGER   :: diff_const                      ! D: diffusion constant
   INTEGER   :: nsteps                          ! nsteps: amount of time steps made
   INTEGER   :: diagfreq                        ! diagfreq: amount of time steps made per diagnostic print-out
   LOGICAL   :: save_bin                        ! save_bin: .TRUE.: binary backups of the field is made
   INTEGER   :: binfreq                         ! binfreq: amount of time steps made per binary backup
   REAL      :: Tinit                           ! Tinit: initial field temperature
   REAL      :: Tboundary                       ! Tboundary: Dirilect-boundary field temperature

   ! ------------------------------------------------- !
   ! DATA EXTRACT PARAMETERS                           !
   ! ------------------------------------------------- !
   CHARACTER(LEN = 24)    :: output_file            ! output_file: Name of the field data output file
   CHARACTER(LEN = 24)    :: binary_file            ! binary_file: Name of the field data binary backup file (for perfect continuation)
   CHARACTER(LEN = 24)    :: diagnostic_file        ! diagnostic_file: Name of the diagnostic file
   INTEGER                :: diagnostic_unit        ! diagnostic_file_unit: File-unit for the diagnostic file

   ! ------------------------------------------------- !
   ! DEPENDENT PARAMETERS                              !
   ! ------------------------------------------------- !
   REAL      :: dx          ! dx: cell width in the x-dir
   REAL      :: dy          ! dy: cell width in the y-dir
   REAL      :: rdx2        ! rdx2: 1/(dx ** 2), to aid setting up the equations
   REAL      :: rdy2        ! rdy2: see above
   REAL      :: dt          ! dt: time-step size, satisfying the Fourier limit


END MODULE

