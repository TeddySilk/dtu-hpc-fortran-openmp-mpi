MODULE m_global

   USE m_precision
   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! INPUT FILE                                        !
   ! ------------------------------------------------- !
   CHARACTER(LEN = 24)   :: input_file = 'inputs.txt'

   ! ------------------------------------------------- !
   ! INPUTS                                            !
   ! ------------------------------------------------- !

   ! SIMULATION
   INTEGER     :: nx               = 21         ! nx: amount of cells in x-dir,
   INTEGER     :: ny               = 21         ! ny: amount of cells in y-dir
   INTEGER     :: lx               = 1          ! lx: length in x-dir
   INTEGER     :: ly               = 1          ! ly: length in y-dir
   INTEGER     :: diff             = 1          ! diff: diffusion constant
   INTEGER     :: fstep            = 0          ! fstep: first step 
   INTEGER     :: nsteps           = 200        ! nsteps: amount of time steps made
   REAL(wp)    :: temp_init        = 0.0_wp     ! temp_init: initial field temperature
   REAL(wp)    :: temp_boundary    = 1.0_wp     ! temp_boundary: Dirilect-BC wall temperature

   ! DATA EXTRACTION
   INTEGER :: diag_freq = 10                       ! diagfreq: amount of time steps made per diagnostic print-out
   INTEGER :: bin_freq  = 100                      ! binfreq: amount of time steps made per binary backup

   ! LOGICALS
   LOGICAL :: save_field   = .TRUE.
   LOGICAL :: save_bin     = .TRUE.
   LOGICAL :: save_diag    = .TRUE.
   LOGICAL :: benchmark    = .FALSE.

   ! FILENAMES
   CHARACTER(LEN = 48)    :: field_out = "field"       ! output_file: Name of the formatted field data file
   CHARACTER(LEN = 48)    :: bin_out   = "field"     ! bin_out: Name of the binary field data file
   CHARACTER(LEN = 48)    :: diag_out  = "diag"        ! diag_out: Name of the diagnostics file
   CHARACTER(LEN = 48)    :: bench_out = "runtimes"    ! bench_out: Name of the runtimes data file

   ! INPUT-DEPENDANT PARAMETERS
   REAL(wp) :: dx          ! dx: cell width in the x-dir
   REAL(wp) :: dy          ! dy: cell width in the y-dir
   REAL(wp) :: rdx2        ! rdx2: 1/(dx ** 2), to aid setting up the equations
   REAL(wp) :: rdy2        ! rdy2: see above
   REAL(wp) :: dt          ! dt: time-step size, satisfying the Fourier limit

   ! ------------------------------------------------- !
   ! FIELD ARRAY                                       !
   ! ------------------------------------------------- !
   REAL(wp), DIMENSION(:,:), ALLOCATABLE :: field


END MODULE


