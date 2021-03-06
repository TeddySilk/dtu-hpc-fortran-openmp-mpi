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
   INTEGER              :: nx, ny, nz                ! cells per direction
   INTEGER              :: lx, ly, lz                ! box dimensions
   INTEGER              :: nsteps                    ! integration steps
   DOUBLE PRECISION                 :: eps_min                   ! threshold on error value
   !DOUBLE PRECISION                :: Tinit                     ! Tinit: initial field temperature
   !DOUBLE PRECISION                :: Tboundary                 ! Tboundary: Dirilect-boundary field temperature
   CHARACTER(LEN = 12)  :: source_type               ! source type, supports "radiator", "sinusoidal"

   ! ------------------------------------------------- !
   ! DATA EXTRACT PARAMETERS                           !
   ! ------------------------------------------------- !
   CHARACTER(LEN = 24)     :: output_file            ! output_file: Name of the field data output file
   !CHARACTER(LEN = 24)    :: binary_file            ! binary_file: Name of the field data binary backup file (for perfect continuation)
   !CHARACTER(LEN = 24)    :: diagnostic_file        ! diagnostic_file: Name of the diagnostic file
   !INTEGER                :: diagnostic_unit        ! diagnostic_file_unit: File-unit for the diagnostic file

   ! ------------------------------------------------- !
   ! DEPENDENT PARAMETERS                              !
   ! ------------------------------------------------- !
   DOUBLE PRECISION      :: dx, dy, dz  ! dx: cell width
   
END MODULE

