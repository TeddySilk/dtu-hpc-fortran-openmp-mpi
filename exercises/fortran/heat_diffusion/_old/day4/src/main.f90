PROGRAM main
   ! ------------------------------------------------- !
   !  Filename : heat_diffusion_2d                     !
   !  Version  : 0.4                                   !
   !  Author   : Kristian Ebstrup Jacobsen             !
   !  Created  : January 7, 2022                       !
   ! ------------------------------------------------- !

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global                     ! contains global-scoped parameters
   USE m_arrays, ONLY : alloc       ! contains array manipulation functions
   USE m_io, ONLY : extract_field   ! contains input / output functions
   USE m_heat_diffusion             ! contains heat diffusion time simulation and field initialization

   ! ------------------------------------------------- !
   ! INITIALIZATION                                    !
   ! ------------------------------------------------- !
   IMPLICIT NONE

   ! initialize info
   INTEGER :: info

   ! empty array to contain the temperature field
   REAL, DIMENSION(:,:), ALLOCATABLE :: tfield

   ! allocate memory to the global field
   CALL alloc(tfield, Nx, Ny, info)

   ! initialize the temperature field according to BC
   CALL init_tfield(tfield, initial_temperature = 0.0)

   ! ------------------------------------------------- !
   ! HEAT DIFFUSION                                    !
   ! ------------------------------------------------- !
   CALL simulate_diffusion(tfield, nsteps, verbose = 0)

   ! ------------------------------------------------- !
   ! EXTRACT FIELD(S)                                  !
   ! ------------------------------------------------- !
   CALL extract_field(tfield, output_file)

END PROGRAM main
