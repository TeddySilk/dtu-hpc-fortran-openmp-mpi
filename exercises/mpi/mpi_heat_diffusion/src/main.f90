PROGRAM main
   ! ------------------------------------------------- !
   !  Filename : diff2d                                !
   !  Version  : 0.6                                   !
   !  Author   : Kristian Ebstrup Jacobsen             !
   !  Created  : January 17, 2022                       !
   ! ------------------------------------------------- !

   USE m_precision
   USE m_global
   USE m_init
   USE m_io
   USE m_diffuse
   USE m_timer
   IMPLICIT NONE


   ! initialize time variables
   REAL(wp)     :: cpu_t1, cpu_t2, wall_t1, wall_t2

   INTEGER      :: k, nk, iostat

   ! ------------------------------------------------- !
   ! INITIALIZATION                                    !
   ! ------------------------------------------------- !
   PRINT*, "# ___________ PROGRAM STARTED ___________ #"
   PRINT "(A)"

   ! read input file
   CALL read_input()

   ! check for terminal inputs to continue field
   CALL continue()

   IF (benchmark) THEN
      nk = 10
   ELSE
      nk = 1
   ENDIF

   DO k = 1, nk
      ! start timer
      CALL start_timer(cpu_t1, wall_t1)

      ! allocate and initialize field
      CALL alloc(field, nx, ny)
      CALL init_field(field, temp_boundary, temp_init)

      ! ------------------------------------------------- !
      ! SIMULATION                                        !
      ! ------------------------------------------------- !
      CALL diffuse(field, nsteps, fstep)

      ! end timer
      CALL stop_timer(cpu_t1, cpu_t2, wall_t1, wall_t2)
   ENDDO

   ! ------------------------------------------------- !
   ! EXTRACT FIELD(S)                                  !
   ! ------------------------------------------------- !
   IF (save_field) THEN
      CALL extract_field(field, formatted=.TRUE., step=nsteps+fstep, output=field_out)
   ENDIF
   IF (save_bin) THEN
      CALL extract_field(field, formatted=.FALSE., step=nsteps+fstep, output=bin_out)
   ENDIF

   PRINT "(A)"
   PRINT*, "# ___________ PROGRAM SUCCESSFUL ___________ #"

END PROGRAM main

