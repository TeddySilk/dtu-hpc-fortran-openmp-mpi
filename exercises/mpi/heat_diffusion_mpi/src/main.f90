PROGRAM main
   ! ------------------------------------------------- !
   !  Filename : diff2d_mpi.e                          !
   !  Version  : 0.7                                   !
   !  Author   : Kristian Ebstrup Jacobsen             !
   !  Created  : January 22, 2022                      !
   ! ------------------------------------------------- !

   USE mpi
   USE m_precision
   USE m_global
   USE m_init
   USE m_io
   USE m_diffuse
   USE m_timer
   IMPLICIT NONE

   ! define local field
   REAL(wp), DIMENSION(:,:), ALLOCATABLE :: lfield

   ! initialize time variables
   REAL(wp)     :: cpu_t1, cpu_t2, wall_t1, wall_t2
   INTEGER      :: k, nk, iostat, bctype

   ! MPI variables
   INTEGER :: np, rank, ierror         ! number of processors, processor rank, and error integer
   INTEGER :: nxl, nyl                 ! processor-local nx and ny
   INTEGER :: status(MPI_STATUS_SIZE)

   ! ------------------------------------------------- !
   ! MPI START                                         !
   ! ------------------------------------------------- !
   CALL MPI_Init(ierror)
   CALL check_iostat(ierror)

   CALL MPI_Comm_Rank(MPI_COMM_WORLD, rank, ierror)
   CALL check_iostat(ierror)

   CALL MPI_Comm_Size(MPI_COMM_WORLD, np, ierror)
   CALL check_iostat(ierror)

   ! ------------------------------------------------- !
   ! INITIALIZATION                                    !
   ! ------------------------------------------------- !
   IF (rank.EQ.0) THEN
      PRINT*, "# ___________ PROGRAM STARTED ___________ #"
      PRINT "(A)"
   ENDIF

   ! read input file
   CALL read_input()

   ! check for terminal inputs to continue field
   ! CALL continue()

   IF (benchmark) THEN
      nk = 10
   ELSE
      nk = 1
   ENDIF


   ! subdivide field sizes
   nxl = nx / np
   nyl = ny

   ! last subdivision handles remainder
   IF (rank.EQ.(np-1)) THEN
      nxl = nx - nxl*(np-1)
   ENDIF

   ! subdivison boundary conditions
   IF (np.GT.1) THEN
      IF (rank.EQ.0) THEN
         bctype = 1     ! Left, Top, Bottom Boundaries
      ELSEIF (rank.EQ.(np-1)) THEN
         bctype = 2     ! Right, Top, Bottom Boundaries
      ELSE
         bctype = 3     ! Top, Bottom Boundaries
      ENDIF
   ELSE
      bctype = 0        ! Left, Right, Top Bottom Boundaries
   ENDIF

   DO k = 1, nk
      IF (rank.EQ.0) THEN
         ! start timer
         wall_t1 = MPI_WTime()

         ! allocate global field
         CALL alloc(field, nx, ny)
      ENDIF

      ! allocate and initialize local field
      CALL alloc(lfield, nxl, nyl)
      PRINT *, "bctype = ", bctype
      CALL init_field(lfield, temp_boundary, temp_init, bctype)

      ! ------------------------------------------------- !
      ! SIMULATION                                        !
      ! ------------------------------------------------- !
      CALL diffuse(lfield, nsteps, nxl, nyl, fstep, rank, np)

      IF (rank.EQ.0) THEN
         ! end timer
         wall_t2 = MPI_WTime()
         PRINT*, "elapsed wall clock time:", wall_t2 - wall_t1
      ENDIF
   ENDDO

   ! combine into global field
   IF (rank.EQ.0) THEN
      field(1:nxl,:) = lfield
      DO k = 1, (np-1)
         IF (k.NE.(np-1)) THEN
            PRINT *, "M INPUT FIELD SIZE =", SIZE(field((1 + nxl * k):(nxl * (k+1)),:))
            PRINT *, "M EXPECTED SIZE =", nxl*nyl
            CALL MPI_Recv(field(:,(1 + nxl * k):(nxl * (k+1))), nxl*nyl,&
               MPI_DOUBLE_PRECISION, k, 2, MPI_COMM_WORLD, status, ierror)
               PRINT *, "ierror MPI_Recv1 = ", ierror
         ELSE
            PRINT *, "E INPUT FIELD SIZE =", SIZE(field((1 + (nx / np) * k):nx,:))
            PRINT *, "E EXPECTED SIZE =", (nx-nxl*(k))*nyl
            CALL MPI_Recv(field((1 + (nx / np) * k):nx,:), (nx-nxl*(np-1))*nyl,&
               MPI_DOUBLE_PRECISION, k, 2, MPI_COMM_WORLD, status, ierror)
               PRINT *, "ierror MPI_Recv2 = ", ierror
         ENDIF
      ENDDO
   ELSE
      CALL MPI_Send(lfield, SIZE(lfield), MPI_DOUBLE_PRECISION, 0, 2, MPI_COMM_WORLD, ierror)
      PRINT *, "lfield size = ", SIZE(lfield)
      PRINT *, "ierror MPI_Send = ", ierror
   ENDIF

   ! ------------------------------------------------- !
   ! EXTRACT FIELD(S)                                  !
   ! ------------------------------------------------- !
   IF (rank.EQ.0) THEN
      IF (save_field) THEN
         CALL extract_field(field, formatted=.TRUE., step=nsteps+fstep, output=field_out)
      ENDIF
      IF (save_bin) THEN
         CALL extract_field(field, formatted=.FALSE., step=nsteps+fstep, output=bin_out)
      ENDIF

      PRINT "(A)"
      PRINT*, "# ___________ PROGRAM SUCCESSFUL ___________ #"
   ENDIF

   ! ------------------------------------------------- !
   ! MPI END                                           !
   ! ------------------------------------------------- !
   CALL MPI_Finalize(ierror)

END PROGRAM main

