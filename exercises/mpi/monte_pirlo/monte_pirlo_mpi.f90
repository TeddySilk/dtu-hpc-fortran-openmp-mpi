PROGRAM monte_pirlo

   USE mpi
   IMPLICIT NONE

   INTEGER, PARAMETER :: wp = KIND(1.0D0) ! double working precision

   INTEGER :: n, ntotal, ncheck, istat, np, rank, i, counter, ierror, ccount
   INTEGER :: status(MPI_STATUS_SIZE)
   INTEGER, DIMENSION(:), ALLOCATABLE :: seed
   REAL(wp), DIMENSION(:), ALLOCATABLE :: posx, posy, times
   REAL(wp) :: rand, dist, t1, t2

   CALL MPI_Init(ierror)
   CALL MPI_Comm_Rank(MPI_COMM_WORLD, rank, ierror)
   CALL MPI_Comm_Size(MPI_COMM_WORLD, np, ierror)

   ntotal = 40000000   ! total points

   ! ntotal divided onto processors
   n = ntotal / np

   IF (rank.EQ.0) THEN
      t1 = MPI_WTime()
   ENDIF

   ! allocate arrays
   ALLOCATE(seed(n * 2), STAT=istat)
   IF (istat.NE.0) THEN
      PRINT *, "Warning. seed array not allocated."
   ENDIF
   ALLOCATE(posx(n), STAT=istat)
   IF (istat.NE.0) THEN
      PRINT *, "Warning. posx array not allocated."
   ENDIF
   ALLOCATE(posy(n), STAT=istat)
   IF (istat.NE.0) THEN
      PRINT *, "Warning. posy array not allocated."
   ENDIF

   ! create seeds
   DO i = 1, n * 2
      seed(i) = 123919 + rank*8365
   ENDDO

   ! distribute points
   CALL RANDOM_SEED(PUT=seed)
   DO i = 1, n
      CALL RANDOM_NUMBER(rand)
      posx(i) = rand
      CALL RANDOM_NUMBER(rand)
      posy(i) = rand
   ENDDO

   ! count points inside the circle
   counter = 0
   DO i = 1, n
      dist = (posx(i)**2_wp + posy(i)**2_wp)**0.5_wp
      IF (dist.LE.1.0_wp) THEN
         counter = counter + 1
      ENDIF
   ENDDO

   CALL MPI_Reduce(counter, ccount, 1, MPI_INTEGER,&
        MPI_Sum, 0, MPI_COMM_WORLD, ierror)

   IF (rank.EQ.0) THEN
      t2 = MPI_Wtime()
      PRINT *, "n  = ", ntotal
      PRINT *, "pi = ", ccount / REAL(ntotal, wp) * 4.0_wp
      PRINT *, "t  = ", t2 - t1
   ENDIF

   DEALLOCATE(seed)
   DEALLOCATE(posx)
   DEALLOCATE(posy)

   CALL MPI_Finalize(ierror)

END PROGRAM monte_pirlo
