PROGRAM monte_pirlo
   IMPLICIT NONE

   INTEGER, PARAMETER :: wp = KIND(1.0D0) ! double working precision

   INTEGER :: n, istat, rank, i, counter, sys_count, sys_count_rate
   INTEGER, DIMENSION(:), ALLOCATABLE :: seed
   REAL(wp), DIMENSION(:), ALLOCATABLE :: posx, posy
   REAL(wp) :: rand, dist, t1, t2

   CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
   t1 = sys_count * 1.0 / sys_count_rate

   n = 40000000

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

   rank = 0
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

   CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
   t2 = sys_count * 1.0 / sys_count_rate

   PRINT *, "n  = ", n
   PRINT *, "pi = ", counter / REAL(n, wp) * 4.0_wp
   PRINT *, "t  = ", t2 - t1

   DEALLOCATE(seed)
   DEALLOCATE(posx)
   DEALLOCATE(posy)

END PROGRAM monte_pirlo
