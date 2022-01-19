PROGRAM ping_pong_mpi

    USE mpi
    IMPLICIT NONE

    INTEGER :: rank, ierror, i, n, m
    INTEGER :: status(MPI_STATUS_SIZE)
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: times, bandwidth, imsg
    DOUBLE PRECISION :: t1, t2

    INTEGER, PARAMETER :: wp = KIND(1.0D0)  ! working precision

    ! MPI
    CALL MPI_Init(ierror)
    CALL MPI_Comm_Rank(MPI_COMM_WORLD, rank, ierror)

    n = 10
    ALLOCATE(times(n))
    ALLOCATE(bandwidth(n))

    OPEN(10, FILE=TRIM("timings.dat"))
    WRITE(10, '(1x, A, 7(",", A))') "m", "mean(t)", "max(t)", "min(t)", &
                      "mean(bandwidth)", "max(bandwidth)", "min(bandwidth)"
    DO m = 1, 2**18, 50
        PRINT*, "m = ", m, "out of ", 2**18
        
        ALLOCATE(imsg(m))
        DO i = 1, m
            imsg(i) = 1.798391290239
        ENDDO


        DO i = 1, n
            t1 = MPI_Wtime()
            IF (rank.EQ.0) THEN
                ! (1) PING
                CALL MPI_SSend(imsg, m, MPI_DOUBLE_PRECISION, 1, 0, MPI_COMM_WORLD, ierror)
                !IF (ierror.EQ.0) THEN 
                !   PRINT *, "PINGED!"
                !ENDIF

                ! (4) PONG'ED
                CALL MPI_Recv(imsg, m, MPI_DOUBLE_PRECISION, 1, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierror)
                IF (status(MPI_SOURCE).EQ.1) THEN
                    !PRINT *, TRIM("PONG!")
                    !t2 = MPI_Wtime()
                    !times(i) = t2 - t1
                    !bandwidth(i) = (8.0_wp*m)/times(i)
                ENDIF

            ELSEIF (rank.EQ.1) THEN
                ! (2) PING'ED
                CALL MPI_Recv(imsg, m, MPI_DOUBLE_PRECISION, 0, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierror)
                !IF (status(MPI_SOURCE).EQ.0) THEN
                !    PRINT *, TRIM("PING!")
                !ENDIF
                ! (3) PONG
                CALL MPI_SSend(imsg, m, MPI_DOUBLE_PRECISION, 0, 0, MPI_COMM_WORLD, ierror)
                !IF (ierror.EQ.0) THEN
                !    PRINT *, "PONGED!"
                !ENDIF
            ENDIF
            t2 = MPI_Wtime()
            times(i) = t2 - t1
            bandwidth(i) = (8.0_wp*m)/times(i)
        ENDDO
        
        IF (rank.EQ.0) THEN        
            ! Prepare data saving
            WRITE(10, '(1x, E12.4, 7(",", E12.4))') 1.0_wp*m, SUM(times)/SIZE(times), MAXVAL(times), MINVAL(times), &
                SUM(bandwidth)/SIZE(bandwidth), MAXVAL(bandwidth), MINVAL(bandwidth)
        ENDIF

        ! DEALLOCATE imsg
        DEALLOCATE(imsg)
        
    ENDDO

    CLOSE(10)

    CALL MPI_Finalize(ierror)


END PROGRAM ping_pong_mpi