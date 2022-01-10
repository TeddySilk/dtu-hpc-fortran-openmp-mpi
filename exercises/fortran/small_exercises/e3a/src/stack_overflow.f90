PROGRAM stack_overflow

    IMPLICIT NONE

    REAL, DIMENSION(:, :), ALLOCATABLE          :: a, b
    INTEGER                                     :: n, m, istat, i, j

    n = 30000
    m = n

    ALLOCATE(a(n, m), STAT=istat)
    print*, "STATUS = ", istat

    ALLOCATE(b(n, m), STAT=istat)
    print*, "STATUS = ", istat

    DO j = 1, m
        DO i = 1, n
            a(i, j) = 0.0
            b(i, j) = 1.0
        ENDDO
    ENDDO

    print*, "SIZE(a) = ", SIZE(a)
    print*, "SIZE(b) = ", SIZE(b)

    CALL swap(a, b)
    print*, "a(1,1) = ", a(1, 1)
    print*, "b(1,1) = ", b(1, 1)


CONTAINS

    SUBROUTINE swap(a, b)

        ! input variables
        REAL, DIMENSION(:, :), INTENT(INOUT)    :: a, b

        ! local work array
        REAL, DIMENSION(:, :), ALLOCATABLE      :: w
        INTEGER, DIMENSION(2)                   :: s
        INTEGER                                 :: istat

        s = SHAPE(a)
        ALLOCATE(w(s(1), s(2)), STAT=istat)

        ! swap
        w = a
        a = b
        b = w

        DEALLOCATE(w, STAT=istat)

    END SUBROUTINE


END PROGRAM stack_overflow