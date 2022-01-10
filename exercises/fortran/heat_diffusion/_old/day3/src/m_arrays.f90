MODULE m_arrays

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE alloc
      MODULE PROCEDURE alloc_single, alloc_double
   END INTERFACE

   INTERFACE copy_arrays
      MODULE PROCEDURE copy_arrays_single, copy_arrays_double
   END INTERFACE

   INTERFACE swap
      MODULE PROCEDURE swap_single, swap_double
   END INTERFACE


CONTAINS


   ! ------------------------------------------------- !
   ! SUBROUTINE: ALLOC                                 !
   ! ------------------------------------------------- !
   SUBROUTINE alloc_single(c, n, m, info)

      ! input variables
      REAL, DIMENSION(:, :), ALLOCATABLE     :: c
      INTEGER, INTENT(IN)                    :: n, m
      INTEGER, INTENT(INOUT)                 :: info

      ! local variables
      INTEGER, DIMENSION(2)                  :: s
      REAL, DIMENSION(:, :), ALLOCATABLE     :: w

      s = SHAPE(c)

      IF (.NOT.ALLOCATED(c)) THEN
         ALLOCATE(c(n, m), STAT=info)
      ELSE
         IF (s(1).NE.n.OR.s(2).NE.m) THEN
            ALLOCATE(w(s(1), s(2)), STAT=info)
            CALL copy_arrays(c, w)
            DEALLOCATE(c)
            ALLOCATE(c(n, m), STAT=info)
            CALL copy_arrays(w, c)
            DEALLOCATE(w)
         ENDIF
      ENDIF

   END SUBROUTINE alloc_single

   SUBROUTINE alloc_double(c, n, m, info)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE     :: c
      INTEGER, INTENT(IN)                                :: n, m
      INTEGER, INTENT(INOUT)                             :: info

      ! local variables
      INTEGER, DIMENSION(2)                              :: s
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE     :: w

      s = SHAPE(c)

      IF (.NOT.ALLOCATED(c)) THEN
         ALLOCATE(c(n, m), STAT=info)
      ELSE
         IF (s(1).NE.n.OR.s(2).NE.m) THEN
            ALLOCATE(w(s(1), s(2)), STAT=info)
            CALL copy_arrays(c, w)
            DEALLOCATE(c)
            ALLOCATE(c(n, m), STAT=info)
            CALL copy_arrays(w, c)
            DEALLOCATE(w)
         ENDIF
      ENDIF

   END SUBROUTINE alloc_double


   ! ------------------------------------------------- !
   ! SUBROUTINE: COPY_ARRAYS                           !
   ! ------------------------------------------------- !
   SUBROUTINE copy_arrays_single(a, b)

      ! copies content of array a into array b
      ! to the extent it is possible, i.e.
      ! effectively a is superimposed onto b

      ! input variables
      REAL, DIMENSION(:,:), INTENT(IN)    :: a
      REAL, DIMENSION(:,:), INTENT(INOUT) :: b

      ! local variables
      INTEGER                 :: i, j
      INTEGER, DIMENSION(2)   :: asize, bsize

      asize = SHAPE(a)
      bsize = SHAPE(b)

      IF (asize(1).LE.bsize(1).AND.asize(2).LE.bsize(2)) THEN
         DO j = 1, asize(2)
            DO i = 1, asize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ELSEIF (asize(1).LE.bsize(1).AND.asize(2).GT.bsize(2)) THEN
         DO j = 1, bsize(2)
            DO i = 1, asize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ELSEIF (asize(1).GT.bsize(1).AND.asize(2).LE.bsize(2)) THEN
         DO j = 1, asize(2)
            DO i = 1, bsize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ELSEIF (asize(1).GT.bsize(1).AND.asize(2).GT.bsize(2)) THEN
         DO j = 1, bsize(2)
            DO i = 1, bsize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE copy_arrays_single

   SUBROUTINE copy_arrays_double(a, b)

      ! copies content of array a into array b
      ! to the extent it is possible, i.e.
      ! effectively a is superimposed onto b

      ! input variables
      DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN)    :: a
      DOUBLE PRECISIOn, DIMENSION(:,:), INTENT(INOUT) :: b

      ! local variables
      INTEGER                 :: i, j
      INTEGER, DIMENSION(2)   :: asize, bsize

      asize = SHAPE(a)
      bsize = SHAPE(b)

      IF (asize(1).LE.bsize(1).AND.asize(2).LE.bsize(2)) THEN
         DO j = 1, asize(2)
            DO i = 1, asize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ELSEIF (asize(1).LE.bsize(1).AND.asize(2).GT.bsize(2)) THEN
         DO j = 1, bsize(2)
            DO i = 1, asize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ELSEIF (asize(1).GT.bsize(1).AND.asize(2).LE.bsize(2)) THEN
         DO j = 1, asize(2)
            DO i = 1, bsize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ELSEIF (asize(1).GT.bsize(1).AND.asize(2).GT.bsize(2)) THEN
         DO j = 1, bsize(2)
            DO i = 1, bsize(1)
               b(i, j) = a(i, j)
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE copy_arrays_double


   ! ------------------------------------------------- !
   ! SUBROUTINE: SWAP                                  !
   ! ------------------------------------------------- !
   ELEMENTAL SUBROUTINE swap_single(a, b)

      ! swaps content of a with content of b

      ! input variables
      REAL, INTENT(INOUT)  :: a, b

      ! local variables
      REAL                 :: w

      w = a
      a = b
      b = w

   END SUBROUTINE swap_single

   ELEMENTAL SUBROUTINE swap_double(a, b)

      ! swaps content of a with content of b

      ! input variables
      DOUBLE PRECISION, INTENT(INOUT)  :: a, b

      ! local variables
      DOUBLE PRECISION                 :: w

      w = a
      a = b
      b = w

   END SUBROUTINE swap_double


END MODULE m_arrays
