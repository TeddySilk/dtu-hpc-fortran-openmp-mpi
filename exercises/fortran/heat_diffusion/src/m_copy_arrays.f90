MODULE m_copy_arrays

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE copy_arrays
      MODULE PROCEDURE copy_arrays_single, copy_arrays_double
   END INTERFACE


CONTAINS


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

END MODULE m_copy_arrays
