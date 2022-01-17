MODULE m_arrays
   ! This module contains array manipulation subroutines:
   !  - check_iostat(array, iostate)
   !  - alloc(array, dim1, dim2)
   !  - copy(array1, array2)
   !  - swap(array1. array2)
   USE m_precision
   IMPLICIT NONE

   ! internal work array
   REAL(wp), DIMENSION(:, :), ALLOCATABLE :: work

CONTAINS


   SUBROUTINE check_iostat(iostat)
      ! check_iostat checks if the an STAT was
      ! has returned 0, and if not, exists with
      ! an error message.
      INTEGER, INTENT(IN)  :: iostat

      IF (iostat.NE.0) THEN
         PRINT*, TRIM("STAT ERROR: "), iostat, TRIM(" REGISTERED!")
      ENDIF

   END SUBROUTINE check_iostat


   SUBROUTINE alloc(array, dim1, dim2)
      ! alloc(array, dim1, dim2) takes an allocatable 2d array,
      ! an allocates it according to the given dimensions,
      ! dim1 and dim2.

      REAL(wp), DIMENSION(:, :), ALLOCATABLE    :: array
      INTEGER, INTENT(IN)                       :: dim1, dim2

      ! local variables
      INTEGER :: iostat
      INTEGER :: temp_dim1, temp_dim2

      ! if not allocated, simply allocate
      IF (.NOT.ALLOCATED(array)) THEN
         ALLOCATE(array(dim1, dim2), STAT=iostat)
         CALL check_iostat(iostat)
         ! else, reallocate to the intended size
      ELSE
         temp_dim1 = SIZE(array, 1)
         temp_dim2 = SIZE(array, 2)

         IF (temp_dim1.NE.dim1.OR.temp_dim2.NE.dim2) THEN

            ! allocate work array
            ALLOCATE(work(temp_dim1, temp_dim2), STAT=iostat)
            CALL check_iostat(iostat)

            ! copy to work array
            CALL copy(array, work)

            ! deallocate copied array
            DEALLOCATE(array, STAT=iostat)
            CALL check_iostat(iostat)

            ! allocate array to the new size
            ALLOCATE(array(dim1, dim2), STAT=iostat)
            CALL check_iostat(iostat)

            ! copy saved contents from work array
            CALL copy(work, array)

            ! deallocate work
            DEALLOCATE(work)

         ENDIF
      ENDIF

   END SUBROUTINE alloc


   SUBROUTINE swap(array1, array2)
      ! swap(array1, array2) swaps the
      ! contents of the two arrays.

      ! input variables
      REAL(wp), DIMENSION(:,:), INTENT(INOUT)   :: array1, array2

      ! local variables
      INTEGER :: i, j, iostat

      ! check if equal size
      IF (SIZE(array1, 1).EQ.SIZE(array2, 1).AND.SIZE(array1, 2).EQ.SIZE(array2, 2)) THEN
         ! allocate work to facilitate swapping
         ALLOCATE(work(SIZE(array1, 1), SIZE(array1, 2)), STAT=iostat)
         CALL check_iostat(iostat)

         ! swap around
         DO j = 1, SIZE(array1, 2)
            DO i = 1, SIZE(array1, 1)
               work(i, j) = array1(i, j)
               array1(i, j) = array2(i, j)
               array2(i, j) = work(i, j)
            ENDDO
         ENDDO

         ! deallocate work again
         DEALLOCATE(work, STAT=iostat)
         CALL check_iostat(iostat)

      ! if not equal size, complain
      ELSE
         PRINT *, TRIM("WARNING! ARRAY1 AND ARRAY2 HAVE DIFFERENT SIZES! &
         &SWAP-SUBROUTINE FAILED!")
      ENDIF

   END SUBROUTINE swap


   SUBROUTINE copy(array1, array2)
      ! copy(array1, array) copies the content of
      ! array1 -> array2, to the extent it is
      ! possible, if the sizes are not the same.

      REAL(wp), DIMENSION(:,:), INTENT(IN)    :: array1
      REAL(wp), DIMENSION(:,:), INTENT(INOUT) :: array2

      ! local variables
      INTEGER  :: i, j
      INTEGER  :: dim1_1, dim1_2, dim2_1, dim2_2

      ! get dimensions
      dim1_1 = SIZE(array1, 1)
      dim2_1 = SIZE(array1, 2)
      dim1_2 = SIZE(array2, 1)
      dim2_2 = SIZE(array2, 2)

      ! copy conditionals to ensure as much data
      ! is copied, regardless of size
      IF (dim1_1.LE.dim1_2.AND.dim2_1.LE.dim2_2) THEN
         DO j = 1, dim1_1
            DO i = 1, dim1_1
               array2(i, j) = array1(i, j)
            ENDDO
         ENDDO
      ELSEIF (dim1_1.LE.dim1_2.AND.dim2_1.GT.dim2_2) THEN
         DO j = 1, dim2_2
            DO i = 1, dim1_1
               array2(i, j) = array1(i, j)
            ENDDO
         ENDDO
      ELSEIF (dim1_1.GT.dim1_2.AND.dim2_1.LE.dim2_2) THEN
         DO j = 1, dim2_1
            DO i = 1, dim1_2
               array2(i, j) = array1(i, j)
            ENDDO
         ENDDO
      ELSEIF (dim1_1.GT.dim1_2.AND.dim2_1.GT.dim2_2) THEN
         DO j = 1, dim2_2
            DO i = 1, dim1_2
               array2(i, j) = array1(i, j)
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE copy

END MODULE m_arrays


