PROGRAM matrix_times_vector

   USE m_timer
   USE OMP_LIB

   IMPLICIT NONE

   REAL, DIMENSION(:, :), ALLOCATABLE :: mat
   REAL, DIMENSION(:), ALLOCATABLE    :: vec
   REAL, DIMENSION(:), ALLOCATABLE    :: prod

   REAL :: cpu_t1, cpu_t2, wall_t1, wall_t2
   INTEGER :: i, j

   CALL start_timer(cpu_t1, wall_t1)

   ALLOCATE(mat(10000,10000))
   ALLOCATE(vec(10000))
   ALLOCATE(prod(SIZE(mat, 2)))

   !$OMP PARALLEL
   !$OMP DO COLLAPSE(1) PRIVATE(i, j)
   DO j = 1, SIZE(mat, 2)
      DO i = 1, SIZE(mat, 1)
         mat(i, j) = i + j
      ENDDO
   ENDDO
   !$OMP END DO
   
   !$OMP DO PRIVATE(i)
   DO i = 1, SIZE(vec)
      vec(i) = i
   ENDDO
   !$OMP END DO
   !$OMP END PARALLEL


   CALL mtv(mat, vec, prod)

   CALL stop_timer(cpu_t1, cpu_t2, wall_t1, wall_t2)
   PRINT *, TRIM("Sum of matrix-vector product:"), SUM(prod)

CONTAINS

   SUBROUTINE mtv(mat, vec, prod)

      REAL, DIMENSION(:, :), INTENT(IN)   :: mat
      REAL, DIMENSION(:), INTENT(IN)      :: vec
      REAL, DIMENSION(:)                  :: prod

      REAL    :: temp
      INTEGER :: i, j, dim1, dim2

      dim1 = SIZE(mat, 1)
      dim2 = SIZE(mat, 2)

      !$OMP PARALLEL
      !$OMP DO COLLAPSE(1) PRIVATE(i, j, temp)
      DO j = 1, dim2
         DO i = 1, dim1
            temp = mat(i, j) * vec(i)
            prod(j) = prod(j) + temp
         ENDDO
         prod(j) = temp
      ENDDO
      !$OMP END DO
      !$OMP END PARALLEL
   END SUBROUTINE mtv

END PROGRAM matrix_times_vector
