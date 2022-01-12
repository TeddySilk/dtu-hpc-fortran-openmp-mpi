MODULE m_init

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE alloc
      MODULE PROCEDURE alloc3
   END INTERFACE

   INTERFACE copy_arrays
      MODULE PROCEDURE copy_arrays3
   END INTERFACE

   INTERFACE init
      MODULE PROCEDURE init3
   END INTERFACE

   ! ------------------------------------------------- !
   ! LOCAL HEAP VARIABLES                              !
   ! ------------------------------------------------- !
   REAL, DIMENSION(:, :, :), ALLOCATABLE     :: work3

CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE: ALLOC                                 !
   ! ------------------------------------------------- !
   SUBROUTINE alloc3(array, nx, ny, nz, info)

      ! input variables
      REAL, DIMENSION(:, :, :), ALLOCATABLE     :: array
      INTEGER, INTENT(IN)                    :: nx, ny, nz
      INTEGER, INTENT(INOUT)                 :: info

      IF (.NOT.ALLOCATED(array)) THEN
         ALLOCATE(array(nx, ny, nz), STAT=info)
      ELSE
         IF (&
            SIZE(array, 1).NE.nx.OR.SIZE(array, 2).NE.ny.OR.SIZE(array, 3).NE.nz&
            ) THEN
            ALLOCATE(work3&
               (&
               SIZE(array, 1), SIZE(array, 2), SIZE(array, 3)), &
               STAT=info&
               )
            CALL copy_arrays(array, work3)
            DEALLOCATE(array)
            ALLOCATE(array(nx, ny, nz))
            CALL copy_arrays(work3, array)
            DEALLOCATE(work3)
         ENDIF
      ENDIF





   END SUBROUTINE alloc3

   ! ------------------------------------------------- !
   ! SUBROUTINE: COPY_ARRAYS                           !
   ! ------------------------------------------------- !
   SUBROUTINE copy_arrays3(a, b)

      ! copies content of array a into array b
      ! to the extent it is possible, i.e.
      ! effectively a is superimposed onto b

      ! input variables
      REAL, DIMENSION(:,:,:), INTENT(IN)    :: a
      REAL, DIMENSION(:,:,:), INTENT(INOUT) :: b

      ! local variables
      INTEGER                 :: i, j, k
      INTEGER, DIMENSION(3)   :: asize, bsize

      asize = SHAPE(a)
      bsize = SHAPE(b)

      IF (&
         asize(1).LE.bsize(1).AND.asize(2).LE.bsize(2).AND.asize(3).LE.bsize(3)&
         ) THEN
         DO k = 1, asize(3)
            DO j = 1, asize(2)
               DO i = 1, asize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (&
         asize(1).LE.bsize(1).AND.asize(2).LE.bsize(2).AND.asize(3).GT.bsize(3)&
         ) THEN
         DO k = 1, bsize(3)
            DO j = 1, asize(2)
               DO i = 1, asize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (&
         asize(1).LE.bsize(1).AND.asize(2).GT.bsize(2).AND.asize(3).GT.bsize(3)&
         ) THEN
         DO k = 1, bsize(3)
            DO j = 1, bsize(2)
               DO i = 1, asize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (&
         asize(1).GT.bsize(1).AND.asize(2).GT.bsize(2).AND.asize(3).GT.bsize(3)&
         ) THEN
         DO k = 1, bsize(3)
            DO j = 1, bsize(2)
               DO i = 1, bsize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (&
         asize(1).GT.bsize(1).AND.asize(2).LE.bsize(2).AND.asize(3).GT.bsize(3)&
         ) THEN
         DO k = 1, bsize(3)
            DO j = 1, asize(2)
               DO i = 1, bsize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (&
         asize(1).GT.bsize(1).AND.asize(2).LE.bsize(2).AND.asize(3).LE.bsize(3)&
         ) THEN
         DO k = 1, asize(3)
            DO j = 1, asize(2)
               DO i = 1, bsize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (&
         asize(1).LE.bsize(1).AND.asize(2).GT.bsize(2).AND.asize(3).LE.bsize(3)&
         ) THEN
         DO k = 1, asize(3)
            DO j = 1, bsize(2)
               DO i = 1, asize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (&
         asize(1).GT.bsize(1).AND.asize(2).GT.bsize(2).AND.asize(3).LE.bsize(3)&
         ) THEN
         DO k = 1, asize(3)
            DO j = 1, bsize(2)
               DO i = 1, bsize(1)
                  b(i, j, k) = a(i, j, k)
               ENDDO
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE copy_arrays3

   ! ------------------------------------------------- !
   ! SUBROUTINE: INIT                                  !
   ! ------------------------------------------------- !
   SUBROUTINE init3(field, boundary_temperature, initial_temperature)


      ! VARIABLES
      ! ------------------------------------------------ !

      ! input variables
      REAL, DIMENSION(:,:,:), INTENT(INOUT) :: field

      ! local variables
      INTEGER                 :: nx, ny, nz
      INTEGER                 :: i, j, k

      ! optional variables
      REAL, OPTIONAL                :: boundary_temperature
      REAL, OPTIONAL                :: initial_temperature


      ! FUNCTION
      ! ------------------------------------------------ !

      nx = SIZE(field, 1)
      ny = SIZE(field, 2)
      nz = SIZE(field, 3)

      ! apply Dirilect boundary condition
      IF (.NOT.PRESENT(boundary_temperature)) THEN
         DO i = 1, nx
            field( i,  1,  1)   = 1.0
            field( i, ny,  1)   = 1.0
            field( i,  1, nz)   = 1.0
            field( i, ny, nz)   = 1.0
         ENDDO

         DO j = 1, ny
            field( 1,  j,  1)   = 1.0
            field( 1,  j, nz)   = 1.0
            field(nx,  j,  1)   = 1.0
            field(nx,  j, nz)   = 1.0
         ENDDO

         DO j = 1, nz
            field( 1,  1,  k)   = 1.0
            field( 1, ny,  k)   = 1.0
            field(nx,  1,  k)   = 1.0
            field(nx, ny,  k)   = 1.0
         ENDDO
      ELSE
         DO i = 1, nx
            field( i,  1,  1)   = boundary_temperature
            field( i, ny,  1)   = boundary_temperature
            field( i,  1, nz)   = boundary_temperature
            field( i, ny, nz)   = boundary_temperature
         ENDDO

         DO j = 1, ny
            field( 1,  j,  1)   = boundary_temperature
            field( 1,  j, nz)   = boundary_temperature
            field(nx,  j,  1)   = boundary_temperature
            field(nx,  j, nz)   = boundary_temperature
         ENDDO

         DO j = 1, nz
            field( 1,  1,  k)   = boundary_temperature
            field( 1, ny,  k)   = boundary_temperature
            field(nx,  1,  k)   = boundary_temperature
            field(nx, ny,  k)   = boundary_temperature
         ENDDO
      ENDIF

      ! initialize field to T = 0, unless other value given
      IF (.NOT.PRESENT(initial_temperature)) THEN
         DO k = 2, nz - 1
            DO j = 2, ny - 1
               DO i = 2, nx - 1
                  field(i, j, k) = 0.0
               ENDDO
            ENDDO
         ENDDO
      ELSE
         DO k = 2, nz - 1
            DO j = 2, ny - 1
               DO i = 2, nx - 1
                  field(i, j, k) = initial_temperature
               ENDDO
            ENDDO
         ENDDO
      ENDIF

      ! ------------------------------------------------ !

   END SUBROUTINE init3
END MODULE
