MODULE m_init

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global
   USE OMP_LIB

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
   DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE     :: work3

CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE: ALLOC                                 !
   ! ------------------------------------------------- !
   SUBROUTINE alloc3(array, nx, ny, nz, info)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE     :: array
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
      DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(IN)    :: a
      DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(INOUT) :: b

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
   SUBROUTINE init3(field, type, source)


      ! VARIABLES
      ! ------------------------------------------------ !

      ! input variables
      DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(INOUT) :: field

      ! local variables
      INTEGER                 :: nx, ny, nz
      INTEGER                 :: i, j, k
      DOUBLE PRECISION                    :: pi

      ! optional variables
      CHARACTER(LEN = *), OPTIONAL  :: type
      LOGICAL, OPTIONAL             :: source


      ! FUNCTION
      ! ------------------------------------------------ !

      nx = SIZE(field, 1)
      ny = SIZE(field, 2)
      nz = SIZE(field, 3)

      ! FIELD INPUT
      ! ------------------------------------------------ !
      IF (.NOT.PRESENT(source).OR..NOT.source) THEN
         ! apply Dirilect boundary condition based on type
         ! sinusoidal boundary conditions
         IF (type.EQ."sinusoidal") THEN
            DO i = 1, nx
               field( i,  1,  1)   = 0.0
               field( i, ny,  1)   = 0.0
               field( i,  1, nz)   = 0.0
               field( i, ny, nz)   = 0.0
            ENDDO

            DO j = 1, ny
               field( 1,  j,  1)   = 0.0
               field( 1,  j, nz)   = 0.0
               field(nx,  j,  1)   = 0.0
               field(nx,  j, nz)   = 0.0
            ENDDO

            DO k = 1, nz
               field( 1,  1,  k)   = 0.0
               field( 1, ny,  k)   = 0.0
               field(nx,  1,  k)   = 0.0
               field(nx, ny,  k)   = 0.0
            ENDDO
         ! radiator boundary conditions
         ELSEIF (type.EQ."radiator") THEN
            !$OMP PARALLEL
            !$OMP DO COLLAPSE(1) PRIVATE(j, i)
            DO j = 1, ny
               DO i = 1, nx
                  field( i,  j,  1) = 20.0
                  field( i,  j, nz) = 20.0
               ENDDO
            ENDDO
            !$OMP END DO
            !$OMP DO COLLAPSE(1) PRIVATE(j, i)
            DO k = 1, nz
               DO j = 1, ny
                  field( 1,  j,  k) = 20.0
                  field(nx,  j,  k) = 20.0
               ENDDO
            ENDDO
            !$OMP END DO
            !$OMP DO COLLAPSE(1) PRIVATE(j, i)
            DO k = 1, nz
               DO i = 1, nx
                  field( i,  1,  k) =  0.0
                  field( i, ny,  k) = 20.0
               ENDDO
            ENDDO
            !$OMP END DO
            !$OMP END PARALLEL
         ENDIF

         ! initialize field to T = 0, unless other value given
         !$OMP PARALLEL
         !$OMP DO COLLAPSE(2) PRIVATE(k, j, i)
         DO k = 2, nz - 1
            DO j = 2, ny - 1
               DO i = 2, nx - 1
                  field(i, j, k) = 0.0
               ENDDO
            ENDDO
         ENDDO
         !$OMP END DO
         !$OMP END PARALLEL

      ! SOURCE INPUT
      ! ------------------------------------------------ !
      ELSE
         IF (type.EQ."sinusoidal") THEN
            ! source term:
            ! f(x,y,z) = 3*pi**2 * sin(pi*x) * sin(pi*y) * sin(pi*z)
            ! assumes symmetrical domain (x=[-0.5Lx;0.5Lx], etc)
            pi = 4.0 * ATAN(1.0)
            DO k = 1, nz
               DO j = 1, ny
                  DO i = 1, nx
                     field(i, j, k) =  3.0 * pi * pi *&
                        sin(pi * (-0.5*Lx + (i-1)*dx)) *&
                        sin(pi * (-0.5*Ly + (j-1)*dy)) *&
                        sin(pi * (-0.5*Lz + (k-1)*dz))
                  ENDDO
               ENDDO
            ENDDO
         ELSEIF (type.EQ."radiator") THEN
            ! source term:
            ! f(x,y,z) = 200 for x = [-1; -3/8], y = [-1; -1/2], z = [-2/3; 0]
            !          = 0 elsewhere
            !$OMP PARALLEL
            !$OMP DO COLLAPSE(2) PRIVATE(k, j, i)
            DO k = 1, nz
               DO j = 1, ny
                  DO i = 1, nx
                     IF (&
                        ((-0.5*Lx) + REAL(i - 1) * dx).LE.(-3.0/8.0).AND.&
                        ((-0.5*Ly) + REAL(j - 1) * dy).LE.(-1.0/2.0).AND.&
                        ((-0.5*Lz) + REAL(k - 1) * dz).LE.(0.0).AND.&
                        ((-0.5*Lz) + REAL(k - 1) * dz).GE.(-2.0/3.0)&
                        ) THEN
                        field(i, j, k) = 200
                     ELSE
                        field(i, j, k) = 0
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
            !$OMP END DO
            !$OMP END PARALLEL
         ENDIF
      ENDIF

      ! ------------------------------------------------ !

   END SUBROUTINE init3
END MODULE
