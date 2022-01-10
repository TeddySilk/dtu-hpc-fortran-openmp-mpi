PROGRAM e1b
   ! ---------------------------------------------------------------------------------------------- !

   ! define all parameters, variables, and arrays
   IMPLICIT NONE

   ! spatial parameters
   INTEGER, PARAMETER :: Nx = 21, Ny = 21       ! nx: amount of cells in x-dir, ny: amount of cells in y-dir
   INTEGER, PARAMETER :: Lx = 1, Ly = 1         ! box dimensions (Lx: length in x-dir, Ly: length in y-dir)
   INTEGER, PARAMETER :: D = 1                  ! D: diffusion constant
   REAL, PARAMETER :: dx = REAL(Lx) /&
      REAL(Nx - 1)                              ! dx: cell width in the x-dir
   REAL, PARAMETER :: dy = REAL(Ly) /&
      REAL(Ny - 1)                              ! dy: cell width in the y-dir
   REAL, PARAMETER :: rdx2 = 1/(dx ** 2)        ! rdx2: 1/(dx ** 2), to aid setting up the equations
   REAL, PARAMETER :: rdy2 = 1/(dy ** 2)        ! rdy2: see above

   ! temporal parameters
   INTEGER, PARAMETER :: nsteps = 200           ! nsteps: amount of time steps made
   REAL, PARAMETER :: dt = MIN(dx, dy) ** 2 /&
      (4*D)                                     ! dt: time-step size, satisfying the Fourier limit

   ! initialize variables
   REAL :: t = 0.0
   INTEGER :: i, j, istep

   ! initialize field arrays (field1 = old, field2 = new) to zero
   REAL, DIMENSION(Nx, Ny) :: field1 = 0.0, field2 = 0.0

   ! ---------------------------------------------------------------------------------------------- !

   ! apply Dirilect boundary condition (i.e. boundaries set to and kept at 1)
   DO i = 1, Nx
      field1(i, 1)    = 1
      field1(i, Ny)   = 1
   ENDDO

   DO j = 1, Ny
      field1(1, j)    = 1
      field1(Nx, j)   = 1
   ENDDO

   ! ---------------------------------------------------------------------------------------------- !

   ! perform time-stepping
   DO istep = 1, nsteps

      ! compute field2 (time-advanced)
      DO j = 2, Ny - 1
         DO i = 2, Nx - 1
            field2(i, j) = field1(i, j) &
               + dt * (field1(i + 1, j) - 2 * field1(i, j) + field1(i - 1, j)) * rdx2 &
               + dt * (field1(i, j + 1) - 2 * field1(i, j) + field1(i, j - 1)) * rdy2
         ENDDO
      ENDDO

      ! copy field2 (time-advanced) into field1 (old) to prepare for new time step
      DO j = 2, Ny - 1
         DO i = 2, Nx - 1
            field1(i, j) = field2(i, j)
         ENDDO
      ENDDO

      t = t + dt

   ENDDO

   ! ---------------------------------------------------------------------------------------------- !

   ! produce output
   PRINT *, 't = ', t, 'dt = ', dt
   PRINT *, 'dx = ', dx, 'dy = ', dy

   OPEN(10, FILE='main.dat')
   DO j = 1, Ny
      DO i = 1, Nx
         WRITE(10, '(3E12.4)') REAL(i - 1) * dx, REAL(j - 1) * dy, field1(i, j)
      ENDDO
      WRITE(10, '(A)')
   ENDDO
   CLOSE(10)

END PROGRAM e1b
