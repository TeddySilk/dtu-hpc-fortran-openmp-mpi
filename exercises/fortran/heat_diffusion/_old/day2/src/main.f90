PROGRAM main
   ! ---------------------------------------------------------------------------------------------- !

   ! import modules
   USE m_constants   ! contains parameters shared by the modules
   USE m_alloc       ! used to allocate memory for the 2D-field data
   USE m_init        ! used to initiate the 2D field
   USE m_out         ! used to extract the field data to .dat files
   USE m_copy        ! used to copy new field data to the old field array

   ! define all parameters, variables, and arrays
   IMPLICIT none

   ! filename
   CHARACTER(LEN = 24) :: output_name = 'Tfield'

   ! initialize variables
   REAL :: t = 0.0
   INTEGER :: i, j, istep

   ! initialize field pointers
   REAL, DIMENSION(:,:), POINTER :: field1, field2

   ! ---------------------------------------------------------------------------------------------- !

   ! allocate memory to the fields
   CALL alloc(field1, Nx, Ny)
   CALL alloc(field2, Nx, Ny)

   ! initialize the fields according to the Dirilect BC, and otherwise equal 0
   CALL init(field1)
   CALL init(field2)

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
      CALL cp_field(field2, field1)

      t = t + dt

      CALL extract(field1, output_name, istep)

   ENDDO

   ! ---------------------------------------------------------------------------------------------- !

   ! produce output
   PRINT *, ''
   PRINT *, '------ SELECT OUTPUT ------'
   PRINT *, 't  = ', t, 'dt = ', dt
   PRINT *, 'dx =', dx, ' dy = ', dy

END PROGRAM main
