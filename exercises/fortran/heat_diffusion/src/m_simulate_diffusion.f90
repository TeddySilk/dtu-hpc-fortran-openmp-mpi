MODULE m_simulate_diffusion

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global
   USE m_alloc
   USE m_copy_arrays
   USE m_extract_field
   USE m_extract_binary
   USE m_diagnostics

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE simulate_diffusion
      MODULE PROCEDURE simulate_diffusion_single, simulate_diffusion_double
   END INTERFACE


CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE: SIMULATE_DIFFUSION                    !
   ! ------------------------------------------------- !
   SUBROUTINE simulate_diffusion_single(tfield, nsteps, input_step, verbose, binary_backup)

      ! input variables
      REAL, DIMENSION(:, :), INTENT(INOUT) :: tfield
      INTEGER, INTENT(IN) :: nsteps, verbose
      LOGICAL, INTENT(IN) :: binary_backup
      INTEGER, OPTIONAL   :: input_step

      ! local variables
      REAL, DIMENSION(:, :), ALLOCATABLE  :: w
      INTEGER, DIMENSION(2)               :: s
      INTEGER                             :: i, j, istep, info

      ! (local) date-time variables
      CHARACTER(LEN = 8)   :: date
      CHARACTER(LEN = 12)  :: time
      CHARACTER(LEN = 5)   :: zone

      ! cpu and wall times
      REAL     :: cpu_t1, cpu_t2, time_init, time_end
      INTEGER  :: sys_count, sys_count_rate
      
      ! -------------------------------------------------------- !

      ! generate work array w
      s = SHAPE(tfield)
      CALL alloc(w, s(1), s(2), info)
      CALL copy_arrays(tfield, w)

      ! print date-time of start of time-loop
      CALL DATE_AND_TIME(date, time, zone)
      PRINT*,  date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//&
               time(1:2)//":"//time(3:4)//":"//time(5:6)//" "//&
               "UTC"//zone//" |", " simulation started"

      ! save initial times
      CALL CPU_TIME(cpu_t1)
      CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
      time_init = sys_count * 1.0 / sys_count_rate

      ! perform time-stepping
      IF (.NOT.PRESENT(input_step)) THEN
         input_step = 0
      ENDIF

      DO istep = input_step + 1, input_step + nsteps

         ! compute time-advanced field
         ! compute time-advanced field
         DO j = 2, Ny - 1
            DO i = 2, Nx - 1
               w(i, j) = tfield(i, j) + dt * diff_const *(&
                    (tfield(i + 1, j) - 2 * tfield(i, j) + tfield(i - 1, j)) * rdx2 &
                  + (tfield(i, j + 1) - 2 * tfield(i, j) + tfield(i, j - 1)) * rdy2 &
               )
            ENDDO
         ENDDO

         ! copy temporary work array w (time-advanced) into tfield (old)
         CALL copy_arrays(w, tfield)
         !CALL swap(w, tfield)

         IF (verbose.EQ.1) THEN
            CALL extract_field(tfield, output_file, istep)
         ENDIF

         IF (MOD(istep, diagfreq).EQ.0) THEN
            CALL diagnostics(tfield, REAL(istep) * dt, diagnostic_unit, diagnostic_file)
         ENDIF

         IF (MOD(istep, binfreq).EQ.0.AND.binary_backup) THEN
            CALL extract_binary(tfield, binary_file, istep)
         ENDIF

      ENDDO

      ! save end times
      CALL CPU_TIME(cpu_t2)
      CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
      time_end = sys_count * 1.0 / sys_count_rate

      CALL DATE_AND_TIME(date, time, zone)
      PRINT*,  date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//&
               time(1:2)//":"//time(3:4)//":"//time(5:6)//" "//&
               "UTC"//zone//" |", " simulation ended"
      
      PRINT*, "elapsed wall clock time:", time_end - time_init
      PRINT*, "elapsed cpu time       :", cpu_t2 - cpu_t1

      ! -------------------------------------------------------- !

   END SUBROUTINE simulate_diffusion_single

   SUBROUTINE simulate_diffusion_double(tfield, nsteps, input_step, verbose, binary_backup)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:, :), INTENT(INOUT) :: tfield
      INTEGER, INTENT(IN) :: nsteps
      INTEGER, INTENT(IN) :: verbose
      LOGICAL, INTENT(IN) :: binary_backup
      INTEGER, OPTIONAL   :: input_step

      ! local variables
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE  :: w
      INTEGER, DIMENSION(2)               :: s
      INTEGER                             :: i, j, istep, info

      ! (local) date-time variables
      CHARACTER(LEN = 8)   :: date
      CHARACTER(LEN = 12)  :: time
      CHARACTER(LEN = 5)   :: zone

      ! cpu and wall times
      REAL     :: cpu_t1, cpu_t2, time_init, time_end
      INTEGER  :: sys_count, sys_count_rate
      
      ! -------------------------------------------------------- !

      ! generate work array w
      s = SHAPE(tfield)
      CALL alloc(w, s(1), s(2), info)
      CALL copy_arrays(tfield, w)

      ! print date-time of start of time-loop
      CALL DATE_AND_TIME(date, time, zone)
      PRINT*,  date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//&
               time(1:2)//":"//time(3:4)//":"//time(5:6)//" "//&
               "UTC"//zone//" |", " simulation started"

      ! save initial times
      CALL CPU_TIME(cpu_t1)
      CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
      time_init = sys_count * 1.0 / sys_count_rate

      ! perform time-stepping
      IF (.NOT.PRESENT(input_step)) THEN
         input_step = 0
      ENDIF

      DO istep = input_step + 1, input_step + nsteps 

         ! compute time-advanced field
         ! compute time-advanced field
         DO j = 2, Ny - 1
            DO i = 2, Nx - 1
               w(i, j) = tfield(i, j) + dt * diff_const *(&
                    (tfield(i + 1, j) - 2 * tfield(i, j) + tfield(i - 1, j)) * rdx2 &
                  + (tfield(i, j + 1) - 2 * tfield(i, j) + tfield(i, j - 1)) * rdy2 &
               )
            ENDDO
         ENDDO

         ! copy temporary work array w (time-advanced) into tfield (old)
         CALL copy_arrays(w, tfield)
         !CALL swap(w, tfield)

         IF (verbose.EQ.1) THEN
            CALL extract_field(tfield, output_file, istep)
         ENDIF

         IF (MOD(istep, diagfreq).EQ.0) THEN
            CALL diagnostics(tfield, REAL(istep) * dt, diagnostic_unit, diagnostic_file)
         ENDIF

         IF (MOD(istep, binfreq).EQ.0.AND.binary_backup) THEN
            CALL extract_binary(tfield, binary_file, istep)
         ENDIF

      ENDDO

      ! save end times
      CALL CPU_TIME(cpu_t2)
      CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
      time_end = sys_count * 1.0 / sys_count_rate

      CALL DATE_AND_TIME(date, time, zone)
      PRINT*,  date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//&
               time(1:2)//":"//time(3:4)//":"//time(5:6)//" "//&
               "UTC"//zone//" |", " simulation ended"
      
      PRINT*, "elapsed wall clock time:", time_end - time_init
      PRINT*, "elapsed cpu time       :", cpu_t2 - cpu_t1

      ! -------------------------------------------------------- !

   END SUBROUTINE simulate_diffusion_double

END MODULE m_simulate_diffusion
