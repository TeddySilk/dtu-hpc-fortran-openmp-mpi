MODULE m_timer
   ! This module contains:
   !  - start_timer(cpu_t1, wall_t1): Records starting times
   !  - stop_timer(cpu_t1, cpu_t2, wall_t1, wall_t2): Outputs elapsed time

   USE m_precision
   USE m_global
   IMPLICIT NONE

CONTAINS


   SUBROUTINE start_timer(cpu_t1, wall_t1)
      ! start_timer(cpu_t1, wall_t1) prints the date and time,
      ! and saves the cpu and wall times to cpu_t1 and wall_t1,
      ! respectively.

      ! inputs
      REAL(wp), INTENT(INOUT)  :: cpu_t1, wall_t1

      ! local count, count rate
      INTEGER  :: sys_count, sys_count_rate

      ! local date-time variables
      CHARACTER(LEN = 8)   :: date
      CHARACTER(LEN = 12)  :: time
      CHARACTER(LEN = 5)   :: zone

      ! print date-time
      CALL DATE_AND_TIME(date, time, zone)
      PRINT*,  date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//&
         time(1:2)//":"//time(3:4)//":"//time(5:6)//" "//&
         "UTC"//zone//" |", " TIMER STARTED"

      ! save initial times
      CALL CPU_TIME(cpu_t1)
      CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
      wall_t1 = sys_count * 1.0 / sys_count_rate

   END SUBROUTINE


   SUBROUTINE stop_timer(cpu_t1, cpu_t2, wall_t1, wall_t2)
      ! stop_timer(cpu_t1, cpu_t2, wall_t1, wall_t2) prints the date
      ! and time, and elapsed wall clock time and cpu time, and saves
      ! the (new) cpu and wall times to cpu_t2 and wall_t2, respectively.

      ! inputs
      REAL(wp), INTENT(INOUT)  :: cpu_t1, cpu_t2, wall_t1, wall_t2

      ! local variables
      INTEGER              :: sys_count, sys_count_rate
      LOGICAL              :: file_exists
      CHARACTER(LEN = 48)  :: string

      ! local date-time variables
      CHARACTER(LEN = 8)   :: date
      CHARACTER(LEN = 12)  :: time
      CHARACTER(LEN = 5)   :: zone

      ! save end times
      CALL CPU_TIME(cpu_t2)
      CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
      wall_t2 = sys_count * 1.0 / sys_count_rate

      ! print date and time
      CALL DATE_AND_TIME(date, time, zone)
      PRINT*,  date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//&
         time(1:2)//":"//time(3:4)//":"//time(5:6)//" "//&
         "UTC"//zone//" |", " TIMER STOPPED"

      PRINT*, "elapsed wall clock time:", wall_t2 - wall_t1
      PRINT*, "elapsed cpu time       :", cpu_t2 - cpu_t1

      ! if benchmarking, save times to bench_out
      IF (benchmark) THEN
         WRITE(string, '(A,A,A)') './res/', trim(bench_out), '.dat'
         INQUIRE(FILE=trim(string), EXIST=file_exists)
         IF (file_exists) THEN
            OPEN(300, FILE = trim(string), STATUS="old", POSITION="append", ACTION="write")
         ELSE
            OPEN(300, FILE = trim(string), STATUS="new", ACTION="write")
         ENDIF
         WRITE(300, '(3E12.4)') &
            cpu_t2 - cpu_t1, &
            wall_t2 - wall_t1, &
            (cpu_t2 - cpu_t1) / (wall_t2 - wall_t1)
         CLOSE(300)
      ENDIF

   END SUBROUTINE

END MODULE m_timer

