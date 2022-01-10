MODULE m_timer

   IMPLICIT NONE

CONTAINS

   SUBROUTINE start_timer(cpu_t1, wall_t1)

      ! input cpu and wall times
      REAL, INTENT(INOUT)  :: cpu_t1, wall_t1

      ! local count, count rate
      INTEGER  :: sys_count, sys_count_rate

      ! (local) date-time variables
      CHARACTER(LEN = 8)   :: date
      CHARACTER(LEN = 12)  :: time
      CHARACTER(LEN = 5)   :: zone



      ! print date-time of start of time-loop
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

      ! input cpu and wall times
      REAL, INTENT(INOUT)  :: cpu_t1, cpu_t2, wall_t1, wall_t2

      ! local count, count rate
      INTEGER  :: sys_count, sys_count_rate

      ! (local) date-time variables
      CHARACTER(LEN = 8)   :: date
      CHARACTER(LEN = 12)  :: time
      CHARACTER(LEN = 5)   :: zone

      ! save end times
      CALL CPU_TIME(cpu_t2)
      CALL SYSTEM_CLOCK(sys_count, sys_count_rate)
      wall_t2 = sys_count * 1.0 / sys_count_rate

      CALL DATE_AND_TIME(date, time, zone)
      PRINT*,  date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//&
         time(1:2)//":"//time(3:4)//":"//time(5:6)//" "//&
         "UTC"//zone//" |", " TIMER STOPPED"

      PRINT*, "elapsed wall clock time:", wall_t2 - wall_t1
      PRINT*, "elapsed cpu time       :", cpu_t2 - cpu_t1

   END SUBROUTINE

END MODULE m_timer
