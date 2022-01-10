MODULE m_init
CONTAINS
   SUBROUTINE init(data, n)
      REAL, DIMENSION(*) :: data
      INTEGER :: n

      DO i = 1, n
         data(i) = -i
      ENDDO
   END SUBROUTINE init
END MODULE m_init