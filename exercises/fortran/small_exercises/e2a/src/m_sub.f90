MODULE m_sub
CONTAINS
   SUBROUTINE sub(data, n, info)
      REAL, DIMENSION(:), POINTER :: data
      INTEGER :: n, info

      ALLOCATE(data(n), stat=info)
   END SUBROUTINE sub
END MODULE m_sub
