MODULE m_alloc
   IMPLICIT none

CONTAINS
   SUBROUTINE alloc(c, n, m)

      REAL, DIMENSION(:, :), POINTER :: c
      INTEGER, INTENT(IN) :: n, m

      IF (.NOT.ASSOCIATED(c)) THEN
         ALLOCATE(c(n, m))
      ENDIF

   END SUBROUTINE alloc

END MODULE m_alloc
