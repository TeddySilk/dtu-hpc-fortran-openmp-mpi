MODULE m_alloc

   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_copy_arrays

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE alloc
      MODULE PROCEDURE alloc_single, alloc_double
   END INTERFACE


CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE: ALLOC                                 !
   ! ------------------------------------------------- !
   SUBROUTINE alloc_single(c, n, m, info)

      ! input variables
      REAL, DIMENSION(:, :), ALLOCATABLE     :: c
      INTEGER, INTENT(IN)                    :: n, m
      INTEGER, INTENT(INOUT)                 :: info

      ! local variables
      INTEGER, DIMENSION(2)                  :: s
      REAL, DIMENSION(:, :), ALLOCATABLE     :: w

      s = SHAPE(c)

      IF (.NOT.ALLOCATED(c)) THEN
         ALLOCATE(c(n, m), STAT=info)
      ELSE
         IF (s(1).NE.n.OR.s(2).NE.m) THEN
            ALLOCATE(w(s(1), s(2)), STAT=info)
            CALL copy_arrays(c, w)
            DEALLOCATE(c)
            ALLOCATE(c(n, m), STAT=info)
            CALL copy_arrays(w, c)
            DEALLOCATE(w)
         ENDIF
      ENDIF

   END SUBROUTINE alloc_single

   SUBROUTINE alloc_double(c, n, m, info)

      ! input variables
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE     :: c
      INTEGER, INTENT(IN)                                :: n, m
      INTEGER, INTENT(INOUT)                             :: info

      ! local variables
      INTEGER, DIMENSION(2)                              :: s
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE     :: w

      s = SHAPE(c)

      IF (.NOT.ALLOCATED(c)) THEN
         ALLOCATE(c(n, m), STAT=info)
      ELSE
         IF (s(1).NE.n.OR.s(2).NE.m) THEN
            ALLOCATE(w(s(1), s(2)), STAT=info)
            CALL copy_arrays(c, w)
            DEALLOCATE(c)
            ALLOCATE(c(n, m), STAT=info)
            CALL copy_arrays(w, c)
            DEALLOCATE(w)
         ENDIF
      ENDIF

   END SUBROUTINE alloc_double


END MODULE m_alloc

