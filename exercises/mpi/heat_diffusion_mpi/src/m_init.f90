MODULE m_init

   USE m_precision
   USE m_global
   IMPLICIT NONE

CONTAINS


   SUBROUTINE init_field(field, init_boundary, init_vals, boundary_type)

      ! input variables
      REAL(wp), DIMENSION(:,:), INTENT(INOUT) :: field
      REAL(wp)   :: init_boundary   ! boundary wall values
      REAL(wp)   :: init_vals       ! initial field values
      INTEGER    :: boundary_type   ! indicates where the boundaries are

      ! iterators
      INTEGER  :: i, j

      ! dimensions
      INTEGER  :: n, m


      ! get dimension sizes
      n = SIZE(field, 1)
      m = SIZE(field, 2)

      ! apply Dirilect boundary conditions
      ! FULL
      IF (boundary_type.EQ.0) THEN
         DO i = 1, n
            field(i, 1)   = init_boundary
            field(i, m)   = init_boundary
         ENDDO
         DO j = 1, m
            field(1, j)   = init_boundary
            field(n, j)   = init_boundary
         ENDDO
      ! LEFT, TOP, BOTTOM
      ELSEIF (boundary_type.EQ.1) THEN
         DO i = 1, n
            field(i, 1)   = init_boundary
            field(i, m)   = init_boundary
         ENDDO
         DO j = 1, m
            field(1, j)   = init_boundary
            field(n, j)   = init_vals
         ENDDO
      ! RIGHT, TOP, BOTTOM
      ELSEIF (boundary_type.EQ.2) THEN
         DO i = 1, n
            field(i, 1)   = init_boundary
            field(i, m)   = init_boundary
         ENDDO
         DO j = 1, m
            field(1, j)   = init_vals
            field(n, j)   = init_boundary
         ENDDO
      ! TOP, BOTTOM   
      ELSEIF (boundary_type.EQ.3) THEN
         DO i = 1, n
            field(i, 1)   = init_boundary
            field(i, m)   = init_boundary
         ENDDO
         DO j = 1, m
            field(1, j)   = init_vals
            field(n, j)   = init_vals
         ENDDO
      ENDIF



      ! initialize field to T = 0, unless other value given
      DO j = 2, m - 1
         DO i = 2, n - 1
            field(i, j)       = init_vals
         ENDDO
      ENDDO

   END SUBROUTINE init_field

END MODULE m_init


