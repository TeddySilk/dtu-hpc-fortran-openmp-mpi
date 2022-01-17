MODULE m_init

    USE m_precision
    USE m_global
    IMPLICIT NONE
 
 CONTAINS
 

    SUBROUTINE init_field(field, init_boundary, init_vals)
 
       ! input variables
       REAL(wp), DIMENSION(:,:), INTENT(INOUT) :: field
 
       ! iterators
       INTEGER  :: i, j

       ! dimensions
       INTEGER  :: n, m
 
       ! optional variables
       REAL(wp), OPTIONAL   :: init_boundary   ! boundary wall values
       REAL(wp), OPTIONAL   :: init_vals       ! initial field values

 
       ! get dimension sizes
       n = SIZE(field, 1)
       m = SIZE(field, 2)
 
       ! apply Dirilect boundary conditions
       IF (.NOT.PRESENT(init_boundary)) THEN
          DO i = 1, n
             field(i, 1)   = 1.0_wp
             field(i, m)   = 1.0_wp
          ENDDO
 
          DO j = 1, m
             field(1, j)   = 1.0_wp
             field(n, j)   = 1.0_wp
          ENDDO
       ELSE
          DO i = 1, n
             field(i, 1)   = init_boundary
             field(i, m)   = init_boundary
          ENDDO
 
          DO j = 1, m
             field(1, j)   = init_boundary
             field(n, j)   = init_boundary
          ENDDO
       ENDIF
 
       ! initialize field to T = 0, unless other value given
       IF (.NOT.PRESENT(init_vals)) THEN
          DO j = 2, m - 1
             DO i = 2, n - 1
                field(i, j)       = 0.0_wp
             ENDDO
          ENDDO
       ELSE
          DO j = 2, m - 1
             DO i = 2, n - 1
                field(i, j)       = init_vals
             ENDDO
          ENDDO
       ENDIF
 
    END SUBROUTINE init_field
 
 END MODULE m_init
 
 