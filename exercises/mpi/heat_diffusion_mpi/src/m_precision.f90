MODULE m_precision

    IMPLICIT NONE

    ! Precision parameters
    INTEGER, PARAMETER  :: sp = KIND(1.0E0)         ! single precision
    INTEGER, PARAMETER  :: dp = KIND(1.0D0)         ! double precision        

    ! Used precision parameter
    INTEGER, PARAMETER  :: wp = dp                  ! working precision

END MODULE m_precision