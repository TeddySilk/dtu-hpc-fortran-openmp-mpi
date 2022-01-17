MODULE m_swap

   IMPLICIT NONE

   ! ------------------------------------------------- !
   ! OVERLOADING                                       !
   ! ------------------------------------------------- !
   INTERFACE swap
      MODULE PROCEDURE swap_single, swap_double
   END INTERFACE
   

CONTAINS

   ! ------------------------------------------------- !
   ! SUBROUTINE: SWAP                                  !
   ! ------------------------------------------------- !
   ELEMENTAL SUBROUTINE swap_single(a, b)

      ! swaps content of a with content of b

      ! input variables
      REAL, INTENT(INOUT)  :: a, b

      ! local variables
      REAL                 :: w

      ! w: work array is used

      w = a
      a = b
      b = w

   END SUBROUTINE swap_single

   ELEMENTAL SUBROUTINE swap_double(a, b)

      ! swaps content of a with content of b

      ! input variables
      DOUBLE PRECISION, INTENT(INOUT)  :: a, b

      ! local variables
      DOUBLE PRECISION                 :: w

      w = a
      a = b
      b = w

   END SUBROUTINE swap_double


END MODULE m_swap

