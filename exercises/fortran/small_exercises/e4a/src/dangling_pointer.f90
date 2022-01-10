PROGRAM main

   IMPLICIT NONE

   REAL, DIMENSION(:), POINTER :: p
   REAL, DIMENSION(:), ALLOCATABLE, TARGET :: t
   INTEGER :: info, N1, N2
   LOGICAL :: REASSIGN_POINTER

   ! Q:  Does the program crash for all values of N1?
   ! A:  No.

   REASSIGN_POINTER = .FALSE.

   N1 = 31980
   N2 = 1000

   ! FIRST ALLOCATION
   ALLOCATE(t(N1))
   CALL RANDOM_NUMBER(t) ! assign some value to the target
   p => t ! let p point to t
   PRINT*,' p(3) = ',p(3) ! PRINT some value of p

   ! SECOND ALLOCATION
   DEALLOCATE(t,STAT=info) ! Deallocate t
   ALLOCATE(t(N2))
   IF (REASSIGN_POINTER) THEN
      PRINT*, ' Reassigning p => t'
      p => t   ! Is this required ?

   ENDIF
   CALL RANDOM_NUMBER(t)

   PRINT*, ""
   ! Q:  Is p associated if t is deallocated?
   ! A:  Yes - it just points to memory occupied by t, which is now open for reassignment.
   DEALLOCATE(t,STAT=info)
   PRINT*, 'deallocating t'
   PRINT*, ' status = ', info
   IF (ASSOCIATED(p)) THEN ! Is p associated if t is deallocated ?
      PRINT*,' p is associated'
      PRINT*,' p(3) = ',p(3) ! Does this always crash ?
   ELSE
      PRINT*,' p is not associated'
   ENDIF

   PRINT*, ""
   NULLIFY(p)
   PRINT*, 'nullifying p'
   IF (ASSOCIATED(p)) THEN ! Is p associated if t is deallocated ?
      PRINT*,' p is associated'
      PRINT*,' p(3) = ',p(3) ! Does this always crash ? If p is nullified, yes
   ELSE
      PRINT*,' p is not associated'
   ENDIF



END PROGRAM main

