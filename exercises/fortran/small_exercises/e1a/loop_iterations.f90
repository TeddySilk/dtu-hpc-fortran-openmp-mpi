PROGRAM e1a
   IMPLICIT NONE
   INTEGER :: i, j, k = 0, n

   PRINT *, 'Enter j n'
   READ (*, *) j, n

   DO i = 1, n
      IF (i .EQ. j) THEN
      k = i + 10 ! changed from i = i + 10 to k = i + 10 to avoid reassignment of the looping variable inside the loop
      ENDIF
   END DO

   PRINT *, 'k = ', k
   
END PROGRAM e1a
