PROGRAM main
   USE m_sub
   USE m_init

   REAL, DIMENSION(:), POINTER :: my_data
   INTEGER :: n, status

   n = 12
   CALL sub(my_data, n, status)
   PRINT*, 'status = ', status, ASSOCIATED(my_data)

   CALL init(my_data, n)
   DO i = 1, n
      PRINT*, 'my_data(i) = ', my_data(i)
   ENDDO

END PROGRAM main
