MODULE m_diffuse

   USE m_precision
   USE m_arrays
   USE m_global
   USE m_io
   IMPLICIT NONE

   ! define temporary field array
   REAL(wp), DIMENSION(:,:), ALLOCATABLE :: temp_field

CONTAINS


   SUBROUTINE diffuse(field, nsteps, input_step)
      ! diffuse(field, nsteps, input_step) takes a field,
      ! and computes the diffusion of the field values
      ! according to the Poisson diffusion equation

      REAL(wp), DIMENSION(:, :), INTENT(INOUT) :: field
      INTEGER, INTENT(IN)                      :: nsteps
      INTEGER, OPTIONAL                        :: input_step

      ! local variables
      INTEGER             :: i, j, istep, iostat
      CHARACTER(LEN = 24) :: string
      LOGICAL             :: file_exists



      ! allocate temp_field array and initialize by copying
      CALL alloc(temp_field, SIZE(field, 1), SIZE(field, 2))
      CALL copy(field, temp_field)

      ! if starting step not given, start at 0
      IF (.NOT.PRESENT(input_step)) THEN
         input_step = 0
      ENDIF

      ! TIME STEPPING
      DO istep = input_step + 1, input_step + nsteps

         ! compute time-advanced field
         DO j = 2, ny - 1
            DO i = 2, nx - 1
               temp_field(i, j) = field(i, j) + dt * diff *&
                  (&
                  (field(i + 1, j) - 2 * field(i, j) + field(i - 1, j)) * rdx2 &
                  + (field(i, j + 1) - 2 * field(i, j) + field(i, j - 1)) * rdy2 &
                  )
            ENDDO
         ENDDO

         ! swap temp_field array (time-advanced) and field (old)
         CALL swap(temp_field, field)

         ! check if diagnostics should be output
         IF (MOD(istep, diag_freq).EQ.0.AND.save_diag) THEN
            CALL diagnostics(field, REAL(istep) * dt, diag_out)
         ENDIF

         ! check if binary field should be output
         IF (MOD(istep, bin_freq).EQ.0.AND.save_bin) THEN
            CALL extract_field(field, formatted=.FALSE., step=istep, output=bin_out)
         ENDIF

      ENDDO

      ! deallocate temp_field at the end
      DEALLOCATE(temp_field, STAT=iostat)
      CALL check_iostat(iostat)

   END SUBROUTINE diffuse

END MODULE m_diffuse

