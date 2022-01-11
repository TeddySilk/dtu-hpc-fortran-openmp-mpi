s="static"
for t in 1 2 4 8
do
hyperfine matrix_times_vector_$t.exe
hyperfine "OMP_WAIT_POLICY=active OMP_NUM_THREADS=$t OMP_SCHEDULE=$s matrix_times_vector.exe"
done