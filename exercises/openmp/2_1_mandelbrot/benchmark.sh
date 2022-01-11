s="static"
file=mandelbrot
for t in 1 2 4 8
do
hyperfine "OMP_WAIT_POLICY=active OMP_NUM_THREADS=$t OMP_SCHEDULE=$s $file"
done