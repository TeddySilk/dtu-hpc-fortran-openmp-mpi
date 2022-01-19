hello_world: ping_pong_mpi.f90
	mpif90 -free -O3 ping_pong_mpi.f90 -o ping_pong.e

run:
	rm -f ping_pong.out
	rm -f timings.dat
	mpirun -np 2 ping_pong.e