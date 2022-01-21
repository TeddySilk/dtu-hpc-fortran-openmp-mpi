seq: monte_pirlo_seq.f90
	mpif90 -free -O3 monte_pirlo_seq.f90 -o monte_pirlo_seq.e

mpi: monte_pirlo_mpi.f90
	mpif90 -free -O3 monte_pirlo_mpi.f90 -o monte_pirlo_mpi.e

rseq:
	rm -f monte_pirlo_seq.out
	rm -f timings_seq.dat
	monte_pirlo_seq.e

rmpi:
	rm -f monte_pirlo_mpi.out
	rm -f timings_mpi.dat
	mpirun -np 8 monte_pirlo-mpi.e