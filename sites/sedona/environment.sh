# Load MPI module. This should be available as standard module on a cluster.
# If not, build your own MPI and update PATH, LD_LIBRARY_PATH
module load openmpi-4.1.1

# Set MPI_HOME by quering path loaded by site module
export MPI_HOME=$(which mpicc | sed s/'\/bin\/mpicc'//)

# Load HDF5 module in desired configuration if available. If not specified
# the HDF5 will be built when setting up software
module load hdf5-1.8.20

# Path to parallel HDF5 installtion with fortran support
export HDF5_HOME=$(which h5pfc | sed s/'\/bin\/h5pfc'//)

module load flashxtest
