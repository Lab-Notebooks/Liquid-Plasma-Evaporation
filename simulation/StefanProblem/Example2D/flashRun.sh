# chdir into working directory
cd $JobWorkDir
echo Running on $SiteName
mpirun job.target
