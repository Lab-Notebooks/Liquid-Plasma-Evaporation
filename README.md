## Execution Environment for Multiphase Evaporation Tutorials

For instructions on how to use the notebook pelase see the README for these repositories:
- https://github.com/Lab-Notebooks/Outflow-Forcing-BubbleML
- https://github.com/akashdhruv/Jobrunner
Overview of steps to get this lab notebook running.

1. Install Jobrunner, `pip install pyjobrunner==2023.10.4 --user`, make sure `~/.local/bin` is in your path.
   
2. Create a new sites folder for you machine see `sites/<site-name>`
 
3. Copy `sites/sedona/environment.sh` and `sites/sedona/Makefile.h` to `sites/<site-name>`.

4. Edit `sites/<site-name>/environment.sh` to set `MPI_HOME` and `HDF5_HOME`. 
   If HDF5 is not installed on your machine do step 7. 

5. Run `configure`:
  ```
  ./configure -s <site-name>
  ```
  This will create `config.sh`


6. Setup software stack (`-V` is for verbose):
  ```
  jobrunner setup software/amrex -V 
  jobrunner setup software/flashx -V
  jobrunner setup software/flashkit -V
  ```

7. If HDF5 is not available on your local machine do:
  ```
  jobrunner setup  software/hdf5
  ```
   
8. Setup a test simulation:
  ```
  jobrunner setup simulation/StefanProblem/Example2D -V
  ```

9. Run the simulation
  ```
  jobrunner submit simulation/StefanProblem/Example2D
  ```

10. Postprocessing:
   ```
   cd simulation/StefanProblem/Example2D
   flashkit create xdmf --auto
   ```    
   This will create `INS_Stefan_Problem.xmf` that can be opened in paraview.
