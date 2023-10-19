# cache the value of current working directory
NodeDir=$(realpath .)

StefanProblem="incompFlow/FlashExternal"

# Link private simulation directory
if [ -d "$FLASHX_HOME/source/Simulation/SimulationMain/$StefanProblem" ]; then
	rm -r $FLASHX_HOME/source/Simulation/SimulationMain/$StefanProblem
fi
mkdir -pv $FLASHX_HOME/source/Simulation/SimulationMain/$StefanProblem
ln -s $NodeDir/Config $FLASHX_HOME/source/Simulation/SimulationMain/$StefanProblem/.
ln -s $NodeDir/Makefile $FLASHX_HOME/source/Simulation/SimulationMain/$StefanProblem/.
ln -s $NodeDir/*.F90 $FLASHX_HOME/source/Simulation/SimulationMain/$StefanProblem/.
