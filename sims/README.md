# Scripts to run the simulations

The contents of this folder are:

**Simulation**

- `paramlists.R`: sets the parameters for each type of simulation
- `sim-helpers.R`: main function that runs the simulations based on a set of arguments
- `sim-run.R`: calls `sim-helpers.R` after setting up arguments for this simulation
- `sim-submit.R`: submission script that submits parallel `sim-run.R` calls based on the `paramlists.R`
- `shell.sh`: needed by the `sim-submit.R` script

**Helpers**

- `phi-func.R`: helper function to create the phi function based on simulation arguments
- `arg-construct.R`: helper functions for making arguments to the simulations

