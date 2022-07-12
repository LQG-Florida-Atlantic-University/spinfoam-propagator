# Spin foam on Lefschetz thimbles with MCMC implementation

Here is the code for evaluating spin foam observables, more specifically spinfoam propagators, on Lefschetz thimbles with MCMC method. The original code is in Mathematica and the conversion to Julia is undergoing.

The code implemented the Differential Evolution Adaptive Metropolis (DREMA) method described in J. Vrugt, C. ter Braak, C. Diks, B. Robinson, J. Hyman, and D. Higdon, International Journal of Nonlinear Sciences and Numerical Simulation 10, 273 (2009), DOI:10.1515/IJNSNS.2009.10.3.273. Switch to other MCMC method is also possible

The boundary data can be generated using the code at https://github.com/LQG-Florida-Atlantic-University/extended_spinfoam
