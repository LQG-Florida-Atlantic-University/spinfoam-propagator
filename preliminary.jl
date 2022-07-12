required_pkg=["Conda","IJulia","DifferentialEquations","QuadGK","ForwardDiff","DataFrames","SymPy","LinearAlgebra","Plots","DynamicHMC","AdvancedHMC","Turing"]

using Pkg;

@time for x in required_pkg 
    try
        print("try to load ")
        printstyled(x*"\n"; color = :blue)
        eval(Meta.parse("using "*x))
    catch
        printstyled(x; color = :blue)
        println(" not found. "*"We will install it now")
        Pkg.add(x)
        eval(Meta.parse("using "*x))
    end
    printstyled(x; color = :blue)
    println(" loaded")
end

try
    println("Test if jupyter can be found")
    cmd=IJulia.find_jupyter_subcommand("lab");
catch
    error("JUPYTER is not installed, please rerun the script or install it by yourself")
end

println("All done. Have fun. \nYou can run the script again to see how long it takes to load the packages\n In julia, you can run \"using IJulia; jupyterlab() \" or \"using IJulia; notebook() \" to start jupyter.")