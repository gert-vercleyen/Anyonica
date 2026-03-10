using DelimitedFiles, PicoSAT, SparseArrays

fn   = joinpath( @__DIR__, ARGS[1] )

m    = readdlm( fn, ',', Int, '\n' )

A    = sparse( m[1,:], m[2,:], m[3,:] )

cnf  = [ findnz( A[i,:] )[2] for i in 1:size(A)[1] ]

soln = PicoSAT.itersolve(cnf);

open( joinpath( @__DIR__, "solutions.csv" ), "w" ) do io
    writedlm( io, collect(soln), ',' )
end
