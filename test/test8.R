


library(kwai)
source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/llvm_helper.R")
source("../R/visitStackMachine3.R")
source("../R/discoverBlocks.R")
source("../R/lowerOps.R")

pisum = function() {
    t = 0.0
    for (j in 1:500) {
        t = 0.0
        for (k in 1:10000) {
            t = t + 1.0/(k*k)
        }
    }
    return(t)
}

#debug(inferType2)
llvmFunc=byte2llvm(pisum)
print(pisum)
print(pisum())
print(llvmFunc())

