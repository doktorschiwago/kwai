


library(kwai)
source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/llvm_helper.R")
source("../R/visitStackMachine3.R")
source("../R/discoverBlocks.R")
source("../R/lowerOps.R")

source("test14b.R", keep.source=TRUE)

#debug(inferType2)
llvmFunc=byte2llvm(pisum)
print(pisum)
print(pisum())
print(llvmFunc())
