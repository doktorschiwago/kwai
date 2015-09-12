

library(kwai)

source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/typelib.R")
source("../R/llvm_helper.R")
source("../R/visitStackMachine3.R")
source("../R/discoverBlocks.R")

source("test4b.R", keep.source=TRUE)


#debug(inferType2)
llvmFunc=byte2llvm(myfunction)
print(myfunction)

gctorture(on = TRUE)
print(myfunction(-11,0))
print(llvmFunc(-11,0))



qq=11
#ff=32

print(llvmFunc(-11,0))
gctorture(on = FALSE)

