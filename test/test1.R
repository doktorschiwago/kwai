

library(kwai)

source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR.R")
source("../R/typelib.R")
source("../R/llvm_helper.R")
source("../R/visitStackMachine2.R")

myfunction <- function(arg1,arg2){
	qq=1+2
	return(qq)
}


#debug(inferType2)
llvmFunc=byte2llvm(myfunction)
print(myfunction)
print(myfunction(-11,0))
print(llvmFunc(-11,0))

