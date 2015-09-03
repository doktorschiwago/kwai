

library(kwai)

source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/llvm_helper.R")

myfunction <- function(arg1,arg2){
	qq=2*arg1+arg2
	return(qq)
}


#debug(inferType2)
llvmFunc=byte2llvm(myfunction)
print(myfunction)
print(myfunction(-11,0))
print(llvmFunc(-11,0))

