myfunction <- function(arg1){
	qq=arg1+1
	qq2=arg1+1
	return(arg1)
}

myfunction2 <-function(a) {
	return(myfunction(a+1))
}


library(kwai)
source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/visitStackMachine3.R")
source("../R/llvm_helper.R")
source("../R/llvm_native_helper.R")

#debug(inferType2)
llvmFunc=byte2llvm(myfunction2)
print(myfunction2)
print(myfunction2(-11))
print(llvmFunc(-11))

