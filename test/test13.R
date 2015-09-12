myfunction <- function(arg1,arg2){
	qq=arg1+1
	qq2=arg1+1
	if (qq2>arg2) {
		return(qq)
	} else {
		return(-11)
	}
}

myfunction2 <-function(a) {
	return(myfunction(arg2=a+1,arg3=a-1))
}


library(kwai)
source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/llvm_helper.R")
source("../R/visitStackMachine3.R")

#debug(inferType2)
llvmFunc=byte2llvm(myfunction2)
print(myfunction2)
#print(myfunction2(-11))
print(llvmFunc(-11))

