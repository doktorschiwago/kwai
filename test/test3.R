

library(kwai)

source("../R/compile.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/visitStackMachine2.R")

myfunction2 <-function(a) {
	qq=if (a>2) {
		-1
	} else {
		a
	}
	return(qq)
}




#debug(inferType2)
llvmFunc=byte2llvm(myfunction2)
print(myfunction2)
print(myfunction2(3))
print(llvmFunc(3))

