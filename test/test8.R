


library(kwai)
source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/llvm_helper.R")
source("../R/llvm_native_helper.R")
source("../R/visitStackMachine3.R")
source("../R/discoverBlocks.R")
source("../R/lowerOps.R")
source("../R/inferTemporaries.R")

pisum = function() {
	#qq= 1:500
	#qq2=qq[[1]]
    for (j in 1:2) {
		#print("j")
		print(j)
    #    t = 0.0
        #for (k in 1:10000) {
		#	print("k")
		#	print(k)
        #    t = t + 1.0/(k*k)
		#	print("end k")
        #}
		#print("end j")
    }
    #return(t)
	return(1)
}

#debug(inferType2)
llvmFunc=byte2llvm(pisum, useNative=TRUE)
#readLines(file("stdin"), n=1)
print(pisum)
print(pisum())
print(llvmFunc())

