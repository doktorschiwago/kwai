myfunction <- function(arg1,arg2){
	qq=1
	#for (i in 1:arg1) {
		#print(i)
		#print(qq)
		print(arg2)
		qq=qq-arg2
		print(arg2)
		#print(qq)
	#}
	return(qq)
}

#STARTFOR

#init
#for seq is on stack
#init counter and push on stack
 
#loop_init
#pop counter from stack and add 1
#get nth entry from sequence

#NEXTFOR
#if counter=length sequence-> goto endfor
#else jump to loop_init

#ENDFOR
#pop counter and seq
#push null on stack



library(kwai)
source("../R/compile.R")
source("../R/typelib.R")
source("../R/inferType.R")
source("../R/createIR2.R")
source("../R/llvm_helper.R")
source("../R/visitStackMachine3.R")
source("../R/lowerOps.R")
source("../R/discoverBlocks.R")

#debug(inferType2)
llvmFunc=byte2llvm(myfunction)
print(myfunction)
print(myfunction(2,0))
print("-----")
print(llvmFunc(2,0))

