

library(kwai)

myfunction <- function(arg1,arg2){
	qq=arg1+1
	qq2=arg1+1
	if (qq2>arg2) {
		return(qq)
	} else {
		return(-11)
	}
}


#debug(inferType2)
llvmFunc=byte2llvm(myfunction)
print(myfunction)
print(myfunction(-11,0))
print(llvmFunc(-11,0))

