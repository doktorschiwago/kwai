

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
sexpType=getType(tpAny)
posList=list()
posList[[1]]=sexpType
posList[[2]]=sexpType
funcSignature=list(named=list(),pos=posList)

llvmFunc=byte2llvm(myfunction)

print(myfunction)
print(myfunction(-11,0))
print(llvmFunc(-11,0))

qq=function(x) {
	if (missing(x)) {
		print("missing x")
	}
	x=2
	if (missing(x)) {
		print("missing x")
	}
}

