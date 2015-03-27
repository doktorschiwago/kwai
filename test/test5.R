

library(kwai)

#debug(inferType2)
sexpType=getType(tpAny,FALSE)
posList=list()
posList[[1]]=sexpType
posList[[2]]=sexpType
funcSignature=list(named=list(),pos=posList)

llvmFunc=byte2llvm(myfunction,funcSignature)

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

