

library(kwai)


source("../R/visitStackMachine2.R")

myfunction <- function(arg1,arg2){
	qq=arg1+arg2
	return(qq)
}


#debug(inferType2)
#llvmFunc=byte2llvm(myfunction)
print(myfunction)

cmpld = cmpfun(myfunction)
da_source = disassemble(cmpld)

vsmMockupFunc <- function(...) {

	args=list(...)
	print(op)
	print(args)
}

vsmHandler=list()

for (op in Opcodes) {
	vsmHandler[[op$op]]=vsmMockupFunc
	env=new.env()
	env$op=op$op
	parent.env(env)=environment(vsmHandler[[op$op]])
	environment(vsmHandler[[op$op]])=env
}

visitStackMachine2(
	source=da_source[2][[1]][-1],
	constants=da_source[3][[1]][-1],
	vsmHandler=vsmHandler)


