library(kwai)

findme = function(x) {
	x+1
}

source("../R/compile.R")
source("../R/llvm_helper.R")
m = Module()

print(m)	


r_helper=new("r_module",m)


f2 = Function("fun2", SEXPType,  list(x = SEXPType, y=SEXPType), module = m)
b = Block(f2)
ir = IRBuilder(b)

tmp=r_comp_eq(r_helper, ir, getParameters(f2)$x, getParameters(f2)$y)
r_unprotect(r_helper,ir,3)
ir$createReturn(tmp)


# Now we create the EEs
eeNew = ExecutionEngine(m, useMCJIT=TRUE)


# getNativePointerToFunction returns a pointer that can be used by .C et al
fnPtr = getNativePointerToFunction(f2, eeNew)

# the MC JIT must be finalized before callin into the code
finalizeEngine(eeNew)

browser()
qq=.Call(fnPtr,11,11)
print(qq)

