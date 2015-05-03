library(kwai)

findme = function(x) {
	x+1
}

source("../R/compile.R")
source("../R/llvm_helper.R")
m = Module()

print(m)	


r_helper=new("r_module",m)


f2 = Function("fun2", SEXPType,  module = m)
b = Block(f2)
ir = IRBuilder(b)

ir$createReturn(r_findfun(r_helper, ir, r_helper@string_findme))


# Now we create the EEs
eeNew = ExecutionEngine(m, useMCJIT=TRUE)


# getNativePointerToFunction returns a pointer that can be used by .C et al
fnPtr = getNativePointerToFunction(f2, eeNew)

# the MC JIT must be finalized before callin into the code
finalizeEngine(eeNew)

browser()
qq=.C(fnPtr)
print(qq)

