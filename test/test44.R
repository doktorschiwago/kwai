library(Rllvm)

library(bit64)

library(compiler)

m=parseAssembly("~/unison/dev/kwai/llvm_source/complete.ll")
ee = ExecutionEngine(m)


#browser()

hex2int64 = function(hex) {
	#browser()
	res=as.integer64(0)
	if (nchar(hex)>6) {
		res=hex2int64(substr(hex,0,nchar(hex)-6))*(256**3)
		hex=substr(hex,nchar(hex)-5, stop=nchar(hex))
	}
	res=res+as.integer64(strtoi(hex, base=16L))
}

symbolTable=readRDS("symbolTable.raw")

refSymbol="Rf_protect"
refAddress=hex2int64(substr(getSymbol(refSymbol),3,20))
print(substr(getSymbol(refSymbol),3,20))
print(refAddress)
refOffset=refAddress-hex2int64(symbolTable[[refSymbol]])
print(symbolTable[[refSymbol]])
print(refOffset)
#browser()

#loop over all global variable
globalList=c(getGlobalVariables(m), getModuleFunctions(m))

ownFunctions=""

for (global in names(globalList)) {

	#browser()
	
	
	#check if global is resolvable
	linkage=getLinkage(globalList[[global]])

	if (linkage == InternalLinkage) next
	if (global == "op_DFLTSUBSET2") next
	if (global == "op2_DFLTSUBSET2") next
	if (global == "llvm.dbg.declare") next

	#retrieve debug address

	globalAddress=symbolTable[[global]]
	print(paste("Symbol:", global, "at",globalAddress))

	if (! is.null(globalAddress)) {
		globalAddress2=hex2int64(globalAddress)+refOffset

		addGlobalMapping(ee, globalList[[global]], as.character(globalAddress2))
	} else if (substr(global,0,3) == "op_" ){
		
		ownFunctions=c(ownFunctions,global)
		
	} else {
		print(paste("unknown function:",global))
	}
	
	#print(globalAddress)
}

browser()
ownFunctions=ownFunctions[-1]

#retrieve pointer to our function
funcList=getModuleFunctions(m)


# getNativePointerToFunction returns a pointer that can be used by .C et al

readLines(file("stdin"), n=1)

# the MC JIT must be finalized before callin into the code
finalizeEngine(ee)

readLines(file("stdin"), n=1)

for (func in ownFunctions) {
	print(func)
	func2=funcList[[func]]
	fnPtr = getNativePointerToFunction(func2, ee)
	
	funcArgs=getParameters(func2)

	callArgs=list()
	callArgs[[1]]=fnPtr

	argCount=1
	for (funcArg in names(funcArgs)) {
		argCount=argCount+1
		if (funcArg=="rho") {
			callArgs[[argCount]]=globalenv()
		} else if (funcArg=="constant") {
			callArgs[[argCount]]=NULL
		} else {
			callArgs[[argCount]]=argCount
		}
	}

	#readLines(file("stdin"), n=1)
	
	#print(.Call(fnPtr,globalenv(), 1,2, NULL))
	print(callArgs)
	print(do.call(.Call,callArgs))
}

