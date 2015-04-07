# This is an R version of the example in the tutorial
#   http://llvm.org/releases/2.6/docs/tutorial/JITTutorial1.html
#
#
#

#library(Rllvm)
#library(compiler)
#library(ref)

#source("~/dev/scratch/tst3/Rllvm/R/mkFun.R")

#dyn.load("/usr/lib/llvm-3.5/lib/libLLVM-3.5.0.so")

options(error=traceback)

#source("typelib.R")
#source("visitStackMachine.R")
#source("inferType.R")
#source("createIR.R")



discoverBlocks <-function(source) {
	currentBlock=list()
	currentBlock$start=2
	currentBlock$firstBlock=TRUE
	currentBlock$deps=list()
	currentBlock$terminatingBranch=FALSE
	
	blockList=list()
	blockList[[2]]<-currentBlock

	print(length(blockList[2]))
	print(length(currentBlock))

	i=2
	length=length(source)

	branch=-1

	while (i<=length) {
		op=as.character(source[i][[1]])

		branch=-1
		fallthrough=-1
		switch(op,
	        "BRIFNOT.OP" = {
				branch=source[i+2][[1]]+1
				fallthrough=1
				},
	        "GOTO.OP" = {
				branch=source[i+1][[1]]+1
				fallthrough=0
				},
			"RETURN.OP" = {
				branch=0
				fallthrough=0
			}
		)
		
		if (branch > -1) {
			if (op != "RETURN.OP") {
				if (is.null(blockList[branch][[1]])) {
					blockList[[branch]]=list()
					blockList[[branch]]$deps=list()
					blockList[[branch]]$start=branch
					blockList[[branch]]$terminatingBranch=FALSE
				}
				block=blockList[branch]
				#browser()
				if (is.null(block$deps[currentBlock$start])) {
					blockDeps=blockList[branch]$deps
					blockDeps[[length(blockDeps)+1]]=i
					blockList[[branch]]$deps=blockDeps
				}
			}
		}
		i=i+1+Opcodes.argc[[as.character(source[i][[1]])]]	

		if (i<=length) {
			if (branch > -1) {
				if (is.null(blockList[i][[1]])) {
					blockList[[i]]=list()
					blockList[[i]]$deps=list()

					blockList[[i]]$start=i
					blockList[[i]]$terminatingBranch=FALSE
				}
				if (fallthrough == 1) {
					blockDeps=blockList[i]$deps
					blockDeps[[length(blockDeps)+1]]=i-1
					blockList[[i]]$deps=blockDeps
				}
			} else if (! is.null(blockList[i][[1]])) {
				#so theres a new block, but no branch 
				blockDeps=blockList[[i]]$deps
				blockDeps[[length(blockDeps)+1]]=i-1
				blockList[[i]]$deps=blockDeps
			}
		}
	}

	#browser()

	i=2
	j=1
	blockList2=list()
	blockList2[[j]]=currentBlock

	while (i<=length) {
		if (i==length(blockList)) {
			break
		} else if (! is.null(blockList[[i+1]])) {
			currentBlock$end=i
			blockList2[[j]]=currentBlock
			currentBlock=blockList[[i+1]]
			j=j+1

		}
		i=i+1
	}
	currentBlock$end=length
	blockList2[[j]]=currentBlock

	length=length(blockList2)

	#browser()

	for ( i in 1:length) {
		block=blockList2[[i]]
		if (length(block$deps)>0) {
			for (j in 1:length(block$deps)) {
				for (k in 1:length) {
					if ((blockList2[[k]]$start <= block$deps[j]) && (blockList2[[k]]$end >= block$deps[j])) {
						blockList2[[i]]$deps[j]=k
						
						break
					}
				}
			}
		}
	}
	i=2
	length=length(source)

	branch=-1

	#browser()

	while (i<=length) {
		op=as.character(source[i][[1]])

		branchIns=-1
		switch(op,
	        "BRIFNOT.OP" = {
				branchIns=i+2
				},
	        "GOTO.OP" = {
				branchIns=i+1
				},
			"RETURN.OP" = {
				branchIns=i
				}
		)
		
		if (branchIns > -1) {
			if (op != "RETURN.OP") {
				target=source[branchIns][[1]]+1
				found2=FALSE
			} else {
				found2=TRUE
			}

			found=FALSE
			#browser()
			for (j in 1:length(blockList2)) {
				if (blockList2[[j]]$end == branchIns) {
					found=TRUE
					blockList2[[j]]$terminatingBranch=TRUE
				}
				if (op != "RETURN.OP") {
					if (blockList2[[j]]$start == target) {
						found2=TRUE
						source[branchIns][[1]]=j
					}
				}
			}
			if (! found) stop("Temrinating Branch could not be set")
			if (! found2) stop("Error label could not be replaced with block")
		}
		#browser()
		i=i+1+Opcodes.argc[[as.character(source[i][[1]])]]	

	}

	res=list()
	res$blockList=blockList2
	res$source=source

	return(res)
}







printBlockSource <- function(op,insNo,insList,insNo2,args,types) {
	cat(insNo, " ", op, " ")
	cat("\tArguments: ", args, "\n")
	cat("\t Type Name: ", types[[insNo]]$name, ", needed Type: ", types[[insNo]]$neededType$name, "\n\n")
	return("xx")
}



printBlock <- function(block, no, source) {
	print("")
	print(paste("Block ",no))
	print("Block pos:")
	print(block$start)
	print(block$end)
	print(block$deps)

	printBlockSource2 <- function(op,insNo,insList,insNo2,args) {
		printBlockSource(op,insNo,insList,insNo2,args,block$typeInformation)
	}

	#creatin a fake init Stack
	#browser()
	initStack=list(stackPos=10, stackOnExit=rep(list(), 10))

	visitStackMachine(source,printBlockSource2, initStack)
	
}



attachLineInformation2 <- function(elements,lineNumbers,default) {
	for (i in 1:length(elements)) {
		currentElement=elements[[i]]
		if (typeof(currentElement) == "language") {

			if (length(currentElement) > 1)  {
				if (is.null(lineNumbers)) {
					currentElement=attachLineInformation2(currentElement,attr(elements[[i]],"srcref"),default)
				} else {
					currentElement=attachLineInformation2(currentElement,attr(elements[[i]],"srcref"),lineNumbers[[i]])
				}
			}
			#browser()
			if (is.null(lineNumbers)) {
				#currentElement$srcref=default
				attr(currentElement,"srcref")=default
			} else {
				#currentElement$srcref=lineNumbers[[i]]
				attr(currentElement,"srcref")=lineNumbers[[i]]
			}

			elements[[i]]=currentElement
		}
	}
	return(elements)
}

attachLineInformation <- function(func) {
	functionElements=body(func)
	functionLineNumbers=attr(functionElements,"srcref")

	if (is.null(functionLineNumbers)) {
		return(func)
	}

	newBody=attachLineInformation2(functionElements,functionLineNumbers,NULL)
	body(func)=newBody

	return(func)
}

is.compile <- function(func)
{
	# this function lets us know if a function has been byte-coded or not
	#If you have a better idea for how to do this - please let me know...
    if(class(func) != "function") stop("You need to enter a function")
    last_2_lines <- tail(capture.output(func),2)
    any(grepl("bytecode:", last_2_lines)) # returns TRUE if it finds the text "bytecode:" in any of the last two lines of the function's print
}

initCompileContext <- function(func,funcName) {
	#reading function Signature


	#save ans line filename of function
	absPath=normalizePath(getSrcFilename(func))
	funcLine=func

	#then attaching line information for debugging
	func=attachLineInformation(func)
	
	if (is.compile(func)) {
		compiledFunc=func
	} else {
		compiledFunc=cmpfun(func)
	}

	disasm=disassemble(compiledFunc)
	source=disasm[2][[1]]
	

	res=discoverBlocks(source)
	blockList=res$blockList

	res=list(blockList=blockList,argList=names(formals(func)),source=res$source,constants=disasm[3][[1]],
		absPath=absPath, func=func, funcName=funcName)

	#browser()
	return(res)

}

inferFunctionType = function(context, funcSignature=NULL) {

	blockList=context$blockList
	argList=context$argList
	source=context$source
	constants=context$constants
	absPath=context$absPath

	args=list()

	llvmSignature=list()

	vars=list()
	#browser()


	for (i in 1:length(argList)) {
		arg=list()
		arg$name=argList[i]
		
		if (is.null(funcSignature)) {
			arg$returnType=list(name=tpAny,vector=FALSE)
		} else {
			if (length(funcSignature$named) >0) {
				if (is.null(funcSignature$named[[argList[i]]])) {
					arg$returnType=list(name=tpMissing,vector=FALSE)
				} else {
					arg$returnType=funcSignature$named[[argList[i]]]
				}
			} else {
				if (is.null(funcSignature$pos[i][[1]])) {
					arg$returnType=list(name=tpMissing,vector=FALSE)
				} else {
					arg$returnType=funcSignature$pos[[i]]
				}
			}
		}
			
		

		arg$pos=i

		#print(argList[i])
		args[[arg$name]]=arg

		if (arg$returnType$name != tpMissing) {
			llvmSignature[[arg$name]]=rType2Llvm(arg$returnType)
		}

		vars[[arg$name]]=arg$returnType
	}

	returnType=NULL
	cntLoops=0
	returnTypeChanged=TRUE

	for (i in 1:length(blockList)) {
		blockList[[i]]$typeInformation=list()
	}

	initStackList=list()
	
	while (returnTypeChanged || changed) {
		cntLoops=cntLoops+1
		returnTypeChanged=FALSE

		
	
		for (i in 1:length(blockList)) {
		
			vars2=as.ref(vars)
			changed=FALSE
			changed2=as.ref(changed)

			initStack=list(stackOnExit=list(),stackPos=0)
			tpInfoInit=list()

			#checkin wheethe an initial stack state must be supplied
			if (! (length(blockList[[i]]$deps)==0)) {
				#browser()
				for (j in blockList[[i]]$deps) {
					if (! is.null(initStackList[j][[1]]	)) {
						if (initStack$stackPos == 0) {
							initStack=initStackList[[j]]
							for (k in (1:initStack$stackPos)) {
								tpInfoInit[[k]]=blockList[[j]]$typeInformation[[initStack$stackOnExit[[k]]]]
								initStack$stackOnExit[[k]]=k
							}
						} else if  (initStack$stackPos != initStackList[[j]]$stackPos ){
							stop("initStack length differs between deps!")
						} else {
							for (k in (1:initStack$stackPos)) {
								tpInfoInit[[k]]=higherType(tpInfoInit[[k]],blockList[[j]]$typeInformation[[initStack$stackOnExit[[k]]]])
							}
						}
					}
				}
			}
			#browser()
			typeInformation=c(tpInfoInit,blockList[[i]]$typeInformation)
			typeInformation2=as.ref(typeInformation)
			myInferType<-function(op,insNo,insList,insNo2,args2) {
				inferType2(op,insNo,insList,insNo2,args2,vars2,constants,typeInformation2,changed2)
			}


			#browser()
			initStackList[[i]]=visitStackMachine(source[blockList[[i]]$start:blockList[[i]]$end],myInferType,initStack)
			#browser()

			if (initStackList[[i]]$stackPos == 0) {
				initStackList[[i]]=NULL
				blockList[[i]]$typeExitStack=NULL
			} else {
				blockList[[i]]$typeExitStack=initStackList[[i]]
			}

			blockList[[i]]$typeInformation=typeInformation
			

			printBlock(blockList[[i]], i, source[blockList[[i]]$start:blockList[[i]]$end])

		
			if (as.character(source[blockList[[i]]$end]) == "RETURN.OP" || i==length(blockList)) {
				returnType2=typeInformation[[blockList[[i]]$end-blockList[[i]]$start+1]]
				if (is.null(returnType)) {
			
					returnType=returnType2
				} else if (! typesMatch(returnType,returnType2)) {

					returnType=higherType(returnType,returnType2)
					returnTypeChanged=TRUE
				}
			}

		}
		print(vars)
		#browser()
		
		if (returnTypeChanged) {
			for (i in 1:length(blockList)) {
				if (as.character(source[blockList[[i]]$end]) == "RETURN.OP" || i==length(blockList)) {
					typeInformation=blockList[[i]]$typeInformation
					returnType2=typeInformation[[blockList[[i]]$end-blockList[[i]]$start+1]]
					returnType2$neededType=returnType
					typeInformation[[blockList[[i]]$end-blockList[[i]]$start+1]]=returnType2
					blockList[[i]]$typeInformation=typeInformation
					#browser()
				}
			}
		}			
	}	

	res=context
	res$args=args
	res$llvmSignature=llvmSignature
	res$returnType=returnType
	res$vars=vars
	res$blockList=blockList
	return(res)
}

initLlvmContext = function(modName, absPath) {
	InitializeNativeTarget()

   # Create the module which will house the function
	mod = Module(modName)

	#creating the debug builder
	#browser()
	debugBuilder = DIBuilder(mod)
	#debugDescriptor=newDebugDescriptor(debugBuilder, debugFile)

	debugDouble=newDebugBasicType(debugBuilder, "double", 64, 64, 4 ) #4 = dwarf::DW_ATE_float
	debugSEXP=newDebugPointerType(debugBuilder, debugDouble, "SEXP") #1 = DW_ATE_address
	#debugDouble="q"
	rType2LlvmDebug <- function(rType) {
		switch(rType$name,
			"numeric" = { debugDouble },
			"any" = { debugSEXP },
			{stop(paste("Type",rType,"is unknown"))}
		)
	}

	CCodeType=functionType(SEXPType,c(SEXPType,SEXPType,SEXPType,SEXPType),0)

	#FUNTAB Type


	FunTabType = structType(list(
		name = StringType,
		cfun = pointerType(CCodeType),
		code = Int32Type,
		eval = Int32Type,
		kind = Int32Type,
		precedence = Int32Type,
		rightassoc = Int32Type	
	), "FunTab") 


	#Resolve external FUNTAB symbol
	R_FunTabSym = getNativeSymbolInfo("R_FunTab")
	llvmAddSymbol(R_FunTabSym) # $address)
	R_FunTab = createGlobalVariable("R_FunTab", mod, FunTabType)

	#Resolve external Rf_lcons symbol
	Rf_lconsType=functionType(SEXPType,c(SEXPType,SEXPType),0)
	Rf_lconsSym = getNativeSymbolInfo("Rf_lcons")
	llvmAddSymbol(Rf_lconsSym) # $address)
	Rf_lcons = createGlobalVariable("Rf_lcons", mod, Rf_lconsType)

	#R_NilValue
	R_NilValueSym = getNativeSymbolInfo("R_NilValue")
	llvmAddSymbol(R_NilValueSym) # $address)
	R_NilValue = createGlobalVariable("R_NilValue", mod, SEXPType)

	#Resolve external Rf_protect
	Rf_protectType=functionType(SEXPType,c(SEXPType),0)
	Rf_protectSym = getNativeSymbolInfo("Rf_protect")
	llvmAddSymbol(Rf_protectSym) # $address)
	Rf_protect = createGlobalVariable("Rf_protect", mod, Rf_protectType)

	#Resolve external Rf_unprotect
	Rf_unprotectType=functionType(VoidType,c(Int32Type),0)
	Rf_unprotectSym = getNativeSymbolInfo("Rf_unprotect")
	llvmAddSymbol(Rf_unprotectSym) # $address)
	Rf_unprotect = createGlobalVariable("Rf_unprotect", mod, Rf_unprotectType)

	#Resolve external Rf_allocVector
	Rf_allocVectorType=functionType(SEXPType,c(Int32Type, Int32Type),0)
	Rf_allocVectorSym = getNativeSymbolInfo("Rf_allocVector")
	llvmAddSymbol(Rf_allocVectorSym) # $address)
	Rf_allocVector = createGlobalVariable("Rf_allocVector", mod, Rf_allocVectorType)

	#Resolve external Rf_duplicate
	Rf_duplicateType=functionType(SEXPType,c(SEXPType),0)
	Rf_duplicateSym = getNativeSymbolInfo("Rf_duplicate")
	llvmAddSymbol(Rf_duplicateSym) # $address)
	Rf_duplicate = createGlobalVariable("Rf_duplicate", mod, Rf_duplicateType)	

	opAdd = createGlobalVariable("opAdd", mod,type=SEXPType,linkage=PrivateLinkage)
	setInitializer(opAdd,getNULLPointer(SEXPType))

	opMul = createGlobalVariable("opMul", mod,type=SEXPType,linkage=PrivateLinkage)
	setInitializer(opMul,getNULLPointer(SEXPType))

	opGt = createGlobalVariable("opGt", mod,type=SEXPType,linkage=PrivateLinkage)
	setInitializer(opGt,getNULLPointer(SEXPType))

	setOpFunArgTypes=c(add=SEXPType, mul=SEXPType, gt=SEXPType)
	setOpFun = Function("setOp", VoidType, setOpFunArgTypes , mod)

	params = getParameters(setOpFun)  

	block = Block(setOpFun)  
	ir = IRBuilder(block)

	createStore(ir,params$add,opAdd)
	createStore(ir,params$mul,opMul)
	createStore(ir,params$gt,opGt)

	createReturnVoid(ir)
	
	

	anyBinOp=function(ir,x,y,opNumber,op,cntProtected) {

		R_NilValue2=createLoad(ir,R_NilValue)

 		tmp=createCall(ir,Rf_lcons,y,R_NilValue2)
		createCall(ir,Rf_protect,tmp)
		argList=createCall(ir,Rf_lcons,x,tmp)
		createCall(ir,Rf_protect,tmp)

		op2=createLoad(ir,op)
		callPointer=createGEP(ir,R_FunTab,c(createConstant(ir,opNumber),createConstant(ir,1L)))

		callPointer2=createLoad(ir,callPointer)
		plusCall=createCall(ir,callPointer2,R_NilValue2,op2,argList,R_NilValue2)
		createCall(ir,Rf_protect,plusCall)
		
		createStore(ir,binOp(ir, Add, createLoad(ir,cntProtected),3L),cntProtected)
		plusCall
	}	

	debugCompUnit = newDebugCU(debugBuilder, basename(absPath), dirname(absPath))
	#

	res=list(debugBuilder=debugBuilder, rType2LlvmDebug=rType2LlvmDebug, mod=mod,
		R_NilValue=R_NilValue, Rf_protect=Rf_protect, Rf_unprotect=Rf_unprotect,
		setOpFun=setOpFun, setOpFunArgTypes=setOpFunArgTypes, debugCompUnit=debugCompUnit,
		Rf_duplicate=Rf_duplicate, Rf_allocVector=Rf_allocVector,anyBinOp=anyBinOp,
		opAdd=opAdd, opGt=opGt)
}

compile2llvm = function(context, llvmContext) {

	absPath=context$absPath
	argList=context$argList
	args=context$args
	llvmSignature=context$llvmSignature
	returnType=context$returnType
	func=context$func
	blockList=context$blockList
	vars=context$vars
	source=context$source
	constants=context$constants
	
	debugBuilder=llvmContext$debugBuilder
	rType2LlvmDebug=llvmContext$rType2LlvmDebug
	mod=llvmContext$mod
	debugCompUnit=llvmContext$debugCompUnit
	Rf_duplicate=llvmContext$Rf_duplicate
	Rf_allocVector=llvmContext$Rf_allocVector
	Rf_protect=llvmContext$Rf_protect
	anyBinOp=llvmContext$anyBinOp
	opAdd=llvmContext$opAdd
	opGt=llvmContext$opGt



	debugSignature=list()

	for (i in 1:length(argList)) {
		arg=args[[argList[i]]]

		debugSignature[i]=rType2LlvmDebug(arg$returnType)

	}

	fun = Function(context$funcName, rType2Llvm(returnType), llvmSignature, mod)

	debugSignature=c(rType2LlvmDebug(returnType),debugSignature)
	debugFunSignature=newDebugFunctionType(debugBuilder, debugSignature, debugCompUnit)

	debugFun=newDebugFunction(debugBuilder, debugCompUnit, fun, debugFunSignature, attr(body(func)[[2]],"srcref")[1])
		
	#browser()
	
	
	params = getParameters(fun)  # get the names. Set them too.

	for (i in 1:length(blockList)) {
		blockList[[i]]$block=Block(fun)
	}

	initStackList=list()

	for (i in 1:length(blockList)) {
		block=blockList[[i]]
		ir = IRBuilder(block$block)

		if (i==1) {


			
			cntProtected=createLocalVariable(ir,Int32Type, "__cntProtected")
			
			#for (j in 1:length(params)) {
			#	varName=names(params)[j]
			#	newVector=createCall(ir,Rf_duplicate,params[[varName]])
			#	params[[varName]]=createCall(ir,Rf_protect,newVector)
			#}
			#createStore(ir,val=as.integer(length(params)),cntProtected)
			createStore(ir,val=0L,cntProtected)

			for (j in 1:length(vars)) {
				varName=names(vars)[j]
				if (is.null(llvmSignature[[varName]])) {
					params[[varName]]=createLocalVariable(ir,rType2Llvm(vars[[varName]]),varName)
					arg_index=0;
				} else {
					arg_index=args[[varName]]$pos
				}
				#browser()
				#if (is.null(llvmSignature[[varName]])) {
				newDebugLocalVariable(debugBuilder, ir, debugFun, params[[varName]], attr(body(func)[[2]],"srcref")[1], debugCompUnit, 
					rType2LlvmDebug(vars[[varName]]), arg_index)
				#}
			}
		}

		blockOps=ops
		blockOps[[tpAny]]=list(
			coerce=function(x,type) {
				#browser()
				switch(type$name,
					any={x},
					numeric={
						if (type$vector) {
							stop("numeric vectors not supported")
						}
						newVector=createCall(ir,Rf_allocVector,createConstant(ir,14L),createConstant(ir,1L))
						#newVector=createCall(ir,Rf_allocVector,14L,1L)
						createCall(ir,Rf_protect,newVector)
						newVector3=createBitCast(ir, newVector,pointerType(DoubleType))
						#data=createGEP(ir,newVector3,c(createConstant(ir,0L),createConstant(ir,9L)))
						data=createGEP(ir,newVector3,c(createConstant(ir,5L)))
						createStore(ir,x,data)
						createStore(ir,binOp(ir, Add, createLoad(ir,cntProtected),1L),cntProtected)
						newVector
					},
					{stop(paste("no coercion from",type$name,"to any"))}
				)
			},
			readVar=function(ir,x) {
				blockOps[[tpAny]]$dup(ir,x)
			},
			dup=function(ir,x) {
				newVector=createCall(ir,Rf_duplicate,x)
				createCall(ir,llvmContext$Rf_protect,newVector)
				createStore(ir,binOp(ir, Add, createLoad(ir,cntProtected),1L),cntProtected)
				newVector
			},
			mul=function(ir,x,y) {
				anyBinOp(ir,x,y,62L,opMul,cntProtected)
			},
			add=function(ir,x,y) {
				anyBinOp(ir,x,y,62L,opAdd,cntProtected)
			},
			gt=function(ir,x,y) {
				anyBinOp(ir,x,y,72L,opGt,cntProtected)
			},
			cleanup=function(ir) {
				createCall(ir,llvmContext$Rf_unprotect,createLoad(ir,cntProtected))
			}
		)

		blockOps[[tpLogical]]=list(
			coerce=function(x,type) {
				#browser()
				switch(type$name,
					any={
						if (type$vector) {
							stop("numeric vectors not supported")
						}
						value=createBitCast(ir, x,pointerType(Int32Type))
						data=createGEP(ir,value,c(createConstant(ir,10L)))
						data2=createLoad(ir,data)
						createTrunc(ir,data2,getIntegerType(1))
					},
					logical={x},
					{stop("in the name of love")}
				)
			},
			mul=function(ir,x,y) {
				{stop("in the name of love")}
			},
			add=function(ir,x,y) {
				{stop("in the name of love")}
			},
			gt=function(ir,x,y) {
				{stop("in the name of love")}
			}
		)





		myCompiler <- function(op,insNo,insList,insNo2,args) {
			createIR(op,insNo,insList,insNo2,args,ir,params,constants,blockList,i,blockOps,vars, llvmContext, debugFun)
		}

		#browser()
		initStack=list(stackOnExit=list(),stackPos=0)
		#stop("continue here.......")

		#checkin wheethe an initial stack state must be supplied
		if (length(blockList[[i]]$deps) == 1) {
			if (blockList[[i]]$deps[[1]]>i) {
				stop("block depends on higher block!")
			}
			initStack=initStackList[[blockList[[i]]$deps[[1]]]]
		}
		else if (length(blockList[[i]]$deps) > 1) {
			#browser()
			for (j in blockList[[i]]$deps) {
				if (j>i) {
					stop("block depends on higher block!")
				}
				if (!is.null(initStackList[j][[1]]	)) {
					browser()
					if (initStack$stackPos == 0) {
						initStack=initStackList[[j]]
						for (k in (1:initStack$stackPos)) {
							rType=blockList[[j]]$typeInformation[[blockList[[j]]$typeExitStack$stackOnExit[[k]]]]
							llvmType=rType2Llvm(rType)
							initStack$stackOnExit[[k]]=createPHI(ir,llvmType,length(blockList[[i]]$deps))
							PHIAddIncoming(initStack$stackOnExit[[k]], initStackList[[j]]$stackOnExit[[k]],blockList[[j]]$block)
						}
					} else if  (initStack$stackPos != initStackList[[j]]$stackPos ){
						stop("initStack length differs between deps!")
					} else {
						for (k in (1:initStack$stackPos)) {
							PHIAddIncoming(initStack$stackOnExit[[k]], initStackList[[j]]$stackOnExit[[k]],blockList[[j]]$block)
						}
					}
				}
			}
		}
		initStackList[[i]]=visitStackMachine(source[blockList[[i]]$start:blockList[[i]]$end], myCompiler, initStack)
		
		if (i<length(blockList) & (!(block$terminatingBranch))) {
			createBranch(ir,blockList[[i+1]]$block)
		}		
	}

	return(fun)
}

makeRFunction2 =
  #
  # Create an R function that wraps an LLVM function Call (via .Call) in a R function
  #
  #
function(func, .ee = ExecutionEngine(as(func, "Module")))
{
  params = getParameters(func)
  i = is.na(params@names) | params@names == ""
  if(any(i))
    params@names[i] = sprintf("var%d", which(i))

  parms = alist(a= )
  parms = structure(replicate(length(params@names), parms), names = params@names)
  
  f = function() {}
  formals(f) = parms
  formals(f)[["..."]] = parms[[1]]

	ptr=getNativePointerToFunction(func,.ee)
  
  e = quote(.args <- a)
  al = quote(.Call())
	al[[2]] = ptr
  for(i in 1:length(params@names))
     al[[2 + i]] = as.name(params@names[i])
  e[[3]] = al

	

  
  body(f)[[2]] = e 
  
  #body(f)[[3]] =  substitute(.Call(ptr, .args = .args, .ee = .ee, ...), list(func = func, .ee = .ee))
  f
}

finishCompilation <- function(fun, llvmContext, llvmSignature) {	
	
	mod=llvmContext$mod
	setOpFun=llvmContext$setOpFun
	setOpFunArgTypes=llvmContext$setOpFunArgTypes
	debugBuilder=llvmContext$debugBuilder

	showModule(mod)
	if (!verifyModule(mod)) {
		print("juhu")
	} else {
		print("ohno")
	}

	
	finalizeDIBuilder(debugBuilder)
	engine=ExecutionEngine(mod, useMCJIT=TRUE)
	finalizeEngine(engine)
	
	

	setOpFunR=makeRFunction2(setOpFun,.ee=engine)
	res<-makeRFunction2(fun,.ee=engine)
	#browser()
	setOpFunR( add=.Primitive("+"), mul=.Primitive("*"), gt=.Primitive(">"))
	print("finish compilation")

	#browser()

	return(res)
}


byte2llvm = function(func) {
	#browser()
	funcName=deparse(substitute(func))
	ctxt=initCompileContext(func,funcName)
	ctxt=inferFunctionType(ctxt)

	llvmContext=initLlvmContext(funcName, ctxt$absPath)
	llvmFun=compile2llvm(ctxt,llvmContext)
	res=finishCompilation(llvmFun,llvmContext, ctxt$llvmSignature)
	return(res)
}
