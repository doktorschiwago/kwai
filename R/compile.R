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








printBlockSource <- function(opName, opNumber="NA", ...) {
	cat(opNumber, " ", opName, "\n")
	
	args=list(...)

	if (! is.null(args[["goto"]])) {
		cat("\tgoto: ", args$goto,"\n")
	}
	#cat("\tArguments: ", args, "\n")
	#cat("\t Type Name: ", tpGetName(types[[insNo]],TRUE), ", needed Type: ", tpGetName(tpGetNeededType(types[[insNo]], TRUE), TRUE), "\n\n")
	cat("\n")	
	return("xx")
}



printBlock <- function(block, no="unknown") {
	print("")
	print(paste("Block ",no))
	print("Block pos:")
	print(block$start)
	print(block$end)
	print(block$deps)

	printBlockSource2 <- function(...) {
		printBlockSource(...)
	}

	printBlockSourceHandler=createStdHandler(printBlockSource2)

	#creatin a fake init Stack
	#browser()
	#initStack=list(stackPos=10, stackOnExit=rep(list(), 10), insStart=0)

	visitStackMachine3(block,printBlockSourceHandler, initStack="xxx")

	invisible()
	
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
	source=disasm[2][[1]][-1]
	constants=disasm[3][[1]]

	opList=renderStackMachine(source,constants)
	
	opList2=lowerOps(opList)

	#browser()

	blockList=discoverBlocks2(opList2)

	res=list(blockList=blockList,argList=names(formals(func)),
		absPath=absPath, func=func, funcName=funcName)

	#browser()
	return(res)

}

inferFunctionType = function(context, funcSignature=NULL, forcedReturnType=NULL) {

	argList=context$argList
	source=context$source
	blockList=context$blockList
	constants=context$constants

	args=list()
	llvmSignature=list()

	vars=list()
	
	if (! is.null(argList)) {
		for (i in 1:length(argList)) {
			arg=list()
			arg$name=argList[i]

			#browser()
		
			if (is.null(funcSignature)) {
				arg$returnType=getType(name=tpAny)
			} else {
				if (length(funcSignature$named) >0) {
					if (is.null(funcSignature$named[[argList[i]]])) {
						arg$returnType=getType(name=tpMissing)
					} else {
						arg$returnType=funcSignature$named[[argList[i]]]
					}
				} else {
					if (is.null(funcSignature$pos[i][[1]])) {
						arg$returnType=getType(name=tpMissing)
					} else {
						arg$returnType=funcSignature$pos[[i]]
					}
				}
			}
			
		

			arg$pos=i

			#print(argList[i])
			browser()
			args[[arg$name]]=arg

			#browser()

			if (tpGetName(arg$returnType) != tpMissing) {
				llvmSignature[[arg$name]]=rType2Llvm(arg$returnType)
			}

			vars[[arg$name]]=arg$returnType

		}
	}
	

	#browser()

	funs=list()

	for (i in 1:length(blockList)) {
		vm=createVarHandler(vars,funs, list())
		visitStackMachine3(blockList[[i]]$opTable,vm, "none")
		vars=environment(vm$SETVAR.OP)$vars
		funs=environment(vm$SETVAR.OP)$funs
		strings=environment(vm$SETVAR.OP)$argNames
	}

	#browser()

	res=context
	res$args=args
	res$llvmSignature=llvmSignature
	res$returnType=getType(name=tpAny)
	res$vars=vars
	res$funs=funs
	res$strings=strings
	#res$blockList=blockList
	return(res)
}


initLlvmContext = function(modName, absPath) {
	InitializeNativeTarget()

   # Create the module which will house the function
	#mod = Module(modName)
	#browser()
	r_helper=new("r_module",modName)

	#creating the debug builder
	#browser()
	debugBuilder = DIBuilder(r_helper$mod)
	#debugDescriptor=newDebugDescriptor(debugBuilder, debugFile)

	debugDouble=newDebugBasicType(debugBuilder, "double", 64, 64, 4 ) #4 = dwarf::DW_ATE_float
	debugSEXP=newDebugPointerType(debugBuilder, debugDouble, "SEXP") #1 = DW_ATE_address
	#debugDouble="q"
	rType2LlvmDebug <- function(rType) {
		switch(tpGetName(rType),
			"numeric" = { debugDouble },
			"any" = { debugSEXP },
			{stop(paste("Type",rType,"is unknown"))}
		)
	}

	debugCompUnit = newDebugCU(debugBuilder, basename(absPath), dirname(absPath))
	#

	res=list(debugBuilder=debugBuilder, rType2LlvmDebug=rType2LlvmDebug, mod=r_helper$mod,
		r_helper=r_helper,
		debugCompUnit=debugCompUnit)
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

	r_helper=llvmContext$r_helper

	funs=context$funs


	globalVarList=createVarList(r_helper$mod,vars, funs, context$strings)

	debugSignature=list()

	#browser()

	if (! is.null(argList)) {

		for (i in 1:length(argList)) {
			arg=args[[argList[i]]]

			debugSignature[i]=rType2LlvmDebug(arg$returnType)

		}
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
			cirHandlerStuff=initCIRHandler(r_helper, globalVarList, params, debugBuilder, debugFun, debugCompUnit, ir, 
				attr(body(func)[[2]],"srcref")[1], rType2LlvmDebug)
		}


		#myCompiler <- function(op,insNo,insList,insNo2,args) {
		#	createIR(op,insNo,insList,insNo2,args,ir,params,constants,blockList,i,blockOps,vars, llvmContext, debugFun)
		#}

		#browser()
		initStack=list(initStack=list(),initStackPos=0)
		#stop("continue here.......")

		#checkin wheethe an initial stack state must be supplied
		if (length(blockList[[i]]$deps) == 1) {
			#browser()
			if (blockList[[i]]$deps[[1]]>=i) {
				stop("block depends on itself or on higher block!")
			} else {
				initStack=initStackList[[blockList[[i]]$deps[[1]]]]
			}
		}
		else if (length(blockList[[i]]$deps) > 1) {
			lowerBlockAvaiable=FALSE
			for (j in blockList[[i]]$deps) {
				if (j<i) {
					lowerBlockAvaiable=TRUE
					if (!is.null(initStackList[j][[1]]	)) {
						#browser()
						if (initStack$initStackPos == 0) {
							initStack=initStackList[[j]]
							initStack$initStackPos=initStack$stackPosOnExit
							for (k in (1:initStack$initStackPos)) {
								rType=getType(name=tpAny)
								llvmType=rType2Llvm(rType)
								initStack$initStack[[k]]=createPHI(ir,llvmType,length(blockList[[i]]$deps))
								PHIAddIncoming(initStack$initStack[[k]], initStackList[[j]]$stackOnExit[[k]],blockList[[j]]$block)
							}
						} else if  (initStack$initStackPos != initStackList[[j]]$stackPosOnExit ){
							stop("initStack length differs between deps!")
						} else {
							for (k in (1:initStack$initStackPos)) {
								PHIAddIncoming(initStack$initStack[[k]], initStackList[[j]]$stackOnExit[[k]],blockList[[j]]$block)
							}
						}
					}
				}
			}
			if (! lowerBlockAvaiable) {
				stop("no lower block avaiable!!")
			}
		}

		initStack$insStart = 0
		#initStackList[[i]]=visitStackMachine(source[blockList[[i]]$start:blockList[[i]]$end], myCompiler, initStack)

		#browser()
		tm=createCIRHandler(cirHandlerStuff, globalVarList,params,r_helper, ir, i, blockList, debugFun)

		initStackList[[i]]=visitStackMachine3(blockList[[i]]$opTable,tm,initStack, callHandlerForStackOp=FALSE)
		
		if (i<length(blockList) & (!(block$terminatingBranch))) {
			createBranch(ir,blockList[[i+1]]$block)
		}		
	}

	#Adding PHI Add Incoming for lower blocks

	for (i in 1:length(blockList)) {
		for (j in blockList[[i]]$deps) {
			if (j>=i) {
				#browser()
				initStack=initStackList[[i]]
				if (is.null(initStackList[j][[1]]	)) {
					stop("how can this happen?!")
				} else if  (initStack$initStackPos != initStackList[[j]]$stackPosOnExit ){
					stop("initStack length differs between deps!")
				} else {
					#browser()
					for (k in (1:initStack$initStackPos)) {
						PHIAddIncoming(initStack$initStack[[k]], initStackList[[j]]$stackOnExit[[k]],blockList[[j]]$block)
					}
				}
			}
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

	browser()
  params = getParameters(func)
  i = is.na(params@names) | params@names == ""
  if(any(i))
    params@names[i] = sprintf("var%d", which(i))

  parms = alist(a= )
  parms = structure(replicate(length(params@names), parms), names = params@names)
  
  f = function() {}

	if (length(params$names) > 0) {
  		formals(f) = parms
  		formals(f)[["..."]] = parms[[1]]
	}

	ptr=getNativePointerToFunction(func,.ee)
  
  e = quote(.args <- a)
  al = quote(.Call())
	al[[2]] = ptr
	if (length(params$names) > 0) {
  		for(i in 1:length(params@names))
     		al[[2 + i]] = as.name(params@names[i])
	}
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
	
	

	res<-makeRFunction2(fun,.ee=engine)
	#browser()

	print("finish compilation")

	#browser()

	return(res)
}


byte2llvm = function(func) {
	#browser()
	funcName=deparse(substitute(func))
	ctxt=initCompileContext(func,funcName)
	ctxt=inferFunctionType(context=ctxt,forcedReturnType=getType(name=tpAny))

	llvmContext=initLlvmContext(funcName, ctxt$absPath)
	llvmFun=compile2llvm(ctxt,llvmContext)
	res=finishCompilation(llvmFun,llvmContext, ctxt$llvmSignature)
	return(res)
}
