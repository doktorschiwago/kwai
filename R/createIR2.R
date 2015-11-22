promiseList = function(...) {
	return(as.list(substitute(list(...))))
}

cirHandlerTemplate=list(
	list(
		opCodeMatch=list(
			ADD.OP="+", 
			SUB.OP="-", 
			MUL.OP="*", 
			DIV.OP="/", 
			EQ.OP="==", 
			NE.OP="!=", 
			LT.OP="<", 
			LE.OP="<=", 
			GE.OP=">=", 
			GT.OP=">", 
			AND.OP="&&", 
			AND2ND.OP="&&", 
			OR.OP="||",
			EXPT.OP="^",
			COLON.OP=":"
		),
		args=promiseList(
			a1=left_operand,
			a2=right_operand
		)
	),
	list(
		opCodeMatch=list(
			DFLTSUBSET.OP="[",
			DFLTSUBSET2.OP="[["
		),
		args=promiseList(
			a1=variable,
			a2=offset
		)
	)
)


createVarList <- function(mod, arguments, symbols, strings, rType) {

	#browser()

	globalVarList=list()

	for (opGroup in cirHandlerTemplate) {
		for (op in names(opGroup$opCodeMatch)) {
			symbols[[op]]=opGroup$opCodeMatch[[op]]
			#globalVarList[[op]]=createGlobalVariable(op, mod, val=opGroup$opCodeMatch[[op]])
		}
	}

	strings$vars="vars"
	strings$stack="stack"
	strings$intEnv="intEnv"
	strings$symEnv="symEnv"


	#browser()
	

	symbols[["SUBSET_ASSIGN2"]]="[[<-"
	symbols[["new.env"]]="new.env"
	symbols[["SUBSET_GET"]]="[["



	globalVarList$STRINGS=list()
	for (varName in strings) {
		globalVarList$STRINGS[[varName]]=createGlobalVariable(paste(sep="","string_",varName), mod$mod, val=strings[[varName]])
	}

	for (varName in names(symbols)) {
		if (is.null(globalVarList$STRINGS[symbols[[varName]]][[1]])) {
			globalVarList$STRINGS[[symbols[[varName]]]]=createGlobalVariable(paste(sep="","string_",varName), mod$mod, val=symbols[[varName]])
		}
	}

	for (varName in names(arguments)) {
		if (is.null(globalVarList$STRINGS[varName][[1]])) {
			globalVarList$STRINGS[[varName]]=createGlobalVariable(paste(sep="","string_",varName), mod$mod, val=varName)
			
		}
		if (is.null(strings[varName][[1]])) {
			strings[[varName]]=varName
		}
	}

	globalSymbolList=list()
	globalStringList=list()

	cntProtected=0

	#browser()

	initFun = Function("__init", rType, list(), mod$mod)
	initBlock=Block(initFun)
	initBuilder=IRBuilder(initBlock)

	#creatin symbols once, not on every usage
	for (name in names(symbols)) {
		if (is.null(globalSymbolList[name][[1]])) {
			name2=mod$r_install(initBuilder, globalVarList$STRINGS[[symbols[[name]]]])
			mod$r_protect(initBuilder, name2)
			
			cntProtected=cntProtected+1
			
			globalSymbolList[[name]]=name2
		}
	}

	#creatin strings once, not on every usage
	for (name in names(strings)) {
		if (is.null(globalStringList[name][[1]])) {

			name2=mod$r_mkString(initBuilder, globalVarList$STRINGS[[name]])
			mod$r_protect(initBuilder, name2)
			
			cntProtected=cntProtected+1
			
			globalStringList[[name]]=name2
		}
	}
	#creating environment that keeps all objects

	protectEnv=mod$r_call_eval(initBuilder,globalSymbolList$new.env)
	mod$r_protect(initBuilder, protectEnv)
	cntProtected=cntProtected+1

	#another environment keeps precalcualted, intermediate results

	intEnv=mod$r_call_eval(initBuilder,globalSymbolList$new.env)
	mod$r_protect(initBuilder, intEnv)
	#cntProtected=cntProtected+1
	
	mod$r_call_eval(initBuilder,globalSymbolList$SUBSET_ASSIGN2,protectEnv,globalStringList$intEnv, intEnv)

	symEnv=mod$r_call_eval(initBuilder,globalSymbolList$new.env)
	mod$r_protect(initBuilder, symEnv)
	#cntProtected=cntProtected+1

	mod$r_call_eval(initBuilder,globalSymbolList$SUBSET_ASSIGN2,protectEnv,globalStringList$symEnv, symEnv)

	mod$r_unprotect(initBuilder, 2)

	#creatin symbols once, not on every usage
	for (name in names(globalSymbolList)) {
		name2_var=createGlobalVariable(paste(sep="_","symbol",name), mod$mod, type=rType,linkage=PrivateLinkage)
		setInitializer(name2_var,getNULLPointer(rType))
			
		#name2_ptr=createGEP(irb,globalSymbolList[[name]],c(createConstant(irb,0L)))
		createStore(initBuilder, globalSymbolList[[name]], name2_var)
			
		globalSymbolList[[name]]=name2_var
	}

	for (name in names(globalStringList)) {

		name2_var=createGlobalVariable(paste(sep="_","Rstring",name), mod$mod, type=rType,linkage=PrivateLinkage)
		setInitializer(name2_var,getNULLPointer(rType))
			
		createStore(initBuilder, globalStringList[[name]], name2_var)

		mod$r_call_eval(initBuilder,
			createLoad(initBuilder, globalSymbolList$SUBSET_ASSIGN2),intEnv,globalStringList[[name]], globalStringList[[name]])
			
		globalStringList[[name]]=name2_var


	}

	mod$r_unprotect(initBuilder, cntProtected)


	createReturn(initBuilder,protectEnv)
	
	globalVarList$STRINGLIST=globalStringList
	globalVarList$SYMBOLLIST=globalSymbolList
	globalVarList$FUN=initFun

	return(globalVarList)
}


initCIRHandler <- function(mod, globalVarList, parameters, debugBuilder, debugFun, debugCompUnit, irb, line,
	rType2LlvmDebug, nativeModule=NULL) {

	useNative=! is.null(nativeModule)

	globalStringList=globalVarList$STRINGLIST
	globalSymbolList=globalVarList$SYMBOLLIST

	handlerStuff=list() 

	#browser()



	#creating environment that keeps all local variables

	funcEnv=mod$r_call_eval(irb,createLoad(irb,globalSymbolList$new.env))
	mod$r_protect(irb, funcEnv)
	

	#creating list for stack

	#FIXME
	#stack size should be calculated not fixed!
	stackList=mod$r_allocVector(irb,19,100)
	mod$r_protect(irb, stackList)
	

	argIndex=1


	#browser()
	


	for (argName in names(parameters)) {
		if (class(parameters[[argName]])[1] != "AllocaInst") {

			mod$r_call_eval(irb,
				createLoad(irb, globalSymbolList$SUBSET_ASSIGN2),
				funcEnv,
				createLoad(irb, globalStringList[[argName]]),
				parameters[[argName]])

			newDebugLocalVariable(debugBuilder, irb, debugFun, parameters[[argName]], line, debugCompUnit, 
					rType2LlvmDebug(getType(name=tpAny)), argIndex)

			argIndex=argIndex+1
		} else {
			newDebugLocalVariable(debugBuilder, irb, debugFun, parameters[[argName]], line, debugCompUnit, 
					rType2LlvmDebug(getType(name=tpAny)), 0)
		}
	}


	if (useNative) {
		handlerStuff$nativeModule=nativeModule


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
		globalList=c(getGlobalVariables(handlerStuff$nativeModule), getModuleFunctions(handlerStuff$nativeModule))

		handlerStuff$nativeAdresses=list()
		handlerStuff$nativeFunctions=list()

		funcList=getModuleFunctions(handlerStuff$nativeModule)

		for (global in names(globalList)) {

			#browser()
	
	
			#check if global is resolvable
			linkage=getLinkage(globalList[[global]])

			if (linkage == InternalLinkage) next
			if (global == "llvm.dbg.declare") next

			#retrieve debug address

			globalAddress=symbolTable[[global]]
			print(paste("Symbol:", global, "at",globalAddress))

			if (! is.null(globalAddress)) {
				globalAddress2=hex2int64(globalAddress)+refOffset

				handlerStuff$nativeAdresses[[global]]=as.character(globalAddress2)

			} else if (substr(global,0,3) == "op_" ){
				#browser()
				if (is.null(handlerStuff$nativeFunctions[[substr(global,4,99)]])) {
					handlerStuff$nativeFunctions[[substr(global,4,99)]]=funcList[[global]]
				}
			} else if (substr(global,0,4) == "op2_" ){
				#browser()
				handlerStuff$nativeFunctions[[substr(global,5,99)]]=funcList[[global]]
			} else {
				print(paste("unknown function:",global))
			}
	
			#print(globalAddress)
		}

		#browser()
		
	} else {
		handlerStuff$nativeFunctions=list()
	}
	


	handlerStuff$funcEnv=funcEnv

	handlerStuff$stackList=stackList
	handlerStuff$stackListPos=1
	handlerStuff$globalStringList=globalStringList
	handlerStuff$globalSymbolList=globalSymbolList
	
	handlerStuff
}

createCIRHandler <- function(handlerStuff, globalVarList, parameters, mod, irb, currentBlock, blockList, debugFunction,
	makeProm=FALSE) {
	#browser()

	funcEnv=handlerStuff$funcEnv	

	handler=list()

	globalSymbolList=handlerStuff$globalSymbolList




	defaultCIRHandler <- function(...) {

		#browser()		
	
		args=list(...)
	
		argCount=length(opGroup$args)

		argList=list()
		for (i in (1:argCount)) {
			argList[[i]]=eval(opGroup$args[[i]], envir=args)
		}

		#browser()
		if (makeProm) {
			res=do.call(mod$r_call,c(irb, createLoad(irb, globalSymbolList[[opName]]), argList))

			#createStore(irb, binOp(irb, Add, createLoad(irb, cntProtected), 2L), cntProtected)
		} else if (is.null(handlerStuff$nativeFunctions[[substr(opName, 0, nchar(opName)-3)]])) {
			res=do.call(mod$r_call_eval,c(irb, createLoad(irb, globalSymbolList[[opName]]), argList))

			#createStore(irb, binOp(irb, Add, createLoad(irb, cntProtected), 3L), cntProtected)
		} else {
			#browser()

			nativeFunc=handlerStuff$nativeFunctions[[substr(opName, 0, nchar(opName)-3)]]
			if (! is.null(getParameters(nativeFunc)[["constant"]])) {
				argList=c(argList,createLoad(irb,mod$R_NilValue))
			}
			res=do.call(createCall, c(irb, handlerStuff$nativeFunctions[[substr(opName, 0, nchar(opName)-3)]], 
				createLoad(irb, mod$R_GlobalEnv), argList))
		}
		return(res)
	}


	for (opGroup in cirHandlerTemplate) {
		argCount=length(opGroup$args)
		for (i in (2:argCount)) {
			opGroup$args[[i-1]]=opGroup$args[[i]]
		}
		opGroup$args[[argCount]]=NULL

		for (op in names(opGroup$opCodeMatch)) {
			handler[[op]]=defaultCIRHandler
			localEnv=new.env()
			localEnv$opGroup=opGroup
			localEnv$opName=op
			parent.env(localEnv)=environment(defaultCIRHandler)

			environment(handler[[op]])=localEnv

			
		}
	}

	#browser()


	handler$RETURN.OP <- function(val, ...) {
		if (! makeProm) {
			mod$r_unprotect(irb, 2)
			createReturn(irb, val)
		} else {
			return(val)
		}
	}

	handler$GETVAR.OP <- function(varName, ...) {
		varName2=createLoad(irb,handlerStuff$globalStringList[[varName]])


		SUBSET_GET=createLoad(irb, globalSymbolList$SUBSET_GET)

		if (makeProm) {
			res=mod$r_call(irb, SUBSET_GET,funcEnv,varName2)
			#mod$r_unprotect(irb, 3)
		} else {
			res=mod$r_call_eval(irb, SUBSET_GET,funcEnv,varName2)
			res=createCall(irb,mod$Rf_duplicate,res)
			#mod$r_unprotect(irb, 4)
		}
		return(res)
	}

	handler$SETVAR.OP<-function(varName, val,...) {
		#browser()
		if (makeProm) {
			stop("no sense in this!")
		}
		varName2=createLoad(irb,handlerStuff$globalStringList[[varName]])
		res=mod$r_call_eval(irb, createLoad(irb, globalSymbolList$SUBSET_ASSIGN2), funcEnv,varName2,val)
		return(res)
	}

	handler$LDCONST.OP<-function(value, ...) {
		#browser()
		if (length(value) != 1) {
			stop("can handle only length==1")
		} else if (typeof(value) == "double") {
			value2=makeConstant(irb,as.double(value))
			newVector=mod$r_allocVector(irb, 14, 1)
			mod$r_protect(irb, newVector)

			newVector3=createBitCast(irb, newVector,pointerType(DoubleType))
			data=createGEP(irb,newVector3,c(createConstant(irb,5L)))
			createStore(irb,value2,data)
			mod$r_unprotect(irb, 1)
		} else if (typeof(value) == "character") {
			#browser()
			newVector=createLoad(irb,handlerStuff$globalStringList[[value]])
		} else {
			stop(paste("can handle only double!", typeof(value), "is not supported!"))
		}

		


		
		newVector
	}

	handler$BRIFNOT.OP<-function(val, goto, expression, ... ) {
		#browser()
		#debugSetLocation(irb, debugFunction, attr(expression,"srcref")[1], attr(expression,"srcref")[5])

		offset=handler$LDCONST.OP(1)
		mod$r_protect(irb, offset)

		val2=handler$DFLTSUBSET2.OP(variable = val, offset = offset)
		mod$r_protect(irb, val2)

		val3=createBitCast(irb, val2,pointerType(Int32Type))
		val4=createGEP(irb,val3,c(createConstant(irb,10L)))
		val5=createLoad(irb,val4)
		val6=createTrunc(irb,val5,getIntegerType(1))

		mod$r_unprotect(irb, 2)
		createCondBranch(irb, val6,blockList[[currentBlock+1]]$block,blockList[[goto]]$block)

	}

	handler$GOTO.OP<-function(goto, ...) {
		createBranch(irb, blockList[[goto]]$block)
	}

#	handler$POP.OP<-function(...) {
#		return(NA)
#	}
#
	handler$DUP2ND.OP<-function(top, secondTop,...) {
		return(createCall(irb,mod$Rf_duplicate,secondTop))
	}

	handler$GETFUN.OP<-function(funName, ...) {
		res=mod$r_findVar(irb, createLoad(irb, handlerStuff$globalSymbolList[[funName]]))
		return(res)
	}

	handler$MAKEPROM.OP<-function(code, resStackPos, ...) {

		#browser()

		handlerStuff2=handlerStuff
		handlerStuff2$stackListPos=resStackPos
		tm=createCIRHandler(handlerStuff2, globalVarList, parameters, mod, irb, currentBlock, blockList, debugFunction,TRUE)


		promSource=code
		promIns=promSource[[2]][-1]
		promConst=promSource[[3]]

		opList=renderStackMachine(promIns,promConst)

		res=visitStackMachine3(opList,tm)
		return(res$stackOnExit[[res$stackPos+1]])
	}

	handler$CALL.OP<-function(fun, argCount, ...) {
		#browser()

		argList=list(...)
		args=c(irb,mod[[paste(sep="","Rf_lang",argCount+1)]], fun)
		for (i in (1:argCount)) {
			args=c(args,argList[[paste(sep="_","callArg",i)]])
		}

		call=do.call(createCall,args)
		mod$r_protect(irb, call)
		res=mod$r_eval(irb,call)

		mod$r_unprotect(irb, 1)
		return(res)

	}

	handler$LDNULL.OP<-function(...) {
		return(createLoad(irb,mod$R_NilValue))
	}


	#this function takes care of some common tasks
	wrapperHandler<-function(...) {
		args=list(...)

		#browser()
		
		#creating debug information
		#browser()
		if (! (is.null(args[["expression"]]) || is.na(args[["expression"]]))) {
			#browser()
			expression=args$expression$expression
			debugSetLocation(irb, debugFunction, attr(expression,"srcref")[1], attr(expression,"srcref")[5])
		}

		func=handler[[args$opName]]
	

		#save res on stack
		if (args$resStackPos>0) {
			res=do.call(func,args)
			mod$r_set_vector_elt(irb,handlerStuff$stackList,handlerStuff$stackListPos+args$resStackPos,res)
			return(res)
		} else {
			do.call(func,args)
		}
	}

	handler2=createStdHandler(func=wrapperHandler)



	return(handler2)
}

createVarHandler=function(vars2, strings2, symbols2) {
	handler=createStdHandler()

	handler$SETVAR.OP=function(varName,...) {
		#browser()
		vars2=vars
		vars2[[varName]]=getType(name=tpAny)
		vars<<-vars2

		strings2=strings
		strings2[[varName]]=varName
		strings<<-strings2
	}
	environment(handler$SETVAR.OP)$vars=vars2
	environment(handler$SETVAR.OP)$strings=strings2

	handler$GETFUN.OP = function(funName, ...) {

		symbols2=symbols
		symbols2[[funName]]=funName
		symbols<<-symbols2

	}
	environment(handler$GETFUN.OP)$symbols=symbols2


	handler$GETVAR.OP = function(varName, ...) {
		strings2=strings
		strings2[[varName]]=varName
		strings<<-strings2
	}
	environment(handler$GETVAR.OP)$strings=strings2

	handler$LDCONST.OP = function(value, ...) {
		if (typeof(value) != "character") {
			return()
		}

		#browser()
		strings2=strings
	
		for (val in value) {
			if (is.null(strings2[[val]])) {
					strings2[[val]]=val
			}
		}
		strings<<-strings2
	}
	environment(handler$LDCONST.OP)$strings=strings2

	return(handler)
}
