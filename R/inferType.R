

getMultipleLength = function(x,y) {
	if (x==y) {
		return(x)
	} else if (x<y) {
		small=x
		big=y
	} else {
		small=y
		big=x
	}

	if (small==0) {
		return(0)
	}

	if ((big %% y) != 0) {
		error("small is not a multiple of big")
	}

	return(big)
}

inferTypeStubs=list(
	list(
		opcodes=c("SUB.OP", "MUL.OP", "ADD.OP"),
		func=function(expression) {
			currentNeededType<<-lowerType(higherType(left_operand,right_operand),currentNeededType)	
			left_operand<<-tpSetNeededType(left_operand, higherType(tpGetNeededType(left_operand),currentNeededType))
			right_operand<<-tpSetNeededType(right_operand, higherType(tpGetNeededType(right_operand),currentNeededType))
			return(currentNeededType)
		}
	),
	list(
		opcodes=c("EQ.OP", "GE.OP", "GT.OP", "LE.OP", "LT.OP"),
		func=function(expression) {
			newType=higherType(left_operand,right_operand)						

			if (tpGetName(newType) == tpAny) {
				newType=getType(name=tpAny,
					subName=tpLogical,
					vectorLength=tpGetVectorLength(newType))
			} else if (tpGetName(newType) == tpNumeric) {
				newType=getType(name=tpLogical,vectorLength=tpGetVectorLength(newType))
			} else {
				stop("unreachable")
			}

			if (! is.null(currentNeededType)) {
			
				if (tpGetName(currentNeededType) == tpAny) {
					neededType=currentNeededType
				} else {
					neededType=lowerType(left_operand,right_operand)
				}
				left_operand=tpSetNeededType(left_operand, higherType(tpGetNeededType(left_operand),neededType))
				right_operand=tpSetNeededType(right_operand, higherType(tpGetNeededType(right_operand),neededType))
			} else {
				neededType=NULL
			}
			return(newType)
		}
	),
	list(
		opcodes=c("AND2ND.OP"),
		func=function(expression) {
			newType=getType(name=tpLogical, vectorLength=1)
			return(newType)
		}
	),
	list(
		opcodes=c("LDCONST.OP", "PUSHCONSTARG.OP"),
		func=function(value) {
			
			if (typeof(value)=="double") {
				if (is.null(currentNeededType) || tpGetName(currentNeededType)==tpNumeric) {
					newType=getType(name=tpNumeric, vectorLength=length(var))
				} else {
					newType=getType(name=tpAny, subName=tpNumeric, vectorLength=length(var))
				}
			} else {
				newType=getType(name=tpAny)
			}
			return(newType)
		}
	),
	list(
		opcodes=c("LDNULL.OP"),
		func=function(expression) {
			newType=getType(name=tpNull, vectorLength=1)
			return(newType)
		}
	),
	list(
		opcodes=c("GETVAR.OP"),
		func=function(varName) {
			newType=vars[[varName]]
		
			if (is.null(newType)) {
				stop(paste("Variable ",varName, "not defined before"))
			}
			return(newType)
		}
	),
	list(
		opcodes=c("SETVAR.OP"),
		func=function(varName) {
			#browser()
			var=vars[[varName]]
			if (is.null(var)) {
				vars[[varName]]<<-val
				newType=val
			} else if (! typesMatch(var,val)) {
				newType=higherType(var,val)
				vars[[varName]]<<-newType
			} else {
				newType=val
			}
			return(newType)
		}
	),
	list(
		opcodes=c("BRIFNOT.OP", "GOTO.OP", "POP.OP"),
		func=function(...) {
			return(getType(name=tpVoid))
		}
	),
	list(
		opcodes=c("RETURN.OP"),
		func=function() {
			if (! is.null(currentNeededType)) {
				val<<-tpSetNeededType(val,higherType(currentNeededType,tpGetNeededType(val)))
				newType=currentNeededType
				newType=tpSetNeededType(newType, currentNeededType)
			} else {
				newType=val
			}
			return(newType)
		}
	),
	list(
		opcodes=c("GETFUN.OP", "GETBUILTIN.OP", "GETINTLBUILTIN.OP"),
		func=function(funName) {
			newType=getType(name=tpFunctionName)
			newType=tpSetFunName(newType,funName)
			switch(opName,
				"GETFUN.OP" = { 
					#browser()
					funBody=body(get(funName, envir=globalenv()))
					if (is.null(funBody)) {
						newType=tpSetFunClass(newType,"primitive")
					} else {
						newType=tpSetFunClass(newType,"function")
					}
				},
				"GETBUILTIN.OP" = { newType=tpSetFunClass(newType,"builtin")},
				"GETINTLBUILTIN.OP" = { newType=tpSetFunClass(newType,"internal")},
				{stop("who am i?")}
			)
			return(newType)
		}
	),
	list(
		opcodes=c("MAKEPROM.OP"),
		func=function(code) {
			#browser()


			if (is.null(typeArray[opNumber][[1]])) {
				tmpType=list(promiseList=list())
				promTypeList=tmpType$promiseList
			} else {
				tmpType=typeArray[[opNumber]]
				promTypeList=tpGetPromiseList(tmpType)
			}

			promIns=code[[2]][-1]
			promConst=code[[3]]

			tm=getInferTypeManager(promTypeList, vars)

			visitStackMachine2(promIns,promConst,tm)
			#browser()

			promTypeList=tm$environ$typeArray

			newType = promTypeList[length(promIns)][[1]]
			newType = tpSetPromiseList(newType,promTypeList)
			return(newType)
		}
	),
	list(
		opcodes=c("PUSHFALSEARG.OP"),
		func=function() {
			newType=getType(name=tpLogical, vectorLength=1)
			return(newType)
		}
	),
	list(
		opcodes=c("CALL.OP", "CALLBUILTIN.OP"),
		func=function(expression, argCount) {
			#browser()
			
			if (tpGetFunClass(fun) == "function") {
				
				
				argListPos=list()
				argListPosCount=0
				argListNamed=list()
				argListNamedPos=list()
			
				if (argCount>0) {
					for (i in (1:argCount)) {
						#browser()
						arg=get(paste(sep="_","callArg",i))
						if (length(tpGetArgName(arg)) == 0) {
							argListPos[[argListPosCount+1]]=arg
							argListPosCount=argListPosCount+1;
						} else {
							argListNamed[[tpGetArgName(arg)]]=arg
							argListNamedPos[[i]]=tpGetArgName(arg)
						}
					}
				}
				ctxt=initCompileContext(get(tpGetFunName(fun), envir=globalenv()),tpGetFunName(fun))
				ctxt=inferFunctionType(ctxt,funcSignature=list(pos=argListPos,named=argListNamed))
				newType=ctxt$returnType
				newType = tpSetFuncType(newType,fun)
				newType = tpSetFuncContext(newType, ctxt)
				newType = tpSetArgListNamedPos(newType, argListNamedPos)
			} else {
				#browser()
				newType = getType(tpAny)
				newType = tpSetFuncType(newType,getType(tpAny))	
			}
			return(newType)
		}
	),	
	list(
		opcodes=c("CALLSPECIAL.OP"),
		func=function(expression) {
			newType=getType(name=tpAny)
			return(newType)
		}
	),
	list(
		opcodes=c("DODOTS.OP"),
		func=function() {
			return(getType(name=tpDots))
		}
	),
	list(
		opcodes=c("AND1ST.OP", "PUSHARG.OP", "STARTSUBSET2.OP"),
		func=function(...) {
			return(val)
		}
	),
	list(
		opcodes=c("SETTAG.OP"),
		func=function(argName) {
			newType=tpSetArgName(val,argName)
			return(newType)
		}
	),
	list(
		opcodes=c("DFLTSUBSET2.OP"),
		func=function() {
			newType=tpSetVectorLength(variable,1)
			return(newType)
		}
	)
)

inferTypeHandler=function(..., opNumber, opName, argTypes) {
	args=list(...)

	env=new.env()

	args2=list()
	

	for (arg in names(argTypes)) {
		
		if (argTypes[[arg]] == "stack") {
			#replacing stack arguemnts with the entry in the stack array
			env[[arg]]=typeArray[[args[[arg]]]]
		} else {
			args2[[arg]]=args[[arg]]
		}
	}

	if (is.null(typeArray[opNumber][[1]])) {
		env$currentNeededType=NULL
	} else {
		env$currentNeededType=tpGetNeededType(typeArray[[opNumber]])
	}

	#env$vars=vars
	#env$typeArray=parent.env(parent.env(environment()))$typeArray
	#browser()

	func=func
	parent.env(env)=environment()
	environment(func)=env

	if (is.null(func)) {
		stop(paste("no handler for op=", opName, " avaiable!"))
	}

	res=do.call(func,args2, envir=env)

	opArgTypes=list()

	for (arg in names(argTypes)) {
		
		if (argTypes[[arg]] == "stack") {
			#propragatin needed type
			typeArray[[args[[arg]]]]<<-env[[arg]]
			#collecting args
			opArgTypes[[arg]]=env[[arg]]
		}		
	}


	#if (! typesMatch(res, getType(name=tpVoid))) {
	res=tpSetNeededType(res,env$currentNeededType)
	res=tpSetArgs(res,opArgTypes)

	#browser()

	if (is.null(typeArray[opNumber][[1]])) {
		changed<<-TRUE
	} else {
		changed<<-(changed || (! (typesMatch(typeArray[[opNumber]],res))))
	}

	typeArray[opNumber]<<-res
	#}
	#typeArray<<-env$typeArray
	#vars<<-env$vars

	return(opNumber)
}

getInferTypeManager = function(typeArray, vars) {
	res=list()
	env=new.env()

	#browser()
	parent.env(env)=parent.env(environment(inferTypeHandler))
	
	env$typeArray=typeArray
	env$vars=vars
	env$changed=FALSE

	for (opGroup in inferTypeStubs) {
		for (op in opGroup$opcodes) {
			res[[op]]=inferTypeHandler
			environment(res[[op]])=new.env()
			parent.env(environment(res[[op]]))=env
			environment(res[[op]])$func=opGroup$func
		}
	}

	#browser()

	res$environ=env
	return(res)
}


