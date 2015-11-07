


promiseList = function(...) {
	return(as.list(substitute(list(...))))
}

stackPos.NONE=99
stackPos.POP=98
stackPos.CALL=97
stackPos.DUP=96

OpcodeList = list(
	list(
		opcodes=c(
			"ADD.OP", "SUB.OP", "MUL.OP", "DIV.OP", "EQ.OP", "NE.OP", "LT.OP", "LE.OP", "GE.OP", "GT.OP", "AND.OP", "AND2ND.OP", "OR.OP",
			"EXPT.OP", "COLON.OP"),
		args=promiseList(
			expression		= constantArgCode(1),
			left_operand	= vmStack(-1),
			right_operand	= vmStack(0)
		)
	),
	list(
		opcodes=c(
			"SETVAR.OP"),
		args=promiseList(
			val				= vmStack(0),
			varName			= constantArgString(1)
		)
	),
	list(
		opcodes=c(
			"GETVAR.OP"),
		args=promiseList(
			varName			= constantArgString(1)
		)
	),
	list(
		opcodes=c(
			"LDCONST.OP", "PUSHCONSTARG.OP"),
		args=promiseList(
			value			= constantArg(1)
		)
	),
	list(
		opcodes=c(
			"DODOTS.OP", "LDNULL.OP", "PUSHFALSEARG.OP", "LDTRUE.OP", "PUSHTRUEARG.OP"),
		args=promiseList()
	),
	list(
		opcodes=c(
			"POP.OP"),
		args=promiseList(
			val				= vmStack(0)
		),
		stackPos=stackPos.POP
	),
	list(
		opcodes=c(
			"RETURN.OP"),
		args=promiseList(
			val				= vmStack(0)
		),
		stackPos=stackPos.POP,
		fallthrough=FALSE,
		branchIns=TRUE
	),
	list(
		opcodes=c(
			"AND1ST.OP"),
		args=promiseList(
			expression		= constantArgCode(1),
			goto			= arg(2),
			val				= vmStack(0)
		),
		stackPos=stackPos.POP
	),
	list(
		opcodes=c(
			"BRIFNOT.OP"),
		args=promiseList(
			expression		= constantArgCode(1),
			goto			= arg(2),
			val				= vmStack(0)
		),
		stackPos=stackPos.POP,
		branchIns=TRUE
	),
	list(
		opcodes=c(
			"GOTO.OP"),
		args=promiseList(
			goto			= arg(1)
		),
		stackPos=stackPos.NONE,
		fallthrough=FALSE,
		branchIns=TRUE
	),

	list(
		opcodes=c(
			"CALL.OP", "CALLBUILTIN.OP"),
		args=promiseList(
			expression		= constantArgCode(1)
		),
		stackPos=stackPos.CALL
	),
	list(
		opcodes=c(
			"CALLSPECIAL.OP"),
		args=promiseList(
			expression		= constantArgCode(1)
		)
	),
	list(
		opcodes=c(
			"GETFUN.OP", "GETBUILTIN.OP", "GETINTLBUILTIN.OP"),
		args=promiseList(
			funName			= constantArgString(1)
		)
	),
	list(
		opcodes=c(
			"MAKEPROM.OP"),
		args=promiseList(
			code			= constantArg(1)
		)
	),
	list(
		opcodes=c(
			"PUSHARG.OP"),
		args=promiseList(
			val			= vmStack(0)
		)
	),
	list(
		opcodes=c(
			"DUP2ND.OP"),
		args=promiseList(
			top			= vmStack(0),
			secondTop	= vmStack(-1)
		),
		stackPos=stackPos.DUP
	),
	list(
		opcodes=c(
			"SWAP.OP"),
		args=promiseList(
		),
		stackPos=stackPos.NONE
	),
	list(
		opcodes=c(
			"SETTAG.OP"),
		args=promiseList(
			val			= vmStack(0),
			argName		= constantArgString(1)
		)
	),
	list(
		opcodes=c(
			"STARTSUBSET.OP", "STARTSUBSET2.OP"),
		args=promiseList(
			expression	= constantArgCode(1),
			unknown		= arg(1),
			val			= vmStack(0)
		),
		stackPos=stackPos.NONE
	),
	list(
		opcodes=c(
			"DFLTSUBSET.OP", "DFLTSUBSET2.OP"),
		args=promiseList(
			variable	= vmStack(-1),
			offset		= vmStack(0)
		)
	),
	list(
		opcodes=c(
			"STARTFOR.OP"),
		args=promiseList(
			expression		= constantArgCode(1),
			loop_var		= constantArgString(2),
			goto			= arg(3)
		)
	),
	list(
		opcodes=c(
			"STEPFOR.OP"),
		args=promiseList(
			goto			= arg(1)
		)
	),
	list(
		opcodes=c(
			"ENDFOR.OP"),
		args=promiseList(
		)
	)
)

Opcodes=list()

isStackArg=function(funcName) {
	switch(funcName,
		"vmStack" = {
			return(TRUE)
		},
		{
			return(FALSE)
		}
	)
}

dummyCreateOp = function(opNumber) {
	res=c(as.list(environment()),as.list(parent.env(environment())))

	if (missing(opNumber)) {
		res$opNumber=NULL
	}

	#check if all arguemnts are present
	for (arg in names(res)) {
		#browser()
		if (typeof(res[[arg]]) == "symbol") {
			stop(paste(arg, "is missing!"))
		}
	} 

	#if we are a call.op theres sth more to do

	if (opName == "CALL.OP") {
		#browser()
		#we need to collect all call arguments on the stack
		for (j in 1:argCount) {
			tmp=j-argCount
			res[[paste(sep="_","callArg",j)]]=substitute(vmStack(tmp))
			res$argTypes[[paste(sep="_","callArg",j)]]="stack"
		}
		
		res$argTypes$argCount="other"
	
		#put the getfun in front
		tmp=0-argCount
		res$fun=substitute(vmStack(tmp))
		res$argTypes$fun="stack"
		res$deltaStack=0-argCount
	}

	#browser()
	return(res)
}

countOps=0

for (opGroup in OpcodeList) {
	opGroup2=opGroup
	opGroup2$opcodes=NULL

	opGroup2$argCount=0
	stackPos=1

	opGroup2$args[[1]]=NULL

	#browser()

	createOpArgs=formals(dummyCreateOp)
	dummyEmptyArg=alist(dummy=)

	funcArgTypes=list()
	stackArgs=list()
	
	for (argName in names(opGroup2$args)) {

		#browser()

		arg=opGroup2$args[[argName]]

		if (is.null(arg)) {
			stop("missing definition!!")
		}
		
		if (typeof(arg) != "language") {
			stop("hammertime!")
			next
		}
		func=as.character(arg[1])

		if (isStackArg(func)) {
			stackPos=stackPos-1
			funcArgTypes[[argName]]="stack"
			stackArgs[[argName]]=arg
		} else {
			funcArgTypes[[argName]]="other"
			switch(func,
				"arg" =,
				"constantArg" =,
				"constantArgString" =,
				"constantArgCode" = {
					opGroup2$argCount=opGroup2$argCount+1
					if (func == "constantArgCode") {
						createOpArgs[[argName]]=NA
					} else {
						createOpArgs[[argName]]=dummyEmptyArg$dummy
					}
				},
				{
					stop(paste("unknown func", func, "!"))
				}
			)
		}
				
	}

	if (is.null(opGroup2$stackPos)) {
		opGroup2$stackPos=stackPos
		opGroup2$deltaStack = stackPos
		opGroup2$assignsStack=TRUE
	} else if (opGroup2$stackPos == stackPos.NONE) {
		#browser()
		opGroup2$deltaStack = 0
		opGroup2$assignsStack=FALSE
	} else if (opGroup2$stackPos == stackPos.POP) {
		opGroup2$deltaStack = -1
		opGroup2$assignsStack=FALSE
	} else if (opGroup2$stackPos == stackPos.CALL) {
		#browser()
		opGroup2$assignsStack=TRUE
		
		createOpArgs$argCount=dummyEmptyArg$dummy
	} else if (opGroup2$stackPos == stackPos.DUP) {
		opGroup2$deltaStack = 1
		opGroup2$assignsStack = TRUE
	} else {
		stop("unknown stackPos type!@")
	}



	if (is.null(opGroup2$fallthrough)) {
		opGroup2$fallthrough=TRUE
	}

	#globEnv=parent.env(environment())
	globEnv=globalenv()

	for (op in opGroup$opcodes) {

#		if (op=="BRIFNOT.OP") {
#			browser()
#		}

		countOps=countOps+1

		Opcodes[[op]]=opGroup2
		Opcodes[[op]]$op=op

		#browser()

		opFunc=dummyCreateOp
		
		if (length(stackArgs)==0) {
			env=new.env()
		} else {
			env=list2env(stackArgs)
		}
		env[["opName"]]=op
		env[["deltaStack"]]=opGroup2$deltaStack
		env[["assignsStack"]]=opGroup2$assignsStack
		env[["argTypes"]]=funcArgTypes
		env[["fallthrough"]]=opGroup2$fallthrough
		env[["branchIns"]]=opGroup2$branchIns

		parent.env(env)=environment(opFunc)
		environment(opFunc)=env

		formals(opFunc)=createOpArgs
		globEnv[[paste(sep="","create",op)]]=opFunc
	}
}

print(paste(countOps,"ops are supported!"))

#this function renders the r bytecode to a list of instructions
#it resolves all constants, only stack arguments must be resolved during runtime
renderStackMachine <- function(source, constants) {
	
	length=length(source)
	i=1

	funArgCounter=list()

	constantArgCode = function(pos) {
		list(expression=constants[[1+source[[i+pos]]]])
	}

	constantArgString = function(pos) {
		as.character(constants[[1+source[[i+pos]]]])
	}

	constantArg = function(pos) {
		constants[[1+source[[i+pos]]]]
	}

	arg = function(pos) {
		source[[i+pos]]
	}

	res=list()
	counter=1
	

	while (i<=length) {

		#browser()

		opName=as.character(source[i][[1]])

		op=Opcodes[[opName]]

		if (is.null(op)) {
			stop(paste("Opcode ",opName, " not supported"))
		}

		funcArgs=op$args
		funcArgTypes=list()

		#browser()

		for (argName in names(funcArgs)) {

			if (isStackArg(as.character(funcArgs[[argName]][1]))) {
				funcArgTypes[[argName]]="stack"
			} else {
				funcArgTypes[[argName]]="other"
				
				funcArgs[[argName]]=eval(funcArgs[[argName]])
			}
		}

		#browser()

		funcArgs$opNumber=i

		if (op$stackPos == stackPos.CALL) {
			#browser()
			#we need to collect all call arguments on the stack
			for (j in (1:funArgCounter[[length(funArgCounter)]])) {
				tmp=j-funArgCounter[[length(funArgCounter)]]
				funcArgs[[paste(sep="_","callArg",j)]]=substitute(vmStack(tmp))
				funcArgTypes[[paste(sep="_","callArg",j)]]="stack"
			}
			
			funcArgs$argCount=funArgCounter[[length(funArgCounter)]]
			funcArgTypes$argCount="other"
		
			#put the getfun in front
			tmp=0-funArgCounter[[length(funArgCounter)]]
			funcArgs$fun=substitute(vmStack(tmp))
			funcArgTypes$fun="stack"
		}

		funcArgs$argTypes=funcArgTypes
		funcArgs$opName=opName
		funcArgs$fallthrough=op$fallthrough
		funcArgs$branchIns=op$branchIns

		#browser()


		if (op$stackPos == stackPos.CALL) {
			#browser()
			deltaStack=0 - funArgCounter[[length(funArgCounter)]]
			assignsStack=TRUE
		} else {
			deltaStack = op$deltaStack
			assignsStack=op$assignsStack
		}

		funcArgs$deltaStack=deltaStack
		funcArgs$assignsStack=assignsStack
		res[[counter]]=funcArgs
		counter=counter+1


		switch(opName,
			"GETINTLBUILTIN.OP" =,
			"GETBUILTIN.OP" =,
			"GETFUN.OP" = {
				#browser()
				funArgCounter[[length(funArgCounter)+1]]=0
			},
			"CALLBUILTIN.OP" =,
			"CALL.OP" = {
				#browser()
				funArgCounter[[length(funArgCounter)]]=NULL
				if (length(funArgCounter)>0) {
					funArgCounter[[length(funArgCounter)]]=funArgCounter[[length(funArgCounter)]]+1
				}
			},
			{
				if (length(funArgCounter)>0) {
					funArgCounter[[length(funArgCounter)]]=funArgCounter[[length(funArgCounter)]]+deltaStack
				}
			}

		)

		i=i+1+op$argCount
	}


	res=fixGotos(res)

	return(res)
}

#this function updates goto labels and renumbers all ops
fixGotos = function(opTable) {
	#replacing goto args with ref to block list member

	#browser()
	for (i in 1:(length(opTable))) {
		if (! is.null(opTable[[i]][["goto"]])) {
			#browser()
			found=FALSE
			for (j in 1:(length(opTable))) {
				if (! is.null(opTable[[j]]$opNumber)) {
					if (opTable[[i]]$goto==opTable[[j]]$opNumber) {
						opTable[[i]]$goto=j
						found=TRUE
						break
					}
				}
			}
			if (found == FALSE) {
				stop("could not connect goto to op!")
			}
		}
	}

	#replacing opNumber
	for (i in 1:(length(opTable))) {
		opTable[[i]]$opNumber=i
	}

	return(opTable)
}



isStackArg=function(funcName) {
	switch(funcName,
		"vmStack" = {
			return(TRUE)
		},
		{
			return(FALSE)
		}
	)
}

#this function calls the vsmHandler for each instruction in opTable
#stack arguments are resolved

visitStackMachine3 <- function(opTable, vsmHandler, initStack=NULL, callHandlerForStackOp=TRUE) {


	vmStack = function(pos) {
		stack[[currentStackPos+pos]]
	}
	
	length=length(opTable)
	i=1
	if (is.null(initStack)) {
		currentStackPos	= 0
		stack			= list()
		insStart		= 0
	} else if (typeof(initStack) != "list") {
		#browser()
		currentStackPos	= length*2
		stack			= as.list(rep(NA,currentStackPos))
		insStart		= 0
	} else {
		currentStackPos = initStack$initStackPos
		stack 			= initStack$initStack
		insStart		= initStack$insStart
		#browser()
	} 

	#browser()

	initStack2=stack
	initStackPos=currentStackPos

	#browser()

	while (i<=length) {

		#browser()

		op=opTable[[i]]

		opName=op$opName

		funcArgTypes=op$funcArgTypes

		ignoreRes=FALSE
		callHandler=TRUE

		switch(opName,
			"SWAP.OP" = {
				tmp=stack[[currentStackPos]]
				stack[[currentStackPos]]=stack[[currentStackPos-1]]
				stack[[currentStackPos-1]]=tmp

				callHandler=callHandlerForStackOp
				ignoreRes=TRUE
			},
			"POP.OP" = {
				callHandler=callHandlerForStackOp
				ignoreRes=TRUE			
			},
			"DUP2ND.OP" = {
				res=stack[[currentStackPos-1]]

				callHandler=callHandlerForStackOp
				ignoreRes=TRUE	
			}
		)

		#browser()

		

		if (callHandler) {

			for (argName in names(funcArgTypes)) {
				if (funcArgTypes[[argName]] == "stack") {
					browser()
					op[[argName]]=eval(op[[argName]])
				}
			}

			newStackPos = currentStackPos + op$deltaStack

			if (! op$assignsStack) {
				op$resStackPos=-1
			} else {
				op$resStackPos=newStackPos
			}


			func=vsmHandler[[opName]]

			if (is.null(func)) {
				stop(paste("no handler for op=", opName, " avaiable!"))
			}

			#browser()
			
			if (ignoreRes) {
				do.call(func,op)
			} else {
				res=do.call(func,op)
			}
			currentStackPos = newStackPos
		} else {
			currentStackPos = currentStackPos + op$deltaStack
		}

		#browser()
		

		if (op$assignsStack) {
			#browser()
			stack[[currentStackPos]] = res
		} 

		i=i+1
	}
	return(list(stackOnExit=stack, stackPosOnExit=currentStackPos, insStart=insStart, initStack=initStack2, initStackPos=initStackPos))
}

createStdHandler=function(func=function(...){return(NA)}) {
	res=list()
	for (op in Opcodes) {
		res[[op$op]]=func
	}
	return(res)
}
