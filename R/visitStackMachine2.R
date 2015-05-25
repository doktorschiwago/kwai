
promiseList = function(...) {
	return(as.list(substitute(list(...))))
}

stackPos.NONE=99
stackPos.POP=98
stackPos.CALL=97

OpcodeList = list(
	list(
		opcodes=c(
			"ADD.OP", "SUB.OP", "MUL.OP", "DIV.OP", "EQ.OP", "NE.OP", "LT.OP", "LE.OP", "GE.OP", "GT.OP", "AND.OP", "AND2ND.OP", "OR.OP"),
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
			"DODOTS.OP", "LDNULL.OP", "PUSHFALSEARG.OP"),
		args=promiseList()
	),
	list(
		opcodes=c(
			"POP.OP", "RETURN.OP"),
		args=promiseList(
			val				= vmStack(0)
		),
		stackPos=stackPos.POP
	),
	list(
		opcodes=c(
			"AND1ST.OP", "BRIFNOT.OP"),
		args=promiseList(
			expression		= constantArgCode(1),
			goto			= arg(2),
			val				= vmStack(0)
		),
		stackPos=stackPos.POP
	),
	list(
		opcodes=c(
			"GOTO.OP"),
		args=promiseList(
			goto			= arg(1)
		),
		stackPos=stackPos.NONE
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
			"SETTAG.OP"),
		args=promiseList(
			val			= vmStack(0),
			argName		= constantArgString(1)
		)
	),
	list(
		opcodes=c(
			"STARTSUBSET2.OP"),
		args=promiseList(
			expression	= constantArgCode(1),
			unknown		= arg(1),
			val			= vmStack(0)
		),
		stackPos=stackPos.NONE
	),
	list(
		opcodes=c(
			"DFLTSUBSET2.OP"),
		args=promiseList(
			variable	= vmStack(-1),
			offset		= vmStack(0)
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

for (opGroup in OpcodeList) {
	opGroup2=opGroup
	opGroup2$opcodes=NULL

	opGroup2$argCount=0
	stackPos=1

	opGroup2$args[[1]]=NULL

	#browser()
	
	for (arg in opGroup2$args) {

		#browser()

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
		} else {
			switch(func,
				"arg" =,
				"constantArg" =,
				"constantArgString" =,
				"constantArgCode" = {
					opGroup2$argCount=opGroup2$argCount+1
				},
				{
					stop(paste("unknown func", func, "!"))
				}
			)
		}
				
	}

	if (is.null(opGroup2$stackPos)) {
		opGroup2$stackPos=stackPos
	}

	for (op in opGroup$opcodes) {
		Opcodes[[op]]=opGroup2
		Opcodes[[op]]$op=op

	}
}


visitStackMachine2 <- function(source, constants, vsmHandler, initStack=NULL) {
	
	length=length(source)
	i=1
	if (is.null(initStack)) {
		currentStackPos	= 0
		stack			= list()
		insStart		= 0
	} else {
		currentStackPos = initStack$stackPos
		stack 			= initStack$stackOnExit
		insStart		= initStack$insStart
		#browser()
	} 

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

	vmStack = function(pos) {
		stack[[currentStackPos+pos]]
	}
	

	while (i<=length) {

		#browser()

		opName=as.character(source[i][[1]])

		op=Opcodes[[opName]]

		if (is.null(op)) {
			stop(paste("Opcode ",opName, " not supported"))
		}

		funcArgs=op$args
		funcArgTypes=list()

		for (argName in names(funcArgs)) {

			if (isStackArg(as.character(funcArgs[[argName]][1]))) {
				funcArgTypes[[argName]]="stack"
			} else {
				funcArgTypes[[argName]]="other"
			}
			funcArgs[[argName]]=eval(funcArgs[[argName]])
		}

		funcArgs$opNumber=i+insStart

		func=vsmHandler[[opName]]

		if (is.null(func)) {
			stop(paste("no handler for op=", opName, " avaiable!"))
		}

		if (op$stackPos == stackPos.CALL) {
			#browser()
			#we need to collect all call arguments on the stack
			for (j in (1:funArgCounter[[length(funArgCounter)]])) {
				funcArgs[[paste(sep="_","callArg",j)]]=vmStack(j-funArgCounter[[length(funArgCounter)]])
				funcArgTypes[[paste(sep="_","callArg",j)]]="stack"
			}
			
			funcArgs$argCount=funArgCounter[[length(funArgCounter)]]
			funcArgTypes$argCount="other"
		
			#put the getfun in front
			funcArgs$fun=vmStack(0-funArgCounter[[length(funArgCounter)]])
			funcArgTypes$fun="stack"
		}

		funcArgs$argTypes=funcArgTypes
		funcArgs$opName=opName

		res=do.call(func,funcArgs)

		#browser()
		if (op$stackPos == stackPos.NONE) {
			#browser()
			deltaStack = 0
		} else if (op$stackPos == stackPos.POP) {
			currentStackPos	= currentStackPos -1
			deltaStack = -1
		} else if (op$stackPos == stackPos.CALL) {
			#browser()
			deltaStack=0 - funArgCounter[[length(funArgCounter)]]
			
			currentStackPos = currentStackPos + deltaStack

			stack[[currentStackPos]] = res
		} else {

			deltaStack = op$stackPos
			currentStackPos = currentStackPos+deltaStack

			if (currentStackPos<1) { browser() }

			stack[[currentStackPos]] = res
		
			
		}

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
	return(list(stackOnExit=stack, stackPos=currentStackPos, insStart=insStart))
}

