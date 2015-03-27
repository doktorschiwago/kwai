



inferBuiltinType <- function(funcName) {
	switch(funcName,
		"as.character" = ,
		"is.na" = ,
		"pmatch" = ,
		"list" = ,
		"length" = {
			return(getType(tpAny,FALSE))
		},
		{
			stop(paste("unknown function: ",funcName))
		}
	)
}


inferType2 <- function(op,insNo,insList,insNo2,args,vars,constants,typeInformation, changed) {
	tpInfo=deref(typeInformation)
	argTypes=list()

	#browser()

	if (is.null(tpInfo[insNo][[1]])) {
		currentNeededType=NULL
	} else {
		currentNeededType=tpInfo[[insNo]]$neededType
	}
	switch(op,
		"SUB.OP" = ,
		"MUL.OP" = ,
		"ADD.OP" = , 
		"GT.OP" = {
			#browser()
			argTypes$A=tpInfo[[insList[[insNo2-1]]]]
			argTypes$B=tpInfo[[insList[[insNo2]]]]
	
			switch(op,
				"GT.OP" = {
					#if (! is.null(currentNeededType)) {
					#	newType=higherType(currentNeededType,list(name=tpLogical,vector=currentNeededType$vector))
					#} else 
					if (argTypes$A$name == tpAny) {
						newType=list(name=tpAny,vector=FALSE)
					} else if (argTypes$A$name == tpNumeric) {
						newType=list(name=tpLogical,vector=FALSE)
					}
					neededType=newType
				
				}, {
					newType=lowerType(higherType(argTypes$A,argTypes$B),currentNeededType)
					neededType=newType
				}
			)
			#browser()
			neededType=higherType(neededType,argTypes$A$neededType)
			neededType=higherType(neededType,argTypes$B$neededType)
			argTypes$A$neededType=neededType
			argTypes$B$neededType=neededType
			tpInfo[[insList[[insNo2-1]]]]=argTypes$A
			tpInfo[[insList[[insNo2]]]]=argTypes$B
		},

		"GETVAR.OP" = {
			newType=deref(vars)[[as.character(constants[[1+args[1]]])]]
		
			if (is.null(newType)) {
				stop(paste("Variable ",as.character(constants[[1+args[1]]]), "not defined before"))
			}
		},
		"BRIFNOT.OP" =,
		"GOTO.OP" =,
		"POP.OP" = {
			newType=list(name=tpVoid, vector=FALSE)
			if (op == "BRIFNOT.OP") {
				argTypes$A=tpInfo[[insList[[insNo2]]]]
				neededType=list(name=tpLogical,vector=FALSE)
				argTypes$A$neededType=higherType(neededType,argTypes$A$neededType)
				tpInfo[[insList[[insNo2]]]]=argTypes$A
			}
		},
		"SETVAR.OP" = {
			#browser()
			varname=as.character(constants[[1+args[1]]])
			var=deref(vars)[[varname]]
			newType=tpInfo[[insList[[insNo2]]]]
			if (is.null(var)) {
				deref(vars)[[varname]]=newType
			} else if (! typesMatch(deref(vars)[[varname]],newType)) {
				newType=higherType(deref(vars)[[varname]],newType)
				deref(vars)[[varname]]=newType
			}
			argTypes$A=newType
			
		},
		"PUSHCONSTARG.OP" =,
		"LDCONST.OP" = {
			#browser()
			var=constants[[1+args[1]]]
			newType=list(name=tpAny, vector=(length(var)>1))
			if (typeof(var)=="double") {
				newType$name=tpNumeric
			}
				
		},
		"RETURN.OP" = {
			#browser()
			
			argTypes$A=tpInfo[[insList[[insNo2]]]]
			
			if (! is.null(currentNeededType)) {
				argTypes$A$neededType=higherType(currentNeededType,argTypes$A$neededType)
				tpInfo[[insList[[insNo2]]]]=argTypes$A
				newType=currentNeededType
				newType$neededType=currentNeededType
			} else {
				newType=tpInfo[[insList[[insNo2]]]]
			}
		},
		"GETINTLBUILTIN.OP" =,
		"GETBUILTIN.OP" =,
		"GETFUN.OP" = {
			#browser()
			newType=list(name=tpFunctionName,vector=FALSE, funName=as.character(constants[[1+args[1]]]))
			switch(op,
				"GETFUN.OP" = { 
					funBody=body(get(newType$funName))
					if (is.null(funBody)) {
						newType$funType="primitive"
					} else {
						newType$funType="function"
					}
				},
				"GETBUILTIN.OP" = { newType$funType="builtin"},
				"GETINTLBUILTIN.OP" = { newType$funType="internal"},
				{stop("who am i?")}
			)
		},
		"MAKEPROM.OP" = {
			#browser()

			changed2=FALSE
			changed3=as.ref(changed2)


			if (is.null(tpInfo[insNo][[1]])) {
				tmpType=list(promiseList=list())
			} else {
				tmpType=tpInfo[[insNo]]
			}

			promSource=constants[[1+args[1]]]
			promIns=promSource[[2]][-1]
			promConst=promSource[[3]]

			promTypeList=tmpType$promiseList
			promTypeList2=as.ref(promTypeList)

			myInferType<-function(op,insNo,insList,insNo2,args2) {
				inferType2(op,insNo,insList,insNo2,args2,vars,promConst,promTypeList2,changed3)
			}


			visitStackMachine(promIns,myInferType)

			newType=promTypeList[length(promIns)][[1]]
			newType$promiseList=promTypeList
		},
		"CALLBUILTIN.OP" =,
		"CALL.OP" = {
			#browser()
			callSymbol=constants[[1+args[[1]]]]
			argCount=length(callSymbol)-1
			functionType=tpInfo[[insList[[insNo2-argCount]]]]
			
			if (functionType$funType == "function") {
				
				
				argListPos=list()
				argListPosCount=0
				argListNamed=list()
				argListNamedPos=list()
			
				if (argCount>0) {
					for (i in (1:argCount)) {
						if (is.null(tpInfo[[insList[[insNo2+1-i]]]]$argName)) {
							argListPos[[argListPosCount+1]]=tpInfo[[insList[[insNo2+1-i]]]]
							argListPosCount=argListPosCount+1;
						} else {
							argListNamed[[tpInfo[[insList[[insNo2+1-i]]]]$argName]]=tpInfo[[insList[[insNo2+1-i]]]]
							argListNamedPos[[i]]=tpInfo[[insList[[insNo2+1-i]]]]$argName
						}
					}
				}
				ctxt=initCompileContext(get(functionType$funName),functionType$funName)
				ctxt=inferFunctionType(ctxt,list(pos=argListPos,named=argListNamed))
				newType=ctxt$returnType
				newType$funcContext=ctxt
				newType$argListNamedPos=argListNamedPos
			} else {
				#browser()
				newType=inferBuiltinType(as.character(callSymbol[1]))		
			}
			newType$funcType=functionType
		},
		"SETTAG.OP" = {
			#browser()			
			newType=tpInfo[[insList[[insNo2]]]]
			newType[["argName"]]=as.character(constants[[1+args[1]]])
		},
		"CALLSPECIAL.OP" = {
			#browser()
			switch(as.character(constants[[1+args[1]]][1]),
				"missing" = {
					newType=list(name=tpLogical, vector=FALSE)
				}, {
					stop(paste("CALLSPECIAL.OP ",op, " not supported"))
				}
			)
		},
		"LDNULL.OP" = {
			newType=list(name=tpNull,vector=FALSE)
		},
		"DODOTS.OP" = {
			newType=getType(name=tpDots)
		},
		{
			stop(paste("Opcode ",op, " not supported"))
		}
	)
	
	if (is.null(tpInfo[insNo][[1]])) {
		deref(changed)=TRUE
		newType$neededType=NULL
	} else {
		currentType=tpInfo[[insNo]]
		deref(changed)=(! (typesMatch(currentType,newType)))
		if (deref(changed)) {
			#browser()
		}
	}

	newType$args=argTypes
	tpInfo[[insNo]]<-newType
	deref(typeInformation)=tpInfo
	insNo
	
		
}


