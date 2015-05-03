



inferBuiltinType <- function(funcName) {
	switch(funcName,
		"as.character" = ,
		"error" =,
		"is.na" = ,
		"pmatch" = ,
		"list" = ,
		"length" = {
			return(getType(tpAny))
		},
		{
			stop(paste("unknown function: ",funcName))
		}
	)
}

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


inferType2 <- function(op,insNo,insList,insNo2,args,vars,constants,typeInformation, changed) {
	tpInfo=deref(typeInformation)
	argTypes=list()

	#browser()

	if (is.null(tpInfo[insNo][[1]])) {
		currentNeededType=NULL
	} else {
		currentNeededType=tpGetNeededType(tpInfo[[insNo]])
	}
	switch(op,
		"SUB.OP" = ,
		"MUL.OP" = ,
		"ADD.OP" = ,
		"EQ.OP" = , 
		"GT.OP" = {
			#browser()
			argTypes$A=tpInfo[[insList[[insNo2-1]]]]
			argTypes$B=tpInfo[[insList[[insNo2]]]]
			#browser()

			switch(op,
				"EQ.OP" = ,
				"GT.OP" = {
					#if (! is.null(currentNeededType)) {
					#	newType=higherType(currentNeededType,getType(name=tpLogical,vector=currentNeededType$vector))
					#} else 

					newType=higherType(argTypes$A,argTypes$B)						

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
							neededType=lowerType(argTypes$A,argTypes$B)
						}
						argTypes$A=tpSetNeededType(argTypes$A, higherType(tpGetNeededType(argTypes$A),neededType))
						argTypes$B=tpSetNeededType(argTypes$B, higherType(tpGetNeededType(argTypes$B),neededType))
					} else {
						neededType=NULL
					}
				},
				{
					newType=lowerType(higherType(argTypes$A,argTypes$B),currentNeededType)
					neededType=newType		
					argTypes$A=tpSetNeededType(argTypes$A, higherType(tpGetNeededType(argTypes$A),neededType))
					argTypes$B=tpSetNeededType(argTypes$B, higherType(tpGetNeededType(argTypes$B),neededType))		
				}			
			)
			#browser()

			tpInfo[[insList[[insNo2-1]]]]=argTypes$A
			tpInfo[[insList[[insNo2]]]]=argTypes$B
		},
		"GETVAR.OP" = {
			newType=deref(vars)[[as.character(constants[[1+args[1]]])]]
		
			if (is.null(newType)) {
				stop(paste("Variable ",as.character(constants[[1+args[1]]]), "not defined before"))
			}
		},	
		"STARTSUBSET2.OP" =,
		"AND1ST.OP" =,
		"BRIFNOT.OP" =,
		"GOTO.OP" =,
		"POP.OP" = {
			newType=getType(name=tpVoid)
			if (op == "BRIFNOT.OP") {
				argTypes$A=tpInfo[[insList[[insNo2]]]]
				neededType=getType(name=tpLogical,vectorLength=1)
				argTypes$A=tpSetNeededType(argTypes$A,higherType(neededType,tpGetNeededType(argTypes$A)))
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
			
			if (typeof(var)=="double") {
				if (is.null(currentNeededType) || tpGetName(currentNeededType)==tpNumeric) {
					newType=getType(name=tpNumeric, vectorLength=length(var))
				} else {
					newType=getType(name=tpAny, subName=tpNumeric, vectorLength=length(var))
				}
			} else {
				newType=getType(name=tpAny)
			}
				
		},
		"RETURN.OP" = {
			#browser()
			
			argTypes$A=tpInfo[[insList[[insNo2]]]]
			
			if (! is.null(currentNeededType)) {
				argTypes$A=tpSetNeededType(argTypes$A,higherType(currentNeededType,tpGetNeededType(argTypes$A)))
				tpInfo[[insList[[insNo2]]]]=argTypes$A
				newType=currentNeededType
				newType=tpSetNeededType(newType, currentNeededType)
			} else {
				newType=tpInfo[[insList[[insNo2]]]]
			}
		},
		"GETINTLBUILTIN.OP" =,
		"GETBUILTIN.OP" =,
		"GETFUN.OP" = {
			#browser()
			newType=getType(name=tpFunctionName)
			newType=tpSetFunName(newType,as.character(constants[[1+args[1]]]))
			switch(op,
				"GETFUN.OP" = { 
					funBody=body(get(as.character(constants[[1+args[1]]])))
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
		},
		"MAKEPROM.OP" = {
			#browser()

			changed2=FALSE
			changed3=as.ref(changed2)


			if (is.null(tpInfo[insNo][[1]])) {
				tmpType=list(promiseList=list())
				promTypeList=tmpType$promiseList
			} else {
				tmpType=tpInfo[[insNo]]
				promTypeList=tpGetPromiseList(tmpType)
			}

			promSource=constants[[1+args[1]]]
			promIns=promSource[[2]][-1]
			promConst=promSource[[3]]

			
			promTypeList2=as.ref(promTypeList)

			myInferType<-function(op,insNo,insList,insNo2,args2) {
				inferType2(op,insNo,insList,insNo2,args2,vars,promConst,promTypeList2,changed3)
			}


			visitStackMachine(promIns,myInferType)

			newType = promTypeList[length(promIns)][[1]]
			newType = tpSetPromiseList(newType,promTypeList)
		},
		"CALLBUILTIN.OP" =,
		"CALL.OP" = {
			#browser()
			callSymbol=constants[[1+args[[1]]]]
			argCount=length(callSymbol)-1
			functionType=tpInfo[[insList[[insNo2-argCount]]]]
			
			if (tpGetFunClass(functionType) == "function") {
				
				
				argListPos=list()
				argListPosCount=0
				argListNamed=list()
				argListNamedPos=list()
			
				if (argCount>0) {
					for (i in (1:argCount)) {
						#browser()
						if (length(tpGetArgName(tpInfo[[insList[[insNo2+1-i]]]])) == 0) {
							argListPos[[argListPosCount+1]]=tpInfo[[insList[[insNo2+1-i]]]]
							argListPosCount=argListPosCount+1;
						} else {
							argListNamed[[tpGetArgName(tpInfo[[insList[[insNo2+1-i]]]])]]=tpInfo[[insList[[insNo2+1-i]]]]
							argListNamedPos[[i]]=tpGetArgName(tpInfo[[insList[[insNo2+1-i]]]])
						}
					}
				}
				ctxt=initCompileContext(get(tpGetFunName(functionType)),tpGetFunName(functionType))
				ctxt=inferFunctionType(ctxt,list(pos=argListPos,named=argListNamed))
				newType=ctxt$returnType
				newType = tpSetFuncType(newType,functionType)
				newType = tpSetFuncContext(newType, ctxt)
				newType = tpSetArgListNamedPos(newType, argListNamedPos)
			} else {
				#browser()
				newType=inferBuiltinType(as.character(callSymbol[1]))
				newType = tpSetFuncType(newType,functionType)	
			}
			
		},
		"SETTAG.OP" = {
			#browser()			
			newType=tpInfo[[insList[[insNo2]]]]
			newType=tpSetArgName(newType,as.character(constants[[1+args[1]]]))
		},
		"CALLSPECIAL.OP" = {
			#browser()
			switch(as.character(constants[[1+args[1]]][1]),
				"missing" = {
					newType=getType(name=tpLogical, vector=FALSE)
				}, {
					stop(paste("CALLSPECIAL.OP ",op, " not supported"))
				}
			)
		},
		"LDNULL.OP" = {
			newType=getType(name=tpNull,vector=FALSE)
		},
		"DODOTS.OP" = {
			newType=getType(name=tpDots)
		},
		"DFLTSUBSET2.OP" = {
			varType=tpInfo[[insList[[insNo2-1]]]]
			subsetType=tpInfo[[insList[[insNo2]]]]

			if (varType$vector==FALSE) {
				stop("Trying to subset sth. that is not a vector")
			}

			newType=varType

			if (subsetType$vector==FALSE) {
				newType$vector=FALSE
			}
		
			
		},
		{
			stop(paste("Opcode ",op, " not supported"))
		}
	)
	
	if (is.null(tpInfo[insNo][[1]])) {
		deref(changed)=TRUE
		newType=tpSetNeededType(newType, NULL)
	} else {
		currentType=tpInfo[[insNo]]
		deref(changed)=(! (typesMatch(currentType,newType)))
		if (deref(changed)) {
			#browser()
		}
	}
	#browser()
	newType=tpSetArgs(newType,argTypes)
	tpInfo[[insNo]]<-newType
	deref(typeInformation)=tpInfo
	insNo
	
		
}


