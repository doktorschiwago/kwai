createIR <- function(op,insNo,insList,insNo2,args,ir,params,constants, blockList,currentBlock,blockOps,vars,llvmContext,debugFunction,
	doReturn=TRUE) {
	#browser()

	switch(op,
		"GETVAR.OP" = {
			#browser()
			varName=as.character(constants[[1+args[1]]])
			var=params[[varName]]
			varType=tpGetName(vars[[varName]])
			if (class(var)[1]=="AllocaInst") {
				
				blockOps[[varType]]$readVar(ir,createLoad(ir,var))
				#createLoad(ir,var)
			} else {
				#blockOps[[tpAny]]$dup(ir,var)
				blockOps[[varType]]$readVar(ir,var)
			}
		},
		"LDCONST.OP" = {
			#browser()
			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			typeOp=blockOps[[tpGetName(typeInformation)]]

			typeOp$coerce(makeConstant(ir,as.double(constants[[1+args[1]]])),getType(tpNumeric,vectorLength=length(var)))
		},
		"SUB.OP"	= ,
		"MUL.OP" 	= ,
		"ADD.OP" 	= {
			#browser()
			#addin debug information
			debugSetLocation(ir, debugFunction, attr(constants[[1+args[[1]]]],"srcref")[1], attr(constants[[1+args[[1]]]],"srcref")[5])

			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			typeOp=blockOps[[tpGetName(typeInformation)]]
			typeOpA=blockOps[[tpGetName(tpGetArg(typeInformation, "left_operand"))]]
			typeOpB=blockOps[[tpGetName(tpGetArg(typeInformation, "right_operand"))]]
			argA=typeOp$coerce(insList[[insNo2-1]], tpGetArg(typeInformation, "left_operand"))
			argB=typeOp$coerce(insList[[insNo2]], tpGetArg(typeInformation, "right_operand"))
			switch(op,
				"MUL.OP" = {
					typeOp$mul(ir,argA,argB)
				}, 
				"ADD.OP" = {
					typeOp$add(ir,argA,argB)
				}, 
				"SUB.OP" = {
					typeOp$add(ir,argA,argB)
				}, 
				"GT.OP" = {
					
					typeOp$gt(ir,argA,argB)
				}, {
					stop("hamertime")
				}
			)

		},
		"EQ.OP"		= ,
		"GT.OP" 	=  {
			#addin debug information
			#browser()
			debugSetLocation(ir, debugFunction, attr(constants[[1+args[[1]]]],"srcref")[1], attr(constants[[1+args[[1]]]],"srcref")[5])

			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			if (typesMatch(tpGetArg(typeInformation, "left_operand"), tpGetArg(typeInformation, "right_operand"))) {
				typeOp=blockOps[[tpGetName(tpGetArg(typeInformation, "left_operand"))]]
			} else {
				typeOp=blockOps[[tpGetName(higherType(tpGetArg(typeInformation, "left_operand"), tpGetArg(typeInformation, "right_operand")))]]
			}
			argA=typeOp$coerce(insList[[insNo2-1]],tpGetArg(typeInformation, "left_operand"))
			argB=typeOp$coerce(insList[[insNo2]],tpGetArg(typeInformation, "right_operand"))
			switch(op,
				"EQ.OP" = {
					
					typeOp$eq(ir,argA,argB)
				},				
				"GT.OP" = {
					
					typeOp$gt(ir,argA,argB)
				}, {
					stop("hamertime")
				}
			)

		},
		"BRIFNOT.OP" = {
			#addin debug information
			#browser()
			debugSetLocation(ir, debugFunction, attr(constants[[1+args[[1]]]],"srcref")[1], attr(constants[[1+args[[1]]]],"srcref")[5])

			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			typeInformationArg=tpGetArg(typeInformation, "val")
			typeOpLogical=blockOps[["logical"]]
			typeOp=blockOps[[tpGetName(typeInformationArg)]]
			
			
			if (tpGetVectorLength(typeInformationArg) != 1) {
				reduced=typeOp$subset2(
					ir,
					insList[[insNo2]],
					typeOp$coerce(makeConstant(ir,1.0),getType(name=tpNumeric,vectorLength=1))
				)
				typeInformationArg=tpSetVectorLength(typeInformationArg,1)
			} else {
				reduced=insList[[insNo2]]
			}
			cond=typeOpLogical$coerce(reduced, typeInformationArg)
			createCondBranch(ir, cond,blockList[[currentBlock+1]]$block,blockList[[args[2]]]$block)


		},
		"GOTO.OP" = {
			createBranch(ir, blockList[[args[1]]]$block)
		},
		"RETURN.OP" = {
			
			
			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			typeOp=blockOps[[tpGetName(typeInformation)]]
			retVal=typeOp$coerce(insList[[insNo2]], tpGetArg(typeInformation, "val"))
			
			if (doReturn) {
				blockOps$any$cleanup(ir)
				createReturn(ir, retVal)
			} else {
				retVal
			}
		},
		"SETVAR.OP" = {
			createStore(ir, insList[[insNo2]],params[[as.character(constants[[1+args[1]]])]])
			insList[[insNo2]]
		},
		"MAKEPROM.OP" = {
			#browser()
			promSource=constants[[1+args[1]]]
			promIns=promSource[[2]][-1]
			promConst=promSource[[3]]

			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]

			promBlockList=list()
			promBlockList[[1]]=list()
			promBlockList[[1]]$typeInformation=tpGetPromiseList(typeInformation)


			myCreateIR<-function(op,insNo,insList,insNo2,args2) {
				createIR(op,insNo,insList,insNo2,args2,ir,params,promConst, promBlockList,1,blockOps,vars,llvmContext, debugFunction,
					FALSE)
			}


			res=visitStackMachine(promIns,myCreateIR)
			res$stackOnExit[[res$stackPos]]
		},
		"CALL.OP" = {
			#browser()
			callSymbol=constants[[1+args[[1]]]]
			argCount=length(callSymbol)-1
			
			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]

			if (tpGetFunClass(typeInformation) == "function") {
				
				llvmFun=compile2llvm(tpGetFuncContext(typeInformation),llvmContext)

				if (argCount != length(tpGetFuncContext(typeInformation)$argList)) {
					stop("error: call and func definiton should match!")
				}

				namedArguments=tpGetArgListNamedPos(typeInformation)
				args=tpGetFuncContext(typeInformation)$args
				
				callArgs=list()
	
				for (i in (1:argCount)) {
					if (is.null(namedArguments[i][[1]])) {
						posNo=i
					} else {
						posNo=args[[namedArguments[[i]]]]$pos
					}
					if (is.null(callArgs[posNo][[1]])) {
						callArgs[[posNo]]=insList[[insNo2-argCount+i]]
					} else {
						stop("arguments was set twice!")
					}
				}

				createCall(ir,llvmFun, .args=callArgs)

			} else {
				browser()
				stop("not yet implemented!")
				
			}
		},
		"GETFUN.OP" = ,
		"POP.OP" = {
			
		},
		{
			stop(paste("Opcode ",op, " not supported"))
		}
	)
}

