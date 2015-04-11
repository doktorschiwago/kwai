createIR <- function(op,insNo,insList,insNo2,args,ir,params,constants, blockList,currentBlock,blockOps,vars,llvmContext,debugFunction,
	doReturn=TRUE) {
	#browser()

	switch(op,
		"GETVAR.OP" = {
			#browser()
			varName=as.character(constants[[1+args[1]]])
			var=params[[varName]]
			varType=vars[[varName]]$name
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
			typeOp=blockOps[[typeInformation$name]]

			typeOp$coerce(makeConstant(ir,as.double(constants[[1+args[1]]])),getType(tpNumeric,(length(var)>1)))
		},
		"SUB.OP"	= ,
		"MUL.OP" 	= ,
		"ADD.OP" 	= {
			#browser()
			#addin debug information
			debugSetLocation(ir, debugFunction, attr(constants[[1+args[[1]]]],"srcref")[1], attr(constants[[1+args[[1]]]],"srcref")[5])

			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			typeOp=blockOps[[typeInformation$name]]
			typeOpA=blockOps[[typeInformation$args$A$name]]
			typeOpB=blockOps[[typeInformation$args$B$name]]
			argA=typeOp$coerce(insList[[insNo2-1]],typeInformation$args$A)
			argB=typeOp$coerce(insList[[insNo2]],typeInformation$args$B)
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
		"GT.OP" 	=  {
			#addin debug information
			#browser()
			debugSetLocation(ir, debugFunction, attr(constants[[1+args[[1]]]],"srcref")[1], attr(constants[[1+args[[1]]]],"srcref")[5])

			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			if (typesMatch(typeInformation$args$A,typeInformation$args$B)) {
				typeOp=blockOps[[typeInformation$args$A$name]]
			} else {
				typeOp=blockOps[[higherType(typeInformation$args$A,typeInformation$args$B)$name]]
			}
			argA=typeOp$coerce(insList[[insNo2-1]],typeInformation$args$A)
			argB=typeOp$coerce(insList[[insNo2]],typeInformation$args$B)
			switch(op,
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

			typeOp=blockOps[["logical"]]
			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			cond=typeOp$coerce(insList[[insNo2]],typeInformation$args$A)
			createCondBranch(ir, cond,blockList[[currentBlock+1]]$block,blockList[[args[2]]]$block)


		},
		"GOTO.OP" = {
			createBranch(ir, blockList[[args[1]]]$block)
		},
		"RETURN.OP" = {
			
			
			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			typeOp=blockOps[[typeInformation$name]]
			retVal=typeOp$coerce(insList[[insNo2]],typeInformation$args$A)
			
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
			promBlockList[[1]]$typeInformation=typeInformation$promiseList


			myCreateIR<-function(op,insNo,insList,insNo2,args2) {
				createIR(op,insNo,insList,insNo2,args2,ir,params,promConst, promBlockList,1,blockOps,vars,llvmContext, debugFunction,
					FALSE)
			}


			res=visitStackMachine(promIns,myCreateIR)
			res$stackOnExit[[res$stackPos]]
		},
		"CALL.OP" = {
			browser()
			callSymbol=constants[[1+args[[1]]]]
			argCount=length(callSymbol)-1
			
			typeInformation=blockList[[currentBlock]]$typeInformation[[insNo]]
			functionType=typeInformation$funcType

			if (functionType$funType == "function") {
				
				llvmFun=compile2llvm(typeInformation$funcContext,llvmContext)

				if (argCount != length(typeInformation$funcContext$argList)) {
					stop("error: call and func definiton should match!")
				}

				namedArguments=typeInformation$argListNamedPos
				args=typeInformation$funcContext$args
				
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

