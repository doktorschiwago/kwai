

lowerOps=function(opTable) {

	length=length(opTable)

	i=1

	while (i<=length) {
		op=opTable[[i]]
		opName=op$opName
		#browser()
		switch(opName,
			"STARTFOR.OP"= {
				#Push counter on stack
				counter=createLDCONST.OP(value=0)
				#add 1 to counter
				add1_1=createLDCONST.OP(value=1,opNumber=op$opNumber)
				add1_2=createADD.OP(expression=op$expression)
				
				#dup values
				dup_1=createDUP2ND.OP()
				dup_2=createDUP2ND.OP()

				#get value
				loop_val=createDFLTSUBSET2.OP()

				#store it
				store_var=createSETVAR.OP(varName=op$loop_var)

				#pop IT

				pop_var=createPOP.OP()
				
				#browser()
				opTable=c(opTable[1:(i-1)], list(counter, add1_1, add1_2, dup_1, dup_2, loop_val, store_var, pop_var), opTable[(i+1):length])
				i=i+6
			},
			"STEPFOR.OP" = {
				#check if we are at the final iteration
				#get length of seq
				#first get seq in front
				dup=createDUP2ND.OP()
				#then get length fun
				length_fun=createGETFUN.OP(funName="length")
				#seq is our arg so we change position
				length_arg=createSWAP.OP()
				#call it
				length_call=createCALL.OP(argCount=1)

				#now we have the length on top of the stack and the loop iteration is below

				#dup the loop iteration
				dup_2=createDUP2ND.OP()
				#compare them
				eq=createEQ.OP()
				
				#move back to loop header if unequal
				goto=createBRIFNOT.OP(goto=op$goto-1)
			
				opTable=c(opTable[1:(i-1)], list(dup, length_fun, length_arg, length_call, dup_2, eq, goto), opTable[(i+1):length])
				i=i+5
			
				#opTable=c(opTable[1:(i-1)], list(dup, length_fun, length_arg, length_call, createPOP.OP()), opTable[(i+1):length])
				#i=i+4
			},
			"ENDFOR.OP" = {
				#pop counter and seq
				pop_1=createPOP.OP()
				pop_2=createPOP.OP()
				#put NULL on stack
				ldnull=createLDNULL.OP()

				opTable=c(opTable[1:(i-1)], list(pop_1, pop_2, ldnull), opTable[(i+1):length])
				i=i+2
			},
			"GETBUILTIN.OP" = {
				op$opName="GETFUN.OP"
				opTable[[i]]=op
				i=i-1
			},
			"CALLBUILTIN.OP" = {
				op$opName="CALL.OP"
				opTable[[i]]=op
				i=i-1
			},
			"PUSHCONSTARG.OP" = {
				op$opName="LDCONST.OP"
				opTable[[i]]=op
				i=i-1
			},

			"PUSHARG.OP" = {
				opTable=c(opTable[1:(i-1)], opTable[(i+1):length])
				i=i-1
			},
			"PUSHTRUEARG.OP" = {
				op$opName="LDTRUE.OP"
				opTable[[i]]=op
				i=i-1
			}
		)
				

		i=i+1
		length=length(opTable)
	}
	
	res=fixGotos(opTable)

	return(res)
}
