Opcodes.argc <- list(
BCMISMATCH.OP = 0,
"RETURN.OP" = 0,
"GOTO.OP" = 1,
"BRIFNOT.OP" = 2,
"POP.OP" = 0,
DUP.OP = 0,
PRINTVALUE.OP = 0,
STARTLOOPCNTXT.OP = 1,
ENDLOOPCNTXT.OP = 0,
DOLOOPNEXT.OP = 0,
DOLOOPBREAK.OP = 0,
STARTFOR.OP = 3,
STEPFOR.OP = 1,
ENDFOR.OP = 0,
SETLOOPVAL.OP = 0,
INVISIBLE.OP = 0,
"LDCONST.OP" = 1,
"LDNULL.OP" = 0,
LDTRUE.OP = 0,
LDFALSE.OP = 0,
"GETVAR.OP" = 1,
DDVAL.OP = 1,
"SETVAR.OP" = 1,
"GETFUN.OP" = 1,
GETGLOBFUN.OP = 1,
GETSYMFUN.OP = 1,
"GETBUILTIN.OP" = 1,
"GETINTLBUILTIN.OP" = 1,
CHECKFUN.OP = 0,
"MAKEPROM.OP" = 1,
DOMISSING.OP = 0,
"SETTAG.OP" = 1,
"DODOTS.OP" = 0,
"PUSHARG.OP" = 0,
"PUSHCONSTARG.OP" = 1,
PUSHNULLARG.OP = 0,
PUSHTRUEARG.OP = 0,
PUSHFALSEARG.OP = 0,
"CALL.OP" = 1,
"CALLBUILTIN.OP" = 1,
"CALLSPECIAL.OP" = 1,
MAKECLOSURE.OP = 1,
UMINUS.OP = 1,
UPLUS.OP = 1,
"ADD.OP" = 1,
"SUB.OP" = 1,
"MUL.OP" = 1,
"DIV.OP" = 1,
"EXPT.OP" = 1,
"SQRT.OP" = 1,
"EXP.OP" = 1,
"EQ.OP" = 1,
"NE.OP" = 1,
LT.OP = 1,
LE.OP = 1,
GE.OP = 1,
"GT.OP" = 1,
AND.OP = 1,
OR.OP = 1,
NOT.OP = 1,
DOTSERR.OP = 0,
STARTASSIGN.OP = 1,
ENDASSIGN.OP = 1,
STARTSUBSET.OP = 2,
DFLTSUBSET.OP = 0,
STARTSUBASSIGN.OP = 2,
DFLTSUBASSIGN.OP = 0,
STARTC.OP = 2,
DFLTC.OP = 0,
"STARTSUBSET2.OP" = 2,
"DFLTSUBSET2.OP" = 0,
STARTSUBASSIGN2.OP = 2,
DFLTSUBASSIGN2.OP = 0,
DOLLAR.OP = 2,
DOLLARGETS.OP = 2,
ISNULL.OP = 0,
ISLOGICAL.OP = 0,
ISINTEGER.OP = 0,
ISDOUBLE.OP = 0,
ISCOMPLEX.OP = 0,
ISCHARACTER.OP = 0,
ISSYMBOL.OP = 0,
ISOBJECT.OP = 0,
ISNUMERIC.OP = 0,
NVECELT.OP = 0,
NMATELT.OP = 0,
SETNVECELT.OP = 0,
SETNMATELT.OP = 0,
"AND1ST.OP" = 2,
"AND2ND.OP" = 1,
OR1ST.OP = 2,
OR2ND.OP = 1,
GETVAR_MISSOK.OP = 1,
DDVAL_MISSOK.OP = 1,
VISIBLE.OP = 0,
SETVAR2.OP = 1,
STARTASSIGN2.OP = 1,
ENDASSIGN2.OP = 1,
SETTER_CALL.OP = 2,
GETTER_CALL.OP = 1,
SWAP.OP = 0,
DUP2ND.OP = 0,
SWITCH.OP = 4,
RETURNJMP.OP = 0
)


Opcodes.stack <- list(
"RETURN.OP" = 99,
"GOTO.OP" = 99,
"BRIFNOT.OP" = 98,
"POP.OP" = 98,
"LDCONST.OP" = 1,
"GETVAR.OP" = 1,
"SETVAR.OP" = 99,
"ADD.OP" = -1,
"SUB.OP" = -1,
"MUL.OP" = -1,
"DIV.OP" = -1,
"EXPT.OP" = 0,
"SQRT.OP" = 0,
"EXP.OP" = 0,
"EQ.OP" = -1,
"NE.OP" = -1,
"LT.OP" = -1,
"LE.OP" = -1,
"GE.OP" = -1,
"GT.OP" = -1,
"AND.OP" = -1,
"OR.OP" = -1,
"NOT.OP" = -1,
"GETFUN.OP" = 1,
"GETBUILTIN.OP" = 1,
"GETINTLBUILTIN.OP" = 1,
"MAKEPROM.OP" = 1,
"CALL.OP" = 97,
"CALLBUILTIN.OP" = 97,
"PUSHCONSTARG.OP" = 1,
"CALLSPECIAL.OP" = 1,
"SETTAG.OP" = 0,
"LDNULL.OP" = 1,
"PUSHARG.OP" = 96,

"DODOTS.OP" = 1,

"AND1ST.OP" = 99,

"STARTSUBSET2.OP" = 99,
"DFLTSUBSET2.OP" = -1
)



visitStackMachine <- function(source,func, initStack=NULL) {
	
	length=length(source)
	i=1
	if (is.null(initStack)) {
		insNo=0
		insList=list()
		insStart=0
	} else {
		insNo=initStack$stackPos
		insList=initStack$stackOnExit
		insStart=initStack$insStart
	} 

	funArgCounter=list()
	

	while (i<=length) {

		op=as.character(source[i][[1]])

		switch(as.character(Opcodes.argc[[op]]),
			"1" = { 
				args=c(source[i+1][[1]])
			},
			"2" = { 
				args=c(source[i+1][[1]],source[i+2][[1]])
			},
			{
				args=list()
			}
		)
	
		deltaStack=Opcodes.stack[[op]]
		insNo2=deltaStack
		#browser()
		if (is.null(insNo2)) {
			stop(paste("Opcode ",op, " not supported"))
		} else if (insNo2==99) {
			func(op,i+insStart,insList,insNo,args)
			deltaStack=0
		} else if (insNo2==98) {
			func(op,i+insStart,insList,insNo,args)
			insNo=insNo-1
			deltaStack=-1
		} else if (insNo2==97) {
			#browser()
			insNo2=0 - funArgCounter[[length(funArgCounter)]]
			deltaStack=insNo2
			insNo2=insNo2+insNo

			insList[[insNo2]]=func(op,i+insStart,insList,insNo,args)
			insNo=insNo2
		} else if (insNo2==96) {
			deltaStack=0
		} else {
			insNo2=insNo2+insNo

			if (insNo2<1) { browser() }

			insList[[insNo2]]=func(op,i+insStart,insList,insNo,args)
			insNo=insNo2
		
			
		}

		switch(op,
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

		i=i+1+Opcodes.argc[[as.character(source[i][[1]])]]
	}
	return(list(stackOnExit=insList, stackPos=insNo))
}

