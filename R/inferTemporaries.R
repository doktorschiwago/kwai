

inferTemporaries<-function(opTable) {

	

	#default is temporary
	defFunc=function(opNumber, opName, ...) {

		#browser()

		argList=list(...)
		argTemp=list()

		for (arg in names(opTable[[opNumber]]$argTypes)) {
			if (opTable[[opNumber]]$argTypes[[arg]] == "stack") {
				argTemp[[arg]]=argList[[arg]]
				if (is.null(argTemp[[arg]])) {
					argTemp[[arg]]=FALSE
				} else if (is.na(argTemp[[arg]])) {
					argTemp[[arg]]=FALSE
				}
			}
		}

		opTable2=opTable
		opTable2[[opNumber]]$argTemp=argTemp
		opTable<<-opTable2

		switch(opName,
			"DUP3RD.OP" =,
			"DUP2ND.OP" =,
			"DUP.OP" =,
			"GETVAR.OP" =,
			"SETVAR.OP" = { return(FALSE)}
		)
		return(TRUE)
	}
	itHandler=createStdHandler(func=defFunc)

	itHandler$GETVAR.op=function(...) { return(FALSE)}

	visitStackMachine3(opTable,itHandler, initStack="default")

	return(opTable)
}
