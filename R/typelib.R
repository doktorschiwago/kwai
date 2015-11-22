tpLogical="logical"
tpAny="any"
tpVoid="void"
tpNumeric="numeric"
tpFunctionName="function_name"
tpMissing="missing"
tpNull="null"
tpDots="dots"

func_type = setClass("func_type",
	slots = list(
		funName = "character",
		funClass = "character",
		promiseList = "ANY",
		funcContext = "ANY",
		argListNamedPos = "ANY"

	)
)

kwai_type = setClass("kwai_type",
	slots = list(
		name = "character",
		subName = "character",
		vectorLength = "numeric",
		neededType = "ANY",
		args = "ANY",
		funcType = "func_type",
		argName = "character"
	)
)

setMethod("initialize",
          "kwai_type",
			function(.Object, name="", subName="",vectorLength=0) {
			#browser()

			.Object@name=name
			.Object@subName=subName
			.Object@vectorLength=vectorLength

			if (name==tpAny) {
				if (subName == "") {
					if (vectorLength != 0) {
						stop("for type Any wothout subtype you cannot specifiy vectorLength")
					}
				}
				return(.Object)
			} else if ((name==tpVoid) || (name==tpFunctionName) || (name==tpMissing) || (name==tpDots)) {
				if (subName != "") {
					stop(paste("for type", name, "you cannot specifiy subname"))
				}
				if (vectorLength != 0) {
					stop(paste("for type", name, "you cannot specifiy vectorLength"))
				}
				if ((name==tpFunctionName)) {
					.Object@funcType=new("func_type")
				}
				return(.Object)
			} else {
				if (subName != "") {
					stop("not setting subNames for types other than any not supported!")
				}
				if (vectorLength==0) {
					stop("arg vectorLength must be set!")
				}
				return(.Object)
			}

			stop("unreachable")
		}
)


getType=function(...) {
	return(new("kwai_type", ...))
}

tpGetName = function(type, allowNull=FALSE) {
	if (is.null(type)) {
		if (allowNull) {
			return("no type (NULL)")
		} else {
			stop("type is NULL")
		}
	}
	return(type@name)
}

tpGetNeededType = function(type, allowNull=FALSE) {
	if (is.null(type)) {
		if (allowNull) {
			return(NULL)
		} else {
			stop("type is NULL")
		}
	}
	return(type@neededType)
}

tpGetVectorLength = function(type) {
	return(type@vectorLength)
}

tpSetVectorLength = function(type, vectorLength) {
	type@vectorLength=vectorLength
	return(type)
}

tpGetSubname = function(type) {
	#browser()
	if (type@name != tpAny) {
		stop("only any has a subname")
	} else if (type@subName == "") {
		stop("subName not set")
	}
	return(type@subName)
}


tpSetNeededType = function(type, neededType) {
	type@neededType=neededType
	return(type)
}

tpSetArgs = function(type, args) {
	#browser()
	type@args=args
	return(type)
}

tpGetArg = function(type, arg) {
	type@args[[arg]]
}

tpSetFunName = function(type, funName) {
	#browser()
	type@funcType@funName=funName
	return(type)
}

tpGetFunName = function(type) {
	#browser()
	return(type@funcType@funName)
}

tpSetFunClass = function(type, funType) {
	#browser()
	type@funcType@funClass=funType
	return(type)
}

tpGetFunClass = function(type) {
	#browser()
	return(type@funcType@funClass)
}

tpSetPromiseList = function(type, promiseList) {
	#browser()
	type@funcType = new("func_type")
	type@funcType@promiseList = promiseList
	return(type)
}

tpGetPromiseList = function(type) {
	#browser()
	return(type@funcType@promiseList)
}

tpGetArgName = function(type) {
	#browser()
	return(type@argName)
}

tpSetArgName = function(type,argName) {
	type@argName=argName
	return(type)
}

tpSetFuncType = function(type,funcType) {
	type@funcType=funcType@funcType
	return(type)
}

tpSetFuncContext = function(type,funcContext) {
	type@funcType@funcContext=funcContext
	return(type)
}

tpGetFuncContext = function(type) {
	return(type@funcType@funcContext)
}

tpSetArgListNamedPos = function(type,argListNamedPos) {
	type@funcType@argListNamedPos=argListNamedPos
	return(type)
}

tpGetArgListNamedPos = function(type) {
	return(type@funcType@argListNamedPos)
}

ops=list()
ops[[tpNumeric]]=list(
	coerce=function(x,type) {
		#browser()
		switch(type@name,
			numeric={x},
			{stop("in the name of love")}
		)
	},
	mul=function(ir,x,y) {
		binOp(ir, FMul, x,y)
	},
	add=function(ir,x,y) {
		binOp(ir, FAdd, x,y)
	},
	gt=function(ir,x,y) {
		createFCmp(ir, FCMP_UGT, x,y)
	},
	readVar=function(ir,x) {
		x
	}
)





typeNumber<-function(type) {
	#browser()
	if (type@name==tpAny) {
		2
	} else {
		1
	}
}

higherType<-function(type1,type2) {
	newType=list()
	if (is.null(type1)) {
		if (is.null(type2)) {
			stop("both types are null!")
		} else {
			return(type2)
		}
	} else if (is.null(type2)) {
		return(type1)
	}

	compType=0

	if (type1@name == tpAny) {
		if (type2@name == tpAny) {
			#browser()
			if ((type1@subName != type2@subName) || (type1@vectorLength != type2@vectorLength)) {
				return(getType(name=tpAny))
			}
			return(type1)
			
		} else {
			compType=1
		}
	} else if (type2@name == tpAny) {
		compType=2
	} else {
		compType=3
	}

	if (compType < 3) {
		#browser()
		if (compType==1) {
			anyType=type1
			natType=type2
		} else {
			anyType=type2
			natType=type1
		}
		if (anyType@subName == "") {
			return(getType(name=tpAny))
		}
		if (anyType@vectorLength != natType@vectorLength) {
			stop("vectorLength differs between types!")
		}
		if (anyType@subName == natType@name) {
			return(anyType)
		}
		return(getType(name=tpAny))
	} 
	stop("unreachable")
}


lowerType<-function(type1,type2) {
	#browser()
	if (is.null(type1)) {
		if (is.null(type2)) {
			stop("both types are null!")
		} else {
			return(type2)
		}
	} else if (is.null(type2)) {
		return(type1)
	} else if (type1@name != type2@name) {
		if (type1@name == tpAny) {
			abst_type=type1
			nat_type=type2
		} else if (type2@name == tpAny) {
			abst_type=type2
			nat_type=type1
		} else {
			stop("no lower type deductible as both are native but different")
		}
		if (abst_type@subName == "") {
			return(nat_type)
		}

		if ((abst_type@subName == nat_type@name) && (abst_type@vectorLength == nat_type@vectorLength)) {
			return(nat_type)
		}
		stop("no lower type deductible as the abstract subname differs from the native name")
	} else if (type1@name == tpAny) {
		
		if (type1@subName == "") {
			return(type2)
		} else if (type2@subName == "") {
			return(type1)
		} else if ((type2@subName == type1@subName) && (type2@vectorLength == type1@vectorLength)) {
			return(type1)
		}
		stop("no lower type deductible as both types differ in subName or vectorLength")
	} 
	stop("unreachable")
}

typesMatch = function(type1,type2) {
	return(type1@name == type2@name && type1@vectorLength == type2@vectorLength)
}


