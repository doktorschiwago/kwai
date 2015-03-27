
tpLogical="logical"
tpAny="any"
tpVoid="void"
tpNumeric="numeric"
tpFunctionName="function_name"
tpMissing="missing"
tpNull="null"
tpDots="dots"

getType=function(name,vector=FALSE) {
	return(list(name=name,vector=vector))
}

ops=list()
ops[[tpNumeric]]=list(
	coerce=function(x,type) {
		#browser()
		switch(type$name,
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
	if (type$name==tpAny) {
		2
	} else {
		1
	}
}

higherType<-function(type1,type2) {
	newType=list()
	if (is.null(type1)) {
		if (is.null(type)) {
			stop("both types are null!")
		} else {
			type2
		}
	} else if (is.null(type2)) {
		type1
	} else {
		if (type1$vector || type2$vector) {
			newType$vector=TRUE
		} else {
			newType$vector=FALSE
		}

		if (max(typeNumber(type1),typeNumber(type2))==2) {
			newType$name=tpAny
		} else if (max(typeNumber(type1),typeNumber(type2))==1) {
			if (type1$name==type2$name) {
				newType$name=type1$name
			} else {
				newType$name=tpAny
			}
		}
		newType
	}
}


lowerType<-function(type1,type2) {
	if (is.null(type1)) {
		if (is.null(type2)) {
			stop("both types are null!")
		} else {
			type2
		}
	} else if (is.null(type2)) {
		type1
	} else {

		newType=list()
		if (type1$vector && type2$vector) {
			newType$vector=TRUE
		} else {
			newType$vector=FALSE
		}

		if (min(typeNumber(type1),typeNumber(type2))==2) {
			newType$name=tpAny
		} else if (type1$name==type2$name) {
		
			newType$name=type1$name
		} else {
			newType$name=tpAny
		}
		newType
	}
}

typesMatch = function(type1,type2) {
	return(type1$name == type2$name && type1$vector == type2$vector)
}

rType2Llvm <- function(rType) {
	switch(rType$name,
		"numeric" = { DoubleType },
		"any" = {SEXPType},
		{stop(paste("Type",rType,"is unknown"))}
	)
}
