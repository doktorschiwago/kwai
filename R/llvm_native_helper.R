#SEXP foo(SEXP fn, SEXP elmt1, SEXP elmt2, SEXP rho)
#{
#     SEXP R_fcall, args, ans,s;
##     PROTECT( R_fcall = lang3(fn, elmt1, elmt2) );
#     s = CDR(R_fcall);
#     SET_TAG(s, install("x"));
#     s = CDR(R_fcall);
#     SET_TAG(s, install("y"));
 #    ans = eval(R_fcall, rho);
#     UNPROTECT(1);
#     return ans;
#}


#in general all calls should have no impacy on the protecion stack. all calls to protec must be matches to a call to unprotect. The
#result should be returned unprotected


r_native_module = setRefClass("r_native_module",
    # Define the slots
    fields = list(

			mod = "Module",

			R_NilValue = "GlobalVariable",
			R_GlobalEnv = "GlobalVariable",

			Rf_lcons = "GlobalVariable",
			
			Rf_protect = "Function",
			Rf_unprotect = "Function",
			Rf_allocVector = "Function",
			Rf_duplicate =  "Function",
			Rf_eval = "GlobalVariable",
			Rf_install = "GlobalVariable",
			Rf_mkString = "GlobalVariable",

			Rf_lang1 = "GlobalVariable",
			Rf_lang2 = "GlobalVariable",
			Rf_lang3 = "GlobalVariable",
			Rf_lang4 = "GlobalVariable",
			Rf_findVar = "GlobalVariable",

			SET_TAG = "GlobalVariable",
			SET_VECTOR_ELT= "GlobalVariable"

            ),
	methods = list(
		initialize = function(nativeModule, rNativeType) {

			#browser()
			
			mod<<-nativeModule

			#Resolve external Rf_lcons symbol
			Rf_lconsType=functionType(rNativeType,c(rNativeType,rNativeType),0)
			Rf_lconsSym = getNativeSymbolInfo("Rf_lcons")
			llvmAddSymbol(Rf_lconsSym) # $address)
			Rf_lcons <<- createGlobalVariable("Rf_lcons", mod, Rf_lconsType)

			#R_NilValue
			R_NilValue <<- getGlobalVariable(mod,"R_NilValue")

			#Resolve external Rf_protect
			Rf_protect <<- getModuleFunctions(mod)$Rf_protect

			#Resolve external Rf_unprotect
			Rf_unprotect <<- getModuleFunctions(mod)$Rf_unprotect

			#Resolve external Rf_allocVector
			Rf_allocVector <<- getModuleFunctions(mod)$Rf_allocVector

			#Resolve external Rf_duplicate
			Rf_duplicate <<- getModuleFunctions(mod)$Rf_duplicate	

			#Resolve external Rf_eval
			Rf_evalType=functionType(rNativeType,c(rNativeType,rNativeType),0)
			Rf_evalSym = getNativeSymbolInfo("Rf_eval")
			llvmAddSymbol(Rf_evalSym) # $address)
			Rf_eval <<- createGlobalVariable("Rf_eval", mod, Rf_evalType)	

			#Resolve external Rf_install
			Rf_installType=functionType(rNativeType,c(StringType),0)
			Rf_installSym = getNativeSymbolInfo("Rf_install")
			llvmAddSymbol(Rf_installSym) # $address)
			Rf_install <<- createGlobalVariable("Rf_install", mod, Rf_installType)	

			#Resolve lang3*
			Rf_lang1 <<- r_installFunctionSymbol("Rf_lang1",functionType(rNativeType,c(rNativeType),0))
			Rf_lang2 <<- r_installFunctionSymbol("Rf_lang2",functionType(rNativeType,c(rNativeType, rNativeType),0))
			Rf_lang3 <<- r_installFunctionSymbol("Rf_lang3",functionType(rNativeType,c(rNativeType, rNativeType, rNativeType),0))
			Rf_lang4 <<- r_installFunctionSymbol("Rf_lang4",functionType(rNativeType,c(rNativeType, rNativeType, rNativeType, rNativeType),0))

			R_GlobalEnv <<- r_installFunctionSymbol("R_GlobalEnv", rNativeType)
			Rf_findVar <<- r_installFunctionSymbol("Rf_findVar", functionType(rNativeType,c(rNativeType,rNativeType),0))

			Rf_mkString <<- r_installFunctionSymbol("Rf_mkString",functionType(rNativeType,c(StringType),0))

			SET_TAG <<- r_installFunctionSymbol("SET_TAG",functionType(VoidType,c(rNativeType,rNativeType),0))
			SET_VECTOR_ELT <<- r_installFunctionSymbol("SET_VECTOR_ELT",functionType(rNativeType,c(rNativeType, Int32Type, rNativeType),0))

          },



		r_installFunctionSymbol = function(name,type) {
			newSym = getNativeSymbolInfo(name)
			llvmAddSymbol(newSym) # $address)
			createGlobalVariable(name, mod, type)	
		},

		r_protect=function(builder, op) {
			createCall(builder,Rf_protect,op)
		},

		r_unprotect=function(builder, op) {
			createCall(builder,Rf_unprotect,createConstant(builder,as.integer(op)))
		},


		r_call=function(builder,func,...) {
			#browser()
			args=list(...)
			args2=c(builder,get(paste(sep="","Rf_lang",length(args)+1)), func, args)

			if (length(args)+3 != length(args2)) {
				stop("error!@")
			}
			call=do.call(createCall,args2)
			#r_protect(builder, call)
		
			return(call)
		},

		r_call_eval=function(builder,func,env=NULL, ...) {
			if (is.null(env)) {
				env=createLoad(builder,R_GlobalEnv)
			}
			call=r_call(builder,func,...)
			r_protect(builder,call)
			res=r_eval(builder,call, env)
			r_unprotect(builder,1)
			return(res)
		},

		r_install=function(builder, op) {
			op2=createGEP(builder,op,c(createConstant(builder,0L),createConstant(builder,0L)))
			createCall(builder,Rf_install, op2)

		},

		r_mkString=function(builder, op) {
			op2=createGEP(builder,op,c(createConstant(builder,0L),createConstant(builder,0L)))
			createCall(builder,Rf_mkString, op2)

		},

	
		r_eval=function(builder, op, env) {
			createCall(builder,Rf_eval, op, env)
		},

		r_findVar=function(builder, name, env= R_GlobalEnv) {			
			r_env=createLoad(builder, env)
			res=createCall(builder,Rf_findVar, name, r_env)
			#r_unprotect(builder, 1)
			return(res)
		},

		r_set_tag = function(builder, var, tag) {
			createCall(builder,SET_TAG, var, tag)
		},	  	

		r_set_vector_elt = function(builder, list, pos, element) {
			createCall(builder,SET_VECTOR_ELT, list, createConstant(builder,as.integer(pos)), element)
		},

		r_allocVector = function(builder, type, length) {
			#browser()
			destType=Rllvm:::getType(getFunctionArgs(mod$Rf_allocVector)[[2]])
			createCall(
				builder,
				mod$Rf_allocVector,
				createConstant(builder,as.integer(type)), 
				createIntCast(builder,createConstant(builder,as.integer(length)), destType)
			)
		}

	)
)
