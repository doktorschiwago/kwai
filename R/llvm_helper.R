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


r_module = setRefClass("r_module",
    # Define the slots
    fields = list(

			mod = "Module",

			R_NilValue = "GlobalVariable",
			R_GlobalEnv = "GlobalVariable",

			Rf_lcons = "GlobalVariable",
			
			Rf_protect = "GlobalVariable",
			Rf_unprotect = "GlobalVariable",
			Rf_allocVector = "GlobalVariable",
			Rf_duplicate =  "GlobalVariable",
			Rf_eval = "GlobalVariable",
			Rf_install = "GlobalVariable",
			Rf_mkString = "GlobalVariable",

			Rf_lang1 = "GlobalVariable",
			Rf_lang2 = "GlobalVariable",
			Rf_lang3 = "GlobalVariable",
			Rf_lang4 = "GlobalVariable",
			Rf_findVar = "GlobalVariable",

			SET_TAG = "GlobalVariable"

            ),
	methods = list(
		initialize = function(modName) {

			#browser()
			

			mod<<-Module(modName)

			#Resolve external Rf_lcons symbol
			Rf_lconsType=functionType(SEXPType,c(SEXPType,SEXPType),0)
			Rf_lconsSym = getNativeSymbolInfo("Rf_lcons")
			llvmAddSymbol(Rf_lconsSym) # $address)
			Rf_lcons <<- createGlobalVariable("Rf_lcons", mod, Rf_lconsType)

			#R_NilValue
			R_NilValueSym = getNativeSymbolInfo("R_NilValue")
			llvmAddSymbol(R_NilValueSym) # $address)
			R_NilValue <<- createGlobalVariable("R_NilValue", mod, SEXPType)

			#Resolve external Rf_protect
			Rf_protectType=functionType(SEXPType,c(SEXPType),0)
			Rf_protectSym = getNativeSymbolInfo("Rf_protect")
			llvmAddSymbol(Rf_protectSym) # $address)
			Rf_protect <<- createGlobalVariable("Rf_protect", mod, Rf_protectType)

			#Resolve external Rf_unprotect
			Rf_unprotectType=functionType(VoidType,c(Int32Type),0)
			Rf_unprotectSym = getNativeSymbolInfo("Rf_unprotect")
			llvmAddSymbol(Rf_unprotectSym) # $address)
			Rf_unprotect <<- createGlobalVariable("Rf_unprotect", mod, Rf_unprotectType)

			#Resolve external Rf_allocVector
			Rf_allocVectorType=functionType(SEXPType,c(Int32Type, Int32Type),0)
			Rf_allocVectorSym = getNativeSymbolInfo("Rf_allocVector")
			llvmAddSymbol(Rf_allocVectorSym) # $address)
			Rf_allocVector <<- createGlobalVariable("Rf_allocVector", mod, Rf_allocVectorType)

			#Resolve external Rf_duplicate
			Rf_duplicateType=functionType(SEXPType,c(SEXPType),0)
			Rf_duplicateSym = getNativeSymbolInfo("Rf_duplicate")
			llvmAddSymbol(Rf_duplicateSym) # $address)
			Rf_duplicate <<- createGlobalVariable("Rf_duplicate", mod, Rf_duplicateType)	

			#Resolve external Rf_eval
			Rf_evalType=functionType(SEXPType,c(SEXPType,SEXPType),0)
			Rf_evalSym = getNativeSymbolInfo("Rf_eval")
			llvmAddSymbol(Rf_evalSym) # $address)
			Rf_eval <<- createGlobalVariable("Rf_eval", mod, Rf_evalType)	

			#Resolve external Rf_install
			Rf_installType=functionType(SEXPType,c(StringType),0)
			Rf_installSym = getNativeSymbolInfo("Rf_install")
			llvmAddSymbol(Rf_installSym) # $address)
			Rf_install <<- createGlobalVariable("Rf_install", mod, Rf_installType)	

			#Resolve lang3*
			Rf_lang1 <<- r_installFunctionSymbol("Rf_lang1",functionType(SEXPType,c(SEXPType),0))
			Rf_lang2 <<- r_installFunctionSymbol("Rf_lang2",functionType(SEXPType,c(SEXPType, SEXPType),0))
			Rf_lang3 <<- r_installFunctionSymbol("Rf_lang3",functionType(SEXPType,c(SEXPType, SEXPType, SEXPType),0))
			Rf_lang4 <<- r_installFunctionSymbol("Rf_lang4",functionType(SEXPType,c(SEXPType, SEXPType, SEXPType, SEXPType),0))

			R_GlobalEnv <<- r_installFunctionSymbol("R_GlobalEnv", SEXPType)
			Rf_findVar <<- r_installFunctionSymbol("Rf_findVar", functionType(SEXPType,c(SEXPType,SEXPType),0))

			Rf_mkString <<- r_installFunctionSymbol("Rf_mkString",functionType(SEXPType,c(StringType),0))

			SET_TAG <<- r_installFunctionSymbol("SET_TAG",functionType(VoidType,c(SEXPType,SEXPType),0))

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
			func2=r_install(builder, func)
			args=c(builder,get(paste(sep="","Rf_lang",length(args)+1)), func2, args)
			call=do.call(createCall,args)
			r_protect(builder, call)
		
			return(call)
		},

		r_call_eval=function(builder,func,...) {
			call=r_call(builder,func,...)
			res=r_eval(builder,call)
			return(res)
		},


		r_install=function(builder, op) {
			op2=createGEP(builder,op,c(createConstant(builder,0L),createConstant(builder,0L)))
			r_protect(builder, createCall(builder,Rf_install, op2))

		},

		r_mkString=function(builder, op) {
			op2=createGEP(builder,op,c(createConstant(builder,0L),createConstant(builder,0L)))
			r_protect(builder, createCall(builder,Rf_mkString, op2))

		},


		r_eval=function(builder, op) {
			r_globalenv=createLoad(builder,R_GlobalEnv)
			r_protect(builder,createCall(builder,Rf_eval, op, r_globalenv))
		},

		r_findVar=function(builder, name, env= R_GlobalEnv) {
			name2=r_install(builder, name)
			r_env=createLoad(builder, env)
			r_protect(builder, createCall(builder,Rf_findVar, name2, r_env))
		},

		r_set_tag = function(builder, var, tag) {
			createCall(builder,SET_TAG, var, tag)
		}

	)
)
