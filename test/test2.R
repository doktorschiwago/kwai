garch_loglik2<-function(omega0,alpha,beta,x){
	# Volatility and loglik initialisation
	loglik=0
	h=var(x=x+2)
	# Start of the loop
	vol=c()
	for (i in 2:length(x)){
		epsilon=x[i-1]/sqrt(h)
		h=omega0+alpha*(epsilon)^2+beta*h
		loglik=loglik+dnorm(x[i],0,sqrt(h),log=TRUE)
	}
	return(-1*loglik)
}


library(kwai)

#debug(inferType2)
llvmFunc=byte2llvm(garch_loglik2)
print(myfunction)
print(myfunction(-11,0))
print(llvmFunc(-11,0))

qq=function(x) {
	if (missing(x)) {
		print("missing x")
	}
	x=2
	if (missing(x)) {
		print("missing x")
	}
}

