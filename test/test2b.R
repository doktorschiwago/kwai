source("test2.R", keep.source=TRUE)


#debug(inferType2)
llvmFunc=byte2llvm(myfunction)
print(myfunction)
print(myfunction(-11,0))
print(llvmFunc(-11,0))

