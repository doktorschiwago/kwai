
relevantFile="/usr/lib/debug/usr/lib/R/lib/libR.so"

library(compiler)

print("Break in debugger now")
print("and issue the following command:")
print("maint print msymbols msymbols")
print("then return here and press enter")
readLines(file("stdin"), n=1)

#browser()

lines=scan("msymbols", what="", sep="\n")
startLine=grep(paste("Object file",relevantFile), lines)+1

#browser()

symbolTable=list()

currentSymbol=""
currentAddress=""

patLine="\\[\\s*\\d+\\] \\D 0x([0-9a-f]+) ([^ ]+) section.*"

while (TRUE) {
	line=lines[[startLine]]
	if (grepl(patLine, line)) {
		currentAddress=sub(patLine, "\\1",line)
		currentSymbol=sub(patLine, "\\2", line)
		# if theres a dot, take only in string front of dot
		#browser()
		#currentSymbol=strsplit(currentSymbol,"\\.")[[1]][[1]]
		
		#browser()
		symbolTable[[currentSymbol]]=currentAddress
		startLine=startLine+1
	} else {
		break
	}
}

saveRDS(symbolTable,"symbolTable.raw")
