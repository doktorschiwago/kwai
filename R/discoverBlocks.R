

discoverBlocks2 <-function(opTable) {
	currentBlock=list()
	currentBlock$start=0
	currentBlock$firstBlock=TRUE
	currentBlock$deps=list()
	currentBlock$terminatingBranch=FALSE
	
	blockList=list()
	blockList[[opTable[[1]]$opNumber]]<-currentBlock

	print(length(blockList[2]))
	print(length(currentBlock))

	i=1
	length=length(opTable)

	branch=-1

	#browser()

	#the following loop collects all places, where a block boundary is needed

	while (i<=length) {
		op=opTable[[i]]$opName

		branch=opTable[[i]]$branchIns
		if (is.null(branch)) branch=-1
		else {
			branch=opTable[[i]]$goto

			if (is.null(branch)) branch=0
		}
		
		if (branch>-1) {
			if (op != "RETURN.OP") {
				if (is.null(blockList[branch][[1]])) {
					blockList[[branch]]=list()
					blockList[[branch]]$deps=list()
					#if op before new block has fallthrough then add it to dep
					#browser()
					if (opTable[[branch-1]]$fallthrough) {
						blockList[[branch]]$deps[[1]]=branch-1
					}
					blockList[[branch]]$start=branch
					blockList[[branch]]$terminatingBranch=FALSE
				}
				block=blockList[[branch]]
				#browser()
				found=FALSE
				if (length(block$deps)>0) {
					for (j in 1:length(block$deps)) {
						if (block$deps[[j]]==currentBlock$start) {
							found=TRUE
							break
						}
					}
				}
				if (! found) {
					blockDeps=block$deps
					blockDeps[[length(blockDeps)+1]]=i
					blockList[[branch]]$deps=blockDeps
				}
			}
		}
		i=i+1	

		if (i<length) {
			if (branch > -1) {
				if (is.null(blockList[i][[1]])) {
					blockList[[i]]=list()
					blockList[[i]]$deps=list()

					blockList[[i]]$start=i
					blockList[[i]]$terminatingBranch=FALSE
				}
				#browser()	
				if (opTable[[i-1]]$fallthrough) {
					blockDeps=blockList[i]$deps
					blockDeps[[length(blockDeps)+1]]=i-1
					blockList[[i]]$deps=blockDeps
				}
			} else if (! is.null(blockList[i][[1]])) {
				#so theres a new block, but no branch 
				blockDeps=blockList[[i]]$deps
				blockDeps[[length(blockDeps)+1]]=i-1
				blockList[[i]]$deps=blockDeps
			}
		}
	}

	#browser()

	i=1
	j=1
	blockList2=list()
	blockList2[[j]]=currentBlock

	# the following loop wlaks through the blockList and sets the end attribute for each block
	# additionaly the list condensed, so we have an entry for each block

	while (i<length) {
		if (i==length(blockList)) {
			break
		} else if (! is.null(blockList[[i+1]])) {
			currentBlock$end=i
			blockList2[[j]]=currentBlock
			currentBlock=blockList[[i+1]]
			j=j+1

		}
		i=i+1
	}
	currentBlock$end=length
	blockList2[[j]]=currentBlock

	length=length(blockList2)

	#browser()

	#the following loop walks though the blocklist and sets the $deps attribute

	for ( i in 1:length) {
		block=blockList2[[i]]
		if (length(block$deps)>0) {
			for (j in 1:length(block$deps)) {
				for (k in 1:length) {
					if ((blockList2[[k]]$start <= block$deps[j]) && (blockList2[[k]]$end >= block$deps[j])) {
						blockList2[[i]]$deps[j]=k
						
						break
					}
				}
			}
		}
	}
	i=1
	length=length(opTable)

	#browser()

	while (i<=length) {
		op=opTable[[i]]$opName

		branch=opTable[[i]]$branchIns
		if (is.null(branch)) branch=FALSE
		
		if (branch) {
			if (op != "RETURN.OP") {
				target=opTable[[i]]$goto
				found2=FALSE
			} else {
				found2=TRUE
			}

			found=FALSE
			#browser()
			for (j in 1:length(blockList2)) {
				if (blockList2[[j]]$end == i) {
					found=TRUE
					blockList2[[j]]$terminatingBranch=TRUE
				}
				if (op != "RETURN.OP") {
					if (blockList2[[j]]$start == target) {
						found2=TRUE
						opTable[[i]]$goto=j
					}
				}
			}
			if (! found) stop("Temrinating Branch could not be set")
			if (! found2) stop("Error label could not be replaced with block")
		}
		#browser()
		i=i+1	

	}

	length=length(blockList2)
	#browser()

	for ( i in 1:length) {
		blockList2[[i]]$opTable=opTable[blockList2[[i]]$start:blockList2[[i]]$end]
	}

	return(blockList2)
}
