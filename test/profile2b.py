#!/usr/bin/python

import re
import pdb

relevantLine = re.compile('#(\d+)\s+(?:0x[\da-f]+\sin\s)?([^\s]+)')

func="pisum"

with open("profile.txt") as f:
	content = f.readlines()


outputLine=False



for line in content:
	match=relevantLine.match(line)
	if match:
		#pdb.set_trace()
		if outputLine:
			if match.group(2) == func:
				outputLine=False
				print("------------------")
			else:
				print(match.group(2))
		else:
			if match.group(1) == "0":
				if match.group(2) != func:
					print(match.group(2))
					outputLine=True
