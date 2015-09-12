import gdb
import re
import time

print("hello from python")

#save pid of process
pid=gdb.execute("info inferiors", to_string=True)
print(pid)

pid=re.search("process\s(\d+)", pid) # Match

pid=pid.group(1)
print(pid)

gdb.execute("detach")
gdb.execute("set logging on")
gdb.execute("set logging file profile.txt")

#loop it
x = 0

while x < 10:
	#gdb.execute("attach " + pid)
	x = x + 1
	gdb.execute("bt")
	gdb.execute("detach")
	time.sleep(5)
