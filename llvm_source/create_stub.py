#!/usr/bin/env python
""" Usage: call with <filename> <typename>
"""

import sys
import clang.cindex

import pdb; 

from itertools import permutations, islice

import xml.etree.ElementTree as ET

import tempfile
import os

import re

#the following class is used to render the function body
class Fragment:

	def __init__(self, source='', stackArgNo=None, returnsValue=False, usesConstant=False):
		if type(source) is str:
			self.source = source
		else:
			#pdb.set_trace()
			self.source= ''

		self.stackArgs=[]

		if stackArgNo != None:
			self.stackArgs.append(stackArgNo)

		self.returnsValue=returnsValue

		self.funcRefList={}

		if not type(source) is str:
			self=self + source

		self.usesConstant=usesConstant

	def join(self, left, right):
		#pdb.set_trace()
		res=Fragment(
			source=left.source+right.source,
			returnsValue=left.returnsValue or right.returnsValue,
			usesConstant=left.usesConstant or right.usesConstant)

		for x in left.stackArgs:
			res.stackArgs.append(x)

		for x in right.stackArgs:
			if not x in res.stackArgs:
				res.stackArgs.append(x)

		for x in left.funcRefList.keys():
			#pdb.set_trace()
			res.funcRefList[x]=left.funcRefList[x]

		for x in right.funcRefList.keys():
			if not x in res.funcRefList:
				#pdb.set_trace()				
				res.funcRefList[x]=right.funcRefList[x]

		return res

	def rstrip(self,garbage):
		#pdb.set_trace()
		self.source=self.source.rstrip(garbage)
		return self

	def add(self,left,right):
		if type(right) is str:
			self.source+=right
			return self
		if type(left) is str:
			self.source=left+self.source
			return self
		elif left.__class__ is Fragment and right.__class__ is Fragment:
			return self.join(left, right)
		else:
			pdb.set_trace()

	def __add__(self, other):
		return self.add(self,other)

	def __radd__(self, other):
		return self.add(other,self)

	def addFuncRef(self, funcRef):
		if not funcRef.attrib["spelling"] in self.funcRefList:
			self.funcRefList[funcRef.attrib["spelling"]]=funcRef


#count usage of stack
stackPos=None

def nth(iterable, n, default=None):
        "Returns the nth item or a default value"
        return next(islice(iterable, n, None), default)

def findSwitchZero(node):
	
	if node.kind != clang.cindex.CursorKind.FUNCTION_DECL:
		return Fragment()

	if node.spelling != "bcEval":
		return Fragment()

	#pdb.set_trace()

	if not (node.is_definition()):
		node = node.get_definition()

	
	print "found bcEval!"

	return loopElements(node,1)

	#sys.exit()

def findSwitchOne(node):
	if node.kind != clang.cindex.CursorKind.SWITCH_STMT:
		return loopElements(node,1)

	if not any(node.get_children()):
		return Fragment()

	if node.get_children().next().kind != clang.cindex.CursorKind.UNARY_OPERATOR:
		return loopElements(node,1)

	if node.get_children().next().get_children().next().spelling != "which":
		return loopElements(node,1)

	print "found Switch!"

	return findSwitchTwo(nth(node.get_children(),1))


def getTokens(node):
	res=""
	for c in node.get_tokens():
		res+=c.spelling

	return res

def getBinaryToken(node):
	children=list(node.get_children())
	#pdb.set_trace()
	left=getTokens(children[0])
	right=getTokens(children[1])
	binOp=getTokens(node)

	
	return left[::-1]
	
def convertET(node, withKind=True):
	node2 = ET.Element(str(node.kind)[11:])

	node2.attrib["spelling"] = node.spelling
	node2.attrib["type"] = node.type.spelling

	if withKind:
		node2.attrib["kind"] = node.kind

	op_name=""
	'it seems like the type of binop is not avaible through libclang'
	if node.kind == clang.cindex.CursorKind.BINARY_OPERATOR:
		op_name_raw=getBinaryToken(node)
		#pdb.set_trace()
		for x in ["==", ">=", "<=", "+=", "-=", "*=", "/=", "!=", \
			"&&", "||", \
			"=", "+", ",", "-", ">", "<", "&", "|", "%", "/", "*"]:
			if op_name_raw.startswith(x[::-1]):
				op_name=x
				break
		if op_name=="":
			pdb.set_trace()

	elif node.kind == clang.cindex.CursorKind.UNARY_OPERATOR:
		for c in node.get_tokens():
			if c.spelling.startswith("-"):
				op_name="-"
				break
			elif c.spelling.startswith("+"):
				op_name="+"
				break
			elif c.spelling.startswith("*"):
				op_name="*"
				break
			elif c.spelling.startswith("&"):
				op_name="&"
				break
			elif c.spelling.startswith("!"):
				op_name="!"
				break
		#if op_name == "":
		#	pdb.set_trace()
	elif node.kind == clang.cindex.CursorKind.STRING_LITERAL:
		try:
			node2.attrib["spelling"] = node.get_tokens().next().spelling
		except StopIteration:
			
			node2.attrib["spelling"] = 'keinPlanString'

	elif node.kind == clang.cindex.CursorKind.INTEGER_LITERAL:
		try:
			node2.attrib["spelling"] = node.get_tokens().next().spelling
		except StopIteration:
			#pdb.set_trace()
			node2.attrib["spelling"] = 'keinPlanInt'

	#pdb.set_trace()
	
	elif node.kind == clang.cindex.CursorKind.COMPOUND_ASSIGNMENT_OPERATOR:
		for c in node.get_tokens():
			if c.spelling.startswith("-="):
				op_name=c.spelling
				break

	elif node.kind == clang.cindex.CursorKind.UNEXPOSED_EXPR:
		tokens=getTokens(node)
		if tokens.startswith("sizeof"):
			op_name="sizeof"
			#we need to save the tokens, because sizeof(float) cannot be extracted otherwise
			#pdb.set_trace()
			m = re.search('(sizeof\([^\)]+\))', tokens)
			node2.attrib["spelling"]=m.group(1)

	if op_name == "":
		#pdb.set_trace()
		#raise NameError('no binop found at' + node.location)
		op_name = "????"	

	node2.attrib["op_name"]=op_name

	for c in node.get_children():
		node2.append(convertET(c, withKind))

	return node2


def findSwitchTwo(node):

	global stackPos

	op_name = ""
	new_op_name = ""

	node2=convertET(node)

	ET.ElementTree(convertET(node,False)).write('out.xml')

	source=Fragment()
	
	for c in node2:
		if c.attrib["kind"] == clang.cindex.CursorKind.LABEL_STMT:
			if not c.attrib["spelling"].startswith("op_"):
				new_op_name = ""
				continue

			new_op_name = c.attrib["spelling"].lstrip("op_")

			if new_op_name in {
				"BCMISMATCH", "RETURN",
				"BRIFNOT", "GOTO", "POP", "DUP",
				"STARTLOOPCNTXT","ENDLOOPCNTXT",
				"DOLOOPNEXT", "DOLOOPBREAK","STARTFOR", "STEPFOR", "ENDFOR","SETLOOPVAL",
				"INVISIBLE", "LDCONST", "LDNULL", "LDTRUE", "LDFALSE",
				"GETVAR", "SETVAR", "DDVAL",
				"GETFUN", "GETGLOBFUN", "GETSYMFUN", "GETBUILTIN", "GETINTLBUILTIN", "CHECKFUN",  
				"MAKEPROM", "DOMISSING" , "SETTAG" , "DODOTS", 
				"PUSHARG", "PUSHCONSTARG", "PUSHNULLARG", "PUSHTRUEARG", "PUSHFALSEARG",
		  		"CALL", "CALLBUILTIN", "CALLSPECIAL", "MAKECLOSURE",
				"DOTSERR", "STARTASSIGN", "ENDASSIGN", "STARTSUBSET", "DFLTSUBSET",
		  		"STARTSUBASSIGN", "DFLTSUBASSIGN", "STARTC", "DFLTC", "STARTSUBSET2",
		  		"STARTSUBASSIGN2", "DFLTSUBASSIGN2",
		  		"GETVAR_MISSOK", "DDVAL_MISSOK", "VISIBLE", "SETVAR2", "STARTASSIGN2",
		  		"ENDASSIGN2", "SETTER_CALL", "GETTER_CALL", "SWAP", "DUP2ND",
		  		"SWITCH", "RETURNJMP", 
				"STARTVECSUBSET", "STARTMATSUBSET", "STARTSETVECSUBSET", "STARTSETMATSUBSET", 
				"VECSUBSET", "MATSUBSET",
				"OPCOUNT",
				"SUBSET_N", "SUBSET2_N", "SUBASSIGN_N", "SUBASSIGN2_N", "DOLLAR", "DOLLARGETS", 
				"VECSUBASSIGN", "MATSUBASSIGN", "STARTSUBSET_N", "STARTSUBASSIGN_N", "VECSUBSET2", 
				"MATSUBSET2", "VECSUBASSIGN2", "MATSUBASSIGN2", "STARTSUBSET2_N", "STARTSUBASSIGN2_N",
				"AND1ST", "AND2ND", "OR1ST", "OR2ND", "AND", "OR", "NOT",
				"DOTCALL", "SEQALONG", "SEQLEN",
				"PRINTVALUE", "COLON", "MATH1"
				}:

				new_op_name = ""
				continue

			#if new_op_name not in {
			#	"ADD"
			#	}:
			#
			#	new_op_name = ""
			#	continue

			#pdb.set_trace()

			print(new_op_name)

		if c.attrib["kind"] == clang.cindex.CursorKind.CASE_STMT:
			new_op_name = ""

		if not (new_op_name == op_name):
			if not (op_name == ""):
				source+="}\n\n"
				stackPos=None

			op_name = new_op_name
			
			if not (op_name == ""): 

				stackPos=0

				fragment=printChildren(c,";\n",True)
			

				source+="SEXP op_" + op_name + "("



				source+="SEXP rho"
				for x in fragment.stackArgs:
					source+=", SEXP _stack" + str(x)

				if fragment.usesConstant:
					source+=", SEXP constant"


				source+=") {\n"

				if fragment.returnsValue:
					source+="SEXP res=NULL;\n"
				
				source+="SEXP value=NULL;\n"
				source+="int R_BCNodeStackTop=0;\n"
				source+="int pc=0;\n"
		
				

				source+=fragment

				if fragment.returnsValue:
					source+="return res;"
				
				

		elif not (op_name == ""): 
			#pdb.set_trace()
			source+=printSource(c)

	return source


	

	

	

def printChildren(node, delimiter, trailingDelimiter):
	resChildren=Fragment()

	if type(node) is ET.Element:
		children=node
	elif type(node) is list:
		children=iter(node)
	else:
		raise NameError('unknown node type')

	for child in children:
		resChildren+=printSource(child)
		resChildren+=delimiter

	if not trailingDelimiter:
		resChildren=resChildren.rstrip(delimiter)
	
	return resChildren

def printCall(node):
	#pdb.set_trace()
	children=list(node)

	funcDec=children.pop(0)

	#pdb.set_trace()

	res=Fragment(source=node.attrib["spelling"]) + "(" + printChildren(children,",",False) + ")"
	res.addFuncRef(funcDec)

	return res

def printBinaryOperator(node):
	#pdb.set_trace()

	opName=node.attrib["op_name"]

	if len(list(list(node))) == 1:
		if opName == "+":
			return printSource(list(node)[0])
	
	return printSource(list(node)[0]) + opName + printSource(list(node)[1]) 

def printUnaryOperator(node):
	'it seems like the type of binop is not avaible through libclang'
	opName=node.attrib["op_name"]



	if opName == "+":
		return printSource(list(node)[0])

	return opName + printSource(list(node)[0])

def printPlain(node):
	#pdb.set_trace()
	if node.attrib["spelling"] != "" and node.attrib["kind"] != clang.cindex.CursorKind.UNEXPOSED_EXPR:
		return node.attrib["spelling"]

	return printChildren(node,"",False)

def printLiteral(node,escapeCharacter):
	#pdb.set_trace()
	if node.attrib["spelling"] != '':
		res = escapeCharacter + node.attrib["spelling"] + escapeCharacter   
	else:
		res = ''
	return res

def printDo(node):
	children=list(node)
	children.reverse
	children.pop(0)

	return "do {\n" + printChildren(list(node)[0],";\n",True) + "} while (" + printSource(children.pop(0)) + ");\n"
	
def printDecl(node):
	res=Fragment()
	#pdb.set_trace()
	for child in node:
		res += child.attrib["type"] + " " + child.attrib["spelling"];
		if len(child) > 1 or node.find('*/*').tag != 'TYPE_REF':
			res+="=" + printChildren(child,"",False);
		res+=";\n"
	return res;

def printIf(node):
	#pdb.set_trace()
	children=list(node)
	
	res="if (" + printSource(children[0]) + ")\n{\n" + printSource(children[1]) + "}\n"
	
	if len(children)>2:
		res+="else {" + printSource(children[2]) + "}\n"

	return res

def printUnexposed(node):
	children=list(node)
	if node.attrib["op_name"]=="" or node.attrib["op_name"]=="????":
		return printPlain(node)
	elif node.attrib["op_name"]=="sizeof":
		if len(children)==1:
			#pdb.set_trace()
			return "sizeof(" + printSource(children[0]) + ")"
		else:
			return node.attrib["spelling"]
	else:
		pdb.set_trace()

def printMember(node):
	if node.attrib["spelling"] == "sxpinfo":
		return"(" + printSource(list(node)[0]) + ")->" + node.attrib["spelling"]
	else:
		return"(" + printSource(list(node)[0]) + ")." + node.attrib["spelling"]

def printSource(node):
	global stackPos

	#first handling some special ones

	#no assignment to tag
	if node.find( \
"MEMBER_REF_EXPR[@spelling='tag']/PAREN_EXPR/BINARY_OPERATOR/UNEXPOSED_EXPR/DECL_REF_EXPR[@spelling='R_BCNodeStackTop']") != None \
		and node.attrib["kind"] == clang.cindex.CursorKind.BINARY_OPERATOR \
		and node.attrib['op_name'] == '=':
		return Fragment(source="_stack0->sxpinfo.type=") + printSource(list(node)[1]) + ";\n"
	#assignements for value into res
	elif node.find( \
"MEMBER_REF_EXPR[@spelling='sxpval']/MEMBER_REF_EXPR/PAREN_EXPR/BINARY_OPERATOR/UNEXPOSED_EXPR/DECL_REF_EXPR[@spelling='R_BCNodeStackTop']") != None \
		and node.attrib["kind"] == clang.cindex.CursorKind.BINARY_OPERATOR \
		and node.attrib['op_name'] == '=':
		
		res=Fragment(source='res=', returnsValue=True)
		res+=printSource(list(node)[1])
		return(res)
	#assignements of double or integer value into res
	elif node.find( \
"MEMBER_REF_EXPR/MEMBER_REF_EXPR/PAREN_EXPR/BINARY_OPERATOR/UNEXPOSED_EXPR/DECL_REF_EXPR[@spelling='R_BCNodeStackTop']") != None \
		and node.attrib["kind"] == clang.cindex.CursorKind.BINARY_OPERATOR \
		and node.attrib['op_name'] == '=':

		if node.find("MEMBER_REF_EXPR[@spelling='dval']") != None:
			res=Fragment(source='REAL(_stack0)[0]=', returnsValue=True)
		elif node.find("MEMBER_REF_EXPR[@spelling='ival']") != None:
			res=Fragment(source='INTEGER(_stack0)[0]=', returnsValue=True)
		else:
			pdb.set_trace()

		res+=printSource(list(node)[1]) + ";\n"
		res+='res=_stack0;'
		return(res)
	#requests for tag return 0
	elif node.find( \
"MEMBER_REF_EXPR[@spelling='tag']/PAREN_EXPR/BINARY_OPERATOR/UNEXPOSED_EXPR/DECL_REF_EXPR[@spelling='R_BCNodeStackTop']") != None :
		return Fragment(source="0")
	#assignements for value into res
	elif node.find( \
"PAREN_EXPR/UNARY_OPERATOR/PAREN_EXPR/BINARY_OPERATOR/PAREN_EXPR/UNARY_OPERATOR/INTEGER_LITERAL") != None:
		#GETSTACK
		pos=node.find( \
"PAREN_EXPR/UNARY_OPERATOR/PAREN_EXPR/BINARY_OPERATOR/PAREN_EXPR/UNARY_OPERATOR/INTEGER_LITERAL").attrib["spelling"]
		#res="_stack" + str(int(pos)-1)
		res=Fragment(source="_stack" + str(int(pos)-1), stackArgNo=int(pos)-1)
		return(res)
	elif node.find("UNARY_OPERATOR/StmtExpr/COMPOUND_STMT/INDIRECT_GOTO_STMT") != None:
		#NEXT()
		#return res
		return(Fragment(source="return res;"))
	#elif node.find("BINARY_OPERATOR/UNARY_OPERATOR/PAREN_EXPR/BINARY_OPERATOR/PAREN_EXPR/UNARY_OPERATOR/INTEGER_LITERAL"):
	elif node.find("UNARY_OPERATOR/PAREN_EXPR/BINARY_OPERATOR/PAREN_EXPR/UNARY_OPERATOR/INTEGER_LITERAL") != None:
		#pdb.set_trace()
		res=Fragment(source="res="+printSource(node.find('UNEXPOSED_EXPR')), returnsValue=True)
		return(res)
	elif node.tag=="UNEXPOSED_EXPR" and node.find("MEMBER_REF_EXPR[@spelling='sxpval']/MEMBER_REF_EXPR[@spelling='u']/PAREN_EXPR/BINARY_OPERATOR/UNEXPOSED_EXPR/DECL_REF_EXPR/[@spelling='R_BCNodeStackTop']") != None:
		if node.find(".//INTEGER_LITERAL") == None:
			pdb.set_trace()
		pos=node.find(".//INTEGER_LITERAL").attrib["spelling"]
		#pdb.set_trace()
		res=Fragment(source="_stack" + str(int(pos)-1), stackArgNo=int(pos)-1)
		return res
	elif node.tag=="BINARY_OPERATOR" and node.find("UNEXPOSED_EXPR/DECL_REF_EXPR/[@spelling='R_BCNodeStackTop']") != None:
		if node.find(".//INTEGER_LITERAL") == None:
			pdb.set_trace()
		pos=node.find(".//INTEGER_LITERAL").attrib["spelling"]
		#pdb.set_trace()
		res=Fragment(source="_stack" + str(int(pos)-1), stackArgNo=int(pos)-1)
		return res
	elif node.find("ARRAY_SUBSCRIPT_EXPR/PAREN_EXPR/CSTYLE_CAST_EXPR/PAREN_EXPR/BINARY_OPERATOR/PAREN_EXPR/CSTYLE_CAST_EXPR/UNEXPOSED_EXPR/PAREN_EXPR/DECL_REF_EXPR[@spelling='constants']") != None:
		res=Fragment(source="constant", usesConstant=True)
		return res
	#else: pdb.set_trace()
	return {
        clang.cindex.CursorKind.CALL_EXPR: printCall,
		clang.cindex.CursorKind.BINARY_OPERATOR: printBinaryOperator,
		clang.cindex.CursorKind.COMPOUND_ASSIGNMENT_OPERATOR: printBinaryOperator,
		clang.cindex.CursorKind.DECL_STMT: printDecl,
		clang.cindex.CursorKind.DO_STMT: printDo,
		clang.cindex.CursorKind.UNARY_OPERATOR:	printUnaryOperator,
		clang.cindex.CursorKind.STRING_LITERAL: lambda x : printLiteral(x,""),
		clang.cindex.CursorKind.INTEGER_LITERAL: lambda x : printLiteral(x,""),
		clang.cindex.CursorKind.PAREN_EXPR: lambda x : "(" + printSource(list(node)[0]) + ")",
		clang.cindex.CursorKind.TYPE_REF: lambda x : "",
		clang.cindex.CursorKind.IF_STMT: printIf,
		clang.cindex.CursorKind.CSTYLE_CAST_EXPR: lambda x : "((" + x.attrib["type"] + ")" + printSource(list(node)[-1]) + ")",
		clang.cindex.CursorKind.MEMBER_REF_EXPR : lambda x : printMember(x),
		clang.cindex.CursorKind.CONDITIONAL_OPERATOR : lambda x : printSource(list(node)[0]) + " ? " + printSource(list(node)[1]) +
			" : " + printSource(list(node)[2]),
		clang.cindex.CursorKind.ARRAY_SUBSCRIPT_EXPR: lambda x : printSource(list(node)[0]) + "[" + printSource(list(node)[1]) + "]",
		clang.cindex.CursorKind.COMPOUND_STMT: lambda x : printChildren(x,";\n",True),
		clang.cindex.CursorKind.UNEXPOSED_EXPR: lambda x : printUnexposed(x)
	}.get(node.attrib["kind"], printPlain)(node)

def findSwitch(node,level):
	options = {
        0: findSwitchZero,
        1: findSwitchOne,
		2: findSwitchTwo
        }

	return options[level](node)

def loopElements(node,level):
	source=Fragment()
	gen = node.get_children()
	for c in gen:
		source+=findSwitch(c, level)
		if source.source != "" and level == 0:
			break
	return source


#running cpp on source file
#pdb.set_trace()
inFile=tempfile.mkstemp()[1]
#os.system('cpp -I/usr/share/R/include -I\"' + sys.argv[2] + '\" \"' + sys.argv[1] + '\" > \"' + inFile + '\"')
#os.system('cpp -I \"' + sys.argv[1] + '/src/include\" -D HAVE_DECL_SIZE_MAX -include \"' + sys.argv[1] + '/src/include/config.h\" \"' + sys.argv[1] + '/src/main/eval.c\" > \"' + inFile + '.c\"')
os.system('clang -E -D HAVE_DECL_SIZE_MAX -I \"' + sys.argv[1] + '/src/include\" \"' + sys.argv[1] + '/src/main/eval.c\" > \"' + inFile + '.c\"')

index = clang.cindex.Index.create()
#tu = index.parse(inFile, args={"-I/usr/share/R/include", "-I" + sys.argv[2]}, options=1)
tu = index.parse(inFile + ".c", args={"-I/usr/share/R/include", "-I/home/chris/unison/dev/kwai/llvm_source/r-base-3.2.2/src/include"}, options=1)
print 'Translation unit: %s', tu.spelling

res = open('out.c', 'w')

res.write("#include <Rdefines.h>\n#include \"config.h\"\n#include \"Defn.h\"\n")
res.write("#include \"Rinternals.h\"\n")
res.write("#include \"misc.c\"\n")


source=loopElements(tu.cursor, 0)

for funcRef in source.funcRefList:
	#pdb.set_trace()
	funcRef2=source.funcRefList[funcRef]

	if funcRef2.attrib["spelling"] == 'bcStackScalarEx':
		continue

	if funcRef2.attrib["spelling"] == 'GETSTACK_PTR_TAG':
		continue

	if funcRef2.attrib["spelling"] == 'gettext':
		continue

	if funcRef2.attrib["spelling"] == 'bcStackScalarRealEx':
		continue

	tp=funcRef2.attrib["type"].partition(" ")
	#pdb.set_trace()
	res.write(tp[0] + " " + funcRef2.attrib["spelling"] + tp[2][3::] + ";\n")

res.write(source.source)
#res.write("\n}")

res.close()

os.system('clang-format-3.5 out.c > out2.c')
