from __future__ import division
from nltk import word_tokenize as Nwt
#from nltk import WordNetLemmatizer.lemmatize as Wnt


import math
import random
import re

TNUMBER = 0
TOP1 = 1
TOP2 = 2
TVAR = 3
TFUNCALL = 4


class Token():

    def __init__(self, type_, index_, prio_, number_):
        self.type_ = type_
        self.index_ = index_ or 0
        self.prio_ = prio_ or 0
        self.number_ = number_ if number_ != None else 0

    def toString(self):
        if self.type_ == TNUMBER:
            return self.number_
        if self.type_ == TOP1 or self.type_ == TOP2 or self.type_ == TVAR:
            return self.index_
        elif self.type_ == TFUNCALL:
            return 'CALL'
        else:
            return 'Invalid Token'


class Expression():

    def __init__(self, tokens, ops1, ops2, functions):
        self.tokens = tokens
        self.ops1 = ops1
        self.ops2 = ops2
        self.functions = functions

    def simplify(self, values):
        values = values or {}
        nstack = []
        newexpression = []
        L = len(self.tokens)
        for i in range(0, L):
            item = self.tokens[i]
            type_ = item.type_
            if type_ == TNUMBER:
                nstack.append(item)
            elif type_ == TVAR and item.index_ in values:
                item = Token(TNUMBER, 0, 0, values[item.index_])
                nstack.append(item)
            elif type_ == TOP2 and len(nstack) > 1:
                n2 = nstack.pop()
                n1 = nstack.pop()
                f = self.ops2[item.index_]
                item = Token(TNUMBER, 0, 0, f(n1.number_, n2.number_))
                nstack.append(item)
            elif type_ == TOP1 and nstack:
                n1 = nstack.pop()
                f = self.ops1[item.index_]
                item = Token(TNUMBER, 0, 0, f(n1.number_))
                nstack.append(item)
            else:
                while len(nstack) > 0:
                    newexpression.append(nstack.pop(0))
                newexpression.append(item)
        while nstack:
            newexpression.append(nstack.pop(0))

        return Expression(newexpression, self.ops1, self.ops2, self.functions)
    
    def toString(self, toJS=False):
        nstack = []
        L = len(self.tokens)
        for i in range(0, L):
            item = self.tokens[i]
            type_ = item.type_
            if type_ == TNUMBER:
                if type(item.number_) == str:
                    nstack.append("'"+item.number_+"'")
                else:
                    nstack.append( item.number_)
            elif type_ == TOP2:
                n2 = nstack.pop()
                n1 = nstack.pop()
                f = item.index_
                if toJS and f == '^':
                    nstack.append('math.pow(' + n1 + ',' + n2 + ')')
                else:
                    frm='({n1}{f}{n2})'
                    if f == ',':
                        frm = '{n1}{f}{n2}'

                    nstack.append(frm.format(
                        n1=n1,
                        n2=n2,
                        f=f,
                    ))


            elif type_ == TVAR:
                nstack.append(item.index_)
            elif type_ == TOP1:
                n1 = nstack.pop()
                f = item.index_
                if f == '-':
                    nstack.append('(' + f + str(n1) + ')')
                else:
                    nstack.append(f + '(' + n1 + ')')
            elif type_ == TFUNCALL:
                n1 = nstack.pop()
                f = nstack.pop()
                nstack.append(f + '(' + n1 + ')')
            else:
                raise Exception('invalid Expression')
        if len(nstack) > 1:
            raise Exception('invalid Expression (parity)')
        return nstack[0]

class Parser:

    class Expression(Expression):
        pass

    PRIMARY      = 1
    OPERATOR     = 2
    FUNCTION     = 4
    LPAREN       = 8
    RPAREN       = 16
    COMMA        = 32
    SIGN         = 64
    CALL         = 128
    NULLARY_CALL = 256

    def add(self, a, b):
        return a + b

    def sub(self, a, b):
        return a - b

    def mul(self, a, b):
        return a * b

    def div(self, a, b):
        return a / b

    def mod(self, a, b):
        return a % b

    def neg(self, a):
        return -a


    def __init__(self):
        self.success = False
        self.errormsg = ''
        self.expression = ''

        self.pos = 0

        self.tokennumber = 0
        self.tokenprio = 0
        self.tokenindex = 0
        self.tmpprio = 0

        self.ops1 = {
            'sin': math.sin,
            'cos': math.cos,
            'tan': math.tan,
            'asin': math.asin,
            'acos': math.acos,
            'atan': math.atan,
            '-': self.neg,
            'exp': math.exp,
        }

        self.ops2 = {
            '+': self.add,
            '-': self.sub,
            '*': self.mul,
            '/': self.div,
            '%': self.mod,
            '^': math.pow,
            '**': math.pow,
        }

        self.functions = {
            'log': math.log,
            'min': min,
            'max': max,
            'pow': math.pow,
            'atan2': math.atan2,

        }

        self.consts = {
            'E': math.e,
            'PI': math.pi,
        }

        self.values = {
            'sin': math.sin,
            'cos': math.cos,
            'tan': math.tan,
            'asin': math.asin,
            'acos': math.acos,
            'atan': math.atan,
            'log': math.log,
            'exp': math.exp,
            'min': min,
            'max': max,
            'pow': math.pow,
            'atan2': math.atan2,
            'E': math.e,
            'PI': math.pi
        }

    def parse(self, expr):
        self.errormsg = ''
        self.success = True
        operstack = []
        tokenstack = []
        self.tmpprio = 0
        expected = self.PRIMARY | self.LPAREN | self.FUNCTION | self.SIGN
        noperators = 0
        self.expression = expr
        self.pos = 0

        while self.pos < len(self.expression):
            if self.isOperator():
                if self.isSign() and expected & self.SIGN:
                    if self.isNegativeSign():
                        self.tokenprio = 5
                        self.tokenindex = '-'
                        noperators += 1
                        self.addfunc(tokenstack, operstack, TOP1)
                    expected = \
                        self.PRIMARY | self.LPAREN | self.FUNCTION | self.SIGN
                elif self.isComment():
                    pass
                else:
                    if expected and self.OPERATOR == 0:
                        self.error_parsing(self.pos, 'unexpected operator')
                    noperators += 2
                    self.addfunc(tokenstack, operstack, TOP2)
                    expected = \
                        self.PRIMARY | self.LPAREN | self.FUNCTION | self.SIGN
            elif self.isNumber():
                if expected and self.PRIMARY == 0:
                    self.error_parsing(self.pos, 'unexpected number')
                token = Token(TNUMBER, 0, 0, self.tokennumber)
                tokenstack.append(token)
                expected = self.OPERATOR | self.RPAREN | self.COMMA
            elif self.isString():
                if (expected & self.PRIMARY) == 0:
                    self.error_parsing(self.pos, 'unexpected string')
                token = Token(TNUMBER, 0, 0, self.tokennumber)
                tokenstack.append(token)
                expected = self.OPERATOR | self.RPAREN | self.COMMA
            elif self.isLeftParenth():
                if (expected & self.LPAREN) == 0:
                    self.error_parsing(self.pos, 'unexpected \"(\"')
                if expected & self.CALL:
                    noperators += 2
                    self.tokenprio = -2
                    self.tokenindex = -1
                    self.addfunc(tokenstack, operstack, TFUNCALL)
                expected = \
                    self.PRIMARY | self.LPAREN | self.FUNCTION | \
                    self.SIGN | self.NULLARY_CALL
            elif self.isRightParenth():
                if expected & self.NULLARY_CALL:
                    token = Token(TNUMBER, 0, 0, [])
                    tokenstack.append(token)
                elif (expected & self.RPAREN) == 0:
                    self.error_parsing(self.pos, 'unexpected \")\"')
                expected = \
                    self.OPERATOR | self.RPAREN | self.COMMA | \
                    self.LPAREN | self.CALL
            elif self.isComma():
                if (expected & self.COMMA) == 0:
                    self.error_parsing(self.pos, 'unexpected \",\"')
                self.addfunc(tokenstack, operstack, TOP2)
                noperators += 2
                expected = \
                    self.PRIMARY | self.LPAREN | self.FUNCTION | self.SIGN
            elif self.isConst():
                if (expected & self.PRIMARY) == 0:
                    self.error_parsing(self.pos, 'unexpected constant')
                consttoken = Token(TNUMBER, 0, 0, self.tokennumber)
                tokenstack.append(consttoken)
                expected = self.OPERATOR | self.RPAREN | self.COMMA
            elif self.isOp2():
                if (expected & self.FUNCTION) == 0:
                    self.error_parsing(self.pos, 'unexpected function')
                self.addfunc(tokenstack, operstack, TOP2)
                noperators += 2
                expected = self.LPAREN
            elif self.isOp1():
                if (expected & self.FUNCTION) == 0:
                    self.error_parsing(self.pos, 'unexpected function')
                self.addfunc(tokenstack, operstack, TOP1)
                noperators += 1
                expected = self.LPAREN
            elif self.isVar():
                if (expected & self.PRIMARY) == 0:
                    self.error_parsing(self.pos, 'unexpected variable')
                vartoken = Token(TVAR, self.tokenindex, 0, 0)
                tokenstack.append(vartoken)
                expected = \
                    self.OPERATOR | self.RPAREN | \
                    self.COMMA | self.LPAREN | self.CALL
            elif self.isWhite():
                pass
            else:
                if self.errormsg == '':
                    self.error_parsing(self.pos, 'unknown character')
                else:
                    self.error_parsing(self.pos, self.errormsg)
        if self.tmpprio < 0 or self.tmpprio >= 10:
            self.error_parsing(self.pos, 'unmatched \"()\"')
        while len(operstack) > 0:
            tmp = operstack.pop()
            tokenstack.append(tmp)
        if (noperators + 1) != len(tokenstack):
            self.error_parsing(self.pos, 'parity')

        return Expression(tokenstack, self.ops1, self.ops2, self.functions)
    
    def isOperator(self):
        ops = (
            ('+', 2, '+'),
            ('-', 2, '-'),
            ('**', 6, '**'),
            ('*', 3, '*'),
            (u'\u2219', 3, '*'), # bullet operator
            (u'\u2022', 3, '*'), # black small circle
            ('/', 4, '/'),
            ('%', 4, '%'),
            ('^', 6, '^'),
            ('||', 1, '||'),
            ('==', 1, '=='),
            ('!=', 1, '!='),
            ('<=', 1, '<='),
            ('>=', 1, '>='),
            ('<', 1, '<'),
            ('>', 1, '>'),
            ('and ', 0, 'and'),
            ('or ', 0, 'or'),
        )
        for token, priority, index in ops:
            if self.expression.startswith(token, self.pos):
                self.tokenprio = priority
                self.tokenindex = index
                self.pos += len(token)
                return True
        return False
    
    def isNumber(self):
        r = False

        if self.expression[self.pos] == 'E':
            return False

        # number in scientific notation
        pattern = r'([-+]?([0-9]*\.?[0-9]*)[eE][-+]?[0-9]+).*'
        match = re.match(pattern, self.expression[self.pos: ])
        if match:
            self.pos += len(match.group(1))
            self.tokennumber = float(match.group(1))
            return True

        # number in decimal
        str = ''
        while self.pos < len(self.expression):
            code = self.expression[self.pos]
            if (code >= '0' and code <= '9') or code == '.':
                if (len(str) == 0 and code == '.' ):
                    str = '0'
                str += code
                self.pos += 1
                try:
                    self.tokennumber = int(str)
                except ValueError:
                    self.tokennumber = float(str)
                r = True
            else:
                break
        return r
    def isString(self):
        r = False
        str = ''
        startpos = self.pos
        if self.pos < len(self.expression) and self.expression[self.pos] == "'":
            self.pos += 1
            while self.pos < len(self.expression):
                code = self.expression[self.pos]
                if code != '\'' or (str != '' and str[-1] == '\\'):
                    str += self.expression[self.pos]
                    self.pos += 1
                else:
                    self.pos += 1
                    self.tokennumber = self.unescape(str, startpos)
                    r = True
                    break
        return r
    def isLeftParenth(self):
        code = self.expression[self.pos]
        if code == '(':
            self.pos += 1
            self.tmpprio += 10
            return True
        return False

    def isRightParenth(self):
        code = self.expression[self.pos]
        if code == ')':
            self.pos += 1
            self.tmpprio -= 10
            return True
        return False
    def isComma(self):
        code = self.expression[self.pos]
        if code==',':
            self.pos+=1
            self.tokenprio=-1
            self.tokenindex=","
            return True
        return False

    def isWhite(self):
        code = self.expression[self.pos]
        if code.isspace():
            self.pos += 1
            return True
        return False

    def isOp1(self):
        str = ''
        for i in range(self.pos, len(self.expression)):
            c = self.expression[i]
            if c.upper() == c.lower():
                if i == self.pos or (c != '_' and (c < '0' or c > '9')):
                    break
            str += c
        if len(str) > 0 and str in self.ops1:
            self.tokenindex = str
            self.tokenprio = 7
            self.pos += len(str)
            return True
        return False

    def isOp2(self):
        str = ''
        for i in range(self.pos, len(self.expression)):
            c = self.expression[i]
            if c.upper() == c.lower():
                if i == self.pos or (c != '_' and (c < '0' or c > '9')):
                    break
            str += c
        if len(str) > 0 and (str in self.ops2):
            self.tokenindex = str
            self.tokenprio = 7
            self.pos += len(str)
            return True
        return False

    def isVar(self):
        str = ''
        inQuotes = False
        for i in range(self.pos, len(self.expression)):
            c = self.expression[i]
            if c.lower() == c.upper():
                if ((i == self.pos and c != '"') or (not (c in '_."') and (c < '0' or c > '9'))) and not inQuotes :
                    break
            if c == '"':
                inQuotes = not inQuotes
            str += c
        if str:
            self.tokenindex = str
            self.tokenprio = 4
            self.pos += len(str)
            return True
        return False
    def isConst(self):
        for i in self.consts:
            L = len(i)
            str = self.expression[self.pos:self.pos+L]
            if i == str:
                if len(self.expression) <= self.pos + L:
                    self.tokennumber = self.consts[i]
                    self.pos += L
                    return True
                if not self.expression[self.pos + L].isalnum() and self.expression[self.pos + L] != "_":
                    self.tokennumber = self.consts[i]
                    self.pos += L
                    return True
        return False
    def isSign(self):
        code = self.expression[self.pos - 1]
        return (code == '+') or (code == '-')

    def isNegativeSign(self):
        code = self.expression[self.pos - 1]
        return code == '-'
    def isComment(self):
        code = self.expression[self.pos - 1]
        if code == '/' and self.expression[self.pos] == '*':
            self.pos = self.expression.index('*/', self.pos) + 2
            if self.pos == 1:
                self.pos = len(self.expression)
            return True
        return False
    def addfunc(self, tokenstack, operstack, type_):
        operator = Token(
            type_,
            self.tokenindex,
            self.tokenprio + self.tmpprio,
            0,
        )
        while len(operstack) > 0:
            if operator.prio_ <= operstack[len(operstack) - 1].prio_:
                tokenstack.append(operstack.pop())
            else:
                break
        operstack.append(operator)




''' cleans problem and splits in words '''
def listproblem(string):
  #s=remove_punctuations(string)
  problem=Nwt(string)
  #print("problem",problem)
  for p in range(len(problem)):
    print("problem[p]=",problem[p])
    if len(problem[p])>1 and problem[p][-1]=='s' and problem[p][-2] not in 'aoui/':
      problem[p]=problem[p][:-1]
    #print("PROBLEM IN listproblem::",problem)
  #print("problem",problem)
  return problem

''' TransformWords insures that: numeral data is separated from alphabetic data, like -24m/s is spaced; '''
def TransformWords(s):
  l=len(s); i=0
  while i<l:
    if s[i].isdigit() and s[i+1].isalpha():
      s=s[:i+1]+' '+s[i+1:]
    i+=1
  return s

''' convert string to integer numeral or decimal numeral '''
def convert_numeral(word):
  #print(word)
  if '.' in word:
    return float(word)
  else:
    return int(word)


'''produces a list containing numerical data and its given unit in the problem. The unit is determined simply by the word next to the numerical value.'''
def numeralunitList():
  now=len(problem)
  numerical_data=[]                   #Of the form (id,numeral,unit,variable(optional))
  i=0
  while i<now:
    word=problem[i]
    i+=1
    if word[0].isdigit():             # ! what when it is a negative value
      assocUnit=problem[i]
      if assocUnit[-1]=='.':
        assocUnit=assocUnit[:-1]
        problem[i]=problem[i][:-1]
        problem.insert(i+1,'.')
        otter.insert(i+1,0)
        now+=1
      assocUnit=map_unit_UNIT[assocUnit]
      numerical_data.append([i-1,convert_numeral(word),assocUnit])
      otter[i]=2; otter[i-1]=1
      i+=1
  print("numeralunitList returns :",numerical_data)
  return numerical_data

def wordTOnum():
  print("prb",problem)
  s=problem
  rest={0:'zero',1:'one',2:'two',3:'three',4:'four',5:'five',6:'six',7:'seven',8:'eight',9:'nine'}
  tens={20:'twenty',30:'thirty',40:'forty',50:'fifty',60:'sixty',70:'seventy',80:'eighty',90:'ninety'}
  elevtens={10:'ten',11:'eleven',12:'twelve',13:'thirteen',14:'fourteen', 15:'fifteen',16:'sixteen',17:'seventeen', 18:'eighteen',19:'nineteen'}
  
  number={**{rest[i]:i for i in rest},**{tens[i]:i for i in tens},**{elevtens[i]:i for i in elevtens}}
  denomination={'hundred':100,'thousand':1000,'million':1000000,'lakh':100000}

  now=len(s); w=0
  while w<now:
    last=1; num=0
    flag=-1; first=w
    #print("w,s[w]")
    #print(w,s[w])
    while s[w] in denomination or s[w] in number:
      #print("num,w,s[w]")
      #print(num,w,s[w])
      if s[w] in denomination:
        #print("num,s[w],last*denomination[s[w]]",num,s[w],last*denomination[s[w]])
        num+=last*denomination[s[w]]; flag=1
      else:
        #print("last,number[s[w]]",last,number[s[w]])
        if last in tens:
          last+=number[s[w]]
        else:
          last=number[s[w]]
        flag=0
      #print(num)
      w+=1; now-=1
    if flag==0:
      num+=last
    if flag!=-1:
      #print("last,first,w,s[first:w],str(num)")
      #print(last,first,w,s[first:w],str(num))
      s[first:w]=[str(num)]
      now+=1
    w+=1
  print("s",s)
  return s
    


def var_associations(numerical_data):
  #check the words following and previous to numeral is a keyword until 'and' or ',' or '.' appears
  ## numerical_data has structure of list with elements like (index, numeral, unit)
  ### we want another structure or will later modify numerical_data , that has (variable, numeral, unit) and maybe index as additional information
  #==> VarNumUnit=[]
  VarNumUnit=[]
  for j in numerical_data:
    l=len(problem)
    VarNumUnit.append([map_UNIT_VAR[j[2]],j[1],j[2]])
  return VarNumUnit


'''
Find what is the unknown variable to be found.
'''
def FindUnknown(now):
  question={'find':0,'determine':0,'what':0,'how':0,'calculate':0,
            'when':0,'where':0,'convert':0,'where':0,'change':0}

  #The first keyword found following question word

  #keyword is something in the map_unit_UNIT dictionary for now

  unknownVar=0; unknownUnit=0
  for i in range(now):        #Here we are asserting that we have only one value to find.
    word=problem[i]; rv=0; ru=0
    if word in question:
      j=i+1
      while j<now:
        req=problem[j]
        if rv==0 and req in map_var_VAR and otter[j]==0:
          unknownVar=map_var_VAR[req]
          otter[j]=4
          rv=1
        if ru==0 and req in map_unit_UNIT and otter[j]==0:          #to check that the unit is lone and not preceded by a numerical
          unknownUnit=map_unit_UNIT[req]
          otter[j]=5
          ru=1
        #if rv==1 and ru==1:
        #  break
        j+=1
      break
  if unknownVar==0 and 'how' in problem:
    h=problem.index('how')
    if problem[h+1]=='long':
      unknownVar='time'
  return unknownVar,unknownUnit


'''
otter processing

#word                               #corresponding value in otter
unidentified                            0
known numerical                         1
known numericals unit                   2
known numericals mentioned variable     3
unknown variable(to be found)           4
unknown variables unit(optional)        5
'''


operators='+-*/^'
def findmy_close(j,dirt,s):  #j has the index just left of a closing parenthesis and it is for sure a number
    if dirt=='left':
        ff=j
        numends=j
        numbegins=0
        while s[ff-1] not in operators:
            ff-=1
        numbegins=ff
        if '.' in s[numbegins:numends+1]:
            num=float(s[numbegins:numends+1])
        else:
            num=int(s[numbegins:numends+1])
        op=s[numbegins-1]
        i=numbegins-2
        
    elif dirt=='right':
        ff=j # that is actually i
        numends=0
        numbegins=j # that is actually i
        while s[ff+1] not in operators:
            ff+=1
        numends=ff
        if '.' in s[numbegins:numends+1]:
            num=float(s[numbegins:numends+1])
        else:
            num=int(s[numbegins:numends+1])
        op=s[numends+1]
        i=numends+2 # that is actually j
    print("Returning i,op,num",i,op,num)
    return i,op,num

def lolzeeee(unknown, var, s):       #s is simplified_equation      var is vardict
    #s=parser.parse(equ).simplify(var).toString()
    #print("Enter an expression with one unknown variable:")
    #s=input()
    i=0
    #print("Printing s",s)
    j=len(s)-1
    ans=0
    #unknown=''     ##Please redefine later on
    while i<j:
        print("Printing for i and j as ",i,j)
        print("where s[i] and s[j] are : ",s[i],s[j])
        if s[i]=='(' and s[j]==')':
            print("Condition: ( )")
            i+=1
            j-=1
        elif s[i]=='(' and s[j]!=')':
            print("Condition: (  !)")
            j,op,num=findmy_close(j,'left',s)
            if op=='+':
                ans-=num
            elif op=='-':
                ans+=num
            elif op=='*':
                ans/=num
            elif op=='/':
                ans*=num
            elif op=='^':
                ans=math.pow(ans,1/num)
            ##    ans+=num
            print("Current value of ans=",ans)                        #(unknown, var, s)
        elif s[i]!='(' and s[j]==')':
            print("Condition: !(  )")
            i,op,num=findmy_close(i,'right',s)
            
            if op=='+':
                ans-=num
            elif op=='-':
                ans=num-ans
            elif op=='*':
                ans/=num
            elif op=='/':
                ans=num/ans
            print("Current value of ans=",ans)
        else:
            print("Condition: !(  !)")
            if s[j].isalpha():
                unknown=s[j]
                i,op,num=findmy_close(i,'right',s)
                
                if op=='+':
                    ans-=num
                elif op=='-':
                    ans=num-ans
                elif op=='*':
                    ans/=num
                elif op=='/':
                    ans=num/ans
                elif op=='^':
                    ans=math.log(ans,num)
                print("Current value of ans=",ans)
            elif s[i].isalpha():
                unknown=s[i]
                j,op,num=findmy_close(j,'left',s)
                if op=='+':
                    ans-=num
                elif op=='-':
                    ans+=num
                elif op=='*':
                    ans/=num
                elif op=='/':
                    ans*=num
                elif op=='^':
                    ans=math.pow(ans,1/num)
                print("Current value of ans=",ans)
    print("Value of unknown",unknown,"is :",ans)
    return ans

def simplii(equation,Var_Num_Unit,unknownVar):              #equation -> string   Var_Num_Unit -> [['keyword',number,'unit'],...]   unknownVar -> string
  parser=Parser()
  use=parser.parse(equation)
  vardict={}
  commonIndexofDiff=0
  if len(Var_Num_Unit)==1:          # we assume that this problem shall arise only when we deal with curunit to requnit conversion
    setOfuniques=set()
    r=unknownVar; c=Var_Num_Unit[0][2]
    for i in range(5):
      if r[i]!=c[i]:
        commonIndexofDiff=i
        break
  for u in Var_Num_Unit:
    #print("u",u)
    #print("u[0][commonIndexofDiff],u[1]",u[0][commonIndexofDiff],u[1],"u[0]=",u[0],"commonIndexofDiff=",commonIndexofDiff,"u[0][commonIndexofDiff]=",u[0][commonIndexofDiff])
    vardict[u[0][commonIndexofDiff]]=u[1]
  print("vardict=>",vardict)
  simplifiedEqui=use.simplify(vardict).toString()
  ans=lolzeeee(unknownVar,vardict,simplifiedEqui)
  return ans

def CommonUNIT(VNU):
  nvnu=[]
  for vnu in VNU:
    Unit_Num_Unit=[[vnu[2],vnu[1],vnu[2]]]
    curUnit = vnu[2]
    reqUnit = map_VAR_UNIT[vnu[0]][0]
    if reqUnit==curUnit:
      nvnu.append(vnu)
      continue
    else:
      EQ=FindMeUCEquation(curUnit,reqUnit)
      d=simplii(EQ,Unit_Num_Unit,reqUnit)
      nvnu.append([vnu[0],d,reqUnit])
  return nvnu
def FindMeEquation(Var_Num_Unit ,unknownVar, unknownUnit):
  keysVar=[unknownVar]
  for g in Var_Num_Unit:  
    keysVar.append(g[0])
  keysVar.sort()

  return map_key_equi[tuple(keysVar)]

def FindMeUCEquation(curUnit,reqUnit):   #Var_Num_Unit ,unknownVar, unknownUnit):
  print(curUnit,reqUnit)
  keysUnit=tuple(sorted([curUnit,reqUnit]))
  print("keysUnit=>",keysUnit)
  
  return map_unit_equi[keysUnit]


'''-------------------------------------------------------------ALL THE MAPPING ARE BELOW-------------------------------------------------------------'''

map_unit_UNIT=dict()
map_UNIT_unit={'kmph':['kmph','km/h','km/hr'],'mps':['mps','m/s','m/sec'],'C':['celsius','centigrade','c','degree'],'F':['f','fahrenheit'],
               'hour':['hour','hr'],'minute':['min','minute'],'second':['second','sec','s'],'day':['day'],'year':['year','yr'],
               'meter':['m','meter','metre'],'kilometer':['kilometer','km'],'mps2':['mps2','m/s2']}
for vt in map_UNIT_unit:                                                         #this will save a little time
  for ut in map_UNIT_unit[vt]:
    map_unit_UNIT[ut]=vt

#map_unit_UNIT={'kmph':'kmph','km/h':'kmph','km/hr':'kmph',
#              'mps':'mps','m/s':'mps','m/min':'mpm',
#              'centigrade':'C','celsius':'C','degrees':'C','fahrenheit':'F'}


map_VAR_UNIT={'speed':['mps','kmph','miph','mph'],'temperature':['C','F'], 'time':['second','hour','minute','day','year'],
              'distance':['meter','kilometer','mile'],'acceleration':['mps2'],'deceleration':['mps2'] }

map_UNIT_VAR=dict() #{'kmph':'speed','mps':'speed','C':'temperature','F':'temperature'}
for vt in map_VAR_UNIT:                                                         #this will save a little time
  for ut in map_VAR_UNIT[vt]:
    map_UNIT_VAR[ut]=vt

map_VAR_var={'speed':{'velocity','speed'},'distance':{'distance','displacement','dist.'},'temparature':{'temparature'},'time':{'time'},
             'acceleration':{'acceleration'},'deceleration':{'deceleration','retardation'}}
map_var_VAR=dict()
for vt in map_VAR_var:                                                         #this will save a little time
  for ut in map_VAR_var[vt]:
    map_var_VAR[ut]=vt
#print(map_var_VAR)

map_key_equi={('distance','speed','time'):'s-d/t',('frequency','timeperiod'):'f-1/t',('acceleration','speed','time'):'a-s/t',
              ('deceleration','speed','time'):'d-s/t'}
map_unit_equi={('kilometer','meter'):'1000*k-m',('meter','mile'):'1609.344*i-e',('kmph','mps'):'3.6*m-k',('miph','mps'):'0.44704*i-p',('mph','mps'):'3600*h-s',
                 ('hour','second'):'3600*h-s',('minute','second'):'60*m-s',('day','second'):'24*3600*d-s',('year','second'):'365*24*3600*y-s'}

'''-------------------------------------------------------------ALL THE MAPPING ARE ABOVE-------------------------------------------------------------'''
problem=[]; otter=[]
# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'MWP_design_I.ui'
#
# Created by: PyQt5 UI code generator 5.13.0
#
# WARNING! All changes made in this file will be lost!


from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName("MainWindow")
        MainWindow.resize(800, 600)
        self.centralwidget = QtWidgets.QWidget(MainWindow)
        self.centralwidget.setObjectName("centralwidget")
        self.TITLE_label = QtWidgets.QLabel(self.centralwidget)
        self.TITLE_label.setGeometry(QtCore.QRect(190, 30, 441, 51))
        font = QtGui.QFont()
        font.setFamily("Algerian")
        font.setPointSize(20)
        font.setUnderline(True)
        self.TITLE_label.setFont(font)
        self.TITLE_label.setObjectName("TITLE_label")
        self.enterproblem_label = QtWidgets.QLabel(self.centralwidget)
        self.enterproblem_label.setGeometry(QtCore.QRect(330, 100, 161, 16))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.enterproblem_label.setFont(font)
        self.enterproblem_label.setObjectName("enterproblem_label")
        self.displayequation_label = QtWidgets.QLabel(self.centralwidget)
        self.displayequation_label.setGeometry(QtCore.QRect(210, 370, 81, 31))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.displayequation_label.setFont(font)
        self.displayequation_label.setObjectName("displayequation_label")
        self.displayanswer_label = QtWidgets.QLabel(self.centralwidget)
        self.displayanswer_label.setGeometry(QtCore.QRect(210, 440, 81, 31))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.displayanswer_label.setFont(font)
        self.displayanswer_label.setObjectName("displayanswer_label")
        self.displayunit_label = QtWidgets.QLabel(self.centralwidget)
        self.displayunit_label.setGeometry(QtCore.QRect(210, 510, 81, 31))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.displayunit_label.setFont(font)
        self.displayunit_label.setObjectName("displayunit_label")
        self.colon1_label = QtWidgets.QLabel(self.centralwidget)
        self.colon1_label.setGeometry(QtCore.QRect(330, 370, 21, 31))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.colon1_label.setFont(font)
        self.colon1_label.setObjectName("colon1_label")
        self.colon2_label = QtWidgets.QLabel(self.centralwidget)
        self.colon2_label.setGeometry(QtCore.QRect(330, 440, 20, 31))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.colon2_label.setFont(font)
        self.colon2_label.setObjectName("colon2_label")
        self.colon3_label = QtWidgets.QLabel(self.centralwidget)
        self.colon3_label.setGeometry(QtCore.QRect(330, 510, 21, 31))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.colon3_label.setFont(font)
        self.colon3_label.setObjectName("colon3_label")
        self.equation_lineEdit = QtWidgets.QLineEdit(self.centralwidget)
        self.equation_lineEdit.setGeometry(QtCore.QRect(410, 360, 191, 41))
        font = QtGui.QFont()
        font.setPointSize(12)

        self.WordProblem_textEdit = QtWidgets.QTextEdit(self.centralwidget)
        self.WordProblem_textEdit.setGeometry(QtCore.QRect(80, 140, 651, 111))
        self.WordProblem_textEdit.setObjectName("WordProblem_textEdit")

        self.equation_lineEdit.setFont(font)
        self.equation_lineEdit.setObjectName("equation_lineEdit")
        self.answer_lineEdit_2 = QtWidgets.QLineEdit(self.centralwidget)
        self.answer_lineEdit_2.setGeometry(QtCore.QRect(410, 440, 191, 41))
        font = QtGui.QFont()
        font.setPointSize(12)
        self.answer_lineEdit_2.setFont(font)
        self.answer_lineEdit_2.setObjectName("answer_lineEdit_2")
        self.unit_lineEdit_3 = QtWidgets.QLineEdit(self.centralwidget)
        self.unit_lineEdit_3.setGeometry(QtCore.QRect(410, 510, 191, 41))
        font = QtGui.QFont()
        font.setPointSize(12)
        self.unit_lineEdit_3.setFont(font)
        self.unit_lineEdit_3.setObjectName("unit_lineEdit_3")
        self.horizontalsepline = QtWidgets.QFrame(self.centralwidget)
        self.horizontalsepline.setGeometry(QtCore.QRect(20, 330, 771, 21))
        self.horizontalsepline.setFrameShape(QtWidgets.QFrame.HLine)
        self.horizontalsepline.setFrameShadow(QtWidgets.QFrame.Sunken)
        self.horizontalsepline.setObjectName("horizontalsepline")
        
        self.SOLVE_pushButton = QtWidgets.QPushButton(self.centralwidget)
        self.SOLVE_pushButton.setGeometry(QtCore.QRect(370, 290, 75, 23))
        font = QtGui.QFont()
        font.setPointSize(9)
        font.setBold(True)
        font.setWeight(75)
        self.SOLVE_pushButton.setFont(font)
        self.SOLVE_pushButton.setObjectName("SOLVE_pushButton")
        MainWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtWidgets.QMenuBar(MainWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 21))
        self.menubar.setObjectName("menubar")
        MainWindow.setMenuBar(self.menubar)
        self.statusbar = QtWidgets.QStatusBar(MainWindow)
        self.statusbar.setObjectName("statusbar")
        MainWindow.setStatusBar(self.statusbar)

        self.retranslateUi(MainWindow)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)
        self.SOLVE_pushButton.clicked.connect(self.integrate)

    def retranslateUi(self, MainWindow):
        _translate = QtCore.QCoreApplication.translate
        MainWindow.setWindowTitle(_translate("MainWindow", "MainWindow"))
        self.TITLE_label.setText(_translate("MainWindow", "Mathematical Problem Solver"))
        self.enterproblem_label.setText(_translate("MainWindow", "Enter Word Problem :"))
        self.displayequation_label.setText(_translate("MainWindow", "Equation "))
        self.displayanswer_label.setText(_translate("MainWindow", "Answer "))
        self.displayunit_label.setText(_translate("MainWindow", "Unit "))
        self.colon1_label.setText(_translate("MainWindow", ":"))
        self.colon2_label.setText(_translate("MainWindow", ":"))
        self.colon3_label.setText(_translate("MainWindow", ":"))
        self.SOLVE_pushButton.setText(_translate("MainWindow", "SOLVE"))
    def integrate(self):
        #string=str(self.WordProblem_textEdit)
        string=self.WordProblem_textEdit.toPlainText()
        #print("string",string)
        string=TransformWords(string.lower())
        #print(string)
        global problem
        problem=listproblem(string)
        problem=wordTOnum()
        #print("problem after wordtonum:",problem)
        global otter
        otter=[0]*len(problem)
        numerical_data=numeralunitList()
        print("numerical_data",numerical_data,"problem",problem)
        Var_Num_Unit=var_associations(numerical_data)
        print("\nKeyword, Numeral, Unit : ",Var_Num_Unit)

        #Function to convert given numerical inputs into calculable types
        neutralizedVNU=CommonUNIT(Var_Num_Unit)                                       
        print("\nNeutralized Keyword, Numeral, Unit : ",neutralizedVNU)
        unknownVar,unknownUnit=FindUnknown(len(problem))
        if unknownVar==0 and unknownUnit==0:
          print("Lol! Can't tell what you want from me lady.")
        elif unknownVar==0:
          unknownVar=map_UNIT_VAR[unknownUnit]
        elif unknownUnit==0:
          unknownUnit=map_VAR_UNIT[unknownVar][0]
        print("\nTo find: ","\u0332".join(str(unknownVar)+' '),"in:","\u0332".join(str(unknownUnit)+' '),"\n")
        #print("\u0332".join("hello "))           Demo of underlining text
        EQUI=FindMeEquation(neutralizedVNU,unknownVar, unknownUnit)
        print("\n The equation found applicable to this problem : ",EQUI)
        self.equation_lineEdit.setText(EQUI+'=0')
        curunit=map_VAR_UNIT[unknownVar][0]
        FinalAnswer=simplii(EQUI,neutralizedVNU,unknownVar)
        print("\nThe final answer is: ",FinalAnswer,curunit)
        print("unknownVar, unknownUnit=>",unknownVar, unknownUnit)

        if curunit!=unknownUnit:
          UCEQUI=FindMeUCEquation(curunit, unknownUnit)
          print("\n Computed answer and req answer unit differed, so : ",UCEQUI)
          uvar_Num_Unit=[[curunit,FinalAnswer,curunit]]
          FinalAnswer_PU=simplii(UCEQUI,uvar_Num_Unit,unknownUnit)
          print("\nThe final answer in preferred unit: ",FinalAnswer_PU,unknownUnit)
          self.answer_lineEdit_2.setText(str(FinalAnswer_PU))
          self.unit_lineEdit_3.setText(unknownUnit)
        else:
          self.answer_lineEdit_2.setText(str(FinalAnswer))
          self.unit_lineEdit_3.setText(unknownUnit)
        ##string=input("\n:Enter your word problem:\n").lower()


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    MainWindow = QtWidgets.QMainWindow()
    ui = Ui_MainWindow()
    ui.setupUi(MainWindow)
    MainWindow.show()
    sys.exit(app.exec_())
'''
string=input(":Enter your word problem:\n").lower()
while string!='':
  string=TransformWords(string)
  print(string)
  problem=listproblem(string)
  problem=wordTOnum()
  otter=[0]*len(problem)
  numerical_data=numeralunitList()
  Var_Num_Unit=var_associations(numerical_data)
  print("\nKeyword, Numeral, Unit : ",Var_Num_Unit)

  #Function to convert given numerical inputs into calculable types
  neutralizedVNU=CommonUNIT(Var_Num_Unit)                                       
  print("\nNeutralized Keyword, Numeral, Unit : ",neutralizedVNU)
  unknownVar,unknownUnit=FindUnknown(len(problem))
  if unknownVar==0 and unknownUnit==0:
    print("Lol! Can't tell what you want from me lady.")
  elif unknownVar==0:
    unknownVar=map_UNIT_VAR[unknownUnit]
  elif unknownUnit==0:
    unknownUnit=map_VAR_UNIT[unknownVar][0]
  print("\nTo find: ","\u0332".join(str(unknownVar)+' '),"in:","\u0332".join(str(unknownUnit)+' '),"\n")
  #print("\u0332".join("hello "))           Demo of underlining text
  EQUI=FindMeEquation(neutralizedVNU,unknownVar, unknownUnit)
  print("\n The equation found applicable to this problem : ",EQUI)
  curunit=map_VAR_UNIT[unknownVar][0]
  FinalAnswer=simplii(EQUI,neutralizedVNU,unknownVar)
  print("\nThe final answer is: ",FinalAnswer,curunit)
  print("unknownVar, unknownUnit=>",unknownVar, unknownUnit)

  if curunit!=unknownUnit:
    UCEQUI=FindMeUCEquation(curunit, unknownUnit)
    print("\n Computed answer and req answer unit differed, so : ",UCEQUI)
    uvar_Num_Unit=[[curunit,FinalAnswer,curunit]]
    FinalAnswer_PU=simplii(UCEQUI,uvar_Num_Unit,unknownUnit)
    print("\nThe final answer in preferred unit: ",FinalAnswer_PU,unknownUnit)
  string=input("\n:Enter your word problem:\n").lower()
'''
