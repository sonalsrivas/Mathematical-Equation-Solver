from __future__ import division

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

# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'C:\Users\Sonal Srivastava\Desktop\hello1.ui'
#
# Created by: PyQt5 UI code generator 5.11.3
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_EquationSolver(object):
    def setupUi(self, EquationSolver):
        EquationSolver.setObjectName("EquationSolver")
        EquationSolver.resize(671, 505)
        self.centralwidget = QtWidgets.QWidget(EquationSolver)
        self.centralwidget.setObjectName("centralwidget")
        self.pushButtonNext = QtWidgets.QPushButton(self.centralwidget)
        self.pushButtonNext.setGeometry(QtCore.QRect(150, 180, 101, 31))
        self.pushButtonNext.setObjectName("pushButtonNext")
        self.Equation = QtWidgets.QLabel(self.centralwidget)
        self.Equation.setGeometry(QtCore.QRect(40, 110, 101, 61))
        font = QtGui.QFont()
        font.setPointSize(12)
        self.Equation.setFont(font)
        self.Equation.setObjectName("Equation")
        self.enterequationbox = QtWidgets.QLineEdit(self.centralwidget)
        self.enterequationbox.setGeometry(QtCore.QRect(150, 120, 391, 41))
        self.enterequationbox.setObjectName("enterequationbox")
        self.Heading = QtWidgets.QLabel(self.centralwidget)
        self.Heading.setGeometry(QtCore.QRect(170, 20, 341, 41))
        font = QtGui.QFont()
        font.setFamily("Times New Roman")
        font.setPointSize(18)
        font.setBold(True)
        font.setItalic(True)
        font.setWeight(75)
        self.Heading.setFont(font)
        self.Heading.setFrameShape(QtWidgets.QFrame.NoFrame)
        self.Heading.setLineWidth(0)
        self.Heading.setMidLineWidth(0)
        self.Heading.setTextFormat(QtCore.Qt.RichText)
        self.Heading.setAlignment(QtCore.Qt.AlignCenter)
        self.Heading.setObjectName("Heading")
        self.horizontal_line = QtWidgets.QFrame(self.centralwidget)
        self.horizontal_line.setGeometry(QtCore.QRect(190, 60, 301, 16))
        self.horizontal_line.setFrameShape(QtWidgets.QFrame.HLine)
        self.horizontal_line.setFrameShadow(QtWidgets.QFrame.Sunken)
        self.horizontal_line.setObjectName("horizontal_line")
        self.simplifiedequation = QtWidgets.QLineEdit(self.centralwidget)
        self.simplifiedequation.setGeometry(QtCore.QRect(150, 240, 391, 41))
        self.simplifiedequation.setObjectName("simplifiedequation")
        self.label = QtWidgets.QLabel(self.centralwidget)
        self.label.setGeometry(QtCore.QRect(20, 250, 101, 21))
        self.label.setObjectName("label")
        EquationSolver.setCentralWidget(self.centralwidget)
        self.menubar = QtWidgets.QMenuBar(EquationSolver)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 671, 21))
        self.menubar.setObjectName("menubar")
        self.menuMy_Equations = QtWidgets.QMenu(self.menubar)
        self.menuMy_Equations.setObjectName("menuMy_Equations")
        EquationSolver.setMenuBar(self.menubar)
        self.statusbar = QtWidgets.QStatusBar(EquationSolver)
        self.statusbar.setObjectName("statusbar")
        EquationSolver.setStatusBar(self.statusbar)
        self.menubar.addAction(self.menuMy_Equations.menuAction())

        self.retranslateUi(EquationSolver)
        QtCore.QMetaObject.connectSlotsByName(EquationSolver)

        self.pushButtonNext.clicked.connect(self.simplii)

    def retranslateUi(self, EquationSolver):
        _translate = QtCore.QCoreApplication.translate
        EquationSolver.setWindowTitle(_translate("EquationSolver", "EquationSolver"))
        self.pushButtonNext.setText(_translate("EquationSolver", "Simplify"))
        self.Equation.setText(_translate("EquationSolver", "Equation : "))
        self.Heading.setText(_translate("EquationSolver", "Dad\'s Equation Solver"))
        self.label.setText(_translate("EquationSolver", "Simplified Equation : "))
        self.menuMy_Equations.setTitle(_translate("EquationSolver", "My Equations"))

    def simplii(self):
        print(self.enterequationbox.text())
        parser = Parser()
        #print(self.enterequationbox.text()),self.enterequationbox.value,self.enterequationbox.Value)
        self.s=parser.parse(self.enterequationbox.text()).simplify({}).toString()
        print(self.s)
        self.simplifiedequation.setText(self.s)
        

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    EquationSolver = QtWidgets.QMainWindow()
    ui = Ui_EquationSolver()
    ui.setupUi(EquationSolver)
    EquationSolver.show()
    sys.exit(app.exec_())

