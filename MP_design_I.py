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
        self.WordProblem_textEdit = QtWidgets.QTextEdit(self.centralwidget)
        self.WordProblem_textEdit.setGeometry(QtCore.QRect(80, 140, 651, 111))
        self.WordProblem_textEdit.setObjectName("WordProblem_textEdit")
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


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    MainWindow = QtWidgets.QMainWindow()
    ui = Ui_MainWindow()
    ui.setupUi(MainWindow)
    MainWindow.show()
    sys.exit(app.exec_())