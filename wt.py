# -*- coding: utf-8 -*-
#! /usr/local/bin/python

############################################
## Worldtime Grid (with start stop timer) ##
############################################


import os,sys,string
from PyQt4 import QtCore, QtGui 
from time import strftime ,sleep,tzname,tzset
from pytz import common_timezones

# date/time format
timefmt='%a,  %d %b %Y ,   Week %W    Day %j     %H:%M:%S  %Z'

# load the timezones from th pytz module
myTZ=[]
for tz in common_timezones:
     myTZ.append(tz)

 
def printgreen(s):
	print '\033[1;42m%s\033[1;m'  % s	 
 
def printred(s):
	print '\033[1;48m%s\033[1;m' % s


try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName(_fromUtf8("Dialog"))
        Dialog.resize(974, 680)
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "World Time", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox = QtGui.QGroupBox(Dialog)
        self.groupBox.setGeometry(QtCore.QRect(0, 0, 981, 641))
        self.groupBox.setStyleSheet(QtGui.QApplication.translate("Dialog", "background-color: rgb(141, 188, 79);", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(_fromUtf8(""))
        self.groupBox.setObjectName(_fromUtf8("groupBox"))
        self.timegrid = QtGui.QTableWidget(self.groupBox)
        self.timegrid.setGeometry(QtCore.QRect(5, 8, 965, 625))
        self.timegrid.setMaximumSize(QtCore.QSize(16777215, 16777215))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Courier New"))
        font.setPointSize(14)
        font.setBold(False)
        font.setItalic(False)
        font.setWeight(50)
        self.timegrid.setFont(font)
        self.timegrid.setStyleSheet(_fromUtf8("background-color: rgb(41, 41, 41);\n"
"font: 12pt \"Courier New\";\n"
"color: rgb(221, 244, 11);"))
        self.timegrid.setObjectName(_fromUtf8("timegrid"))
        self.timegrid.setColumnCount(0)
        self.timegrid.setRowCount(0)
        self.quit_button = QtGui.QPushButton(Dialog)
        self.quit_button.setGeometry(QtCore.QRect(890, 650, 71, 25))
        self.quit_button.setStyleSheet(_fromUtf8("background-color: rgb(255, 85, 0);"))
        self.quit_button.setText(QtGui.QApplication.translate("Dialog", "Quit", None, QtGui.QApplication.UnicodeUTF8))
        self.quit_button.setObjectName(_fromUtf8("quit_button"))
        self.refreshbutton = QtGui.QPushButton(Dialog)
        self.refreshbutton.setGeometry(QtCore.QRect(820, 650, 61, 25))
        self.refreshbutton.setStyleSheet(_fromUtf8("background-color: rgb(85, 170, 0);"))
        self.refreshbutton.setText(QtGui.QApplication.translate("Dialog", "Refresh", None, QtGui.QApplication.UnicodeUTF8))
        self.refreshbutton.setObjectName(_fromUtf8("refreshbutton"))
        self.startButton = QtGui.QPushButton(Dialog)
        self.startButton.setGeometry(QtCore.QRect(110, 650, 51, 21))
        self.startButton.setStyleSheet(_fromUtf8("background-color: rgb(85, 170, 0);"))
        self.startButton.setText(QtGui.QApplication.translate("Dialog", "Start", None, QtGui.QApplication.UnicodeUTF8))
        self.startButton.setObjectName(_fromUtf8("startButton"))
        self.stopButton = QtGui.QPushButton(Dialog)
        self.stopButton.setGeometry(QtCore.QRect(170, 650, 51, 21))
        self.stopButton.setStyleSheet(_fromUtf8("background-color: rgb(255, 85, 0);"))
        self.stopButton.setText(QtGui.QApplication.translate("Dialog", "Stop", None, QtGui.QApplication.UnicodeUTF8))
        self.stopButton.setObjectName(_fromUtf8("stopButton"))
        self.timeEdit = QtGui.QLineEdit(Dialog)
        self.timeEdit.setGeometry(QtCore.QRect(10, 647, 91, 23))
        self.timeEdit.setObjectName(_fromUtf8("timeEdit"))

        self.retranslateUi(Dialog)
        QtCore.QObject.connect(self.quit_button, QtCore.SIGNAL(_fromUtf8("clicked()")), Dialog.close)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        pass


class WtQT4(QtGui.QDialog): 
 def __init__(self):
     	 
        QtGui.QDialog.__init__(self)
        self.ui = Ui_Dialog()
        self.ui.setupUi(self)
	self.connect(self.ui.refreshbutton, QtCore.SIGNAL("clicked()"), self.timefill)
        self.connect(self.ui.startButton, QtCore.SIGNAL("clicked()"), self.startTimer)
        self.connect(self.ui.stopButton, QtCore.SIGNAL("clicked()"), self.stopTimer)
        self.myTimer = QtCore.QTimer(self)
        QtCore.QObject.connect(self.myTimer,QtCore.SIGNAL("timeout()"), self.updateTimer)
        self.timerTime = 0
        self.myTimer.start(1000)
   
 def doGridfill(self,mywidget): 
		  
	 try:
              	row=self.TROW
		# best way to set rowheight in qt grid  ?
		mywidget.verticalHeader().setUpdatesEnabled(False)
		while mywidget.rowHeight(row)>20:
		    mywidget.verticalHeader().resizeSection(row,mywidget.rowHeight(row)-1)	
		mywidget.verticalHeader().setUpdatesEnabled(True)
                # set width of column 0
                while mywidget.columnWidth(0)<540:
		    mywidget.horizontalHeader().resizeSection(0,mywidget.columnWidth(0)+1)
                while mywidget.columnWidth(1)<350:
		    mywidget.horizontalHeader().resizeSection(1,mywidget.columnWidth(1)+1)
		mywidget.horizontalHeader().setUpdatesEnabled(True)

		for column in range(2): 
		     if column==0:
		         mywidget.setItem(row,column,QtGui.QTableWidgetItem(str(self.TCUR)))
		         mywidget.item(row,column).setTextAlignment(1)
		       
	             if column==1:
		         mywidget.setItem(row,column,QtGui.QTableWidgetItem(str(self.TBAL)))
		         mywidget.item(row,column).setTextAlignment(1)

	 except:
	     raise 
 
 def timefill(self):
         #self.ui.timegrid=self.ui.timegrid
         self.ui.timegrid.setRowCount(len(myTZ))
	 self.ui.timegrid.setColumnCount(2)
	 self.ui.timegrid.setHorizontalHeaderItem (0,QtGui.QTableWidgetItem('Date-Time'))
	 self.ui.timegrid.setHorizontalHeaderItem (1,QtGui.QTableWidgetItem('Location'))
	 self.ui.timegrid.setMouseTracking(True)
         self.TROW=-1
                  
         for myz in myTZ:                 
	         self.TROW=self.TROW+1
                 os.environ['TZ']= myz
                 tzset()
                 self.TCUR=strftime(timefmt)  
		 self.TBAL=myz
                 self.doGridfill(self.ui.timegrid) 
  

 def startTimer(self):
        self.myTimer.start(1000)
       
 def stopTimer(self):
        self.myTimer.stop()

 def updateTimer(self):
        self.timefill()
   
        totalSeconds = self.timerTime
        totalSeconds = totalSeconds + 1
        self.timerTime = self.timerTime + 1

        finalMinutes = totalSeconds / 60
        finalSeconds = totalSeconds % 60

        finalTime = QtCore.QTime(0, finalMinutes, finalSeconds)
        self.ui.timeEdit.setText(QtCore.QTime.toString(finalTime))
        
         

if __name__ == "__main__":
    printgreen('Hello from wt')	
    printgreen ("Qt Version  : '%s'"  % str(QtCore.QT_VERSION_STR)) 
    printgreen ("PyQt Version: '%s'"  % str(QtCore.PYQT_VERSION_STR))
    app = QtGui.QApplication(sys.argv)
    myapp = WtQT4()
    myapp.show()
    sys.exit(app.exec_())
    
    
    
    
    
    
    
    
    
    
    
    
	
