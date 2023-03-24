from PyQt5.QtWidgets import QApplication, QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QLineEdit, QLabel, QDialog, QComboBox ,QStackedLayout

# USER WINDOW (INSERT USER) ==================================================================
class InsertWindow(QDialog):
    def __init__(self):
        super().__init__()

        # Store a reference to the main window(login)

        self.setWindowTitle('Insert')
        self.setGeometry(400, 400, 500, 400)
        
        # Create the form with two fields
        self.date1l = QLabel('Creation date:')
        self.txtdate1 = QLineEdit()
        self.serverl = QLabel('Server id:')
        self.txtserver = QLineEdit()
        self.datel2 = QLabel('Date of service:')
        self.txtdate2 = QLineEdit()


        #LISTS ===================================
        self.typel = QLabel('Support type:')
        self.cbmtype = QComboBox(self)
        self.cbmtype.addItem('PREVENTIVO')
        self.cbmtype.addItem('CORRECTIVO')
        self.cbmtype.move(50, 50)


        self.btninsert2 = QPushButton('Insert')
        self.btnupdate = QPushButton('Update')
        self.btndelete = QPushButton('Delete')
       

        # Add the fields to a layout
        form_layout = QVBoxLayout()
        form_layout.addWidget(self.date1l)
        form_layout.addWidget(self.txtdate1)

        form_layout.addWidget(self.typel)
        form_layout.addWidget(self.cbmtype)

        form_layout.addWidget(self.serverl)
        form_layout.addWidget(self.txtserver)

        form_layout.addWidget(self.datel2)
        form_layout.addWidget(self.txtdate2)

        form_layout.addWidget(self.btninsert2)

        self.setLayout(form_layout)
#===============================================================================


# USER WINDOW (UPDATE USER) ==================================================================
class updateWindow(QDialog):
    def __init__(self):
        super().__init__()

        # Store a reference to the main window(login)

        self.setWindowTitle('Update Delete')
        self.setGeometry(400, 400, 500, 400)
        

        self.idl = QLabel('User ID:')
        self.txtid = QLineEdit()
        self.btnsearch = QPushButton('Search')

# Create the form with two fields
        self.date1l = QLabel('Creation date:')
        self.txtdate1 = QLineEdit()
        self.serverl = QLabel('Server id:')
        self.txtserver = QLineEdit()
        self.datel2 = QLabel('Date of service:')
        self.txtdate2 = QLineEdit()


        #LISTS ===================================
        self.typel = QLabel('Support type:')
        self.cbmtype = QComboBox(self)
        self.cbmtype.addItem('PREVENTIVO')
        self.cbmtype.addItem('CORRECTIVO')
        self.cbmtype.move(50, 50)
      

        self.btnupdate = QPushButton('Update')
        self.btndelete = QPushButton('Delete')

        # Add the fields to a layout
        form_layout = QVBoxLayout()
        form_layout.addWidget(self.idl)
        form_layout.addWidget(self.txtid)
        form_layout.addWidget(self.btnsearch)

        form_layout.addWidget(self.date1l)
        form_layout.addWidget(self.txtdate1)

        form_layout.addWidget(self.typel)
        form_layout.addWidget(self.cbmtype)

        form_layout.addWidget(self.serverl)
        form_layout.addWidget(self.txtserver)

        form_layout.addWidget(self.datel2)
        form_layout.addWidget(self.txtdate2)

        form_layout.addWidget(self.btnupdate)
        form_layout.addWidget(self.btndelete)
        

        self.setLayout(form_layout)
#===============================================================================


#===============================================================================
class codeqr(QDialog):
    def __init__(self):
        super().__init__()

        # Store a reference to the main window(login)

        self.setWindowTitle('QR Query')
        self.setGeometry(400, 400, 400, 400)
        
        
#===============================================================================
class Window(QWidget):
    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):
        # Create the layout for the window
        layout = QVBoxLayout(self)

        # Create the buttons for column 1
        insert_button = QPushButton("Insert")
        update_button = QPushButton("Update")
        query_button = QPushButton("Query")

        insert_button.clicked.connect(self.showInsertWindow)
        update_button.clicked.connect(self.showUpdateWindow)
        query_button.clicked.connect(self.showcode)

        # Add the buttons to column 1 layout
        layout.addWidget(insert_button)
        layout.addWidget(update_button)
        layout.addWidget(query_button)

        self.setLayout(layout)
        self.setGeometry(100, 100, 500, 500)
        self.setWindowTitle("Support Window")
        self.show()

    def showInsertWindow(self):
        # Show the insert window
        insert_window = InsertWindow()
        insert_window.exec_()

    def showUpdateWindow(self):
        # Show the insert window
        update_window = updateWindow()
        update_window.exec_()
    
    def showcode(self):
        code_qr = codeqr()
        code_qr.exec_()

if __name__ == '__main__':
    app = QApplication([])
    window = Window()
    app.exec_()


