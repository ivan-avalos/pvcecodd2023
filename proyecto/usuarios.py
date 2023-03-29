from PyQt5.QtGui import QPixmap
from PyQt5.QtWidgets import (QComboBox, QDialog, QLabel, QLineEdit,
                             QMainWindow, QPushButton, QVBoxLayout, QWidget)

from database import *
from models import *


# USER WINDOW (INSERT USER) ==================================================================
class InsertUserWindow(QDialog):
    db: Database
    
    def __init__(self, db: Database):
        super().__init__()
        self.db = db

        # Store a reference to the main window(login)

        self.setWindowTitle('Insert')
        self.setGeometry(400, 400, 500, 400)
        
        # Create the form with two fields
        self.namel = QLabel('Name:')
        self.txtname = QLineEdit()
        self.lastnl = QLabel('Last Name:')
        self.txtlastn = QLineEdit()
        self.userl = QLabel('Username:')
        self.txtuser = QLineEdit()
        self.clavel = QLabel('Password:')

        self.genderl = QLabel('Gender:')
        self.cbmgender = QComboBox(self)
        self.cbmgender.addItem('Male', 'masculino')
        self.cbmgender.addItem('Female', 'femenino')
        self.cbmgender.addItem('Other', 'otro')

        self.txtclave = QLineEdit()
        self.datel = QLabel('Birthdate (yyyy-mm-dd):')
        self.txtdate = QLineEdit()

        #LISTS ===================================
        self.typel = QLabel('User type:')
        self.cbmtype = QComboBox(self)
        self.cbmtype.addItem('User', 0)
        self.cbmtype.addItem('Admin', 1)
        self.cbmtype.move(50, 50)

        depas = Departamento.all(self.db)
        self.depal = QLabel('Department:')
        self.cbmdepa = QComboBox(self)
        for d in (depas if depas else []):
            self.cbmdepa.addItem(d.nombre, d.id)

        self.btninsert2 = QPushButton('Insert')
        self.btnupdate = QPushButton('Update')
        self.btndelete = QPushButton('Delete')

        # Connect buttons to events
        self.btninsert2.clicked.connect(self.insert_user)

        # Add the fields to a layout
        form_layout = QVBoxLayout()
        form_layout.addWidget(self.namel)
        form_layout.addWidget(self.txtname)
        form_layout.addWidget(self.lastnl)
        form_layout.addWidget(self.txtlastn)
        form_layout.addWidget(self.userl)
        form_layout.addWidget(self.txtuser)
        form_layout.addWidget(self.clavel)
        form_layout.addWidget(self.txtclave)
        form_layout.addWidget(self.genderl)
        form_layout.addWidget(self.cbmgender)
        form_layout.addWidget(self.datel)
        form_layout.addWidget(self.txtdate)
        form_layout.addWidget(self.typel)
        form_layout.addWidget(self.cbmtype)
        form_layout.addWidget(self.depal)
        form_layout.addWidget(self.cbmdepa)
        form_layout.addWidget(self.btninsert2)

        self.setLayout(form_layout)

    def insert_user(self):
        Usuario(
            id = None,
            nombre = self.txtname.text(),
            apellidos = self.txtlastn.text(),
            usuario = self.txtuser.text(),
            clave = self.txtclave.text(),
            tipo = self.cbmtype.currentData(),
            fecha_nacimiento = sqlite3.Date.fromisoformat(self.txtdate.text()),
            genero = self.cbmgender.currentData(),
            departamento_id = self.cbmdepa.currentData(),
        ).save(self.db)
        self.close()

#===============================================================================


# USER WINDOW (INSERT USER) ==================================================================
class UpdateUserWindow(QDialog):
    db: Database
    
    def __init__(self, db: Database):
        super().__init__()
        self.db = db

        # Store a reference to the main window(login)

        self.setWindowTitle('Update')
        self.setGeometry(400, 400, 500, 400)

        self.userl = QLabel('Username:')
        self.txtuser = QLineEdit()
        self.btnsearch = QPushButton('Search')

        # Create the form with two fields
        self.idl = QLabel('User ID:')
        self.txtid = QLineEdit()
        self.namel = QLabel('Name:')
        self.txtname = QLineEdit()
        self.lastnl = QLabel('Last Name:')
        self.txtlastn = QLineEdit()
        self.userl = QLabel('Username:')
        self.txtuser = QLineEdit()
        self.clavel = QLabel('Password:')

        self.genderl = QLabel('Gender:')
        self.cbmgender = QComboBox(self)
        self.cbmgender.addItem('Male', 'masculino')
        self.cbmgender.addItem('Female', 'femenino')
        self.cbmgender.addItem('Other', 'otro')

        self.txtclave = QLineEdit()
        self.datel = QLabel('Birthdate (yyyy-mm-dd):')
        self.txtdate = QLineEdit()

        #LISTS ===================================
        self.typel = QLabel('User type:')
        self.cbmtype = QComboBox(self)
        self.cbmtype.addItem('User', 0)
        self.cbmtype.addItem('Admin', 1)
        self.cbmtype.move(50, 50)

        depas = Departamento.all(self.db)
        self.depal = QLabel('Department:')
        self.cbmdepa = QComboBox(self)
        for d in (depas if depas else []):
            self.cbmdepa.addItem(d.nombre, d.id)

        self.btnupdate = QPushButton('Update')
        self.btndelete = QPushButton('Delete')

        # Connect buttons to events
        self.btnsearch.clicked.connect(self.search_user)
        self.btnupdate.clicked.connect(self.update_user)
        self.btndelete.clicked.connect(self.delete_user)

        # Add the fields to a layout
        form_layout = QVBoxLayout()
        form_layout.addWidget(self.userl)
        form_layout.addWidget(self.txtuser)
        form_layout.addWidget(self.btnsearch)
        form_layout.addWidget(self.idl)
        form_layout.addWidget(self.txtid)
        form_layout.addWidget(self.namel)
        form_layout.addWidget(self.txtname)
        form_layout.addWidget(self.lastnl)
        form_layout.addWidget(self.txtlastn)
        form_layout.addWidget(self.userl)
        form_layout.addWidget(self.txtuser)
        form_layout.addWidget(self.clavel)
        form_layout.addWidget(self.txtclave)
        form_layout.addWidget(self.genderl)
        form_layout.addWidget(self.cbmgender)
        form_layout.addWidget(self.datel)
        form_layout.addWidget(self.txtdate)
        form_layout.addWidget(self.typel)
        form_layout.addWidget(self.cbmtype)
        form_layout.addWidget(self.depal)
        form_layout.addWidget(self.cbmdepa)
        form_layout.addWidget(self.btnupdate)
        form_layout.addWidget(self.btndelete)

        self.setLayout(form_layout)
#===============================================================================

    def search_user(self):
        self.user = Usuario.findByUsername(self.db, self.txtuser.text())
        if self.user:
            self.txtid.setText(self.user.id or "")
            self.txtname.setText(self.user.nombre)
            self.txtlastn.setText(self.user.apellidos)
            self.txtuser.setText(self.user.usuario)
            self.txtclave.setText(self.user.clave)
            tipo = self.cbmtype.findData(self.user.tipo)
            self.cbmtype.setCurrentIndex(tipo)
            self.txtdate.setText(self.user.fecha_nacimiento.isoformat())
            genero = self.cbmgender.findData(self.user.genero)
            self.cbmgender.setCurrentIndex(genero)
            depa_id = self.cbmdepa.findData(self.user.departamento_id)
            self.cbmdepa.setCurrentIndex(depa_id)

    def update_user(self):
        self.user = Usuario(
            id = self.txtid.text(),
            nombre = self.txtname.text(),
            apellidos = self.txtlastn.text(),
            usuario = self.txtuser.text(),
            clave = self.txtclave.text(),
            tipo = self.cbmtype.currentData(),
            fecha_nacimiento = sqlite3.Date.fromisoformat(self.txtdate.text()),
            genero = self.cbmgender.currentData(),
            departamento_id = self.cbmdepa.currentData(),
        )
        self.user.save(self.db)
        self.close()

    def delete_user(self):
        if self.user:
            self.user.remove(self.db)
            self.close()

#===============================================================================
class QRcode(QDialog):
    def __init__(self, qr_file: str):
        super().__init__()

        # Store a reference to the main window(login)

        self.setWindowTitle('QR Query')
        self.setGeometry(400, 400, 400, 400)

        pixmap = QPixmap(qr_file)

        label = QLabel()
        label.setPixmap(pixmap)

        l = QVBoxLayout(self)
        l.addChildWidget(label)

        self.setLayout(l)
        
#===============================================================================
class UserOperationMenu(QWidget):
    db: Database
    
    def __init__(self, db: Database):
        super().__init__()
        self.db = db
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
        self.setWindowTitle("Admin Window")
        self.show()

    def showInsertWindow(self):
        # Show the insert window
        InsertUserWindow(self.db).exec_()

    def showUpdateWindow(self):
        # Show the insert window
        UpdateUserWindow(self.db).exec_()
    
    def showcode(self):
        QRcode("usuarios_qr.png").exec_()
