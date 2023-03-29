from sqlite3 import Date

from PyQt5.QtWidgets import (QComboBox, QDialog, QLabel, QLineEdit,
                             QPushButton, QVBoxLayout, QWidget)

from database import Database
from models import Mantenimiento


# USER WINDOW (INSERT USER) ==================================================================
class InsertMaintenanceWindow(QDialog):
    db: Database
    
    def __init__(self, db: Database):
        super().__init__()
        self.db = db

        # Store a reference to the main window(login)

        self.setWindowTitle('Insert')
        self.setGeometry(400, 400, 500, 400)
        
        # Create the form with two fields
        self.serverl = QLabel('Server ID:')
        self.txtserver = QLineEdit()

        #LISTS ===================================
        self.typel = QLabel('Maintenance type:')
        self.cbmtype = QComboBox(self)
        self.cbmtype.addItem('PREVENTIVO', 'preventivo')
        self.cbmtype.addItem('CORRECTIVO', 'correctivo')
        self.cbmtype.move(50, 50)


        self.btninsert2 = QPushButton('Insert')
        self.btninsert2.clicked.connect(self.insert_maintenance)

        # Add the fields to a layout
        form_layout = QVBoxLayout()

        form_layout.addWidget(self.typel)
        form_layout.addWidget(self.cbmtype)

        form_layout.addWidget(self.serverl)
        form_layout.addWidget(self.txtserver)

        form_layout.addWidget(self.btninsert2)

        self.setLayout(form_layout)

    def insert_maintenance(self):
        Mantenimiento(
            id = None,
            tipo = self.cbmtype.currentData(),
            fecha_creacion = Date.today(),
            servidor_id = self.txtserver.text(),
            fecha_servicio = None
        ).save(self.db)
        self.close()

#===============================================================================


# USER WINDOW (UPDATE USER) ==================================================================
class UpdateMaintenanceWindow(QDialog):
    db: Database
    def __init__(self, db: Database):
        super().__init__()
        self.db = db

        # Store a reference to the main window(login)

        self.setWindowTitle('Update')
        self.setGeometry(400, 400, 500, 400)
        

        self.idl = QLabel('Mantenance ID:')
        self.txtid = QLineEdit()
        self.btnsearch = QPushButton('Search')

# Create the form with two fields
        self.serverl = QLabel('Server id:')
        self.txtserver = QLineEdit()
        self.datel2 = QLabel('Date of service:')
        self.txtdate2 = QLineEdit()

        #LISTS ===================================
        self.typel = QLabel('Support type:')
        self.cbmtype = QComboBox(self)
        self.cbmtype.addItem('PREVENTIVO', 'preventivo')
        self.cbmtype.addItem('CORRECTIVO', 'correctivo')
        self.cbmtype.move(50, 50)
      

        self.btnupdate = QPushButton('Update')
        self.btndelete = QPushButton('Delete')

        self.btnsearch.clicked.connect(self.search_maintenance)
        self.btnupdate.clicked.connect(self.actualizar_mantenimiento)
        self.btndelete.clicked.connect(self.eliminar_mantenimiento)

        # Add the fields to a layout
        form_layout = QVBoxLayout()
        form_layout.addWidget(self.idl)
        form_layout.addWidget(self.txtid)
        form_layout.addWidget(self.btnsearch)

        form_layout.addWidget(self.typel)
        form_layout.addWidget(self.cbmtype)

        form_layout.addWidget(self.serverl)
        form_layout.addWidget(self.txtserver)

        form_layout.addWidget(self.datel2)
        form_layout.addWidget(self.txtdate2)

        form_layout.addWidget(self.btnupdate)
        form_layout.addWidget(self.btndelete)
        

        self.setLayout(form_layout)

    def search_maintenance(self):
        id = int(self.txtid.text())
        self.mant = Mantenimiento.find(self.db, id)
        if self.mant:
            type = self.cbmtype.findData(self.mant.tipo)
            self.cbmtype.setCurrentIndex(type)
            self.txtserver.setText(self.mant.servidor_id or "")
            self.txtdate2.setText(self.mant.fecha_servicio.isoformat() if self.mant.fecha_servicio else "")

    def actualizar_mantenimiento(self):
        if self.mant:
            fecha_servicio = self.txtdate2.text()
            Mantenimiento(
                id = self.mant.id,
                tipo = self.cbmtype.currentData(),
                fecha_creacion = self.mant.fecha_creacion,
                servidor_id = self.txtserver.text(),
                fecha_servicio = Date.fromisoformat(fecha_servicio) if fecha_servicio != "" else None
            ).save(self.db)
            self.close()

    def eliminar_mantenimiento(self):
        if self.mant:
            self.mant.remove(self.db)
            self.close()
        
#===============================================================================


#===============================================================================
class QRcode(QDialog):
    def __init__(self):
        super().__init__()

        # Store a reference to the main window(login)

        self.setWindowTitle('QR Query')
        self.setGeometry(400, 400, 400, 400)
        
        
#===============================================================================
class MaintenanceOperationMenu(QWidget):
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
        self.setWindowTitle("Support Window")
        self.show()

    def showInsertWindow(self):
        # Show the insert window
        InsertMaintenanceWindow(self.db).exec_()

    def showUpdateWindow(self):
        # Show the insert window
        UpdateMaintenanceWindow(self.db).exec_()
    
    def showcode(self):
        QRcode().exec_()
