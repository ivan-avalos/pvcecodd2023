import sys

from PyQt5.QtWidgets import (QApplication, QLabel, QLineEdit, QMainWindow,
                             QMessageBox, QPushButton, QVBoxLayout, QWidget)

from database import Database
from models import Usuario


class Login(QWidget):
    db: Database
     
    def __init__(self, db: Database):
        super().__init__()
        self.db = db
        self.setWindowTitle('Login')
        self.setGeometry(300, 300, 500, 300)

        # Create widgets
        self.username_label = QLabel('Username:')
        self.username_input = QLineEdit()
        self.password_label = QLabel('Password:')
        self.password_input = QLineEdit()
        self.password_input.setEchoMode(QLineEdit.Password)
        self.login_button = QPushButton('Login')
        self.exit_button = QPushButton('Exit')
        self.login_button.clicked.connect(self.login)
        self.exit_button.clicked.connect(self.quit)

        # Create layout and add widgets
        layout = QVBoxLayout()
        layout.addWidget(self.username_label)
        layout.addWidget(self.username_input)
        layout.addWidget(self.password_label)
        layout.addWidget(self.password_input)
        layout.addWidget(self.login_button)
        layout.addWidget(self.exit_button)

        self.setLayout(layout)

    def login(self):
        # OBTENER DATOS DE LAS CAJAS DE
        username = self.username_input.text()
        password = self.password_input.text()

        # CHECA SI EL USUARIO ESTA EN LA BD
        cur = self.db.conn.cursor()
        cur.execute("SELECT * FROM usuarios WHERE usuario=? AND clave=?", (username, password))
        row = cur.fetchone()

        if row is None:
            # USUARIO INCORRECTO
            QMessageBox.warning(self, 'Error', 'Invalid username')
        else:
            usuario = Usuario.from_row(row)
            print(usuario)
            if usuario.tipo == "1":
                #VERIFY IF YOU ARE AN ADMINISTRATOR OR NOT
                self.admin_window = AdminWindow()
                self.admin_window.show()
                self.close()
            else:
                self.user_window = UserWindow()
                self.user_window.show()
                self.close()

    def quit(self):
        # Close the entire application
        QApplication.quit()
          
# -----------------------------------------------------------------------VENTANA PRINCIPAL PARA EL ADMINISTRADOR         
class AdminWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        # Store a reference to the main window(login)
        self.login = login

        self.setWindowTitle("Admin Window")
        self.setGeometry(300, 300, 500, 300)
        
        #interfaz simple
        self.message_label = QLabel("Administrator", self)

        # Create 3 buttons and set their text
        self.btnusers = QPushButton('USERS', self)
        self.btnm = QPushButton('MAINTENANCE', self)
        self.btnexit = QPushButton('LOG OUT', self)

        self.message_label.move(120, 70)
        self.btnusers.move(210,100)
        self.btnm.move(210,150)
        self.btnexit.move(210,200)

        # Connect the buttons to their respective functions
        self.btnexit.clicked.connect(self.go_back)
    
    def go_back(self):
        # Close the second window
         self.close()
        # Show the main window
         self.login.show()

# ========================================================================VENTANA PRINCIPAL PARA EL USUARIO
class UserWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        # Store a reference to the main window(login)
        self.login = login

        self.setWindowTitle("User Window")
        self.setGeometry(300, 300, 500, 300)

        #interfaz simple
        self.message_label = QLabel("User", self)

        # Create two buttons and set their text
        self.btnm = QPushButton('MAINTENANCE', self)
        self.btnexit = QPushButton('LOG OUT', self)
       
        self.message_label.move(120, 70)
        self.btnm.move(210,100)
        self.btnexit.move(210,150)

        # Connect the buttons to their respective functions
        self.btnexit.clicked.connect(self.go_back)

    def go_back(self):
        # Close the second window
         self.close()
        # Show the main window
         self.login.show()
  
#--------------------------------------------------------------------------MAIN
if __name__ == '__main__':
    db = Database()
    db.connect('mantenimiento.db')
    db.migrate()
    app = QApplication(sys.argv)
    login = Login(db)
    login.show()
    res = app.exec_()
    db.disconnect()
    # sys.exit(res)

'''
CÓDIGO DE PRUEBA
----------------
from models import *

db = Database()
db.connect('mantenimiento.db')
'''
