import sys
from PyQt5.QtWidgets import QApplication, QWidget, QLabel, QLineEdit, QPushButton, QVBoxLayout, QMessageBox
from PyQt5.QtWidgets import QMainWindow
import sqlite3
import bcrypt

class Login(QWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle('Login')

        # Create widgets
        self.username_label = QLabel('Username:')
        self.username_input = QLineEdit()
        self.password_label = QLabel('Password:')
        self.password_input = QLineEdit()
        self.password_input.setEchoMode(QLineEdit.Password)
        self.login_button = QPushButton('Login')
        self.login_button.clicked.connect(self.login)

        # Create layout and add widgets
        layout = QVBoxLayout()
        layout.addWidget(self.username_label)
        layout.addWidget(self.username_input)
        layout.addWidget(self.password_label)
        layout.addWidget(self.password_input)
        layout.addWidget(self.login_button)

        self.setLayout(layout)

    def login(self):
        # OBTENER DATOS DE LAS CAJAS DE
        username = self.username_input.text()
        password = self.password_input.text()

        # CHECA SI EL USUARIO ESTA EN LA BD
        conn = sqlite3.connect('mantenimiento.db')
        c = conn.cursor()
        c.execute("SELECT * FROM usuario WHERE id=? AND clave=?", (username, password))
        result = c.fetchone()
        #conn.close()

        if result is None:
            # USUARIO INCORRECTO
            QMessageBox.warning(self, 'Error', 'Invalid username')
        #VERIFY IF YOU ARE AN ADMINISTRATOR OR NOT
        elif result[4] == "admin":
            self.admin_window = AdminWindow()
            self.admin_window.show()
            self.close() 
        else:
            self.user_window = UserWindow()
            self.user_window.show()
            self.close()
          
        
class AdminWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Admin Window")
        self.setGeometry(100, 100, 300, 150)

        self.message_label = QLabel("Welcome Administrator!", self)
        self.message_label.move(50, 50)

class UserWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("User Window")
        self.setGeometry(100, 100, 300, 150)

        self.button = QPushButton("Click me", self)
        self.button.move(120, 50)
        self.button.clicked.connect(self.button_clicked)

    def button_clicked(self):
        QMessageBox.information(self, "Information", "You clicked the button.")


if __name__ == '__main__':
    app = QApplication(sys.argv)
    login = Login()
    login.show()
    sys.exit(app.exec_())
