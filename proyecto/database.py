import sqlite3


class Database:
    conn: sqlite3.Connection
    
    def connect(self, db_path: str) -> None:
        self.conn = sqlite3.connect(db_path)

    def disconnect(self):
        self.conn.close()

    def migrate(self):
        cursor = self.conn.cursor()
        
        cursor.execute('''
        CREATE TABLE IF NOT EXISTS departamentos (
        id INTEGER PRIMARY KEY,
        nombre TEXT NOT NULL)
        ''')

        cursor.execute('''
        CREATE TABLE IF NOT EXISTS usuarios (
        id TEXT PRIMARY KEY,
        nombre TEXT NOT NULL,
        apellidos TEXT NOT NULL,
        usuario TEXT NOT NULL,
        clave TEXT NOT NULL,
        tipo TEXT DEFAULT 0,
        fecha_nacimiento TEXT NOT NULL,
        genero TEXT NOT NULL,
        departamento_id INTEGER,
        FOREIGN KEY (departamento_id) REFERENCES departamentos (id) ON DELETE SET NULL)
        ''')

        cursor.execute('''
        CREATE TABLE IF NOT EXISTS maquinas (
        serie TEXT PRIMARY KEY,
        modelo TEXT NOT NULL,
        marca TEXT NOT NULL,
        usuario_id TEXT,
        departamento_id INTEGER,
        FOREIGN KEY (usuario_id) REFERENCES usuario (id) ON DELETE SET NULL,
        FOREIGN KEY (departamento_id) REFERENCES departamentos (id) ON DELETE SET NULL)
        ''')

        cursor.execute('''
        CREATE TABLE IF NOT EXISTS mantenimientos (
        id INTEGER PRIMARY KEY,
        tipo TEXT NOT NULL,
        fecha_creacion TEXT NOT NULL,
        servidor_id TEXT,
        fecha_servicio TEXT,
        FOREIGN KEY (servidor_id) REFERENCES usuarios (id) ON DELETE SET NULL)
        ''')

        cursor.execute('''
        CREATE TABLE IF NOT EXISTS materiales (
        id INTEGER PRIMARY KEY,
        nombre TEXT NOT NULL,
        stock NUMERIC DEFAULT 0)
        ''')

        cursor.execute('''
        CREATE TABLE IF NOT EXISTS mantenimientos_materiales(
        mantenimiento_id INTEGER,
        material_id INTEGER,
        cantidad NUMERIC NOT NULL,
        FOREIGN KEY (mantenimiento_id) REFERENCES mantenimientos (id) ON DELETE CASCADE,
        FOREIGN KEY (material_id) REFERENCES materiales (id) ON DELETE CASCADE,
        PRIMARY KEY (mantenimiento_id, material_id))
        ''')

        cursor.close()

