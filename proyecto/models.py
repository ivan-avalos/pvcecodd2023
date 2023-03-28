import uuid
from dataclasses import dataclass
from sqlite3 import Date
from typing import List, Optional, Tuple

from database import Database


@dataclass
class Departamento:
    id: Optional[int]
    nombre: str

    @staticmethod
    def from_row(row: Tuple) -> "Departamento":
        return Departamento(
            id = row[0],
            nombre = row[1]
        )

    @staticmethod
    def all(db: Database) -> List["Departamento"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM departamentos")
        deps = [Departamento.from_row(d) for d in cur.fetchall()]
        cur.close()
        return deps

    @staticmethod
    def find(db: Database, id: int) -> Optional["Departamento"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM departamentos WHERE id = ?", (id,))
        row = cur.fetchone()
        cur.close()
        return Departamento.from_row(row) if row else None

    def save(self, db: Database) -> None:
        cur = db.conn.cursor()
        row = (self.nombre,)
        if self.id == None:
            cur.execute('''
            INSERT INTO departamentos (nombre) VALUES (?)
            ''', row)
        else:
            cur.execute('''
            UPDATE departamentos SET nombre = ?
            WHERE id = ?
            ''', row + (self.id,))
        db.conn.commit()
        cur.close()

    def remove(self, db: Database) -> None:
        cur = db.conn.cursor()
        cur.execute("DELETE FROM departamentos WHERE id = ?", (self.id,))
        db.conn.commit()
        cur.close()
        self.id = None

@dataclass
class Usuario:
    id: Optional[str]
    nombre: str
    apellidos: str
    usuario: str
    clave: str
    tipo: int
    fecha_nacimiento: Date
    genero: str
    departamento_id: Optional[int]

    @staticmethod
    def from_row(row: Tuple) -> "Usuario":
        return Usuario(
            id = row[0],
            nombre = row[1],
            apellidos = row[2],
            usuario = row[3],
            clave = row[4],
            tipo = row[5],
            fecha_nacimiento = Date.fromisoformat(row[6]),
            genero = row[7],
            departamento_id = row[8],
        )

    def departamento(self, db: Database) -> Optional[Departamento]:
        if self.departamento_id == None:
            return None
        return Departamento.find(db, self.departamento_id)

    @staticmethod
    def all(db: Database) -> List["Usuario"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM usuarios")
        users = [Usuario.from_row(u) for u in cur.fetchall()]
        cur.close()
        return users

    @staticmethod
    def find(db: Database, id: str) -> Optional["Usuario"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM usuarios WHERE id = ?", (id,))
        row = cur.fetchone()
        cur.close()
        return Usuario.from_row(row) if row else None

    @staticmethod
    def findByUsername(db: Database, usuario: str) -> Optional["Usuario"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM usuarios WHERE usuario = ?", (usuario,))
        row = cur.fetchone()
        cur.close()
        return Usuario.from_row(row) if row else None

    def save(self, db: Database) -> None:
        cur = db.conn.cursor()
        row = (
            self.nombre,
            self.apellidos,
            self.usuario,
            self.clave,
            self.tipo,
            self.fecha_nacimiento.isoformat(),
            self.genero,
            self.departamento_id,
        )
        if self.id == None:
            id = str(uuid.uuid4())
            cur.execute('''
            INSERT INTO usuarios (
            id, nombre, apellidos, usuario, clave, tipo,
            fecha_nacimiento, genero, departamento_id)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (id,) + row)
        else:
            cur.execute('''
            UPDATE usuarios SET
            nombre = ?, apellidos = ?, usuario = ?,
            clave = ?, tipo = ?, fecha_nacimiento = ?,
            genero = ?, departamento_id = ?
            WHERE id = ?
            ''', row + (self.id,))
        db.conn.commit()
        cur.close()

    def remove(self, db: Database) -> None:
        cur = db.conn.cursor()
        cur.execute("DELETE FROM usuarios WHERE id = ?", (self.id,))
        db.conn.commit()
        cur.close()
        self.id = None

@dataclass
class Maquina:
    serie: str
    modelo: str
    marca: str
    usuario_id: Optional[str]
    departamento_id: Optional[int]

    @staticmethod
    def from_row(row: Tuple) -> "Maquina":
        return Maquina(
            serie = row[0],
            modelo = row[1],
            marca = row[2],
            usuario_id = row[3],
            departamento_id = row[4],
        )

    def usuario(self, db: Database) -> Optional[Usuario]:
        if self.usuario_id == None:
            return None
        return Usuario.find(db, self.usuario_id)

    def departamento(self, db: Database) -> Optional[Departamento]:
        if self.departamento_id == None:
            return None
        return Departamento.find(db, self.departamento_id)

    @staticmethod
    def all(db: Database) -> List["Maquina"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM maquinas")
        maquinas = [Maquina.from_row(m) for m in cur.fetchall()]
        cur.close()
        return maquinas

    @staticmethod
    def find(db: Database, serie: str) -> Optional["Maquina"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM maquinas WHERE serie = ?", (serie,))
        row = cur.fetchone()
        cur.close()
        return Maquina.from_row(row) if row else None

    def save(self, db: Database) -> None:
        cur = db.conn.cursor()
        row = (
            self.modelo,
            self.marca,
            self.usuario_id,
            self.departamento_id,
            self.serie,
        )
        # Si no existe, entonces crear la máquina
        if Maquina.find(db, self.serie) == None:
            cur.execute('''
            INSERT INTO maquinas (
            modelo, marca, usuario_id, departamento_id, serie
            ) VALUES (?, ?, ?, ?, ?)
            ''', row)
        else:
            cur.execute('''
            UPDATE maquinas SET
            modelo = ?, marca = ?, usuario_id = ?, departamento_id = ?
            WHERE serie = ?
            ''', row)
        db.conn.commit()
        cur.close()

    def remove(self, db: Database) -> None:
        cur = db.conn.cursor()
        cur.execute("DELETE FROM maquinas WHERE serie = ?", (self.serie,))
        db.conn.commit()
        cur.close()

@dataclass
class Material:
    id: Optional[int]
    nombre: str
    stock: float

    @staticmethod
    def from_row(row: Tuple) -> "Material":
        return Material(
            id = row[0],
            nombre = row[1],
            stock = row[2],
        )

    @staticmethod
    def all(db: Database) -> List["Material"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM materiales")
        materiales = [Material.from_row(m) for m in cur.fetchall()]
        cur.close()
        return materiales

    @staticmethod
    def find(db: Database, id: int) -> Optional["Maquina"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM materiales WHERE id = ?", (id,))
        row = cur.fetchone()
        cur.close()
        return Maquina.from_row(row) if row else None

    def save(self, db: Database) -> None:
        cur = db.conn.cursor()
        row = (
            self.nombre,
            self.stock,
        )
        if self.id == None:
            cur.execute('''
            INSERT INTO materiales (nombre, stock) VALUES (?, ?)
            ''', row)
        else:
            cur.execute('''
            UPDATE materiales SET nombre = ?, stock = ?
            WHERE id = ?
            ''', row + (self.id,))
        db.conn.commit()
        cur.close()

    def remove(self, db: Database) -> None:
        cur = db.conn.cursor()
        cur.execute("DELETE FROM materiales WHERE id = ?", (self.id,))
        db.conn.commit()
        cur.close()
        self.id = None

@dataclass
class Mantenimiento:
    id: Optional[int]
    tipo: str
    fecha_creacion: Date
    servidor_id: Optional[str]
    fecha_servicio: Optional[Date]

    @staticmethod
    def from_row(row: Tuple) -> "Mantenimiento":
        return Mantenimiento(
            id = row[0],
            tipo = row[1],
            fecha_creacion = Date.fromisoformat(row[2]),
            servidor_id = row[3],
            fecha_servicio = Date.fromisoformat(row[4]) if row[4] else None,
        )

    def servidor(self, db: Database) -> Optional[Usuario]:
        if self.servidor_id == None:
            return None
        return Usuario.find(db, self.servidor_id)

    def materiales(self, db: Database) -> List[Material]:
        cur = db.conn.cursor()
        cur.execute('''
        SELECT materiales.* FROM mantenimientos_materiales
        JOIN materiales ON materiales.id = mantenimientos_materiales.material_id
        WHERE mantenimiento_id = ?
        ''', (self.id,))
        materials = [Material.from_row(m) for m in cur.fetchall()]
        cur.close()
        return materials

    @staticmethod
    def all(db: Database) -> List["Mantenimiento"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM mantenimientos")
        mants = [Mantenimiento.from_row(m) for m in cur.fetchall()]
        cur.close()
        return mants

    @staticmethod
    def find(db: Database, id: int) -> Optional["Mantenimiento"]:
        cur = db.conn.cursor()
        cur.execute("SELECT * FROM mantenimientos WHERE id = ?", (id,))
        row = cur.fetchone()
        cur.close()
        return Mantenimiento.from_row(row) if row else None

    def save(self, db: Database) -> None:
        cur = db.conn.cursor()
        row = (
            self.tipo,
            self.fecha_creacion.isoformat(),
            self.servidor_id,
            self.fecha_servicio.isoformat() if self.fecha_servicio else None,
        )
        if self.id == None:
            cur.execute('''
            INSERT INTO mantenimientos (
            tipo, fecha_creacion, servidor, fecha_servicio
            ) VALUES (?, ?, ?, ?)
            ''', row)
        else:
            cur.execute('''
            UPDATE mantenimientos SET
            tipo = ?, fecha_creacion = ?, servidor = ?, fecha_servicio = ?
            WHERE id = ?
            ''', row + (self.id,))
        db.conn.commit()
        cur.close()

    def add_material(self, db: Database, material_id: int, cantidad: float):
        if self.id != None:
            cur = db.conn.cursor()
            cur.execute('''
            INSERT INTO mantenimientos_materiales (
            mantenimiento_id, material_id, cantidad
            ) VALUES (?, ?, ?)
            ''', (self.id, material_id, cantidad))
            db.conn.commit()
            cur.close()

    def edit_material(self, db: Database, material_id: int, cantidad: float):
        if self.id != None:
            cur = db.conn.cursor()
            cur.execute('''
            UPDATE mantenimientos_materiales SET cantidad = ?
            WHERE mantenimiento_id = ? AND material_id = ?
            ''', (cantidad, self.id, material_id))
            db.conn.commit()
            cur.close()

    def remove_material(self, db: Database, material_id: int):
        if self.id != None:
            cur = db.conn.cursor()
            cur.execute('''
            DELETE FROM mantenimientos_materiales
            WHERE mantenimiento_id = ? AND material_id = ?
            ''', (self.id, material_id))
            db.conn.commit()
            cur.close()

    def remove(self, db: Database) -> None:
        cur = db.conn.cursor()
        cur.execute("DELETE FROM mantenimientos WHERE id = ?", (self.id,))
        db.conn.commit()
        cur.close()
        self.id = None
    
    
