from dataclasses import dataclass
from sqlite3 import Date
from typing import Optional, Tuple


@dataclass
class Departamento:
    id: int
    nombre: str

    @staticmethod
    def from_row(row: Tuple) -> "Departamento":
        return Departamento(
            id = row[0],
            nombre = row[1]
        )

    def to_row(self) -> Tuple:
        return (
            self.id,
            self.nombre
        )

@dataclass
class Usuario:
    id: str
    nombre: str
    apellidos: str
    clave: str
    tipo: str
    fecha_nacimiento: Date
    genero: str
    departamento: Optional[Departamento]

    @staticmethod
    def from_row(row: Tuple) -> "Usuario":
        return Usuario(
            id = row[0],
            nombre = row[1],
            apellidos = row[2],
            clave = row[3],
            tipo = row[4],
            fecha_nacimiento = row[5],
            genero = row[6],
            departamento = None,
        )

    def to_row(self) -> Tuple:
        return (
            self.id,
            self.nombre,
            self.apellidos,
            self.clave,
            self.tipo,
            self.fecha_nacimiento,
            self.genero,
            self.departamento.id if self.departamento else None,
        )

@dataclass
class Maquina:
    id: str
    modelo: str
    marca: str
    usuario: Optional[Usuario]
    departamento: Optional[Departamento]

    @staticmethod
    def from_row(row: Tuple) -> "Maquina":
        return Maquina(
            id = row[0],
            modelo = row[1],
            marca = row[2],
            usuario = None,
            departamento = None,
        )

    def to_row(self) -> Tuple:
        return (
            self.id,
            self.modelo,
            self.marca,
            self.departamento
        )

@dataclass
class Material:
    id: int
    nombre: str
    stock: float

    @staticmethod
    def from_row(row: Tuple) -> "Material":
        return Material(
            id = row[0],
            nombre = row[1],
            stock = row[2],
        )

    def to_row(self) -> Tuple:
        return (
            self.id,
            self.nombre,
            self.stock,
        )

@dataclass
class Mantenimiento:
    id: int
    tipo: str
    fecha_creacion: Date
    servidor: Optional[Usuario]
    fecha_servicio: Date

    @staticmethod
    def from_row(row: Tuple) -> "Mantenimiento":
        return Mantenimiento(
            id = row[0],
            tipo = row[1],
            fecha_creacion = row[2],
            servidor = None,
            fecha_servicio = row[3],
        )
    
