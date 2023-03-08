{-# LANGUAGE OverloadedStrings #-}
import Database.SQLite.Simple

migrate conn = do
  -- Crear tabla «departamentos»
  execute_ conn "CREATE TABLE IF NOT EXISTS departamentos ( \
                \ id INTEGER PRIMARY KEY, \
                \ nombre TEXT NOT NULL)"
  -- Crear tabla «usuarios»
  execute_ conn "CREATE TABLE IF NOT EXISTS usuarios ( \
                \ id INTEGER PRIMARY KEY, \
                \ nombre TEXT NOT NULL, \
                \ apellidos TEXT NOT NULL, \
                \ fecha_nacimiento TEXT NOT NULL, \
                \ genero TEXT NOT NULL, \
                \ departamento_id INTEGER, \
                \ FOREIGN KEY (departamento_id) REFERENCES departamentos (id))"
  -- Crear tabla «máquinas»
  execute_ conn "CREATE TABLE IF NOT EXISTS maquinas ( \
                \ serie TEXT PRIMARY KEY, \
                \ modelo TEXT NOT NULL, \
                \ marca TEXT NOT NULL, \
                \ usuario_id INTEGER, \
                \ departamento_id INTEGER, \
                \ FOREIGN KEY (usuario_id) REFERENCES usuario (id), \
                \ FOREIGN KEY (departamento_id) REFERENCES departamentos (id))"
  -- Crear tabla «mantenimientos»
  execute_ conn "CREATE TABLE IF NOT EXISTS mantenimientos ( \
                \ id INTEGER PRIMARY KEY, \
                \ tipo TEXT NOT NULL, \
                \ fecha TEXT NOT NULL)"
  -- Crear tabla «materiales«
  execute_ conn "CREATE TABLE IF NOT EXISTS materiales ( \
                \ id INTEGER PRIMARY KEY, \
                \ nombre TEXT NOT NULL, \
                \ stock NUMERIC DEFAULT 0)"
  -- Crear tabla pivote entre «mantenimientos» y «materiales»
  execute_ conn "CREATE TABLE IF NOT EXISTS mantenimientos_materiales( \
                \ mantenimiento_id INTEGER, \
                \ material_id INTEGER, \
                \ cantidad NUMERIC NOT NULL, \
                \ FOREIGN KEY (mantenimiento_id) REFERENCES mantenimientos (id), \
                \ FOREIGN KEY (material_id) REFERENCES materiales (id), \
                \ PRIMARY KEY (mantenimiento_id, material_id))"

main :: IO ()
main = do
  conn <- open "mantenimiento.sql"
  migrate conn
