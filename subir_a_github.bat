@echo off
:: Script para subir un directorio a GitHub en Windows 11

:: Solicitar datos al usuario
set /p USERNAME=Introduce tu nombre de usuario de GitHub:
set /p REPO=Introduce el nombre del repositorio:
set /p COMMIT_MSG=Introduce el mensaje del commit:

:: Inicializar repositorio Git si no existe
if not exist ".git" (
    git init
)

:: Agregar todos los archivos
git add .

:: Hacer commit
git commit -m "%COMMIT_MSG%"

:: Agregar repositorio remoto
git remote remove origin 2>nul
git remote add origin https://github.com/%USERNAME%/%REPO%.git

:: Subir a la rama main
git branch -M main
git push -u origin main

pause
