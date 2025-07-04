import os
import shutil

directorio_origen = '.'
directorio_destino_root = 'AICOMP'

def mover_archivo(origen, destino):
    directorio = os.path.dirname(destino)
    if not os.path.exists(directorio):
        os.makedirs(directorio)
    shutil.move(origen, destino)
    print(f'Movido: {os.path.basename(origen)} -> {directorio}/')

for archivo in os.listdir(directorio_origen):
    ruta_archivo = os.path.join(directorio_origen, archivo)
    if not os.path.isfile(ruta_archivo):
        continue  # Solo archivos, no directorios

    # Solo procesar archivos, no directorios, y que tengan extensión .txt
    if not os.path.isfile(ruta_archivo) or not archivo.lower().endswith('.txt'):
        continue
        
    # Solo si el nombre tiene al menos 4 caracteres
    if len(archivo) < 4:
        continue

    prefijo = archivo[:4]

    # Si contiene VCXI (y no ZCL_RF ni YZCL_RF), va a VCXI
    if "VCXI" in archivo:
        destino = os.path.join(directorio_destino_root, 'VCXI', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)
    # Si contiene ZCL_RF o YZCL_RF, va a RF
    elif "ZCL_RF" in archivo or "YCL_RF" in archivo or "YCL_HP" in archivo or "YRF" in archivo or "YHP" in archivo:
        destino = os.path.join(directorio_destino_root, 'RF', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)
    elif "_AP" in archivo or "ZAP_" in archivo:
        destino = os.path.join('', 'APC', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)
    elif "EXCEL" in archivo:
        destino = os.path.join('', 'EXCEL', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)
    elif "TEST" in archivo or "PRUEBA" in archivo or "CLAS_Y_CHECK" in archivo:
        destino = os.path.join('', 'PRUEBA', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)    
    elif "IDOC" in archivo:
        destino = os.path.join('', 'IDOC', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)             
    elif "UI5" in archivo:
        destino = os.path.join('', 'UI5', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)          
    elif "ZSEI" in archivo:
        destino = os.path.join('', 'SEIDOR', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)       
    elif "ZDYN" in archivo:
        destino = os.path.join('', 'ZDYN', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)    
    elif "PROG_ZX" in archivo or "CLAS_ZCL_IM" in archivo or "BADI" in archivo:
        destino = os.path.join('', 'EXITS', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)          
    elif "SD" in archivo or "PROG_RV" in archivo:
        destino = os.path.join('', 'SD', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)          
    elif "FI" in archivo:
        destino = os.path.join('', 'FI', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)           
    elif "MM" in archivo:
        destino = os.path.join('', 'MM', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)            
    elif "WM" in archivo:
        destino = os.path.join('', 'WM', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)   
    elif "PP" in archivo:
        destino = os.path.join('', 'PP', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)   
    elif "QM" in archivo:
        destino = os.path.join('', 'QM', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)           
    elif "PM" in archivo:
        destino = os.path.join('', 'PM', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)    
    elif "SCM" in archivo:
        destino = os.path.join('', 'SCM', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)            

    # Para cualquier otro archivo, NO crear directorios ni mover