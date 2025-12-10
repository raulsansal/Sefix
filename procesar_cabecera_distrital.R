# Script para procesar archivos CSV y agregar prefijo a cabecera_distrital
# Autor: Procesamiento de datos electorales INE
# Descripción: Agrega un prefijo formado por cve_entidad y cve_distrito 
#              a la columna cabecera_distrital

# Instalar/cargar librerías necesarias
if (!require("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}

# ============================================================================
# CONFIGURACIÓN INICIAL
# ============================================================================

# Especificar la ruta de la carpeta con los archivos CSV
ruta_entrada <- "dataSin/"

# Especificar la ruta donde guardar los archivos procesados
ruta_salida <- "dataSin_procesados/"

# Crear carpeta de salida si no existe
if (!dir.exists(ruta_salida)) {
  dir.create(ruta_salida, recursive = TRUE)
  cat("✓ Carpeta de salida creada:", ruta_salida, "\n\n")
}

# ============================================================================
# FUNCIÓN PRINCIPAL
# ============================================================================

procesar_archivo_csv <- function(ruta_archivo) {
  
  # Leer el archivo CSV
  cat("Procesando:", basename(ruta_archivo), "\n")
  
  datos <- read_csv(
    ruta_archivo,
    col_types = cols(.default = "c"),  # Leer todas como character inicialmente
    show_col_types = FALSE
  )
  
  # Verificar que existan las columnas necesarias
  cols_requeridas <- c("cve_entidad", "cve_distrito", "cabecera_distrital")
  if (!all(cols_requeridas %in% names(datos))) {
    stop("El archivo no contiene todas las columnas requeridas: ",
         paste(cols_requeridas, collapse = ", "))
  }
  
  # Convertir columnas numéricas a numeric para formateo
  # Usar suppressWarnings para evitar advertencias de NAs en coerción
  # y trimws() para limpiar espacios en blanco
  datos <- datos %>%
    mutate(
      cve_entidad = suppressWarnings(as.numeric(trimws(cve_entidad))),
      cve_distrito = suppressWarnings(as.numeric(trimws(cve_distrito))),
      # Crear el prefijo con formato de 2 dígitos cada uno
      prefijo = paste0(
        sprintf("%02d", cve_entidad),
        sprintf("%02d", cve_distrito)
      ),
      # Agregar el prefijo a cabecera_distrital, separado por espacio
      cabecera_distrital = paste(prefijo, cabecera_distrital, sep = " ")
    ) %>%
    # Eliminar la columna temporal 'prefijo'
    select(-prefijo)
  
  return(datos)
}

# ============================================================================
# PROCESAMIENTO DE TODOS LOS ARCHIVOS
# ============================================================================

# Obtener lista de archivos CSV en la carpeta
archivos_csv <- list.files(
  path = ruta_entrada,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos_csv) == 0) {
  cat("⚠ No se encontraron archivos CSV en la carpeta:", ruta_entrada, "\n")
  cat("Verifica que la carpeta existe y contiene archivos .csv\n")
  stop("Ningún archivo para procesar")
}

cat("Se encontraron", length(archivos_csv), "archivo(s) CSV\n\n")

# Procesar cada archivo
for (archivo in archivos_csv) {
  tryCatch({
    # Procesar el archivo
    datos_procesados <- procesar_archivo_csv(archivo)
    
    # Generar nombre del archivo de salida
    nombre_archivo <- basename(archivo)
    ruta_salida_archivo <- file.path(ruta_salida, nombre_archivo)
    
    # Guardar el archivo procesado
    write_csv(datos_procesados, ruta_salida_archivo)
    
    cat("  ✓ Guardado en:", ruta_salida_archivo, "\n")
    cat("    Filas procesadas:", nrow(datos_procesados), "\n\n")
    
  }, error = function(e) {
    cat("  ✗ Error al procesar:", basename(archivo), "\n")
    cat("    Detalle:", e$message, "\n\n")
  })
}

cat(strrep("=", 70), "\n")
cat("✓ Procesamiento completado\n")
cat("Los archivos procesados están en:", ruta_salida, "\n")
cat(strrep("=", 70), "\n")