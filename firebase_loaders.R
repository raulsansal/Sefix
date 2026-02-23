# firebase_loaders.R
# Sistema de carga de datos desde Firebase Storage para Sefix Dashboard
# Versión: 2.0 - Lectura directa UTF-8 (archivos ya corregidos)
#
# Cambios v2.0:
#   - REMOVIDA toda la lógica de reparación de encoding
#   - Los archivos en Storage ya están en UTF-8 correcto
#   - Lectura directa con fread(encoding = "UTF-8")
#   - Máximo rendimiento: sin procesamiento adicional

library(httr)
library(data.table)

# ========== CONFIGURACIÓN ==========

FIREBASE_STORAGE_BASE <- "https://firebasestorage.googleapis.com/v0/b/eskemma-3c4c3.firebasestorage.app/o"

# Caché en memoria para evitar recargas
.firebase_cache <- new.env(parent = emptyenv())

# ========== FUNCIONES AUXILIARES ==========

#' @title Construir URL de Firebase Storage
#' @param path Ruta del archivo en Storage (ej: "sefix/pdln/historico/archivo.csv")
#' @return URL completa para descargar el archivo
construir_url_firebase <- function(path) {
  
  path_encoded <- gsub("/", "%2F", path)
  url <- paste0(FIREBASE_STORAGE_BASE, "/", path_encoded, "?alt=media")
  return(url)
}

#' @title Verificar si archivo existe en Firebase Storage
#' @param firebase_path Ruta del archivo en Storage
#' @return TRUE si existe, FALSE si no
archivo_existe_firebase <- function(firebase_path) {
  url <- construir_url_firebase(firebase_path)
  
  response <- tryCatch({
    HEAD(url, timeout(10))
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(response)) return(FALSE)
  return(status_code(response) == 200)
}

# ========== FUNCIÓN PRINCIPAL DE CARGA ==========

#' @title Cargar CSV desde Firebase Storage
#' @description Descarga y lee CSV desde Firebase Storage
#' @param firebase_path Ruta del archivo en Storage
#' @param usar_cache Si TRUE, usa caché en memoria para evitar recargas
#' @param verbose Si TRUE, muestra mensajes de progreso
#' @return data.table con los datos, o NULL si hay error
cargar_csv_firebase <- function(firebase_path, usar_cache = TRUE, verbose = TRUE) {
  
  # ══════════════════════════════════════════════════════════════════
  # Verificar caché
  # ══════════════════════════════════════════════════════════════════
  if (usar_cache && exists(firebase_path, envir = .firebase_cache)) {
    if (verbose) message("📦 [CACHE] Usando datos en caché: ", firebase_path)
    return(get(firebase_path, envir = .firebase_cache))
  }
  
  if (verbose) message("🌐 [FIREBASE] Descargando: ", firebase_path)
  
  tryCatch({
    url <- construir_url_firebase(firebase_path)
    
    # ══════════════════════════════════════════════════════════════════
    # Descargar archivo a temporal
    # ══════════════════════════════════════════════════════════════════
    temp_file <- tempfile(fileext = ".csv")
    
    response <- GET(
      url,
      timeout(60),
      write_disk(temp_file, overwrite = TRUE)
    )
    
    if (status_code(response) != 200) {
      if (verbose) warning("❌ Error HTTP ", status_code(response), " al descargar: ", firebase_path)
      unlink(temp_file)
      return(NULL)
    }
    
    # ══════════════════════════════════════════════════════════════════
    # Leer CSV directamente con UTF-8
    # Los archivos ya están en UTF-8 correcto (convertidos con v9)
    # ══════════════════════════════════════════════════════════════════
    datos <- fread(
      temp_file,
      encoding = "UTF-8",
      stringsAsFactors = FALSE,
      na.strings = c("", "NA"),
      strip.white = TRUE,
      showProgress = FALSE,
      blank.lines.skip = TRUE
    )
    
    # Limpiar archivo temporal
    unlink(temp_file)
    
    if (verbose) {
      message("✅ [FIREBASE] Descargado: ", firebase_path)
      message("   📊 Filas: ", format(nrow(datos), big.mark = ","), 
              " | Columnas: ", ncol(datos))
      
      # Verificar caracteres especiales (opcional, para debug)
      col_mun <- NULL
      if ("NOMBRE MUNICIPIO" %in% names(datos)) col_mun <- "NOMBRE MUNICIPIO"
      if ("nombre_municipio" %in% names(datos)) col_mun <- "nombre_municipio"
      if ("municipio" %in% names(datos)) col_mun <- "municipio"
      
      if (!is.null(col_mun)) {
        muestra <- unique(datos[[col_mun]])
        con_enie <- sum(grepl("Ñ", muestra))
        con_u <- sum(grepl("Ü", muestra))
        if (con_enie > 0 || con_u > 0) {
          message("   🔤 Encoding OK: ", con_enie, " con Ñ, ", con_u, " con Ü")
        }
      }
    }
    
    # ══════════════════════════════════════════════════════════════════
    # Guardar en caché
    # ══════════════════════════════════════════════════════════════════
    if (usar_cache) {
      assign(firebase_path, datos, envir = .firebase_cache)
    }
    
    return(datos)
    
  }, error = function(e) {
    if (verbose) warning("❌ Error al cargar ", firebase_path, ": ", e$message)
    return(NULL)
  })
}

# ========== FUNCIONES ESPECÍFICAS POR TIPO DE DATOS ==========

#' @title Cargar datos históricos de PDLN desde Firebase
#' @param fecha Fecha en formato Date o string "YYYYMMDD"
#' @param usar_cache Si TRUE, usa caché en memoria
#' @return data.table con los datos
cargar_pdln_historico_firebase <- function(fecha, usar_cache = TRUE) {
  if (inherits(fecha, "Date")) {
    fecha <- format(fecha, "%Y%m%d")
  }
  path <- paste0("sefix/pdln/historico/derfe_pdln_", fecha, "_base.csv")
  cargar_csv_firebase(path, usar_cache = usar_cache)
}

#' @title Cargar datos semanales de PDLN desde Firebase
#' @param fecha Fecha en formato Date o string "YYYYMMDD"
#' @param tipo Tipo de datos: "edad", "origen" o "sexo"
#' @param usar_cache Si TRUE, usa caché en memoria
#' @return data.table con los datos
cargar_pdln_semanal_firebase <- function(fecha, tipo = "edad", usar_cache = TRUE) {
  if (!tipo %in% c("edad", "origen", "sexo")) {
    stop("Tipo debe ser 'edad', 'origen' o 'sexo'")
  }
  if (inherits(fecha, "Date")) {
    fecha <- format(fecha, "%Y%m%d")
  }
  path <- paste0("sefix/pdln/semanal/derfe_pdln_", fecha, "_", tipo, ".csv")
  cargar_csv_firebase(path, usar_cache = usar_cache)
}

#' @title Cargar resultados electorales federales desde Firebase
#' @param year Año de la elección
#' @param cargo Tipo de cargo: "dip", "sen" o "pdte"
#' @param usar_cache Si TRUE, usa caché en memoria
#' @return data.table con los datos
cargar_resultados_federales_firebase <- function(year, cargo, usar_cache = TRUE) {
  if (!cargo %in% c("dip", "sen", "pdte")) {
    stop("Cargo debe ser 'dip', 'sen' o 'pdte'")
  }
  path <- paste0("sefix/results/federals/pef_", cargo, "_", year, ".csv")
  cargar_csv_firebase(path, usar_cache = usar_cache)
}

# ========== FUNCIONES DE UTILIDAD ==========

#' @title Listar fechas disponibles en Firebase Storage
#' @description Retorna las fechas conocidas de archivos disponibles
#' @return Lista con fechas de histórico, semanal y años electorales
listar_fechas_disponibles_firebase <- function() {
  message("ℹ️ Listado de archivos disponibles basado en estructura conocida")
  
  fechas_historico <- seq(
    from = as.Date("2017-01-31"),
    to = as.Date("2025-07-31"),
    by = "month"
  )
  
  fechas_semanal <- seq(
    from = as.Date("2025-01-02"),
    to = as.Date("2025-10-16"),
    by = "week"
  )
  
  años_electorales <- c(2006, 2009, 2012, 2015, 2018, 2021, 2023, 2024)
  
  list(
    historico = fechas_historico,
    semanal = fechas_semanal,
    años_electorales = años_electorales
  )
}

#' @title Limpiar caché de Firebase
#' @description Elimina todos los datos cacheados en memoria
limpiar_cache_firebase <- function() {
  rm(list = ls(envir = .firebase_cache), envir = .firebase_cache)
  message("🗑️ Caché de Firebase limpiado")
}

#' @title Obtener tamaño del caché
#' @description Retorna el número de archivos en caché
#' @return Número de archivos cacheados
tamaño_cache_firebase <- function() {
  length(ls(envir = .firebase_cache))
}

#' @title Obtener uso de memoria del caché
#' @description Retorna el tamaño aproximado en MB del caché
#' @return Tamaño en MB
memoria_cache_firebase <- function() {
  if (tamaño_cache_firebase() == 0) return(0)
  
  
  total_bytes <- sum(sapply(ls(envir = .firebase_cache), function(x) {
    object.size(get(x, envir = .firebase_cache))
  }))
  
  round(total_bytes / 1024 / 1024, 2)
}

# ========== MENSAJE DE INICIALIZACIÓN ==========

message("✅ firebase_loaders.R v2.0 cargado")
message("   📦 Sistema de caché inicializado")
message("   🔤 Lectura directa UTF-8 (archivos ya corregidos)")
