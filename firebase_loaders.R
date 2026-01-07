# firebase_loaders.R
# Sistema de carga de datos desde Firebase Storage para Sefix Dashboard
# Versión: 1.1 - BUCKET CORRECTO

library(httr)
library(readr)
library(data.table)

# ========== CONFIGURACIÓN - BUCKET CORRECTO ==========

# ✅ URL base con el bucket NUEVO (no el legacy)
FIREBASE_STORAGE_BASE <- "https://firebasestorage.googleapis.com/v0/b/eskemma-3c4c3.firebasestorage.app/o"

# Caché en memoria para evitar recargas
.firebase_cache <- new.env(parent = emptyenv())

#' @title Construir URL de Firebase Storage
construir_url_firebase <- function(path) {
  # Codificar path para URL (reemplazar / con %2F)
  path_encoded <- gsub("/", "%2F", path)
  
  # Construir URL completa
  url <- paste0(FIREBASE_STORAGE_BASE, "/", path_encoded, "?alt=media")
  
  return(url)
}

#' @title Verificar si archivo existe en Firebase Storage
#' @param firebase_path Ruta del archivo en Firebase Storage
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

#' @title Cargar CSV desde Firebase Storage
#' @description Descarga y lee CSV desde Firebase con caché opcional
#' @param firebase_path Ruta del archivo en Firebase Storage
#' @param usar_cache Lógico, si usar caché en memoria
#' @param verbose Lógico, si mostrar mensajes de progreso
#' @return data.table con los datos o NULL si falla
cargar_csv_firebase <- function(firebase_path, usar_cache = TRUE, verbose = TRUE) {
  
  # Verificar caché
  if (usar_cache && exists(firebase_path, envir = .firebase_cache)) {
    if (verbose) message("📦 [CACHE] Usando datos en caché: ", firebase_path)
    return(get(firebase_path, envir = .firebase_cache))
  }
  
  if (verbose) message("🌐 [FIREBASE] Descargando: ", firebase_path)
  
  tryCatch({
    url <- construir_url_firebase(firebase_path)
    
    # Descargar archivo a temporal
    temp_file <- tempfile(fileext = ".csv")
    
    response <- GET(
      url,
      timeout(60),  # 60 segundos timeout
      write_disk(temp_file, overwrite = TRUE)
    )
    
    if (status_code(response) != 200) {
      if (verbose) warning("❌ Error HTTP ", status_code(response), " al descargar: ", firebase_path)
      return(NULL)
    }
    
    # Leer CSV con fread (más rápido que read_csv)
    datos <- fread(
      temp_file,
      encoding = "Latin-1",
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
    }
    
    # Guardar en caché
    if (usar_cache) {
      assign(firebase_path, datos, envir = .firebase_cache)
    }
    
    return(datos)
    
  }, error = function(e) {
    if (verbose) warning("❌ Error al cargar ", firebase_path, ": ", e$message)
    return(NULL)
  })
}

#' @title Cargar datos históricos de PDLN desde Firebase
#' @param fecha Fecha en formato "YYYYMMDD" (ej: "20170131") o Date
#' @param usar_cache Lógico, si usar caché
#' @return data.table con datos históricos
cargar_pdln_historico_firebase <- function(fecha, usar_cache = TRUE) {
  
  # Convertir Date a string si es necesario
  if (inherits(fecha, "Date")) {
    fecha <- format(fecha, "%Y%m%d")
  }
  
  path <- paste0("sefix/pdln/historico/derfe_pdln_", fecha, "_base.csv")
  cargar_csv_firebase(path, usar_cache = usar_cache)
}

#' @title Cargar datos semanales de PDLN desde Firebase
#' @param fecha Fecha en formato "YYYYMMDD" o Date
#' @param tipo Tipo de datos: "edad", "origen", o "sexo"
#' @param usar_cache Lógico, si usar caché
#' @return data.table con datos semanales
cargar_pdln_semanal_firebase <- function(fecha, tipo = "edad", usar_cache = TRUE) {
  
  # Validar tipo
  if (!tipo %in% c("edad", "origen", "sexo")) {
    stop("Tipo debe ser 'edad', 'origen' o 'sexo'")
  }
  
  # Convertir Date a string si es necesario
  if (inherits(fecha, "Date")) {
    fecha <- format(fecha, "%Y%m%d")
  }
  
  path <- paste0("sefix/pdln/semanal/derfe_pdln_", fecha, "_", tipo, ".csv")
  cargar_csv_firebase(path, usar_cache = usar_cache)
}

#' @title Cargar resultados electorales federales desde Firebase
#' @param year Año electoral (ej: 2024)
#' @param cargo Tipo de cargo: "dip", "sen", "pdte"
#' @param usar_cache Lógico, si usar caché
#' @return data.table con resultados electorales
cargar_resultados_federales_firebase <- function(year, cargo, usar_cache = TRUE) {
  
  # Validar cargo
  if (!cargo %in% c("dip", "sen", "pdte")) {
    stop("Cargo debe ser 'dip', 'sen' o 'pdte'")
  }
  
  path <- paste0("sefix/results/federals/pef_", cargo, "_", year, ".csv")
  cargar_csv_firebase(path, usar_cache = usar_cache)
}

#' @title Listar archivos disponibles en Firebase Storage (simulado)
#' @description Como no tenemos acceso directo al listado, retornamos fechas conocidas
#' @return Lista con fechas disponibles
listar_fechas_disponibles_firebase <- function() {
  
  message("ℹ️ Listado de archivos disponibles basado en estructura conocida")
  
  # Fechas históricas conocidas (2017-2025)
  fechas_historico <- seq(
    from = as.Date("2017-01-31"),
    to = as.Date("2025-07-31"),
    by = "month"
  )
  
  # Fechas semanales conocidas (2025)
  fechas_semanal <- seq(
    from = as.Date("2025-01-02"),
    to = as.Date("2025-10-16"),
    by = "week"
  )
  
  # Años electorales conocidos
  años_electorales <- c(2006, 2009, 2012, 2015, 2018, 2021, 2023, 2024)
  
  list(
    historico = fechas_historico,
    semanal = fechas_semanal,
    años_electorales = años_electorales
  )
}

#' @title Limpiar caché de Firebase
#' @description Elimina todos los datos en caché
limpiar_cache_firebase <- function() {
  rm(list = ls(envir = .firebase_cache), envir = .firebase_cache)
  message("🗑️ Caché de Firebase limpiado")
}

#' @title Obtener tamaño del caché
#' @return Número de objetos en caché
tamaño_cache_firebase <- function() {
  length(ls(envir = .firebase_cache))
}

message("✅ firebase_loaders.R cargado")
message("   📦 Sistema de caché inicializado")
message("   🌐 URL base: ", FIREBASE_STORAGE_BASE)
