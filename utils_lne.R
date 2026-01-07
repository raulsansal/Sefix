# utils_lne.R
# Funciones utilitarias para manejo de datos de Lista Nominal Electoral (LNE)
# Versión: 2.0 - FIREBASE STORAGE

library(data.table)

# Cargar sistema de Firebase
if (file.exists("firebase_loaders.R")) {
  source("firebase_loaders.R")
  message("✅ firebase_loaders.R cargado en utils_lne.R")
} else {
  stop("❌ No se encontró firebase_loaders.R")
}

#' @title Generar catálogo de fechas disponibles desde Firebase
#' @description Genera listas de fechas disponibles por tipo
#' @return Lista con fechas históricas y semanales
build_lne_catalog <- function() {
  
  message("🔍 Construyendo catálogo LNE desde Firebase Storage...")
  
  # Obtener fechas disponibles
  fechas <- listar_fechas_disponibles_firebase()
  
  catalog <- list(
    historico = fechas$historico,
    semanal = list(
      edad = fechas$semanal,
      origen = fechas$semanal,
      sexo = fechas$semanal
    ),
    semanal_comun = fechas$semanal
  )
  
  message("📅 Fechas históricas: ", length(catalog$historico))
  message("📅 Fechas semanales: ", length(catalog$semanal_comun))
  
  return(catalog)
}

#' @title Crear catálogo de mapeo geográfico desde archivo semanal
#' @description Carga un archivo semanal reciente y extrae el mapeo de claves a nombres
#' @return Lista con mapeos: distritos (clave → nombre) y municipios (clave → nombre)
crear_catalogo_geografico_lne <- function() {
  message("🗺️ Creando catálogo geográfico desde Firebase Storage...")
  
  # Usar archivo semanal más reciente (origen tiene toda la info geográfica)
  fecha_reciente <- "20250206"  # Actualizar según necesidad
  
  # Cargar desde Firebase
  dt <- cargar_pdln_semanal_firebase(fecha_reciente, tipo = "origen")
  
  if (is.null(dt) || nrow(dt) == 0) {
    message("❌ No se pudo cargar archivo para catálogo geográfico")
    return(list(distritos = character(0), municipios = character(0)))
  }
  
  # Normalizar columnas
  dt <- normalizar_columnas_semanal(dt)
  
  # Crear mapeos
  mapeo_distritos <- character(0)
  mapeo_municipios <- character(0)
  
  # Mapeo de distritos
  if (all(c("clave_entidad", "clave_distrito", "cabecera_distrital") %in% colnames(dt))) {
    dt_distritos <- unique(dt[!is.na(cabecera_distrital) & cabecera_distrital != "", 
                              .(clave_entidad, clave_distrito, cabecera_distrital)])
    
    dt_distritos[, clave_compuesta := paste(clave_entidad, clave_distrito, sep = "_")]
    
    mapeo_distritos <- setNames(
      dt_distritos$cabecera_distrital,
      dt_distritos$clave_compuesta
    )
    
    message("✅ Mapeo distritos creado: ", length(mapeo_distritos), " registros")
  }
  
  # Mapeo de municipios
  if (all(c("clave_entidad", "clave_municipio", "nombre_municipio") %in% colnames(dt))) {
    dt_municipios <- unique(dt[!is.na(nombre_municipio) & nombre_municipio != "", 
                               .(clave_entidad, clave_municipio, nombre_municipio)])
    
    dt_municipios[, clave_compuesta := paste(clave_entidad, clave_municipio, sep = "_")]
    
    mapeo_municipios <- setNames(
      dt_municipios$nombre_municipio,
      dt_municipios$clave_compuesta
    )
    
    message("✅ Mapeo municipios creado: ", length(mapeo_municipios), " registros")
  }
  
  catalogo <- list(
    distritos = mapeo_distritos,
    municipios = mapeo_municipios
  )
  
  message("✅ Catálogo geográfico completo")
  return(catalogo)
}

#' @title Normalizar nombres de columnas de archivo histórico
#' @param dt data.table con columnas originales
#' @return data.table con columnas normalizadas
normalizar_columnas_historico <- function(dt) {
  
  cols_originales <- colnames(dt)
  
  tryCatch({
    # Limpiar nombres de columnas
    cols_limpios <- gsub("\n", " ", cols_originales)
    cols_limpios <- gsub("\\s+", " ", cols_limpios)
    cols_limpios <- trimws(cols_limpios)
    setnames(dt, old = cols_originales, new = cols_limpios)
    
    # Renombrar claves geográficas
    if ("CLAVE ENTIDAD" %in% colnames(dt)) {
      setnames(dt, "CLAVE ENTIDAD", "clave_entidad", skip_absent = TRUE)
    }
    
    if ("CLAVE DISTRITO" %in% colnames(dt)) {
      setnames(dt, "CLAVE DISTRITO", "clave_distrito", skip_absent = TRUE)
    }
    
    if ("CLAVE MUNICIPIO" %in% colnames(dt)) {
      setnames(dt, "CLAVE MUNICIPIO", "clave_municipio", skip_absent = TRUE)
    }
    
    if ("SECCION" %in% colnames(dt)) {
      setnames(dt, "SECCION", "seccion", skip_absent = TRUE)
    }
    
  }, error = function(e) {
    message("⚠️ Error al normalizar columnas históricas: ", e$message)
  })
  
  return(dt)
}

#' @title Normalizar nombres de columnas de archivo semanal
#' @param dt data.table con columnas originales
#' @return data.table con columnas normalizadas
normalizar_columnas_semanal <- function(dt) {
  
  # Si solo hay 1 columna, re-parsear
  if (ncol(dt) == 1) {
    message("⚠️ Solo 1 columna detectada, intentando re-parsear con tabulador")
    
    primera_col <- names(dt)[1]
    
    if (grepl("\t", primera_col)) {
      message("✅ Detectados tabuladores, re-parseando...")
      
      headers <- strsplit(primera_col, "\t")[[1]]
      headers <- trimws(headers)
      
      filas_parseadas <- lapply(1:nrow(dt), function(i) {
        valores <- strsplit(as.character(dt[[primera_col]][i]), "\t")[[1]]
        valores <- trimws(valores)
        
        if (length(valores) < length(headers)) {
          valores <- c(valores, rep(NA, length(headers) - length(valores)))
        } else if (length(valores) > length(headers)) {
          valores <- valores[1:length(headers)]
        }
        
        return(valores)
      })
      
      dt_nuevo <- data.table(do.call(rbind, filas_parseadas))
      colnames(dt_nuevo) <- headers
      
      message("✅ Re-parseado exitoso: ", ncol(dt_nuevo), " columnas")
      dt <- dt_nuevo
    }
  }
  
  # Normalizar nombres
  cols_originales <- colnames(dt)
  
  cols_nuevas <- gsub("\n", " ", cols_originales, fixed = TRUE)
  cols_nuevas <- gsub("\r", "", cols_nuevas, fixed = TRUE)
  cols_nuevas <- gsub("\\s+", " ", cols_nuevas)
  cols_nuevas <- trimws(cols_nuevas)
  
  setnames(dt, cols_originales, cols_nuevas, skip_absent = TRUE)
  
  # Mapeo de nombres estándar
  mapeo_nombres <- c(
    "CLAVE ENTIDAD" = "clave_entidad",
    "NOMBRE ENTIDAD" = "nombre_entidad",
    "CLAVE DISTRITO" = "clave_distrito",
    "CABECERA DISTRITAL" = "cabecera_distrital",
    "CLAVE MUNICIPIO" = "clave_municipio",
    "NOMBRE MUNICIPIO" = "nombre_municipio",
    "SECCION" = "seccion",
    "PADRON HOMBRES" = "padron_hombres",
    "PADRON MUJERES" = "padron_mujeres",
    "PADRON NO BINARIO" = "padron_no_binario",
    "PADRON ELECTORAL" = "padron_electoral",
    "LISTA HOMBRES" = "lista_hombres",
    "LISTA MUJERES" = "lista_mujeres",
    "LISTA NO BINARIO" = "lista_no_binario",
    "LISTA NOMINAL" = "lista_nominal"
  )
  
  for (viejo in names(mapeo_nombres)) {
    if (viejo %in% colnames(dt)) {
      setnames(dt, viejo, mapeo_nombres[[viejo]], skip_absent = TRUE)
    }
  }
  
  return(dt)
}

#' @title Catálogo de entidades federativas
#' @return Named vector con clave como names y nombre como values
catalogo_entidades <- function() {
  c(
    "1" = "AGUASCALIENTES",
    "2" = "BAJA CALIFORNIA",
    "3" = "BAJA CALIFORNIA SUR",
    "4" = "CAMPECHE",
    "5" = "COAHUILA",
    "6" = "COLIMA",
    "7" = "CHIAPAS",
    "8" = "CHIHUAHUA",
    "9" = "CIUDAD DE MEXICO",
    "10" = "DURANGO",
    "11" = "GUANAJUATO",
    "12" = "GUERRERO",
    "13" = "HIDALGO",
    "14" = "JALISCO",
    "15" = "MEXICO",
    "16" = "MICHOACAN",
    "17" = "MORELOS",
    "18" = "NAYARIT",
    "19" = "NUEVO LEON",
    "20" = "OAXACA",
    "21" = "PUEBLA",
    "22" = "QUERETARO",
    "23" = "QUINTANA ROO",
    "24" = "SAN LUIS POTOSI",
    "25" = "SINALOA",
    "26" = "SONORA",
    "27" = "TABASCO",
    "28" = "TAMAULIPAS",
    "29" = "TLAXCALA",
    "30" = "VERACRUZ",
    "31" = "YUCATAN",
    "32" = "ZACATECAS",
    "0" = "RESIDENTES EXTRANJERO"
  )
}

#' @title Limpiar datos especiales (TOTALES y RESIDENTES EXTRANJERO)
#' @param dt data.table con datos LNE
#' @param incluir_extranjero Lógico, si incluir residentes en el extranjero
#' @param incluir_totales Lógico, si incluir fila de totales
#' @return data.table limpio
limpiar_filas_especiales <- function(dt, incluir_extranjero = TRUE, incluir_totales = FALSE) {
  
  if (!incluir_totales) {
    cols_texto <- sapply(dt, is.character)
    if (any(cols_texto)) {
      filas_totales <- apply(dt[, ..cols_texto], 1, function(row) any(grepl("TOTALES", row, ignore.case = TRUE)))
      if (any(filas_totales)) {
        dt <- dt[!filas_totales]
        message("🧹 Fila de TOTALES removida")
      }
    }
  }
  
  if (!incluir_extranjero) {
    if ("clave_entidad" %in% colnames(dt) && "clave_distrito" %in% colnames(dt)) {
      dt <- dt[!(clave_entidad %in% c("0", "1") & clave_distrito == "0")]
      message("🧹 Residentes EXTRANJERO removidos")
    }
  }
  
  return(dt)
}

#' @title Validar integridad de datos cargados
#' @param dt data.table con datos LNE
#' @param tipo "historico" o "semanal"
#' @return Lista con resultado de validación
validar_datos_lne <- function(dt, tipo = "historico") {
  resultado <- list(valido = TRUE, mensajes = character(0))
  
  if (nrow(dt) == 0) {
    resultado$valido <- FALSE
    resultado$mensajes <- c(resultado$mensajes, "❌ Data.table vacío")
    return(resultado)
  }
  
  cols_geograficas <- c("clave_entidad", "clave_distrito", "clave_municipio", "seccion")
  faltantes <- setdiff(cols_geograficas, colnames(dt))
  if (length(faltantes) > 0) {
    resultado$valido <- FALSE
    resultado$mensajes <- c(resultado$mensajes, paste("❌ Faltan columnas:", paste(faltantes, collapse = ", ")))
  }
  
  if (resultado$valido) {
    resultado$mensajes <- c(resultado$mensajes, "✅ Validación exitosa")
  }
  
  return(resultado)
}

message("✅ utils_lne.R v2.0 cargado (Firebase Storage)")
