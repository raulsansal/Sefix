# server/datos_lne.R
# Sistema de carga de datos de Lista Nominal Electoral (LNE)
# Versión: 1.4 - CORRECCIÓN CRÍTICA: Buscar fila totales con cve_entidad = NA

library(data.table)
library(dplyr)

# ========== FUNCIÓN: ESCANEAR ARCHIVOS DISPONIBLES ==========

escanear_archivos_lne <- function() {
  
  # Rutas de carpetas
  ruta_historico <- file.path("data", "pdln", "historico")
  ruta_semanal <- file.path("data", "pdln", "semanal")
  
  # Inicializar catálogo
  catalogo <- list(
    historico = character(0),
    historico_fechas = as.Date(character(0)),
    semanal_comun = character(0),
    semanal_comun_fechas = as.Date(character(0)),
    semanal_sexo = character(0),
    semanal_edad = character(0),
    semanal_origen = character(0)
  )
  
  # ========== ESCANEAR ARCHIVOS HISTÓRICOS ==========
  if (dir.exists(ruta_historico)) {
    archivos_historico <- list.files(ruta_historico, pattern = "\\.csv$", full.names = FALSE)
    
    if (length(archivos_historico) > 0) {
      # Extraer fechas de nombres como: derfe_pdln_20250731_base.csv
      fechas_extraidas <- gsub(".*_(\\d{8})_.*\\.csv", "\\1", archivos_historico)
      
      # Convertir a formato Date
      fechas_date <- as.Date(fechas_extraidas, format = "%Y%m%d")
      
      # Filtrar fechas válidas
      fechas_validas <- !is.na(fechas_date)
      
      catalogo$historico <- archivos_historico[fechas_validas]
      catalogo$historico_fechas <- fechas_date[fechas_validas]
      
      message("📂 Archivos históricos encontrados: ", length(catalogo$historico))
      if (length(catalogo$historico) > 0) {
        message("   Última fecha: ", max(catalogo$historico_fechas))
      }
    } else {
      message("⚠️ No se encontraron archivos en ", ruta_historico)
    }
  } else {
    warning("❌ Carpeta no encontrada: ", ruta_historico)
  }
  
  # ========== ESCANEAR ARCHIVOS SEMANALES ==========
  if (dir.exists(ruta_semanal)) {
    archivos_semanal <- list.files(ruta_semanal, pattern = "\\.csv$", full.names = FALSE)
    
    if (length(archivos_semanal) > 0) {
      # Separar por tipo
      catalogo$semanal_comun <- grep("comun|completo", archivos_semanal, value = TRUE, ignore.case = TRUE)
      catalogo$semanal_sexo <- grep("sexo", archivos_semanal, value = TRUE, ignore.case = TRUE)
      catalogo$semanal_edad <- grep("edad", archivos_semanal, value = TRUE, ignore.case = TRUE)
      catalogo$semanal_origen <- grep("origen", archivos_semanal, value = TRUE, ignore.case = TRUE)
      
      # Extraer fechas de archivos comunes
      if (length(catalogo$semanal_comun) > 0) {
        fechas_semanal <- gsub(".*_(\\d{8})_.*\\.csv", "\\1", catalogo$semanal_comun)
        fechas_semanal_date <- as.Date(fechas_semanal, format = "%Y%m%d")
        catalogo$semanal_comun_fechas <- fechas_semanal_date[!is.na(fechas_semanal_date)]
      }
      
      message("📂 Archivos semanales encontrados: ", 
              length(catalogo$semanal_comun), " comunes, ",
              length(catalogo$semanal_sexo), " sexo, ",
              length(catalogo$semanal_edad), " edad, ",
              length(catalogo$semanal_origen), " origen")
    }
  } else {
    message("⚠️ Carpeta no encontrada: ", ruta_semanal)
  }
  
  return(catalogo)
}

# ========== CATÁLOGO GLOBAL DE ARCHIVOS DISPONIBLES ==========

if (!exists("LNE_CATALOG")) {
  message("🔍 Escaneando archivos LNE disponibles...")
  LNE_CATALOG <- escanear_archivos_lne()
  
  # Crear versión simplificada para compatibilidad
  LNE_CATALOG$historico <- LNE_CATALOG$historico_fechas
  LNE_CATALOG$semanal_comun <- LNE_CATALOG$semanal_comun_fechas
  
  message("✅ Catálogo LNE inicializado: ", 
          length(LNE_CATALOG$historico), " históricos, ",
          length(LNE_CATALOG$semanal_comun), " semanales")
}

# ========== MAPEO DE ENTIDADES ==========

entidades <- c(
  "01" = "AGUASCALIENTES", "02" = "BAJA CALIFORNIA", "03" = "BAJA CALIFORNIA SUR",
  "04" = "CAMPECHE", "05" = "COAHUILA", "06" = "COLIMA", "07" = "CHIAPAS",
  "08" = "CHIHUAHUA", "09" = "CIUDAD DE MEXICO", "10" = "DURANGO",
  "11" = "GUANAJUATO", "12" = "GUERRERO", "13" = "HIDALGO", "14" = "JALISCO",
  "15" = "MEXICO", "16" = "MICHOACAN", "17" = "MORELOS", "18" = "NAYARIT",
  "19" = "NUEVO LEON", "20" = "OAXACA", "21" = "PUEBLA", "22" = "QUERETARO",
  "23" = "QUINTANA ROO", "24" = "SAN LUIS POTOSI", "25" = "SINALOA",
  "26" = "SONORA", "27" = "TABASCO", "28" = "TAMAULIPAS", "29" = "TLAXCALA",
  "30" = "VERACRUZ", "31" = "YUCATAN", "32" = "ZACATECAS", "33" = "EXTRANJERO"
)

# ========== FUNCIÓN AUXILIAR: ENCONTRAR ARCHIVO POR FECHA ==========

encontrar_archivo_lne <- function(tipo_corte, fecha, dimension = "completo") {
  
  fecha <- as.Date(fecha)
  fecha_str <- format(fecha, "%Y%m%d")
  
  if (tipo_corte == "historico") {
    ruta_carpeta <- file.path("data", "pdln", "historico")
    
    # Buscar archivo que contenga la fecha
    archivos <- list.files(ruta_carpeta, pattern = paste0(".*", fecha_str, ".*\\.csv$"), full.names = TRUE)
    
    if (length(archivos) > 0) {
      message("✅ [encontrar_archivo_lne] Archivo encontrado: ", basename(archivos[1]))
      return(archivos[1])
    } else {
      message("❌ [encontrar_archivo_lne] No se encontró archivo para fecha: ", fecha)
      return(NULL)
    }
    
  } else if (tipo_corte == "semanal") {
    ruta_carpeta <- file.path("data", "pdln", "semanal")
    
    # Determinar patrón según dimensión
    patron_dimension <- switch(dimension,
                               "completo" = "(comun|completo)",
                               "sexo" = "sexo",
                               "edad" = "edad",
                               "origen" = "origen",
                               "comun")
    
    # Buscar archivo que contenga la fecha y dimensión
    archivos <- list.files(ruta_carpeta, 
                           pattern = paste0(".*", patron_dimension, ".*", fecha_str, ".*\\.csv$"), 
                           full.names = TRUE, ignore.case = TRUE)
    
    if (length(archivos) > 0) {
      message("✅ [encontrar_archivo_lne] Archivo encontrado: ", basename(archivos[1]))
      return(archivos[1])
    } else {
      message("❌ [encontrar_archivo_lne] No se encontró archivo para fecha: ", fecha, ", dimensión: ", dimension)
      return(NULL)
    }
    
  } else {
    stop("Tipo de corte no válido: ", tipo_corte)
  }
}

# ========== FUNCIÓN PRINCIPAL: CARGAR LNE ==========

cargar_lne <- function(tipo_corte, fecha, dimension = "completo", 
                       estado = "Nacional", distrito = "Todos", 
                       municipio = "Todos", seccion = "Todas",
                       incluir_extranjero = TRUE) {
  
  inicio_total <- Sys.time()
  
  # Validar fecha
  fecha <- as.Date(fecha)
  if (is.na(fecha)) {
    stop("Fecha inválida: ", fecha)
  }
  
  # Detectar año del archivo
  año_archivo <- as.integer(format(fecha, "%Y"))
  
  # Encontrar archivo por fecha
  ruta_archivo <- encontrar_archivo_lne(tipo_corte, fecha, dimension)
  
  if (is.null(ruta_archivo)) {
    message("❌ [cargar_lne] No se pudo encontrar archivo para fecha: ", fecha)
    return(NULL)
  }
  
  message("📂 [cargar_lne] Cargando: ", ruta_archivo)
  
  # ========== LEER ARCHIVO CSV CON FREAD ==========
  inicio_lectura <- Sys.time()
  
  dt <- tryCatch({
    fread(
      ruta_archivo,
      encoding = "Latin-1",
      stringsAsFactors = FALSE,
      na.strings = c("", "NA"),
      strip.white = TRUE,
      showProgress = FALSE,
      blank.lines.skip = TRUE
    )
  }, error = function(e) {
    message("❌ [cargar_lne] Error leyendo CSV con fread: ", e$message)
    message("   Intentando con read.csv...")
    
    tryCatch({
      df_temp <- read.csv(
        ruta_archivo,
        stringsAsFactors = FALSE,
        colClasses = "character",
        fileEncoding = "Latin-1",
        check.names = FALSE,
        strip.white = TRUE,
        na.strings = c("", "NA")
      )
      as.data.table(df_temp)
    }, error = function(e2) {
      message("❌ [cargar_lne] Error leyendo CSV con read.csv: ", e2$message)
      return(NULL)
    })
  })
  
  if (is.null(dt) || nrow(dt) == 0) {
    message("❌ [cargar_lne] CSV vacío o NULL")
    return(NULL)
  }
  
  message("📊 [cargar_lne] Filas leídas: ", format(nrow(dt), big.mark = ","))
  message("📊 [cargar_lne] Columnas leídas: ", ncol(dt))
  
  if (ncol(dt) <= 1) {
    message("⚠️ [cargar_lne] ADVERTENCIA: Solo ", ncol(dt), " columna(s) detectada(s)")
    message("   Esto indica un problema con el delimitador")
  }
  
  # ========== ✅ v1.4: EXTRAER FILA TOTALES ANTES DE NORMALIZAR (BUSCAR NA) ==========
  
  fila_totales_raw <- NULL
  idx_totales <- NULL
  
  # Primera columna (antes de normalizar)
  primera_columna <- dt[[1]]
  
  # MÉTODO 1: Buscar "TOTALES" (compatibilidad con archivos antiguos)
  idx_totales_texto <- which(grepl("^TOTALES$", primera_columna, ignore.case = TRUE))
  
  # MÉTODO 2: Buscar fila con cve_entidad = NA (archivos actuales del INE)
  idx_totales_na <- which(is.na(primera_columna))
  
  if (length(idx_totales_na) > 0) {
    # Usar fila con NA (método preferido)
    if (length(idx_totales_na) > 1) {
      message("⚠️ Encontradas ", length(idx_totales_na), " filas con NA, usando la última")
      idx_totales <- idx_totales_na[length(idx_totales_na)]
    } else {
      idx_totales <- idx_totales_na[1]
    }
    
    fila_totales_raw <- as.list(dt[idx_totales, ])
    message("✅ [ANTES NORMALIZAR] Fila TOTALES extraída en posición ", idx_totales, " (método: NA)")
    message("   📊 Total columnas en fila: ", length(fila_totales_raw))
    
    # Eliminar fila de totales del dataset
    dt <- dt[-idx_totales, ]
    message("🗑️ [ANTES NORMALIZAR] Fila TOTALES eliminada - Quedan ", nrow(dt), " filas")
    
  } else if (length(idx_totales_texto) > 0) {
    # Usar fila con "TOTALES" (compatibilidad)
    if (length(idx_totales_texto) > 1) {
      message("⚠️ Encontradas ", length(idx_totales_texto), " filas TOTALES, usando la última")
      idx_totales <- idx_totales_texto[length(idx_totales_texto)]
    } else {
      idx_totales <- idx_totales_texto[1]
    }
    
    fila_totales_raw <- as.list(dt[idx_totales, ])
    message("✅ [ANTES NORMALIZAR] Fila TOTALES extraída en posición ", idx_totales, " (método: texto)")
    message("   📊 Total columnas en fila: ", length(fila_totales_raw))
    
    # Eliminar fila de totales del dataset
    dt <- dt[-idx_totales, ]
    message("🗑️ [ANTES NORMALIZAR] Fila TOTALES eliminada - Quedan ", nrow(dt), " filas")
    
  } else {
    message("⚠️ [ANTES NORMALIZAR] No se encontró fila TOTALES (ni NA ni texto)")
  }
  
  tiempo_lectura <- round(difftime(Sys.time(), inicio_lectura, units = "secs"), 2)
  message("⏱️ Lectura: ", tiempo_lectura, " seg")
  
  # ========== NORMALIZAR COLUMNAS ==========
  inicio_proceso <- Sys.time()
  
  # Normalizar nombres
  colnames(dt) <- tolower(colnames(dt))
  colnames(dt) <- gsub("\\s+", "_", colnames(dt))
  colnames(dt) <- gsub("[áàäâ]", "a", colnames(dt))
  colnames(dt) <- gsub("[éèëê]", "e", colnames(dt))
  colnames(dt) <- gsub("[íìïî]", "i", colnames(dt))
  colnames(dt) <- gsub("[óòöô]", "o", colnames(dt))
  colnames(dt) <- gsub("[úùüû]", "u", colnames(dt))
  colnames(dt) <- gsub("ñ", "n", colnames(dt))
  
  message("📋 [cargar_lne] Columnas normalizadas (primeras 10): ", paste(head(colnames(dt), 10), collapse = ", "))
  
  # Renombrar columnas clave (cve_ → clave_)
  col_map <- c(
    "cve_entidad" = "clave_entidad",
    "cve_distrito" = "clave_distrito",
    "cve_municipio" = "clave_municipio",
    "cve_seccion" = "seccion"
  )
  
  for (col_viejo in names(col_map)) {
    col_nuevo <- col_map[col_viejo]
    if (col_viejo %in% colnames(dt)) {
      setnames(dt, col_viejo, col_nuevo)
    }
  }
  
  # ========== v1.3: TRANSFORMACIÓN PARA RESIDENTES EXTRANJERO (AÑOS < 2020) ==========
  
  if (año_archivo < 2020) {
    message("🔄 [TRANSFORMACIÓN v1.3] Procesando datos de RESIDENTES EXTRANJERO para año ", año_archivo)
    
    tiene_cabecera <- "cabecera_distrital" %in% colnames(dt)
    tiene_municipio <- "nombre_municipio" %in% colnames(dt)
    
    if (tiene_cabecera || tiene_municipio) {
      filas_extranjero <- rep(FALSE, nrow(dt))
      
      if (tiene_cabecera) {
        filas_extranjero <- filas_extranjero | grepl("RESIDENTES EXTRANJERO", dt$cabecera_distrital, ignore.case = TRUE)
      }
      
      if (tiene_municipio) {
        filas_extranjero <- filas_extranjero | grepl("^RESIDENTES EXTRANJERO$", dt$nombre_municipio, ignore.case = TRUE)
      }
      
      num_filas_extranjero <- sum(filas_extranjero)
      
      if (num_filas_extranjero > 0) {
        message("   📍 Encontradas ", num_filas_extranjero, " filas de RESIDENTES EXTRANJERO")
        
        if (!"padron_extranjero" %in% colnames(dt)) {
          dt[, padron_extranjero := NA_real_]
        }
        if (!"lista_extranjero" %in% colnames(dt)) {
          dt[, lista_extranjero := NA_real_]
        }
        if (!"padron_extranjero_hombres" %in% colnames(dt)) {
          dt[, padron_extranjero_hombres := NA_real_]
        }
        if (!"padron_extranjero_mujeres" %in% colnames(dt)) {
          dt[, padron_extranjero_mujeres := NA_real_]
        }
        if (!"lista_extranjero_hombres" %in% colnames(dt)) {
          dt[, lista_extranjero_hombres := NA_real_]
        }
        if (!"lista_extranjero_mujeres" %in% colnames(dt)) {
          dt[, lista_extranjero_mujeres := NA_real_]
        }
        
        idx_extranjero <- which(filas_extranjero)
        
        if ("padron_nacional" %in% colnames(dt)) {
          dt[idx_extranjero, padron_extranjero := padron_nacional]
          dt[idx_extranjero, padron_nacional := 0]
        }
        
        if ("lista_nacional" %in% colnames(dt)) {
          dt[idx_extranjero, lista_extranjero := lista_nacional]
          dt[idx_extranjero, lista_nacional := 0]
        }
        
        if ("padron_nacional_hombres" %in% colnames(dt)) {
          dt[idx_extranjero, padron_extranjero_hombres := padron_nacional_hombres]
          dt[idx_extranjero, padron_nacional_hombres := 0]
        }
        
        if ("padron_nacional_mujeres" %in% colnames(dt)) {
          dt[idx_extranjero, padron_extranjero_mujeres := padron_nacional_mujeres]
          dt[idx_extranjero, padron_nacional_mujeres := 0]
        }
        
        if ("lista_nacional_hombres" %in% colnames(dt)) {
          dt[idx_extranjero, lista_extranjero_hombres := lista_nacional_hombres]
          dt[idx_extranjero, lista_nacional_hombres := 0]
        }
        
        if ("lista_nacional_mujeres" %in% colnames(dt)) {
          dt[idx_extranjero, lista_extranjero_mujeres := lista_nacional_mujeres]
          dt[idx_extranjero, lista_nacional_mujeres := 0]
        }
        
        message("   ✅ Transformación completada: datos movidos de columnas nacional → extranjero")
        message("   ✅ Columnas nacional puestas en 0 para filas de RESIDENTES EXTRANJERO")
      } else {
        message("   ℹ️ No se encontraron filas de RESIDENTES EXTRANJERO")
      }
    }
  }
  
  # ========== PROCESAR FILA DE TOTALES (DESPUÉS DE NORMALIZAR) ==========
  
  fila_totales <- NULL
  
  if (!is.null(fila_totales_raw)) {
    nombres_normalizados <- colnames(dt)
    
    if (length(fila_totales_raw) == length(nombres_normalizados)) {
      names(fila_totales_raw) <- nombres_normalizados
      fila_totales <- fila_totales_raw
      
      message("✅ [DESPUÉS NORMALIZAR] Fila TOTALES procesada con nombres normalizados")
      
      # Mostrar valores clave
      col_padron_nac <- grep("^padron_nacional$", names(fila_totales), ignore.case = TRUE, value = TRUE)[1]
      col_lista_nac <- grep("^lista_nacional$", names(fila_totales), ignore.case = TRUE, value = TRUE)[1]
      col_padron_ext <- grep("^padron_extranjero$", names(fila_totales), ignore.case = TRUE, value = TRUE)[1]
      col_lista_ext <- grep("^lista_extranjero$", names(fila_totales), ignore.case = TRUE, value = TRUE)[1]
      
      if (!is.na(col_padron_nac)) {
        valor_padron <- as.numeric(gsub(",", "", as.character(fila_totales[[col_padron_nac]])))
        if (!is.na(valor_padron)) {
          message("   📊 Padrón Nacional: ", format(valor_padron, big.mark = ","))
        }
      }
      
      if (!is.na(col_lista_nac)) {
        valor_lista <- as.numeric(gsub(",", "", as.character(fila_totales[[col_lista_nac]])))
        if (!is.na(valor_lista)) {
          message("   📊 Lista Nacional: ", format(valor_lista, big.mark = ","))
        }
      }
      
      if (!is.na(col_padron_ext)) {
        valor_padron_ext <- as.numeric(gsub(",", "", as.character(fila_totales[[col_padron_ext]])))
        if (!is.na(valor_padron_ext)) {
          message("   📊 Padrón Extranjero: ", format(valor_padron_ext, big.mark = ","))
        }
      }
      
      if (!is.na(col_lista_ext)) {
        valor_lista_ext <- as.numeric(gsub(",", "", as.character(fila_totales[[col_lista_ext]])))
        if (!is.na(valor_lista_ext)) {
          message("   📊 Lista Extranjero: ", format(valor_lista_ext, big.mark = ","))
        }
      }
      
    } else {
      message("⚠️ [DESPUÉS NORMALIZAR] Longitud no coincide: raw=", length(fila_totales_raw), ", columnas=", length(nombres_normalizados))
    }
  } else {
    message("⚠️ [DESPUÉS NORMALIZAR] No hay fila de totales para procesar")
  }
  
  # ========== AGREGAR MAPEOS GEOGRÁFICOS ==========
  
  if ("clave_entidad" %in% colnames(dt)) {
    dt[, nombre_entidad := entidades[sprintf("%02d", as.integer(clave_entidad))]]
  }
  
  if (!"cabecera_distrital" %in% colnames(dt)) {
    if ("clave_distrito" %in% colnames(dt)) {
      dt[, cabecera_distrital := sprintf("%02d", as.integer(clave_distrito))]
      message("⚠️ cabecera_distrital no encontrada, creada desde clave_distrito")
    }
  } else {
    message("✅ cabecera_distrital ya existe en CSV con nombres")
  }
  
  if ("clave_distrito" %in% colnames(dt)) {
    dt[, codigo_distrito := sprintf("%02d", as.integer(clave_distrito))]
  }
  
  if (!"nombre_municipio" %in% colnames(dt)) {
    if (all(c("clave_entidad", "clave_municipio") %in% colnames(dt))) {
      dt[, nombre_municipio := paste0(
        sprintf("%02d", as.integer(clave_entidad)), "-",
        sprintf("%03d", as.integer(clave_municipio))
      )]
      message("⚠️ nombre_municipio no encontrado, creado desde claves")
    }
  } else {
    message("✅ nombre_municipio ya existe en CSV")
  }
  
  # ========== PROCESAR COLUMNAS NUMÉRICAS ==========
  
  cols_numericas <- setdiff(
    colnames(dt),
    c("clave_entidad", "clave_distrito", "clave_municipio", "seccion",
      "nombre_entidad", "cabecera_distrital", "nombre_municipio")
  )
  
  for (col in cols_numericas) {
    if (col %in% colnames(dt)) {
      dt[[col]] <- suppressWarnings(as.numeric(dt[[col]]))
    }
  }
  
  # Calcular tasa de inclusión
  if (all(c("padron_nacional", "lista_nacional") %in% colnames(dt))) {
    dt[, tasa_inclusion_nacional := round((lista_nacional / padron_nacional) * 100, 2)]
    dt[is.nan(tasa_inclusion_nacional) | is.infinite(tasa_inclusion_nacional), tasa_inclusion_nacional := NA]
    message("✅ tasa_inclusion_nacional calculada")
  }
  
  tiempo_proceso <- round(difftime(Sys.time(), inicio_proceso, units = "secs"), 2)
  message("⏱️ Procesamiento: ", tiempo_proceso, " seg")
  
  # ========== APLICAR FILTROS ==========
  inicio_filtros <- Sys.time()
  
  seccion_filtro <- seccion
  
  if (estado != "Nacional" && "nombre_entidad" %in% colnames(dt)) {
    dt <- dt[toupper(nombre_entidad) == toupper(estado)]
    message("🔍 Filtro estado: ", estado, " → ", nrow(dt), " filas")
  }
  
  if (distrito != "Todos" && "cabecera_distrital" %in% colnames(dt)) {
    dt <- dt[cabecera_distrital == distrito]
    message("🔍 Filtro distrito: ", distrito, " → ", nrow(dt), " filas")
  }
  
  if (municipio != "Todos" && "nombre_municipio" %in% colnames(dt)) {
    dt <- dt[nombre_municipio == municipio]
    message("🔍 Filtro municipio: ", municipio, " → ", nrow(dt), " filas")
  }
  
  if (!is.null(seccion_filtro) && length(seccion_filtro) > 0 && 
      !("Todas" %in% seccion_filtro) && "seccion" %in% colnames(dt)) {
    
    secciones_char <- as.character(seccion_filtro)
    dt <- dt[as.character(seccion) %in% secciones_char]
    
    message("🔍 Filtro secciones: ", paste(secciones_char, collapse = ", "), " → ", nrow(dt), " filas")
  }
  
  if (!incluir_extranjero && "nombre_entidad" %in% colnames(dt)) {
    dt <- dt[nombre_entidad != "EXTRANJERO"]
    message("🔍 Excluir extranjero → ", nrow(dt), " filas")
  }
  
  tiempo_filtros <- round(difftime(Sys.time(), inicio_filtros, units = "secs"), 2)
  message("⏱️ Filtros: ", tiempo_filtros, " seg")
  
  # ========== PREPARAR RESULTADO ==========
  
  df <- as.data.frame(dt)
  
  todos_estados <- if ("nombre_entidad" %in% colnames(df)) {
    sort(unique(df$nombre_entidad[df$nombre_entidad != "EXTRANJERO"]))
  } else character(0)
  
  todos_distritos <- if ("cabecera_distrital" %in% colnames(df)) {
    sort(unique(df$cabecera_distrital))
  } else character(0)
  
  todos_municipios <- if ("nombre_municipio" %in% colnames(df)) {
    sort(unique(df$nombre_municipio))
  } else character(0)
  
  todas_secciones <- if ("seccion" %in% colnames(df)) {
    sort(unique(df$seccion))
  } else character(0)
  
  tiempo_total <- round(difftime(Sys.time(), inicio_total, units = "secs"), 2)
  message("✅ [cargar_lne] Cargados: ", nrow(df), " filas, ", ncol(df), " columnas (", tiempo_total, " seg)")
  
  resultado <- list(
    datos = df,
    totales = fila_totales,
    todos_estados = todos_estados,
    todos_distritos = todos_distritos,
    todos_municipios = todos_municipios,
    todas_secciones = todas_secciones
  )
  
  if (!is.null(fila_totales)) {
    message("✅ [cargar_lne] Retornando con fila de totales incluida")
  } else {
    message("⚠️ [cargar_lne] Retornando SIN fila de totales")
  }
  
  return(resultado)
}

message("✅ datos_lne.R v1.4 cargado")
message("   ✅ CORRECCIÓN CRÍTICA: Busca fila totales con cve_entidad = NA")
message("   ✅ Compatibilidad con archivos antiguos que usan 'TOTALES'")
