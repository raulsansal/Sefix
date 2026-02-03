# server/datos_lne.R
# Sistema de carga de datos de Lista Nominal Electoral (LNE) desde Firebase
# Versión: 2.2 - CORRECCIÓN: Buscar padron_nacional_hombres en archivos pre-2020
# Cambios vs v2.1:
#   - Transformación legacy ahora busca padron_nacional_hombres (no padron_hombres)
#   - Los archivos originales pre-2020 usan padron_nacional_hombres incluso para extranjero
#   - Agregado diagnóstico de columnas disponibles antes de transformación
# Fecha: 03 de febrero de 2026

library(data.table)
library(dplyr)

# Cargar sistema de Firebase
if (file.exists("firebase_loaders.R")) {
  source("firebase_loaders.R")
  message("✅ firebase_loaders.R cargado en datos_lne.R")
} else {
  stop("❌ No se encontró firebase_loaders.R")
}

# ========== ESCANEAR ARCHIVOS DISPONIBLES (FIREBASE) ==========

escanear_archivos_lne <- function() {
  
  message("🔍 Escaneando archivos LNE desde Firebase Storage...")
  
  # Obtener fechas disponibles
  fechas <- listar_fechas_disponibles_firebase()
  
  catalogo <- list(
    historico = basename(as.character(fechas$historico)),
    historico_fechas = fechas$historico,
    semanal_comun = basename(as.character(fechas$semanal)),
    semanal_comun_fechas = fechas$semanal,
    semanal_sexo = basename(as.character(fechas$semanal)),
    semanal_edad = basename(as.character(fechas$semanal)),
    semanal_origen = basename(as.character(fechas$semanal))
  )
  
  message("📂 Archivos históricos disponibles: ", length(catalogo$historico))
  message("📂 Archivos semanales disponibles: ", length(catalogo$semanal_comun))
  
  return(catalogo)
}

# ========== CATÁLOGO GLOBAL ==========

if (!exists("LNE_CATALOG")) {
  message("🔍 Inicializando catálogo LNE desde Firebase...")
  LNE_CATALOG <- escanear_archivos_lne()
  
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

# ========== FUNCIÓN PRINCIPAL: CARGAR LNE DESDE FIREBASE ==========

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
  fecha_str <- format(fecha, "%Y%m%d")
  
  message("📂 [cargar_lne] Cargando desde Firebase: ", tipo_corte, " - ", fecha_str)
  
  # ========== CARGAR DESDE FIREBASE ==========
  
  dt <- if (tipo_corte == "historico") {
    cargar_pdln_historico_firebase(fecha_str)
  } else if (tipo_corte == "semanal") {
    # Determinar dimensión
    tipo_dimension <- switch(dimension,
                             "completo" = "edad",  # Por defecto usar edad
                             "sexo" = "sexo",
                             "edad" = "edad",
                             "origen" = "origen",
                             "edad")
    
    cargar_pdln_semanal_firebase(fecha_str, tipo = tipo_dimension)
  } else {
    stop("Tipo de corte no válido: ", tipo_corte)
  }
  
  if (is.null(dt) || nrow(dt) == 0) {
    message("❌ [cargar_lne] No se pudo cargar archivo desde Firebase")
    return(NULL)
  }
  
  message("📊 [cargar_lne] Filas leídas: ", format(nrow(dt), big.mark = ","))
  message("📊 [cargar_lne] Columnas leídas: ", ncol(dt))
  
  # ========== EXTRAER FILA TOTALES ==========
  
  fila_totales_raw <- NULL
  idx_totales <- NULL
  
  primera_columna <- dt[[1]]
  
  # Buscar fila con NA (método preferido)
  idx_totales_na <- which(is.na(primera_columna))
  
  # Buscar "TOTALES" (compatibilidad)
  idx_totales_texto <- which(grepl("^TOTALES$", primera_columna, ignore.case = TRUE))
  
  if (length(idx_totales_na) > 0) {
    idx_totales <- idx_totales_na[length(idx_totales_na)]
    fila_totales_raw <- as.list(dt[idx_totales, ])
    message("✅ Fila TOTALES extraída (método: NA)")
    dt <- dt[-idx_totales, ]
    
  } else if (length(idx_totales_texto) > 0) {
    idx_totales <- idx_totales_texto[length(idx_totales_texto)]
    fila_totales_raw <- as.list(dt[idx_totales, ])
    message("✅ Fila TOTALES extraída (método: texto)")
    dt <- dt[-idx_totales, ]
  }
  
  # ========== NORMALIZAR COLUMNAS ==========
  
  colnames(dt) <- tolower(colnames(dt))
  colnames(dt) <- gsub("\\s+", "_", colnames(dt))
  colnames(dt) <- gsub("[áàäâ]", "a", colnames(dt))
  colnames(dt) <- gsub("[éèëê]", "e", colnames(dt))
  colnames(dt) <- gsub("[íìïî]", "i", colnames(dt))
  colnames(dt) <- gsub("[óòöô]", "o", colnames(dt))
  colnames(dt) <- gsub("[úùüû]", "u", colnames(dt))
  colnames(dt) <- gsub("ñ", "n", colnames(dt))
  
  # Renombrar columnas clave
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
  
  # ========== TRANSFORMACIÓN RESIDENTES EXTRANJERO (< 2020) ==========
  # v2.2: Corregido para buscar padron_nacional_hombres (nombre real en archivos originales)
  
  if (año_archivo < 2020) {
    message("🔄 Procesando RESIDENTES EXTRANJERO para año ", año_archivo)
    
    # Definir columnas esperadas de extranjero
    cols_extranjero <- c("padron_extranjero", "lista_extranjero",
                         "padron_extranjero_hombres", "padron_extranjero_mujeres",
                         "lista_extranjero_hombres", "lista_extranjero_mujeres")
    
    # ── DETECCIÓN: ¿El CSV ya tiene columnas de extranjero con datos? ──
    # Esto ocurre cuando el CSV fue modificado manualmente para 2017-2019
    tiene_cols_sexo_ext <- all(c("padron_extranjero_hombres", 
                                 "padron_extranjero_mujeres") %in% colnames(dt))
    
    tiene_datos_extranjero <- tiene_cols_sexo_ext &&
      sum(dt$padron_extranjero_hombres, na.rm = TRUE) > 0
    
    if (tiene_datos_extranjero) {
      # ════════════════════════════════════════════════════════════════
      # CASO A: Columnas ya pobladas desde el CSV modificado
      # → No transformar, los datos vienen correctos del archivo
      # ════════════════════════════════════════════════════════════════
      message("   ℹ️ Año ", año_archivo, 
              ": columnas de extranjero ya pobladas en CSV. Omitiendo transformación.")
      
      # Solo asegurar que NO falte ninguna columna auxiliar
      for (col in cols_extranjero) {
        if (!col %in% colnames(dt)) {
          dt[, (col) := NA_real_]
          message("      ⚠️ Columna faltante creada como NA: ", col)
        }
      }
      
    } else {
      # ════════════════════════════════════════════════════════════════
      # CASO B: Transformación legacy mejorada
      # → Extraer datos de filas mixtas de RESIDENTES EXTRANJERO
      # ════════════════════════════════════════════════════════════════
      message("   🔄 Año ", año_archivo, 
              ": aplicando transformación legacy de extranjero.")
      
      # v2.2: Diagnóstico de columnas disponibles
      cols_sexo_disponibles <- grep("hombres|mujeres", colnames(dt), value = TRUE)
      message("      📋 Columnas de sexo disponibles: ", paste(cols_sexo_disponibles, collapse = ", "))
      
      tiene_cabecera <- "cabecera_distrital" %in% colnames(dt)
      tiene_municipio <- "nombre_municipio" %in% colnames(dt)
      
      if (tiene_cabecera || tiene_municipio) {
        filas_extranjero <- rep(FALSE, nrow(dt))
        
        if (tiene_cabecera) {
          filas_extranjero <- filas_extranjero | 
            grepl("RESIDENTES EXTRANJERO", dt$cabecera_distrital, ignore.case = TRUE)
        }
        
        if (tiene_municipio) {
          filas_extranjero <- filas_extranjero | 
            grepl("^RESIDENTES EXTRANJERO$", dt$nombre_municipio, ignore.case = TRUE)
        }
        
        num_filas_extranjero <- sum(filas_extranjero)
        
        if (num_filas_extranjero > 0) {
          message("      📍 Encontradas ", num_filas_extranjero, " filas de RESIDENTES EXTRANJERO")
          
          # Crear columnas de extranjero si no existen
          for (col in cols_extranjero) {
            if (!col %in% colnames(dt)) {
              dt[, (col) := NA_real_]
            }
          }
          
          idx_extranjero <- which(filas_extranjero)
          
          # ── Mover TOTALES: padron_nacional → padron_extranjero ──
          if ("padron_nacional" %in% colnames(dt)) {
            dt[idx_extranjero, padron_extranjero := padron_nacional]
            dt[idx_extranjero, padron_nacional := 0]
          }
          
          if ("lista_nacional" %in% colnames(dt)) {
            dt[idx_extranjero, lista_extranjero := lista_nacional]
            dt[idx_extranjero, lista_nacional := 0]
          }
          
          # ══════════════════════════════════════════════════════════════
          # v2.2 CORRECCIÓN CRÍTICA: Mover SEXO
          # Los archivos originales pre-2020 usan "padron_nacional_hombres"
          # (NO "padron_hombres") incluso para las filas de extranjero
          # ══════════════════════════════════════════════════════════════
          
          # Intentar primero padron_nacional_hombres (archivos originales)
          # Si no existe, intentar padron_hombres (por compatibilidad)
          col_padron_h <- if ("padron_nacional_hombres" %in% colnames(dt)) {
            "padron_nacional_hombres"
          } else if ("padron_hombres" %in% colnames(dt)) {
            "padron_hombres"
          } else NULL
          
          col_padron_m <- if ("padron_nacional_mujeres" %in% colnames(dt)) {
            "padron_nacional_mujeres"
          } else if ("padron_mujeres" %in% colnames(dt)) {
            "padron_mujeres"
          } else NULL
          
          col_lista_h <- if ("lista_nacional_hombres" %in% colnames(dt)) {
            "lista_nacional_hombres"
          } else if ("lista_hombres" %in% colnames(dt)) {
            "lista_hombres"
          } else NULL
          
          col_lista_m <- if ("lista_nacional_mujeres" %in% colnames(dt)) {
            "lista_nacional_mujeres"
          } else if ("lista_mujeres" %in% colnames(dt)) {
            "lista_mujeres"
          } else NULL
          
          # Mover datos de sexo
          if (!is.null(col_padron_h)) {
            dt[idx_extranjero, padron_extranjero_hombres := get(col_padron_h)]
            dt[idx_extranjero, (col_padron_h) := 0]
            message("      ✅ Movido ", col_padron_h, " → padron_extranjero_hombres")
          }
          
          if (!is.null(col_padron_m)) {
            dt[idx_extranjero, padron_extranjero_mujeres := get(col_padron_m)]
            dt[idx_extranjero, (col_padron_m) := 0]
            message("      ✅ Movido ", col_padron_m, " → padron_extranjero_mujeres")
          }
          
          if (!is.null(col_lista_h)) {
            dt[idx_extranjero, lista_extranjero_hombres := get(col_lista_h)]
            dt[idx_extranjero, (col_lista_h) := 0]
            message("      ✅ Movido ", col_lista_h, " → lista_extranjero_hombres")
          }
          
          if (!is.null(col_lista_m)) {
            dt[idx_extranjero, lista_extranjero_mujeres := get(col_lista_m)]
            dt[idx_extranjero, (col_lista_m) := 0]
            message("      ✅ Movido ", col_lista_m, " → lista_extranjero_mujeres")
          }
          
          message("      ✅ Transformación completada (totales + sexo)")
        } else {
          message("      ⚠️ No se encontraron filas de RESIDENTES EXTRANJERO")
        }
      }
    }
  }
  
  # ========== PROCESAR FILA TOTALES ==========
  
  fila_totales <- NULL
  
  if (!is.null(fila_totales_raw)) {
    nombres_normalizados <- colnames(dt)
    
    if (length(fila_totales_raw) == length(nombres_normalizados)) {
      names(fila_totales_raw) <- nombres_normalizados
      fila_totales <- fila_totales_raw
      message("✅ Fila TOTALES procesada")
    }
  }
  
  # ========== AGREGAR MAPEOS GEOGRÁFICOS ==========
  
  if ("clave_entidad" %in% colnames(dt)) {
    dt[, nombre_entidad := entidades[sprintf("%02d", as.integer(clave_entidad))]]
  }
  
  if (!"cabecera_distrital" %in% colnames(dt) && "clave_distrito" %in% colnames(dt)) {
    dt[, cabecera_distrital := sprintf("%02d", as.integer(clave_distrito))]
  }
  
  if (!"nombre_municipio" %in% colnames(dt) && all(c("clave_entidad", "clave_municipio") %in% colnames(dt))) {
    dt[, nombre_municipio := paste0(
      sprintf("%02d", as.integer(clave_entidad)), "-",
      sprintf("%03d", as.integer(clave_municipio))
    )]
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
  }
  
  # ========== APLICAR FILTROS ==========
  
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
  
  if (!is.null(seccion) && length(seccion) > 0 && 
      !("Todas" %in% seccion) && "seccion" %in% colnames(dt)) {
    secciones_char <- as.character(seccion)
    dt <- dt[as.character(seccion) %in% secciones_char]
    message("🔍 Filtro secciones: ", paste(secciones_char, collapse = ", "), " → ", nrow(dt), " filas")
  }
  
  if (!incluir_extranjero && "nombre_entidad" %in% colnames(dt)) {
    dt <- dt[nombre_entidad != "EXTRANJERO"]
    message("🔍 Excluir extranjero → ", nrow(dt), " filas")
  }
  
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
  message("✅ [cargar_lne] Cargados: ", nrow(df), " filas (", tiempo_total, " seg)")
  
  resultado <- list(
    datos = df,
    totales = fila_totales,
    todos_estados = todos_estados,
    todos_distritos = todos_distritos,
    todos_municipios = todos_municipios,
    todas_secciones = todas_secciones
  )
  
  return(resultado)
}

message("✅ datos_lne.R v2.2 cargado (Firebase Storage + corrección columnas sexo pre-2020)")
