# modules/lista_nominal_graficas/graficas_semanal_data_loaders.R
# Versión: 2.1 — Agrega datos_semanal_serie_origen() para proyección O2
#
# CAMBIOS vs v1.2 (v2.0):
#   - Agrega datos_semanal_serie_edad(): serie temporal con un punto por semana,
#     columnas de padrón y LNE por cada uno de los 12 rangos de edad (sin distinción
#     de sexo) + totales. Usado por Gráfica E1.
#   - Agrega datos_semanal_serie_sexo(): serie temporal con padrón/LNE por sexo
#     (hombres, mujeres, no_binario). Usado por Gráfica S7.
#   - Ambas series se cargan al primer acceso y se cachean en LNE_CACHE_SEMANAL
#     (claves 'serie_edad' y 'serie_sexo') por 24h.
#   - La serie siempre refleja el TOTAL nacional o extranjero (no aplica filtros
#     geográficos del usuario), consistente con graficas_historico_1_2.R.
#   - Sin cambios en datos_semanal_edad/sexo/origen ni en la interfaz de retorno
#     (se agregan dos entradas al list de retorno).

graficas_semanal_data_loaders <- function(input, output, session,
                                          estado_app,
                                          filtros_usuario,
                                          ambito_reactivo) {
  
  message("📥 Inicializando graficas_semanal_data_loaders v2.0")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CONSTANTES
  # ══════════════════════════════════════════════════════════════════════════
  
  ORDEN_EDAD <- c("18","19","20_24","25_29","30_34","35_39",
                  "40_44","45_49","50_54","55_59","60_64","65_y_mas")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CACHÉ GLOBAL
  # ══════════════════════════════════════════════════════════════════════════
  
  if (!exists("LNE_CACHE_SEMANAL", envir = .GlobalEnv)) {
    assign("LNE_CACHE_SEMANAL",
           list(edad       = NULL,
                sexo       = NULL,
                origen     = NULL,
                serie_edad = NULL,
                serie_sexo = NULL,
                fecha      = NULL,
                timestamp  = NULL),
           envir = .GlobalEnv)
    message("📦 Caché semanal inicializado (v2.0)")
  } else {
    # Asegurar que las claves nuevas existan si el caché fue creado por v1.x
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (is.null(cache$serie_edad)) cache$serie_edad <- NULL
    if (is.null(cache$serie_sexo)) cache$serie_sexo <- NULL
    assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPERS — sin cambios vs v1.2
  # ══════════════════════════════════════════════════════════════════════════
  
  cache_semanal_valido <- function(fecha_requerida) {
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (is.null(cache$timestamp) || is.null(cache$fecha)) return(FALSE)
    if (!identical(cache$fecha, fecha_requerida)) return(FALSE)
    difftime(Sys.time(), cache$timestamp, units = "hours") < 24
  }
  
  # Caché de serie: no depende de fecha de corte, solo de antigüedad
  cache_serie_valido <- function(clave) {
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (is.null(cache[[clave]]) || is.null(cache$timestamp)) return(FALSE)
    difftime(Sys.time(), cache$timestamp, units = "hours") < 24
  }
  
  obtener_fecha_semanal_reciente <- function() {
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    if (length(catalog$semanal_comun) == 0) return(NULL)
    as.Date(max(catalog$semanal_comun), origin = "1970-01-01")
  }
  
  obtener_todas_fechas_semanal <- function() {
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    if (length(catalog$semanal_comun) == 0) return(NULL)
    sort(as.Date(catalog$semanal_comun, origin = "1970-01-01"))
  }
  
  es_nacional_sin_filtros <- function(f) {
    f$entidad == "Nacional" && f$distrito == "Todos" && f$municipio == "Todos" &&
      (is.null(f$seccion) || "Todas" %in% f$seccion || length(f$seccion) == 0)
  }
  
  cargar_tipo_semanal <- function(fecha, tipo, filtros) {
    tryCatch({
      res <- cargar_lne(tipo_corte = "semanal", fecha = fecha, dimension = tipo,
                        estado     = filtros$entidad   %||% "Nacional",
                        distrito   = filtros$distrito  %||% "Todos",
                        municipio  = filtros$municipio %||% "Todos",
                        seccion    = filtros$seccion   %||% "Todas",
                        incluir_extranjero = TRUE)
      if (!is.null(res) && !is.null(res$datos) && nrow(res$datos) > 0) return(res$datos)
      return(NULL)
    }, error = function(e) {
      message("❌ Error semanal/", tipo, " (", format(fecha, "%Y%m%d"), "): ", e$message)
      return(NULL)
    })
  }
  
  filtros_defecto <- list(entidad = "Nacional", distrito = "Todos",
                          municipio = "Todos",  seccion  = "Todas")
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPERS DE AGREGACIÓN PARA LA SERIE — NUEVOS EN v2.0
  # ══════════════════════════════════════════════════════════════════════════
  
  # Extrae el total nacional de un data.frame de un corte semanal.
  # Para "nacional": usa la fila TOTALES si existe; si no, suma todas las
  # filas que NO sean RESIDENTES EXTRANJERO ni TOTALES.
  # Para "extranjero": suma las 32 filas RESIDENTES EXTRANJERO.
  extraer_fila_agregada <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0 || !"nombre_entidad" %in% colnames(df)) return(NULL)
    
    if (ambito == "extranjero") {
      filas <- df[grepl("RESIDENTES EXTRANJERO", toupper(df$nombre_entidad), fixed = TRUE), ]
      if (nrow(filas) == 0) return(NULL)
    } else {
      # Intentar fila TOTALES primero
      idx_tot <- which(toupper(trimws(df$nombre_entidad)) == "TOTALES")
      if (length(idx_tot) > 0) {
        return(df[idx_tot[1], , drop = FALSE])
      }
      # Si no hay TOTALES, sumar filas nacionales (excluye RESIDENTES EXTRANJERO y TOTALES)
      filas <- df[!grepl("RESIDENTES EXTRANJERO|^TOTALES$",
                         toupper(trimws(df$nombre_entidad))), ]
      if (nrow(filas) == 0) return(NULL)
    }
    
    # Sumar columnas numéricas
    cols_id  <- c("cve_entidad","nombre_entidad","cve_distrito","cabecera_distrital",
                  "cve_municipio","nombre_municipio","seccion")
    cols_num <- setdiff(colnames(filas), cols_id)
    cols_num <- cols_num[sapply(filas[, cols_num, drop = FALSE], function(x)
      is.numeric(x) || suppressWarnings(!any(is.na(as.numeric(x)))))]
    
    fila_suma <- as.data.frame(
      lapply(filas[, cols_num, drop = FALSE], function(x)
        sum(as.numeric(x), na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
    fila_suma$nombre_entidad <- if (ambito == "extranjero") "RESIDENTES EXTRANJERO" else "TOTALES"
    fila_suma
  }
  
  # Construye una fila de la serie de edad para una fecha dada.
  # Devuelve: fecha + padron_total + lista_total + padron_[rango] + lista_[rango] × 12
  construir_fila_serie_edad <- function(df, fecha, ambito) {
    fila <- extraer_fila_agregada(df, ambito)
    if (is.null(fila)) return(NULL)
    
    resultado <- list(fecha = fecha)
    
    for (rango in ORDEN_EDAD) {
      # Sumar hombres + mujeres + no_binario para padrón y lista
      col_ph <- paste0("padron_", rango, "_hombres")
      col_pm <- paste0("padron_", rango, "_mujeres")
      col_pn <- paste0("padron_", rango, "_no_binario")
      col_lh <- paste0("lista_", rango, "_hombres")
      col_lm <- paste0("lista_", rango, "_mujeres")
      col_ln <- paste0("lista_", rango, "_no_binario")
      
      val_pad <- sum(
        if (col_ph %in% colnames(fila)) as.numeric(fila[[col_ph]]) else 0,
        if (col_pm %in% colnames(fila)) as.numeric(fila[[col_pm]]) else 0,
        if (col_pn %in% colnames(fila)) as.numeric(fila[[col_pn]]) else 0,
        na.rm = TRUE
      )
      val_lst <- sum(
        if (col_lh %in% colnames(fila)) as.numeric(fila[[col_lh]]) else 0,
        if (col_lm %in% colnames(fila)) as.numeric(fila[[col_lm]]) else 0,
        if (col_ln %in% colnames(fila)) as.numeric(fila[[col_ln]]) else 0,
        na.rm = TRUE
      )
      
      resultado[[paste0("padron_", rango)]] <- val_pad
      resultado[[paste0("lista_",  rango)]] <- val_lst
    }
    
    # Totales generales (suma de todos los rangos)
    resultado$padron_total <- sum(
      unlist(resultado[paste0("padron_", ORDEN_EDAD)]), na.rm = TRUE)
    resultado$lista_total <- sum(
      unlist(resultado[paste0("lista_",  ORDEN_EDAD)]), na.rm = TRUE)
    
    as.data.frame(resultado, stringsAsFactors = FALSE)
  }
  
  # Construye una fila de la serie de sexo para una fecha dada.
  # Devuelve: fecha + padron_hombres + padron_mujeres + padron_no_binario
  #                  + lista_hombres  + lista_mujeres  + lista_no_binario
  construir_fila_serie_sexo <- function(df, fecha, ambito) {
    fila <- extraer_fila_agregada(df, ambito)
    if (is.null(fila)) return(NULL)
    
    get_col <- function(col) {
      if (col %in% colnames(fila)) {
        v <- as.numeric(fila[[col]])
        if (is.na(v)) 0 else v
      } else 0
    }
    
    data.frame(
      fecha              = fecha,
      padron_hombres     = get_col("padron_hombres"),
      padron_mujeres     = get_col("padron_mujeres"),
      padron_no_binario  = get_col("padron_no_binario"),
      lista_hombres      = get_col("lista_hombres"),
      lista_mujeres      = get_col("lista_mujeres"),
      lista_no_binario   = get_col("lista_no_binario"),
      stringsAsFactors   = FALSE
    )
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVE: fecha_semanal_efectiva — sin cambios vs v1.2
  # ══════════════════════════════════════════════════════════════════════════
  
  fecha_semanal_efectiva <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    estado_actual <- estado_app()
    if (estado_actual %in% c("restablecido", "inicial")) {
      fecha <- obtener_fecha_semanal_reciente()
      message("📅 [semanal] estado=", estado_actual, " → fecha: ", fecha)
      return(fecha)
    }
    if (estado_actual == "consultado") {
      btn      <- input$btn_consultar
      year_sel <- isolate(input$year)
      if (is.null(year_sel) || is.na(year_sel)) return(obtener_fecha_semanal_reciente())
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      fechas  <- catalog$semanal_comun[
        format(catalog$semanal_comun, "%Y") == as.character(year_sel)]
      if (length(fechas) == 0) { message("⚠️ Sin fechas para año ", year_sel); return(NULL) }
      fecha <- as.Date(max(fechas), origin = "1970-01-01")
      message("📅 [semanal] CONSULTADO → ", fecha)
      return(fecha)
    }
    return(NULL)
  })
  
  anio_semanal <- reactive({
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(as.integer(format(Sys.Date(), "%Y")))
    as.integer(format(fecha, "%Y"))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVES: corte único — sin cambios vs v1.2
  # ══════════════════════════════════════════════════════════════════════════
  
  datos_semanal_edad <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") {
      btn <- input$btn_consultar; isolate(filtros_usuario())
    } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$edad)) { message("✅ [CACHÉ] edad semanal"); return(cache$edad) }
    }
    message("📥 [semanal_edad] ", format(fecha, "%Y%m%d"))
    datos <- cargar_tipo_semanal(fecha, "edad", filtros)
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$edad <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_edad] ", nrow(datos), " filas")
    return(datos)
  })
  
  datos_semanal_sexo <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") {
      btn <- input$btn_consultar; isolate(filtros_usuario())
    } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$sexo)) { message("✅ [CACHÉ] sexo semanal"); return(cache$sexo) }
    }
    message("📥 [semanal_sexo] ", format(fecha, "%Y%m%d"))
    datos <- cargar_tipo_semanal(fecha, "sexo", filtros)
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$sexo <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_sexo] ", nrow(datos), " filas")
    return(datos)
  })
  
  datos_semanal_origen <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") {
      btn <- input$btn_consultar; isolate(filtros_usuario())
    } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$origen)) { message("✅ [CACHÉ] origen semanal"); return(cache$origen) }
    }
    message("📥 [semanal_origen] ", format(fecha, "%Y%m%d"))
    datos <- cargar_tipo_semanal(fecha, "origen", filtros)
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$origen <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_origen] ", nrow(datos), " filas")
    return(datos)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVE: datos_semanal_serie_edad — NUEVO EN v2.0
  #
  # Devuelve un data.frame con una fila por semana disponible en el catálogo:
  #   fecha | padron_total | lista_total |
  #   padron_[rango] | lista_[rango]  (×12 rangos, sin distinción de sexo)
  #
  # Siempre usa totales nacionales o extranjero según ambito_reactivo().
  # No aplica filtros geográficos del usuario (la proyección es siempre
  # a nivel nacional/extranjero completo).
  # Se cachea en LNE_CACHE_SEMANAL$serie_edad por 24h.
  # ══════════════════════════════════════════════════════════════════════════
  
  datos_semanal_serie_edad <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    
    ambito <- ambito_reactivo()
    clave_cache <- paste0("serie_edad_", ambito)
    
    # Verificar caché
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (!is.null(cache[[clave_cache]]) && !is.null(cache$timestamp) &&
        difftime(Sys.time(), cache$timestamp, units = "hours") < 24) {
      message("✅ [CACHÉ] serie_edad (", ambito, ") — ", nrow(cache[[clave_cache]]), " semanas")
      return(cache[[clave_cache]])
    }
    
    fechas <- obtener_todas_fechas_semanal()
    if (is.null(fechas) || length(fechas) == 0) {
      message("⚠️ [serie_edad] Sin fechas en el catálogo")
      return(NULL)
    }
    
    message("📥 [serie_edad] Cargando ", length(fechas), " semanas para ámbito=", ambito, "...")
    
    filas <- list()
    n_ok  <- 0L
    n_err <- 0L
    
    for (f in fechas) {
      fecha_d <- as.Date(f, origin = "1970-01-01")
      df <- cargar_tipo_semanal(fecha_d, "edad", filtros_defecto)
      if (is.null(df)) { n_err <- n_err + 1L; next }
      fila <- construir_fila_serie_edad(df, fecha_d, ambito)
      if (!is.null(fila)) {
        filas[[length(filas) + 1]] <- fila
        n_ok <- n_ok + 1L
      } else {
        n_err <- n_err + 1L
      }
    }
    
    if (length(filas) == 0) {
      message("❌ [serie_edad] Sin datos válidos en ninguna semana")
      return(NULL)
    }
    
    serie <- do.call(rbind, filas)
    serie <- serie[order(serie$fecha), ]
    rownames(serie) <- NULL
    
    # Guardar en caché
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    cache[[clave_cache]] <- serie
    cache$timestamp <- Sys.time()
    assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    
    message("✅ [serie_edad] ", nrow(serie), " semanas cargadas (OK=", n_ok, " ERR=", n_err, ")")
    message("   Rango: ", format(min(serie$fecha), "%Y-%m-%d"),
            " → ", format(max(serie$fecha), "%Y-%m-%d"))
    return(serie)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVE: datos_semanal_serie_sexo — NUEVO EN v2.0
  #
  # Devuelve un data.frame con una fila por semana disponible en el catálogo:
  #   fecha | padron_hombres | padron_mujeres | padron_no_binario |
  #           lista_hombres  | lista_mujeres  | lista_no_binario
  #
  # Misma lógica de caché y ámbito que datos_semanal_serie_edad.
  # ══════════════════════════════════════════════════════════════════════════
  
  datos_semanal_serie_sexo <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    
    ambito <- ambito_reactivo()
    clave_cache <- paste0("serie_sexo_", ambito)
    
    # Verificar caché
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (!is.null(cache[[clave_cache]]) && !is.null(cache$timestamp) &&
        difftime(Sys.time(), cache$timestamp, units = "hours") < 24) {
      message("✅ [CACHÉ] serie_sexo (", ambito, ") — ", nrow(cache[[clave_cache]]), " semanas")
      return(cache[[clave_cache]])
    }
    
    fechas <- obtener_todas_fechas_semanal()
    if (is.null(fechas) || length(fechas) == 0) {
      message("⚠️ [serie_sexo] Sin fechas en el catálogo")
      return(NULL)
    }
    
    message("📥 [serie_sexo] Cargando ", length(fechas), " semanas para ámbito=", ambito, "...")
    
    filas <- list()
    n_ok  <- 0L
    n_err <- 0L
    
    for (f in fechas) {
      fecha_d <- as.Date(f, origin = "1970-01-01")
      df <- cargar_tipo_semanal(fecha_d, "sexo", filtros_defecto)
      if (is.null(df)) { n_err <- n_err + 1L; next }
      fila <- construir_fila_serie_sexo(df, fecha_d, ambito)
      if (!is.null(fila)) {
        filas[[length(filas) + 1]] <- fila
        n_ok <- n_ok + 1L
      } else {
        n_err <- n_err + 1L
      }
    }
    
    if (length(filas) == 0) {
      message("❌ [serie_sexo] Sin datos válidos en ninguna semana")
      return(NULL)
    }
    
    serie <- do.call(rbind, filas)
    serie <- serie[order(serie$fecha), ]
    rownames(serie) <- NULL
    
    # Guardar en caché
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    cache[[clave_cache]] <- serie
    cache$timestamp <- Sys.time()
    assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    
    message("✅ [serie_sexo] ", nrow(serie), " semanas cargadas (OK=", n_ok, " ERR=", n_err, ")")
    message("   Rango: ", format(min(serie$fecha), "%Y-%m-%d"),
            " → ", format(max(serie$fecha), "%Y-%m-%d"))
    return(serie)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVE: datos_semanal_serie_origen — NUEVO EN v2.1
  #
  # Devuelve un data.frame con una fila por semana disponible en el catálogo:
  #   fecha | pad_01..pad_32 | pad87 | pad88 |
  #           ln_01..ln_32   | ln87  | ln88
  #
  # Cada columna representa el total de padrón o LNE cuyo estado de origen
  # corresponde a esa clave (34 orígenes posibles: 32 entidades + 87 + 88).
  # Siempre usa totales nacionales o extranjero según ambito_reactivo().
  # Se cachea en LNE_CACHE_SEMANAL con clave 'serie_origen_[ambito]' por 24h.
  # ══════════════════════════════════════════════════════════════════════════
  
  # Helper: construye una fila de la serie de origen para una fecha dada.
  # Detecta automáticamente los patrones de columna pad_NN / ln_NN presentes.
  construir_fila_serie_origen <- function(df, fecha, ambito) {
    fila <- extraer_fila_agregada(df, ambito)
    if (is.null(fila)) return(NULL)
    
    resultado <- list(fecha = fecha)
    
    # Detectar columnas pad y ln presentes (pad_01..pad_32, pad87, pad88)
    cols_pad <- grep("^pad_\\d{2}$|^pad8[78]$", colnames(fila),
                     value = TRUE, ignore.case = TRUE)
    cols_ln  <- grep("^ln_\\d{2}$|^ln8[78]$",  colnames(fila),
                     value = TRUE, ignore.case = TRUE)
    
    # Fallback: patrón alternativo si los anteriores no encuentran nada
    if (length(cols_pad) == 0)
      cols_pad <- grep("^padron_\\d{2}$|^padron_8[78]$", colnames(fila),
                       value = TRUE, ignore.case = TRUE)
    if (length(cols_ln) == 0)
      cols_ln  <- grep("^lista_\\d{2}$|^lista_8[78]$|^ln_\\d",  colnames(fila),
                       value = TRUE, ignore.case = TRUE)
    
    for (col in cols_pad) {
      resultado[[col]] <- as.numeric(fila[[col]])
      if (is.na(resultado[[col]])) resultado[[col]] <- 0
    }
    for (col in cols_ln) {
      resultado[[col]] <- as.numeric(fila[[col]])
      if (is.na(resultado[[col]])) resultado[[col]] <- 0
    }
    
    if (length(resultado) == 1) return(NULL)   # solo fecha, sin columnas útiles
    as.data.frame(resultado, stringsAsFactors = FALSE)
  }
  
  datos_semanal_serie_origen <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    
    ambito      <- ambito_reactivo()
    clave_cache <- paste0("serie_origen_", ambito)
    
    # Verificar caché
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (!is.null(cache[[clave_cache]]) && !is.null(cache$timestamp) &&
        difftime(Sys.time(), cache$timestamp, units = "hours") < 24) {
      message("✅ [CACHÉ] serie_origen (", ambito, ") — ",
              nrow(cache[[clave_cache]]), " semanas")
      return(cache[[clave_cache]])
    }
    
    fechas <- obtener_todas_fechas_semanal()
    if (is.null(fechas) || length(fechas) == 0) {
      message("⚠️ [serie_origen] Sin fechas en el catálogo")
      return(NULL)
    }
    
    message("📥 [serie_origen] Cargando ", length(fechas),
            " semanas para ámbito=", ambito, "...")
    
    filas <- list()
    n_ok  <- 0L
    n_err <- 0L
    
    for (f in fechas) {
      fecha_d <- as.Date(f, origin = "1970-01-01")
      df <- cargar_tipo_semanal(fecha_d, "origen", filtros_defecto)
      if (is.null(df)) { n_err <- n_err + 1L; next }
      fila <- construir_fila_serie_origen(df, fecha_d, ambito)
      if (!is.null(fila)) {
        filas[[length(filas) + 1]] <- fila
        n_ok <- n_ok + 1L
      } else {
        n_err <- n_err + 1L
      }
    }
    
    if (length(filas) == 0) {
      message("❌ [serie_origen] Sin datos válidos en ninguna semana")
      return(NULL)
    }
    
    # rbind con fill para tolerar semanas con columnas distintas
    serie <- tryCatch(
      dplyr::bind_rows(filas),
      error = function(e) do.call(rbind, filas)
    )
    serie[is.na(serie)] <- 0
    serie <- serie[order(serie$fecha), ]
    rownames(serie) <- NULL
    
    # Guardar en caché
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    cache[[clave_cache]] <- serie
    cache$timestamp      <- Sys.time()
    assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    
    message("✅ [serie_origen] ", nrow(serie), " semanas cargadas (OK=",
            n_ok, " ERR=", n_err, ")")
    message("   Rango: ", format(min(serie$fecha), "%Y-%m-%d"),
            " → ", format(max(serie$fecha), "%Y-%m-%d"))
    return(serie)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # RETORNO — versión 2.1: agrega datos_semanal_serie_origen
  # ══════════════════════════════════════════════════════════════════════════
  
  message("✅ graficas_semanal_data_loaders v2.1 inicializado")
  message("   ✅ Nuevos v2.0: datos_semanal_serie_edad(), datos_semanal_serie_sexo()")
  message("   ✅ Nuevo  v2.1: datos_semanal_serie_origen()")
  message("   ✅ Sin cambios en: datos_semanal_edad/sexo/origen, fecha_semanal_efectiva, anio_semanal")
  
  return(list(
    datos_semanal_edad          = datos_semanal_edad,
    datos_semanal_sexo          = datos_semanal_sexo,
    datos_semanal_origen        = datos_semanal_origen,
    datos_semanal_serie_edad    = datos_semanal_serie_edad,
    datos_semanal_serie_sexo    = datos_semanal_serie_sexo,
    datos_semanal_serie_origen  = datos_semanal_serie_origen,  # NUEVO v2.1
    fecha_semanal_efectiva      = fecha_semanal_efectiva,
    anio_semanal                = anio_semanal
  ))
}
