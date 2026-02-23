# modules/lista_nominal_graficas/graficas_data_loaders.R
# Reactives de carga de datos: year_actual, year_consulta, anuales
# Versión: 3.0 - LAZY LOADING OPTIMIZADO
#
# CAMBIOS vs v2.12:
#   - OPTIMIZACIÓN: datos_anuales_completos carga solo 1 archivo por año (el último)
#   - OPTIMIZACIÓN: datos_year_actual carga solo al iniciar o consultar año actual
#   - OPTIMIZACIÓN: datos_year_consulta carga bajo demanda para años anteriores
#   - Mejor manejo de caché para evitar recargas innecesarias
#
# ARQUITECTURA DE CARGA:
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │ AL ABRIR LA APP (estado: restablecido)                                  │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │ • datos_year_actual: Todos los meses del año del último archivo        │
#   │   Ejemplo: 7 archivos (ene-jul 2025) para Gráfica 1                     │
#   │                                                                         │
#   │ • datos_anuales_completos: Último archivo de cada año 2017→último      │
#   │   Ejemplo: 9 archivos para Gráficas 2-3                                 │
#   │                                                                         │
#   │ • datos_year_consulta: NO SE CARGA (lazy)                               │
#   └─────────────────────────────────────────────────────────────────────────┘
#
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │ AL CONSULTAR AÑO ANTERIOR (estado: consultado, año != año_ultimo)      │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │ • datos_year_consulta: Todos los meses del año consultado              │
#   │   Ejemplo: 12 archivos de 2020 para Gráficas 4-5                        │
#   └─────────────────────────────────────────────────────────────────────────┘

graficas_data_loaders <- function(input, output, session, anio_actual, anio_consultado, filtros_usuario, estado_app) {
  
  message("📥 Inicializando graficas_data_loaders v3.0 (LAZY LOADING)")
  
  # ════════════════════════════════════════════════════════════════════════════
  # FUNCIÓN AUXILIAR: Validar caché
  # ════════════════════════════════════════════════════════════════════════════
  
  cache_valido <- function(timestamp, max_horas = 24) {
    if (is.null(timestamp)) return(FALSE)
    difftime(Sys.time(), timestamp, units = "hours") < max_horas
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # HELPER: Extraer columna de sexo con fallback
  # ════════════════════════════════════════════════════════════════════════════
  
  extraer_col_sexo <- function(obj, col_preferida, col_alternativa = NULL, es_lista = FALSE) {
    nombres <- if (es_lista) names(obj) else colnames(obj)
    
    if (col_preferida %in% nombres) {
      if (es_lista) {
        return(as.numeric(gsub(",", "", as.character(obj[[col_preferida]]))))
      } else {
        return(sum(obj[[col_preferida]], na.rm = TRUE))
      }
    }
    
    if (!is.null(col_alternativa) && col_alternativa %in% nombres) {
      if (es_lista) {
        return(as.numeric(gsub(",", "", as.character(obj[[col_alternativa]]))))
      } else {
        return(sum(obj[[col_alternativa]], na.rm = TRUE))
      }
    }
    
    return(NA)
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # HELPER: Crear registro de datos desde totales o dataframe
  # ════════════════════════════════════════════════════════════════════════════
  
  crear_registro <- function(fecha, datos_temp, filtros) {
    registro <- NULL
    
    if (filtros$entidad == "Nacional" && !is.null(datos_temp$totales)) {
      # Usar fila de totales
      totales_fila <- datos_temp$totales
      
      padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
      padron_extranjero <- extraer_col_sexo(totales_fila, "padron_extranjero", es_lista = TRUE)
      lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
      lista_extranjero <- extraer_col_sexo(totales_fila, "lista_extranjero", es_lista = TRUE)
      
      padron_hombres <- extraer_col_sexo(totales_fila, "padron_hombres", "padron_nacional_hombres", es_lista = TRUE)
      padron_mujeres <- extraer_col_sexo(totales_fila, "padron_mujeres", "padron_nacional_mujeres", es_lista = TRUE)
      lista_hombres <- extraer_col_sexo(totales_fila, "lista_hombres", "lista_nacional_hombres", es_lista = TRUE)
      lista_mujeres <- extraer_col_sexo(totales_fila, "lista_mujeres", "lista_nacional_mujeres", es_lista = TRUE)
      
      padron_nacional_no_binario <- extraer_col_sexo(totales_fila, "padron_nacional_no_binario", es_lista = TRUE)
      lista_nacional_no_binario <- extraer_col_sexo(totales_fila, "lista_nacional_no_binario", es_lista = TRUE)
      
      padron_extranjero_hombres <- extraer_col_sexo(totales_fila, "padron_extranjero_hombres", es_lista = TRUE)
      padron_extranjero_mujeres <- extraer_col_sexo(totales_fila, "padron_extranjero_mujeres", es_lista = TRUE)
      lista_extranjero_hombres <- extraer_col_sexo(totales_fila, "lista_extranjero_hombres", es_lista = TRUE)
      lista_extranjero_mujeres <- extraer_col_sexo(totales_fila, "lista_extranjero_mujeres", es_lista = TRUE)
      
      padron_extranjero_no_binario <- extraer_col_sexo(totales_fila, "padron_extranjero_no_binario", es_lista = TRUE)
      lista_extranjero_no_binario <- extraer_col_sexo(totales_fila, "lista_extranjero_no_binario", es_lista = TRUE)
      
      if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
        registro <- data.frame(
          fecha = as.Date(fecha, origin = "1970-01-01"),
          padron_nacional = padron_nacional,
          padron_extranjero = ifelse(is.na(padron_extranjero), NA, padron_extranjero),
          lista_nacional = lista_nacional,
          lista_extranjero = ifelse(is.na(lista_extranjero), NA, lista_extranjero),
          padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
          lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
          padron_hombres = padron_hombres,
          padron_mujeres = padron_mujeres,
          lista_hombres = lista_hombres,
          lista_mujeres = lista_mujeres,
          padron_nacional_no_binario = padron_nacional_no_binario,
          lista_nacional_no_binario = lista_nacional_no_binario,
          padron_extranjero_hombres = padron_extranjero_hombres,
          padron_extranjero_mujeres = padron_extranjero_mujeres,
          lista_extranjero_hombres = lista_extranjero_hombres,
          lista_extranjero_mujeres = lista_extranjero_mujeres,
          padron_extranjero_no_binario = padron_extranjero_no_binario,
          lista_extranjero_no_binario = lista_extranjero_no_binario,
          stringsAsFactors = FALSE
        )
      }
      
    } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
      # Agregar desde dataframe
      df <- datos_temp$datos
      
      padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
      padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
      lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
      lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
      
      registro <- data.frame(
        fecha = as.Date(fecha, origin = "1970-01-01"),
        padron_nacional = padron_nacional,
        padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
        lista_nacional = lista_nacional,
        lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
        padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
        lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
        padron_hombres = extraer_col_sexo(df, "padron_hombres", "padron_nacional_hombres"),
        padron_mujeres = extraer_col_sexo(df, "padron_mujeres", "padron_nacional_mujeres"),
        lista_hombres = extraer_col_sexo(df, "lista_hombres", "lista_nacional_hombres"),
        lista_mujeres = extraer_col_sexo(df, "lista_mujeres", "lista_nacional_mujeres"),
        padron_nacional_no_binario = extraer_col_sexo(df, "padron_nacional_no_binario"),
        lista_nacional_no_binario = extraer_col_sexo(df, "lista_nacional_no_binario"),
        padron_extranjero_hombres = extraer_col_sexo(df, "padron_extranjero_hombres"),
        padron_extranjero_mujeres = extraer_col_sexo(df, "padron_extranjero_mujeres"),
        lista_extranjero_hombres = extraer_col_sexo(df, "lista_extranjero_hombres"),
        lista_extranjero_mujeres = extraer_col_sexo(df, "lista_extranjero_mujeres"),
        padron_extranjero_no_binario = extraer_col_sexo(df, "padron_extranjero_no_binario"),
        lista_extranjero_no_binario = extraer_col_sexo(df, "lista_extranjero_no_binario"),
        stringsAsFactors = FALSE
      )
    }
    
    return(registro)
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # HELPER: Cargar datos de múltiples fechas
  # ════════════════════════════════════════════════════════════════════════════
  
  cargar_datos_fechas <- function(fechas, filtros, descripcion = "") {
    lista_datos <- list()
    
    estado_filtro <- if (filtros$entidad == "Nacional") "Nacional" else filtros$entidad
    
    message("📥 Cargando ", length(fechas), " fechas ", descripcion)
    
    for (i in seq_along(fechas)) {
      fecha <- fechas[i]
      
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = filtros$distrito,
          municipio = filtros$municipio,
          seccion = filtros$seccion,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("   ⚠️ Error cargando ", fecha, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        registro <- crear_registro(fecha, datos_temp, filtros)
        if (!is.null(registro)) {
          lista_datos[[length(lista_datos) + 1]] <- registro
          message("   ✅ ", format(fecha, "%Y-%m-%d"))
        }
      }
    }
    
    if (length(lista_datos) == 0) return(NULL)
    
    # Unificar columnas
    if (length(lista_datos) > 1) {
      cols_primer_df <- names(lista_datos[[1]])
      for (i in 2:length(lista_datos)) {
        for (col in setdiff(cols_primer_df, names(lista_datos[[i]]))) {
          lista_datos[[i]][[col]] <- NA
        }
        lista_datos[[i]] <- lista_datos[[i]][, cols_primer_df]
      }
    }
    
    datos_completos <- do.call(rbind, lista_datos)
    datos_completos <- datos_completos[order(datos_completos$fecha), ]
    
    return(datos_completos)
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: DATOS DEL AÑO ACTUAL (PARA GRÁFICA 1)
  # ════════════════════════════════════════════════════════════════════════════
  # 
  # Carga TODOS los meses disponibles del año del último archivo
  # Ejemplo: Si último archivo es jul 2025, carga ene-jul 2025 (7 archivos)
  # ════════════════════════════════════════════════════════════════════════════
  
  datos_year_actual <- reactive({
    año_ultimo <- anio_actual()
    estado_actual <- estado_app()
    
    # Determinar filtros según estado
    if (estado_actual == "restablecido") {
      filtros <- list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas",
        ambito = "nacional"
      )
    } else {
      filtros <- list(
        entidad = input$entidad %||% "Nacional",
        distrito = input$distrito %||% "Todos",
        municipio = input$municipio %||% "Todos",
        seccion = input$seccion %||% "Todas",
        ambito = input$ambito_datos %||% "nacional"
      )
    }
    
    # Verificar si es consulta nacional sin filtros (cacheable)
    es_nacional_sin_filtros <- (
      filtros$entidad == "Nacional" && 
        filtros$distrito == "Todos" && 
        filtros$municipio == "Todos" && 
        (is.null(filtros$seccion) || "Todas" %in% filtros$seccion || length(filtros$seccion) == 0) &&
        filtros$ambito == "nacional"
    )
    
    message("🔵 [datos_year_actual] Año: ", año_ultimo, " | Estado: ", estado_actual)
    
    # Verificar caché
    cache <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
    
    if (es_nacional_sin_filtros &&
        !is.null(cache$datos_year_actual) && 
        !is.null(cache$año_cacheado) &&
        cache$año_cacheado == año_ultimo &&
        cache_valido(cache$timestamp_year, max_horas = 24)) {
      
      message("✅ [CACHÉ HIT] Datos del año ", año_ultimo, " desde caché")
      return(cache$datos_year_actual)
    }
    
    # Cargar desde archivos
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año_ultimo]
    
    if (length(fechas_año) == 0) {
      message("⚠️ Sin fechas para año ", año_ultimo)
      return(NULL)
    }
    
    datos <- cargar_datos_fechas(fechas_año, filtros, paste0("del año ", año_ultimo))
    
    if (!is.null(datos)) {
      message("✅ [datos_year_actual] ", nrow(datos), " registros cargados")
      
      # Guardar en caché si es nacional sin filtros
      if (es_nacional_sin_filtros) {
        cache$datos_year_actual <- datos
        cache$timestamp_year <- Sys.time()
        cache$año_cacheado <- año_ultimo
        assign("LNE_CACHE_GRAFICAS", cache, envir = .GlobalEnv)
        message("💾 Datos del año ", año_ultimo, " cacheados")
      }
    }
    
    return(datos)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: DATOS ANUALES 2017 → AÑO ÚLTIMO ARCHIVO (PARA GRÁFICAS 2-3)
  # ════════════════════════════════════════════════════════════════════════════
  #
  # OPTIMIZACIÓN v3.0: Carga solo 1 archivo por año (el último del año)
  # Los datos ya vienen acumulados desde el origen
  # ════════════════════════════════════════════════════════════════════════════
  
  datos_anuales_completos <- reactive({
    año_ultimo <- anio_actual()
    estado_actual <- estado_app()
    
    # Determinar filtros según estado
    if (estado_actual == "restablecido") {
      filtros <- list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas",
        ambito = "nacional"
      )
    } else {
      filtros <- list(
        entidad = input$entidad %||% "Nacional",
        distrito = input$distrito %||% "Todos",
        municipio = input$municipio %||% "Todos",
        seccion = input$seccion %||% "Todas",
        ambito = input$ambito_datos %||% "nacional"
      )
    }
    
    es_nacional_sin_filtros <- (
      filtros$entidad == "Nacional" && 
        filtros$distrito == "Todos" && 
        filtros$municipio == "Todos" && 
        (is.null(filtros$seccion) || "Todas" %in% filtros$seccion || length(filtros$seccion) == 0) &&
        filtros$ambito == "nacional"
    )
    
    message("🔵 [datos_anuales_completos] 2017 → ", año_ultimo, " | Estado: ", estado_actual)
    
    # Verificar caché
    cache <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
    
    if (es_nacional_sin_filtros &&
        !is.null(cache$datos_anuales) &&
        cache_valido(cache$timestamp_anuales, max_horas = 24)) {
      
      message("✅ [CACHÉ HIT] Datos anuales desde caché")
      return(cache$datos_anuales)
    }
    
    # Cargar desde archivos
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    años <- 2017:año_ultimo
    
    lista_anuales <- list()
    estado_filtro <- if (filtros$entidad == "Nacional") "Nacional" else filtros$entidad
    
    message("📥 [datos_anuales_completos] Cargando último archivo de cada año (", length(años), " años)")
    
    for (año in años) {
      fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año]
      
      if (length(fechas_año) == 0) {
        message("   ⚠️ Sin fechas para ", año)
        next
      }
      
      # ✅ OPTIMIZACIÓN: Solo cargar el ÚLTIMO archivo del año
      ultima_fecha <- max(fechas_año)
      
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = filtros$distrito,
          municipio = filtros$municipio,
          seccion = filtros$seccion,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("   ❌ Error cargando ", año, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        registro <- crear_registro(ultima_fecha, datos_temp, filtros)
        
        if (!is.null(registro)) {
          # Agregar columna de año
          registro$año <- as.character(año)
          lista_anuales[[length(lista_anuales) + 1]] <- registro
          message("   ✅ ", año, " (", format(ultima_fecha, "%Y-%m-%d"), ")")
        }
      }
    }
    
    if (length(lista_anuales) == 0) {
      message("⚠️ No se cargaron datos anuales")
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_anuales)
    
    message("✅ [datos_anuales_completos] ", nrow(datos_completos), " años cargados")
    
    # Guardar en caché si es nacional sin filtros
    if (es_nacional_sin_filtros) {
      cache$datos_anuales <- datos_completos
      cache$timestamp_anuales <- Sys.time()
      assign("LNE_CACHE_GRAFICAS", cache, envir = .GlobalEnv)
      message("💾 Datos anuales cacheados")
    }
    
    return(datos_completos)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: DATOS DEL AÑO CONSULTADO (PARA GRÁFICAS 4-5)
  # ════════════════════════════════════════════════════════════════════════════
  #
  # LAZY LOADING: Solo se carga cuando el usuario consulta un año diferente
  # al año del último archivo disponible
  # ════════════════════════════════════════════════════════════════════════════
  
  datos_year_consulta <- reactive({
    req(input$tipo_corte == "historico")
    
    año_consultado_val <- anio_consultado()
    año_ultimo <- anio_actual()
    
    # Si es el mismo año que el último archivo, reutilizar datos_year_actual
    if (año_consultado_val == año_ultimo) {
      message("✅ [datos_year_consulta] Mismo año que último archivo, usando datos_year_actual")
      return(datos_year_actual())
    }
    
    # Obtener filtros
    filtros <- list(
      entidad = isolate(input$entidad %||% "Nacional"),
      distrito = isolate(input$distrito %||% "Todos"),
      municipio = isolate(input$municipio %||% "Todos"),
      seccion = isolate(input$seccion %||% "Todas"),
      ambito = isolate(input$ambito_datos %||% "nacional")
    )
    
    message("🔵 [datos_year_consulta] Cargando año ", año_consultado_val, " (diferente a ", año_ultimo, ")")
    
    # Cargar desde archivos
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año_consultado_val]
    
    if (length(fechas_año) == 0) {
      message("⚠️ Sin fechas para año ", año_consultado_val)
      return(NULL)
    }
    
    datos <- cargar_datos_fechas(fechas_año, filtros, paste0("del año consultado ", año_consultado_val))
    
    if (!is.null(datos)) {
      message("✅ [datos_year_consulta] ", nrow(datos), " registros cargados para ", año_consultado_val)
    }
    
    return(datos)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ════════════════════════════════════════════════════════════════════════════
  # RETORNAR LISTA DE REACTIVES
  # ════════════════════════════════════════════════════════════════════════════
  
  message("✅ graficas_data_loaders v3.0 inicializado (LAZY LOADING)")
  message("   📊 datos_year_actual: Todos los meses del año del último archivo")
  message("   📊 datos_anuales_completos: Solo último archivo por año (2017→último)")
  message("   📊 datos_year_consulta: Carga bajo demanda para años anteriores")
  
  return(list(
    datos_year_actual = datos_year_actual,
    datos_year_consulta = datos_year_consulta,
    datos_anuales_completos = datos_anuales_completos
  ))
}
