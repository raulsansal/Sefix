# modules/lista_nominal_graficas/graficas_data_loaders.R
# Reactives de carga de datos: year_actual, year_consulta, anuales
# Versión: 2.3 - CORRECCIÓN: Eliminado parámetro solo_extranjero (conflicto conceptual)

graficas_data_loaders <- function(input, output, session, anio_actual, anio_consultado, filtros_usuario, estado_app) {
  
  message("📥 Inicializando graficas_data_loaders v2.3")
  
  # Obtener función cache_valido del entorno padre
  cache_valido <- function(timestamp, max_horas = 24) {
    if (is.null(timestamp)) return(FALSE)
    difftime(Sys.time(), timestamp, units = "hours") < max_horas
  }
  
  # ========== REACTIVE: DATOS DEL AÑO ACTUAL (PARA GRÁFICAS 1, 2, 3) ==========
  
  datos_year_actual <- reactive({
    año_actual_valor <- anio_actual()
    
    # ✅ AISLAR INPUTS PARA EVITAR REACTIVIDAD NO DESEADA
    filtros <- list(
      entidad = isolate(input$entidad %||% "Nacional"),
      distrito = isolate(input$distrito %||% "Todos"),
      municipio = isolate(input$municipio %||% "Todos"),
      seccion = isolate(input$seccion %||% "Todas"),
      ambito = isolate(input$ambito_datos %||% "nacional")
    )
    
    # ========== DETERMINAR SI USAR CACHÉ ==========
    es_nacional_sin_filtros <- (
      filtros$entidad == "Nacional" && 
        filtros$distrito == "Todos" && 
        filtros$municipio == "Todos" && 
        (is.null(filtros$seccion) || "Todas" %in% filtros$seccion || length(filtros$seccion) == 0) &&
        filtros$ambito == "nacional"
    )
    
    cache <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
    
    message("🔵 [datos_year_actual] Solicitando datos del año actual: ", año_actual_valor)
    message("   Estado app: ", estado_app())
    message("   Ámbito: ", filtros$ambito)
    message("   Es Nacional sin filtros: ", es_nacional_sin_filtros)
    
    # ========== VERIFICAR CACHÉ ==========
    if (es_nacional_sin_filtros &&
        !is.null(cache$datos_year_actual) && 
        !is.null(cache$año_cacheado) &&
        cache$año_cacheado == año_actual_valor &&
        cache_valido(cache$timestamp_year, max_horas = 24)) {
      
      message("✅ [CACHÉ HIT] Usando datos cacheados del año actual ", año_actual_valor)
      return(cache$datos_year_actual)
    }
    
    message("📥 [CACHÉ MISS O FILTROS] Cargando datos del año actual ", año_actual_valor, " desde archivos...")
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas_anio_actual <- catalog$historico[format(catalog$historico, "%Y") == año_actual_valor]
    
    if (length(fechas_anio_actual) == 0) {
      message("⚠️ Sin fechas para año actual")
      return(NULL)
    }
    
    message("📥 Cargando ", length(fechas_anio_actual), " fechas del año ", año_actual_valor)
    
    lista_datos <- list()
    
    # ✅ CORRECCIÓN v2.3: Los filtros son iguales para ambos ámbitos
    estado_filtro <- if (filtros$entidad == "Nacional") "Nacional" else filtros$entidad
    message("   📍 Estado: ", estado_filtro, " | Ámbito: ", filtros$ambito)
    
    for (i in seq_along(fechas_anio_actual)) {
      fecha <- fechas_anio_actual[i]
      
      # ✅ v2.3: Filtros normales, sin solo_extranjero
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
        message("⚠️ Error cargando fecha ", fecha, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
          totales_fila <- datos_temp$totales
          
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          padron_extranjero <- if ("padron_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
          } else NA_real_
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          lista_extranjero <- if ("lista_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
          } else NA_real_
          
          padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres)))
          } else NA
          
          padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres)))
          } else NA
          
          lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres)))
          } else NA
          
          lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres)))
          } else NA
          
          padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_hombres)))
          } else NA
          
          padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_mujeres)))
          } else NA
          
          lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_hombres)))
          } else NA
          
          lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_mujeres)))
          } else NA
          
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
              padron_extranjero_hombres = padron_extranjero_hombres,
              padron_extranjero_mujeres = padron_extranjero_mujeres,
              lista_extranjero_hombres = lista_extranjero_hombres,
              lista_extranjero_mujeres = lista_extranjero_mujeres,
              stringsAsFactors = FALSE
            )
            
            message("   ✅ ", format(fecha, "%Y-%m-%d"))
            lista_datos[[length(lista_datos) + 1]] <- registro
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          df <- datos_temp$datos
          
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          padron_hombres <- if ("padron_nacional_hombres" %in% colnames(df)) {
            sum(df$padron_nacional_hombres, na.rm = TRUE)
          } else NA
          
          padron_mujeres <- if ("padron_nacional_mujeres" %in% colnames(df)) {
            sum(df$padron_nacional_mujeres, na.rm = TRUE)
          } else NA
          
          lista_hombres <- if ("lista_nacional_hombres" %in% colnames(df)) {
            sum(df$lista_nacional_hombres, na.rm = TRUE)
          } else NA
          
          lista_mujeres <- if ("lista_nacional_mujeres" %in% colnames(df)) {
            sum(df$lista_nacional_mujeres, na.rm = TRUE)
          } else NA
          
          padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% colnames(df)) {
            sum(df$padron_extranjero_hombres, na.rm = TRUE)
          } else NA
          
          padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% colnames(df)) {
            sum(df$padron_extranjero_mujeres, na.rm = TRUE)
          } else NA
          
          lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% colnames(df)) {
            sum(df$lista_extranjero_hombres, na.rm = TRUE)
          } else NA
          
          lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% colnames(df)) {
            sum(df$lista_extranjero_mujeres, na.rm = TRUE)
          } else NA
          
          registro <- data.frame(
            fecha = as.Date(fecha, origin = "1970-01-01"),
            padron_nacional = padron_nacional,
            padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
            lista_nacional = lista_nacional,
            lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
            padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
            lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
            padron_hombres = padron_hombres,
            padron_mujeres = padron_mujeres,
            lista_hombres = lista_hombres,
            lista_mujeres = lista_mujeres,
            padron_extranjero_hombres = padron_extranjero_hombres,
            padron_extranjero_mujeres = padron_extranjero_mujeres,
            lista_extranjero_hombres = lista_extranjero_hombres,
            lista_extranjero_mujeres = lista_extranjero_mujeres,
            stringsAsFactors = FALSE
          )
          
          message("   ✅ ", format(fecha, "%Y-%m-%d"), " (sumado desde ", nrow(df), " filas)")
          lista_datos[[length(lista_datos) + 1]] <- registro
        }
      }
    }
    
    if (length(lista_datos) == 0) {
      return(NULL)
    }
    
    if (length(lista_datos) > 1) {
      cols_primer_df <- names(lista_datos[[1]])
      
      for (i in 2:length(lista_datos)) {
        cols_actual <- names(lista_datos[[i]])
        
        if (!identical(cols_primer_df, cols_actual)) {
          cols_faltantes <- setdiff(cols_primer_df, cols_actual)
          if (length(cols_faltantes) > 0) {
            for (col in cols_faltantes) {
              lista_datos[[i]][[col]] <- NA
            }
          }
          lista_datos[[i]] <- lista_datos[[i]][, cols_primer_df]
        }
      }
    }
    
    datos_completos <- do.call(rbind, lista_datos)
    datos_completos <- datos_completos[order(datos_completos$fecha), ]
    
    message("✅ Datos del año actual cargados: ", nrow(datos_completos), " registros")
    
    if (es_nacional_sin_filtros) {
      cache$datos_year_actual <- datos_completos
      cache$timestamp_year <- Sys.time()
      cache$año_cacheado <- año_actual_valor
      assign("LNE_CACHE_GRAFICAS", cache, envir = .GlobalEnv)
      message("💾 Datos del año actual cacheados")
    }
    
    return(datos_completos)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: DATOS DEL AÑO CONSULTADO (PARA GRÁFICAS 4, 5) ==========
  # ✅ CORRECCIÓN CRÍTICA: Eliminar req(input$year) para evitar reactividad prematura
  
  datos_year_consulta <- reactive({
    req(input$tipo_corte == "historico")
    
    # ✅ CRÍTICO: Aislar TODOS los inputs desde el principio para evitar pestañeo
    year <- isolate(input$year)
    entidad <- isolate(input$entidad)
    distrito <- isolate(input$distrito %||% "Todos")
    municipio <- isolate(input$municipio %||% "Todos")
    seccion <- isolate(input$seccion %||% "Todas")
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔄 [datos_year_consulta] CONSULTA - Año ", year, ", Ámbito: ", ambito)
    
    # Validar que year no sea NULL
    if (is.null(year)) {
      message("⚠️ [datos_year_consulta] year es NULL, usando año actual")
      year <- anio_actual()
    }
    
    # Si es año actual + nacional + sin filtros → redirigir a datos_year_actual
    if (year == anio_actual() && 
        ambito == "nacional" &&
        entidad == "Nacional" && 
        distrito == "Todos" && 
        municipio == "Todos" && 
        (is.null(seccion) || length(seccion) == 0 || "Todas" %in% seccion)) {
      
      message("✅ Redirigiendo a datos_year_actual()")
      return(datos_year_actual())
    }
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas_year <- catalog$historico[format(catalog$historico, "%Y") == year]
    
    if (length(fechas_year) == 0) {
      return(NULL)
    }
    
    # ✅ CORRECCIÓN v2.3: Los filtros son iguales para ambos ámbitos
    estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
    message("📍 Estado: ", estado_filtro, " | Ámbito: ", ambito)
    
    message("📥 Cargando ", length(fechas_year), " fechas del año ", year)
    
    lista_datos <- list()
    
    for (i in seq_along(fechas_year)) {
      fecha <- fechas_year[i]
      
      # ✅ v2.3: Filtros normales, sin solo_extranjero
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = distrito,
          municipio = municipio,
          seccion = seccion,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("⚠️ Error cargando fecha ", fecha, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
          totales_fila <- datos_temp$totales
          
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          padron_extranjero <- if ("padron_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
          } else NA_real_
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          lista_extranjero <- if ("lista_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
          } else NA_real_
          
          padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres)))
          } else NA
          
          padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres)))
          } else NA
          
          lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres)))
          } else NA
          
          lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres)))
          } else NA
          
          padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_hombres)))
          } else NA
          
          padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_mujeres)))
          } else NA
          
          lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_hombres)))
          } else NA
          
          lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_mujeres)))
          } else NA
          
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
              padron_extranjero_hombres = padron_extranjero_hombres,
              padron_extranjero_mujeres = padron_extranjero_mujeres,
              lista_extranjero_hombres = lista_extranjero_hombres,
              lista_extranjero_mujeres = lista_extranjero_mujeres,
              stringsAsFactors = FALSE
            )
            
            lista_datos[[length(lista_datos) + 1]] <- registro
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          df <- datos_temp$datos
          
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          padron_hombres <- if ("padron_nacional_hombres" %in% colnames(df)) {
            sum(df$padron_nacional_hombres, na.rm = TRUE)
          } else NA
          
          padron_mujeres <- if ("padron_nacional_mujeres" %in% colnames(df)) {
            sum(df$padron_nacional_mujeres, na.rm = TRUE)
          } else NA
          
          lista_hombres <- if ("lista_nacional_hombres" %in% colnames(df)) {
            sum(df$lista_nacional_hombres, na.rm = TRUE)
          } else NA
          
          lista_mujeres <- if ("lista_nacional_mujeres" %in% colnames(df)) {
            sum(df$lista_nacional_mujeres, na.rm = TRUE)
          } else NA
          
          padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% colnames(df)) {
            sum(df$padron_extranjero_hombres, na.rm = TRUE)
          } else NA
          
          padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% colnames(df)) {
            sum(df$padron_extranjero_mujeres, na.rm = TRUE)
          } else NA
          
          lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% colnames(df)) {
            sum(df$lista_extranjero_hombres, na.rm = TRUE)
          } else NA
          
          lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% colnames(df)) {
            sum(df$lista_extranjero_mujeres, na.rm = TRUE)
          } else NA
          
          registro <- data.frame(
            fecha = as.Date(fecha, origin = "1970-01-01"),
            padron_nacional = padron_nacional,
            padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
            lista_nacional = lista_nacional,
            lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
            padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
            lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
            padron_hombres = padron_hombres,
            padron_mujeres = padron_mujeres,
            lista_hombres = lista_hombres,
            lista_mujeres = lista_mujeres,
            padron_extranjero_hombres = padron_extranjero_hombres,
            padron_extranjero_mujeres = padron_extranjero_mujeres,
            lista_extranjero_hombres = lista_extranjero_hombres,
            lista_extranjero_mujeres = lista_extranjero_mujeres,
            stringsAsFactors = FALSE
          )
          
          message("   ✅ ", format(fecha, "%Y-%m-%d"), " (sumado desde ", nrow(df), " filas)")
          lista_datos[[length(lista_datos) + 1]] <- registro
        }
      }
    }
    
    if (length(lista_datos) == 0) {
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_datos)
    datos_completos <- datos_completos[order(datos_completos$fecha), ]
    
    message("✅ Datos del año ", year, " cargados: ", nrow(datos_completos), " registros")
    
    return(datos_completos)
  }) %>% 
    bindCache(input$btn_consultar, input$tipo_corte, input$year, input$entidad, 
              input$distrito, input$municipio, input$seccion, input$ambito_datos) %>%
    # ✅ CORRECCIÓN CRÍTICA: Solo se ejecuta cuando se presiona el botón o cambia el ámbito
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: DATOS ANUALES (2017-ACTUAL) ==========
  
  datos_anuales_completos <- reactive({
    
    filtros <- list(
      entidad = isolate(input$entidad %||% "Nacional"),
      distrito = isolate(input$distrito %||% "Todos"),
      municipio = isolate(input$municipio %||% "Todos"),
      seccion = isolate(input$seccion %||% "Todas"),
      ambito = isolate(input$ambito_datos %||% "nacional")
    )
    
    es_nacional_sin_filtros <- (
      filtros$entidad == "Nacional" && 
        filtros$distrito == "Todos" && 
        filtros$municipio == "Todos" && 
        (is.null(filtros$seccion) || "Todas" %in% filtros$seccion || length(filtros$seccion) == 0) &&
        filtros$ambito == "nacional"
    )
    
    cache <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
    
    año_actual_valor <- anio_actual()
    
    if (input$btn_consultar == 0) {
      message("🚀 [datos_anuales_completos] CARGA INICIAL - Evolución 2017-", año_actual_valor)
    } else {
      message("🔄 [datos_anuales_completos] CONSULTA - Evolución 2017-", año_actual_valor)
    }
    
    if (es_nacional_sin_filtros &&
        !is.null(cache$datos_anuales) &&
        cache_valido(cache$timestamp_anuales, max_horas = 24)) {
      
      message("✅ [CACHÉ HIT] Usando datos anuales cacheados")
      return(cache$datos_anuales)
    }
    
    message("📥 [CACHÉ MISS O FILTROS] Cargando datos anuales desde archivos...")
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    años <- 2017:año_actual_valor
    
    lista_anuales <- list()
    
    # ✅ CORRECCIÓN v2.3: Los filtros son iguales para ambos ámbitos
    estado_filtro <- if (filtros$entidad == "Nacional") "Nacional" else filtros$entidad
    message("   📍 Estado: ", estado_filtro, " | Ámbito: ", filtros$ambito)
    
    for (año in años) {
      message("🔍 Procesando año: ", año)
      
      fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año]
      
      if (length(fechas_año) == 0) {
        message("   ⚠️ Sin fechas para año ", año)
        next
      }
      
      ultima_fecha <- max(fechas_año)
      message("   📅 Última fecha del año ", año, ": ", as.Date(ultima_fecha, origin = "1970-01-01"))
      
      # ✅ v2.3: Filtros normales, sin solo_extranjero
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
        message("   ❌ Error en cargar_lne para año ", año, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
          totales_fila <- datos_temp$totales
          
          message("   ✅ Fila totales obtenida para año ", año)
          
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          
          padron_extranjero <- if ("padron_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
          } else NA_real_
          
          lista_extranjero <- if ("lista_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
          } else NA_real_
          
          padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres)))
          } else NA
          
          padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres)))
          } else NA
          
          lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres)))
          } else NA
          
          lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres)))
          } else NA
          
          padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_hombres)))
          } else NA
          
          padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_mujeres)))
          } else NA
          
          lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_hombres)))
          } else NA
          
          lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_mujeres)))
          } else NA
          
          if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
            lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
              año = as.character(año),
              fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
              padron_nacional = padron_nacional,
              padron_extranjero = padron_extranjero,
              lista_nacional = lista_nacional,
              lista_extranjero = lista_extranjero,
              padron_hombres = padron_hombres,
              padron_mujeres = padron_mujeres,
              lista_hombres = lista_hombres,
              lista_mujeres = lista_mujeres,
              padron_extranjero_hombres = padron_extranjero_hombres,
              padron_extranjero_mujeres = padron_extranjero_mujeres,
              lista_extranjero_hombres = lista_extranjero_hombres,
              lista_extranjero_mujeres = lista_extranjero_mujeres,
              stringsAsFactors = FALSE
            )
            
            padron_electoral <- padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero)
            lista_nominal <- lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero)
            
            message("   ✅ ", año, " | Padrón: ", format(padron_electoral, big.mark = ","),
                    " | Lista: ", format(lista_nominal, big.mark = ","))
          } else {
            message("   ❌ Valores principales son NA para año ", año)
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          df <- datos_temp$datos
          
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
            año = as.character(año),
            fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
            padron_nacional = padron_nacional,
            padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
            lista_nacional = lista_nacional,
            lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
            padron_hombres = if ("padron_nacional_hombres" %in% colnames(df)) sum(df$padron_nacional_hombres, na.rm = TRUE) else NA,
            padron_mujeres = if ("padron_nacional_mujeres" %in% colnames(df)) sum(df$padron_nacional_mujeres, na.rm = TRUE) else NA,
            lista_hombres = if ("lista_nacional_hombres" %in% colnames(df)) sum(df$lista_nacional_hombres, na.rm = TRUE) else NA,
            lista_mujeres = if ("lista_nacional_mujeres" %in% colnames(df)) sum(df$lista_nacional_mujeres, na.rm = TRUE) else NA,
            padron_extranjero_hombres = if ("padron_extranjero_hombres" %in% colnames(df)) sum(df$padron_extranjero_hombres, na.rm = TRUE) else NA,
            padron_extranjero_mujeres = if ("padron_extranjero_mujeres" %in% colnames(df)) sum(df$padron_extranjero_mujeres, na.rm = TRUE) else NA,
            lista_extranjero_hombres = if ("lista_extranjero_hombres" %in% colnames(df)) sum(df$lista_extranjero_hombres, na.rm = TRUE) else NA,
            lista_extranjero_mujeres = if ("lista_extranjero_mujeres" %in% colnames(df)) sum(df$lista_extranjero_mujeres, na.rm = TRUE) else NA,
            stringsAsFactors = FALSE
          )
          
          message("   ✅ ", año, " (sumado desde ", nrow(df), " filas)")
        }
      }
    }
    
    if (length(lista_anuales) == 0) {
      message("⚠️ No se cargaron datos anuales")
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_anuales)
    
    message("✅ Datos anuales cargados: ", nrow(datos_completos), " años (2017-", año_actual_valor, ")")
    
    if (es_nacional_sin_filtros) {
      cache$datos_anuales <- datos_completos
      cache$timestamp_anuales <- Sys.time()
      assign("LNE_CACHE_GRAFICAS", cache, envir = .GlobalEnv)
      message("💾 Datos anuales cacheados")
    }
    
    return(datos_completos)
  }) %>% 
    bindCache(input$btn_consultar, input$tipo_corte, input$ambito_datos) %>%
    bindEvent(input$btn_consultar, estado_app(), input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== RETORNAR LISTA DE REACTIVES ==========
  
  message("✅ graficas_data_loaders v2.3 inicializado (eliminado solo_extranjero)")
  
  return(list(
    datos_year_actual = datos_year_actual,
    datos_year_consulta = datos_year_consulta,
    datos_anuales_completos = datos_anuales_completos
  ))
}