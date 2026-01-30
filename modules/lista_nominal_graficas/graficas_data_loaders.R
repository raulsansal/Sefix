# modules/lista_nominal_graficas/graficas_data_loaders.R
# Reactives de carga de datos: year_actual, year_consulta, anuales
# Versión: 2.11 - CORRECCIÓN CRÍTICA: bindCache incluye estado_app para invalidar en restablecer

graficas_data_loaders <- function(input, output, session, anio_actual, anio_consultado, filtros_usuario, estado_app) {
  
  message("📥 Inicializando graficas_data_loaders v2.11")
  
  # Obtener función cache_valido del entorno padre
  cache_valido <- function(timestamp, max_horas = 24) {
    if (is.null(timestamp)) return(FALSE)
    difftime(Sys.time(), timestamp, units = "hours") < max_horas
  }
  
  # ========== REACTIVE: DATOS DEL AÑO ACTUAL (PARA GRÁFICAS 1, 2, 3) ==========
  
  datos_year_actual <- reactive({
    año_actual_valor <- anio_actual()
    estado_actual <- estado_app()
    
    # ✅ SOLUCIÓN DEFINITIVA: En estado restablecido, usar valores por defecto HARDCODEADOS
    if (estado_actual == "restablecido") {
      filtros <- list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas",
        ambito = "nacional"
      )
      
      # ✅ CRÍTICO: Invalidar caché en estado restablecido
      if (exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
        cache_actual <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
        cache_actual$datos_year_actual <- NULL
        cache_actual$timestamp_year <- NULL
        cache_actual$año_cacheado <- NULL
        assign("LNE_CACHE_GRAFICAS", cache_actual, envir = .GlobalEnv)
      }
      
    } else {
      # En estado consultado, leer inputs normalmente
      filtros <- list(
        entidad = input$entidad %||% "Nacional",
        distrito = input$distrito %||% "Todos",
        municipio = input$municipio %||% "Todos",
        seccion = input$seccion %||% "Todas",
        ambito = input$ambito_datos %||% "nacional"
      )
    }
    
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
    
    estado_filtro <- if (filtros$entidad == "Nacional") "Nacional" else filtros$entidad
    message("   📍 Estado: ", estado_filtro, " | Ámbito: ", filtros$ambito)
    
    for (i in seq_along(fechas_anio_actual)) {
      fecha <- fechas_anio_actual[i]
      
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
          
          # ========== EXTRACCIÓN COLUMNAS PRINCIPALES ==========
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          padron_extranjero <- if ("padron_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
          } else NA_real_
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          lista_extranjero <- if ("lista_extranjero" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
          } else NA_real_
          
          # ========== EXTRACCIÓN COLUMNAS SEXO NACIONAL ==========
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
          
          # ========== COLUMNAS NO BINARIO NACIONAL ==========
          padron_nacional_no_binario <- if ("padron_nacional_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_no_binario)))
          } else NA
          
          lista_nacional_no_binario <- if ("lista_nacional_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_no_binario)))
          } else NA
          
          # ========== EXTRACCIÓN COLUMNAS SEXO EXTRANJERO ==========
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
          
          # ========== COLUMNAS NO BINARIO EXTRANJERO ==========
          padron_extranjero_no_binario <- if ("padron_extranjero_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_no_binario)))
          } else NA
          
          lista_extranjero_no_binario <- if ("lista_extranjero_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_no_binario)))
          } else NA
          
          # ========== CREAR REGISTRO CON TODAS LAS COLUMNAS ==========
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
            
            message("   ✅ ", format(fecha, "%Y-%m-%d"))
            lista_datos[[length(lista_datos) + 1]] <- registro
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          # ========== AGREGACIÓN DESDE DATAFRAME ==========
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
          
          padron_nacional_no_binario <- if ("padron_nacional_no_binario" %in% colnames(df)) {
            sum(df$padron_nacional_no_binario, na.rm = TRUE)
          } else NA
          
          lista_nacional_no_binario <- if ("lista_nacional_no_binario" %in% colnames(df)) {
            sum(df$lista_nacional_no_binario, na.rm = TRUE)
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
          
          padron_extranjero_no_binario <- if ("padron_extranjero_no_binario" %in% colnames(df)) {
            sum(df$padron_extranjero_no_binario, na.rm = TRUE)
          } else NA
          
          lista_extranjero_no_binario <- if ("lista_extranjero_no_binario" %in% colnames(df)) {
            sum(df$lista_extranjero_no_binario, na.rm = TRUE)
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
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: DATOS DEL AÑO CONSULTADO (PARA GRÁFICAS 4, 5) ==========
  
  datos_year_consulta <- reactive({
    req(input$tipo_corte == "historico")
    
    year <- isolate(input$year)
    entidad <- isolate(input$entidad)
    distrito <- isolate(input$distrito %||% "Todos")
    municipio <- isolate(input$municipio %||% "Todos")
    seccion <- isolate(input$seccion %||% "Todas")
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔄 [datos_year_consulta] CONSULTA - Año ", year, ", Ámbito: ", ambito)
    
    if (is.null(year)) {
      message("⚠️ [datos_year_consulta] year es NULL, usando año actual")
      year <- anio_actual()
    }
    
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
    
    estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
    message("📍 Estado: ", estado_filtro, " | Ámbito: ", ambito)
    message("📥 Cargando ", length(fechas_year), " fechas del año ", year)
    
    lista_datos <- list()
    
    for (i in seq_along(fechas_year)) {
      fecha <- fechas_year[i]
      
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
          
          # Igual que en datos_year_actual - copiar toda la lógica de extracción incluyendo NB
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
          
          padron_nacional_no_binario <- if ("padron_nacional_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_no_binario)))
          } else NA
          
          lista_nacional_no_binario <- if ("lista_nacional_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_no_binario)))
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
          
          padron_extranjero_no_binario <- if ("padron_extranjero_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_no_binario)))
          } else NA
          
          lista_extranjero_no_binario <- if ("lista_extranjero_no_binario" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_no_binario)))
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
            
            lista_datos[[length(lista_datos) + 1]] <- registro
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          df <- datos_temp$datos
          
          # Igual agregación con NB
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          padron_hombres <- if ("padron_nacional_hombres" %in% colnames(df)) sum(df$padron_nacional_hombres, na.rm = TRUE) else NA
          padron_mujeres <- if ("padron_nacional_mujeres" %in% colnames(df)) sum(df$padron_nacional_mujeres, na.rm = TRUE) else NA
          lista_hombres <- if ("lista_nacional_hombres" %in% colnames(df)) sum(df$lista_nacional_hombres, na.rm = TRUE) else NA
          lista_mujeres <- if ("lista_nacional_mujeres" %in% colnames(df)) sum(df$lista_nacional_mujeres, na.rm = TRUE) else NA
          
          padron_nacional_no_binario <- if ("padron_nacional_no_binario" %in% colnames(df)) sum(df$padron_nacional_no_binario, na.rm = TRUE) else NA
          lista_nacional_no_binario <- if ("lista_nacional_no_binario" %in% colnames(df)) sum(df$lista_nacional_no_binario, na.rm = TRUE) else NA
          
          padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% colnames(df)) sum(df$padron_extranjero_hombres, na.rm = TRUE) else NA
          padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% colnames(df)) sum(df$padron_extranjero_mujeres, na.rm = TRUE) else NA
          lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% colnames(df)) sum(df$lista_extranjero_hombres, na.rm = TRUE) else NA
          lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% colnames(df)) sum(df$lista_extranjero_mujeres, na.rm = TRUE) else NA
          
          padron_extranjero_no_binario <- if ("padron_extranjero_no_binario" %in% colnames(df)) sum(df$padron_extranjero_no_binario, na.rm = TRUE) else NA
          lista_extranjero_no_binario <- if ("lista_extranjero_no_binario" %in% colnames(df)) sum(df$lista_extranjero_no_binario, na.rm = TRUE) else NA
          
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
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: DATOS ANUALES (2017-ACTUAL) ==========
  
  datos_anuales_completos <- reactive({
    estado_actual <- estado_app()
    
    # ✅ SOLUCIÓN DEFINITIVA: En estado restablecido, usar valores por defecto HARDCODEADOS
    if (estado_actual == "restablecido") {
      filtros <- list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas",
        ambito = "nacional"
      )
      
      # ✅ CRÍTICO: Invalidar caché en estado restablecido
      if (exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
        cache_actual <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
        cache_actual$datos_anuales <- NULL
        cache_actual$timestamp_anuales <- NULL
        assign("LNE_CACHE_GRAFICAS", cache_actual, envir = .GlobalEnv)
        message("🧹 [datos_anuales_completos] Caché invalidado en estado restablecido")
      }
      
    } else {
      # En estado consultado, leer inputs normalmente
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
          
          # Igual extracción con NB
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          
          padron_extranjero <- if ("padron_extranjero" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero))) else NA_real_
          lista_extranjero <- if ("lista_extranjero" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero))) else NA_real_
          
          padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres))) else NA
          padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres))) else NA
          lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres))) else NA
          lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres))) else NA
          
          padron_nacional_no_binario <- if ("padron_nacional_no_binario" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_no_binario))) else NA
          lista_nacional_no_binario <- if ("lista_nacional_no_binario" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_no_binario))) else NA
          
          padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_hombres))) else NA
          padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_mujeres))) else NA
          lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_hombres))) else NA
          lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_mujeres))) else NA
          
          padron_extranjero_no_binario <- if ("padron_extranjero_no_binario" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_no_binario))) else NA
          lista_extranjero_no_binario <- if ("lista_extranjero_no_binario" %in% names(totales_fila)) as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_no_binario))) else NA
          
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
            
            padron_electoral <- padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero)
            lista_nominal <- lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero)
            
            message("   ✅ ", año, " | Padrón: ", format(padron_electoral, big.mark = ","),
                    " | Lista: ", format(lista_nominal, big.mark = ","))
          } else {
            message("   ❌ Valores principales son NA para año ", año)
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          df <- datos_temp$datos
          
          # Igual agregación con NB
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
            padron_nacional_no_binario = if ("padron_nacional_no_binario" %in% colnames(df)) sum(df$padron_nacional_no_binario, na.rm = TRUE) else NA,
            lista_nacional_no_binario = if ("lista_nacional_no_binario" %in% colnames(df)) sum(df$lista_nacional_no_binario, na.rm = TRUE) else NA,
            padron_extranjero_hombres = if ("padron_extranjero_hombres" %in% colnames(df)) sum(df$padron_extranjero_hombres, na.rm = TRUE) else NA,
            padron_extranjero_mujeres = if ("padron_extranjero_mujeres" %in% colnames(df)) sum(df$padron_extranjero_mujeres, na.rm = TRUE) else NA,
            lista_extranjero_hombres = if ("lista_extranjero_hombres" %in% colnames(df)) sum(df$lista_extranjero_hombres, na.rm = TRUE) else NA,
            lista_extranjero_mujeres = if ("lista_extranjero_mujeres" %in% colnames(df)) sum(df$lista_extranjero_mujeres, na.rm = TRUE) else NA,
            padron_extranjero_no_binario = if ("padron_extranjero_no_binario" %in% colnames(df)) sum(df$padron_extranjero_no_binario, na.rm = TRUE) else NA,
            lista_extranjero_no_binario = if ("lista_extranjero_no_binario" %in% colnames(df)) sum(df$lista_extranjero_no_binario, na.rm = TRUE) else NA,
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
    bindCache(estado_app(), input$btn_consultar, input$tipo_corte, input$ambito_datos) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== RETORNAR LISTA DE REACTIVES ==========
  
  message("✅ graficas_data_loaders v2.11 inicializado")
  message("   ✅ CORRECCIÓN v2.11: bindCache incluye estado_app en datos_anuales_completos")
  message("   ✅ MANTIENE v2.10: Invalidar caché + hardcodear filtros en restablecido")
  
  return(list(
    datos_year_actual = datos_year_actual,
    datos_year_consulta = datos_year_consulta,
    datos_anuales_completos = datos_anuales_completos
  ))
}
