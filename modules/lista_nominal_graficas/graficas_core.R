# modules/lista_nominal_graficas/graficas_core.R
# Reactives base: caché, filtros, año, estado
# Versión: 2.2 - CORRECCIÓN: Gráficas 1,2,3 se muestran en Nacional Y Extranjero en estado inicial

graficas_core <- function(input, output, session, estado_app) {
  
  message("📦 Inicializando graficas_core v2.2")
  
  # ========== SISTEMA DE CACHÉ GLOBAL ==========
  
  if (!exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
    message("📦 Inicializando caché global de gráficas")
    assign("LNE_CACHE_GRAFICAS", list(
      datos_year_actual = NULL,
      datos_anuales = NULL,
      timestamp_year = NULL,
      timestamp_anuales = NULL,
      año_cacheado = NULL
    ), envir = .GlobalEnv)
  }
  
  # ========== FUNCIÓN AUXILIAR: VALIDAR CACHÉ ==========
  
  cache_valido <- function(timestamp, max_horas = 24) {
    if (is.null(timestamp)) return(FALSE)
    difftime(Sys.time(), timestamp, units = "hours") < max_horas
  }
  
  # ========== REACTIVE: OBTENER AÑO ACTUAL (DINÁMICO) ==========
  
  anio_actual <- reactive({
    # ✅ CORRECCIÓN CRÍTICA: Detectar año actual automáticamente desde catálogo
    if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (length(catalog$historico) > 0) {
        # Obtener el año de la fecha más reciente disponible
        ultima_fecha <- max(catalog$historico)
        año_detectado <- as.integer(format(ultima_fecha, "%Y"))
        message("📅 [anio_actual] Año detectado automáticamente: ", año_detectado)
        return(año_detectado)
      }
    }
    
    # Fallback: usar año del sistema
    año_sistema <- as.integer(format(Sys.Date(), "%Y"))
    message("⚠️ [anio_actual] Usando año del sistema: ", año_sistema)
    return(año_sistema)
  })
  
  # ========== REACTIVE: DETERMINAR AÑO CONSULTADO ==========
  
  anio_consultado <- reactive({
    if (estado_app() == "consultado" && !is.null(input$year)) {
      año_consultado <- as.integer(input$year)
      message("📊 [anio_consultado] Año consultado: ", año_consultado)
      return(año_consultado)
    }
    
    año_actual_val <- anio_actual()
    message("📊 [anio_consultado] Usando año actual: ", año_actual_val)
    return(año_actual_val)
  })
  
  # ========== REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS ANUALES (1, 2, 3) ==========
  # ✅ CORRECCIÓN CRÍTICA: Mostrar en Nacional Y Extranjero cuando año consultado == año actual
  
  mostrar_graficas_anuales <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_actual_val <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔍 [mostrar_graficas_anuales] Estado: ", estado_actual, " | Año consultado: ", anio_consult, " | Año actual: ", anio_actual_val, " | Ámbito: ", ambito)
    
    # ✅ NUEVA LÓGICA: Mostrar gráficas 1, 2, 3 cuando año consultado == año actual
    # Esto aplica tanto para Nacional como Extranjero
    
    if (estado_actual == "restablecido") {
      # En estado inicial, siempre mostrar gráficas 1, 2, 3 (independiente del ámbito)
      message("✅ [mostrar_graficas_anuales] Estado RESTABLECIDO → Mostrar gráficas 1, 2, 3")
      return(TRUE)
    }
    
    if (estado_actual == "consultado") {
      # Después de consultar, mostrar 1, 2, 3 solo si año consultado == año actual
      mostrar <- (anio_consult == anio_actual_val)
      message(ifelse(mostrar, "✅", "❌"), " [mostrar_graficas_anuales] Estado CONSULTADO → ", 
              ifelse(mostrar, "Mostrar gráficas 1, 2, 3", "NO mostrar gráficas 1, 2, 3"))
      return(mostrar)
    }
    
    message("❌ [mostrar_graficas_anuales] Estado no reconocido → NO mostrar")
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$year, input$ambito_datos, 
              ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS 4, 5 ==========
  # ✅ CORRECCIÓN CRÍTICA: Solo mostrar cuando estado == "consultado" Y año ≠ año actual
  
  mostrar_graficas_consultadas <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_actual_val <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔍 [mostrar_graficas_consultadas] Estado: ", estado_actual, " | Año consultado: ", anio_consult, " | Año actual: ", anio_actual_val, " | Ámbito: ", ambito)
    
    # ✅ NUEVA LÓGICA: Mostrar gráficas 4, 5 SOLO cuando:
    # 1. Estado == "consultado" (botón presionado)
    # 2. Año consultado ≠ año actual
    
    if (estado_actual == "consultado") {
      mostrar <- (anio_consult != anio_actual_val)
      message(ifelse(mostrar, "✅", "❌"), " [mostrar_graficas_consultadas] Estado CONSULTADO → ", 
              ifelse(mostrar, "Mostrar gráficas 4, 5", "NO mostrar gráficas 4, 5"))
      return(mostrar)
    }
    
    # En estado inicial, NUNCA mostrar gráficas 4, 5
    message("❌ [mostrar_graficas_consultadas] Estado ", estado_actual, " → NO mostrar gráficas 4, 5")
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$year, input$ambito_datos, 
              ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: FILTROS ACTUALES DEL USUARIO ==========
  
  filtros_usuario <- reactive({
    if (estado_app() %in% c("inicial", "restablecido")) {
      return(list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas"
      ))
    }
    
    list(
      entidad = input$entidad %||% "Nacional",
      distrito = input$distrito %||% "Todos",
      municipio = input$municipio %||% "Todos",
      seccion = input$seccion %||% "Todas"
    )
  })
  
  # ========== REACTIVE: TEXTO DE ALCANCE ==========
  
  texto_alcance <- reactive({
    # Llamar a función helper que maneja toda la lógica
    texto <- generar_texto_alcance(input)
    return(texto)
  })
  
  # ========== RETORNAR LISTA DE REACTIVES Y FUNCIONES ==========
  
  message("✅ graficas_core v2.2 inicializado")
  
  return(list(
    anio_actual = anio_actual,
    anio_consultado = anio_consultado,
    mostrar_graficas_anuales = mostrar_graficas_anuales,
    mostrar_graficas_consultadas = mostrar_graficas_consultadas,
    filtros_usuario = filtros_usuario,
    texto_alcance = texto_alcance,
    cache_valido = cache_valido
  ))
}