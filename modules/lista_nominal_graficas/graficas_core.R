# modules/lista_nominal_graficas/graficas_core.R
# Reactives base: caché, filtros, año, estado
# Versión: 2.1 - CORRECCIÓN: texto_alcance usa función helper con soporte mejorado para extranjero

graficas_core <- function(input, output, session, estado_app) {
  
  message("📦 Inicializando graficas_core v2.1")
  
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
  
  # ========== REACTIVE: OBTENER AÑO ACTUAL ==========
  
  anio_actual <- reactive({
    as.integer(format(Sys.Date(), "%Y"))
  })
  
  # ========== REACTIVE: DETERMINAR AÑO CONSULTADO ==========
  
  anio_consultado <- reactive({
    if (estado_app() == "consultado" && !is.null(input$year)) {
      return(as.integer(input$year))
    }
    return(anio_actual())
  })
  
  # ========== REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS ANUALES (1, 2, 3) ==========
  # ✅ CORRECCIÓN CRÍTICA: Considerar ámbito
  
  mostrar_graficas_anuales <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_actual_val <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    # ✅ REGLA CRÍTICA: Si ámbito es extranjero, NO mostrar gráficas 1, 2, 3
    if (ambito == "extranjero") {
      message("📊 [mostrar_graficas_anuales] Ámbito EXTRANJERO → NO mostrar gráficas 1, 2, 3")
      return(FALSE)
    }
    
    if (estado_actual == "restablecido") {
      return(TRUE)
    }
    
    if (estado_actual == "consultado") {
      return(anio_consult == anio_actual_val)
    }
    
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$year, input$btn_consultar, input$ambito_datos, 
              ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS 4, 5 ==========
  # ✅ CORRECCIÓN CRÍTICA: Considerar ámbito
  
  mostrar_graficas_consultadas <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_actual_val <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    # ✅ REGLA CRÍTICA: Si ámbito es extranjero, SIEMPRE mostrar gráficas 4, 5
    if (ambito == "extranjero") {
      message("📊 [mostrar_graficas_consultadas] Ámbito EXTRANJERO → MOSTRAR gráficas 4, 5")
      return(TRUE)
    }
    
    # Para ámbito nacional: mostrar gráficas 4, 5 solo si año consultado != año actual
    if (estado_actual == "consultado") {
      return(anio_consult != anio_actual_val)
    }
    
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$year, input$btn_consultar, input$ambito_datos, 
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
  # ✅ CORRECCIÓN PROBLEMA 1: Ahora usa función helper generar_texto_alcance()
  
  texto_alcance <- reactive({
    # Llamar a función helper que maneja toda la lógica
    texto <- generar_texto_alcance(input)
    return(texto)
  })
  
  # ========== RETORNAR LISTA DE REACTIVES Y FUNCIONES ==========
  
  message("✅ graficas_core v2.1 inicializado")
  
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