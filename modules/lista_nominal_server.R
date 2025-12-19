# modules/lista_nominal_server.R
# Versión: 3.4 - SOLUCIÓN DEFINITIVA: Reimplementación de lógica de fecha sin llamar a reactive
# FILTROS EN CASCADA: Reactivos e independientes de botón "Consultar"

# Configurar nombres de meses en español
meses_es <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)
names(meses_es) <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

# Función auxiliar para formatear fechas en español
formatear_fecha_es <- function(fecha, formato = "%d de %B de %Y") {
  if (is.null(fecha) || is.na(fecha)) return("")
  
  fecha_str <- format(as.Date(fecha), formato)
  
  for (mes_en in names(meses_es)) {
    fecha_str <- gsub(mes_en, meses_es[mes_en], fecha_str)
  }
  
  return(fecha_str)
}

lista_nominal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========== CARGAR HELPERS PARA FILTROS ==========
    source("modules/lista_nominal_helpers_ui.R", local = TRUE)
    message("✅ Helpers UI cargados")
    
    # ========== CONTROL DE ESTADO DE LA APP ==========
    estado_app <- reactiveVal("restablecido")
    
    # Cargar submódulos
    source("modules/lista_nominal_server_main.R", local = TRUE)
    source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
    
    # ========== REACTIVE: OBTENER ÚLTIMA FECHA DEL AÑO SELECCIONADO ==========
    
    ultima_fecha_disponible <- reactive({
      req(input$tipo_corte, input$year)
      
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        return(NULL)
      }
      
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (input$tipo_corte == "historico") {
        fechas_year <- catalog$historico[format(catalog$historico, "%Y") == input$year]
      } else {
        fechas_year <- catalog$semanal_comun[format(catalog$semanal_comun, "%Y") == input$year]
      }
      
      if (length(fechas_year) == 0) {
        return(NULL)
      }
      
      # Retornar la fecha más reciente
      ultima <- max(fechas_year)
      message("📅 [ultima_fecha_disponible] Última fecha para año ", input$year, ": ", ultima)
      return(as.Date(ultima, origin = "1970-01-01"))
    })
    
    # ========== FUNCIÓN AUXILIAR: OBTENER FECHA SEGURA PARA FILTROS ==========
    # ✅ v3.4 DEFINITIVO: Reimplementación SIN llamar al reactive
    
    obtener_fecha_para_filtros <- function() {
      # Obtener valores necesarios con isolate
      tipo_corte_val <- isolate(input$tipo_corte)
      year_val <- isolate(input$year)
      
      # Validar catálogo
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        message("❌ [obtener_fecha_para_filtros] LNE_CATALOG no existe")
        return(NULL)
      }
      
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      # Si NO hay año seleccionado, usar última fecha del catálogo
      if (is.null(year_val) || is.na(year_val) || year_val == "") {
        if (length(catalog$historico) > 0) {
          ultima_fecha_catalogo <- as.Date(max(catalog$historico), origin = "1970-01-01")
          message("⚠️ [obtener_fecha_para_filtros] No hay año, usando última del catálogo: ", ultima_fecha_catalogo)
          return(ultima_fecha_catalogo)
        } else {
          message("❌ [obtener_fecha_para_filtros] No hay fechas en catálogo")
          return(NULL)
        }
      }
      
      # Si NO hay tipo de corte, asumir histórico
      if (is.null(tipo_corte_val) || is.na(tipo_corte_val) || tipo_corte_val == "") {
        tipo_corte_val <- "historico"
        message("⚠️ [obtener_fecha_para_filtros] No hay tipo_corte, asumiendo 'historico'")
      }
      
      # Obtener fechas del año seleccionado (REIMPLEMENTACIÓN DIRECTA)
      if (tipo_corte_val == "historico") {
        fechas_year <- catalog$historico[format(catalog$historico, "%Y") == year_val]
      } else {
        fechas_year <- catalog$semanal_comun[format(catalog$semanal_comun, "%Y") == year_val]
      }
      
      # Si no hay fechas para ese año, usar última del catálogo
      if (length(fechas_year) == 0) {
        if (length(catalog$historico) > 0) {
          ultima_fecha_catalogo <- as.Date(max(catalog$historico), origin = "1970-01-01")
          message("⚠️ [obtener_fecha_para_filtros] No hay fechas para año ", year_val, ", usando última del catálogo: ", ultima_fecha_catalogo)
          return(ultima_fecha_catalogo)
        } else {
          message("❌ [obtener_fecha_para_filtros] No hay fechas disponibles")
          return(NULL)
        }
      }
      
      # Obtener la fecha más reciente del año
      ultima <- max(fechas_year)
      fecha_final <- as.Date(ultima, origin = "1970-01-01")
      
      # Validación final robusta
      if (!inherits(fecha_final, "Date")) {
        message("❌ [obtener_fecha_para_filtros] Conversión a Date falló")
        return(NULL)
      }
      
      if (is.na(fecha_final)) {
        message("❌ [obtener_fecha_para_filtros] Fecha es NA")
        return(NULL)
      }
      
      message("✅ [obtener_fecha_para_filtros] Fecha para año ", year_val, ": ", fecha_final)
      return(fecha_final)
    }
    
    # ========== INFORMACIÓN TIPO DE CORTE ==========
    
    output$info_tipo_corte <- renderUI({
      req(input$tipo_corte)
      
      if (input$tipo_corte == "historico") {
        HTML(paste0(
          "<div style='background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
          "<small><strong>Datos históricos anuales</strong><br>",
          "Información mensual agregada por entidad, distrito, municipio y sección.<br>",
          "Periodo: 2017 a la última actualización</small>",
          "</div>"
        ))
      } else {
        HTML(paste0(
          "<div style='background-color: #fff4e6; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
          "<small><strong>Datos detallados del año en curso</strong><br>",
          "Desgloses por edad, sexo y origen.<br>",
          "Periodo: enero 2025 a la última actualización</small>",
          "</div>"
        ))
      }
    })
    
    # ========== SELECTOR DE DESGLOSE DINÁMICO (SOLO PARA SEMANAL) ==========
    
    output$selector_desglose <- renderUI({
      req(input$tipo_corte)
      
      if (input$tipo_corte == "semanal") {
        selectInput(
          ns("desglose"),
          "Desglose:",
          choices = c("Sexo", "Rango de Edad", "Entidad de Origen"),
          selected = "Sexo"
        )
      } else {
        return(NULL)
      }
    })
    
    # ========== ENCABEZADO PRINCIPAL ==========
    
    output$encabezado_principal <- renderUI({
      req(input$tipo_corte)
      
      # Obtener última fecha disponible
      fecha <- ultima_fecha_disponible()
      
      if (is.null(fecha)) {
        return(h3("Lista Nominal Electoral", style = "color: #666;"))
      }
      
      fecha_formateada <- formatear_fecha_es(fecha, "%d de %B de %Y")
      tipo_texto <- if (input$tipo_corte == "historico") "Datos Históricos" else "Datos Semanales"
      
      # Determinar ámbito para el encabezado
      ambito_texto <- if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
        "Extranjero"
      } else if (!is.null(input$entidad)) {
        input$entidad
      } else {
        "Nacional"
      }
      
      HTML(paste0(
        "<h3>Lista Nominal Electoral - ", tipo_texto, "</h3>",
        "<p style='font-size: 14px; color: #666;'>",
        "Corte: <strong>", fecha_formateada, "</strong> | ",
        "Ámbito: <strong>", ambito_texto, "</strong>",
        "</p>"
      ))
    })
    
    # ========== ACTUALIZAR AÑOS DISPONIBLES ==========
    
    observeEvent(input$tipo_corte, {
      req(input$tipo_corte)
      
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          if (length(catalog$historico) > 0) {
            años_disponibles <- sort(unique(format(catalog$historico, "%Y")), decreasing = TRUE)
            
            updateSelectInput(session, "year",
                              choices = años_disponibles,
                              selected = años_disponibles[1])
            
            message("📅 Años históricos actualizados: ", paste(años_disponibles, collapse = ", "))
          } else {
            updateSelectInput(session, "year", choices = NULL)
          }
          
        } else {
          if (length(catalog$semanal_comun) > 0) {
            años_disponibles <- sort(unique(format(catalog$semanal_comun, "%Y")), decreasing = TRUE)
            
            updateSelectInput(session, "year",
                              choices = años_disponibles,
                              selected = años_disponibles[1])
            
            message("📅 Años semanales actualizados: ", paste(años_disponibles, collapse = ", "))
          } else {
            updateSelectInput(session, "year", choices = NULL)
          }
        }
      }
    }, priority = 100)
    
    # ========== FUNCIÓN AUXILIAR: CARGA INICIAL RÁPIDA ==========
    
    cargar_datos_defecto <- function() {
      message("🚀 [CARGA INICIAL] Cargando datos por defecto...")
      
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        return(NULL)
      }
      
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      # Obtener última fecha histórica disponible
      ultima_fecha <- max(catalog$historico)
      
      message("📅 [CARGA INICIAL] Última fecha: ", ultima_fecha)
      
      # Cargar SOLO 1 archivo: último mensual Nacional sin filtros
      datos_lne <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = ultima_fecha,
          dimension = "completo",
          estado = "Nacional",
          distrito = "Todos",
          municipio = "Todos",
          seccion = "Todas",
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("❌ [CARGA INICIAL] Error: ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_lne)) {
        message("✅ [CARGA INICIAL] Datos cargados: ", nrow(datos_lne$datos), " filas")
      }
      
      return(datos_lne)
    }
    
    # ========== REACTIVOS PRINCIPALES ==========
    
    combinacion_valida <- reactive({
      # CARGA INICIAL O RESTABLECIDA: siempre válida
      if (estado_app() %in% c("inicial", "restablecido")) {
        return(TRUE)
      }
      
      # CARGA PERSONALIZADA: validar inputs
      req(input$tipo_corte, input$year)
      
      # ✅ NUEVA LÓGICA: Validar que existan fechas para el año seleccionado
      fecha_disponible <- ultima_fecha_disponible()
      
      if (is.null(fecha_disponible) || is.na(fecha_disponible)) {
        return(FALSE)
      }
      
      return(TRUE)
    })
    
    # ========== REACTIVE OPTIMIZADO: datos_columnas CON BOTÓN ==========
    
    datos_columnas <- reactive({
      # ========== CARGA RESTABLECIDA (AUTOMÁTICA AL INICIO) ==========
      if (estado_app() == "restablecido") {
        message("🚀 [DATOS_COLUMNAS] CARGA RESTABLECIDA - Mostrando datos iniciales")
        return(cargar_datos_defecto())
      }
      
      # ========== CARGA PERSONALIZADA (BOTÓN PRESIONADO) ==========
      if (estado_app() == "consultado") {
        req(input$btn_consultar > 0)
        message("🔍 [DATOS_COLUMNAS] CARGA PERSONALIZADA - Botón presionado: ", input$btn_consultar)
        
        # Aislar inputs para evitar reactividad no deseada
        tipo_corte <- isolate(input$tipo_corte)
        year <- isolate(input$year)
        entidad <- isolate(input$entidad)
        distrito <- isolate(input$distrito %||% "Todos")
        municipio <- isolate(input$municipio %||% "Todos")
        seccion <- isolate(input$seccion %||% "Todas")
        desglose <- isolate(input$desglose %||% "Sexo")
        ambito <- isolate(input$ambito_datos %||% "nacional")
        
        # ✅ OBTENER ÚLTIMA FECHA AUTOMÁTICAMENTE
        fecha_seleccionada <- isolate(ultima_fecha_disponible())
        
        if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
          message("❌ No hay fecha disponible para el año ", year)
          return(NULL)
        }
        
        message("📊 Configuración: tipo=", tipo_corte, ", año=", year, ", fecha=", fecha_seleccionada, ", ámbito=", ambito, ", entidad=", entidad)
        
        # Los filtros son iguales para ambos ámbitos
        estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
        message("📍 Estado: ", estado_filtro, " | Ámbito: ", ambito)
        
        dimension <- if (tipo_corte == "semanal") {
          switch(desglose,
                 "Sexo" = "sexo",
                 "Rango de Edad" = "edad",
                 "Entidad de Origen" = "origen",
                 "completo")
        } else {
          "completo"
        }
        
        message("📂 Llamando cargar_lne: tipo=", tipo_corte, ", fecha=", fecha_seleccionada, 
                ", dimension=", dimension, ", estado=", estado_filtro,
                ", distrito=", distrito, ", municipio=", municipio, ", seccion=", seccion)
        
        datos_lne <- tryCatch({
          cargar_lne(
            tipo_corte = tipo_corte,
            fecha = fecha_seleccionada,
            dimension = dimension,
            estado = estado_filtro,
            distrito = distrito,
            municipio = municipio,
            seccion = seccion,
            incluir_extranjero = TRUE
          )
        }, error = function(e) {
          message("❌ Error en cargar_lne: ", e$message)
          return(NULL)
        })
        
        if (is.null(datos_lne) || !is.list(datos_lne)) {
          message("❌ cargar_lne retornó NULL o no es lista")
          return(NULL)
        }
        
        if (!"datos" %in% names(datos_lne) || nrow(datos_lne$datos) == 0) {
          message("⚠️ Sin datos tras filtros")
          return(NULL)
        }
        
        message("✅ Datos LNE cargados: ", nrow(datos_lne$datos), " filas")
        return(datos_lne)
      }
      
      # Por defecto, retornar datos iniciales
      return(cargar_datos_defecto())
      
    }) %>% bindCache(estado_app(), input$btn_consultar, input$tipo_corte, input$year,
                     input$entidad, input$distrito, input$municipio, input$seccion, input$ambito_datos)
    
    # ========== ✅ FILTROS EN CASCADA REACTIVOS (SIN PESTAÑEO) ==========
    
    # PASO 1: ACTUALIZAR ESTADOS (SOLO AL INICIO)
    observeEvent(input$tipo_corte, {
      req(input$tipo_corte)
      
      # Obtener TODOS los estados disponibles (lista estática)
      todos_estados <- get_entidades()
      
      # Preservar selección actual si es válida
      current_estado <- isolate(input$entidad)
      selected_estado <- if (!is.null(current_estado) && current_estado %in% todos_estados) {
        current_estado
      } else {
        "Nacional"
      }
      
      updateSelectInput(session, "entidad",
                        choices = todos_estados,
                        selected = selected_estado)
      
      message("🗺️ [FILTROS CASCADA] Estados actualizados: ", length(todos_estados) - 1, " entidades")
    }, priority = 50, once = TRUE)
    
    # PASO 2: ACTUALIZAR DISTRITOS (cuando cambia Estado)
    observeEvent(input$entidad, {
      req(input$entidad)
      
      # ✅ v3.4: Obtener fecha con función mejorada
      fecha_actual <- obtener_fecha_para_filtros()
      
      # Validación robusta
      if (is.null(fecha_actual)) {
        message("⚠️ [FILTROS CASCADA] No hay fecha disponible, abortando actualización de distritos")
        return()
      }
      
      if (!inherits(fecha_actual, "Date")) {
        message("❌ [FILTROS CASCADA] Fecha no es objeto Date: ", class(fecha_actual))
        return()
      }
      
      if (is.na(fecha_actual)) {
        message("❌ [FILTROS CASCADA] Fecha es NA")
        return()
      }
      
      # Obtener distritos usando función helper
      nuevos_distritos <- get_distritos_por_entidad(
        entidad = input$entidad,
        fecha = fecha_actual
      )
      
      # Preservar selección actual si es válida
      current_distrito <- isolate(input$distrito)
      selected_distrito <- if (!is.null(current_distrito) && current_distrito %in% nuevos_distritos) {
        current_distrito
      } else {
        "Todos"
      }
      
      updateSelectInput(session, "distrito",
                        choices = nuevos_distritos,
                        selected = selected_distrito)
      
      message("🗺️ [FILTROS CASCADA] Distritos actualizados para ", input$entidad, ": ", length(nuevos_distritos) - 1, " distritos")
    }, priority = 40, ignoreInit = TRUE)
    
    # PASO 3: ACTUALIZAR MUNICIPIOS (cuando cambia Distrito)
    observeEvent(input$distrito, {
      req(input$distrito)
      
      entidad_actual <- isolate(input$entidad)
      
      if (is.null(entidad_actual)) {
        return()
      }
      
      # ✅ v3.4: Obtener fecha con función mejorada
      fecha_actual <- obtener_fecha_para_filtros()
      
      # Validación robusta
      if (is.null(fecha_actual)) {
        message("⚠️ [FILTROS CASCADA] No hay fecha disponible, abortando actualización de municipios")
        return()
      }
      
      if (!inherits(fecha_actual, "Date")) {
        message("❌ [FILTROS CASCADA] Fecha no es objeto Date: ", class(fecha_actual))
        return()
      }
      
      if (is.na(fecha_actual)) {
        message("❌ [FILTROS CASCADA] Fecha es NA")
        return()
      }
      
      # Obtener municipios usando función helper
      nuevos_municipios <- get_municipios_por_distrito(
        entidad = entidad_actual,
        distrito = input$distrito,
        fecha = fecha_actual
      )
      
      # Preservar selección actual si es válida
      current_municipio <- isolate(input$municipio)
      selected_municipio <- if (!is.null(current_municipio) && current_municipio %in% nuevos_municipios) {
        current_municipio
      } else {
        "Todos"
      }
      
      updateSelectInput(session, "municipio",
                        choices = nuevos_municipios,
                        selected = selected_municipio)
      
      message("🗺️ [FILTROS CASCADA] Municipios actualizados para ", input$distrito, ": ", length(nuevos_municipios) - 1, " municipios")
    }, priority = 30, ignoreInit = TRUE)
    
    # PASO 4: ACTUALIZAR SECCIONES (cuando cambia Municipio)
    observeEvent(input$municipio, {
      req(input$municipio)
      
      entidad_actual <- isolate(input$entidad)
      distrito_actual <- isolate(input$distrito)
      
      if (is.null(entidad_actual) || is.null(distrito_actual)) {
        return()
      }
      
      # ✅ v3.4: Obtener fecha con función mejorada
      fecha_actual <- obtener_fecha_para_filtros()
      
      # Validación robusta
      if (is.null(fecha_actual)) {
        message("⚠️ [FILTROS CASCADA] No hay fecha disponible, abortando actualización de secciones")
        return()
      }
      
      if (!inherits(fecha_actual, "Date")) {
        message("❌ [FILTROS CASCADA] Fecha no es objeto Date: ", class(fecha_actual))
        return()
      }
      
      if (is.na(fecha_actual)) {
        message("❌ [FILTROS CASCADA] Fecha es NA")
        return()
      }
      
      # Obtener secciones usando función helper
      nuevas_secciones <- get_secciones_por_municipio(
        entidad = entidad_actual,
        distrito = distrito_actual,
        municipio = input$municipio,
        fecha = fecha_actual
      )
      
      # Preservar selección actual si es válida
      current_seccion <- isolate(input$seccion)
      
      if (!is.null(current_seccion) && length(current_seccion) > 0) {
        if ("Todas" %in% current_seccion) {
          selected_seccion <- "Todas"
        } else {
          valid_secciones <- current_seccion[current_seccion %in% nuevas_secciones]
          selected_seccion <- if (length(valid_secciones) > 0) valid_secciones else "Todas"
        }
      } else {
        selected_seccion <- "Todas"
      }
      
      updateSelectizeInput(session, "seccion",
                           choices = nuevas_secciones,
                           selected = selected_seccion,
                           options = list(
                             placeholder = "Selecciona una o más secciones",
                             plugins = list("remove_button"),
                             maxItems = NULL
                           ))
      
      message("🗺️ [FILTROS CASCADA] Secciones actualizadas para ", input$municipio, ": ", length(nuevas_secciones) - 1, " secciones")
    }, priority = 20, ignoreInit = TRUE)
    
    # PASO 5: Manejar selección de "Todas" en secciones
    observeEvent(input$seccion, {
      req(input$seccion)
      
      if (length(input$seccion) > 1 && "Todas" %in% input$seccion) {
        updateSelectizeInput(session, "seccion", 
                             selected = "Todas",
                             options = list(
                               placeholder = "Selecciona una o más secciones",
                               plugins = list("remove_button"),
                               maxItems = NULL
                             ))
        message("🗺️ Usuario seleccionó 'Todas' - limpiando otras selecciones")
      }
    }, priority = 10, ignoreInit = TRUE)
    
    # ========== BOTÓN CONSULTAR ==========
    observeEvent(input$btn_consultar, {
      req(input$btn_consultar > 0)
      
      message("🔍 [BOTÓN CONSULTAR] Presionado - Cambiando estado a 'consultado'")
      estado_app("consultado")
    }, ignoreInit = TRUE)
    
    # ========== BOTÓN RESTABLECER CONSULTA ==========
    observeEvent(input$reset_config, {
      message("🔄 [RESTABLECER] Botón presionado - Restableciendo configuración...")
      
      estado_app("restablecido")
      message("   ✅ Estado → restablecido")
      
      if (exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
        message("🧹 [RESTABLECER] Limpiando caché de gráficas...")
        assign("LNE_CACHE_GRAFICAS", list(
          datos_year_actual = NULL,
          datos_anuales = NULL,
          timestamp_year = NULL,
          timestamp_anuales = NULL,
          año_cacheado = NULL
        ), envir = .GlobalEnv)
        message("   ✅ Caché limpiado")
      }
      
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        message("⚠️ [RESTABLECER] LNE_CATALOG no existe")
        return(NULL)
      }
      
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      updateRadioButtons(session, "tipo_corte", selected = "historico")
      message("   ✅ tipo_corte → historico")
      
      updateRadioButtons(session, "ambito_datos", selected = "nacional")
      message("   ✅ ambito_datos → nacional")
      
      if (length(catalog$historico) > 0) {
        años_disponibles <- sort(unique(format(catalog$historico, "%Y")), decreasing = TRUE)
        updateSelectInput(session, "year", selected = años_disponibles[1])
        message("   ✅ year → ", años_disponibles[1])
      }
      
      updateSelectInput(session, "entidad", selected = "Nacional")
      message("   ✅ entidad → Nacional")
      
      message("✅ [RESTABLECER] Configuración restablecida correctamente")
    })
    
    # ========== ✅ LLAMAR A SUBMÓDULOS CON estado_app ==========
    
    if (file.exists("modules/lista_nominal_server_main.R")) {
      source("modules/lista_nominal_server_main.R", local = TRUE)
      lista_nominal_server_main(input, output, session, datos_columnas, combinacion_valida, estado_app)
    } else {
      message("⚠️ No se encontró lista_nominal_server_main.R")
    }
    
    if (file.exists("modules/lista_nominal_graficas/graficas_main.R")) {
      source("modules/lista_nominal_graficas/graficas_main.R", local = TRUE)
      lista_nominal_server_graficas(input, output, session, datos_columnas, combinacion_valida, estado_app)
      message("✅ Módulo de gráficas modularizado cargado correctamente")
    } else {
      message("⚠️ No se encontró módulo de gráficas modularizado en modules/lista_nominal_graficas/graficas_main.R")
    }
    
    if (file.exists("modules/lista_nominal_server_text_analysis.R")) {
      source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
      lista_nominal_server_text_analysis(input, output, session, datos_columnas, estado_app)
    } else {
      message("⚠️ No se encontró lista_nominal_server_text_analysis.R")
    }
    
    message("✅ Módulo lista_nominal_server v3.4 inicializado (SOLUCIÓN DEFINITIVA)")
  })
}