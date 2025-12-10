# modules/lista_nominal_server.R
# Versión: 2.4 - CORRECCIÓN: Filtros persisten al cambiar entre Nacional ↔ Extranjero

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
    
    # ========== CONTROL DE ESTADO DE LA APP ==========
    estado_app <- reactiveVal("restablecido")  # ✅ INICIAR EN "restablecido" para carga automática
    
    # Cargar submódulos
    source("modules/lista_nominal_server_main.R", local = TRUE)
    source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
    
    # ========== INFORMACIÓN TIPO DE CORTE ==========
    
    output$info_tipo_corte <- renderUI({
      req(input$tipo_corte)
      
      if (input$tipo_corte == "historico") {
        HTML(paste0(
          "<div style='background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
          "<small><strong>Datos históricos mensuales</strong><br>",
          "Información agregada por entidad, distrito, municipio y sección.<br>",
          "Periodo: 2017 a la última actualización</small>",
          "</div>"
        ))
      } else {
        HTML(paste0(
          "<div style='background-color: #fff4e6; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
          "<small><strong>Datos semanales detallados</strong><br>",
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
    
    # ========== INFORMACIÓN DE FECHA SELECCIONADA ==========
    
    output$info_fecha <- renderUI({
      req(input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        return(NULL)
      }
      
      fecha <- as.Date(input$date)
      fecha_formateada <- formatear_fecha_es(fecha, "%d de %B de %Y")
      
      HTML(paste0(
        "<div style='background-color: #f0f0f0; padding: 5px; border-radius: 3px; margin-top: 5px;'>",
        "<small><strong>Fecha seleccionada:</strong><br>",
        fecha_formateada, "</small>",
        "</div>"
      ))
    })
    
    # ========== ENCABEZADO PRINCIPAL ==========
    
    output$encabezado_principal <- renderUI({
      req(input$tipo_corte, input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        return(h3("Lista Nominal Electoral", style = "color: #666;"))
      }
      
      fecha <- as.Date(input$date)
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
    
    # ========== ACTUALIZAR FECHAS DISPONIBLES ==========
    
    observeEvent(list(input$tipo_corte, input$year), {
      req(input$tipo_corte, input$year)
      
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          fechas_year <- catalog$historico[format(catalog$historico, "%Y") == input$year]
          
          if (length(fechas_year) > 0) {
            fechas_year <- sort(fechas_year, decreasing = TRUE)
            
            choices <- setNames(
              as.character(fechas_year),
              sapply(fechas_year, formatear_fecha_es, formato = "%B %Y")
            )
            
            names(choices) <- paste0(toupper(substr(names(choices), 1, 1)), 
                                     substr(names(choices), 2, nchar(names(choices))))
            
            updateSelectInput(session, "date",
                              choices = choices,
                              selected = choices[1])
            
            message("📅 Fechas históricas para ", input$year, ": ", length(fechas_year), " opciones")
          } else {
            updateSelectInput(session, "date", choices = c("Sin datos" = ""))
          }
          
        } else {
          fechas_year <- catalog$semanal_comun[format(catalog$semanal_comun, "%Y") == input$year]
          
          if (length(fechas_year) > 0) {
            fechas_year <- sort(fechas_year, decreasing = TRUE)
            
            choices <- setNames(
              as.character(fechas_year),
              sapply(fechas_year, formatear_fecha_es, formato = "%d de %B de %Y")
            )
            
            updateSelectInput(session, "date",
                              choices = choices,
                              selected = choices[1])
            
            message("📅 Fechas semanales para ", input$year, ": ", length(fechas_year), " opciones")
          } else {
            updateSelectInput(session, "date", choices = c("Sin datos" = ""))
          }
        }
      }
    }, priority = 90)
    
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
          incluir_extranjero = TRUE,
          solo_extranjero = FALSE
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
      req(input$tipo_corte, input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        return(FALSE)
      }
      
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
        return(FALSE)
      }
      
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          return(fecha_seleccionada %in% catalog$historico)
        } else {
          return(fecha_seleccionada %in% catalog$semanal_comun)
        }
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
        req(input$btn_consultar > 0)  # ✅ CRÍTICO: Verificar que botón fue presionado
        message("🔍 [DATOS_COLUMNAS] CARGA PERSONALIZADA - Botón presionado: ", input$btn_consultar)
        
        # Aislar inputs para evitar reactividad no deseada
        tipo_corte <- isolate(input$tipo_corte)
        year <- isolate(input$year)
        date <- isolate(input$date)
        entidad <- isolate(input$entidad)
        distrito <- isolate(input$distrito %||% "Todos")
        municipio <- isolate(input$municipio %||% "Todos")
        seccion <- isolate(input$seccion %||% "Todas")
        desglose <- isolate(input$desglose %||% "Sexo")
        ambito <- isolate(input$ambito_datos %||% "nacional")
        
        message("📊 Configuración: tipo=", tipo_corte, ", fecha=", date, ", ámbito=", ambito, ", entidad=", entidad)
        
        if (date == "" || date == "Sin datos") {
          message("❌ Fecha no válida")
          return(NULL)
        }
        
        fecha_seleccionada <- tryCatch({
          as.Date(date)
        }, error = function(e) {
          message("❌ Error convirtiendo fecha: ", e$message)
          return(NULL)
        })
        
        if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
          message("❌ Fecha inválida")
          return(NULL)
        }
        
        # ✅ CORRECCIÓN CRÍTICA: Determinar filtros según ÁMBITO
        solo_extranjero <- FALSE
        
        if (ambito == "extranjero") {
          # NO cambiar estado_filtro - mantener el estado seleccionado (ej: COLIMA)
          estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
          solo_extranjero <- TRUE  # Marcar que queremos SOLO datos de extranjero
          message("📍 Ámbito EXTRANJERO - Estado: ", estado_filtro, ", solo_extranjero=TRUE")
        } else {
          estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
          message("📍 Ámbito Nacional - Estado: ", estado_filtro)
        }
        
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
                ", distrito=", distrito, ", municipio=", municipio, ", seccion=", seccion,
                ", solo_extranjero=", solo_extranjero)
        
        datos_lne <- tryCatch({
          cargar_lne(
            tipo_corte = tipo_corte,
            fecha = fecha_seleccionada,
            dimension = dimension,
            estado = estado_filtro,
            distrito = distrito,
            municipio = municipio,
            seccion = seccion,
            incluir_extranjero = TRUE,
            solo_extranjero = solo_extranjero  # ✅ NUEVO PARÁMETRO
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
      
    }) %>% bindCache(estado_app(), input$btn_consultar, input$tipo_corte, input$date, 
                     input$entidad, input$distrito, input$municipio, input$seccion, input$ambito_datos)
    
    # ========== ACTUALIZAR FILTROS GEOGRÁFICOS - SOLO ACTUALIZAN OPCIONES ==========
    
    # PASO 1: Actualizar ESTADOS
    observeEvent(datos_columnas(), {
      datos <- datos_columnas()
      
      if (!is.null(datos) && is.list(datos)) {
        estados <- c("Nacional", datos$todos_estados)
        
        current_estado <- isolate(input$entidad)
        selected_estado <- if (!is.null(current_estado) && current_estado %in% estados) {
          current_estado
        } else {
          "Nacional"
        }
        
        updateSelectInput(session, "entidad",
                          choices = estados,
                          selected = selected_estado)
        
        message("🗺️ Estados actualizados: ", length(estados) - 1, " entidades")
      }
    }, priority = 50)
    
    # ✅ CORRECCIÓN PROBLEMA 2: Remover input$ambito_datos de dependencias
    # PASO 2: Actualizar DISTRITOS
    observeEvent(list(input$entidad, input$year, input$date), {
      req(input$year, input$date)
      
      # ✅ VERIFICAR ámbito sin incluirlo en dependencias
      if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
        # NO resetear, solo mantener actual valor o "Todos"
        current_distrito <- isolate(input$distrito)
        if (is.null(current_distrito) || current_distrito == "") {
          updateSelectInput(session, "distrito", choices = c("Todos"), selected = "Todos")
        }
        message("🗺️ Ámbito = Extranjero → Filtros geográficos deshabilitados (valor preservado)")
        return()
      }
      
      req(input$entidad)
      
      if (input$entidad == "Nacional") {
        # NO resetear si ya es "Todos"
        current_distrito <- isolate(input$distrito)
        if (current_distrito != "Todos") {
          updateSelectInput(session, "distrito", choices = c("Todos"), selected = "Todos")
          message("🗺️ Estado = Nacional → Distrito resetado")
        }
        return()
      }
      
      message("🔍 [FILTRADO CASCADA] Cargando distritos para: ", input$entidad)
      
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) NULL)
      
      if (is.null(fecha_seleccionada)) {
        message("❌ Fecha inválida para cargar distritos")
        return()
      }
      
      datos_filtrados <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = fecha_seleccionada,
          dimension = "completo",
          estado = input$entidad,
          distrito = "Todos",
          municipio = "Todos",
          seccion = "Todas",
          incluir_extranjero = TRUE,
          solo_extranjero = FALSE
        )
      }, error = function(e) {
        message("❌ Error cargando datos para distritos: ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_filtrados) && !is.null(datos_filtrados$datos)) {
        if ("cabecera_distrital" %in% colnames(datos_filtrados$datos)) {
          distritos_unicos <- sort(unique(datos_filtrados$datos$cabecera_distrital))
          distritos_unicos <- distritos_unicos[distritos_unicos != "RESIDENTES EXTRANJERO"]
          distritos <- c("Todos", distritos_unicos)
          
          current_distrito <- isolate(input$distrito)
          selected_distrito <- if (!is.null(current_distrito) && current_distrito %in% distritos) {
            current_distrito
          } else {
            "Todos"
          }
          
          updateSelectInput(session, "distrito",
                            choices = distritos,
                            selected = selected_distrito)
          
          message("✅ Distritos de ", input$entidad, ": ", length(distritos) - 1)
        } else {
          updateSelectInput(session, "distrito", choices = c("Todos"), selected = "Todos")
        }
      } else {
        updateSelectInput(session, "distrito", choices = c("Todos"), selected = "Todos")
      }
    }, priority = 40, ignoreInit = TRUE)
    
    # ✅ CORRECCIÓN PROBLEMA 2: Remover input$ambito_datos de dependencias
    # PASO 3: Actualizar MUNICIPIOS
    observeEvent(list(input$distrito, input$entidad, input$year, input$date), {
      req(input$distrito, input$year, input$date)
      
      # ✅ VERIFICAR ámbito sin incluirlo en dependencias
      if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
        # NO resetear, solo mantener actual valor o "Todos"
        current_municipio <- isolate(input$municipio)
        if (is.null(current_municipio) || current_municipio == "") {
          updateSelectInput(session, "municipio", choices = c("Todos"), selected = "Todos")
        }
        return()
      }
      
      req(input$entidad)
      
      if (input$entidad == "Nacional" || input$distrito == "Todos") {
        # NO resetear si ya es "Todos"
        current_municipio <- isolate(input$municipio)
        if (current_municipio != "Todos") {
          updateSelectInput(session, "municipio", choices = c("Todos"), selected = "Todos")
          message("🗺️ Distrito = Todos → Municipio resetado")
        }
        return()
      }
      
      message("🔍 [FILTRADO CASCADA] Cargando municipios para: ", input$entidad, " - ", input$distrito)
      
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) NULL)
      
      if (is.null(fecha_seleccionada)) {
        return()
      }
      
      datos_filtrados <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = fecha_seleccionada,
          dimension = "completo",
          estado = input$entidad,
          distrito = input$distrito,
          municipio = "Todos",
          seccion = "Todas",
          incluir_extranjero = TRUE,
          solo_extranjero = FALSE
        )
      }, error = function(e) {
        message("❌ Error cargando datos para municipios: ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_filtrados) && !is.null(datos_filtrados$datos)) {
        if ("nombre_municipio" %in% colnames(datos_filtrados$datos)) {
          municipios_unicos <- sort(unique(datos_filtrados$datos$nombre_municipio))
          municipios_unicos <- municipios_unicos[municipios_unicos != "RESIDENTES EXTRANJERO"]
          municipios <- c("Todos", municipios_unicos)
          
          current_municipio <- isolate(input$municipio)
          selected_municipio <- if (!is.null(current_municipio) && current_municipio %in% municipios) {
            current_municipio
          } else {
            "Todos"
          }
          
          updateSelectInput(session, "municipio",
                            choices = municipios,
                            selected = selected_municipio)
          
          message("✅ Municipios: ", length(municipios) - 1)
        } else {
          updateSelectInput(session, "municipio", choices = c("Todos"), selected = "Todos")
        }
      } else {
        updateSelectInput(session, "municipio", choices = c("Todos"), selected = "Todos")
      }
    }, priority = 30, ignoreInit = TRUE)
    
    # ✅ CORRECCIÓN PROBLEMA 2: Remover input$ambito_datos de dependencias
    # PASO 4: Actualizar SECCIONES
    observeEvent(list(input$municipio, input$distrito, input$entidad, input$year, input$date), {
      req(input$municipio, input$year, input$date)
      
      # ✅ VERIFICAR ámbito sin incluirlo en dependencias
      if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
        # NO resetear, solo mantener actual valor o "Todas"
        current_seccion <- isolate(input$seccion)
        if (is.null(current_seccion) || length(current_seccion) == 0) {
          updateSelectizeInput(session, "seccion", 
                               choices = c("Todas"), 
                               selected = "Todas",
                               options = list(
                                 placeholder = "Selecciona una o más secciones",
                                 plugins = list("remove_button"),
                                 maxItems = NULL
                               ))
        }
        return()
      }
      
      req(input$distrito, input$entidad)
      
      if (input$entidad == "Nacional" || input$distrito == "Todos" || input$municipio == "Todos") {
        # NO resetear si ya es "Todas"
        current_seccion <- isolate(input$seccion)
        if (!"Todas" %in% current_seccion) {
          updateSelectizeInput(session, "seccion", 
                               choices = c("Todas"), 
                               selected = "Todas",
                               options = list(
                                 placeholder = "Selecciona una o más secciones",
                                 plugins = list("remove_button"),
                                 maxItems = NULL
                               ))
          message("🗺️ Municipio = Todos → Sección resetada")
        }
        return()
      }
      
      message("🔍 [FILTRADO CASCADA] Cargando secciones para: ", input$municipio)
      
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) NULL)
      
      if (is.null(fecha_seleccionada)) {
        return()
      }
      
      datos_filtrados <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = fecha_seleccionada,
          dimension = "completo",
          estado = input$entidad,
          distrito = input$distrito,
          municipio = input$municipio,
          seccion = "Todas",
          incluir_extranjero = TRUE,
          solo_extranjero = FALSE
        )
      }, error = function(e) {
        message("❌ Error cargando datos para secciones: ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_filtrados) && !is.null(datos_filtrados$datos)) {
        if ("seccion" %in% colnames(datos_filtrados$datos)) {
          secciones_unicas <- sort(unique(as.character(datos_filtrados$datos$seccion)))
          secciones_unicas <- secciones_unicas[secciones_unicas != "0"]
          secciones <- c("Todas", secciones_unicas)
          
          current_seccion <- isolate(input$seccion)
          
          if (!is.null(current_seccion) && length(current_seccion) > 0) {
            if ("Todas" %in% current_seccion) {
              selected_seccion <- "Todas"
            } else {
              valid_secciones <- current_seccion[current_seccion %in% secciones]
              selected_seccion <- if (length(valid_secciones) > 0) valid_secciones else "Todas"
            }
          } else {
            selected_seccion <- "Todas"
          }
          
          updateSelectizeInput(session, "seccion",
                               choices = secciones,
                               selected = selected_seccion,
                               options = list(
                                 placeholder = "Selecciona una o más secciones",
                                 plugins = list("remove_button"),
                                 maxItems = NULL
                               ))
          
          message("✅ Secciones: ", length(secciones) - 1)
        } else {
          updateSelectizeInput(session, "seccion", 
                               choices = c("Todas"), 
                               selected = "Todas",
                               options = list(
                                 placeholder = "Selecciona una o más secciones",
                                 plugins = list("remove_button"),
                                 maxItems = NULL
                               ))
        }
      } else {
        updateSelectizeInput(session, "seccion", 
                             choices = c("Todas"), 
                             selected = "Todas",
                             options = list(
                               placeholder = "Selecciona una o más secciones",
                               plugins = list("remove_button"),
                               maxItems = NULL
                             ))
      }
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
        
        fechas_year <- catalog$historico[format(catalog$historico, "%Y") == años_disponibles[1]]
        if (length(fechas_year) > 0) {
          fechas_year_sorted <- sort(fechas_year, decreasing = TRUE)
          updateSelectInput(session, "date", selected = as.character(fechas_year_sorted[1]))
          message("   ✅ date → ", as.character(fechas_year_sorted[1]))
        }
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
    
    message("✅ Módulo lista_nominal_server inicializado")
  })
}