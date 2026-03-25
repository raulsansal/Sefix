# modules/lista_nominal_server.R
# Versión: 3.9 - Fases 1b + 1c + 1d
#
# CAMBIOS vs v3.8:
#   1b. Al cambiar tipo_corte → semanal: resetear entidad→Nacional, filtros→Todos
#   1c. obtener_fecha_para_filtros() siempre usa fechas HISTÓRICAS (la geografía
#       es idéntica en ambos cortes; evita fallo cuando semanal no tiene fechas
#       para el año seleccionado)
#   1d. Botón "Restablecer" contextual:
#       - En histórico: regresa a Nacional + año más reciente (comportamiento actual)
#       - En semanal:   regresa a Nacional dentro de semanal, SIN cambiar tipo_corte

# ── Meses en español ──────────────────────────────────────────────────────────
meses_es <- c(
  "enero","febrero","marzo","abril","mayo","junio",
  "julio","agosto","septiembre","octubre","noviembre","diciembre"
)
names(meses_es) <- c(
  "January","February","March","April","May","June",
  "July","August","September","October","November","December"
)

formatear_fecha_es <- function(fecha, formato = "%d de %B de %Y") {
  if (is.null(fecha) || is.na(fecha)) return("")
  fecha_str <- format(as.Date(fecha), formato)
  for (mes_en in names(meses_es)) fecha_str <- gsub(mes_en, meses_es[mes_en], fecha_str)
  return(fecha_str)
}

lista_nominal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("modules/lista_nominal_helpers_ui.R", local = TRUE)
    message("✅ Helpers UI cargados")
    
    estado_app <- reactiveVal("restablecido")
    
    source("modules/lista_nominal_server_main.R", local = TRUE)
    source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
    
    # ── Última fecha disponible ───────────────────────────────────────────────
    ultima_fecha_disponible <- reactive({
      req(input$tipo_corte, input$year)
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      fechas_year <- if (input$tipo_corte == "historico") {
        catalog$historico[format(catalog$historico, "%Y") == input$year]
      } else {
        catalog$semanal_comun[format(catalog$semanal_comun, "%Y") == input$year]
      }
      if (length(fechas_year) == 0) return(NULL)
      ultima <- max(fechas_year)
      message("📅 [ultima_fecha_disponible] ", input$year, ": ", ultima)
      as.Date(ultima, origin = "1970-01-01")
    })
    
    # ── 1c. obtener_fecha_para_filtros: SIEMPRE usa históricas ───────────────
    # La geografía (distritos, municipios, secciones) es idéntica en ambos
    # cortes. Usar siempre el catálogo histórico evita fallos cuando semanal
    # no tiene fechas para el año actualmente seleccionado.
    obtener_fecha_para_filtros <- function() {
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        message("❌ [obtener_fecha_para_filtros] LNE_CATALOG no existe")
        return(NULL)
      }
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      # Intentar con el año seleccionado (siempre en histórico)
      year_val <- isolate(input$year)
      if (!is.null(year_val) && !is.na(year_val) && year_val != "") {
        fechas_year <- catalog$historico[format(catalog$historico, "%Y") == year_val]
        if (length(fechas_year) > 0) {
          fecha <- as.Date(max(fechas_year), origin = "1970-01-01")
          message("✅ [obtener_fecha_para_filtros] año ", year_val, " → ", fecha)
          return(fecha)
        }
      }
      
      # Fallback: última fecha histórica disponible
      if (length(catalog$historico) > 0) {
        fecha <- as.Date(max(catalog$historico), origin = "1970-01-01")
        message("⚠️ [obtener_fecha_para_filtros] Usando última histórica: ", fecha)
        return(fecha)
      }
      
      message("❌ [obtener_fecha_para_filtros] Sin fechas disponibles")
      return(NULL)
    }
    
    # ── Info tipo de corte ────────────────────────────────────────────────────
    output$info_tipo_corte <- renderUI({
      req(input$tipo_corte)
      if (input$tipo_corte == "historico") {
        HTML("<div style='background-color:#e8f4f8;padding:10px;border-radius:5px;margin-top:10px;'>
              <small><strong>Datos históricos anuales</strong><br>
              Información mensual agregada por entidad, distrito, municipio y sección.<br>
              Periodo: 2017 a la última actualización</small></div>")
      } else {
        anio_sem <- tryCatch({
          if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
            cat <- get("LNE_CATALOG", envir = .GlobalEnv)
            format(max(as.Date(cat$semanal_comun, origin = "1970-01-01"),
                       na.rm = TRUE), "%Y")
          } else format(Sys.Date(), "%Y")
        }, error = function(e) format(Sys.Date(), "%Y"))
        HTML(paste0(
          "<div style='background-color:#fff4e6;padding:10px;border-radius:5px;margin-top:10px;'>",
          "<small><strong>Datos detallados del año en curso</strong><br>",
          "Desgloses por edad, sexo y entidad de origen.<br>",
          "Periodo: enero ", anio_sem, " a la \u00faltima actualizaci\u00f3n</small></div>"
        ))
      }
    })
    
    # ── Selector de desglose (solo semanal) ───────────────────────────────────
    output$selector_desglose <- renderUI({
      selectInput(
        ns("desglose"),
        "Desglose:",
        choices  = c("Rango de Edad" = "edad", "Distribución por Sexo" = "sexo", "Entidad de Origen" = "origen"),
        selected = "edad"
      )
    })
    
    # ── Encabezado principal ──────────────────────────────────────────────────
    output$encabezado_principal <- renderUI({
      req(input$tipo_corte)
      fecha <- ultima_fecha_disponible()
      if (is.null(fecha)) return(h3("Lista Nominal Electoral", style = "color:#666;"))
      tipo_texto  <- if (input$tipo_corte == "historico") "Datos Históricos" else "Datos Semanales"
      ambito_texto <- if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
        "Extranjero"
      } else if (!is.null(input$entidad)) input$entidad else "Nacional"
      HTML(paste0(
        "<h3>Lista Nominal Electoral - ", tipo_texto, "</h3>",
        "<p style='font-size:14px;color:#666;'>Corte: <strong>",
        formatear_fecha_es(fecha, "%d de %B de %Y"),
        "</strong> | Ámbito: <strong>", ambito_texto, "</strong></p>"
      ))
    })
    
    # ── Actualizar años al cambiar tipo_corte ─────────────────────────────────
    observeEvent(input$tipo_corte, {
      req(input$tipo_corte)
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return()
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (input$tipo_corte == "historico") {
        años <- sort(unique(format(catalog$historico, "%Y")), decreasing = TRUE)
        updateSelectInput(session, "year", choices = años, selected = años[1])
        message("📅 Años históricos: ", paste(años, collapse = ", "))
      } else {
        años <- sort(unique(format(catalog$semanal_comun, "%Y")), decreasing = TRUE)
        updateSelectInput(session, "year", choices = años, selected = años[1])
        message("📅 Años semanales: ", paste(años, collapse = ", "))
      }
    }, priority = 100)
    
    # ── 1b. Resetear filtros geográficos al cambiar a semanal ─────────────────
    # Al pasar a semanal el usuario siempre arranca en Nacional sin filtros.
    # Al volver a histórico NO reseteamos — el usuario conserva su selección
    # (puede querer seguir comparando la misma geografía).
    observeEvent(input$tipo_corte, {
      req(input$tipo_corte)
      if (input$tipo_corte == "semanal") {
        message("🔄 [TIPO_CORTE → semanal] Reseteando filtros geográficos a valores por defecto")
        updateSelectInput(session, "entidad",   selected = "Nacional")
        updateSelectInput(session, "distrito",  selected = "Todos")
        updateSelectInput(session, "municipio", selected = "Todos")
        updateSelectizeInput(session, "seccion", selected = "Todas",
                             options = list(placeholder = "Selecciona una o más secciones",
                                            plugins = list("remove_button"), maxItems = NULL))
        updateRadioButtons(session, "ambito_datos", selected = "nacional")
        # Resetear estado para que cargue la vista semanal por defecto (Nacional)
        estado_app("restablecido")
        message("   ✅ Filtros reseteados | estado → restablecido")
      }
    }, priority = 90)
    
    # ── Carga inicial de datos ────────────────────────────────────────────────
    cargar_datos_defecto <- function() {
      message("🚀 [CARGA INICIAL] Cargando datos por defecto...")
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog    <- get("LNE_CATALOG", envir = .GlobalEnv)
      ultima_fecha <- max(catalog$historico)
      message("📅 [CARGA INICIAL] Última fecha: ", ultima_fecha)
      datos_lne <- tryCatch(
        cargar_lne(tipo_corte = "historico", fecha = ultima_fecha, dimension = "completo",
                   estado = "Nacional", distrito = "Todos", municipio = "Todos",
                   seccion = "Todas", incluir_extranjero = TRUE),
        error = function(e) { message("❌ [CARGA INICIAL] Error: ", e$message); NULL }
      )
      if (!is.null(datos_lne)) message("✅ [CARGA INICIAL] Datos cargados: ", nrow(datos_lne$datos), " filas")
      return(datos_lne)
    }
    
    combinacion_valida <- reactive({
      if (estado_app() %in% c("inicial", "restablecido")) return(TRUE)
      req(input$tipo_corte, input$year)
      fecha_disponible <- ultima_fecha_disponible()
      !is.null(fecha_disponible) && !is.na(fecha_disponible)
    })
    
    datos_columnas <- reactive({
      if (estado_app() == "restablecido") {
        message("🚀 [DATOS_COLUMNAS] CARGA RESTABLECIDA")
        return(cargar_datos_defecto())
      }
      if (estado_app() == "consultado") {
        req(input$btn_consultar > 0)
        tipo_corte        <- isolate(input$tipo_corte)
        year              <- isolate(input$year)
        entidad           <- isolate(input$entidad)
        distrito          <- isolate(input$distrito   %||% "Todos")
        municipio         <- isolate(input$municipio  %||% "Todos")
        seccion           <- isolate(input$seccion    %||% "Todas")
        desglose          <- isolate(input$desglose   %||% "edad")
        ambito            <- isolate(input$ambito_datos %||% "nacional")
        fecha_seleccionada <- isolate(ultima_fecha_disponible())
        
        if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
          message("❌ Sin fecha para año ", year); return(NULL)
        }
        estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
        dimension <- if (tipo_corte == "semanal") desglose else "completo"
        
        message("📂 cargar_lne: tipo=", tipo_corte, " dim=", dimension,
                " fecha=", fecha_seleccionada, " estado=", estado_filtro)
        
        datos_lne <- tryCatch(
          cargar_lne(tipo_corte = tipo_corte, fecha = fecha_seleccionada,
                     dimension = dimension, estado = estado_filtro,
                     distrito = distrito, municipio = municipio, seccion = seccion,
                     incluir_extranjero = TRUE),
          error = function(e) { message("❌ Error cargar_lne: ", e$message); NULL }
        )
        if (is.null(datos_lne) || !"datos" %in% names(datos_lne) || nrow(datos_lne$datos) == 0) {
          message("⚠️ Sin datos tras filtros"); return(NULL)
        }
        message("✅ Datos LNE: ", nrow(datos_lne$datos), " filas")
        return(datos_lne)
      }
      return(cargar_datos_defecto())
    }) %>% bindCache(estado_app(), input$btn_consultar, input$tipo_corte, input$year,
                     input$entidad, input$distrito, input$municipio, input$seccion, input$ambito_datos)
    
    # ── Filtros en cascada ────────────────────────────────────────────────────
    
    # Paso 1: poblar entidades (solo al inicio)
    observeEvent(input$tipo_corte, {
      todos_estados  <- get_entidades()
      current_estado <- isolate(input$entidad)
      selected       <- if (!is.null(current_estado) && current_estado %in% todos_estados) current_estado else "Nacional"
      updateSelectInput(session, "entidad", choices = todos_estados, selected = selected)
      message("🗺️ [CASCADA] Estados: ", length(todos_estados) - 1, " entidades")
    }, priority = 50, once = TRUE)
    
    # Paso 2: distritos
    observeEvent(input$entidad, {
      req(input$entidad)
      fecha_actual <- obtener_fecha_para_filtros()
      if (is.null(fecha_actual)) return()
      nuevos_distritos <- get_distritos_por_entidad(entidad = input$entidad, fecha = fecha_actual)
      current <- isolate(input$distrito)
      selected <- if (!is.null(current) && current %in% nuevos_distritos) current else "Todos"
      updateSelectInput(session, "distrito", choices = nuevos_distritos, selected = selected)
      message("🗺️ [CASCADA] Distritos para ", input$entidad, ": ", length(nuevos_distritos) - 1)
    }, priority = 40, ignoreInit = TRUE)
    
    # Paso 3: municipios
    observeEvent(input$distrito, {
      req(input$distrito)
      entidad_actual <- isolate(input$entidad)
      if (is.null(entidad_actual)) return()
      fecha_actual <- obtener_fecha_para_filtros()
      if (is.null(fecha_actual)) return()
      nuevos_municipios <- get_municipios_por_distrito(entidad = entidad_actual,
                                                       distrito = input$distrito,
                                                       fecha = fecha_actual)
      current  <- isolate(input$municipio)
      selected <- if (!is.null(current) && current %in% nuevos_municipios) current else "Todos"
      updateSelectInput(session, "municipio", choices = nuevos_municipios, selected = selected)
      message("🗺️ [CASCADA] Municipios para ", input$distrito, ": ", length(nuevos_municipios) - 1)
    }, priority = 30, ignoreInit = TRUE)
    
    # Paso 4: secciones
    observeEvent(input$municipio, {
      req(input$municipio)
      entidad_actual  <- isolate(input$entidad)
      distrito_actual <- isolate(input$distrito)
      if (is.null(entidad_actual) || is.null(distrito_actual)) return()
      fecha_actual <- obtener_fecha_para_filtros()
      if (is.null(fecha_actual)) return()
      nuevas_secciones <- get_secciones_por_municipio(entidad = entidad_actual,
                                                      distrito = distrito_actual,
                                                      municipio = input$municipio,
                                                      fecha = fecha_actual)
      current  <- isolate(input$seccion)
      selected <- if (!is.null(current) && length(current) > 0) {
        if ("Todas" %in% current) "Todas"
        else { valid <- current[current %in% nuevas_secciones]; if (length(valid) > 0) valid else "Todas" }
      } else "Todas"
      updateSelectizeInput(session, "seccion", choices = nuevas_secciones, selected = selected,
                           options = list(placeholder = "Selecciona una o más secciones",
                                          plugins = list("remove_button"), maxItems = NULL))
      message("🗺️ [CASCADA] Secciones para ", input$municipio, ": ", length(nuevas_secciones) - 1)
    }, priority = 20, ignoreInit = TRUE)
    
    # Paso 5: limpiar "Todas" cuando se combina
    observeEvent(input$seccion, {
      req(input$seccion)
      if (length(input$seccion) > 1 && "Todas" %in% input$seccion) {
        updateSelectizeInput(session, "seccion", selected = "Todas",
                             options = list(placeholder = "Selecciona una o más secciones",
                                            plugins = list("remove_button"), maxItems = NULL))
      }
    }, priority = 10, ignoreInit = TRUE)
    
    # ── Botón Consultar ───────────────────────────────────────────────────────
    observeEvent(input$btn_consultar, {
      req(input$btn_consultar > 0)
      message("🔍 [CONSULTAR] → estado: consultado")
      estado_app("consultado")
    }, ignoreInit = TRUE)
    
    # ── 1d. Botón Restablecer (contextual) ───────────────────────────────────
    observeEvent(input$reset_config, {
      tipo_actual <- isolate(input$tipo_corte)
      message("🔄 [RESTABLECER] tipo_corte actual: ", tipo_actual)
      
      # PASO 1: estado inicial para detener reactivos
      estado_app("inicial")
      
      # PASO 2: limpiar caché
      if (exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
        assign("LNE_CACHE_GRAFICAS", list(
          datos_year_actual = NULL, datos_anuales = NULL,
          timestamp_year = NULL, timestamp_anuales = NULL, año_cacheado = NULL
        ), envir = .GlobalEnv)
        message("   🧹 Caché histórico limpiado")
      }
      if (exists("LNE_CACHE_SEMANAL", envir = .GlobalEnv)) {
        assign("LNE_CACHE_SEMANAL", list(
          edad = NULL, sexo = NULL, origen = NULL, fecha = NULL, timestamp = NULL
        ), envir = .GlobalEnv)
        message("   🧹 Caché semanal limpiado")
      }
      
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return()
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (tipo_actual == "historico") {
        # ── HISTÓRICO: regresa a Nacional + año más reciente ────────────────
        message("   ↩️ Restableciendo vista HISTÓRICA")
        updateRadioButtons(session, "tipo_corte",   selected = "historico")
        updateRadioButtons(session, "ambito_datos", selected = "nacional")
        if (length(catalog$historico) > 0) {
          años <- sort(unique(format(catalog$historico, "%Y")), decreasing = TRUE)
          updateSelectInput(session, "year", selected = años[1])
          message("   ✅ year → ", años[1])
        }
        updateSelectInput(session, "entidad",   selected = "Nacional")
        updateSelectInput(session, "distrito",  selected = "Todos")
        updateSelectInput(session, "municipio", selected = "Todos")
        updateSelectizeInput(session, "seccion", selected = "Todas",
                             options = list(placeholder = "Selecciona una o más secciones",
                                            plugins = list("remove_button"), maxItems = NULL))
        Sys.sleep(0.2)
        estado_app("restablecido")
        message("✅ [RESTABLECER] Histórico restablecido")
        
      } else {
        # ── SEMANAL: regresa a Nacional dentro de semanal, SIN cambiar tipo ─
        message("   ↩️ Restableciendo vista SEMANAL (sin cambiar tipo_corte)")
        updateRadioButtons(session, "ambito_datos", selected = "nacional")
        if (length(catalog$semanal_comun) > 0) {
          años <- sort(unique(format(catalog$semanal_comun, "%Y")), decreasing = TRUE)
          updateSelectInput(session, "year", selected = años[1])
          message("   ✅ year semanal → ", años[1])
        }
        updateSelectInput(session, "entidad",   selected = "Nacional")
        updateSelectInput(session, "distrito",  selected = "Todos")
        updateSelectInput(session, "municipio", selected = "Todos")
        updateSelectizeInput(session, "seccion", selected = "Todas",
                             options = list(placeholder = "Selecciona una o más secciones",
                                            plugins = list("remove_button"), maxItems = NULL))
        updateSelectInput(session, "desglose", selected = "edad")
        message("   ✅ desglose → edad (Rango de Edad)")
        Sys.sleep(0.2)
        estado_app("restablecido")
        message("✅ [RESTABLECER] Semanal restablecido (Nacional, sin cambiar tipo_corte)")
      }
    })
    
    # ── Submódulos ────────────────────────────────────────────────────────────
    if (file.exists("modules/lista_nominal_server_main.R")) {
      source("modules/lista_nominal_server_main.R", local = TRUE)
      lista_nominal_server_main(input, output, session, datos_columnas, combinacion_valida, estado_app)
    } else {
      message("⚠️ No se encontró lista_nominal_server_main.R")
    }
    
    core_reactives <- NULL
    data_reactives <- NULL
    
    if (file.exists("modules/lista_nominal_graficas/graficas_main.R")) {
      source("modules/lista_nominal_graficas/graficas_main.R", local = TRUE)
      graficas_result <- lista_nominal_server_graficas(input, output, session,
                                                       datos_columnas, combinacion_valida, estado_app)
      if (is.list(graficas_result)) {
        core_reactives <- graficas_result$core
        data_reactives <- graficas_result$data
        message("✅ Módulo de gráficas cargado")
        message("   ✅ v3.9: core_reactives y data_reactives capturados")
      }
    } else {
      message("⚠️ No se encontró graficas_main.R")
    }
    
    if (file.exists("modules/lista_nominal_server_text_analysis.R")) {
      source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
      if (!is.null(core_reactives) && !is.null(data_reactives)) {
        lista_nominal_server_text_analysis(
          input, output, session,
          datos_year_actual       = data_reactives$datos_year_actual,
          datos_anuales_completos = data_reactives$datos_anuales_completos,
          datos_year_consulta     = data_reactives$datos_year_consulta,
          filtros_usuario         = core_reactives$filtros_usuario,
          estado_app              = estado_app,
          anio_actual             = core_reactives$anio_actual,
          anio_consultado         = core_reactives$anio_consultado,
          ambito_reactivo         = core_reactives$ambito_reactivo,
          texto_alcance           = core_reactives$texto_alcance
        )
        message("✅ text_analysis v3.0 conectado")
      } else {
        lista_nominal_server_text_analysis(
          input, output, session,
          datos_year_actual = reactive(NULL), datos_anuales_completos = reactive(NULL),
          datos_year_consulta = reactive(NULL),
          filtros_usuario = reactive(list(entidad="Nacional", distrito="Todos",
                                          municipio="Todos", seccion="Todas")),
          estado_app = estado_app,
          anio_actual = reactive(as.integer(format(Sys.Date(), "%Y"))),
          anio_consultado = reactive(as.integer(format(Sys.Date(), "%Y"))),
          ambito_reactivo = reactive("nacional"),
          texto_alcance = reactive("Estado: Nacional - Distrito: Todos - Municipio: Todos - Sección: Todas")
        )
        message("⚠️ text_analysis: usando firma fallback")
      }
    }
    
    message("✅ lista_nominal_server v3.9 inicializado")
  })
}
