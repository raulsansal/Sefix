# modules/lista_nominal_server_main.R
# Versión: 3.4 - SIMPLIFICADO: Solo Ámbito y Alcance en caption (SIN fuente)
# Coordinador principal que integra los módulos de gráficas y tablas

lista_nominal_server_main <- function(input, output, session, datos_columnas, combinacion_valida, estado_app) {
  ns <- session$ns
  
  message("📊 Inicializando lista_nominal_server_main v3.4")
  
  # ========== CARGAR HELPERS PARA TEXTO DE ALCANCE ==========
  source("modules/lista_nominal_graficas/graficas_helpers.R", local = TRUE)
  message("✅ Helpers cargados para generar texto de alcance")
  
  # ========== CARGAR Y EJECUTAR MÓDULO DE GRÁFICAS ==========
  
  if (file.exists("modules/lista_nominal_server_graficas.R")) {
    source("modules/lista_nominal_server_graficas.R", local = TRUE)
    lista_nominal_server_graficas(input, output, session, datos_columnas, combinacion_valida)
    message("✅ Módulo de gráficas cargado correctamente")
  } else {
    message("⚠️ No se encontró lista_nominal_server_graficas.R")
  }
  
  # ========== REACTIVE: TEXTO DE ALCANCE PARA DATATABLE ==========
  
  texto_alcance <- reactive({
    texto <- generar_texto_alcance(input)
    return(texto)
  })
  
  # ========== REACTIVE: OBTENER ÚLTIMA FECHA DISPONIBLE ==========
  
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
    
    ultima <- max(fechas_year)
    return(as.Date(ultima, origin = "1970-01-01"))
  })
  
  # ========== DICCIONARIO DE ETIQUETAS PARA TABLA ==========
  
  etiquetas_mapeo_tabla <- list(
    "clave_entidad" = "Clave Entidad",
    "nombre_entidad" = "Entidad",
    "clave_distrito" = "Clave Distrito",
    "cabecera_distrital" = "Cabecera Distrital",
    "clave_municipio" = "Clave Municipio",
    "nombre_municipio" = "Municipio",
    "seccion" = "Sección",
    "padron_electoral" = "Padrón Electoral",
    "lista_nominal" = "Lista Nominal",
    "padron_hombres" = "Padrón Hombres",
    "padron_mujeres" = "Padrón Mujeres",
    "lista_hombres" = "Lista Hombres",
    "lista_mujeres" = "Lista Mujeres",
    "tasa_inclusion" = "Tasa de Inclusión (%)"
  )
  
  # ========== RENDERIZAR TABLA CON ENCABEZADO SIMPLE ==========
  
  output$`main-table_data` <- renderDT({
    
    # ========== CONTROL DE ESTADO ==========
    estado_actual <- estado_app()
    
    if (estado_actual == "inicial") {
      message("⏸️ [DATATABLE] Estado inicial - Mostrando mensaje")
      return(datatable(
        data.frame(Mensaje = "Configure su consulta y presione 'Consultar' para cargar datos"),
        options = list(
          pageLength = 10, 
          dom = 't',
          ordering = FALSE,
          searching = FALSE
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ))
    }
    
    # ========== VALIDAR ESTADO RESTABLECIDO O CONSULTADO ==========
    if (estado_actual == "consultado") {
      req(input$btn_consultar > 0)
      message("🔍 [DATATABLE] Renderizando en estado CONSULTADO - Botón: ", input$btn_consultar)
    } else {
      message("🔍 [DATATABLE] Renderizando en estado RESTABLECIDO")
    }
    
    # ========== VALIDAR datos_columnas ==========
    req(combinacion_valida())
    
    datos <- datos_columnas()
    req(is.list(datos), !is.null(datos$datos))
    
    df <- datos$datos
    req(is.data.frame(df), nrow(df) > 0)
    
    message("📊 [DATATABLE] Datos disponibles: ", nrow(df), " filas")
    
    # Definir columnas base
    columnas_base <- c("nombre_entidad", "seccion")
    
    if ("cabecera_distrital" %in% colnames(df)) {
      columnas_base <- c(columnas_base[1], "cabecera_distrital", columnas_base[-1])
    }
    if ("nombre_municipio" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "nombre_municipio")
    }
    
    # Columnas principales
    columnas_principales <- c("padron_electoral", "lista_nominal", "tasa_inclusion")
    
    # Obtener desglose actual
    desglose_actual <- input$desglose %||% "Sexo"
    
    # Determinar columnas de desglose
    columnas_desglose <- c()
    
    if (desglose_actual == "Sexo") {
      cols_sexo <- c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres")
      columnas_desglose <- cols_sexo[cols_sexo %in% colnames(df)]
    } else if (desglose_actual == "Rango de Edad") {
      cols_edad <- grep("^(padron|lista)_\\d+", colnames(df), value = TRUE, ignore.case = TRUE)
      columnas_desglose <- cols_edad
    } else if (desglose_actual == "Entidad de Origen") {
      cols_origen <- grep("^(pad|ln)_[A-Z]", colnames(df), value = TRUE, ignore.case = TRUE)
      columnas_desglose <- cols_origen
    }
    
    # Combinar todas las columnas
    columnas_seleccionadas <- c(columnas_base, columnas_principales, columnas_desglose)
    columnas_seleccionadas <- columnas_seleccionadas[columnas_seleccionadas %in% colnames(df)]
    
    if (length(columnas_seleccionadas) == 0) {
      message("⚠️ No hay columnas válidas para mostrar en la tabla")
      return(datatable(
        data.frame(Mensaje = "No hay datos disponibles"),
        options = list(pageLength = 10)
      ))
    }
    
    # Seleccionar datos
    datos_tabla <- df[, columnas_seleccionadas, drop = FALSE]
    
    # Aplicar nombres legibles a las columnas
    nombres_columnas <- sapply(columnas_seleccionadas, function(col) {
      if (col %in% names(etiquetas_mapeo_tabla)) {
        etiquetas_mapeo_tabla[[col]]
      } else {
        nombre_limpio <- gsub("_", " ", col)
        nombre_limpio <- gsub("padron", "Padrón", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- gsub("lista", "Lista", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- gsub("hombres", "H", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- gsub("mujeres", "M", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- tools::toTitleCase(nombre_limpio)
        nombre_limpio
      }
    })
    
    colnames(datos_tabla) <- nombres_columnas
    
    # Formatear tasa de inclusión
    if ("Tasa de Inclusión (%)" %in% colnames(datos_tabla)) {
      tasa_numeric <- suppressWarnings(as.numeric(datos_tabla[["Tasa de Inclusión (%)"]]))
      if (!all(is.na(tasa_numeric))) {
        datos_tabla[["Tasa de Inclusión (%)"]] <- sprintf("%.2f%%", tasa_numeric)
      } else {
        datos_tabla[["Tasa de Inclusión (%)"]] <- "NA%"
      }
    }
    
    # Identificar columnas numéricas para formateo con comas
    columnas_con_comas <- c(
      "Padrón Electoral", "Lista Nominal", 
      "Padrón Hombres", "Padrón Mujeres", 
      "Lista Hombres", "Lista Mujeres"
    )
    
    cols_numericas_adicionales <- grep("Padrón|Lista|PAD|LN", colnames(datos_tabla), value = TRUE)
    columnas_con_comas <- unique(c(columnas_con_comas, cols_numericas_adicionales))
    columnas_con_comas <- columnas_con_comas[columnas_con_comas %in% colnames(datos_tabla)]
    
    # Obtener índices de columnas (base 0 para JavaScript)
    indices_con_comas <- which(colnames(datos_tabla) %in% columnas_con_comas) - 1
    indices_con_comas <- indices_con_comas[!is.na(indices_con_comas) & indices_con_comas >= 0]
    
    # Configurar columnDefs para formateo
    column_defs <- if (length(indices_con_comas) > 0) list(
      list(
        targets = indices_con_comas,
        render = JS(
          "function(data, type, row) {",
          "  return type === 'display' && data != null ?",
          "    data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',') : data;",
          "}"
        )
      )
    ) else NULL
    
    # ========== OBTENER TEXTO DE ALCANCE ==========
    alcance_texto <- texto_alcance()
    
    # Determinar ámbito
    ambito_display <- if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
      "Extranjero"
    } else {
      "Nacional"
    }
    
    # ========== CREAR CAPTION SIMPLE: SOLO ÁMBITO Y ALCANCE ==========
    caption_html <- htmltools::tags$caption(
      style = "caption-side: top; text-align: center; margin-bottom: 10px;",
      htmltools::tags$div(
        style = "font-size: 14px; color: #555555; font-weight: bold; margin-bottom: 5px;",
        paste0("Ámbito: ", ambito_display)
      ),
      htmltools::tags$div(
        style = "font-size: 13px; color: #555555;",
        alcance_texto
      )
    )
    
    # Crear DataTable
    dt <- datatable(
      datos_tabla,
      caption = caption_html,
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "Todos")),
        dom = 'lfrtip',
        columnDefs = column_defs,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
    
    message("✅ DataTable renderizado con encabezado simple")
    dt
    
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      input$ambito_datos,
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  # ========== DESCARGA CSV ==========
  
  output$download_csv <- downloadHandler(
    filename = function() {
      tipo <- if (input$tipo_corte == "historico") "historico" else "semanal"
      
      fecha <- ultima_fecha_disponible()
      fecha_str <- if (!is.null(fecha)) {
        format(fecha, "%Y%m%d")
      } else {
        format(Sys.Date(), "%Y%m%d")
      }
      
      entidad_str <- gsub(" ", "_", input$entidad %||% "Nacional")
      
      ambito_str <- if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
        "Extranjero"
      } else {
        "Nacional"
      }
      
      nombre_archivo <- paste0("lista_nominal_", tipo, "_", fecha_str, "_", ambito_str, "_", entidad_str, ".csv")
      message("📥 [DESCARGA CSV] Nombre archivo: ", nombre_archivo)
      
      return(nombre_archivo)
    },
    content = function(file) {
      datos <- datos_columnas()
      req(is.list(datos), !is.null(datos$datos))
      
      df <- datos$datos
      req(is.data.frame(df), nrow(df) > 0)
      
      message("📥 [DESCARGA CSV] Preparando ", nrow(df), " filas para descarga")
      
      columnas_base <- c("nombre_entidad", "seccion")
      
      if ("cabecera_distrital" %in% colnames(df)) {
        columnas_base <- c(columnas_base[1], "cabecera_distrital", columnas_base[-1])
      }
      if ("nombre_municipio" %in% colnames(df)) {
        columnas_base <- c(columnas_base, "nombre_municipio")
      }
      
      columnas_principales <- c("padron_electoral", "lista_nominal", "tasa_inclusion")
      
      desglose_actual <- input$desglose %||% "Sexo"
      columnas_desglose <- c()
      
      if (desglose_actual == "Sexo") {
        cols_sexo <- c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres")
        columnas_desglose <- cols_sexo[cols_sexo %in% colnames(df)]
      } else if (desglose_actual == "Rango de Edad") {
        cols_edad <- grep("^(padron|lista)_\\d+", colnames(df), value = TRUE, ignore.case = TRUE)
        columnas_desglose <- cols_edad
      } else if (desglose_actual == "Entidad de Origen") {
        cols_origen <- grep("^(pad|ln)_[A-Z]", colnames(df), value = TRUE, ignore.case = TRUE)
        columnas_desglose <- cols_origen
      }
      
      columnas_seleccionadas <- c(columnas_base, columnas_principales, columnas_desglose)
      columnas_seleccionadas <- columnas_seleccionadas[columnas_seleccionadas %in% colnames(df)]
      
      datos_tabla <- df[, columnas_seleccionadas, drop = FALSE]
      
      nombres_columnas <- sapply(columnas_seleccionadas, function(col) {
        if (col %in% names(etiquetas_mapeo_tabla)) {
          etiquetas_mapeo_tabla[[col]]
        } else {
          nombre_limpio <- gsub("_", " ", col)
          nombre_limpio <- tools::toTitleCase(nombre_limpio)
          nombre_limpio
        }
      })
      
      colnames(datos_tabla) <- nombres_columnas
      
      alcance_info <- texto_alcance()
      ambito_display <- if (!is.null(input$ambito_datos) && input$ambito_datos == "extranjero") {
        "Extranjero"
      } else {
        "Nacional"
      }
      
      writeLines(c(
        paste0("# Ámbito: ", ambito_display),
        paste0("# ", alcance_info),
        "# Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
        ""
      ), file)
      
      write.table(datos_tabla, file, 
                  append = TRUE,
                  sep = ",", 
                  row.names = FALSE, 
                  fileEncoding = "UTF-8", 
                  quote = TRUE,
                  na = "")
      
      message("✅ [DESCARGA CSV] Archivo generado exitosamente")
    }
  )
  
  # ========== BOTÓN RESTABLECER ==========
  
  observeEvent(input$reset_config, {
    message("🔄 Restableciendo configuración de Lista Nominal Electoral")
    
    if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (input$tipo_corte == "historico" && length(catalog$historico) > 0) {
        año_reciente <- format(max(catalog$historico), "%Y")
        updateSelectInput(session, "year", selected = año_reciente)
      } else if (input$tipo_corte == "semanal" && length(catalog$semanal_comun) > 0) {
        año_reciente <- format(max(catalog$semanal_comun), "%Y")
        updateSelectInput(session, "year", selected = año_reciente)
      }
    }
    
    updateRadioButtons(session, "tipo_corte", selected = "historico")
    updateRadioButtons(session, "ambito_datos", selected = "nacional")
    updateSelectInput(session, "entidad", selected = "Nacional")
    updateSelectInput(session, "distrito", selected = "Todos")
    updateSelectInput(session, "municipio", selected = "Todos")
    updateSelectInput(session, "seccion", selected = "Todas")
    
    if (!is.null(input$desglose)) {
      updateSelectInput(session, "desglose", selected = "Sexo")
    }
    
    message("✅ Configuración de Lista Nominal restablecida correctamente")
  })
  
  message("✅ Módulo lista_nominal_server_main v3.4 inicializado (SIMPLIFICADO)")
}

