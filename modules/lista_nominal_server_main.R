# modules/lista_nominal_server_main.R
# Versión: 3.8 - Handler para botón de descarga móvil (download_csv_mobile)
# Cambios vs v3.7:
#   - Nuevo output$download_csv_mobile que replica funcionalidad de output$download_csv

lista_nominal_server_main <- function(input, output, session, datos_columnas, combinacion_valida, estado_app) {
  ns <- session$ns
  
  message("📊 Inicializando lista_nominal_server_main v3.8")
  
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
    "año" = "Año",
    "clave_entidad" = "Clave Entidad",
    "nombre_entidad" = "Entidad",
    "clave_distrito" = "Clave Distrito",
    "cabecera_distrital" = "Cabecera Distrital",
    "clave_municipio" = "Clave Municipio",
    "nombre_municipio" = "Municipio",
    "seccion" = "Sección",
    
    # Nacional
    "padron_nacional" = "Padrón Nacional",
    "padron_nacional_hombres" = "Padrón H",
    "padron_nacional_mujeres" = "Padrón M",
    "padron_nacional_no_binario" = "Padrón NB",
    "lista_nacional" = "Lista Nacional",
    "lista_nacional_hombres" = "Lista H",
    "lista_nacional_mujeres" = "Lista M",
    "lista_nacional_no_binario" = "Lista NB",
    
    # Extranjero
    "padron_extranjero" = "Padrón Extranjero",
    "padron_extranjero_hombres" = "Padrón H",
    "padron_extranjero_mujeres" = "Padrón M",
    "padron_extranjero_no_binario" = "Padrón NB",
    "lista_extranjero" = "Lista Extranjero",
    "lista_extranjero_hombres" = "Lista H",
    "lista_extranjero_mujeres" = "Lista M",
    "lista_extranjero_no_binario" = "Lista NB"
  )
  
  # ========== RENDERIZAR TABLA CON COLUMNAS DINÁMICAS ==========
  
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
    message("📊 [DATATABLE] Columnas disponibles en CSV: ", paste(colnames(df), collapse = ", "))
    
    # ========== AGREGAR COLUMNA AÑO ==========
    df$año <- isolate(input$year)
    
    # ========== DETERMINAR ÁMBITO ==========
    ambito <- isolate(input$ambito_datos %||% "nacional")
    message("📊 [DATATABLE] Ámbito seleccionado: ", ambito)
    
    # ========== DEFINIR COLUMNAS BASE (SIEMPRE PRESENTES) ==========
    columnas_base <- c("año", "nombre_entidad")
    
    if ("cabecera_distrital" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "cabecera_distrital")
    }
    if ("nombre_municipio" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "nombre_municipio")
    }
    if ("seccion" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "seccion")
    }
    
    # ========== DEFINIR COLUMNAS DE DATOS SEGÚN ÁMBITO ==========
    columnas_datos <- c()
    
    if (ambito == "nacional") {
      message("📊 [DATATABLE] Construyendo columnas para vista NACIONAL")
      
      if ("padron_nacional" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_nacional")
      }
      
      if ("padron_nacional_hombres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_nacional_hombres")
      }
      if ("padron_nacional_mujeres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_nacional_mujeres")
      }
      if ("padron_nacional_no_binario" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_nacional_no_binario")
      }
      
      if ("lista_nacional" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_nacional")
      }
      
      if ("lista_nacional_hombres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_nacional_hombres")
      }
      if ("lista_nacional_mujeres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_nacional_mujeres")
      }
      if ("lista_nacional_no_binario" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_nacional_no_binario")
      }
      
    } else {
      message("📊 [DATATABLE] Construyendo columnas para vista EXTRANJERO")
      
      if ("padron_extranjero" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_extranjero")
      }
      
      if ("padron_extranjero_hombres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_extranjero_hombres")
      }
      if ("padron_extranjero_mujeres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_extranjero_mujeres")
      }
      if ("padron_extranjero_no_binario" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "padron_extranjero_no_binario")
      }
      
      if ("lista_extranjero" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_extranjero")
      }
      
      if ("lista_extranjero_hombres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_extranjero_hombres")
      }
      if ("lista_extranjero_mujeres" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_extranjero_mujeres")
      }
      if ("lista_extranjero_no_binario" %in% colnames(df)) {
        columnas_datos <- c(columnas_datos, "lista_extranjero_no_binario")
      }
    }
    
    # ========== COMBINAR COLUMNAS ==========
    columnas_seleccionadas <- c(columnas_base, columnas_datos)
    
    message("📊 [DATATABLE] Columnas seleccionadas para mostrar: ", paste(columnas_seleccionadas, collapse = ", "))
    
    if (length(columnas_seleccionadas) == 0) {
      message("⚠️ No hay columnas válidas para mostrar en la tabla")
      return(datatable(
        data.frame(Mensaje = "No hay datos disponibles para esta configuración"),
        options = list(pageLength = 10)
      ))
    }
    
    # ========== SELECCIONAR DATOS ==========
    datos_tabla <- df[, columnas_seleccionadas, drop = FALSE]
    
    # ========== APLICAR NOMBRES LEGIBLES ==========
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
    
    message("📊 [DATATABLE] Nombres de columnas en tabla: ", paste(nombres_columnas, collapse = ", "))
    
    # ========== IDENTIFICAR COLUMNAS NUMÉRICAS PARA FORMATEO ==========
    columnas_con_comas <- nombres_columnas[grepl("Padrón|Lista", nombres_columnas)]
    indices_con_comas <- which(nombres_columnas %in% columnas_con_comas) - 1
    indices_con_comas <- indices_con_comas[!is.na(indices_con_comas) & indices_con_comas >= 0]
    
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
    ambito_display <- if (ambito == "extranjero") "Extranjero" else "Nacional"
    
    # ========== CREAR CAPTION ==========
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
    
    # ========== CREAR DATATABLE ==========
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
    
    message("✅ DataTable renderizado correctamente con ", ncol(datos_tabla), " columnas")
    dt
    
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  # ========== FUNCIÓN AUXILIAR PARA PREPARAR DATOS CSV ==========
  # ✅ v3.8: Función compartida para ambos botones de descarga
  
  preparar_datos_csv <- function() {
    datos <- datos_columnas()
    req(is.list(datos), !is.null(datos$datos))
    
    df <- datos$datos
    req(is.data.frame(df), nrow(df) > 0)
    
    df$año <- input$year
    ambito <- input$ambito_datos %||% "nacional"
    
    columnas_base <- c("año", "nombre_entidad")
    
    if ("cabecera_distrital" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "cabecera_distrital")
    }
    if ("nombre_municipio" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "nombre_municipio")
    }
    if ("seccion" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "seccion")
    }
    
    columnas_datos <- c()
    
    if (ambito == "nacional") {
      if ("padron_nacional" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_nacional")
      if ("padron_nacional_hombres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_nacional_hombres")
      if ("padron_nacional_mujeres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_nacional_mujeres")
      if ("padron_nacional_no_binario" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_nacional_no_binario")
      if ("lista_nacional" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_nacional")
      if ("lista_nacional_hombres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_nacional_hombres")
      if ("lista_nacional_mujeres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_nacional_mujeres")
      if ("lista_nacional_no_binario" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_nacional_no_binario")
    } else {
      if ("padron_extranjero" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_extranjero")
      if ("padron_extranjero_hombres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_extranjero_hombres")
      if ("padron_extranjero_mujeres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_extranjero_mujeres")
      if ("padron_extranjero_no_binario" %in% colnames(df)) columnas_datos <- c(columnas_datos, "padron_extranjero_no_binario")
      if ("lista_extranjero" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_extranjero")
      if ("lista_extranjero_hombres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_extranjero_hombres")
      if ("lista_extranjero_mujeres" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_extranjero_mujeres")
      if ("lista_extranjero_no_binario" %in% colnames(df)) columnas_datos <- c(columnas_datos, "lista_extranjero_no_binario")
    }
    
    columnas_seleccionadas <- c(columnas_base, columnas_datos)
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
    
    list(
      datos = datos_tabla,
      ambito = ambito
    )
  }
  
  # ========== FUNCIÓN AUXILIAR PARA GENERAR NOMBRE ARCHIVO ==========
  
  generar_nombre_archivo <- function() {
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
    
    paste0("lista_nominal_", tipo, "_", fecha_str, "_", ambito_str, "_", entidad_str, ".csv")
  }
  
  # ========== DESCARGA CSV (BOTÓN ORIGINAL EN SIDEBAR) ==========
  
  output$download_csv <- downloadHandler(
    filename = function() {
      nombre <- generar_nombre_archivo()
      message("📥 [DESCARGA CSV] Nombre archivo: ", nombre)
      return(nombre)
    },
    content = function(file) {
      resultado <- preparar_datos_csv()
      datos_tabla <- resultado$datos
      ambito <- resultado$ambito
      
      message("📥 [DESCARGA CSV] Preparando ", nrow(datos_tabla), " filas para descarga")
      message("📥 [DESCARGA CSV] Columnas exportadas: ", paste(colnames(datos_tabla), collapse = ", "))
      
      alcance_info <- texto_alcance()
      ambito_display <- if (ambito == "extranjero") "Extranjero" else "Nacional"
      
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
  
  # ========== ✅ v3.8: DESCARGA CSV MÓVIL (BOTÓN DEBAJO DEL DATATABLE) ==========
  
  output$download_csv_mobile <- downloadHandler(
    filename = function() {
      nombre <- generar_nombre_archivo()
      message("📥 [DESCARGA CSV MÓVIL] Nombre archivo: ", nombre)
      return(nombre)
    },
    content = function(file) {
      resultado <- preparar_datos_csv()
      datos_tabla <- resultado$datos
      ambito <- resultado$ambito
      
      message("📥 [DESCARGA CSV MÓVIL] Preparando ", nrow(datos_tabla), " filas para descarga")
      
      alcance_info <- texto_alcance()
      ambito_display <- if (ambito == "extranjero") "Extranjero" else "Nacional"
      
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
      
      message("✅ [DESCARGA CSV MÓVIL] Archivo generado exitosamente")
    }
  )
  
  # ========== ✅ v3.7: BOTÓN RESTABLECER - OBSERVER SEPARADO ==========
  
  observeEvent(input$reset_config, {
    message("🔄 [RESTABLECER MAIN] Botón presionado - Actualizando inputs adicionales...")
    
    if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (input$tipo_corte == "historico" && length(catalog$historico) > 0) {
        año_reciente <- format(max(catalog$historico), "%Y")
        updateSelectInput(session, "year", selected = año_reciente)
        message("   ✅ year actualizado → ", año_reciente)
      } else if (input$tipo_corte == "semanal" && length(catalog$semanal_comun) > 0) {
        año_reciente <- format(max(catalog$semanal_comun), "%Y")
        updateSelectInput(session, "year", selected = año_reciente)
        message("   ✅ year actualizado → ", año_reciente)
      }
    }
    
    if (!is.null(input$desglose)) {
      updateSelectInput(session, "desglose", selected = "Sexo")
      message("   ✅ desglose → Sexo")
    }
    
    message("✅ [RESTABLECER MAIN] Inputs adicionales actualizados correctamente")
  })
  
  message("✅ Módulo lista_nominal_server_main v3.8 inicializado")
  message("   ✅ v3.8: Handler para download_csv_mobile agregado")
}
