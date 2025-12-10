# modules/lista_nominal_server_main.R
# Versión: 2.0 - Control total por botón Consultar para DataTable
# Coordinador principal que integra los módulos de gráficas y tablas

lista_nominal_server_main <- function(input, output, session, datos_columnas, combinacion_valida, estado_app) {
  ns <- session$ns
  
  # ========== CARGAR Y EJECUTAR MÓDULO DE GRÁFICAS ==========
  
  if (file.exists("modules/lista_nominal_server_graficas.R")) {
    source("modules/lista_nominal_server_graficas.R", local = TRUE)
    lista_nominal_server_graficas(input, output, session, datos_columnas, combinacion_valida)
    message("✅ Módulo de gráficas cargado correctamente")
  } else {
    message("⚠️ No se encontró lista_nominal_server_graficas.R")
  }
  
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
  
  # ========== ✅ RENDERIZAR TABLA CON CONTROL POR BOTÓN ==========
  
  output$`main-table_data` <- renderDT({
    
    # ========== ✅ CONTROL DE ESTADO - NO RENDERIZAR EN ESTADO INICIAL ==========
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
      req(input$btn_consultar > 0)  # ✅ CRÍTICO: Requiere botón presionado
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
    
    if (length(nombres_columnas) != length(columnas_seleccionadas)) {
      message("⚠️ Error: Longitud de nombres no coincide")
    }
    
    message("🔍 Columnas tabla: ", paste(columnas_seleccionadas, collapse = ", "))
    
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
    
    # Crear DataTable
    dt <- datatable(
      datos_tabla,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: left; font-size: 10px; color: #666666; font-family: Arial, sans-serif;",
        "Fuente: INE. Padrón Electoral y Lista Nominal de Electores."
      ),
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "Todos")),
        dom = 'lfrtip',
        columnDefs = column_defs,
        scrollX = TRUE
      ),
      escape = FALSE
    )
    
    message("✅ DataTable renderizado correctamente")
    dt
    
  }) %>%
    # ========== ✅ BINDEVET CRÍTICO: CONTROLA CUÁNDO SE EJECUTA renderDT ==========
  bindEvent(
    estado_app(),           # Escucha cambios de estado
    input$btn_consultar,    # Escucha botón Consultar
    ignoreNULL = FALSE,     # Permitir valores NULL
    ignoreInit = FALSE      # Renderizar en la carga inicial
  )
  
  # ========== DESCARGA CSV ==========
  
  output$download_csv <- downloadHandler(
    filename = function() {
      tipo <- if (input$tipo_corte == "historico") "historico" else "semanal"
      fecha_str <- format(as.Date(input$date), "%Y%m%d")
      entidad_str <- gsub(" ", "_", input$entidad)
      paste0("lista_nominal_", tipo, "_", fecha_str, "_", entidad_str, ".csv")
    },
    content = function(file) {
      datos <- datos_columnas()
      req(is.list(datos), !is.null(datos$datos))
      
      df <- datos$datos
      req(is.data.frame(df), nrow(df) > 0)
      
      # Definir columnas para exportación (igual que la tabla)
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
      
      # Aplicar nombres legibles
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
      
      message("🔍 Filas exportadas: ", nrow(datos_tabla))
      
      # Escribir CSV
      write.csv(datos_tabla, file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
      write("Fuente: INE. Padrón Electoral y Lista Nominal de Electores.", 
            file, append = TRUE)
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
    updateSelectInput(session, "entidad", selected = "Nacional")
    updateSelectInput(session, "distrito", selected = "Todos")
    updateSelectInput(session, "municipio", selected = "Todos")
    updateSelectInput(session, "seccion", selected = "Todas")
    
    if (!is.null(input$desglose)) {
      updateSelectInput(session, "desglose", selected = "Sexo")
    }
    
    message("✅ Configuración de Lista Nominal restablecida correctamente")
  })
  
  message("✅ Módulo lista_nominal_server_main inicializado correctamente")
}