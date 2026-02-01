# modules/lista_nominal_server_text_analysis.R
# Versión: 3.0 - IMPLEMENTACIÓN COMPLETA: Análisis textual dinámico del sidebar derecho
# Consume: datos_year_actual, datos_anuales_completos, datos_year_consulta,
#          filtros_usuario, estado_app, anio_actual, anio_consultado,
#          ambito_reactivo, texto_alcance
# Genera:  3 bloques uiOutput en el sidebar derecho
# Fecha:   31 de enero de 2026

# ========== FUNCIÓN AUXILIAR: FORMATEAR FECHAS EN ESPAÑOL ==========
if (!exists("formatear_fecha_es")) {
  meses_es <- c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio",
    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
  )
  names(meses_es) <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  
  formatear_fecha_es <- function(fecha, formato = "%d de %B de %Y") {
    if (is.null(fecha) || is.na(fecha)) return("")
    fecha_str <- format(as.Date(fecha), formato)
    for (mes_en in names(meses_es)) {
      fecha_str <- gsub(mes_en, meses_es[mes_en], fecha_str)
    }
    return(fecha_str)
  }
}

# ========== FUNCIÓN PRINCIPAL DEL MÓDULO ==========

lista_nominal_server_text_analysis <- function(input, output, session,
                                               datos_year_actual,
                                               datos_anuales_completos,
                                               datos_year_consulta,
                                               filtros_usuario,
                                               estado_app,
                                               anio_actual,
                                               anio_consultado,
                                               ambito_reactivo,
                                               texto_alcance) {
  ns <- session$ns
  
  message("📝 Inicializando lista_nominal_server_text_analysis v3.0")
  
  # ============================================================
  # HELPERS INTERNOS
  # ============================================================
  
  # Formatear números grandes con separador de miles (coma)
  fmt_num <- function(x) {
    if (is.null(x) || is.na(x)) return("N/D")
    format(round(as.numeric(x)), big.mark = ",", scientific = FALSE)
  }
  
  # Formatear porcentajes con 2 decimales
  fmt_pct <- function(x) {
    if (is.null(x) || is.na(x)) return("N/D")
    paste0(round(as.numeric(x), 2), "%")
  }
  
  # Obtener nombre del mes en español a partir de una fecha
  nombre_mes_es <- function(fecha) {
    if (is.null(fecha) || is.na(fecha)) return("")
    meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
               "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    mes_num <- as.integer(format(as.Date(fecha), "%m"))
    return(meses[mes_num])
  }
  
  # Calcular porcentaje de crecimiento/disminución entre dos valores
  calcular_variacion <- function(valor_final, valor_inicial) {
    if (is.null(valor_inicial) || is.null(valor_final) || 
        is.na(valor_inicial) || is.na(valor_final) ||
        valor_inicial == 0) return(list(pct = NA, texto = "sin datos"))
    
    pct <- ((valor_final - valor_inicial) / abs(valor_inicial)) * 100
    texto <- if (pct >= 0) "crecido" else "disminuido"
    return(list(pct = round(pct, 2), texto = texto))
  }
  
  # Determinar sexo predominante comparando hombres vs mujeres
  sexo_predominante <- function(hombres, mujeres) {
    if (is.na(hombres) || is.na(mujeres)) return("sin datos suficientes")
    if (mujeres > hombres) return("mujeres")
    if (hombres > mujeres) return("hombres")
    return("ambos sexos por igual")
  }
  
  # Detectar si el usuario seleccionó un distrito de Residentes en el Extranjero
  es_distrito_extranjero <- function(distrito_val) {
    if (is.null(distrito_val) || distrito_val == "Todos") return(FALSE)
    return(grepl("RESIDENTES", toupper(distrito_val), fixed = TRUE))
  }
  
  # Obtener columnas según ámbito (nacional vs extranjero)
  cols_ambito <- function(ambito) {
    if (ambito == "extranjero") {
      return(list(
        padron = "padron_extranjero",
        lista  = "lista_extranjero",
        padron_h = "padron_extranjero_hombres",
        padron_m = "padron_extranjero_mujeres",
        lista_h  = "lista_extranjero_hombres",
        lista_m  = "lista_extranjero_mujeres",
        padron_nb = "padron_extranjero_no_binario",
        lista_nb  = "lista_extranjero_no_binario",
        etiqueta = "de Residentes en el Extranjero",
        etiqueta_corta = "de Residentes en el Extranjero"
      ))
    } else {
      return(list(
        padron = "padron_nacional",
        lista  = "lista_nacional",
        padron_h = "padron_hombres",
        padron_m = "padron_mujeres",
        lista_h  = "lista_hombres",
        lista_m  = "lista_mujeres",
        padron_nb = "padron_nacional_no_binario",
        lista_nb  = "lista_nacional_no_binario",
        etiqueta = "Nacional",
        etiqueta_corta = ""
      ))
    }
  }
  
  # Extraer valor seguro de un data.frame
  safe_val <- function(df, fila, col) {
    if (is.null(df) || nrow(df) < fila || !col %in% colnames(df)) return(NA_real_)
    val <- df[fila, col]
    if (is.null(val) || length(val) == 0) return(NA_real_)
    return(as.numeric(val))
  }
  
  # Detectar si vista extranjero necesita advertencia (distrito NO es Residentes Extranjero)
  necesita_advertencia_extranjero <- function(ambito, filtros) {
    if (ambito != "extranjero") return(FALSE)
    # Si es Nacional, se necesita advertencia porque los datos extranjero solo están a nivel estado+distrito especial
    if (filtros$entidad == "Nacional") return(FALSE)
    # Si el distrito NO es el de Residentes en el Extranjero
    if (filtros$distrito == "Todos" || !es_distrito_extranjero(filtros$distrito)) return(TRUE)
    return(FALSE)
  }
  
  # ============================================================
  # ESTILOS CSS INLINE REUTILIZABLES
  # ============================================================
  
  css_contenedor   <- "font-size: 13px; line-height: 1.6; color: #333;"
  css_titulo_h4    <- "margin: 0 0 8px 0; font-size: 15px; color: #2c3e50; font-weight: bold;"
  css_parrafo      <- "margin: 0 0 10px 0;"
  css_separador    <- "border: 0; border-top: 1px solid #e0e0e0; margin: 12px 0;"
  css_alerta       <- "text-align: center; color: #999; padding: 20px; font-style: italic;"
  css_advertencia  <- "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin: 8px 0; font-size: 12px; color: #856404;"
  css_num_principal <- "color: #2c3e50; font-weight: bold;"
  css_num_positivo  <- "color: #27ae60; font-weight: bold;"
  css_num_negativo  <- "color: #e74c3c; font-weight: bold;"
  
  # Helper para colorear variación
  css_variacion <- function(pct) {
    if (is.na(pct)) return(css_num_principal)
    if (pct >= 0) return(css_num_positivo) else return(css_num_negativo)
  }
  
  # ============================================================
  # OUTPUT 1: TÍTULO Y ALCANCE DEL ANÁLISIS
  # ============================================================
  
  output$`text_analysis-titulo_lista` <- renderUI({
    
    estado_actual <- estado_app()
    
    if (estado_actual == "inicial") {
      return(NULL)
    }
    
    message("📝 [TÍTULO] Renderizando en estado: ", estado_actual)
    
    # Obtener año consultado
    anio_consul <- tryCatch(anio_consultado(), error = function(e) anio_actual())
    
    # Obtener ámbito
    ambito <- tryCatch(ambito_reactivo(), error = function(e) "nacional")
    cols <- cols_ambito(ambito)
    
    # Texto de vista según ámbito
    texto_vista <- if (ambito == "extranjero") {
      paste0("An\u00e1lisis de Padr\u00f3n y Lista Nominal Electoral ", cols$etiqueta)
    } else {
      paste0("An\u00e1lisis de Padr\u00f3n y Lista Nominal Electoral ", cols$etiqueta)
    }
    
    # Texto de alcance (de texto_alcance reactive)
    alcance <- tryCatch(texto_alcance(), error = function(e) "")
    # Reformatear: "Estado: X - Distrito: Y" → "Estado: X – Distrito: Y"
    alcance_formateado <- gsub(" - ", " \u2013 ", alcance)
    
    HTML(paste0(
      "<div style='", css_contenedor, "'>",
      "<h3 style='margin: 0 0 4px 0; font-size: 16px; color: #2c3e50; font-weight: bold;'>",
      "Alcance del An\u00e1lisis</h3>",
      "<p style='margin: 0 0 4px 0; font-size: 15px; font-weight: bold; color: #2c3e50;'>",
      anio_consul, "</p>",
      "<p style='margin: 0 0 4px 0; font-size: 13px; color: #555;'>",
      texto_vista, "</p>",
      "<p style='margin: 0 0 0 0; font-size: 12px; color: #777;'>",
      alcance_formateado, "</p>",
      "</div>"
    ))
    
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ============================================================
  # OUTPUT 2: RESUMEN GENERAL (SOLO año == anio_actual)
  # ============================================================
  
  output$`text_analysis-resumen_general_lista` <- renderUI({
    
    estado_actual <- estado_app()
    
    # --- Estado inicial: mensaje de espera ---
    if (estado_actual == "inicial") {
      return(HTML(paste0(
        "<p style='", css_alerta, "'>",
        "Configure su consulta y presione 'Consultar' para ver el an\u00e1lisis</p>"
      )))
    }
    
    # --- Validar estado consultado ---
    if (estado_actual == "consultado") {
      req(input$btn_consultar > 0)
      message("📝 [RESUMEN] Renderizando en estado CONSULTADO - Bot\u00f3n: ", input$btn_consultar)
    } else {
      message("📝 [RESUMEN] Renderizando en estado RESTABLECIDO")
    }
    
    # --- Obtener año consultado y año actual ---
    anio_consul <- tryCatch(anio_consultado(), error = function(e) NULL)
    anio_act <- tryCatch(anio_actual(), error = function(e) NULL)
    
    if (is.null(anio_consul) || is.null(anio_act)) {
      return(HTML(paste0("<p style='", css_alerta, "'>Cargando datos...</p>")))
    }
    
    # --- Si el año consultado NO es el año actual: NO mostrar resumen general ---
    if (anio_consul != anio_act) {
      message("📝 [RESUMEN] A\u00f1o consultado (", anio_consul, ") != a\u00f1o actual (", anio_act, ") - Sin resumen general")
      return(NULL)
    }
    
    # --- Obtener ámbito y filtros ---
    ambito <- tryCatch(ambito_reactivo(), error = function(e) "nacional")
    filtros <- tryCatch(filtros_usuario(), error = function(e) list(entidad = "Nacional", distrito = "Todos"))
    cols <- cols_ambito(ambito)
    
    # --- Verificar advertencia para vista extranjero ---
    if (necesita_advertencia_extranjero(ambito, filtros)) {
      return(HTML(paste0(
        "<div style='", css_contenedor, "'>",
        "<h4 style='", css_titulo_h4, "'>a) Resumen general</h4>",
        "<div style='", css_advertencia, "'>",
        "<strong>\u26A0\uFE0F</strong> Selecciona \"Residentes en el Extranjero\" ",
        "en el campo \"Distrito Electoral\". Estos datos est\u00e1n disponibles a nivel estatal.",
        "</div>",
        "</div>"
      )))
    }
    
    # --- Obtener datos del año actual ---
    datos <- tryCatch(datos_year_actual(), error = function(e) NULL)
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(HTML(paste0("<p style='", css_alerta, "'>No hay datos disponibles.</p>")))
    }
    
    # Último registro (mes más reciente)
    ultimo <- datos[nrow(datos), ]
    ultima_fecha <- ultimo$fecha
    
    # Extraer valores según ámbito
    padron_val <- safe_val(datos, nrow(datos), cols$padron)
    lista_val  <- safe_val(datos, nrow(datos), cols$lista)
    
    if (is.na(padron_val) || padron_val == 0) {
      return(HTML(paste0("<p style='", css_alerta, "'>Sin datos para este \u00e1mbito.</p>")))
    }
    
    tasa_inclusion <- round((lista_val / padron_val) * 100, 2)
    mes_texto <- nombre_mes_es(ultima_fecha)
    anio_texto <- format(as.Date(ultima_fecha), "%Y")
    
    # Construir texto según ámbito
    if (ambito == "extranjero") {
      texto_padron <- paste0(
        "Al mes de ", mes_texto, " de ", anio_texto, ", ",
        "el Padr\u00f3n Electoral de Residentes en el Extranjero totaliza ",
        "<b style='", css_num_principal, "'>", fmt_num(padron_val), "</b> ciudadanos, ",
        "de los cuales, ",
        "<b style='", css_num_principal, "'>", fmt_num(lista_val), "</b> ",
        "est\u00e1n incluidos en la Lista Nominal de Residentes en el Extranjero, ",
        "lo que representa una tasa de inclusi\u00f3n de ",
        "<b style='", css_num_positivo, "'>", fmt_pct(tasa_inclusion), "</b>."
      )
    } else {
      texto_padron <- paste0(
        "Al mes de ", mes_texto, " de ", anio_texto, ", ",
        "el Padr\u00f3n Electoral totaliza ",
        "<b style='", css_num_principal, "'>", fmt_num(padron_val), "</b> ciudadanos, ",
        "de los cuales, ",
        "<b style='", css_num_principal, "'>", fmt_num(lista_val), "</b> ",
        "est\u00e1n incluidos en la Lista Nominal Electoral, ",
        "lo que representa una tasa de inclusi\u00f3n de ",
        "<b style='", css_num_positivo, "'>", fmt_pct(tasa_inclusion), "</b>."
      )
    }
    
    HTML(paste0(
      "<div style='", css_contenedor, "'>",
      "<h4 style='", css_titulo_h4, "'>a) Resumen general</h4>",
      "<p style='", css_parrafo, "'>", texto_padron, "</p>",
      "</div>"
    ))
    
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ============================================================
  # OUTPUT 3: EVOLUCIÓN TEMPORAL
  # ============================================================
  
  output$`text_analysis-comparacion_lista` <- renderUI({
    
    estado_actual <- estado_app()
    
    # --- Estado inicial: no mostrar ---
    if (estado_actual == "inicial") {
      return(NULL)
    }
    
    # --- Validar estado consultado ---
    if (estado_actual == "consultado") {
      req(input$btn_consultar > 0)
      message("📝 [EVOLUCI\u00d3N] Renderizando en estado CONSULTADO")
    } else {
      message("📝 [EVOLUCI\u00d3N] Renderizando en estado RESTABLECIDO")
    }
    
    # --- Obtener año consultado y año actual ---
    anio_consul <- tryCatch(anio_consultado(), error = function(e) NULL)
    anio_act <- tryCatch(anio_actual(), error = function(e) NULL)
    
    if (is.null(anio_consul) || is.null(anio_act)) {
      return(NULL)
    }
    
    # --- Obtener ámbito y filtros ---
    ambito <- tryCatch(ambito_reactivo(), error = function(e) "nacional")
    filtros <- tryCatch(filtros_usuario(), error = function(e) list(entidad = "Nacional", distrito = "Todos"))
    cols <- cols_ambito(ambito)
    
    # --- Verificar advertencia para vista extranjero ---
    if (necesita_advertencia_extranjero(ambito, filtros)) {
      return(HTML(paste0(
        "<div style='", css_contenedor, "'>",
        "<h4 style='", css_titulo_h4, "'>b) Evoluci\u00f3n temporal</h4>",
        "<div style='", css_advertencia, "'>",
        "<strong>\u26A0\uFE0F</strong> Selecciona \"Residentes en el Extranjero\" ",
        "en el campo \"Distrito Electoral\" porque estos datos est\u00e1n disponibles a nivel estatal.",
        "</div>",
        "</div>"
      )))
    }
    
    # ============================================================
    # RAMA I: Año consultado == año actual (gráficas 1, 2, 3)
    # → Evolución multi-anual (2017 → último año) o (2020 → último año para extranjero)
    # ============================================================
    
    if (anio_consul == anio_act) {
      
      datos_anuales <- tryCatch(datos_anuales_completos(), error = function(e) NULL)
      
      if (is.null(datos_anuales) || nrow(datos_anuales) == 0) {
        return(HTML(paste0(
          "<div style='", css_contenedor, "'>",
          "<h4 style='", css_titulo_h4, "'>b) Evoluci\u00f3n temporal</h4>",
          "<p style='", css_alerta, "'>Sin datos anuales disponibles.</p>",
          "</div>"
        )))
      }
      
      # Para extranjero, los datos empiezan en 2020
      if (ambito == "extranjero") {
        # Filtrar solo años donde hay datos de extranjero (columna no NA y > 0)
        datos_anuales_filtrado <- datos_anuales[!is.na(datos_anuales[[cols$padron]]) & datos_anuales[[cols$padron]] > 0, ]
        if (nrow(datos_anuales_filtrado) == 0) {
          return(HTML(paste0(
            "<div style='", css_contenedor, "'>",
            "<h4 style='", css_titulo_h4, "'>b) Evoluci\u00f3n temporal</h4>",
            "<p style='", css_alerta, "'>Sin datos de residentes en el extranjero disponibles.</p>",
            "</div>"
          )))
        }
      } else {
        datos_anuales_filtrado <- datos_anuales
      }
      
      # Primer y último año disponible
      # Acceder a la columna "año" usando [[ ]] para evitar problemas de encoding con $
      col_anio <- if ("año" %in% colnames(datos_anuales_filtrado)) {
        "año"
      } else if ("a\u00f1o" %in% colnames(datos_anuales_filtrado)) {
        "a\u00f1o"
      } else {
        colnames(datos_anuales_filtrado)[1]  # fallback: primera columna
      }
      
      primer_anio <- datos_anuales_filtrado[[col_anio]][1]
      ultimo_anio <- datos_anuales_filtrado[[col_anio]][nrow(datos_anuales_filtrado)]
      
      # Valores de padrón: primero y último
      padron_primero <- safe_val(datos_anuales_filtrado, 1, cols$padron)
      padron_ultimo  <- safe_val(datos_anuales_filtrado, nrow(datos_anuales_filtrado), cols$padron)
      
      # Valores de lista: primero y último
      lista_primero <- safe_val(datos_anuales_filtrado, 1, cols$lista)
      lista_ultimo  <- safe_val(datos_anuales_filtrado, nrow(datos_anuales_filtrado), cols$lista)
      
      # Calcular variaciones
      var_padron <- calcular_variacion(padron_ultimo, padron_primero)
      var_lista  <- calcular_variacion(lista_ultimo, lista_primero)
      
      # Sexo predominante (del último año)
      hombres_ultimo <- safe_val(datos_anuales_filtrado, nrow(datos_anuales_filtrado), cols$padron_h)
      mujeres_ultimo <- safe_val(datos_anuales_filtrado, nrow(datos_anuales_filtrado), cols$padron_m)
      sexo_pred <- sexo_predominante(hombres_ultimo, mujeres_ultimo)
      
      # Construir texto según ámbito
      if (ambito == "extranjero") {
        texto_evolucion <- paste0(
          "Entre ", primer_anio, " y ", ultimo_anio,
          " el Padr\u00f3n Electoral de Residentes en el Extranjero ha ",
          var_padron$texto, " ",
          "<b style='", css_variacion(var_padron$pct), "'>", fmt_pct(var_padron$pct), "</b>.",
          " En tanto que la Lista Nominal de Residentes en el Extranjero ha ",
          var_lista$texto, " ",
          "<b style='", css_variacion(var_lista$pct), "'>", fmt_pct(var_lista$pct), "</b>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<b>", sexo_pred, "</b> en el Padr\u00f3n y en la Lista Nominal de Residentes en el Extranjero."
        )
      } else {
        texto_evolucion <- paste0(
          "Entre ", primer_anio, " y ", ultimo_anio,
          " el padr\u00f3n electoral ha ",
          var_padron$texto, " ",
          "<b style='", css_variacion(var_padron$pct), "'>", fmt_pct(var_padron$pct), "</b>.",
          " En tanto que la LNE ha ",
          var_lista$texto, " ",
          "<b style='", css_variacion(var_lista$pct), "'>", fmt_pct(var_lista$pct), "</b>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<b>", sexo_pred, "</b> en el Padr\u00f3n y en la Lista Nominal Electoral."
        )
      }
      
      HTML(paste0(
        "<div style='", css_contenedor, "'>",
        "<h4 style='", css_titulo_h4, "'>b) Evoluci\u00f3n temporal</h4>",
        "<p style='", css_parrafo, "'>", texto_evolucion, "</p>",
        "<p style='", css_parrafo, "'>", texto_sexo, "</p>",
        "</div>"
      ))
      
    } else {
      
      # ============================================================
      # RAMA II: Año consultado != año actual (gráficas 4, 5)
      # → Evolución intra-año (enero → diciembre del año consultado)
      # ============================================================
      
      datos_consulta <- tryCatch(datos_year_consulta(), error = function(e) NULL)
      
      if (is.null(datos_consulta) || nrow(datos_consulta) < 2) {
        return(HTML(paste0(
          "<div style='", css_contenedor, "'>",
          "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
          "<p style='", css_alerta, "'>Sin datos suficientes para el a\u00f1o ", anio_consul, ".</p>",
          "</div>"
        )))
      }
      
      # Para extranjero, filtrar filas con datos válidos
      if (ambito == "extranjero") {
        datos_consulta_filtrado <- datos_consulta[!is.na(datos_consulta[[cols$padron]]) & datos_consulta[[cols$padron]] > 0, ]
        if (nrow(datos_consulta_filtrado) < 2) {
          return(HTML(paste0(
            "<div style='", css_contenedor, "'>",
            "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
            "<p style='", css_alerta, "'>Sin datos de residentes en el extranjero para el a\u00f1o ", anio_consul, ".</p>",
            "</div>"
          )))
        }
      } else {
        datos_consulta_filtrado <- datos_consulta
      }
      
      # Primer y último registro del año consultado
      padron_primero <- safe_val(datos_consulta_filtrado, 1, cols$padron)
      padron_ultimo  <- safe_val(datos_consulta_filtrado, nrow(datos_consulta_filtrado), cols$padron)
      
      lista_primero <- safe_val(datos_consulta_filtrado, 1, cols$lista)
      lista_ultimo  <- safe_val(datos_consulta_filtrado, nrow(datos_consulta_filtrado), cols$lista)
      
      # Calcular variaciones intra-año
      var_padron <- calcular_variacion(padron_ultimo, padron_primero)
      var_lista  <- calcular_variacion(lista_ultimo, lista_primero)
      
      # Verbo en pasado para años anteriores
      verbo_padron <- if (!is.na(var_padron$pct) && var_padron$pct >= 0) "creci\u00f3" else "disminuy\u00f3"
      verbo_lista  <- if (!is.na(var_lista$pct) && var_lista$pct >= 0) "creci\u00f3" else "disminuy\u00f3"
      
      # Sexo predominante (del último registro del año)
      hombres_ultimo <- safe_val(datos_consulta_filtrado, nrow(datos_consulta_filtrado), cols$padron_h)
      mujeres_ultimo <- safe_val(datos_consulta_filtrado, nrow(datos_consulta_filtrado), cols$padron_m)
      sexo_pred <- sexo_predominante(hombres_ultimo, mujeres_ultimo)
      
      # Construir texto según ámbito
      if (ambito == "extranjero") {
        texto_evolucion <- paste0(
          "Entre enero y diciembre de ", anio_consul,
          " el padr\u00f3n electoral de residentes en el extranjero ",
          verbo_padron, " ",
          "<b style='", css_variacion(var_padron$pct), "'>", fmt_pct(abs(var_padron$pct)), "</b>.",
          " En tanto que la LNE ",
          verbo_lista, " ",
          "<b style='", css_variacion(var_lista$pct), "'>", fmt_pct(abs(var_lista$pct)), "</b>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<b>", sexo_pred, "</b> en el Padr\u00f3n y en la Lista Nominal Electoral ",
          "de residentes en el extranjero."
        )
      } else {
        texto_evolucion <- paste0(
          "Entre enero y diciembre de ", anio_consul,
          " el padr\u00f3n electoral ",
          verbo_padron, " ",
          "<b style='", css_variacion(var_padron$pct), "'>", fmt_pct(abs(var_padron$pct)), "</b>.",
          " En tanto que la LNE ",
          verbo_lista, " ",
          "<b style='", css_variacion(var_lista$pct), "'>", fmt_pct(abs(var_lista$pct)), "</b>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<b>", sexo_pred, "</b> en el Padr\u00f3n y en la Lista Nominal Electoral."
        )
      }
      
      HTML(paste0(
        "<div style='", css_contenedor, "'>",
        "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
        "<p style='", css_parrafo, "'>", texto_evolucion, "</p>",
        "<p style='", css_parrafo, "'>", texto_sexo, "</p>",
        "</div>"
      ))
    }
    
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ============================================================
  # OUTPUT 4: DEMOGRAFÍA (reutilizado del sidebar, puede dejarse NULL o expandirse)
  # ============================================================
  
  output$`text_analysis-demografia_lista` <- renderUI({
    # Este output existe en la UI pero según la especificación del PDF,
    # la información demográfica se integra en la sección de Evolución Temporal
    # (sexo predominante). Si en el futuro se desea expandir, hacerlo aquí.
    return(NULL)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  message("✅ M\u00f3dulo lista_nominal_server_text_analysis v3.0 inicializado")
  message("   ✅ 3 outputs principales: t\u00edtulo, resumen, evoluci\u00f3n temporal")
  message("   ✅ Soporta \u00e1mbito nacional y extranjero")
  message("   ✅ Rama I (a\u00f1o actual) y Rama II (a\u00f1o consultado)")
}
