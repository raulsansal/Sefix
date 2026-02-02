# modules/lista_nominal_server_text_analysis.R
# Versión: 3.7 - Advertencia nacional + reestructuración encabezado
# Cambios vs v3.6:
#   - Nueva advertencia para vista Nacional con distrito "RESIDENTES EN EL EXTRANJERO"
#   - Encabezado OUTPUT 1: label "Alcance del análisis:" alineado a la izquierda
#   - Subtítulo de alcance ahora alineado a la izquierda (era centrado)
#   - Interlineado 16px entre año destacado y label de alcance
# Consume: datos_year_actual, datos_anuales_completos, datos_year_consulta,
#          filtros_usuario, estado_app, anio_actual, anio_consultado,
#          ambito_reactivo, texto_alcance
# Genera:  3 bloques uiOutput en el sidebar derecho
# Fecha:   01 de febrero de 2026

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
  
  message("\U0001f4dd Inicializando lista_nominal_server_text_analysis v3.7")
  
  # ============================================================
  # HELPERS INTERNOS
  # ============================================================
  
  # Formatear números grandes con separador de miles (coma)
  fmt_num <- function(x) {
    if (is.null(x) || is.na(x)) return("N/D")
    format(round(as.numeric(x)), big.mark = ",", scientific = FALSE)
  }
  
  # Formatear porcentajes SIEMPRE con 2 decimales (ej: 99.00%, 12.50%)
  fmt_pct <- function(x) {
    if (is.null(x) || is.na(x)) return("N/D")
    paste0(sprintf("%.2f", round(as.numeric(x), 2)), "%")
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
  
  # Detectar si vista extranjero necesita advertencia
  # Retorna: FALSE (sin advertencia) o TRUE (necesita seleccionar distrito RE)
  # En vista Extranjero, cualquier distrito que NO sea "RESIDENTES EN EL EXTRANJERO"
  # requiere que el usuario reconfigure su consulta
  necesita_advertencia_extranjero <- function(ambito, filtros) {
    if (ambito != "extranjero") return(FALSE)
    # Si es Nacional (nivel país), no necesita advertencia
    if (filtros$entidad == "Nacional") return(FALSE)
    # Si hay estado seleccionado: el distrito DEBE ser RE para ver datos
    if (filtros$distrito == "Todos" || !es_distrito_extranjero(filtros$distrito)) return(TRUE)
    return(FALSE)
  }
  
  # Texto de advertencia para vista extranjero
  texto_advertencia_ext <- paste0(
    "<strong>\u26A0\uFE0F</strong> Selecciona \"RESIDENTES EXTRANJERO\" ",
    "en el campo \"Distrito Electoral\" y luego da clic en el bot\u00f3n \"Consultar\". ",
    "Estos datos est\u00e1n disponibles a nivel estatal."
  )
  
  # Detectar si vista nacional necesita advertencia
  # Retorna: FALSE (sin advertencia) o TRUE (tiene distrito RE en vista nacional)
  # En vista Nacional, si el distrito es "RESIDENTES EN EL EXTRANJERO"
  # los datos nacionales no aplican para ese distrito especial
  necesita_advertencia_nacional <- function(ambito, filtros) {
    if (ambito != "nacional") return(FALSE)
    # Solo aplica si tiene un estado seleccionado con distrito RE
    if (filtros$entidad == "Nacional") return(FALSE)
    if (es_distrito_extranjero(filtros$distrito)) return(TRUE)
    return(FALSE)
  }
  
  # Texto de advertencia para vista nacional con distrito RE
  texto_advertencia_nac <- paste0(
    "<strong>\u26A0\uFE0F</strong> Si deseas realizar una consulta del \u00e1mbito ",
    "de datos \"Nacional\", selecciona un Distrito Electoral distinto a ",
    "\"RESIDENTES EXTRANJERO\" y luego presiona el bot\u00f3n \"Consultar\"."
  )
  
  # ============================================================
  # ESTILOS INLINE v3.2 - Paleta azul marca, tipografía consistente
  # ============================================================
  
  # Contenedor base (+2px: 14→16)
  css_contenedor <- "font-size: 16px; line-height: 1.6; color: #333;"
  
  # Título principal <h3>: centrado (+2px: 16→18)
  css_titulo_h3 <- "text-align: center; margin: 0 0 2px 0; font-size: 18px; color: #2c3e50; font-weight: 600; line-height: 1.4;"
  
  # Año destacado: línea separada, centrado, con interlineado inferior (+2px: 18→20)
  css_year_highlight <- "display: block; text-align: center; color: #1a5276; font-weight: 700; font-size: 20px; margin-top: 4px; margin-bottom: 10px;"
  
  # Label "Alcance del análisis:" - alineado a la izquierda
  css_alcance_label <- "margin: 16px 0 2px 0; font-size: 14px; color: #555; font-weight: 600; text-align: left;"
  
  # Subtítulo de alcance - alineado a la izquierda (+2px: 12→14)
  css_alcance <- "text-align: left; margin: 0 0 0 0; font-size: 14px; color: #777; line-height: 1.4;"
  
  # Encabezados de sección <h4> (+2px: 15→17, margin-top 12→22 para separar del alcance)
  css_titulo_h4 <- "margin: 22px 0 6px 0; font-size: 17px; color: #2c3e50; font-weight: 600; border-bottom: 1px solid #e0e0e0; padding-bottom: 4px;"
  
  # Párrafos de contenido (+2px: 13→15)
  css_parrafo <- "margin: 0 0 8px 0; font-size: 15px; line-height: 1.6; color: #333;"
  
  # Alertas (+2px: 13→15)
  css_alerta <- "text-align: center; color: #999; padding: 15px; font-style: italic; font-size: 15px;"
  
  # Advertencia extranjero (+2px: 12→14)
  css_advertencia <- "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin: 8px 0; font-size: 14px; color: #856404;"
  
  # ============================================================
  # OUTPUT 1: TÍTULO Y ALCANCE DEL ANÁLISIS
  # Diseño: <h3> centrado con año destacado + label + subtítulo de alcance alineado left
  # ============================================================
  
  output$`text_analysis-titulo_lista` <- renderUI({
    
    estado_actual <- estado_app()
    
    if (estado_actual == "inicial") {
      return(NULL)
    }
    
    message("\U0001f4dd [T\u00cdTULO] Renderizando en estado: ", estado_actual)
    
    # Obtener año consultado
    anio_consul <- tryCatch(anio_consultado(), error = function(e) anio_actual())
    
    # Obtener ámbito
    ambito <- tryCatch(ambito_reactivo(), error = function(e) "nacional")
    cols <- cols_ambito(ambito)
    
    # Texto de vista según ámbito
    texto_vista <- paste0("Padr\u00f3n y Lista Nominal Electoral ", cols$etiqueta)
    
    # Texto de alcance (de texto_alcance reactive)
    alcance <- tryCatch(texto_alcance(), error = function(e) "")
    alcance_formateado <- gsub(" - ", " \u2013 ", alcance)
    
    HTML(paste0(
      "<div style='", css_contenedor, "'>",
      "<h3 style='", css_titulo_h3, "'>",
      "An\u00e1lisis de ", texto_vista,
      " <span style='", css_year_highlight, "'>", anio_consul, "</span>",
      "</h3>",
      "<p style='", css_alcance_label, "'>Alcance del an\u00e1lisis:</p>",
      "<p style='", css_alcance, "'>", alcance_formateado, "</p>",
      "</div>"
    ))
    
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ============================================================
  # OUTPUT 2: RESUMEN GENERAL (SOLO año == anio_actual)
  # Diseño: <h4> sin "a)", datos variables en <strong>
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
      message("\U0001f4dd [RESUMEN] Renderizando en estado CONSULTADO - Bot\u00f3n: ", input$btn_consultar)
    } else {
      message("\U0001f4dd [RESUMEN] Renderizando en estado RESTABLECIDO")
    }
    
    # --- Obtener año consultado y año actual ---
    anio_consul <- tryCatch(anio_consultado(), error = function(e) NULL)
    anio_act <- tryCatch(anio_actual(), error = function(e) NULL)
    
    if (is.null(anio_consul) || is.null(anio_act)) {
      return(HTML(paste0("<p style='", css_alerta, "'>Cargando datos...</p>")))
    }
    
    # --- Si el año consultado NO es el año actual: NO mostrar resumen general ---
    if (anio_consul != anio_act) {
      message("\U0001f4dd [RESUMEN] A\u00f1o consultado (", anio_consul, ") != a\u00f1o actual (", anio_act, ") - Sin resumen general")
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
        "<h4 style='", css_titulo_h4, "'>Resumen general</h4>",
        "<div style='", css_advertencia, "'>",
        texto_advertencia_ext,
        "</div>",
        "</div>"
      )))
    }
    
    # --- Verificar advertencia para vista nacional con distrito RE ---
    if (necesita_advertencia_nacional(ambito, filtros)) {
      return(HTML(paste0(
        "<div style='", css_contenedor, "'>",
        "<h4 style='", css_titulo_h4, "'>Resumen general</h4>",
        "<div style='", css_advertencia, "'>",
        texto_advertencia_nac,
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
    
    # Construir texto según ámbito — datos variables en <strong>
    if (ambito == "extranjero") {
      texto_padron <- paste0(
        "Al mes de <strong>", mes_texto, " de ", anio_texto, "</strong>, ",
        "el Padr\u00f3n Electoral de Residentes en el Extranjero totaliza ",
        "<strong>", fmt_num(padron_val), "</strong> ciudadanos, ",
        "de los cuales, ",
        "<strong>", fmt_num(lista_val), "</strong> ",
        "est\u00e1n incluidos en la Lista Nominal de Residentes en el Extranjero, ",
        "lo que representa una tasa de inclusi\u00f3n de ",
        "<strong>", fmt_pct(tasa_inclusion), "</strong>."
      )
    } else {
      texto_padron <- paste0(
        "Al mes de <strong>", mes_texto, " de ", anio_texto, "</strong>, ",
        "el Padr\u00f3n Electoral totaliza ",
        "<strong>", fmt_num(padron_val), "</strong> ciudadanos, ",
        "de los cuales, ",
        "<strong>", fmt_num(lista_val), "</strong> ",
        "est\u00e1n incluidos en la Lista Nominal Electoral, ",
        "lo que representa una tasa de inclusi\u00f3n de ",
        "<strong>", fmt_pct(tasa_inclusion), "</strong>."
      )
    }
    
    HTML(paste0(
      "<div style='", css_contenedor, "'>",
      "<h4 style='", css_titulo_h4, "'>Resumen general</h4>",
      "<p style='", css_parrafo, "'>", texto_padron, "</p>",
      "</div>"
    ))
    
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ============================================================
  # OUTPUT 3: EVOLUCIÓN TEMPORAL
  # Diseño: <h4> sin "b)", datos variables en <strong>
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
      message("\U0001f4dd [EVOLUCI\u00d3N] Renderizando en estado CONSULTADO")
    } else {
      message("\U0001f4dd [EVOLUCI\u00d3N] Renderizando en estado RESTABLECIDO")
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
        "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
        "<div style='", css_advertencia, "'>",
        texto_advertencia_ext,
        "</div>",
        "</div>"
      )))
    }
    
    # --- Verificar advertencia para vista nacional con distrito RE ---
    if (necesita_advertencia_nacional(ambito, filtros)) {
      return(HTML(paste0(
        "<div style='", css_contenedor, "'>",
        "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
        "<div style='", css_advertencia, "'>",
        texto_advertencia_nac,
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
          "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
          "<p style='", css_alerta, "'>Sin datos anuales disponibles.</p>",
          "</div>"
        )))
      }
      
      # Para extranjero, los datos empiezan en 2020 (años previos tienen valor 0)
      # Filtramos filas con datos reales > 0 para que el texto refleje el rango correcto
      if (ambito == "extranjero") {
        datos_anuales_filtrado <- datos_anuales[!is.na(datos_anuales[[cols$padron]]) & datos_anuales[[cols$padron]] > 0, ]
        if (nrow(datos_anuales_filtrado) == 0) {
          return(HTML(paste0(
            "<div style='", css_contenedor, "'>",
            "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
            "<p style='", css_alerta, "'>Sin datos de residentes en el extranjero disponibles.</p>",
            "</div>"
          )))
        }
      } else {
        # Nacional: también filtrar años con datos > 0 por robustez
        datos_anuales_filtrado <- datos_anuales[!is.na(datos_anuales[[cols$padron]]) & datos_anuales[[cols$padron]] > 0, ]
        if (nrow(datos_anuales_filtrado) == 0) {
          return(HTML(paste0(
            "<div style='", css_contenedor, "'>",
            "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
            "<p style='", css_alerta, "'>Sin datos anuales disponibles.</p>",
            "</div>"
          )))
        }
      }
      
      # Primer y último año disponible
      # Usar [[ ]] para evitar problemas de encoding con $
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
      
      # Construir texto según ámbito — datos variables en <strong>
      if (ambito == "extranjero") {
        texto_evolucion <- paste0(
          "Entre <strong>", primer_anio, "</strong> y <strong>", ultimo_anio, "</strong>",
          " el Padr\u00f3n Electoral de Residentes en el Extranjero ha ",
          var_padron$texto, " ",
          "<strong>", fmt_pct(var_padron$pct), "</strong>.",
          " En tanto que la Lista Nominal de Residentes en el Extranjero ha ",
          var_lista$texto, " ",
          "<strong>", fmt_pct(var_lista$pct), "</strong>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<strong>", sexo_pred, "</strong> en el Padr\u00f3n y en la Lista Nominal de Residentes en el Extranjero."
        )
      } else {
        texto_evolucion <- paste0(
          "Entre <strong>", primer_anio, "</strong> y <strong>", ultimo_anio, "</strong>",
          " el Padr\u00f3n Electoral ha ",
          var_padron$texto, " ",
          "<strong>", fmt_pct(var_padron$pct), "</strong>.",
          " En tanto que la LNE ha ",
          var_lista$texto, " ",
          "<strong>", fmt_pct(var_lista$pct), "</strong>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<strong>", sexo_pred, "</strong> en el Padr\u00f3n y en la Lista Nominal Electoral."
        )
      }
      
      HTML(paste0(
        "<div style='", css_contenedor, "'>",
        "<h4 style='", css_titulo_h4, "'>Evoluci\u00f3n temporal</h4>",
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
      
      # Construir texto según ámbito — datos variables en <strong>
      if (ambito == "extranjero") {
        texto_evolucion <- paste0(
          "Entre <strong>enero</strong> y <strong>diciembre de ", anio_consul, "</strong>",
          " el Padr\u00f3n Electoral de Residentes en el Extranjero ",
          verbo_padron, " ",
          "<strong>", fmt_pct(abs(var_padron$pct)), "</strong>.",
          " En tanto que la LNE ",
          verbo_lista, " ",
          "<strong>", fmt_pct(abs(var_lista$pct)), "</strong>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<strong>", sexo_pred, "</strong> en el Padr\u00f3n y en la Lista Nominal Electoral ",
          "de residentes en el extranjero."
        )
      } else {
        texto_evolucion <- paste0(
          "Entre <strong>enero</strong> y <strong>diciembre de ", anio_consul, "</strong>",
          " el Padr\u00f3n Electoral ",
          verbo_padron, " ",
          "<strong>", fmt_pct(abs(var_padron$pct)), "</strong>.",
          " En tanto que la LNE ",
          verbo_lista, " ",
          "<strong>", fmt_pct(abs(var_lista$pct)), "</strong>."
        )
        texto_sexo <- paste0(
          "La distribuci\u00f3n por sexo muestra una mayor presencia constante de ",
          "<strong>", sexo_pred, "</strong> en el Padr\u00f3n y en la Lista Nominal Electoral."
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
  # OUTPUT 4: DEMOGRAFÍA (reservado para expansión futura)
  # ============================================================
  
  output$`text_analysis-demografia_lista` <- renderUI({
    return(NULL)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, input$ambito_datos, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  message("\u2705 M\u00f3dulo lista_nominal_server_text_analysis v3.7 inicializado")
  message("   \u2705 3 outputs principales: t\u00edtulo, resumen, evoluci\u00f3n temporal")
  message("   \u2705 Soporta \u00e1mbito nacional y extranjero")
  message("   \u2705 Rama I (a\u00f1o actual) y Rama II (a\u00f1o consultado)")
  message("   \u2705 Advertencias: extranjero (distrito no-RE) + nacional (distrito RE)")
  message("   \u2705 Encabezado: t\u00edtulo centrado + 'Alcance del an\u00e1lisis:' alineado left")
}
