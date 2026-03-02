# modules/lista_nominal_graficas/graficas_semanal.R
# Vista Semanal: gráficas + dataTables + análisis textual sidebar derecho
# Versión: 2.3 — Fase 2: sidebar derecho condicional por desglose
#
# CAMBIOS vs v2.2:
#   - Análisis textual del sidebar derecho (semanal_texto_titulo,
#     semanal_texto_analisis) ahora muestra SOLO el bloque correspondiente
#     al desglose activo (edad / sexo / origen)
#   - Cada bloque de análisis tiene su propia función auxiliar para
#     mantener el código organizado y facilitar ajustes futuros
#   - Gráficas y DataTables sin cambios (ya estaban correctos en v2.2)

graficas_semanal <- function(input, output, session,
                             datos_semanal_edad,
                             datos_semanal_sexo,
                             datos_semanal_origen,
                             anio_semanal,
                             fecha_semanal_efectiva,
                             texto_alcance,
                             ambito_reactivo,
                             estado_app) {
  
  message("📊 Inicializando graficas_semanal v2.3")
  
  # ════════════════════════════════════════════════════════════════════════════
  # CONSTANTES
  # ════════════════════════════════════════════════════════════════════════════
  
  COLORES <- list(
    nac_padron  = "#003E66", nac_lista   = "#AE0E35",
    nac_hombres = "#44559B", nac_mujeres = "#C0311A",
    ext_padron  = "#EAC43E", ext_lista   = "#B3D491",
    ext_hombres = "#D4A500", ext_mujeres = "#8FB369"
  )
  FUENTE_INE <- "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado"
  ORDEN_EDAD <- c("18","19","20_24","25_29","30_34","35_39",
                  "40_44","45_49","50_54","55_59","60_64","65_y_mas")
  
  # ════════════════════════════════════════════════════════════════════════════
  # HELPERS
  # ════════════════════════════════════════════════════════════════════════════
  
  fmt_num <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE)
  fmt_pct <- function(x) paste0(sprintf("%.2f", round(as.numeric(x), 2)), "%")
  
  color_padron <- function(a) if (a == "extranjero") COLORES$ext_padron  else COLORES$nac_padron
  color_lista  <- function(a) if (a == "extranjero") COLORES$ext_lista   else COLORES$nac_lista
  color_h      <- function(a) if (a == "extranjero") COLORES$ext_hombres else COLORES$nac_hombres
  color_m      <- function(a) if (a == "extranjero") COLORES$ext_mujeres else COLORES$nac_mujeres
  etiq_ambito  <- function(a) if (a == "extranjero") "Extranjero" else "Nacional"
  
  ann_fuente <- function() list(
    text = FUENTE_INE, x = 0.5, y = -0.18, xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "top", showarrow = FALSE,
    font = list(size = 10, color = "#666666", family = "Arial, sans-serif"), align = "center"
  )
  ann_alcance <- function(texto, y_pos = 1.10) list(
    text = texto, x = 0.5, y = y_pos, xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "top", showarrow = FALSE,
    font = list(size = 13, color = "#555555", family = "Arial, sans-serif"), align = "center"
  )
  plot_vacio <- function(msg = "No hay datos disponibles") {
    plot_ly() %>% layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      annotations = list(list(text = msg, xref = "paper", yref = "paper",
                              x = 0.5, y = 0.5, xanchor = "center", yanchor = "middle",
                              showarrow = FALSE, font = list(size = 14, color = "#888")))
    )
  }
  etiqueta_edad <- function(g) gsub("_", "-", gsub("_y_mas", "+", g))
  
  es_historico <- function() { tc <- input$tipo_corte %||% "historico"; tc != "semanal" }
  desglose_activo <- function() input$desglose %||% "edad"
  
  fila_totales_nac <- function(df) {
    if (is.null(df) || !"nombre_entidad" %in% colnames(df)) return(NULL)
    idx <- which(toupper(trimws(df$nombre_entidad)) == "TOTALES")
    if (length(idx) > 0) df[idx[1], ] else NULL
  }
  fila_ext <- function(df) {
    if (is.null(df) || !"nombre_entidad" %in% colnames(df)) return(NULL)
    idx <- which(grepl("RESIDENTES EXTRANJERO", toupper(df$nombre_entidad)))
    if (length(idx) > 0) df[idx[1], ] else NULL
  }
  df_nacional <- function(df) {
    if (is.null(df)) return(NULL)
    df[!grepl("RESIDENTES EXTRANJERO|TOTALES", toupper(df$nombre_entidad %||% ""), ignore.case = TRUE), ]
  }
  
  sumar_rango <- function(df, grupo, sexo, tipo) {
    col <- grep(paste0("^", tipo, "_", grupo, "_", sexo, "$"), colnames(df), value = TRUE, ignore.case = TRUE)
    if (length(col) == 0) return(0L)
    sum(as.numeric(df[[col[1]]]), na.rm = TRUE)
  }
  construir_df_edad <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df_uso <- if (ambito == "extranjero") { f <- fila_ext(df); if (is.null(f)) return(NULL); f } else df_nacional(df)
    if (is.null(df_uso) || nrow(df_uso) == 0) return(NULL)
    do.call(rbind, lapply(ORDEN_EDAD, function(g) data.frame(
      grupo          = etiqueta_edad(g),
      padron_hombres = sumar_rango(df_uso, g, "hombres", "padron"),
      padron_mujeres = sumar_rango(df_uso, g, "mujeres", "padron"),
      lista_hombres  = sumar_rango(df_uso, g, "hombres", "lista"),
      lista_mujeres  = sumar_rango(df_uso, g, "mujeres", "lista"),
      stringsAsFactors = FALSE
    )))
  }
  
  extraer_totales_sexo <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    fila <- if (ambito == "extranjero") fila_ext(df) else fila_totales_nac(df)
    if (is.null(fila)) {
      df_nac <- df_nacional(df)
      if (is.null(df_nac) || nrow(df_nac) == 0) return(NULL)
      cols_num <- sapply(df_nac, is.numeric)
      fila <- as.data.frame(lapply(df_nac[, cols_num, drop = FALSE], function(x) sum(x, na.rm = TRUE)))
    }
    cols_sex <- c("padron_hombres","padron_mujeres","padron_no_binario",
                  "lista_hombres","lista_mujeres","lista_no_binario")
    res <- list()
    for (col in cols_sex) {
      res[[col]] <- if (col %in% colnames(fila)) { val <- as.numeric(fila[[col]][1]); if (is.na(val)) 0 else val } else 0
    }
    as.data.frame(res, stringsAsFactors = FALSE)
  }
  
  NOM_ORIGEN <- c(
    "01"="AGUASCALIENTES","02"="BAJA CALIFORNIA","03"="BAJA CALIFORNIA SUR",
    "04"="CAMPECHE","05"="COAHUILA","06"="COLIMA","07"="CHIAPAS","08"="CHIHUAHUA",
    "09"="CIUDAD DE MEXICO","10"="DURANGO","11"="GUANAJUATO","12"="GUERRERO",
    "13"="HIDALGO","14"="JALISCO","15"="MEXICO","16"="MICHOACAN","17"="MORELOS",
    "18"="NAYARIT","19"="NUEVO LEON","20"="OAXACA","21"="PUEBLA","22"="QUERETARO",
    "23"="QUINTANA ROO","24"="SAN LUIS POTOSI","25"="SINALOA","26"="SONORA",
    "27"="TABASCO","28"="TAMAULIPAS","29"="TLAXCALA","30"="VERACRUZ",
    "31"="YUCATAN","32"="ZACATECAS",
    "87"="Mexicanos nacidos en el extranjero","88"="Ciudadanos naturalizados"
  )
  construir_tabla_origen <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df_uso <- if (ambito == "extranjero")
      df[grepl("RESIDENTES EXTRANJERO", toupper(df$nombre_entidad %||% ""), ignore.case = TRUE), ]
    else df_nacional(df)
    if (is.null(df_uso) || nrow(df_uso) == 0) return(NULL)
    cols_pad <- grep("^pad_\\d{2}$|^pad8[78]$", colnames(df_uso), value = TRUE, ignore.case = TRUE)
    if (length(cols_pad) == 0)
      cols_pad <- grep("^padron_[0-9]", colnames(df_uso), value = TRUE, ignore.case = TRUE)
    if (length(cols_pad) == 0) { message("⚠️ [origen] Sin columnas de estados"); return(NULL) }
    res <- do.call(rbind, lapply(cols_pad, function(cp) {
      clave  <- sprintf("%02s", trimws(gsub("pad_|pad|padron_", "", cp, ignore.case = TRUE)))
      nombre <- NOM_ORIGEN[clave] %||% paste0("Entidad ", clave)
      cl     <- gsub("pad", "ln", cp, ignore.case = TRUE)
      data.frame(
        entidad_origen = nombre,
        padron         = sum(as.numeric(df_uso[[cp]]), na.rm = TRUE),
        lista_nominal  = if (cl %in% colnames(df_uso)) sum(as.numeric(df_uso[[cl]]), na.rm = TRUE) else NA,
        stringsAsFactors = FALSE
      )
    }))
    res[order(res$lista_nominal, decreasing = TRUE, na.last = TRUE), ]
  }
  
  texto_subtitulo <- function() {
    if (estado_app() == "restablecido") return("Vista: Nacional — sin filtros aplicados")
    gsub(" - ", " – ", isolate(texto_alcance()))
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # UI: TÍTULO Y SUBTÍTULOS
  # ════════════════════════════════════════════════════════════════════════════
  
  output$semanal_titulo_principal <- renderUI({
    if (es_historico()) return(NULL)
    div(style = "text-align:center;margin-bottom:10px;",
        h3(paste0(anio_semanal(), " - Padrón y Lista Nominal Electoral - ", etiq_ambito(ambito_reactivo())),
           style = "color:#2c3e50;font-family:Arial,sans-serif;font-weight:700;"))
  })
  output$semanal_subtitulo_edad <- renderUI({
    if (es_historico()) return(NULL)
    p(class = "text-muted", style = "font-size:13px;text-align:center;margin-bottom:6px;", texto_subtitulo())
  })
  output$semanal_subtitulo_sexo <- renderUI({
    if (es_historico()) return(NULL)
    p(class = "text-muted", style = "font-size:13px;text-align:center;margin-bottom:6px;", texto_subtitulo())
  })
  output$semanal_subtitulo_origen <- renderUI({
    if (es_historico()) return(NULL)
    p(class = "text-muted", style = "font-size:13px;text-align:center;margin-bottom:6px;", texto_subtitulo())
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # GRÁFICAS — sin cambios vs v2.2
  # ════════════════════════════════════════════════════════════════════════════
  
  output$semanal_edad_piramide <- renderPlotly({
    if (es_historico()) return(NULL)
    message("📊 [semanal_edad_piramide] Renderizando...")
    ambito <- ambito_reactivo(); alcance <- isolate(texto_alcance()); anio <- anio_semanal()
    datos  <- datos_semanal_edad()
    if (is.null(datos)) { message("⚠️ [piramide] datos NULL"); return(plot_vacio()) }
    df <- construir_df_edad(datos, ambito)
    if (is.null(df) || nrow(df) == 0) return(plot_vacio("Sin datos de edad"))
    message("✅ [semanal_edad_piramide] OK")
    niveles_y <- sapply(rev(ORDEN_EDAD), etiqueta_edad)
    plot_ly() %>%
      add_trace(data = df, x = ~(-lista_hombres), y = ~factor(grupo, levels = niveles_y),
                type = "bar", orientation = "h", name = "LNE Hombres",
                marker = list(color = color_h(ambito)), customdata = ~lista_hombres,
                hovertemplate = "<b>%{y}</b><br>LNE Hombres: %{customdata:,.0f}<extra></extra>") %>%
      add_trace(data = df, x = ~lista_mujeres, y = ~factor(grupo, levels = niveles_y),
                type = "bar", orientation = "h", name = "LNE Mujeres",
                marker = list(color = color_m(ambito)),
                hovertemplate = "<b>%{y}</b><br>LNE Mujeres: %{x:,.0f}<extra></extra>") %>%
      layout(
        title   = list(text = paste0("Pirámide de Edad - LNE (", etiq_ambito(ambito), ") ", anio),
                       font = list(size = 17, color = "#333", family = "Arial, sans-serif"), x = 0.5, xanchor = "center"),
        barmode = "overlay", bargap = 0.1,
        xaxis   = list(title = "Número de electores", tickformat = ",.0f", zeroline = TRUE, zerolinecolor = "#999", zerolinewidth = 1.5),
        yaxis   = list(title = "", categoryorder = "array", categoryarray = niveles_y),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
        margin  = list(t = 110, b = 80, l = 75, r = 40), hovermode = "y unified",
        annotations = list(ann_alcance(alcance), ann_fuente())
      )
  })
  
  output$semanal_edad_distribucion <- renderPlotly({
    if (es_historico()) return(NULL)
    message("📊 [semanal_edad_distribucion] Renderizando...")
    ambito <- ambito_reactivo(); alcance <- isolate(texto_alcance()); anio <- anio_semanal()
    datos  <- datos_semanal_edad()
    if (is.null(datos)) return(plot_vacio())
    df <- construir_df_edad(datos, ambito)
    if (is.null(df) || nrow(df) == 0) return(plot_vacio("Sin datos de edad"))
    df$padron_total <- df$padron_hombres + df$padron_mujeres
    df$lista_total  <- df$lista_hombres  + df$lista_mujeres
    plot_ly() %>%
      add_trace(data = df, x = ~grupo, y = ~padron_total, type = "bar",
                name = "Padrón Electoral", marker = list(color = color_padron(ambito)),
                hovertemplate = "<b>%{x}</b><br>Padrón: %{y:,.0f}<extra></extra>") %>%
      add_trace(data = df, x = ~grupo, y = ~lista_total, type = "bar",
                name = "Lista Nominal", marker = list(color = color_lista(ambito)),
                hovertemplate = "<b>%{x}</b><br>LNE: %{y:,.0f}<extra></extra>") %>%
      layout(
        title   = list(text = paste0("Padrón y LNE por Rango de Edad (", etiq_ambito(ambito), ") ", anio),
                       font = list(size = 17, color = "#333", family = "Arial, sans-serif"), x = 0.5, xanchor = "center"),
        barmode = "group",
        xaxis   = list(title = "Rango de Edad", tickangle = -30),
        yaxis   = list(title = "Número de electores", separatethousands = TRUE),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.18),
        margin  = list(t = 110, b = 100, l = 90, r = 40),
        annotations = list(ann_alcance(alcance), ann_fuente())
      )
  })
  
  output$semanal_sexo_barras <- renderPlotly({
    if (es_historico()) return(NULL)
    message("📊 [semanal_sexo_barras] Renderizando...")
    ambito <- ambito_reactivo(); alcance <- isolate(texto_alcance()); anio <- anio_semanal()
    datos  <- datos_semanal_sexo()
    if (is.null(datos)) return(plot_vacio())
    tot <- extraer_totales_sexo(datos, ambito)
    if (is.null(tot)) return(plot_vacio("Sin datos de sexo"))
    df_g <- data.frame(
      Sexo     = rep(c("Hombres","Mujeres"), 2),
      Tipo     = rep(c("Padrón Electoral","Lista Nominal"), each = 2),
      Cantidad = c(tot$padron_hombres, tot$padron_mujeres, tot$lista_hombres, tot$lista_mujeres),
      stringsAsFactors = FALSE
    )
    p <- plot_ly(data = df_g, x = ~Sexo, y = ~Cantidad, color = ~Tipo,
                 colors = c("Padrón Electoral" = color_padron(ambito), "Lista Nominal" = color_lista(ambito)),
                 type = "bar", text = ~paste0(format(Cantidad, big.mark = ","), " electores"),
                 hovertemplate = "<b>%{x}</b> – %{data.name}<br>%{text}<extra></extra>") %>%
      layout(
        title   = list(text = paste0("Padrón y LNE por Sexo (", etiq_ambito(ambito), ") ", anio),
                       font = list(size = 17, color = "#333", family = "Arial, sans-serif"), x = 0.5, xanchor = "center"),
        barmode = "group", xaxis = list(title = ""),
        yaxis   = list(title = "Número de electores", separatethousands = TRUE),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
        margin  = list(t = 110, b = 80, l = 90, r = 40),
        annotations = list(ann_alcance(alcance), ann_fuente())
      )
    nb <- tot$padron_no_binario %||% 0
    if (!is.na(nb) && nb > 0)
      p <- p %>% add_annotations(
        text = paste0("<b>No binario:</b><br>Padrón: ", fmt_num(nb), "<br>LNE: ", fmt_num(tot$lista_no_binario)),
        x = 1, y = 0.85, xref = "paper", yref = "paper", xanchor = "right", yanchor = "top",
        showarrow = FALSE, bgcolor = "#f8f9fa", bordercolor = "#6C757D",
        borderwidth = 1, borderpad = 6, font = list(size = 12, color = "#444")
      )
    p
  })
  
  output$semanal_sexo_dona <- renderPlotly({
    if (es_historico()) return(NULL)
    message("📊 [semanal_sexo_dona] Renderizando...")
    ambito <- ambito_reactivo(); alcance <- isolate(texto_alcance()); anio <- anio_semanal()
    datos  <- datos_semanal_sexo()
    if (is.null(datos)) return(plot_vacio())
    tot <- extraer_totales_sexo(datos, ambito)
    if (is.null(tot)) return(plot_vacio("Sin datos de sexo"))
    ph <- tot$padron_hombres; pm <- tot$padron_mujeres
    lh <- tot$lista_hombres;  lm <- tot$lista_mujeres
    if ((ph + pm) == 0) return(plot_vacio("Sin datos de padrón"))
    th <- if (ph > 0) round(lh / ph * 100, 2) else NA
    tm <- if (pm > 0) round(lm / pm * 100, 2) else NA
    df_d <- data.frame(Cat = c(paste0("Hombres\n", sprintf("%.2f%%", th)),
                               paste0("Mujeres\n",  sprintf("%.2f%%", tm))),
                       Tasa = c(th, tm), stringsAsFactors = FALSE)
    plot_ly(data = df_d, values = ~Tasa, labels = ~Cat, type = "pie", hole = 0.55,
            textinfo = "label+percent", textposition = "outside",
            marker = list(colors = c(color_h(ambito), color_m(ambito))),
            showlegend = FALSE, hovertemplate = "<b>%{label}</b><br>Tasa: %{value:.2f}%<extra></extra>") %>%
      layout(
        title = list(text = paste0("Tasa de Inclusión en LNE por Sexo (", etiq_ambito(ambito), ") ", anio),
                     font = list(size = 17, color = "#333", family = "Arial, sans-serif"), x = 0.5, xanchor = "center"),
        annotations = list(
          list(text = paste0("Padrón total:<br><b>", fmt_num(ph + pm), "</b>"),
               x = 0.5, y = 0.5, xref = "paper", yref = "paper",
               xanchor = "center", yanchor = "middle", showarrow = FALSE, font = list(size = 13, color = "#333")),
          ann_alcance(alcance, y_pos = 1.12), ann_fuente()
        ),
        margin = list(t = 120, b = 60, l = 40, r = 40)
      )
  })
  
  NOM_CORTOS <- c(
    "01"="AGS","02"="BC","03"="BCS","04"="CAMP","05"="COAH","06"="COL",
    "07"="CHIS","08"="CHIH","09"="CDMX","10"="DGO","11"="GTO","12"="GRO",
    "13"="HGO","14"="JAL","15"="MEX","16"="MICH","17"="MOR","18"="NAY",
    "19"="NL","20"="OAX","21"="PUE","22"="QRO","23"="QROO","24"="SLP",
    "25"="SIN","26"="SON","27"="TAB","28"="TAMS","29"="TLAX","30"="VER",
    "31"="YUC","32"="ZAC","87"="MEX.EXT","88"="NAT."
  )
  etiq_col <- function(col) {
    clave <- sprintf("%02s", gsub("ln_|ln|lista_", "", col, ignore.case = TRUE))
    NOM_CORTOS[clave] %||% col
  }
  
  output$semanal_origen_calor <- renderPlotly({
    if (es_historico()) return(NULL)
    message("📊 [semanal_origen_calor] Renderizando...")
    ambito <- ambito_reactivo(); alcance <- isolate(texto_alcance()); anio <- anio_semanal()
    datos  <- datos_semanal_origen()
    if (is.null(datos)) return(plot_vacio())
    df_uso <- if (ambito == "extranjero")
      datos[grepl("RESIDENTES EXTRANJERO", toupper(datos$nombre_entidad %||% ""), ignore.case = TRUE), ]
    else df_nacional(datos)
    if (is.null(df_uso) || nrow(df_uso) == 0) return(plot_vacio("Sin datos de origen"))
    cols_ln <- grep("^ln_\\d{2}$|^ln8[78]$", colnames(df_uso), value = TRUE, ignore.case = TRUE)
    if (length(cols_ln) == 0) cols_ln <- grep("^lista_\\d|^ln_\\d", colnames(df_uso), value = TRUE, ignore.case = TRUE)
    if (length(cols_ln) == 0) return(plot_vacio("Columnas de origen no encontradas"))
    df_uso$total_lne <- rowSums(df_uso[, cols_ln, drop = FALSE], na.rm = TRUE)
    top10_idx  <- order(df_uso$total_lne, decreasing = TRUE)[1:min(10, nrow(df_uso))]
    df_top     <- df_uso[top10_idx, ]
    tots_orig  <- colSums(df_uso[, cols_ln, drop = FALSE], na.rm = TRUE)
    top10_cols <- names(sort(tots_orig, decreasing = TRUE))[1:min(10, length(cols_ln))]
    mat <- as.matrix(df_top[, top10_cols, drop = FALSE])
    colnames(mat) <- sapply(top10_cols, etiq_col); rownames(mat) <- df_top$nombre_entidad
    cs <- if (ambito == "extranjero") list(c(0,"#FFF9E6"), c(0.5,"#EAC43E"), c(1,"#8F6A00"))
    else list(c(0,"#E8EDF8"), c(0.5,"#44559B"), c(1,"#1A2654"))
    plot_ly(z = mat, x = colnames(mat), y = rownames(mat), type = "heatmap",
            colorscale = cs, showscale = TRUE,
            hovertemplate = "Receptor: <b>%{y}</b><br>Origen: <b>%{x}</b><br>LNE: %{z:,.0f}<extra></extra>") %>%
      layout(
        title  = list(text = paste0("Top 10 Receptores vs. Origen (", etiq_ambito(ambito), ") ", anio),
                      font = list(size = 16, color = "#333", family = "Arial, sans-serif"), x = 0.5, xanchor = "center"),
        xaxis  = list(title = "Estado de Origen", tickangle = -40),
        yaxis  = list(title = "Estado Receptor"),
        margin = list(t = 120, b = 100, l = 160, r = 60),
        annotations = list(ann_alcance(alcance, y_pos = 1.10), ann_fuente())
      )
  })
  
  output$semanal_origen_barras <- renderPlotly({
    if (es_historico()) return(NULL)
    message("📊 [semanal_origen_barras] Renderizando...")
    ambito    <- ambito_reactivo(); alcance <- isolate(texto_alcance()); anio <- anio_semanal()
    datos     <- datos_semanal_origen()
    top_n_sel <- suppressWarnings(as.integer(input$semanal_top_n %||% "5"))
    if (is.na(top_n_sel)) top_n_sel <- 0L
    if (is.null(datos)) return(plot_vacio())
    tabla <- construir_tabla_origen(datos, ambito)
    if (is.null(tabla) || nrow(tabla) == 0) return(plot_vacio("Sin datos de estados de origen"))
    df_plot <- if (top_n_sel > 0) head(tabla, min(top_n_sel, nrow(tabla))) else tabla
    plot_ly() %>%
      add_trace(data = df_plot, y = ~reorder(entidad_origen, lista_nominal), x = ~lista_nominal,
                type = "bar", orientation = "h", name = "Lista Nominal",
                marker = list(color = color_lista(ambito)),
                hovertemplate = "<b>%{y}</b><br>LNE: %{x:,.0f}<extra></extra>") %>%
      add_trace(data = df_plot, y = ~reorder(entidad_origen, lista_nominal), x = ~padron,
                type = "bar", orientation = "h", name = "Padrón Electoral",
                marker = list(color = color_padron(ambito)),
                hovertemplate = "<b>%{y}</b><br>Padrón: %{x:,.0f}<extra></extra>") %>%
      layout(
        title   = list(text = paste0("Top ", nrow(df_plot), " Estados de Origen (", etiq_ambito(ambito), ") ", anio),
                       font = list(size = 17, color = "#333", family = "Arial, sans-serif"), x = 0.5, xanchor = "center"),
        barmode = "group",
        xaxis   = list(title = "Número de electores", separatethousands = TRUE),
        yaxis   = list(title = ""),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.12),
        margin  = list(t = 120, b = 80, l = 200, r = 40),
        annotations = list(ann_alcance(alcance, y_pos = 1.10), ann_fuente())
      )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # DATATABLES
  # ════════════════════════════════════════════════════════════════════════════
  
  datos_dt_edad_r <- reactive({
    if (es_historico()) return(NULL)
    df <- construir_df_edad(datos_semanal_edad(), ambito_reactivo())
    if (is.null(df)) return(NULL)
    df$padron_total <- df$padron_hombres + df$padron_mujeres
    df$lista_total  <- df$lista_hombres  + df$lista_mujeres
    df$tasa <- round(df$lista_total / df$padron_total * 100, 2)
    colnames(df) <- c("Rango de Edad","Padrón Hombres","Padrón Mujeres",
                      "LNE Hombres","LNE Mujeres","Padrón Total","LNE Total","Tasa Inclusión (%)")
    df
  })
  output$semanal_dt_edad <- DT::renderDataTable({
    df <- datos_dt_edad_r()
    if (is.null(df)) return(DT::datatable(data.frame(Mensaje = "Sin datos"), options = list(dom = "t")))
    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE, dom = "tip",
                                 language = list(paginate = list(previous = "Anterior", `next` = "Siguiente"),
                                                 info = "Mostrando _START_ a _END_ de _TOTAL_ registros"))) %>%
      DT::formatRound(c("Padrón Hombres","Padrón Mujeres","LNE Hombres","LNE Mujeres","Padrón Total","LNE Total"), digits = 0) %>%
      DT::formatRound("Tasa Inclusión (%)", digits = 2)
  })
  output$semanal_dt_edad_descarga <- downloadHandler(
    filename = function() paste0("sefix_edad_", etiq_ambito(ambito_reactivo()), "_", anio_semanal(), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) { df <- datos_dt_edad_r(); if (!is.null(df)) write.csv(df, file, row.names = FALSE) }
  )
  
  datos_dt_sexo_r <- reactive({
    if (es_historico()) return(NULL)
    tot <- extraer_totales_sexo(datos_semanal_sexo(), ambito_reactivo())
    if (is.null(tot)) return(NULL)
    ph <- tot$padron_hombres; pm <- tot$padron_mujeres
    lh <- tot$lista_hombres;  lm <- tot$lista_mujeres
    nb_p <- tot$padron_no_binario; nb_l <- tot$lista_no_binario
    data.frame(Sexo = c("Hombres","Mujeres","No binario"),
               `Padrón Electoral` = c(ph, pm, nb_p), `Lista Nominal` = c(lh, lm, nb_l),
               `Tasa de Inclusión (%)` = c(round(lh/ph*100,2), round(lm/pm*100,2),
                                           if (!is.na(nb_p) && nb_p > 0) round(nb_l/nb_p*100,2) else NA),
               stringsAsFactors = FALSE, check.names = FALSE)
  })
  output$semanal_dt_sexo <- DT::renderDataTable({
    df <- datos_dt_sexo_r()
    if (is.null(df)) return(DT::datatable(data.frame(Mensaje = "Sin datos"), options = list(dom = "t")))
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = "t")) %>%
      DT::formatRound(c("Padrón Electoral","Lista Nominal"), digits = 0) %>%
      DT::formatRound("Tasa de Inclusión (%)", digits = 2)
  })
  output$semanal_dt_sexo_descarga <- downloadHandler(
    filename = function() paste0("sefix_sexo_", etiq_ambito(ambito_reactivo()), "_", anio_semanal(), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) { df <- datos_dt_sexo_r(); if (!is.null(df)) write.csv(df, file, row.names = FALSE) }
  )
  
  datos_dt_origen_r <- reactive({
    if (es_historico()) return(NULL)
    tabla <- construir_tabla_origen(datos_semanal_origen(), ambito_reactivo())
    if (is.null(tabla)) return(NULL)
    tabla$tasa <- round(tabla$lista_nominal / tabla$padron * 100, 2)
    colnames(tabla) <- c("Estado de Origen","Padrón Electoral","Lista Nominal","Tasa Inclusión (%)")
    tabla
  })
  output$semanal_dt_origen <- DT::renderDataTable({
    df <- datos_dt_origen_r()
    if (is.null(df)) return(DT::datatable(data.frame(Mensaje = "Sin datos"), options = list(dom = "t")))
    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE, dom = "tip",
                                 language = list(paginate = list(previous = "Anterior", `next` = "Siguiente"),
                                                 info = "Mostrando _START_ a _END_ de _TOTAL_ registros"))) %>%
      DT::formatRound(c("Padrón Electoral","Lista Nominal"), digits = 0) %>%
      DT::formatRound("Tasa Inclusión (%)", digits = 2)
  })
  output$semanal_dt_origen_descarga <- downloadHandler(
    filename = function() paste0("sefix_origen_", etiq_ambito(ambito_reactivo()), "_", anio_semanal(), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) { df <- datos_dt_origen_r(); if (!is.null(df)) write.csv(df, file, row.names = FALSE) }
  )
  
  # ════════════════════════════════════════════════════════════════════════════
  # SIDEBAR DERECHO — Análisis textual condicional por desglose
  # Fase 2: cada renderUI muestra solo el bloque del desglose activo
  # ════════════════════════════════════════════════════════════════════════════
  
  css_h4 <- "margin:22px 0 6px 0;font-size:17px;color:#2c3e50;font-weight:600;border-bottom:1px solid #e0e0e0;padding-bottom:4px;"
  css_p  <- "margin:0 0 8px 0;font-size:15px;line-height:1.6;color:#333;"
  css_na <- "text-align:center;color:#999;padding:10px;font-style:italic;font-size:14px;"
  
  # ── Título del sidebar (siempre muestra el desglose activo) ─────────────────
  output$semanal_texto_titulo <- renderUI({
    if (es_historico()) return(NULL)
    ambito  <- ambito_reactivo()
    anio    <- anio_semanal()
    desglose <- desglose_activo()
    etiq_des <- switch(desglose,
                       "edad"   = "Rango de Edad",
                       "sexo"   = "Distribución por Sexo",
                       "origen" = "Entidad de Origen",
                       "Rango de Edad"
    )
    HTML(paste0(
      "<div style='font-size:16px;line-height:1.6;color:#333;'>",
      "<h3 style='text-align:center;margin:0 0 2px 0;font-size:18px;color:#2c3e50;font-weight:600;'>",
      "Análisis Semanal — ", etiq_des, "<br>",
      "<span style='display:block;text-align:center;color:#1a5276;font-weight:700;font-size:20px;margin-top:4px;margin-bottom:4px;'>",
      etiq_ambito(ambito), " ", anio, "</span></h3>",
      "<p style='margin:12px 0 2px 0;font-size:14px;color:#555;font-weight:600;text-align:left;'>Alcance:</p>",
      "<p style='text-align:left;margin:0;font-size:14px;color:#777;line-height:1.4;'>",
      gsub(" - ", " – ", isolate(texto_alcance())), "</p></div>"
    ))
  })
  
  # ── Análisis textual condicional por desglose ────────────────────────────────
  output$semanal_texto_analisis <- renderUI({
    if (es_historico()) return(NULL)
    ambito   <- ambito_reactivo()
    etiq     <- etiq_ambito(ambito)
    desglose <- desglose_activo()
    
    contenido <- switch(desglose,
                        
                        # ── Bloque EDAD ──────────────────────────────────────────────────────────
                        "edad" = tryCatch({
                          df <- construir_df_edad(datos_semanal_edad(), ambito)
                          if (is.null(df) || nrow(df) == 0)
                            return(HTML(paste0("<p style='", css_na, "'>Sin datos de edad disponibles.</p>")))
                          df$lst <- df$lista_hombres + df$lista_mujeres
                          df$pad <- df$padron_hombres + df$padron_mujeres
                          tot_lne <- sum(df$lst, na.rm = TRUE)
                          tot_pad <- sum(df$pad, na.rm = TRUE)
                          gm  <- df$grupo[which.max(df$lst)]
                          mx  <- max(df$lst)
                          gm2 <- df$grupo[order(df$lst, decreasing = TRUE)[2]]
                          lne_18_19 <- sum(df$lst[df$grupo %in% c("18","19")], na.rm = TRUE)
                          lne_65p   <- sum(df$lst[df$grupo %in% c("65+")], na.rm = TRUE)
                          pct_jov <- if (tot_lne > 0) round(lne_18_19 / tot_lne * 100, 2) else NA
                          pct_65  <- if (tot_lne > 0) round(lne_65p   / tot_lne * 100, 2) else NA
                          tasa_inc <- if (tot_pad > 0) round(tot_lne / tot_pad * 100, 2) else NA
                          paste0(
                            "<h4 style='", css_h4, "'>Rango de edad — ", etiq, "</h4>",
                            "<p style='", css_p, "'>La Lista Nominal ", etiq, " registra <strong>",
                            fmt_num(tot_lne), "</strong> electores, con una tasa de inclusión de <strong>",
                            fmt_pct(tasa_inc), "</strong> respecto al Padrón Electoral.</p>",
                            "<p style='", css_p, "'>El grupo de edad con mayor representación es el de <strong>",
                            gm, " años</strong> (<strong>", fmt_num(mx), "</strong> electores), seguido por el grupo de <strong>",
                            gm2, " años</strong>.</p>",
                            if (!is.na(pct_jov)) paste0("<p style='", css_p, "'>Los electores de 18 y 19 años representan el <strong>",
                                                        fmt_pct(pct_jov), "</strong> de la LNE — el segmento más joven del padrón.</p>") else "",
                            if (!is.na(pct_65)) paste0("<p style='", css_p, "'>Los electores de 65 años o más representan el <strong>",
                                                       fmt_pct(pct_65), "</strong> de la LNE.</p>") else ""
                          )
                        }, error = function(e) paste0("<p style='", css_na, "'>Error al procesar datos de edad.</p>")),
                        
                        # ── Bloque SEXO ──────────────────────────────────────────────────────────
                        "sexo" = tryCatch({
                          tot <- extraer_totales_sexo(datos_semanal_sexo(), ambito)
                          if (is.null(tot))
                            return(HTML(paste0("<p style='", css_na, "'>Sin datos de sexo disponibles.</p>")))
                          ph <- tot$padron_hombres; pm <- tot$padron_mujeres
                          lh <- tot$lista_hombres;  lm <- tot$lista_mujeres
                          pt <- ph + pm; lt <- lh + lm
                          ti   <- if (pt > 0) round(lt / pt * 100, 2) else NA
                          ti_h <- if (ph > 0) round(lh / ph * 100, 2) else NA
                          ti_m <- if (pm > 0) round(lm / pm * 100, 2) else NA
                          pct_h_lne <- if (lt > 0) round(lh / lt * 100, 2) else NA
                          pct_m_lne <- if (lt > 0) round(lm / lt * 100, 2) else NA
                          sm_pad <- if (pm >= ph) "mujeres" else "hombres"
                          sm_ln  <- if (lm >= lh) "mujeres" else "hombres"
                          nb_p <- tot$padron_no_binario %||% 0
                          nb_l <- tot$lista_no_binario  %||% 0
                          txt_nb <- if (!is.na(nb_p) && nb_p > 0)
                            paste0("<p style='", css_p, "'>El Padrón registra <strong>", fmt_num(nb_p),
                                   "</strong> personas no binarias, con <strong>", fmt_num(nb_l),
                                   "</strong> en la Lista Nominal.</p>") else ""
                          paste0(
                            "<h4 style='", css_h4, "'>Distribución por sexo — ", etiq, "</h4>",
                            "<p style='", css_p, "'>El Padrón Electoral ", etiq, " asciende a <strong>", fmt_num(pt),
                            "</strong>, con mayor presencia de <strong>", sm_pad, "</strong>. ",
                            "La Lista Nominal totaliza <strong>", fmt_num(lt),
                            "</strong> con tasa de inclusión global de <strong>", fmt_pct(ti), "</strong>.</p>",
                            "<p style='", css_p, "'>En la LNE predominan las/los <strong>", sm_ln, "</strong>: ",
                            "<strong>", fmt_pct(pct_m_lne), "</strong> son mujeres y <strong>",
                            fmt_pct(pct_h_lne), "</strong> son hombres.</p>",
                            "<p style='", css_p, "'>Tasas de inclusión por sexo: hombres <strong>",
                            fmt_pct(ti_h), "</strong> — mujeres <strong>", fmt_pct(ti_m), "</strong>.</p>",
                            txt_nb
                          )
                        }, error = function(e) paste0("<p style='", css_na, "'>Error al procesar datos de sexo.</p>")),
                        
                        # ── Bloque ORIGEN ────────────────────────────────────────────────────────
                        "origen" = tryCatch({
                          tabla <- construir_tabla_origen(datos_semanal_origen(), ambito)
                          if (is.null(tabla) || nrow(tabla) < 3)
                            return(HTML(paste0("<p style='", css_na, "'>Sin datos suficientes de origen.</p>")))
                          top3    <- head(tabla, 3)
                          tlt     <- sum(tabla$lista_nominal, na.rm = TRUE)
                          txt_top3 <- paste(sapply(1:3, function(i)
                            paste0("<strong>", i, ". ", top3$entidad_origen[i], "</strong> (",
                                   fmt_num(top3$lista_nominal[i]), " en LNE — ",
                                   fmt_pct(round(top3$lista_nominal[i] / tlt * 100, 2)), ")")),
                            collapse = "; ")
                          r87 <- tabla[grepl("nacidos en el extranjero", tabla$entidad_origen, ignore.case = TRUE), ]
                          r88 <- tabla[grepl("naturalizados", tabla$entidad_origen, ignore.case = TRUE), ]
                          txt_87 <- if (nrow(r87) > 0 && !is.na(r87$lista_nominal[1]) && r87$lista_nominal[1] > 0)
                            paste0("<p style='", css_p, "'>Los ciudadanos mexicanos nacidos en el extranjero suman <strong>",
                                   fmt_num(r87$lista_nominal[1]), "</strong> en la LNE (<strong>",
                                   fmt_pct(round(r87$lista_nominal[1] / tlt * 100, 2)), "</strong>).</p>") else ""
                          txt_88 <- if (nrow(r88) > 0 && !is.na(r88$lista_nominal[1]) && r88$lista_nominal[1] > 0)
                            paste0("<p style='", css_p, "'>Los ciudadanos naturalizados representan <strong>",
                                   fmt_num(r88$lista_nominal[1]), "</strong> electores en la LNE.</p>") else ""
                          paste0(
                            "<h4 style='", css_h4, "'>Entidad de origen — ", etiq, "</h4>",
                            "<p style='", css_p, "'>Los tres principales estados de origen son: ", txt_top3, ".</p>",
                            txt_87, txt_88
                          )
                        }, error = function(e) paste0("<p style='", css_na, "'>Error al procesar datos de origen.</p>")),
                        
                        # fallback
                        paste0("<p style='", css_na, "'>Selecciona un desglose para ver el análisis.</p>")
    )
    
    HTML(paste0("<div style='font-size:16px;line-height:1.6;color:#333;'>", contenido, "</div>"))
  })
  
  message("✅ graficas_semanal v2.3 inicializado")
  message("   ✅ Fase 2: sidebar derecho condicional por desglose (edad / sexo / origen)")
  message("   ✅ Gráficas y DataTables sin cambios")
}
