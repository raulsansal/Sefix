# modules/lista_nominal_graficas/graficas_semanal_sexo.R
# Vista Semanal — Gráficas de Sexo: S1–S7
# Versión: 1.0
#
# Gráficas:
#   S1 (semanal_s1_piramide)    — Pirámide por 12 rangos individuales (H vs M en LNE)
#   S2 (semanal_s2_mujeres)     — Barras horiz.: LNE Mujeres por grupo etario
#   S3 (semanal_s3_hombres)     — Barras horiz.: LNE Hombres por grupo etario
#   S4 (semanal_s4_nobinario)   — Barras horiz.: LNE No Binario por grupo etario
#                                  S2/S3/S4 se muestran en 3 columnas en la UI
#   S5 (semanal_s5_barras)      — Barras agrupadas Padrón/LNE por sexo
#   S6 (semanal_s6_dona)        — Dona tasa inclusión, etiquetas mejoradas
#   S7 (semanal_s7_proyeccion)  — Proyección semanal Padrón/LNE por sexo + card NB
#
# Dependencias del entorno padre (graficas_semanal.R):
#   COLORES, ORDEN_EDAD, FUENTE_INE
#   fmt_num(), fmt_pct(), etiq_ambito(), etiqueta_edad()
#   ann_fuente(), ann_alcance(), plot_vacio()
#   color_padron(), color_lista(), color_h(), color_m()
#   es_historico(), desglose_activo()
#   construir_df_edad(), extraer_totales_sexo()
#   crear_card_no_binario()   ← de graficas_helpers.R
#
# Dependencias de datos (pasadas como argumentos):
#   datos_semanal_edad()        ← S1, S2, S3, S4
#   datos_semanal_sexo()        ← S5, S6
#   datos_semanal_serie_sexo()  ← S7
#   anio_semanal(), texto_alcance(), ambito_reactivo(), estado_app()

graficas_semanal_sexo <- function(input, output, session,
                                  datos_semanal_edad,
                                  datos_semanal_sexo,
                                  datos_semanal_serie_sexo,
                                  anio_semanal,
                                  texto_alcance,
                                  ambito_reactivo,
                                  estado_app) {
  
  message("📊 Inicializando graficas_semanal_sexo v1.0")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CONSTANTES LOCALES
  # ══════════════════════════════════════════════════════════════════════════
  
  RANGOS_EDAD <- c("18","19","20_24","25_29","30_34","35_39",
                   "40_44","45_49","50_54","55_59","60_64","65_y_mas")
  
  GRUPOS_ETARIOS <- list(
    "Jóvenes\n(18–29)"  = c("18","19","20_24","25_29"),
    "Adultos\n(30–59)"  = c("30_34","35_39","40_44","45_49","50_54","55_59"),
    "Mayores\n(60+)"    = c("60_64","65_y_mas")
  )
  NOMBRES_GRUPOS <- names(GRUPOS_ETARIOS)
  
  # Paletas por ámbito
  COLOR_H_NAC  <- "#44559B"; COLOR_H_EXT  <- "#D4A500"
  COLOR_M_NAC  <- "#C0311A"; COLOR_M_EXT  <- "#8FB369"
  COLOR_NB_NAC <- "#9B59B6"; COLOR_NB_EXT <- "#5C9900"
  
  color_h_loc  <- function(a) if (a == "extranjero") COLOR_H_EXT  else COLOR_H_NAC
  color_m_loc  <- function(a) if (a == "extranjero") COLOR_M_EXT  else COLOR_M_NAC
  color_nb_loc <- function(a) if (a == "extranjero") COLOR_NB_EXT else COLOR_NB_NAC
  
  # ── Helper: agrega LNE por grupo etario para un sexo dado ─────────────────
  # sexo_sufijo: "hombres" | "mujeres" | "no_binario"
  construir_df_grupos_sexo <- function(df_edad, ambito, sexo_sufijo) {
    if (is.null(df_edad) || nrow(df_edad) == 0) return(NULL)
    
    df_rango <- construir_df_edad(df_edad, ambito)
    if (is.null(df_rango) || nrow(df_rango) == 0) return(NULL)
    
    col_sexo <- paste0("lista_", sexo_sufijo)
    
    # construir_df_edad devuelve: grupo, padron_hombres, padron_mujeres,
    #                             lista_hombres, lista_mujeres
    # Para no_binario necesitamos sumar directamente desde el df original
    if (sexo_sufijo == "no_binario") {
      # Sumar lista_[rango]_no_binario desde el df crudo
      resultado <- do.call(rbind, lapply(seq_along(GRUPOS_ETARIOS), function(i) {
        rangos      <- GRUPOS_ETARIOS[[i]]
        total_nb    <- 0
        for (r in rangos) {
          col <- paste0("lista_", r, "_no_binario")
          if (col %in% colnames(df_edad)) {
            fila_uso <- if (ambito == "extranjero") {
              df_edad[grepl("RESIDENTES EXTRANJERO", toupper(df_edad$nombre_entidad), fixed = TRUE), ]
            } else {
              df_edad[!grepl("RESIDENTES EXTRANJERO|^TOTALES$",
                             toupper(trimws(df_edad$nombre_entidad))), ]
            }
            total_nb <- total_nb + sum(as.numeric(fila_uso[[col]]), na.rm = TRUE)
          }
        }
        data.frame(grupo = NOMBRES_GRUPOS[i], lne = total_nb,
                   stringsAsFactors = FALSE)
      }))
    } else {
      resultado <- do.call(rbind, lapply(seq_along(GRUPOS_ETARIOS), function(i) {
        rangos     <- GRUPOS_ETARIOS[[i]]
        etiq_r     <- sapply(rangos, etiqueta_edad)
        filas      <- df_rango[df_rango$grupo %in% etiq_r, ]
        lne        <- sum(filas[[col_sexo]], na.rm = TRUE)
        data.frame(grupo = NOMBRES_GRUPOS[i], lne = lne,
                   stringsAsFactors = FALSE)
      }))
    }
    resultado
  }
  
  # ── Helper: grafica de barras horizontales por grupo etario (S2, S3, S4) ──
  grafica_grupos_sexo <- function(df_grupos, titulo, color_barra, anio,
                                  alcance, ambito, rango_x_max = NULL) {
    if (is.null(df_grupos) || nrow(df_grupos) == 0) return(plot_vacio())
    
    total <- sum(df_grupos$lne, na.rm = TRUE)
    df_grupos$pct <- if (total > 0) round(df_grupos$lne / total * 100, 1) else 0
    
    # Etiqueta: valor + % solo si lne > 0
    df_grupos$texto <- ifelse(
      df_grupos$lne > 0,
      paste0(format(df_grupos$lne, big.mark = ","), "\n(", df_grupos$pct, "%)"),
      ""
    )
    
    x_max <- if (!is.null(rango_x_max)) rango_x_max * 1.30
    else max(df_grupos$lne, na.rm = TRUE) * 1.35
    
    plot_ly(
      data        = df_grupos,
      y           = ~factor(grupo, levels = rev(NOMBRES_GRUPOS)),
      x           = ~lne,
      type        = "bar",
      orientation = "h",
      marker      = list(color = color_barra,
                         line  = list(color = "#fff", width = 0.5)),
      text        = ~texto,
      textposition = "outside",
      cliponaxis   = FALSE,
      hovertemplate = "<b>%{y}</b><br>LNE: %{x:,.0f}<extra></extra>"
    ) %>%
      layout(
        title  = list(
          text = titulo,
          font = list(size = 15, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        xaxis  = list(
          title = "LNE",
          separatethousands = TRUE,
          range = c(0, x_max)
        ),
        yaxis  = list(title = ""),
        margin = list(t = 80, b = 50, l = 110, r = 20),
        annotations = list(ann_alcance(alcance, y_pos = 1.08), ann_fuente())
      )
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # S1 — Pirámide completa por 12 rangos individuales (H vs M en LNE)
  # Movida desde semanal_edad_piramide en graficas_semanal.R v2.3
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_s1_piramide <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    datos <- datos_semanal_edad()
    if (is.null(datos)) return(plot_vacio())
    
    df <- construir_df_edad(datos, ambito)
    if (is.null(df) || nrow(df) == 0) return(plot_vacio("Sin datos de edad"))
    
    niveles_y <- sapply(rev(RANGOS_EDAD), etiqueta_edad)
    
    plot_ly() %>%
      add_trace(
        data = df,
        x = ~(-lista_hombres), y = ~factor(grupo, levels = niveles_y),
        type = "bar", orientation = "h", name = "LNE Hombres",
        marker    = list(color = color_h_loc(ambito)),
        customdata = ~lista_hombres,
        hovertemplate = "<b>%{y}</b><br>LNE Hombres: %{customdata:,.0f}<extra></extra>"
      ) %>%
      add_trace(
        data = df,
        x = ~lista_mujeres, y = ~factor(grupo, levels = niveles_y),
        type = "bar", orientation = "h", name = "LNE Mujeres",
        marker    = list(color = color_m_loc(ambito)),
        hovertemplate = "<b>%{y}</b><br>LNE Mujeres: %{x:,.0f}<extra></extra>"
      ) %>%
      layout(
        title   = list(
          text = paste0("Pirámide de Edad — LNE por Sexo (", etiq, ") ", anio),
          font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        barmode = "overlay", bargap = 0.1,
        xaxis   = list(
          title = "Número de electores",
          tickformat = ",.0f",
          zeroline = TRUE, zerolinecolor = "#999", zerolinewidth = 1.5
        ),
        yaxis   = list(title = "", categoryorder = "array",
                       categoryarray = niveles_y),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
        margin  = list(t = 110, b = 80, l = 75, r = 40),
        hovermode = "y unified",
        annotations = list(ann_alcance(alcance), ann_fuente())
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # S2, S3, S4 — Pirámides por grupo etario, una por sexo
  # Se calculan los tres en un solo reactive para compartir rango_x_max
  # y mantener ejes comparables entre las tres gráficas
  # ══════════════════════════════════════════════════════════════════════════
  
  datos_grupos_r <- reactive({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    ambito <- ambito_reactivo()
    datos  <- datos_semanal_edad()
    if (is.null(datos)) return(NULL)
    
    df_m  <- construir_df_grupos_sexo(datos, ambito, "mujeres")
    df_h  <- construir_df_grupos_sexo(datos, ambito, "hombres")
    df_nb <- construir_df_grupos_sexo(datos, ambito, "no_binario")
    
    # Calcular rango X máximo compartido (Mujeres y Hombres)
    x_max <- max(
      if (!is.null(df_m))  max(df_m$lne,  na.rm = TRUE) else 0,
      if (!is.null(df_h))  max(df_h$lne,  na.rm = TRUE) else 0,
      na.rm = TRUE
    )
    
    list(mujeres = df_m, hombres = df_h, no_binario = df_nb, x_max = x_max)
  })
  
  output$semanal_s2_mujeres <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    dg <- datos_grupos_r()
    if (is.null(dg)) return(plot_vacio())
    
    grafica_grupos_sexo(
      df_grupos   = dg$mujeres,
      titulo      = paste0("Mujeres (", etiq, ") ", anio),
      color_barra = color_m_loc(ambito),
      anio        = anio,
      alcance     = alcance,
      ambito      = ambito,
      rango_x_max = dg$x_max
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  output$semanal_s3_hombres <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    dg <- datos_grupos_r()
    if (is.null(dg)) return(plot_vacio())
    
    grafica_grupos_sexo(
      df_grupos   = dg$hombres,
      titulo      = paste0("Hombres (", etiq, ") ", anio),
      color_barra = color_h_loc(ambito),
      anio        = anio,
      alcance     = alcance,
      ambito      = ambito,
      rango_x_max = dg$x_max
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  output$semanal_s4_nobinario <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    dg <- datos_grupos_r()
    if (is.null(dg)) return(plot_vacio())
    
    # S4: eje X propio (escala NB es mucho menor que H/M)
    # pero mantiene los mismos 3 grupos en eje Y → congruencia visual
    grafica_grupos_sexo(
      df_grupos   = dg$no_binario,
      titulo      = paste0("No Binario (", etiq, ") ", anio),
      color_barra = color_nb_loc(ambito),
      anio        = anio,
      alcance     = alcance,
      ambito      = ambito,
      rango_x_max = NULL   # escala propia para NB
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # S5 — Barras agrupadas Padrón/LNE por sexo
  # (= semanal_sexo_barras renombrado, sin cambios de lógica)
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_s5_barras <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    datos <- datos_semanal_sexo()
    if (is.null(datos)) return(plot_vacio())
    
    tot <- extraer_totales_sexo(datos, ambito)
    if (is.null(tot)) return(plot_vacio("Sin datos de sexo"))
    
    df_g <- data.frame(
      Sexo     = rep(c("Hombres","Mujeres"), 2),
      Tipo     = rep(c("Padrón Electoral","Lista Nominal"), each = 2),
      Cantidad = c(tot$padron_hombres, tot$padron_mujeres,
                   tot$lista_hombres,  tot$lista_mujeres),
      stringsAsFactors = FALSE
    )
    
    p <- plot_ly(
      data  = df_g, x = ~Sexo, y = ~Cantidad, color = ~Tipo,
      colors = c("Padrón Electoral" = color_padron(ambito),
                 "Lista Nominal"    = color_lista(ambito)),
      type  = "bar",
      text  = ~paste0(format(Cantidad, big.mark = ","), " electores"),
      hovertemplate = "<b>%{x}</b> – %{data.name}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        title   = list(
          text = paste0("Padrón y LNE por Sexo (", etiq, ") ", anio),
          font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        barmode = "group", xaxis = list(title = ""),
        yaxis   = list(title = "Número de electores", separatethousands = TRUE),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
        margin  = list(t = 110, b = 80, l = 90, r = 40),
        annotations = list(ann_alcance(alcance), ann_fuente())
      )
    
    # Card No Binario
    nb_p <- tot$padron_no_binario %||% 0
    nb_l <- tot$lista_no_binario  %||% 0
    if (!is.na(nb_p) && nb_p > 0) {
      p <- p %>% add_annotations(
        text = paste0("<b>No binario:</b><br>Padrón: ", fmt_num(nb_p),
                      "<br>LNE: ", fmt_num(nb_l)),
        x = 1, y = 0.85, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top", showarrow = FALSE,
        bgcolor = "#f8f9fa", bordercolor = "#9B59B6",
        borderwidth = 1, borderpad = 6,
        font = list(size = 12, color = "#444")
      )
    }
    p
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # S6 — Dona tasa de inclusión por sexo (etiquetas mejoradas)
  # Fix vs v2.3: etiquetas muestran "Padrón: X%\nLNE: Y%" por sector
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_s6_dona <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    datos <- datos_semanal_sexo()
    if (is.null(datos)) return(plot_vacio())
    
    tot <- extraer_totales_sexo(datos, ambito)
    if (is.null(tot)) return(plot_vacio("Sin datos de sexo"))
    
    ph <- tot$padron_hombres; pm <- tot$padron_mujeres
    lh <- tot$lista_hombres;  lm <- tot$lista_mujeres
    pt <- ph + pm
    
    if (pt == 0) return(plot_vacio("Sin datos de padrón"))
    
    ti_h <- if (ph > 0) round(lh / ph * 100, 2) else NA
    ti_m <- if (pm > 0) round(lm / pm * 100, 2) else NA
    
    # Peso de cada sector = tasa de inclusión (como en v2.3)
    # Etiqueta mejorada: "Hombres\nPadrón: X%\nLNE: Y%"
    pct_h_pad <- round(ph / pt * 100, 2)
    pct_m_pad <- round(pm / pt * 100, 2)
    pct_h_lne <- if ((lh + lm) > 0) round(lh / (lh + lm) * 100, 2) else NA
    pct_m_lne <- if ((lh + lm) > 0) round(lm / (lh + lm) * 100, 2) else NA
    
    etiq_h <- paste0("Hombres<br>Padrón: ", fmt_pct(pct_h_pad),
                     "<br>LNE: ",    fmt_pct(pct_h_lne))
    etiq_m <- paste0("Mujeres<br>Padrón: ", fmt_pct(pct_m_pad),
                     "<br>LNE: ",    fmt_pct(pct_m_lne))
    
    df_d <- data.frame(
      Cat  = c(etiq_h, etiq_m),
      Tasa = c(ti_h, ti_m),
      stringsAsFactors = FALSE
    )
    
    plot_ly(
      data = df_d, values = ~Tasa, labels = ~Cat,
      type = "pie", hole = 0.55,
      textinfo     = "label",
      textposition = "outside",
      marker = list(colors = c(color_h_loc(ambito), color_m_loc(ambito))),
      showlegend = FALSE,
      hovertemplate = "<b>%{label}</b><br>Tasa inclusión: %{value:.2f}%<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = paste0("Tasa de Inclusión en LNE por Sexo (", etiq, ") ", anio),
          font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        annotations = list(
          list(
            text = paste0("Padrón total:<br><b>", fmt_num(pt), "</b>"),
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "middle",
            showarrow = FALSE, font = list(size = 13, color = "#333")
          ),
          ann_alcance(alcance, y_pos = 1.12),
          ann_fuente()
        ),
        margin = list(t = 120, b = 60, l = 40, r = 40)
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # S7 — Proyección semanal Padrón/LNE por sexo + card No Binario
  # Usa datos_semanal_serie_sexo() (serie temporal)
  # Proyección solo para Hombres y Mujeres; NB en card estática
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_s7_proyeccion <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    serie <- datos_semanal_serie_sexo()
    if (is.null(serie) || nrow(serie) < 2) {
      return(plot_vacio("Sin datos de serie temporal para proyección"))
    }
    
    ultima_fecha <- max(serie$fecha)
    ultimo_mes   <- as.integer(format(ultima_fecha, "%m"))
    meses_rest   <- 12L - ultimo_mes
    
    # Ordenar trazas por valor final descendente
    vals_finales <- c(
      padron_h = tail(serie$padron_hombres, 1),
      padron_m = tail(serie$padron_mujeres, 1),
      lista_h  = tail(serie$lista_hombres,  1),
      lista_m  = tail(serie$lista_mujeres,  1)
    )
    orden_trazas <- names(sort(vals_finales, decreasing = TRUE))
    
    config_trazas <- list(
      padron_h = list(
        col_y = "padron_hombres", nombre = "Padrón Hombres",
        color = color_h_loc(ambito), dash = "solid",
        col_proy_y = "padron_electoral", col_proy_n = "Proy. Padrón H"
      ),
      padron_m = list(
        col_y = "padron_mujeres", nombre = "Padrón Mujeres",
        color = color_m_loc(ambito), dash = "solid",
        col_proy_y = "padron_electoral", col_proy_n = "Proy. Padrón M"
      ),
      lista_h = list(
        col_y = "lista_hombres", nombre = "LNE Hombres",
        color = color_h_loc(ambito), dash = "dot",
        col_proy_y = "lista_proyectada", col_proy_n = "Proy. LNE H"
      ),
      lista_m = list(
        col_y = "lista_mujeres", nombre = "LNE Mujeres",
        color = color_m_loc(ambito), dash = "dot",
        col_proy_y = "lista_proyectada", col_proy_n = "Proy. LNE M"
      )
    )
    
    p <- plot_ly()
    
    for (clave in orden_trazas) {
      cfg <- config_trazas[[clave]]
      
      # Proyección individual por columna
      proy <- NULL
      if (meses_rest > 0) {
        es_padron <- grepl("padron", clave)
        serie_tmp <- serie
        serie_tmp$lista_nominal    <- serie_tmp[[if (es_padron) "lista_hombres" else cfg$col_y]]
        serie_tmp$padron_electoral <- serie_tmp[[if (es_padron) cfg$col_y else "padron_hombres"]]
        if (!es_padron) {
          # Para lista: usar la pareja padron correspondiente
          par_padron <- sub("lista_", "padron_", clave)
          serie_tmp$padron_electoral <- serie_tmp[[config_trazas[[par_padron]]$col_y]]
          serie_tmp$lista_nominal    <- serie_tmp[[cfg$col_y]]
        }
        proy <- tryCatch(
          proyectar_con_tasa_crecimiento(serie_tmp, meses_rest),
          error = function(e) NULL
        )
      }
      
      p <- p %>% add_trace(
        data = serie, x = ~fecha, y = serie[[cfg$col_y]],
        type = "scatter", mode = "lines+markers",
        name = cfg$nombre,
        line   = list(color = cfg$color, width = 2.5, dash = cfg$dash),
        marker = list(size  = 6, color = cfg$color,
                      symbol = if (grepl("lista", clave)) "square" else "circle"),
        hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>",
                               cfg$nombre, ": %{y:,.0f}<extra></extra>")
      )
      
      if (!is.null(proy) && nrow(proy) > 0) {
        col_proy <- if (grepl("padron", clave)) "padron_proyectado" else "lista_proyectada"
        p <- p %>% add_trace(
          data = proy, x = ~fecha, y = proy[[col_proy]],
          type = "scatter", mode = "lines",
          name = cfg$col_proy_n,
          line = list(color = cfg$color, width = 1.5, dash = "dash"),
          showlegend = FALSE,
          hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>",
                                 cfg$col_proy_n, ": %{y:,.0f}<extra></extra>")
        )
      }
    }
    
    # Card No Binario (datos del último corte disponible)
    annotations_s7 <- list(ann_alcance(alcance), ann_fuente())
    ultima_fila <- serie[which.max(serie$fecha), ]
    nb_p <- ultima_fila$padron_no_binario %||% 0
    nb_l <- ultima_fila$lista_no_binario  %||% 0
    
    if (!is.na(nb_p) && nb_p > 0) {
      annotations_s7[[length(annotations_s7) + 1]] <- list(
        text = paste0(
          "<span style='font-size:12px;font-weight:bold;color:#9B59B6;'>⚧ No Binario</span><br>",
          "<span style='font-size:11px;color:#333;'>Padrón: ", fmt_num(nb_p), "</span><br>",
          "<span style='font-size:11px;color:#333;'>LNE: ",    fmt_num(nb_l), "</span><br>",
          "<span style='font-size:10px;color:#888;font-style:italic;'>",
          "(sin proyección)</span>"
        ),
        x = 0.03, y = 0.97, xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "top", showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.95)",
        bordercolor = "#9B59B6", borderwidth = 1.5, borderpad = 8,
        font = list(size = 11, color = "#333", family = "Arial, sans-serif")
      )
    }
    
    p %>% layout(
      title  = list(
        text = paste0("Proyección Semanal — Padrón y LNE por Sexo (", etiq, ") ", anio),
        font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis  = list(
        title = "", type = "date",
        tickformat = "%d %b",
        range = c(min(serie$fecha) - 3, as.Date(paste0(anio, "-12-31")))
      ),
      yaxis  = list(title = "Número de electores", separatethousands = TRUE),
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.22),
      margin = list(t = 110, b = 100, l = 90, r = 40),
      hovermode  = "x unified",
      annotations = annotations_s7
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  message("✅ graficas_semanal_sexo v1.0 inicializado")
  message("   S1: Pirámide por 12 rangos (H vs M)")
  message("   S2/S3/S4: LNE por grupo etario × sexo (3 columnas)")
  message("   S5: Barras Padrón/LNE por sexo")
  message("   S6: Dona tasa inclusión (etiquetas mejoradas)")
  message("   S7: Proyección semanal por sexo + card NB")
}
