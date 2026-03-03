# modules/lista_nominal_graficas/graficas_semanal_edad.R
# Vista Semanal — Gráficas de Edad: E1, E2, E3
# Versión: 1.0
#
# Gráficas:
#   E1 (semanal_e1_proyeccion)  — Serie temporal + proyección por rango de edad
#                                  Widget inline de selección de rangos + botón Restablecer
#   E2 (semanal_e2_grupos)      — LNE por grupos etarios agregados
#                                  (Jóvenes 18-29 / Adultos 30-59 / Mayores 60+)
#   E3 (semanal_e3_barras)      — Padrón y LNE por rango individual (barras agrupadas)
#
# Dependencias del entorno padre (graficas_semanal.R):
#   COLORES, ORDEN_EDAD, FUENTE_INE
#   fmt_num(), fmt_pct(), etiq_ambito(), etiqueta_edad()
#   ann_fuente(), ann_alcance(), plot_vacio()
#   color_padron(), color_lista()
#   es_historico(), desglose_activo()
#   construir_df_edad()   ← usa datos del corte único (E2, E3)
#
# Dependencias de datos (pasadas como argumentos):
#   datos_semanal_edad()       ← corte único (E2, E3)
#   datos_semanal_serie_edad() ← serie temporal (E1)
#   anio_semanal(), fecha_semanal_efectiva()
#   texto_alcance(), ambito_reactivo(), estado_app()

graficas_semanal_edad <- function(input, output, session,
                                  datos_semanal_edad,
                                  datos_semanal_serie_edad,
                                  anio_semanal,
                                  fecha_semanal_efectiva,
                                  texto_alcance,
                                  ambito_reactivo,
                                  estado_app) {
  
  message("📊 Inicializando graficas_semanal_edad v1.0")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CONSTANTES LOCALES
  # ══════════════════════════════════════════════════════════════════════════
  
  RANGOS_EDAD <- c("18","19","20_24","25_29","30_34","35_39",
                   "40_44","45_49","50_54","55_59","60_64","65_y_mas")
  
  ETIQ_RANGOS <- c(
    "18"      = "18 años",
    "19"      = "19 años",
    "20_24"   = "20–24 años",
    "25_29"   = "25–29 años",
    "30_34"   = "30–34 años",
    "35_39"   = "35–39 años",
    "40_44"   = "40–44 años",
    "45_49"   = "45–49 años",
    "50_54"   = "50–54 años",
    "55_59"   = "55–59 años",
    "60_64"   = "60–64 años",
    "65_y_mas"= "65+ años"
  )
  
  # Paleta de 12 colores para trazas por rango
  # Nacional: azules → rojos; Extranjero: dorados → verdes
  PALETA_NAC <- c(
    "#003E66","#005F99","#1A7ABF","#4A90E2",  # azules
    "#44559B","#6B6BB3","#9B59B6","#C0392B",  # violetas → rojo
    "#AE0E35","#D4666C","#E88080","#F5B7B1"   # rojos/rosados
  )
  PALETA_EXT <- c(
    "#8F6A00","#B38A00","#D4A500","#EAC43E",  # dorados oscuros → claros
    "#6B8F00","#8FB369","#B3D491","#CCE4B1",  # verdes oscuros → claros
    "#5C9900","#7FBF3F","#A8D870","#C8ECA0"   # verdes medios
  )
  
  paleta_rangos <- function(ambito) if (ambito == "extranjero") PALETA_EXT else PALETA_NAC
  
  # Grupos etarios para E2
  GRUPOS_ETARIOS <- list(
    "Jóvenes\n(18–29)"    = c("18","19","20_24","25_29"),
    "Adultos\n(30–59)"    = c("30_34","35_39","40_44","45_49","50_54","55_59"),
    "Mayores\n(60+)"      = c("60_64","65_y_mas")
  )
  COLOR_GRUPOS_NAC <- c("#003E66","#44559B","#AE0E35")
  COLOR_GRUPOS_EXT <- c("#8F6A00","#D4A500","#B3D491")
  
  # ══════════════════════════════════════════════════════════════════════════
  # E1 — WIDGET: selector de rangos de edad
  # Renderiza el checkboxGroupInput + botón Restablecer que aparece
  # sobre/dentro de la gráfica E1 (vía uiOutput en graficas_ui_render.R)
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e1_rangos_ui <- renderUI({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    tagList(
      div(
        style = paste(
          "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;",
          "padding:10px 14px 6px 14px;margin-bottom:10px;"
        ),
        div(
          style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:6px;",
          tags$span(
            style = "font-size:13px;font-weight:600;color:#2c3e50;",
            icon("filter"), " Rango de Edad"
          ),
          actionButton(
            inputId = session$ns("semanal_e1_btn_reset"),
            label   = "Restablecer",
            icon    = icon("undo"),
            class   = "btn btn-xs btn-outline-secondary",
            style   = "font-size:11px;padding:2px 8px;"
          )
        ),
        checkboxGroupInput(
          inputId  = session$ns("semanal_e1_rangos"),
          label    = NULL,
          choices  = setNames(RANGOS_EDAD, ETIQ_RANGOS[RANGOS_EDAD]),
          selected = RANGOS_EDAD,
          inline   = TRUE
        )
      )
    )
  })
  
  # Botón Restablecer → seleccionar todos los rangos
  observeEvent(input$semanal_e1_btn_reset, {
    updateCheckboxGroupInput(
      session  = session,
      inputId  = "semanal_e1_rangos",
      selected = RANGOS_EDAD
    )
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # ══════════════════════════════════════════════════════════════════════════
  # E1 — GRÁFICA: proyección por rango de edad
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e1_proyeccion <- renderPlotly({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    serie <- datos_semanal_serie_edad()
    if (is.null(serie) || nrow(serie) < 2) {
      return(plot_vacio("Sin datos de serie temporal para proyección"))
    }
    
    # Rangos seleccionados por el usuario (default = todos)
    rangos_sel <- input$semanal_e1_rangos %||% RANGOS_EDAD
    if (length(rangos_sel) == 0) rangos_sel <- RANGOS_EDAD
    todos_seleccionados <- length(rangos_sel) == length(RANGOS_EDAD) ||
      setequal(rangos_sel, RANGOS_EDAD)
    
    message("📊 [E1] Rangos seleccionados: ", paste(rangos_sel, collapse=", "),
            " | Ámbito: ", ambito)
    
    # Calcular meses restantes para proyección
    ultima_fecha  <- max(serie$fecha)
    ultimo_mes    <- as.integer(format(ultima_fecha, "%m"))
    meses_rest    <- 12L - ultimo_mes
    
    colores <- paleta_rangos(ambito)
    p <- plot_ly()
    
    if (todos_seleccionados) {
      # ── Modo total: 2 trazas (padrón total + lista total) ──────────────────
      
      # Proyección de totales
      proy <- NULL
      if (meses_rest > 0) {
        serie_proy          <- serie
        serie_proy$lista_nominal   <- serie_proy$lista_total
        serie_proy$padron_electoral <- serie_proy$padron_total
        proy <- proyectar_con_tasa_crecimiento(serie_proy, meses_rest)
      }
      
      p <- p %>%
        add_trace(
          data = serie, x = ~fecha, y = ~padron_total,
          type = "scatter", mode = "lines+markers", name = "Padrón Total",
          line   = list(color = color_padron(ambito), width = 3),
          marker = list(size = 7, color = color_padron(ambito)),
          hovertemplate = "<b>%{x|%d %b %Y}</b><br>Padrón Total: %{y:,.0f}<extra></extra>"
        ) %>%
        add_trace(
          data = serie, x = ~fecha, y = ~lista_total,
          type = "scatter", mode = "lines+markers", name = "LNE Total",
          line   = list(color = color_lista(ambito), width = 3),
          marker = list(size = 7, color = color_lista(ambito)),
          hovertemplate = "<b>%{x|%d %b %Y}</b><br>LNE Total: %{y:,.0f}<extra></extra>"
        )
      
      if (!is.null(proy) && nrow(proy) > 0) {
        p <- p %>%
          add_trace(
            data = proy, x = ~fecha, y = ~padron_proyectado,
            type = "scatter", mode = "lines", name = "Proyección Padrón",
            line = list(color = color_padron(ambito), width = 2, dash = "dash"),
            hovertemplate = "<b>%{x|%d %b %Y}</b><br>Proy. Padrón: %{y:,.0f}<extra></extra>"
          ) %>%
          add_trace(
            data = proy, x = ~fecha, y = ~lista_proyectada,
            type = "scatter", mode = "lines", name = "Proyección LNE",
            line = list(color = color_lista(ambito), width = 2, dash = "dash"),
            hovertemplate = "<b>%{x|%d %b %Y}</b><br>Proy. LNE: %{y:,.0f}<extra></extra>"
          )
      }
      
      titulo_graf <- paste0("Proyección Semanal — Padrón y LNE Total (", etiq, ") ", anio)
      
    } else {
      # ── Modo por rango: 2 trazas por rango seleccionado ───────────────────
      
      for (i in seq_along(rangos_sel)) {
        rango  <- rangos_sel[i]
        color  <- colores[((i - 1) %% length(colores)) + 1]
        etiq_r <- ETIQ_RANGOS[rango] %||% rango
        
        col_pad <- paste0("padron_", rango)
        col_lst <- paste0("lista_",  rango)
        
        if (!col_pad %in% colnames(serie)) next
        
        # Proyección por rango
        proy_r <- NULL
        if (meses_rest > 0) {
          serie_r                   <- serie
          serie_r$lista_nominal     <- serie_r[[col_lst]]
          serie_r$padron_electoral  <- serie_r[[col_pad]]
          proy_r <- proyectar_con_tasa_crecimiento(serie_r, meses_rest)
        }
        
        p <- p %>%
          add_trace(
            data = serie, x = ~fecha, y = serie[[col_pad]],
            type = "scatter", mode = "lines+markers",
            name = paste0("Padrón ", etiq_r),
            line   = list(color = color, width = 2.5),
            marker = list(size = 6, color = color),
            hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Padrón ", etiq_r,
                                   ": %{y:,.0f}<extra></extra>")
          ) %>%
          add_trace(
            data = serie, x = ~fecha, y = serie[[col_lst]],
            type = "scatter", mode = "lines+markers",
            name = paste0("LNE ", etiq_r),
            line   = list(color = color, width = 2.5, dash = "dot"),
            marker = list(size = 6, color = color, symbol = "square"),
            hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>LNE ", etiq_r,
                                   ": %{y:,.0f}<extra></extra>")
          )
        
        if (!is.null(proy_r) && nrow(proy_r) > 0) {
          p <- p %>%
            add_trace(
              data = proy_r, x = ~fecha, y = ~padron_proyectado,
              type = "scatter", mode = "lines",
              name = paste0("Proy. Padrón ", etiq_r),
              line = list(color = color, width = 1.5, dash = "dash"),
              showlegend = FALSE,
              hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Proy. Padrón ",
                                     etiq_r, ": %{y:,.0f}<extra></extra>")
            ) %>%
            add_trace(
              data = proy_r, x = ~fecha, y = ~lista_proyectada,
              type = "scatter", mode = "lines",
              name = paste0("Proy. LNE ", etiq_r),
              line = list(color = color, width = 1.5, dash = "dashdot"),
              showlegend = FALSE,
              hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Proy. LNE ",
                                     etiq_r, ": %{y:,.0f}<extra></extra>")
            )
        }
      }
      
      titulo_graf <- paste0(
        "Proyección Semanal — Rangos de Edad (", etiq, ") ", anio
      )
    }
    
    # Eje X: fechas reales + proyectadas
    fechas_reales <- serie$fecha
    fechas_eje    <- fechas_reales
    if (meses_rest > 0) {
      anio_chr  <- as.character(anio)
      dic_31    <- as.Date(paste0(anio_chr, "-12-31"))
      fechas_eje <- c(fechas_reales, seq(max(fechas_reales) + 7, dic_31, by = "week"))
    }
    
    p %>% layout(
      title  = list(
        text = titulo_graf,
        font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis  = list(
        title = "", type = "date",
        tickformat = "%d %b",
        range = c(min(fechas_reales) - 3,
                  as.Date(paste0(anio, "-12-31")))
      ),
      yaxis  = list(title = "Número de electores", separatethousands = TRUE),
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.22),
      margin = list(t = 110, b = 100, l = 90, r = 40),
      hovermode  = "x unified",
      annotations = list(ann_alcance(alcance), ann_fuente())
    )
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      input$semanal_e1_rangos,
      ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # E2 — GRÁFICA: LNE por grupos etarios (Jóvenes / Adultos / Mayores)
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e2_grupos <- renderPlotly({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    datos <- datos_semanal_edad()
    if (is.null(datos)) return(plot_vacio())
    
    df_rango <- construir_df_edad(datos, ambito)
    if (is.null(df_rango) || nrow(df_rango) == 0) {
      return(plot_vacio("Sin datos de grupos de edad"))
    }
    
    # Agregar por grupos etarios (solo LNE, sin distinción de sexo)
    nombres_grupos <- names(GRUPOS_ETARIOS)
    colores_grupos <- if (ambito == "extranjero") COLOR_GRUPOS_EXT else COLOR_GRUPOS_NAC
    
    df_grupos <- do.call(rbind, lapply(seq_along(GRUPOS_ETARIOS), function(i) {
      grupo_nombre <- nombres_grupos[i]
      rangos       <- GRUPOS_ETARIOS[[i]]
      etiq_rangos  <- sapply(rangos, etiqueta_edad)
      
      filas <- df_rango[df_rango$grupo %in% etiq_rangos, ]
      lne   <- sum(filas$lista_hombres + filas$lista_mujeres, na.rm = TRUE)
      
      data.frame(
        grupo  = grupo_nombre,
        lne    = lne,
        color  = colores_grupos[i],
        stringsAsFactors = FALSE
      )
    }))
    
    # Calcular porcentajes
    total_lne    <- sum(df_grupos$lne, na.rm = TRUE)
    df_grupos$pct <- if (total_lne > 0) round(df_grupos$lne / total_lne * 100, 1) else 0
    
    plot_ly(
      data        = df_grupos,
      y           = ~factor(grupo, levels = rev(nombres_grupos)),
      x           = ~lne,
      type        = "bar",
      orientation = "h",
      marker      = list(color = ~color),
      text        = ~paste0(format(lne, big.mark = ","), "  (", pct, "%)"),
      textposition = "outside",
      hovertemplate = "<b>%{y}</b><br>LNE: %{x:,.0f}<extra></extra>"
    ) %>%
      layout(
        title  = list(
          text = paste0("LNE por Grupo Etario (", etiq, ") ", anio),
          font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        xaxis  = list(
          title = "Lista Nominal Electoral",
          separatethousands = TRUE,
          # Espacio para etiquetas externas
          range = c(0, max(df_grupos$lne, na.rm = TRUE) * 1.25)
        ),
        yaxis  = list(title = ""),
        legend = list(showlegend = FALSE),
        margin = list(t = 110, b = 80, l = 140, r = 60),
        annotations = list(ann_alcance(alcance), ann_fuente())
      )
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # E3 — GRÁFICA: Padrón y LNE por rango individual (barras agrupadas)
  # Equivale a semanal_edad_distribucion de graficas_semanal.R v2.3
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e3_barras <- renderPlotly({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    datos <- datos_semanal_edad()
    if (is.null(datos)) return(plot_vacio())
    
    df <- construir_df_edad(datos, ambito)
    if (is.null(df) || nrow(df) == 0) return(plot_vacio("Sin datos de edad"))
    
    df$padron_total <- df$padron_hombres + df$padron_mujeres
    df$lista_total  <- df$lista_hombres  + df$lista_mujeres
    
    plot_ly() %>%
      add_trace(
        data = df, x = ~grupo, y = ~padron_total,
        type = "bar", name = "Padrón Electoral",
        marker = list(color = color_padron(ambito)),
        hovertemplate = "<b>%{x}</b><br>Padrón: %{y:,.0f}<extra></extra>"
      ) %>%
      add_trace(
        data = df, x = ~grupo, y = ~lista_total,
        type = "bar", name = "Lista Nominal",
        marker = list(color = color_lista(ambito)),
        hovertemplate = "<b>%{x}</b><br>LNE: %{y:,.0f}<extra></extra>"
      ) %>%
      layout(
        title   = list(
          text = paste0("Padrón y LNE por Rango de Edad (", etiq, ") ", anio),
          font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        barmode = "group",
        xaxis   = list(title = "Rango de Edad", tickangle = -30),
        yaxis   = list(title = "Número de electores", separatethousands = TRUE),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.22),
        margin  = list(t = 110, b = 100, l = 90, r = 40),
        annotations = list(ann_alcance(alcance), ann_fuente())
      )
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  message("✅ graficas_semanal_edad v1.0 inicializado")
  message("   E1: proyección por rango con widget selector")
  message("   E2: LNE por grupos etarios (Jóvenes / Adultos / Mayores)")
  message("   E3: Padrón y LNE por rango individual (barras agrupadas)")
}
