# modules/lista_nominal_graficas/graficas_semanal_origen.R
# Vista Semanal — Gráficas de Origen: O1, O2
# Versión: 1.0
#
# Gráficas:
#   O1 (semanal_o1_calor)       — Mapa de calor LNE: eje X = 32 entidades
#                                  receptoras, eje Y = top N entidades de origen
#                                  (34 orígenes posibles: 32 + cod.87 + cod.88).
#                                  Widget inline: selectInput top N.
#                                  Sin checks — 87/88 siempre en el ranking.
#
#   O2 (semanal_o2_proyeccion)  — Líneas de evolución y proyección semanal.
#                                  Una traza LNE + proyección por cada entidad
#                                  del top N seleccionado.
#                                  Widget inline: selectInput top N (default=1)
#                                  + checks para incluir/excluir cod.87 y cod.88.
#
# Dependencias del entorno padre (graficas_semanal.R):
#   COLORES, FUENTE_INE, NOM_ORIGEN, NOM_CORTOS
#   fmt_num(), etiq_ambito(), ann_fuente(), ann_alcance(), plot_vacio()
#   color_lista(), color_padron()
#   es_historico(), desglose_activo()
#   proyectar_con_tasa_crecimiento()   ← de graficas_helpers.R
#
# Dependencias de datos (pasadas como argumentos):
#   datos_semanal_origen()        ← corte único (O1)
#   datos_semanal_serie_origen()  ← serie temporal (O2)
#   anio_semanal(), texto_alcance(), ambito_reactivo(), estado_app()

graficas_semanal_origen <- function(input, output, session,
                                    datos_semanal_origen,
                                    datos_semanal_serie_origen,
                                    anio_semanal,
                                    texto_alcance,
                                    ambito_reactivo,
                                    estado_app) {
  
  message("📊 Inicializando graficas_semanal_origen v1.0")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CONSTANTES LOCALES
  # ══════════════════════════════════════════════════════════════════════════
  
  # Claves de columnas de origen (LNE)
  CLAVES_LN <- c(sprintf("ln_%02d", 1:32), "ln87", "ln88")
  # Claves de columnas de origen (padrón)
  CLAVES_PAD <- c(sprintf("pad_%02d", 1:32), "pad87", "pad88")
  
  # Etiquetas cortas para eje X (entidades receptoras) — usa NOM_CORTOS del padre
  # Etiquetas largas para eje Y (entidades de origen) — usa NOM_ORIGEN del padre
  
  nombre_origen <- function(clave) {
    # clave: "ln_01".."ln_32", "ln87", "ln88"
    k <- gsub("ln_|ln|pad_|pad", "", clave, ignore.case = TRUE)
    k <- sprintf("%02s", trimws(k))
    NOM_ORIGEN[k] %||% paste0("Entidad ", k)
  }
  
  # Paleta de 34 colores distintos para trazas O2
  PALETA_O2_NAC <- colorRampPalette(c("#003E66","#44559B","#AE0E35","#C0311A",
                                      "#9B59B6","#1ABC9C","#E67E22","#2ECC71",
                                      "#E74C3C","#3498DB"))(34)
  PALETA_O2_EXT <- colorRampPalette(c("#8F6A00","#D4A500","#EAC43E","#B3D491",
                                      "#5C9900","#8FB369","#CCE4B1","#F5CA45",
                                      "#D4666C","#6B8F00"))(34)
  
  paleta_o2 <- function(ambito) {
    if (ambito == "extranjero") PALETA_O2_EXT else PALETA_O2_NAC
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPER: extraer columnas de entidades receptoras del CSV de origen
  # El CSV tiene filas por entidad receptora/municipio/sección.
  # Las columnas ln_01..ln_32 + ln87 + ln88 indican cuántos electores
  # residentes en esa fila son originarios de cada estado.
  # ══════════════════════════════════════════════════════════════════════════
  
  # Detecta las columnas ln_NN y pad_NN presentes en df
  detectar_cols_origen <- function(df, tipo = "ln") {
    patron <- if (tipo == "ln")
      "^ln_\\d{2}$|^ln8[78]$"
    else
      "^pad_\\d{2}$|^pad8[78]$"
    cols <- grep(patron, colnames(df), value = TRUE, ignore.case = TRUE)
    if (length(cols) == 0) {
      # Fallback patrón alternativo
      patron2 <- if (tipo == "ln") "^ln_\\d|^lista_\\d" else "^pad_\\d|^padron_\\d{2}"
      cols <- grep(patron2, colnames(df), value = TRUE, ignore.case = TRUE)
    }
    cols
  }
  
  # Agrega el corte único según ámbito: suma todas las filas nacionales
  # (o las RESIDENTES EXTRANJERO para ámbito extranjero)
  agregar_corte_origen <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (!"nombre_entidad" %in% colnames(df)) return(NULL)
    filas <- if (ambito == "extranjero") {
      df[grepl("RESIDENTES EXTRANJERO", toupper(df$nombre_entidad), fixed = TRUE), ]
    } else {
      df[!grepl("RESIDENTES EXTRANJERO|^TOTALES$",
                toupper(trimws(df$nombre_entidad))), ]
    }
    if (nrow(filas) == 0) return(NULL)
    cols_ln  <- detectar_cols_origen(filas, "ln")
    cols_pad <- detectar_cols_origen(filas, "pad")
    cols_num <- union(cols_ln, cols_pad)
    if (length(cols_num) == 0) return(NULL)
    res <- as.data.frame(
      lapply(filas[, cols_num, drop = FALSE],
             function(x) sum(as.numeric(x), na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
    res
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPER: construir tabla de origen ordenada por LNE total descendente
  # Devuelve data.frame con: clave_ln, nombre, lne_total, pad_total
  # ══════════════════════════════════════════════════════════════════════════
  
  tabla_origen_ordenada <- function(df, ambito) {
    fila <- agregar_corte_origen(df, ambito)
    if (is.null(fila)) return(NULL)
    cols_ln <- detectar_cols_origen(fila, "ln")
    if (length(cols_ln) == 0) return(NULL)
    
    res <- do.call(rbind, lapply(cols_ln, function(col) {
      clave_pad <- gsub("^ln", "pad", col, ignore.case = TRUE)
      data.frame(
        clave_ln  = col,
        nombre    = nombre_origen(col),
        lne_total = as.numeric(fila[[col]]),
        pad_total = if (clave_pad %in% colnames(fila))
          as.numeric(fila[[clave_pad]]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    res[order(res$lne_total, decreasing = TRUE, na.last = TRUE), ]
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPER: construir matriz para O1
  # Filas = entidades de origen (top N), columnas = entidades receptoras (32)
  # Valor = LNE de personas del origen_i que residen en la entidad receptora_j
  # ══════════════════════════════════════════════════════════════════════════
  
  construir_matriz_o1 <- function(df, ambito, top_n) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Filas a usar según ámbito
    df_uso <- if (ambito == "extranjero") {
      df[grepl("RESIDENTES EXTRANJERO", toupper(df$nombre_entidad), fixed = TRUE), ]
    } else {
      df[!grepl("RESIDENTES EXTRANJERO|^TOTALES$",
                toupper(trimws(df$nombre_entidad))), ]
    }
    if (nrow(df_uso) == 0) return(NULL)
    
    cols_ln <- detectar_cols_origen(df_uso, "ln")
    if (length(cols_ln) == 0) return(NULL)
    
    # Determinar top N entidades de origen por LNE total
    totales_origen <- colSums(df_uso[, cols_ln, drop = FALSE], na.rm = TRUE)
    totales_origen <- sort(totales_origen, decreasing = TRUE)
    n_sel <- if (top_n == 0) length(totales_origen) else min(top_n, length(totales_origen))
    cols_top <- names(totales_origen)[seq_len(n_sel)]
    
    # Etiquetas eje Y (orígenes)
    etiq_y <- sapply(cols_top, nombre_origen)
    
    # Entidades receptoras: cada fila del df es una entidad/municipio/sección.
    # Para O1 necesitamos agrupar por entidad receptora (nombre_entidad).
    # Excluir filas no-geográficas
    df_geo <- df_uso[!grepl("^TOTALES$", toupper(trimws(df_uso$nombre_entidad))), ]
    
    # Obtener entidades receptoras únicas en orden por cve_entidad
    if ("cve_entidad" %in% colnames(df_geo)) {
      entidades <- unique(df_geo[, c("cve_entidad","nombre_entidad")])
      entidades <- entidades[order(as.integer(entidades$cve_entidad)), ]
    } else {
      entidades <- data.frame(
        cve_entidad  = seq_len(32),
        nombre_entidad = unique(df_geo$nombre_entidad)[seq_len(
          min(32, length(unique(df_geo$nombre_entidad))))],
        stringsAsFactors = FALSE
      )
    }
    
    # Etiquetas eje X (receptoras): nombre corto
    etiq_x <- sapply(entidades$cve_entidad, function(cve) {
      k <- sprintf("%02d", as.integer(cve))
      NOM_CORTOS[k] %||% paste0("E", k)
    })
    
    # Construir matriz: para cada entidad receptora, sumar las filas de esa entidad
    mat <- matrix(0, nrow = n_sel, ncol = nrow(entidades),
                  dimnames = list(etiq_y, etiq_x))
    
    for (j in seq_len(nrow(entidades))) {
      ent <- entidades$nombre_entidad[j]
      filas_ent <- df_geo[toupper(trimws(df_geo$nombre_entidad)) ==
                            toupper(trimws(ent)), ]
      if (nrow(filas_ent) == 0) next
      for (i in seq_along(cols_top)) {
        col <- cols_top[i]
        if (col %in% colnames(filas_ent)) {
          mat[i, j] <- sum(as.numeric(filas_ent[[col]]), na.rm = TRUE)
        }
      }
    }
    list(mat = mat, etiq_y = etiq_y, etiq_x = etiq_x)
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # O1 — Widget: selector top N (dentro de la gráfica)
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_o1_topn_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    div(
      style = paste(
        "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;",
        "padding:8px 14px;margin-bottom:10px;",
        "display:flex;align-items:center;gap:16px;"
      ),
      tags$span(
        style = "font-size:13px;font-weight:600;color:#2c3e50;white-space:nowrap;",
        icon("list-ol"), " Top estados de origen:"
      ),
      div(
        style = "width:110px;margin-bottom:0;",
        selectInput(
          inputId  = session$ns("semanal_o1_top_n"),
          label    = NULL,
          choices  = c("Top 5" = "5", "Top 10" = "10",
                       "Top 15" = "15", "Todos" = "0"),
          selected = "10",
          width    = "100%"
        )
      )
    )
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # O1 — Mapa de calor: eje X = entidades receptoras, eje Y = top N orígenes
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_o1_calor <- renderPlotly({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    datos  <- datos_semanal_origen()
    if (is.null(datos)) return(plot_vacio())
    
    top_n  <- suppressWarnings(as.integer(input$semanal_o1_top_n %||% "10"))
    if (is.na(top_n)) top_n <- 10L
    
    res <- construir_matriz_o1(datos, ambito, top_n)
    if (is.null(res)) return(plot_vacio("Sin datos de origen"))
    
    mat    <- res$mat
    etiq_y <- res$etiq_y
    etiq_x <- res$etiq_x
    
    cs <- if (ambito == "extranjero")
      list(c(0,"#FFF9E6"), c(0.5,"#EAC43E"), c(1,"#8F6A00"))
    else
      list(c(0,"#E8EDF8"), c(0.5,"#44559B"), c(1,"#1A2654"))
    
    # Altura dinámica según número de orígenes
    alto_px <- max(320, nrow(mat) * 28 + 120)
    
    plot_ly(
      z = mat,
      x = colnames(mat),
      y = rownames(mat),
      type        = "heatmap",
      colorscale  = cs,
      showscale   = TRUE,
      hovertemplate = paste0(
        "Origen: <b>%{y}</b><br>",
        "Receptor: <b>%{x}</b><br>",
        "LNE: %{z:,.0f}<extra></extra>"
      )
    ) %>%
      layout(
        title  = list(
          text = paste0("LNE por Entidad de Origen y Receptora (",
                        etiq, ") ", anio),
          font = list(size = 16, color = "#333",
                      family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        xaxis  = list(
          title    = "Entidad Receptora",
          tickangle = -40,
          tickfont = list(size = 10)
        ),
        yaxis  = list(
          title    = "Entidad de Origen",
          tickfont = list(size = 11),
          autorange = "reversed"        # origen con más LNE arriba
        ),
        margin = list(t = 120, b = 110, l = 200, r = 60),
        annotations = list(ann_alcance(alcance, y_pos = 1.10), ann_fuente())
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      input$semanal_o1_top_n,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # O2 — Widget: top N + checks 87/88 (dentro de la gráfica)
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_o2_controles_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    div(
      style = paste(
        "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;",
        "padding:10px 14px 6px 14px;margin-bottom:10px;"
      ),
      div(
        style = "display:flex;align-items:flex-start;gap:24px;flex-wrap:wrap;",
        # Selector top N
        div(
          style = "display:flex;align-items:center;gap:10px;",
          tags$span(
            style = "font-size:13px;font-weight:600;color:#2c3e50;white-space:nowrap;",
            icon("chart-line"), " Top entidades de origen:"
          ),
          div(
            style = "width:110px;margin-bottom:0;",
            selectInput(
              inputId  = session$ns("semanal_o2_top_n"),
              label    = NULL,
              choices  = c("Top 1" = "1", "Top 5" = "5", "Top 10" = "10",
                           "Top 15" = "15", "Todos" = "0"),
              selected = "1",
              width    = "100%"
            )
          )
        ),
        # Checks 87/88
        div(
          style = "display:flex;gap:20px;align-items:center;padding-top:4px;",
          checkboxInput(
            inputId = session$ns("semanal_o2_incl_87"),
            label   = tags$span(
              style = "font-size:12px;color:#444;",
              "Mexicanos nacidos en el extranjero"
            ),
            value   = TRUE
          ),
          checkboxInput(
            inputId = session$ns("semanal_o2_incl_88"),
            label   = tags$span(
              style = "font-size:12px;color:#444;",
              "Ciudadanos naturalizados"
            ),
            value   = TRUE
          )
        )
      )
    )
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # O2 — Gráfica: proyección por entidad de origen
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_o2_proyeccion <- renderPlotly({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    serie <- datos_semanal_serie_origen()
    if (is.null(serie) || nrow(serie) < 2) {
      return(plot_vacio("Sin datos de serie temporal para proyección"))
    }
    
    # Parámetros del widget
    top_n    <- suppressWarnings(as.integer(input$semanal_o2_top_n %||% "1"))
    if (is.na(top_n)) top_n <- 1L
    incl_87  <- isTRUE(input$semanal_o2_incl_87)
    incl_88  <- isTRUE(input$semanal_o2_incl_88)
    
    # Columnas LN disponibles en la serie
    cols_ln_serie <- grep("^ln_\\d{2}$|^ln8[78]$", colnames(serie),
                          value = TRUE, ignore.case = TRUE)
    if (length(cols_ln_serie) == 0)
      cols_ln_serie <- grep("^ln_\\d|^lista_\\d", colnames(serie),
                            value = TRUE, ignore.case = TRUE)
    
    # Aplicar filtros 87/88
    if (!incl_87) cols_ln_serie <- cols_ln_serie[!grepl("87$", cols_ln_serie)]
    if (!incl_88) cols_ln_serie <- cols_ln_serie[!grepl("88$", cols_ln_serie)]
    
    if (length(cols_ln_serie) == 0)
      return(plot_vacio("Sin columnas de origen disponibles"))
    
    # Calcular top N usando el último corte de la serie
    ultima_fila  <- serie[which.max(serie$fecha), cols_ln_serie, drop = FALSE]
    totales_ult  <- unlist(ultima_fila)
    totales_ord  <- sort(totales_ult, decreasing = TRUE)
    n_sel        <- if (top_n == 0) length(totales_ord) else min(top_n, length(totales_ord))
    cols_top     <- names(totales_ord)[seq_len(n_sel)]
    
    message("📊 [O2] top_n=", top_n, " | n_sel=", n_sel,
            " | incl_87=", incl_87, " | incl_88=", incl_88)
    
    # Proyección
    ultima_fecha <- max(serie$fecha)
    ultimo_mes   <- as.integer(format(ultima_fecha, "%m"))
    meses_rest   <- 12L - ultimo_mes
    
    colores <- paleta_o2(ambito)
    p       <- plot_ly()
    
    for (i in seq_along(cols_top)) {
      col    <- cols_top[i]
      color  <- colores[((i - 1) %% length(colores)) + 1]
      nombre <- nombre_origen(col)
      
      # Proyección individual
      proy <- NULL
      if (meses_rest > 0) {
        serie_tmp                  <- serie
        serie_tmp$lista_nominal    <- serie_tmp[[col]]
        serie_tmp$padron_electoral <- serie_tmp[[col]]  # aproximación para tasa
        proy <- tryCatch(
          proyectar_con_tasa_crecimiento(serie_tmp, meses_rest),
          error = function(e) NULL
        )
      }
      
      p <- p %>% add_trace(
        data  = serie,
        x     = ~fecha,
        y     = serie[[col]],
        type  = "scatter", mode = "lines+markers",
        name  = nombre,
        line   = list(color = color, width = 2.5),
        marker = list(size = 6, color = color),
        hovertemplate = paste0(
          "<b>%{x|%d %b %Y}</b><br>", nombre, ": %{y:,.0f}<extra></extra>"
        )
      )
      
      if (!is.null(proy) && nrow(proy) > 0) {
        p <- p %>% add_trace(
          data  = proy,
          x     = ~fecha,
          y     = ~lista_proyectada,
          type  = "scatter", mode = "lines",
          name  = paste0("Proy. ", nombre),
          line  = list(color = color, width = 1.5, dash = "dash"),
          showlegend = FALSE,
          hovertemplate = paste0(
            "<b>%{x|%d %b %Y}</b><br>Proy. ", nombre,
            ": %{y:,.0f}<extra></extra>"
          )
        )
      }
    }
    
    # Título dinámico según top N
    titulo_graf <- if (n_sel == 1)
      paste0("Proyección Semanal — Top 1 Entidad de Origen (", etiq, ") ", anio)
    else
      paste0("Proyección Semanal — Top ", n_sel,
             " Entidades de Origen (", etiq, ") ", anio)
    
    p %>% layout(
      title  = list(
        text = titulo_graf,
        font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis  = list(
        title = "", type = "date",
        tickformat = "%d %b",
        range = c(min(serie$fecha) - 3,
                  as.Date(paste0(anio, "-12-31")))
      ),
      yaxis  = list(title = "Lista Nominal Electoral",
                    separatethousands = TRUE),
      legend = list(orientation = "h", xanchor = "center",
                    x = 0.5, y = -0.22),
      margin = list(t = 110, b = 100, l = 90, r = 40),
      hovermode   = "x unified",
      annotations = list(ann_alcance(alcance), ann_fuente())
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      input$semanal_o2_top_n,
      input$semanal_o2_incl_87,
      input$semanal_o2_incl_88,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  message("✅ graficas_semanal_origen v1.0 inicializado")
  message("   O1: mapa de calor LNE (origen × receptor) con top N selector")
  message("   O2: proyección semanal por entidad de origen con checks 87/88")
}
