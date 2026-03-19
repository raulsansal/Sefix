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
  COLOR_H_NAC  <- "#277592"; COLOR_H_EXT  <- "#D4A500"
  COLOR_M_NAC  <- "#D10F3F"; COLOR_M_EXT  <- "#8FB369"
  COLOR_NB_NAC <- "#9B59B6"; COLOR_NB_EXT <- "#5C9900"
  
  color_h_loc  <- function(a) if (a == "extranjero") COLOR_H_EXT  else COLOR_H_NAC
  color_m_loc  <- function(a) if (a == "extranjero") COLOR_M_EXT  else COLOR_M_NAC
  color_nb_loc <- function(a) if (a == "extranjero") COLOR_NB_EXT else COLOR_NB_NAC

  # ── Helper: formato de fecha en español ───────────────────────────────────
  fecha_es <- function(d) {
    meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
               "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
    paste(as.integer(format(d, "%d")), meses[as.integer(format(d, "%m"))], format(d, "%Y"))
  }
  
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

    # Fecha del último corte disponible (mismo patrón que E1/E2 de Edad)
    serie_s    <- datos_semanal_serie_sexo()
    etiq_fecha <- if (!is.null(serie_s) && nrow(serie_s) > 0)
      fecha_es(max(serie_s$fecha))
    else as.character(anio)

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
          text = paste0("Pir\u00e1mide de LNE por Rango de Edad y Sexo \u2013 ",
                        etiq_fecha, " \u2013 ", etiq),
          font = list(size = 17, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        barmode = "overlay", bargap = 0.1,
        xaxis   = list(
          title = "",
          tickformat = ",.0f",
          zeroline = TRUE, zerolinecolor = "#999", zerolinewidth = 1.5
        ),
        yaxis   = list(title = "", categoryorder = "array",
                       categoryarray = niveles_y),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.10),
        margin  = list(t = 120, b = 80, l = 90, r = 40),
        hovermode = "y unified",
        annotations = list(ann_alcance(alcance, y_pos = 1.10))
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # S2 — Barras horizontales agrupadas: LNE por Grupo Etario × Sexo
  # Reemplaza las antiguas S2/S3/S4 (una por sexo) con un solo chart ancho
  # Widget: checkboxes H/M/NB + botón Restablecer (patrón E3 de edad)
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_s2_widget_ui <- renderUI({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    tagList(
      tags$style(HTML(paste0(
        "#", session$ns("semanal_s2_sexos"),
        " .shiny-options-group { display:flex; flex-wrap:nowrap; gap:4px 0; }",
        "#", session$ns("semanal_s2_sexos"),
        " .shiny-options-group .checkbox { margin:0; padding:1px 12px 1px 4px; box-sizing:border-box; }",
        "#", session$ns("semanal_s2_sexos"),
        " label { font-size:12px; color:#333; cursor:pointer; }",
        "#", session$ns("semanal_s2_sexos"),
        " input[type='checkbox'] { accent-color:#44559B; }"
      ))),
      div(
        style = "display:flex;justify-content:center;align-items:center;padding:6px 0;position:relative;",
        checkboxGroupInput(
          session$ns("semanal_s2_sexos"), label = NULL,
          choices  = c("Hombres" = "hombres", "Mujeres" = "mujeres", "No Binario" = "no_binario"),
          selected = c("hombres", "mujeres", "no_binario"),
          inline   = TRUE
        ),
        div(
          style = "position:absolute;right:0;",
          actionButton(
            session$ns("semanal_s2_btn_reset"), label = "Restablecer",
            icon  = icon("rotate-left"), class = "btn btn-sm",
            style = "background:#6c757d;color:#fff;border:none;width:110px;padding:4px 10px;border-radius:4px;"
          )
        )
      )
    )
  })

  observeEvent(input$semanal_s2_btn_reset, {
    updateCheckboxGroupInput(session, "semanal_s2_sexos",
                             selected = c("hombres", "mujeres", "no_binario"))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$semanal_s2_barras <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)

    ambito    <- ambito_reactivo()
    alcance   <- isolate(texto_alcance())
    anio      <- anio_semanal()
    etiq      <- etiq_ambito(ambito)

    serie_s    <- datos_semanal_serie_sexo()
    etiq_fecha <- if (!is.null(serie_s) && nrow(serie_s) > 0)
      fecha_es(max(serie_s$fecha)) else as.character(anio)

    sexos_sel <- input$semanal_s2_sexos %||% c("hombres", "mujeres", "no_binario")
    if (length(sexos_sel) == 0) sexos_sel <- c("hombres", "mujeres", "no_binario")

    datos <- datos_semanal_edad()
    if (is.null(datos)) return(plot_vacio())

    # Calcular los 3 df siempre (escala X estable aunque se filtre)
    df_h  <- construir_df_grupos_sexo(datos, ambito, "hombres")
    df_m  <- construir_df_grupos_sexo(datos, ambito, "mujeres")
    df_nb <- construir_df_grupos_sexo(datos, ambito, "no_binario")

    # Escala X unificada sobre el máximo de H y M
    x_max <- max(
      if (!is.null(df_h)) max(df_h$lne, na.rm = TRUE) else 0,
      if (!is.null(df_m)) max(df_m$lne, na.rm = TRUE) else 0,
      na.rm = TRUE
    ) * 1.30

    config_sexos <- list(
      hombres    = list(df = df_h,  nombre = "Hombres",    color = color_h_loc(ambito)),
      mujeres    = list(df = df_m,  nombre = "Mujeres",    color = color_m_loc(ambito)),
      no_binario = list(df = df_nb, nombre = "No Binario", color = color_nb_loc(ambito))
    )

    p <- plot_ly()

    for (sex_key in c("hombres", "mujeres", "no_binario")) {
      cfg <- config_sexos[[sex_key]]
      if (is.null(cfg$df) || nrow(cfg$df) == 0) next

      df_s  <- cfg$df
      total <- sum(df_s$lne, na.rm = TRUE)
      df_s$pct   <- if (total > 0) round(df_s$lne / total * 100, 1) else 0
      df_s$texto <- paste0(format(df_s$lne, big.mark = ","), "  (", df_s$pct, "%)")

      # visible=TRUE si seleccionado, "legendonly" si no (mantiene escala X)
      visible_val <- if (sex_key %in% sexos_sel) TRUE else "legendonly"

      p <- p %>% add_trace(
        data = df_s,
        y    = ~factor(grupo, levels = rev(NOMBRES_GRUPOS)),
        x    = ~lne,
        type = "bar", orientation = "h",
        name   = cfg$nombre,
        marker = list(color = cfg$color, line = list(color = "#fff", width = 0.5)),
        text   = ~texto,
        textposition = "outside",
        cliponaxis   = FALSE,
        visible      = visible_val,
        hovertemplate = paste0("<b>%{y}</b><br>", cfg$nombre, ": %{x:,.0f}<extra></extra>")
      )
    }

    # Card NB de respaldo: si datos de edad dan NB=0 pero datos de sexo dan NB>0,
    # mostrar la cifra real en una card (los archivos de edad no siempre contienen
    # la desagregación NB por rango de edad para geografías pequeñas).
    annotations_s2 <- list(ann_alcance(alcance, y_pos = 1.10))
    total_nb_edad <- if (!is.null(df_nb)) sum(df_nb$lne, na.rm = TRUE) else 0
    if (total_nb_edad == 0 && "no_binario" %in% sexos_sel) {
      tot_sexo <- tryCatch(
        extraer_totales_sexo(datos_semanal_sexo(), ambito),
        error = function(e) NULL
      )
      if (!is.null(tot_sexo)) {
        nb_p_s <- tot_sexo$padron_no_binario %||% 0
        nb_l_s <- tot_sexo$lista_no_binario  %||% 0
        if (!is.na(nb_p_s) && nb_p_s > 0) {
          texto_nb_s2 <- paste0(
            "<span style='font-size:12px;font-weight:bold;color:#9B59B6;'>\u26a7 No Binario</span><br>",
            "<span style='font-size:11px;color:#333;'>Padr\u00f3n: ", fmt_num(nb_p_s), "</span><br>",
            "<span style='font-size:11px;color:#333;'>LNE: ", fmt_num(nb_l_s), "</span>"
          )
          annotations_s2[[length(annotations_s2) + 1]] <- list(
            text = texto_nb_s2,
            x = 0.03, y = 0.97, xref = "paper", yref = "paper",
            xanchor = "left", yanchor = "top", showarrow = FALSE,
            bgcolor = "rgba(255,255,255,0.95)",
            bordercolor = "#9B59B6", borderwidth = 1.5, borderpad = 8,
            font = list(size = 11, color = "#333", family = "Arial, sans-serif")
          )
        }
      }
    }

    p %>% layout(
      title   = list(
        text = paste0("LNE por Grupo Etario y Sexo \u2013 ", etiq_fecha, " \u2013 ", etiq),
        font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      barmode = "group",
      xaxis   = list(title = "", separatethousands = TRUE, range = c(0, x_max)),
      yaxis   = list(title = ""),
      legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.10),
      margin  = list(t = 120, b = 80, l = 90, r = 40),
      hovermode   = "closest",
      annotations = annotations_s2
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, input$semanal_s2_sexos, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVE: datos_serie_sexo_efectiva()
  #
  # Punto único de acceso a la serie temporal de sexo para S3.
  # Decide qué fuente usar según el estado y los filtros activos:
  #
  #   · "restablecido" o filtros nacionales:
  #       → datos_semanal_serie_sexo()  (pre-cargada en caché, instantáneo)
  #
  #   · "consultado" con filtros geográficos:
  #       → calcula la serie al vuelo descargando cada semana con los filtros
  #         del usuario. Mismo patrón que datos_serie_edad_efectiva() en E1.
  # ══════════════════════════════════════════════════════════════════════════

  es_nacional_sin_filtros_sexo <- function(f) {
    f$entidad == "Nacional" && f$distrito == "Todos" && f$municipio == "Todos" &&
      (is.null(f$seccion) || "Todas" %in% f$seccion || length(f$seccion) == 0)
  }

  construir_fila_sexo_filtrada <- function(df_raw, fecha_d, ambito) {
    if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)

    es_ext_loc <- function(df) {
      if (!"cabecera_distrital" %in% colnames(df)) return(rep(FALSE, nrow(df)))
      grepl("RESIDENTES EXTRANJERO", toupper(trimws(df$cabecera_distrital)), fixed = TRUE)
    }

    fila <- if (ambito == "extranjero") {
      filas <- df_raw[es_ext_loc(df_raw), , drop = FALSE]
      if (nrow(filas) == 0) return(NULL)
      cols_id  <- c("cve_entidad","nombre_entidad","cve_distrito","cabecera_distrital",
                    "cve_municipio","nombre_municipio","seccion")
      cols_num <- setdiff(colnames(filas), cols_id)
      cols_num <- cols_num[sapply(filas[, cols_num, drop = FALSE], function(x)
        is.numeric(x) || suppressWarnings(!any(is.na(as.numeric(x)))))]
      as.data.frame(lapply(filas[, cols_num, drop = FALSE],
                           function(x) sum(as.numeric(x), na.rm = TRUE)),
                    stringsAsFactors = FALSE)
    } else {
      mask_excl <- es_ext_loc(df_raw)
      if ("cve_entidad" %in% colnames(df_raw)) mask_excl <- mask_excl | is.na(df_raw$cve_entidad)
      filas <- df_raw[!mask_excl, , drop = FALSE]
      if (nrow(filas) == 0) return(NULL)
      if (nrow(filas) == 1) {
        filas
      } else {
        cols_id  <- c("cve_entidad","nombre_entidad","cve_distrito","cabecera_distrital",
                      "cve_municipio","nombre_municipio","seccion")
        cols_num <- setdiff(colnames(filas), cols_id)
        cols_num <- cols_num[sapply(filas[, cols_num, drop = FALSE], function(x)
          is.numeric(x) || suppressWarnings(!any(is.na(as.numeric(x)))))]
        as.data.frame(lapply(filas[, cols_num, drop = FALSE],
                             function(x) sum(as.numeric(x), na.rm = TRUE)),
                      stringsAsFactors = FALSE)
      }
    }

    if (is.null(fila) || nrow(fila) == 0) return(NULL)

    get_col <- function(col) {
      if (col %in% colnames(fila)) { v <- as.numeric(fila[[col]]); if (is.na(v)) 0 else v } else 0
    }

    data.frame(
      fecha             = fecha_d,
      padron_hombres    = get_col("padron_hombres"),
      padron_mujeres    = get_col("padron_mujeres"),
      padron_no_binario = get_col("padron_no_binario"),
      lista_hombres     = get_col("lista_hombres"),
      lista_mujeres     = get_col("lista_mujeres"),
      lista_no_binario  = get_col("lista_no_binario"),
      stringsAsFactors  = FALSE
    )
  }

  datos_serie_sexo_efectiva <- reactive({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)

    estado_actual <- estado_app()
    ambito        <- ambito_reactivo()

    if (estado_actual != "consultado") return(datos_semanal_serie_sexo())

    btn     <- input$btn_consultar
    filtros <- isolate({
      list(
        entidad   = input$entidad   %||% "Nacional",
        distrito  = input$distrito  %||% "Todos",
        municipio = input$municipio %||% "Todos",
        seccion   = input$seccion   %||% "Todas"
      )
    })

    if (es_nacional_sin_filtros_sexo(filtros)) {
      message("\U0001f4ca [serie_sexo_efectiva] Filtros nacionales \u2192 cach\u00e9")
      return(datos_semanal_serie_sexo())
    }

    message("\U0001f4ca [serie_sexo_efectiva] Filtros geogr\u00e1ficos: ",
            filtros$entidad, " / ", filtros$distrito, " / ", filtros$municipio)

    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas  <- sort(as.Date(catalog$semanal_comun, origin = "1970-01-01"))
    fechas  <- fechas[fechas <= Sys.Date()]
    if (length(fechas) == 0) return(NULL)

    filas_serie <- list()
    n_ok <- 0L; n_err <- 0L

    cargar_loop <- function() {
      for (f in fechas) {
        fecha_d <- as.Date(f, origin = "1970-01-01")
        df_raw  <- tryCatch({
          res <- cargar_lne(tipo_corte = "semanal", fecha = fecha_d,
                            dimension  = "sexo",
                            estado     = filtros$entidad,
                            distrito   = filtros$distrito,
                            municipio  = filtros$municipio,
                            seccion    = filtros$seccion,
                            incluir_extranjero = TRUE)
          if (!is.null(res) && !is.null(res$datos)) res$datos else NULL
        }, error = function(e) NULL)

        fila <- construir_fila_sexo_filtrada(df_raw, fecha_d, ambito)
        if (!is.null(fila)) {
          filas_serie[[length(filas_serie) + 1]] <<- fila
          n_ok <<- n_ok + 1L
        } else {
          n_err <<- n_err + 1L
        }
      }
    }

    shiny::withProgress(message = "Cargando datos semanales...", value = NULL, cargar_loop())

    if (length(filas_serie) == 0) {
      message("\u26a0\ufe0f [serie_sexo_efectiva] Sin datos para filtros geogr\u00e1ficos")
      return(NULL)
    }

    serie <- do.call(rbind, filas_serie)
    serie <- serie[order(serie$fecha), ]
    rownames(serie) <- NULL
    message("\u2705 [serie_sexo_efectiva] ", nrow(serie), " semanas (OK=", n_ok,
            " ERR=", n_err, ") \u2014 ", filtros$entidad)
    return(serie)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # S3 — Evolución y Proyección Semanal por Sexo + card No Binario
  # Widget: checkboxes H/M/NB + Restablecer + Metodología (patrón E3)
  # NB: card annotation (escala incompatible con H/M); proyección en card si posible
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_s3_widget_ui <- renderUI({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    tagList(
      tags$style(HTML(paste0(
        "#", session$ns("semanal_s3_sexos"),
        " .shiny-options-group { display:flex; flex-wrap:nowrap; gap:4px 0; }",
        "#", session$ns("semanal_s3_sexos"),
        " .shiny-options-group .checkbox { margin:0; padding:1px 12px 1px 4px; box-sizing:border-box; }",
        "#", session$ns("semanal_s3_sexos"),
        " label { font-size:12px; color:#333; cursor:pointer; }",
        "#", session$ns("semanal_s3_sexos"),
        " input[type='checkbox'] { accent-color:#44559B; }"
      ))),
      div(
        style = "display:flex;justify-content:center;align-items:center;padding:6px 0;position:relative;",
        checkboxGroupInput(
          session$ns("semanal_s3_sexos"), label = NULL,
          choices  = c("Hombres" = "hombres", "Mujeres" = "mujeres", "No Binario" = "no_binario"),
          selected = c("hombres", "mujeres", "no_binario"),
          inline   = TRUE
        ),
        div(
          style = "position:absolute;right:0;display:flex;gap:6px;",
          actionButton(
            session$ns("semanal_s3_btn_reset"), label = "Restablecer",
            icon  = icon("rotate-left"), class = "btn btn-sm",
            style = "background:#6c757d;color:#fff;border:none;width:110px;padding:4px 10px;border-radius:4px;"
          ),
          actionButton(
            session$ns("semanal_s3_btn_metodologia"), label = "Metodolog\u00eda",
            icon  = icon("info-circle"), class = "btn btn-sm btn-outline-info",
            style = "padding:4px 10px;border-radius:4px;"
          )
        )
      )
    )
  })

  observeEvent(input$semanal_s3_btn_reset, {
    updateCheckboxGroupInput(session, "semanal_s3_sexos",
                             selected = c("hombres", "mujeres", "no_binario"))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$semanal_s3_btn_metodologia, {
    showModal(modalDialog(
      title     = tagList(icon("chart-line"), " Metodolog\u00eda de Proyecci\u00f3n"),
      size      = "l",
      easyClose = TRUE,
      footer    = tagList(
        tags$button(
          type = "button", class = "btn btn-primary",
          style = "background-color:#277592;border-color:#277592;padding:8px 32px;font-size:14px;border-radius:6px;",
          `data-dismiss` = "modal", "Cerrar"
        )
      ),
      tagList(
        div(
          style = "margin-bottom:16px;",
          h4(tags$span(style = "color:#1a3a5c;font-weight:700;",
                       "\u00bfC\u00f3mo se calcula la proyecci\u00f3n?"),
             style = "margin-top:0;"),
          p("La proyecci\u00f3n utiliza un ",
            tags$strong("modelo de tasa de crecimiento mensual promedio"),
            " basado en los cortes semanales disponibles del a\u00f1o en curso.")
        ),
        h5(tags$span(style = "color:#1a3a5c;font-weight:700;", "Pasos del c\u00e1lculo:"),
           style = "margin-bottom:8px;"),
        tags$ol(
          style = "padding-left:24px;margin-bottom:12px;",
          tags$li(tagList(tags$strong("Datos base:"),
                          " Cortes semanales del a\u00f1o actual para Padr\u00f3n Hombres, Padr\u00f3n Mujeres, LNE Hombres y LNE Mujeres.")),
          tags$li(tagList(tags$strong("Tasa de crecimiento:"),
                          " Promedio mensual entre el primer y \u00faltimo corte disponible.")),
          tags$li(tagList(tags$strong("Proyecci\u00f3n:"),
                          " Se aplica la tasa compuesta hasta diciembre.")),
          tags$li(tagList(tags$strong("Visualizaci\u00f3n:"),
                          " L\u00edneas punteadas = valores proyectados.")),
          tags$li(tagList(tags$strong("No Binario:"),
                          " Si hay suficientes datos, la proyecci\u00f3n de LNE se muestra en la tarjeta informativa."))
        ),
        tags$hr(),
        h5(tags$span(style = "color:#1a3a5c;font-weight:700;", "F\u00f3rmula:"),
           style = "margin-bottom:8px;"),
        div(
          style = "background:#eef4fb;border-left:4px solid #277592;padding:10px 16px;border-radius:4px;font-family:monospace;font-size:13px;color:#D10F3F;margin-bottom:16px;",
          div("Tasa = (Valor_final / Valor_inicial)^(1/(n-1)) \u2014 1"),
          div("Proyecci\u00f3n(i) = \u00daltimo_valor \u00d7 (1 + tasa)^i")
        ),
        div(style = "color:#c0392b;font-weight:700;margin-bottom:6px;",
            icon("triangle-exclamation"), " Consideraciones:"),
        tags$ul(
          style = "padding-left:24px;margin-bottom:12px;",
          tags$li(tagList("Asume crecimiento ", tags$strong("constante"), ".")),
          tags$li(tagList("Es una ", tags$strong("estimaci\u00f3n estad\u00edstica"), ".")),
          tags$li(tagList("Proyecta hasta ", tags$strong("diciembre"), " del a\u00f1o.")),
          tags$li("Los datos oficiales del INE prevalecen.")
        ),
        tags$hr(),
        div(
          style = "text-align:center;font-size:12px;color:#666;",
          icon("circle-info"),
          " Esta es una herramienta de referencia. Los datos oficiales son los publicados por el INE."
        )
      )
    ))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$semanal_s3_proyeccion <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)

    ambito    <- ambito_reactivo()
    alcance   <- isolate(texto_alcance())
    anio      <- anio_semanal()
    etiq      <- etiq_ambito(ambito)

    serie <- datos_serie_sexo_efectiva()
    if (is.null(serie) || nrow(serie) < 2) {
      return(plot_vacio("Sin datos de serie temporal para proyecci\u00f3n"))
    }

    sexos_sel    <- input$semanal_s3_sexos %||% c("hombres", "mujeres", "no_binario")
    if (length(sexos_sel) == 0) sexos_sel <- c("hombres", "mujeres", "no_binario")

    ultima_fecha <- max(serie$fecha)
    etiq_fecha   <- fecha_es(ultima_fecha)
    ultimo_mes   <- as.integer(format(ultima_fecha, "%m"))
    meses_rest   <- 12L - ultimo_mes

    # Trazas H y M: ordenadas por valor final descendente
    vals_finales <- c(
      padron_h = tail(serie$padron_hombres, 1),
      padron_m = tail(serie$padron_mujeres, 1),
      lista_h  = tail(serie$lista_hombres,  1),
      lista_m  = tail(serie$lista_mujeres,  1)
    )
    orden_trazas <- names(sort(vals_finales, decreasing = TRUE))

    config_trazas <- list(
      padron_h = list(col_y = "padron_hombres", nombre = "Padr\u00f3n Hombres",
                      sexo = "hombres", color = color_h_loc(ambito), dash = "solid",
                      col_proy_n = "Proy. Padr\u00f3n H"),
      padron_m = list(col_y = "padron_mujeres", nombre = "Padr\u00f3n Mujeres",
                      sexo = "mujeres", color = color_m_loc(ambito), dash = "solid",
                      col_proy_n = "Proy. Padr\u00f3n M"),
      lista_h  = list(col_y = "lista_hombres",  nombre = "LNE Hombres",
                      sexo = "hombres", color = color_h_loc(ambito), dash = "dot",
                      col_proy_n = "Proy. LNE H"),
      lista_m  = list(col_y = "lista_mujeres",  nombre = "LNE Mujeres",
                      sexo = "mujeres", color = color_m_loc(ambito), dash = "dot",
                      col_proy_n = "Proy. LNE M")
    )

    p <- plot_ly()

    for (clave in orden_trazas) {
      cfg <- config_trazas[[clave]]
      if (!(cfg$sexo %in% sexos_sel)) next

      proy <- NULL
      if (meses_rest > 0) {
        es_padron <- grepl("padron", clave)
        serie_tmp <- serie
        if (es_padron) {
          # lista_clave: "lista_h" o "lista_m" → col_y: "lista_hombres" o "lista_mujeres"
          lista_clave <- sub("padron_", "lista_", clave)
          serie_tmp$padron_electoral <- serie_tmp[[cfg$col_y]]
          serie_tmp$lista_nominal    <- serie_tmp[[config_trazas[[lista_clave]]$col_y]]
        } else {
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
        marker = list(size = 6, color = cfg$color,
                      symbol = if (grepl("lista", clave)) "square" else "circle"),
        hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>",
                               cfg$nombre, ": %{y:,.0f}<extra></extra>")
      )

      if (!is.null(proy) && nrow(proy) > 0) {
        col_proy <- if (es_padron) "padron_proyectado" else "lista_proyectada"
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

    # Card No Binario — solo si NB está en la selección
    annotations_s3 <- list(ann_alcance(alcance, y_pos = 1.10))
    if ("no_binario" %in% sexos_sel) {
      ultima_fila <- serie[which.max(serie$fecha), ]
      nb_p <- ultima_fila$padron_no_binario %||% 0
      nb_l <- ultima_fila$lista_no_binario  %||% 0

      # Intentar proyección para NB (Padrón y LNE)
      proy_nb <- NULL
      if (meses_rest > 0 && ((!is.na(nb_p) && nb_p > 0) || (!is.na(nb_l) && nb_l > 0))) {
        serie_nb <- serie
        serie_nb$padron_electoral <- serie_nb$padron_no_binario
        serie_nb$lista_nominal    <- serie_nb$lista_no_binario
        proy_nb <- tryCatch(
          proyectar_con_tasa_crecimiento(serie_nb, meses_rest),
          error = function(e) NULL
        )
      }

      if (!is.na(nb_p) && nb_p > 0) {
        texto_nb <- paste0(
          "<span style='font-size:12px;font-weight:bold;color:#9B59B6;'>\u26a7 No Binario</span><br>",
          "<span style='font-size:11px;color:#333;'>Padr\u00f3n: ", fmt_num(nb_p), "</span><br>",
          "<span style='font-size:11px;color:#333;'>LNE: ", fmt_num(nb_l), "</span>"
        )
        if (!is.null(proy_nb) && nrow(proy_nb) > 0) {
          if ("padron_proyectado" %in% colnames(proy_nb)) {
            proy_nb_p_val <- tail(proy_nb$padron_proyectado, 1)
            texto_nb <- paste0(
              texto_nb,
              "<br><span style='font-size:10px;color:#666;'>Proy. Padr\u00f3n: ",
              fmt_num(proy_nb_p_val), "</span>"
            )
          }
          if ("lista_proyectada" %in% colnames(proy_nb)) {
            proy_nb_l_val <- tail(proy_nb$lista_proyectada, 1)
            texto_nb <- paste0(
              texto_nb,
              "<br><span style='font-size:10px;color:#666;'>Proy. LNE: ",
              fmt_num(proy_nb_l_val), "</span>"
            )
          }
        }
        annotations_s3[[length(annotations_s3) + 1]] <- list(
          text = texto_nb,
          x = 0.03, y = 0.97, xref = "paper", yref = "paper",
          xanchor = "left", yanchor = "top", showarrow = FALSE,
          bgcolor = "rgba(255,255,255,0.95)",
          bordercolor = "#9B59B6", borderwidth = 1.5, borderpad = 8,
          font = list(size = 11, color = "#333", family = "Arial, sans-serif")
        )
      }
    }

    p %>% layout(
      title  = list(
        text = paste0("Evoluci\u00f3n y Proyecci\u00f3n Semanal por Sexo \u2013 ",
                      etiq_fecha, " \u2013 ", etiq),
        font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis  = list(
        title = "", type = "date",
        tickformat = "%d %b",
        range = c(min(serie$fecha) - 3, as.Date(paste0(anio, "-12-31")))
      ),
      yaxis  = list(title = "N\u00famero de electores", separatethousands = TRUE),
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.10),
      margin = list(t = 120, b = 80, l = 90, r = 40),
      hovermode   = "x unified",
      annotations = annotations_s3
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, input$semanal_s3_sexos, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  # ══════════════════════════════════════════════════════════════════════════
  # S4 — Padrón y LNE por Sexo (era S5, renombrado y estilos homologados)
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_s4_barras <- renderPlotly({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)

    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)

    serie_s    <- datos_semanal_serie_sexo()
    etiq_fecha <- if (!is.null(serie_s) && nrow(serie_s) > 0)
      fecha_es(max(serie_s$fecha)) else as.character(anio)

    datos <- datos_semanal_sexo()
    if (is.null(datos)) return(plot_vacio())

    tot <- extraer_totales_sexo(datos, ambito)
    if (is.null(tot)) return(plot_vacio("Sin datos de sexo"))

    # X = Tipo (Padrón / LNE), color = Sexo → mismos tonos H/M que S1-S3
    df_g <- data.frame(
      Tipo     = rep(c("Padr\u00f3n Electoral", "Lista Nominal"), each = 2),
      Sexo     = rep(c("Hombres", "Mujeres"), 2),
      Cantidad = c(tot$padron_hombres, tot$padron_mujeres,
                   tot$lista_hombres,  tot$lista_mujeres),
      stringsAsFactors = FALSE
    )

    p <- plot_ly(
      data  = df_g, x = ~Tipo, y = ~Cantidad, color = ~Sexo,
      colors = c("Hombres" = color_h_loc(ambito),
                 "Mujeres" = color_m_loc(ambito)),
      type  = "bar",
      text  = ~paste0(format(Cantidad, big.mark = ","), " electores"),
      hovertemplate = "<b>%{x}</b> \u2013 %{data.name}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        title   = list(
          text = paste0("Padr\u00f3n y LNE por Sexo \u2013 ", etiq_fecha, " \u2013 ", etiq),
          font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        barmode = "group",
        xaxis   = list(title = ""),
        yaxis   = list(title = "N\u00famero de electores", separatethousands = TRUE),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.10),
        margin  = list(t = 120, b = 80, l = 90, r = 40),
        annotations = list(ann_alcance(alcance, y_pos = 1.10))
      )

    # Card No Binario — mismo estilo que S3
    nb_p <- tot$padron_no_binario %||% 0
    nb_l <- tot$lista_no_binario  %||% 0
    if (!is.na(nb_p) && nb_p > 0) {
      texto_nb_s4 <- paste0(
        "<span style='font-size:12px;font-weight:bold;color:#9B59B6;'>\u26a7 No Binario</span><br>",
        "<span style='font-size:11px;color:#333;'>Padr\u00f3n: ", fmt_num(nb_p), "</span><br>",
        "<span style='font-size:11px;color:#333;'>LNE: ", fmt_num(nb_l), "</span>"
      )
      p <- p %>% add_annotations(
        text      = texto_nb_s4,
        x = 0.03, y = 0.97, xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "top", showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.95)",
        bordercolor = "#9B59B6", borderwidth = 1.5, borderpad = 8,
        font = list(size = 11, color = "#333", family = "Arial, sans-serif")
      )
    }
    p
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  message("✅ graficas_semanal_sexo v2.1 inicializado")
  message("   S1: Pirámide por 12 rangos (H vs M)")
  message("   S2: Barras agrupadas LNE × Grupo Etario × Sexo + widget H/M/NB")
  message("   S3: Proyección semanal Padrón+LNE por sexo + widget + datos_serie_sexo_efectiva")
  message("   S4: Barras Padrón/LNE por sexo (fecha dinámica)")
}
