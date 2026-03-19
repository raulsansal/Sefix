# modules/lista_nominal_graficas/graficas_semanal_origen.R
# Vista Semanal — Gráficas de Origen: O1, O2 (nuevo), O3 (antes O2)
# Versión: 2.1
#
# CORRECCIÓN v2.1 vs v2.0:
#   · detectar_cols_origen(): patrones ajustados a nombres reales del CSV
#     (ln_aguascalientes, pad_baja_california, etc.) no códigos numéricos.
#   · Agregados MAPA_ESTADO_ABREV y MAPA_ESTADO_NOMBRE con las 32 entidades.
#   · abrev_origen() / nombre_origen() refactorizados para usar esos mapas.
#   · O2 Diferencial: cambiado a (Padrón − LNE), escala blanco→rojo.
#   · O2 eje Y: etiquetas neutras (AGS…ZAC, E87, E88) ya que el eje es
#     compartido entre el panel Padrón y el panel LNE.
#   · Márgenes aumentados para evitar solapamiento de título y ann_alcance.
#
# Gráficas:
#   O1 (semanal_o1_calor)       — Mapa de calor LNE: eje Y = top N orígenes
#                                  (abrev. AGS…ZAC + LN87 + LN88), default Todos
#   O2 (semanal_o2_calor)       — Comparación Padrón vs LNE con radio-button:
#                                    "Absoluto": subplots lado a lado misma escala
#                                    "Diferencial": Padrón − LNE (blanco→rojo)
#   O3 (semanal_o3_proyeccion)  — Proyección semanal LNE por entidad de origen
#
# Dependencias del entorno padre (graficas_semanal.R):
#   NOM_CORTOS, NOM_ORIGEN, FUENTE_INE
#   fmt_num(), etiq_ambito(), ann_fuente(), ann_alcance(), plot_vacio()
#   es_historico(), desglose_activo()
#   proyectar_con_tasa_crecimiento()

graficas_semanal_origen <- function(input, output, session,
                                    datos_semanal_origen,
                                    datos_semanal_serie_origen,
                                    anio_semanal,
                                    texto_alcance,
                                    ambito_reactivo,
                                    estado_app) {

  message("📊 Inicializando graficas_semanal_origen v2.1")

  # ══════════════════════════════════════════════════════════════════════════
  # MAPAS DE ESTADOS — sufijo de columna → abreviación / nombre completo
  # Corresponden a los nombres reales de las columnas en los CSV de origen:
  #   ln_aguascalientes, pad_baja_california, ... ln87, ln88, pad87, pad88
  # ══════════════════════════════════════════════════════════════════════════

  MAPA_ABREV <- c(
    "aguascalientes"      = "AGS",
    "baja_california"     = "BC",
    "baja_california_sur" = "BCS",
    "campeche"            = "CAMP",
    "coahuila"            = "COAH",
    "colima"              = "COL",
    "chiapas"             = "CHIS",
    "chihuahua"           = "CHIH",
    "cdmx"                = "CDMX",
    "durango"             = "DGO",
    "guanajuato"          = "GTO",
    "guerrero"            = "GRO",
    "hidalgo"             = "HGO",
    "jalisco"             = "JAL",
    "estado_de_mexico"    = "MEX",
    "michoacan"           = "MICH",
    "morelos"             = "MOR",
    "nayarit"             = "NAY",
    "nuevo_leon"          = "NL",
    "oaxaca"              = "OAX",
    "puebla"              = "PUE",
    "queretaro"           = "QRO",
    "quintana_roo"        = "QROO",
    "san_luis_potosi"     = "SLP",
    "sinaloa"             = "SIN",
    "sonora"              = "SON",
    "tabasco"             = "TAB",
    "tamaulipas"          = "TAMS",
    "tlaxcala"            = "TLAX",
    "veracruz"            = "VER",
    "yucatan"             = "YUC",
    "zacatecas"           = "ZAC"
  )

  MAPA_NOMBRE <- c(
    "aguascalientes"      = "Aguascalientes",
    "baja_california"     = "Baja California",
    "baja_california_sur" = "Baja California Sur",
    "campeche"            = "Campeche",
    "coahuila"            = "Coahuila",
    "colima"              = "Colima",
    "chiapas"             = "Chiapas",
    "chihuahua"           = "Chihuahua",
    "cdmx"                = "Ciudad de M\u00e9xico",
    "durango"             = "Durango",
    "guanajuato"          = "Guanajuato",
    "guerrero"            = "Guerrero",
    "hidalgo"             = "Hidalgo",
    "jalisco"             = "Jalisco",
    "estado_de_mexico"    = "Estado de M\u00e9xico",
    "michoacan"           = "Michoac\u00e1n",
    "morelos"             = "Morelos",
    "nayarit"             = "Nayarit",
    "nuevo_leon"          = "Nuevo Le\u00f3n",
    "oaxaca"              = "Oaxaca",
    "puebla"              = "Puebla",
    "queretaro"           = "Quer\u00e9taro",
    "quintana_roo"        = "Quintana Roo",
    "san_luis_potosi"     = "San Luis Potos\u00ed",
    "sinaloa"             = "Sinaloa",
    "sonora"              = "Sonora",
    "tabasco"             = "Tabasco",
    "tamaulipas"          = "Tamaulipas",
    "tlaxcala"            = "Tlaxcala",
    "veracruz"            = "Veracruz",
    "yucatan"             = "Yucat\u00e1n",
    "zacatecas"           = "Zacatecas"
  )

  # Paleta O3 — 34 colores para trazas de proyección
  PALETA_O3_NAC <- colorRampPalette(c(
    "#003E66","#44559B","#AE0E35","#C0311A","#9B59B6",
    "#1ABC9C","#E67E22","#2ECC71","#E74C3C","#3498DB"))(34)
  PALETA_O3_EXT <- colorRampPalette(c(
    "#8F6A00","#D4A500","#EAC43E","#B3D491","#5C9900",
    "#8FB369","#CCE4B1","#F5CA45","#D4666C","#6B8F00"))(34)

  paleta_o3 <- function(ambito) {
    if (ambito == "extranjero") PALETA_O3_EXT else PALETA_O3_NAC
  }

  # ══════════════════════════════════════════════════════════════════════════
  # HELPERS LOCALES
  # ══════════════════════════════════════════════════════════════════════════

  # Fecha en español: "16 Octubre 2025"
  fecha_es <- function(d) {
    meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
               "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
    paste(as.integer(format(d, "%d")), meses[as.integer(format(d, "%m"))],
          format(d, "%Y"))
  }

  # Detecta columnas ln_* / pad_* con nombres de estado completos + 87/88
  detectar_cols_origen <- function(df, tipo = "ln") {
    patron <- if (tipo == "ln")
      "^ln_[a-z]|^ln87$|^ln88$"
    else
      "^pad_[a-z]|^pad87$|^pad88$"
    grep(patron, colnames(df), value = TRUE, ignore.case = TRUE)
  }

  # Extrae el sufijo de estado de una clave de columna
  sufijo_estado <- function(clave) {
    gsub("^ln_|^pad_", "", clave, ignore.case = TRUE)
  }

  # Abreviación para eje Y según tipo de gráfica:
  #   tipo = "ln"    → LN87 / LN88 / AGS … ZAC   (O1, LNE sola)
  #   tipo = "pad"   → PAD87 / PAD88 / AGS … ZAC  (no usado aún)
  #   tipo = "neutro"→ E87 / E88 / AGS … ZAC      (O2, eje compartido)
  abrev_origen <- function(clave, tipo = "ln") {
    if (grepl("87$", clave, ignore.case = TRUE)) {
      return(switch(tipo, "pad" = "PAD87", "neutro" = "E87", "LN87"))
    }
    if (grepl("88$", clave, ignore.case = TRUE)) {
      return(switch(tipo, "pad" = "PAD88", "neutro" = "E88", "LN88"))
    }
    suf <- sufijo_estado(clave)
    MAPA_ABREV[suf] %||% toupper(substr(suf, 1, 5))
  }

  # Nombre completo de la entidad de origen (para hovertemplate)
  nombre_origen <- function(clave) {
    if (grepl("87$", clave, ignore.case = TRUE))
      return("Mexicanos nacidos en el extranjero")
    if (grepl("88$", clave, ignore.case = TRUE))
      return("Ciudadanos naturalizados")
    suf <- sufijo_estado(clave)
    MAPA_NOMBRE[suf] %||% paste0("(", suf, ")")
  }

  # ══════════════════════════════════════════════════════════════════════════
  # HELPER: construir_matrices_origen
  # Rankea por LNE total y construye mat_ln + mat_pad con las mismas dims.
  # Devuelve list(mat_ln, mat_pad, abrev_ln_y, abrev_neu_y, full_y, etiq_x)
  # ══════════════════════════════════════════════════════════════════════════

  construir_matrices_origen <- function(df, ambito, top_n) {
    if (is.null(df) || nrow(df) == 0) return(NULL)

    df_uso <- if (ambito == "extranjero") {
      df[grepl("RESIDENTES EXTRANJERO", toupper(df$nombre_entidad),
               fixed = TRUE), ]
    } else {
      df[!grepl("RESIDENTES EXTRANJERO|^TOTALES$",
                toupper(trimws(df$nombre_entidad))), ]
    }
    if (nrow(df_uso) == 0) return(NULL)

    cols_ln  <- detectar_cols_origen(df_uso, "ln")
    cols_pad <- detectar_cols_origen(df_uso, "pad")
    if (length(cols_ln) == 0) return(NULL)

    # Rankeo por LNE total (sumar todas las filas del ámbito)
    totales_ln <- colSums(df_uso[, cols_ln, drop = FALSE], na.rm = TRUE)
    totales_ln <- sort(totales_ln, decreasing = TRUE)
    n_sel      <- if (top_n == 0) length(totales_ln) else min(top_n, length(totales_ln))
    cols_top   <- names(totales_ln)[seq_len(n_sel)]

    # Etiquetas eje Y: abrev LN (para O1), neutras (para O2), nombre completo (hover)
    abrev_ln_y  <- sapply(cols_top, abrev_origen, tipo = "ln")
    abrev_neu_y <- sapply(cols_top, abrev_origen, tipo = "pad")
    full_y      <- sapply(cols_top, nombre_origen)

    # Entidades receptoras únicas, ordenadas por cve_entidad
    df_geo <- df_uso[!grepl("^TOTALES$", toupper(trimws(df_uso$nombre_entidad))), ]
    if ("cve_entidad" %in% colnames(df_geo)) {
      entidades <- unique(df_geo[, c("cve_entidad", "nombre_entidad")])
      entidades <- entidades[order(as.integer(entidades$cve_entidad)), ]
    } else {
      ents_u    <- unique(df_geo$nombre_entidad)
      ents_u    <- ents_u[seq_len(min(32, length(ents_u)))]
      entidades <- data.frame(cve_entidad    = seq_along(ents_u),
                              nombre_entidad = ents_u,
                              stringsAsFactors = FALSE)
    }

    etiq_x <- sapply(entidades$cve_entidad, function(cve) {
      k <- sprintf("%02d", as.integer(cve))
      NOM_CORTOS[k] %||% paste0("E", k)
    })

    # Construir mat_ln y mat_pad con dimensiones idénticas
    nr      <- n_sel; nc <- nrow(entidades)
    mat_ln  <- matrix(0, nrow = nr, ncol = nc, dimnames = list(abrev_ln_y, etiq_x))
    mat_pad <- matrix(0, nrow = nr, ncol = nc, dimnames = list(abrev_ln_y, etiq_x))

    for (j in seq_len(nc)) {
      ent       <- entidades$nombre_entidad[j]
      filas_ent <- df_geo[
        toupper(trimws(df_geo$nombre_entidad)) == toupper(trimws(ent)), ]
      if (nrow(filas_ent) == 0) next
      for (i in seq_len(nr)) {
        col_ln  <- cols_top[i]
        col_pad <- gsub("^ln_", "pad_", gsub("^ln(8[78])$", "pad\\1", col_ln,
                                             ignore.case = TRUE),
                        ignore.case = TRUE)
        if (col_ln  %in% colnames(filas_ent))
          mat_ln[i,  j] <- sum(as.numeric(filas_ent[[col_ln]]),  na.rm = TRUE)
        if (col_pad %in% colnames(filas_ent))
          mat_pad[i, j] <- sum(as.numeric(filas_ent[[col_pad]]), na.rm = TRUE)
      }
    }

    list(mat_ln      = mat_ln,
         mat_pad     = mat_pad,
         abrev_ln_y  = abrev_ln_y,
         abrev_neu_y = abrev_neu_y,
         full_y      = full_y,
         etiq_x      = etiq_x)
  }

  # Helpers para DataTable y O3
  agregar_corte_origen <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0 || !"nombre_entidad" %in% colnames(df))
      return(NULL)
    filas <- if (ambito == "extranjero") {
      df[grepl("RESIDENTES EXTRANJERO", toupper(df$nombre_entidad),
               fixed = TRUE), ]
    } else {
      df[!grepl("RESIDENTES EXTRANJERO|^TOTALES$",
                toupper(trimws(df$nombre_entidad))), ]
    }
    if (nrow(filas) == 0) return(NULL)
    cols_num <- union(detectar_cols_origen(filas, "ln"),
                      detectar_cols_origen(filas, "pad"))
    if (length(cols_num) == 0) return(NULL)
    as.data.frame(
      lapply(filas[, cols_num, drop = FALSE],
             function(x) sum(as.numeric(x), na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  }

  tabla_origen_ordenada <- function(df, ambito) {
    fila    <- agregar_corte_origen(df, ambito)
    if (is.null(fila)) return(NULL)
    cols_ln <- detectar_cols_origen(fila, "ln")
    if (length(cols_ln) == 0) return(NULL)
    res <- do.call(rbind, lapply(cols_ln, function(col) {
      col_pad <- gsub("^ln_", "pad_", gsub("^ln(8[78])$", "pad\\1", col,
                                           ignore.case = TRUE),
                      ignore.case = TRUE)
      data.frame(
        clave_ln  = col,
        nombre    = nombre_origen(col),
        lne_total = as.numeric(fila[[col]]),
        pad_total = if (col_pad %in% colnames(fila))
          as.numeric(fila[[col_pad]]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    res[order(res$lne_total, decreasing = TRUE, na.last = TRUE), ]
  }

  # Fecha del archivo más reciente (para títulos)
  obtener_etiq_fecha <- function() {
    tryCatch({
      serie <- isolate(datos_semanal_serie_origen())
      if (!is.null(serie) && "fecha" %in% colnames(serie) && nrow(serie) > 0)
        fecha_es(max(serie$fecha, na.rm = TRUE))
      else
        as.character(anio_semanal())
    }, error = function(e) as.character(anio_semanal()))
  }

  # ══════════════════════════════════════════════════════════════════════════
  # O1 — Widget: selector top N
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o1_widget_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    div(
      style = paste(
        "display:flex;align-items:center;gap:16px;",
        "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;",
        "padding:8px 14px;margin-bottom:10px;"
      ),
      tags$span(
        style = "font-size:13px;font-weight:600;color:#2c3e50;white-space:nowrap;",
        icon("list-ol"), " Top estados de origen:"
      ),
      div(
        style = "width:115px;margin-bottom:0;",
        selectInput(
          inputId  = session$ns("semanal_o1_top_n"),
          label    = NULL,
          choices  = c("Top 5"  = "5", "Top 10" = "10",
                       "Top 15" = "15", "Todos"  = "0"),
          selected = "0",
          width    = "100%"
        )
      )
    )
  })

  # Contenedor con altura dinámica para O1
  output$semanal_o1_grafica_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    top_n <- suppressWarnings(as.integer(input$semanal_o1_top_n %||% "0"))
    if (is.na(top_n)) top_n <- 0L
    n    <- if (top_n == 0) 34L else min(top_n, 34L)
    alto <- paste0(max(500, n * 26 + 230), "px")
    withSpinner(
      plotlyOutput(session$ns("semanal_o1_calor"), height = alto),
      type = 4, color = "#44559B", size = 0.8
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # O1 — Mapa de calor LNE (origen × receptor)
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o1_calor <- renderPlotly({
    if (es_historico() || desglose_activo() != "origen") return(NULL)

    ambito     <- ambito_reactivo()
    alcance    <- isolate(texto_alcance())
    etiq       <- etiq_ambito(ambito)
    etiq_fecha <- obtener_etiq_fecha()

    datos <- datos_semanal_origen()
    if (is.null(datos)) return(plot_vacio())

    top_n <- suppressWarnings(as.integer(input$semanal_o1_top_n %||% "0"))
    if (is.na(top_n)) top_n <- 0L

    res <- construir_matrices_origen(datos, ambito, top_n)
    if (is.null(res)) return(plot_vacio("Sin datos de origen"))

    mat    <- res$mat_ln   # abrev LN en rownames
    full_y <- res$full_y
    cdata  <- matrix(rep(full_y, ncol(mat)), nrow = nrow(mat))

    cs <- if (ambito == "extranjero")
      list(c(0, "#FFF9E6"), c(0.5, "#EAC43E"), c(1, "#8F6A00"))
    else
      list(c(0, "#E8EDF8"), c(0.5, "#44559B"), c(1, "#1A2654"))

    alto_px <- max(500, nrow(mat) * 26 + 230)

    plot_ly(
      z             = mat,
      x             = colnames(mat),
      y             = rownames(mat),
      customdata    = cdata,
      type          = "heatmap",
      colorscale    = cs,
      showscale     = TRUE,
      hovertemplate = paste0(
        "Origen: <b>%{customdata}</b><br>",
        "Receptor: <b>%{x}</b><br>",
        "LNE: %{z:,.0f}<extra></extra>"
      )
    ) %>%
      layout(
        title = list(
          text    = paste0(
            "LNE por Entidad de Origen y Entidad Receptora \u2013 ",
            etiq_fecha, " \u2013 ", etiq),
          font    = list(size = 15, color = "#333",
                         family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center", y = 0.98
        ),
        xaxis = list(
          title    = "Entidad Receptora",
          tickangle = -40,
          tickfont = list(size = 10)
        ),
        yaxis = list(
          title     = "Entidad de Origen",
          tickfont  = list(size = 10),
          autorange = "reversed"
        ),
        height = alto_px,
        margin = list(t = 140, b = 80, l = 90, r = 80),
        annotations = list(ann_alcance(alcance, y_pos = 1.08))
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      input$semanal_o1_top_n,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  # ══════════════════════════════════════════════════════════════════════════
  # O2 — Widget: top N + radioButtons (Absoluto | Diferencial)
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o2_widget_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    div(
      style = paste(
        "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;",
        "padding:8px 14px 6px 14px;margin-bottom:10px;",
        "display:flex;align-items:center;gap:28px;flex-wrap:wrap;"
      ),
      # Selector top N
      div(
        style = "display:flex;align-items:center;gap:10px;",
        tags$span(
          style = "font-size:13px;font-weight:600;color:#2c3e50;white-space:nowrap;",
          icon("list-ol"), " Top estados:"
        ),
        div(
          style = "width:115px;margin-bottom:0;",
          selectInput(
            inputId  = session$ns("semanal_o2_top_n"),
            label    = NULL,
            choices  = c("Top 5"  = "5", "Top 10" = "10",
                         "Top 15" = "15", "Todos"  = "0"),
            selected = "10",
            width    = "100%"
          )
        )
      ),
      # Radio: Absoluto | Diferencial
      div(
        style = "display:flex;align-items:center;gap:10px;",
        tags$span(
          style = "font-size:13px;font-weight:600;color:#2c3e50;white-space:nowrap;",
          "Vista:"
        ),
        radioButtons(
          inputId  = session$ns("semanal_o2_vista"),
          label    = NULL,
          choices  = c(
            "Absoluto (Padr\u00f3n | LNE)"          = "absoluto",
            "Diferencial (Padr\u00f3n \u2212 LNE)"  = "diferencial"
          ),
          selected = "absoluto",
          inline   = TRUE
        )
      )
    )
  })

  # Contenedor con altura dinámica para O2
  output$semanal_o2_grafica_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    top_n <- suppressWarnings(as.integer(input$semanal_o2_top_n %||% "10"))
    if (is.na(top_n)) top_n <- 10L
    n    <- if (top_n == 0) 34L else min(top_n, 34L)
    alto <- paste0(max(700, n * 45 + 300), "px")
    withSpinner(
      plotlyOutput(session$ns("semanal_o2_calor"), height = alto),
      type = 4, color = "#44559B", size = 0.8
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # O2 — Mapa de calor Padrón vs LNE (absoluto o diferencial)
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o2_calor <- renderPlotly({
    if (es_historico() || desglose_activo() != "origen") return(NULL)

    ambito     <- ambito_reactivo()
    alcance    <- isolate(texto_alcance())
    etiq       <- etiq_ambito(ambito)
    etiq_fecha <- obtener_etiq_fecha()

    datos <- datos_semanal_origen()
    if (is.null(datos)) return(plot_vacio())

    top_n <- suppressWarnings(as.integer(input$semanal_o2_top_n %||% "10"))
    if (is.na(top_n)) top_n <- 10L
    vista <- input$semanal_o2_vista %||% "absoluto"

    res <- construir_matrices_origen(datos, ambito, top_n)
    if (is.null(res)) return(plot_vacio("Sin datos de origen"))

    mat_ln      <- res$mat_ln
    mat_pad     <- res$mat_pad
    abrev_neu_y <- res$abrev_neu_y   # E87/E88 neutro para eje compartido O2
    full_y      <- res$full_y
    etiq_x      <- res$etiq_x
    alto_px     <- max(700, nrow(mat_ln) * 45 + 300)

    titulo_base <- paste0(
      "Padr\u00f3n Electoral y LNE por Entidad de Origen y Receptora \u2013 ",
      etiq_fecha, " \u2013 ", etiq
    )

    # ── DIFERENCIAL: Padrón − LNE ───────────────────────────────────────────
    if (vista == "diferencial") {
      # Padrón ≥ LNE siempre. Valores ≥ 0: blanco(0)→naranja→rojo(máx brecha)
      mat_dif <- mat_pad - mat_ln
      cdata   <- matrix(rep(full_y, ncol(mat_dif)), nrow = nrow(mat_dif))
      cs_dif  <- list(
        c(0,    "#F7F7F7"),
        c(0.35, "#F5C18B"),
        c(0.65, "#D4614F"),
        c(1,    "#8B0000")
      )

      return(
        plot_ly(
          z             = mat_dif,
          x             = etiq_x,
          y             = abrev_neu_y,
          customdata    = cdata,
          type          = "heatmap",
          colorscale    = cs_dif,
          zmin          = 0,
          showscale     = TRUE,
          colorbar      = list(title = "Padr\u00f3n \u2212 LNE"),
          hovertemplate = paste0(
            "Origen: <b>%{customdata}</b><br>",
            "Receptor: <b>%{x}</b><br>",
            "Padr\u00f3n \u2212 LNE: %{z:,.0f}<extra></extra>"
          )
        ) %>%
          layout(
            title = list(
              text = paste0(titulo_base,
                            " \u2013 Diferencial (Padr\u00f3n\u2212LNE)"),
              font = list(size = 15, color = "#333",
                          family = "Arial, sans-serif"),
              x = 0.5, xanchor = "center", y = 0.98
            ),
            xaxis = list(title = "Entidad Receptora", tickangle = -40,
                         tickfont = list(size = 10)),
            yaxis = list(title = "Entidad de Origen",
                         tickfont = list(size = 10), autorange = "reversed"),
            height      = alto_px,
            margin      = list(t = 160, b = 80, l = 90, r = 100),
            annotations = list(ann_alcance(alcance, y_pos = 1.12))
          )
      )
    }

    # ── ABSOLUTO: subplots Padrón | LNE lado a lado ─────────────────────────
    cdata  <- matrix(rep(full_y, ncol(mat_ln)), nrow = nrow(mat_ln))

    cs_nac <- list(c(0, "#E8EDF8"), c(0.5, "#44559B"), c(1, "#1A2654"))
    cs_ext <- list(c(0, "#FFF9E6"), c(0.5, "#EAC43E"), c(1, "#8F6A00"))
    cs     <- if (ambito == "extranjero") cs_ext else cs_nac

    # Misma escala para ambos paneles (comparación justa)
    zmax_g <- max(max(mat_pad, na.rm = TRUE), max(mat_ln, na.rm = TRUE), 1)

    p_pad <- plot_ly(
      z          = mat_pad,
      x          = etiq_x,
      y          = abrev_neu_y,
      customdata = cdata,
      type       = "heatmap",
      colorscale = cs,
      zmin       = 0, zmax = zmax_g,
      showscale  = FALSE,
      hovertemplate = paste0(
        "Origen: <b>%{customdata}</b><br>",
        "Receptor: <b>%{x}</b><br>",
        "Padr\u00f3n Electoral: %{z:,.0f}<extra></extra>"
      )
    )

    p_ln <- plot_ly(
      z          = mat_ln,
      x          = etiq_x,
      y          = abrev_neu_y,
      customdata = cdata,
      type       = "heatmap",
      colorscale = cs,
      zmin       = 0, zmax = zmax_g,
      showscale  = TRUE,
      colorbar   = list(title = "Electores"),
      hovertemplate = paste0(
        "Origen: <b>%{customdata}</b><br>",
        "Receptor: <b>%{x}</b><br>",
        "LNE: %{z:,.0f}<extra></extra>"
      )
    )

    subplot(p_pad, p_ln, nrows = 1, shareY = TRUE,
            margin = 0.04, titleX = TRUE, titleY = TRUE) %>%
      layout(
        title = list(
          text = titulo_base,
          font = list(size = 15, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center", y = 0.98
        ),
        annotations = list(
          ann_alcance(alcance, y_pos = 1.12),
          list(text      = "<b>Padr\u00f3n Electoral</b>",
               x         = 0.22, y    = 1.05,
               xref      = "paper", yref = "paper",
               showarrow = FALSE, xanchor = "center",
               font      = list(size = 12, color = "#2c3e50")),
          list(text      = "<b>Lista Nominal Electoral</b>",
               x         = 0.78, y    = 1.05,
               xref      = "paper", yref = "paper",
               showarrow = FALSE, xanchor = "center",
               font      = list(size = 12, color = "#2c3e50"))
        ),
        yaxis  = list(title = "Entidad de Origen",
                      tickfont = list(size = 10), autorange = "reversed"),
        xaxis  = list(title = "Receptor", tickangle = -40,
                      tickfont = list(size = 9)),
        xaxis2 = list(title = "Receptor", tickangle = -40,
                      tickfont = list(size = 9)),
        height = alto_px,
        margin = list(t = 160, b = 80, l = 90, r = 100)
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      input$semanal_o2_top_n, input$semanal_o2_vista,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — Widget: top N + checks 87/88  (antes era O2)
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o3_controles_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    div(
      style = paste(
        "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;",
        "padding:10px 14px 6px 14px;margin-bottom:10px;"
      ),
      div(
        style = "display:flex;align-items:flex-start;gap:24px;flex-wrap:wrap;",
        div(
          style = "display:flex;align-items:center;gap:10px;",
          tags$span(
            style = "font-size:13px;font-weight:600;color:#2c3e50;white-space:nowrap;",
            icon("chart-line"), " Top entidades de origen:"
          ),
          div(
            style = "width:115px;margin-bottom:0;",
            selectInput(
              inputId  = session$ns("semanal_o3_top_n"),
              label    = NULL,
              choices  = c("Top 1"  = "1", "Top 5"  = "5",
                           "Top 10" = "10", "Top 15" = "15", "Todos" = "0"),
              selected = "1",
              width    = "100%"
            )
          )
        ),
        div(
          style = "display:flex;gap:20px;align-items:center;padding-top:4px;",
          checkboxInput(
            inputId = session$ns("semanal_o3_incl_87"),
            label   = tags$span(style = "font-size:12px;color:#444;",
                                "Mexicanos nacidos en el extranjero"),
            value   = TRUE
          ),
          checkboxInput(
            inputId = session$ns("semanal_o3_incl_88"),
            label   = tags$span(style = "font-size:12px;color:#444;",
                                "Ciudadanos naturalizados"),
            value   = TRUE
          )
        )
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — Proyección semanal LNE por entidad de origen  (antes era O2)
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o3_proyeccion <- renderPlotly({
    if (es_historico() || desglose_activo() != "origen") return(NULL)

    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)

    serie <- datos_semanal_serie_origen()
    if (is.null(serie) || nrow(serie) < 2)
      return(plot_vacio("Sin datos de serie temporal para proyección"))

    top_n   <- suppressWarnings(as.integer(input$semanal_o3_top_n %||% "1"))
    if (is.na(top_n)) top_n <- 1L
    incl_87 <- isTRUE(input$semanal_o3_incl_87)
    incl_88 <- isTRUE(input$semanal_o3_incl_88)

    # Columnas LN en la serie (ya agregadas por construir_fila_serie_origen)
    cols_ln_serie <- detectar_cols_origen(serie, "ln")
    if (length(cols_ln_serie) == 0)
      return(plot_vacio("Sin columnas de origen en la serie"))

    if (!incl_87) cols_ln_serie <- cols_ln_serie[!grepl("87$", cols_ln_serie)]
    if (!incl_88) cols_ln_serie <- cols_ln_serie[!grepl("88$", cols_ln_serie)]
    if (length(cols_ln_serie) == 0)
      return(plot_vacio("Sin columnas de origen disponibles"))

    ultima_fila <- serie[which.max(serie$fecha), cols_ln_serie, drop = FALSE]
    totales_ult <- sort(unlist(ultima_fila), decreasing = TRUE)
    n_sel       <- if (top_n == 0) length(totales_ult) else min(top_n, length(totales_ult))
    cols_top    <- names(totales_ult)[seq_len(n_sel)]

    ultima_fecha <- max(serie$fecha)
    meses_rest   <- 12L - as.integer(format(ultima_fecha, "%m"))
    colores      <- paleta_o3(ambito)
    p            <- plot_ly()

    for (i in seq_along(cols_top)) {
      col    <- cols_top[i]
      color  <- colores[((i - 1L) %% length(colores)) + 1L]
      nombre <- nombre_origen(col)

      proy <- NULL
      if (meses_rest > 0) {
        serie_tmp                  <- serie
        serie_tmp$lista_nominal    <- serie_tmp[[col]]
        serie_tmp$padron_electoral <- serie_tmp[[col]]
        proy <- tryCatch(
          proyectar_con_tasa_crecimiento(serie_tmp, meses_rest),
          error = function(e) NULL
        )
      }

      p <- p %>% add_trace(
        data  = serie, x = ~fecha, y = serie[[col]],
        type  = "scatter", mode = "lines+markers",
        name  = nombre,
        line   = list(color = color, width = 2.5),
        marker = list(size  = 6,     color = color),
        hovertemplate = paste0(
          "<b>%{x|%d %b %Y}</b><br>", nombre, ": %{y:,.0f}<extra></extra>"
        )
      )

      if (!is.null(proy) && nrow(proy) > 0) {
        p <- p %>% add_trace(
          data  = proy, x = ~fecha, y = ~lista_proyectada,
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

    titulo_o3 <- if (n_sel == 1L)
      paste0("Proyecci\u00f3n Semanal \u2013 Top 1 Entidad de Origen \u2013 ", etiq)
    else
      paste0("Proyecci\u00f3n Semanal \u2013 Top ", n_sel,
             " Entidades de Origen \u2013 ", etiq)

    p %>% layout(
      title = list(
        text = titulo_o3,
        font = list(size = 15, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis = list(
        title = "", type = "date", tickformat = "%d %b",
        range = c(min(serie$fecha) - 3, as.Date(paste0(anio, "-12-31")))
      ),
      yaxis       = list(title = "Lista Nominal Electoral",
                         separatethousands = TRUE),
      legend      = list(orientation = "h", xanchor = "center",
                         x = 0.5, y = -0.22),
      margin      = list(t = 120, b = 100, l = 90, r = 40),
      hovermode   = "x unified",
      annotations = list(ann_alcance(alcance, y_pos = 1.08), ann_fuente())
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      input$semanal_o3_top_n, input$semanal_o3_incl_87, input$semanal_o3_incl_88,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  message("✅ graficas_semanal_origen v2.1 inicializado")
  message("   O1: calor LNE — 32 estados + LN87/LN88 en eje Y, default Todos")
  message("   O2: calor Padrón vs LNE — absoluto (subplots) | dif. (Padrón−LNE)")
  message("   O3: proyección semanal por entidad de origen (era O2 v1.0)")
}
