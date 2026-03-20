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

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — CONSTANTES Y HELPERS DE SERIE TEMPORAL
  # ══════════════════════════════════════════════════════════════════════════

  # Valores exactos que usa input$entidad en el sidebar
  ESTADOS_SIDEBAR <- c(
    "AGUASCALIENTES","BAJA CALIFORNIA","BAJA CALIFORNIA SUR",
    "CAMPECHE","COAHUILA","COLIMA","CHIAPAS","CHIHUAHUA",
    "CIUDAD DE MEXICO","DURANGO","GUANAJUATO","GUERRERO",
    "HIDALGO","JALISCO","MEXICO","MICHOACAN","MORELOS",
    "NAYARIT","NUEVO LEON","OAXACA","PUEBLA","QUERETARO",
    "QUINTANA ROO","SAN LUIS POTOSI","SINALOA","SONORA",
    "TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS"
  )

  LABELS_RECEPTORA <- c(
    "Aguascalientes","Baja California","Baja California Sur",
    "Campeche","Coahuila","Colima","Chiapas","Chihuahua",
    "Ciudad de M\u00e9xico","Durango","Guanajuato","Guerrero",
    "Hidalgo","Jalisco","M\u00e9xico","Michoac\u00e1n","Morelos",
    "Nayarit","Nuevo Le\u00f3n","Oaxaca","Puebla","Quer\u00e9taro",
    "Quintana Roo","San Luis Potos\u00ed","Sinaloa","Sonora",
    "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucat\u00e1n","Zacatecas"
  )

  CHOICES_RECEPTORA_O3 <- c(
    "Todas (Nacional)" = "Nacional",
    setNames(ESTADOS_SIDEBAR, LABELS_RECEPTORA)
  )

  # Construye choices para el selectInput de entidad de origen
  choices_origen_tipo <- function(tipo) {
    prefijo <- if (tipo == "lne") "ln" else "pad"
    c(
      "Todas" = "todas",
      setNames(paste0(prefijo, "_", names(MAPA_NOMBRE)), unname(MAPA_NOMBRE)),
      if (tipo == "lne")
        c("LN87 (Nacidos en el extranjero)" = "ln87",
          "LN88 (Naturalizados)"             = "ln88")
      else
        c("PAD87 (Nacidos en el extranjero)" = "pad87",
          "PAD88 (Naturalizados)"             = "pad88")
    )
  }

  # Etiqueta legible para receptora (valor sidebar → nombre en español)
  label_receptora <- function(valor) {
    if (valor == "Nacional") return("Nacional")
    idx <- match(valor, ESTADOS_SIDEBAR)
    if (!is.na(idx)) LABELS_RECEPTORA[idx] else valor
  }

  # Carga la serie temporal de origen para una entidad receptora específica.
  # "Nacional" (y sin filtros sub-entidad) → datos_semanal_serie_origen().
  # Otra entidad → carga semana a semana con filtros geográficos completos.
  cargar_serie_o3 <- function(entidad,
                               distrito  = "Todos",
                               municipio = "Todos",
                               seccion   = "Todas",
                               ambito) {
    if (is.null(entidad) || entidad == "Nacional") {
      return(datos_semanal_serie_origen())
    }

    slug <- function(x) gsub("[^a-z0-9]", "_", tolower(x))
    clave_cache <- paste0("serie_o3_",
                          slug(entidad), "_",
                          slug(distrito), "_",
                          slug(municipio), "_",
                          slug(seccion),  "_",
                          ambito)

    # Verificar caché
    if (exists("LNE_CACHE_SEMANAL", envir = .GlobalEnv)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache[[clave_cache]]) && !is.null(cache$timestamp) &&
          difftime(Sys.time(), cache$timestamp, units = "hours") < 24) {
        message("✅ [O3 CACHÉ] ", clave_cache, " — ",
                nrow(cache[[clave_cache]]), " semanas")
        return(cache[[clave_cache]])
      }
    }

    # Obtener fechas disponibles
    fechas <- tryCatch({
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      f <- sort(as.Date(catalog$semanal_comun, origin = "1970-01-01"))
      f[f <= Sys.Date()]
    }, error = function(e) NULL)

    if (is.null(fechas) || length(fechas) == 0) return(NULL)

    cols_id <- c("cve_entidad","nombre_entidad","cve_distrito",
                 "cabecera_distrital","cve_municipio","nombre_municipio","seccion")
    filas <- list()
    n_ok  <- 0L
    n_err <- 0L

    carga_loop <- function() {
      for (i in seq_along(fechas)) {
        fecha_d <- as.Date(fechas[[i]], origin = "1970-01-01")
        tryCatch({
          res <- cargar_lne(tipo_corte = "semanal", fecha = fecha_d,
                            dimension  = "origen",
                            estado     = entidad,
                            distrito   = distrito,
                            municipio  = municipio,
                            seccion    = seccion,
                            incluir_extranjero = TRUE)
          if (is.null(res) || is.null(res$datos) || nrow(res$datos) == 0) {
            n_err <<- n_err + 1L; return()
          }
          df <- res$datos

          # Agregar según ámbito
          if (ambito == "extranjero") {
            if ("cabecera_distrital" %in% colnames(df)) {
              filas_agg <- df[grepl("RESIDENTES EXTRANJERO",
                                    toupper(trimws(df$cabecera_distrital)),
                                    fixed = TRUE), , drop = FALSE]
            } else {
              filas_agg <- data.frame()
            }
          } else {
            mask <- if ("cabecera_distrital" %in% colnames(df))
              grepl("RESIDENTES EXTRANJERO",
                    toupper(trimws(df$cabecera_distrital)), fixed = TRUE)
            else rep(FALSE, nrow(df))
            if ("cve_entidad" %in% colnames(df))
              mask <- mask | is.na(df$cve_entidad)
            filas_agg <- df[!mask, , drop = FALSE]
          }

          if (is.null(filas_agg) || nrow(filas_agg) == 0) {
            n_err <<- n_err + 1L; return()
          }

          cols_num <- setdiff(colnames(filas_agg), cols_id)
          cols_pad <- grep("^pad_[a-z]|^pad87$|^pad88$", cols_num,
                           value = TRUE, ignore.case = TRUE)
          cols_ln  <- grep("^ln_[a-z]|^ln87$|^ln88$",   cols_num,
                           value = TRUE, ignore.case = TRUE)

          resultado <- list(fecha = fecha_d)
          for (col in c(cols_pad, cols_ln)) {
            v <- sum(as.numeric(filas_agg[[col]]), na.rm = TRUE)
            resultado[[col]] <- if (is.na(v)) 0 else v
          }
          if (length(resultado) > 1) {
            filas[[length(filas) + 1]] <<- as.data.frame(resultado,
                                                          stringsAsFactors = FALSE)
            n_ok <<- n_ok + 1L
          } else {
            n_err <<- n_err + 1L
          }
        }, error = function(e) {
          n_err <<- n_err + 1L
          message("⚠️ [O3] Error en fecha ", format(fecha_d, "%Y-%m-%d"),
                  ": ", e$message)
        })
      }
    }

    msg_prog <- paste0("Cargando serie: ", label_receptora(entidad),
                       if (distrito != "Todos") paste0(" \u2013 D.", distrito) else "",
                       "...")
    shiny::withProgress(
      message = msg_prog,
      value   = NULL,
      carga_loop()
    )

    if (length(filas) == 0) {
      message("❌ [O3] Sin datos para ", entidad)
      return(NULL)
    }

    serie <- tryCatch(
      dplyr::bind_rows(filas),
      error = function(e) do.call(rbind, filas)
    )
    serie[is.na(serie)] <- 0
    serie <- serie[order(serie$fecha), ]
    rownames(serie) <- NULL

    # Guardar en caché
    if (exists("LNE_CACHE_SEMANAL", envir = .GlobalEnv)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache[[clave_cache]] <- serie
      if (is.null(cache$timestamp)) cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }

    message("✅ [O3] ", entidad, " — ", nrow(serie),
            " semanas (OK=", n_ok, " ERR=", n_err, ")")
    serie
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
  # O3 — Widget: 2 filas de filtros independientes + botón Consultar
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o3_controles_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)

    tipo    <- isolate(input$semanal_o3_tipo %||% "lne")
    choices <- choices_origen_tipo(tipo)
    ns_id   <- session$ns("semanal_o3_widget")

    tagList(
      # ── CSS: fuente reducida en dropdowns de este widget ────────────────
      tags$style(HTML(paste0(
        "#", ns_id, " .selectize-input,",
        "#", ns_id, " .selectize-dropdown { font-size:11px !important; }",
        "#", ns_id, " .selectize-dropdown-content .option { ",
        "  font-size:11px !important; padding:3px 8px !important; }"
      ))),
      # ── JS: prevenir salto al header al abrir un dropdown ───────────────
      tags$script(HTML(paste0(
        "(function() {",
        "  var el = document.getElementById('", ns_id, "');",
        "  if (!el) return;",
        "  el.addEventListener('mousedown', function(e) {",
        "    if (e.target.closest && e.target.closest('.selectize-input')) {",
        "      var st = window.pageYOffset || document.documentElement.scrollTop;",
        "      setTimeout(function() { window.scrollTo(0, st); }, 0);",
        "    }",
        "  }, true);",
        "})();"
      ))),

      div(
        id    = ns_id,
        style = paste(
          "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;",
          "padding:8px 14px 10px 14px;margin-bottom:10px;"
        ),

        # ── Fila 0: Vista ────────────────────────────────────────────────
        div(
          style = "display:flex;align-items:center;gap:8px;margin-bottom:8px;",
          tags$span(
            style = "font-size:13px;font-weight:600;color:#2c3e50;white-space:nowrap;",
            "Vista:"
          ),
          radioButtons(
            inputId  = session$ns("semanal_o3_tipo"),
            label    = NULL,
            choices  = c("Lista Nominal Electoral" = "lne",
                         "Padr\u00f3n Electoral"   = "pad"),
            selected = tipo,
            inline   = TRUE
          )
        ),

        # ── Fila 1: Entidad Receptora | Distrito Electoral | Municipio ───
        div(
          style = "display:flex;align-items:flex-end;gap:10px;flex-wrap:nowrap;margin-bottom:8px;",

          div(
            tags$label(
              style = "font-size:11px;font-weight:600;color:#2c3e50;display:block;margin-bottom:2px;",
              "Entidad Receptora"
            ),
            div(
              style = "width:185px;",
              selectInput(
                inputId  = session$ns("semanal_o3_entidad_rec"),
                label    = NULL,
                choices  = c("Nacional" = "Nacional",
                             setNames(ESTADOS_SIDEBAR, LABELS_RECEPTORA)),
                selected = "Nacional",
                width    = "100%"
              )
            )
          ),

          div(
            tags$label(
              style = "font-size:11px;font-weight:600;color:#2c3e50;display:block;margin-bottom:2px;",
              "Distrito Electoral"
            ),
            div(
              style = "width:235px;",
              selectInput(
                inputId  = session$ns("semanal_o3_distrito_rec"),
                label    = NULL,
                choices  = c("Todos" = "Todos"),
                selected = "Todos",
                width    = "100%"
              )
            )
          ),

          div(
            tags$label(
              style = "font-size:11px;font-weight:600;color:#2c3e50;display:block;margin-bottom:2px;",
              "Municipio"
            ),
            div(
              style = "width:210px;",
              selectInput(
                inputId  = session$ns("semanal_o3_municipio_rec"),
                label    = NULL,
                choices  = c("Todos" = "Todos"),
                selected = "Todos",
                width    = "100%"
              )
            )
          )
        ),

        # ── Fila 2: Sección | ● Entidad de Origen | Consultar ────────────
        div(
          style = "display:flex;align-items:flex-end;gap:10px;flex-wrap:nowrap;",

          div(
            tags$label(
              style = "font-size:11px;font-weight:600;color:#2c3e50;display:block;margin-bottom:2px;",
              "Secci\u00f3n Electoral"
            ),
            div(
              style = "width:150px;",
              selectInput(
                inputId  = session$ns("semanal_o3_seccion_rec"),
                label    = NULL,
                choices  = c("Todas" = "Todas"),
                selected = "Todas",
                width    = "100%"
              )
            )
          ),

          # Separador visual
          tags$div(
            style = "width:1px;background:#ccc;align-self:stretch;margin:0 4px 5px 4px;"
          ),

          # Entidad de origen (morado)
          div(
            tags$label(
              style = "font-size:11px;font-weight:600;color:#6c3483;display:block;margin-bottom:2px;",
              "\u25cf Entidad de origen"
            ),
            div(
              style = "width:220px;",
              selectInput(
                inputId  = session$ns("semanal_o3_origen"),
                label    = NULL,
                choices  = choices,
                selected = "todas",
                width    = "100%"
              )
            )
          ),

          # Botón Consultar
          div(
            style = "margin-bottom:5px;",
            actionButton(
              inputId = session$ns("semanal_o3_consultar"),
              label   = "Consultar",
              icon    = icon("search"),
              class   = "btn btn-sm",
              style   = "background-color:#44559B;border-color:#44559B;color:#fff;font-weight:600;"
            )
          )
        )
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — Actualizar choices de origen cuando cambia el tipo LNE/Padrón
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$semanal_o3_tipo, {
    tipo    <- input$semanal_o3_tipo %||% "lne"
    choices <- choices_origen_tipo(tipo)
    updateSelectInput(session, "semanal_o3_origen", choices = choices, selected = "todas")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — Cascada geográfica: Entidad → Distrito → Municipio → Sección
  # ══════════════════════════════════════════════════════════════════════════

  # Helper: fecha semanal más reciente (para el título de la gráfica O3)
  fecha_semanal_o3 <- function() {
    tryCatch({
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      fechas  <- sort(as.Date(catalog$semanal_comun, origin = "1970-01-01"))
      fechas  <- fechas[fechas <= Sys.Date()]
      if (length(fechas) == 0) NULL else max(fechas)
    }, error = function(e) NULL)
  }

  # Helper: fecha histórica más reciente (requerida por get_distritos/municipios/secciones)
  fecha_historica_o3 <- function() {
    tryCatch({
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      if (length(catalog$historico) == 0) return(NULL)
      as.Date(max(catalog$historico), origin = "1970-01-01")
    }, error = function(e) NULL)
  }

  observeEvent(input$semanal_o3_entidad_rec, {
    entidad <- input$semanal_o3_entidad_rec %||% "Nacional"
    if (entidad == "Nacional") {
      updateSelectInput(session, "semanal_o3_distrito_rec",  choices = c("Todos" = "Todos"), selected = "Todos")
      updateSelectInput(session, "semanal_o3_municipio_rec", choices = c("Todos" = "Todos"), selected = "Todos")
      updateSelectInput(session, "semanal_o3_seccion_rec",   choices = c("Todas" = "Todas"), selected = "Todas")
      return()
    }
    fecha     <- fecha_historica_o3()
    distritos <- tryCatch(
      get_distritos_por_entidad(entidad = entidad, fecha = fecha),
      error = function(e) { message("⚠️ [O3 cascada] get_distritos error: ", e$message); c("Todos") }
    )
    # get_distritos_por_entidad ya incluye "Todos" como primer elemento
    updateSelectInput(session, "semanal_o3_distrito_rec",  choices = distritos,          selected = "Todos")
    updateSelectInput(session, "semanal_o3_municipio_rec", choices = c("Todos" = "Todos"), selected = "Todos")
    updateSelectInput(session, "semanal_o3_seccion_rec",   choices = c("Todas" = "Todas"), selected = "Todas")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$semanal_o3_distrito_rec, {
    entidad  <- input$semanal_o3_entidad_rec %||% "Nacional"
    distrito <- input$semanal_o3_distrito_rec %||% "Todos"
    if (entidad == "Nacional" || distrito == "Todos") {
      updateSelectInput(session, "semanal_o3_municipio_rec", choices = c("Todos" = "Todos"), selected = "Todos")
      updateSelectInput(session, "semanal_o3_seccion_rec",   choices = c("Todas" = "Todas"), selected = "Todas")
      return()
    }
    fecha      <- fecha_historica_o3()
    municipios <- tryCatch(
      get_municipios_por_distrito(entidad = entidad, distrito = distrito, fecha = fecha),
      error = function(e) { message("⚠️ [O3 cascada] get_municipios error: ", e$message); c("Todos") }
    )
    # get_municipios_por_distrito ya incluye "Todos" como primer elemento
    updateSelectInput(session, "semanal_o3_municipio_rec", choices = municipios,          selected = "Todos")
    updateSelectInput(session, "semanal_o3_seccion_rec",   choices = c("Todas" = "Todas"), selected = "Todas")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$semanal_o3_municipio_rec, {
    entidad   <- input$semanal_o3_entidad_rec  %||% "Nacional"
    distrito  <- input$semanal_o3_distrito_rec  %||% "Todos"
    municipio <- input$semanal_o3_municipio_rec %||% "Todos"
    if (entidad == "Nacional" || municipio == "Todos") {
      updateSelectInput(session, "semanal_o3_seccion_rec", choices = c("Todas" = "Todas"), selected = "Todas")
      return()
    }
    fecha     <- fecha_historica_o3()
    secciones <- tryCatch(
      get_secciones_por_municipio(entidad   = entidad,   distrito  = distrito,
                                  municipio = municipio, fecha     = fecha),
      error = function(e) { message("⚠️ [O3 cascada] get_secciones error: ", e$message); c("Todas") }
    )
    # get_secciones_por_municipio ya incluye "Todas" como primer elemento
    updateSelectInput(session, "semanal_o3_seccion_rec", choices = secciones, selected = "Todas")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — Reactive: serie temporal (cargada por filtros propios + botón Consultar)
  # ══════════════════════════════════════════════════════════════════════════

  serie_o3_r <- reactive({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    entidad   <- input$semanal_o3_entidad_rec  %||% "Nacional"
    distrito  <- input$semanal_o3_distrito_rec  %||% "Todos"
    municipio <- input$semanal_o3_municipio_rec %||% "Todos"
    seccion   <- input$semanal_o3_seccion_rec   %||% "Todas"
    ambito    <- ambito_reactivo()
    cargar_serie_o3(entidad, distrito, municipio, seccion, ambito)
  }) %>% bindEvent(
    input$semanal_o3_consultar, ambito_reactivo(),
    ignoreNULL = FALSE, ignoreInit = FALSE
  )

  # Contenedor con altura fija para O3
  output$semanal_o3_grafica_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    withSpinner(
      plotlyOutput(session$ns("semanal_o3_evolucion"), height = "460px"),
      type = 4, color = "#44559B", size = 0.8
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — Gráfica: Evolución semanal LNE/Padrón por origen × receptor
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o3_evolucion <- renderPlotly({
    if (es_historico() || desglose_activo() != "origen") return(NULL)

    ambito    <- ambito_reactivo()
    etiq      <- etiq_ambito(ambito)
    serie     <- serie_o3_r()
    tipo      <- input$semanal_o3_tipo         %||% "lne"
    origen    <- input$semanal_o3_origen       %||% "todas"
    receptora <- input$semanal_o3_entidad_rec  %||% "Nacional"
    distrito  <- input$semanal_o3_distrito_rec  %||% "Todos"
    municipio <- input$semanal_o3_municipio_rec %||% "Todos"
    seccion   <- input$semanal_o3_seccion_rec   %||% "Todas"

    # Alcance construido desde los filtros propios de O3 (no del sidebar)
    alcance_o3 <- paste0(
      "Entidad receptora: ", label_receptora(receptora),
      " \u2013 Distrito: ", distrito,
      " \u2013 Municipio: ", municipio,
      " \u2013 Secci\u00f3n: ", seccion
    )

    if (is.null(serie) || nrow(serie) < 2)
      return(plot_vacio("Sin datos de serie temporal"))

    # Usar la fecha del catálogo (igual que O1/O2), no max(serie$fecha)
    # para evitar mostrar fechas de caché desactualizadas.
    fecha_cat <- fecha_semanal_o3()
    etiq_fecha <- if (!is.null(fecha_cat)) fecha_es(fecha_cat)
                  else as.character(anio_semanal())

    # Columnas disponibles según tipo (lne / pad)
    col_prefix <- if (tipo == "lne") "ln" else "pad"
    cols_tipo  <- detectar_cols_origen(serie, col_prefix)
    if (length(cols_tipo) == 0)
      return(plot_vacio("Sin columnas de datos disponibles"))

    # Construir vector de valores Y
    if (origen == "todas") {
      y_vals            <- rowSums(serie[, cols_tipo, drop = FALSE], na.rm = TRUE)
      nombre_origen_sel <- "Todas las entidades de origen"
    } else {
      if (!origen %in% colnames(serie))
        return(plot_vacio(paste0("Columna '", origen, "' no disponible")))
      y_vals            <- as.numeric(serie[[origen]])
      nombre_origen_sel <- nombre_origen(origen)
    }

    color_linea <- if (tipo == "lne")
      (if (ambito == "extranjero") "#71A251" else "#AE0E35")
    else
      (if (ambito == "extranjero") "#D4A500" else "#003E66")

    etiq_yaxis     <- if (tipo == "lne") "Lista Nominal Electoral"
                      else "Padr\u00f3n Electoral"
    receptora_lbl  <- label_receptora(receptora)
    legend_nombre  <- if (origen == "todas")
      paste0(etiq_yaxis, " \u2013 ", receptora_lbl)
    else
      paste0(nombre_origen_sel, " \u2192 ", receptora_lbl)

    titulo <- paste0(
      "Evoluci\u00f3n de Padr\u00f3n y LNE seg\u00fan entidad de origen",
      " y entidad receptora \u2013 ", etiq_fecha, " \u2013 ", etiq
    )

    plot_ly(
      x    = serie$fecha,
      y    = y_vals,
      type = "scatter", mode = "lines+markers",
      name = legend_nombre,
      line   = list(color = color_linea, width = 2.5),
      marker = list(size  = 6, color = color_linea),
      hovertemplate = paste0(
        "<b>%{x|%d %b %Y}</b><br>",
        etiq_yaxis, ": <b>%{y:,.0f}</b><extra></extra>"
      )
    ) %>%
      layout(
        title = list(
          text   = titulo,
          font   = list(size = 15, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center", y = 0.97
        ),
        xaxis = list(
          title      = "",
          type       = "date",
          tickformat = "%d %b %Y",
          tickangle  = -30,
          tickfont   = list(size = 10)
        ),
        yaxis = list(
          title             = etiq_yaxis,
          separatethousands = TRUE,
          tickfont          = list(size = 10)
        ),
        legend    = list(orientation = "h", xanchor = "center",
                         x = 0.5, y = -0.18),
        hovermode = "x unified",
        height    = 460,
        margin    = list(t = 140, b = 100, l = 90, r = 40),
        annotations = list(ann_alcance(alcance_o3, y_pos = 1.10))
      )
  }) %>%
    bindEvent(
      input$semanal_o3_consultar, ambito_reactivo(),
      input$semanal_o3_tipo, input$semanal_o3_origen,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  # ══════════════════════════════════════════════════════════════════════════
  # O3 — Footer dinámico: leyendas LN87/88 | PAD87/88 según vista activa
  # ══════════════════════════════════════════════════════════════════════════

  output$semanal_o3_leyenda_ui <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    tipo <- input$semanal_o3_tipo %||% "lne"
    div(
      style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:6px 0 2px 0;line-height:1.9;",
      if (tipo == "lne") {
        tagList(
          tags$div(
            tags$b("LN87:"),
            " Lista Nominal Electoral de ciudadanos mexicanos nacidos en el extranjero, residentes en la entidad",
            "\u2003|\u2003",
            tags$b("LN88:"),
            " Lista Nominal de ciudadanos naturalizados mexicanos, residentes en la entidad"
          )
        )
      } else {
        tagList(
          tags$div(
            tags$b("PAD87:"),
            " Padr\u00f3n Electoral de ciudadanos mexicanos nacidos en el extranjero, residentes en la entidad",
            "\u2003|\u2003",
            tags$b("PAD88:"),
            " Padr\u00f3n Electoral de ciudadanos naturalizados mexicanos, residentes en la entidad"
          )
        )
      },
      tags$div(
        style = "margin-top:4px;",
        "Fuente: INE. Estad\u00edstica de Padr\u00f3n Electoral y Lista Nominal del Electorado"
      )
    )
  })

  message("✅ graficas_semanal_origen v3.0 inicializado")
  message("   O1: calor LNE — 32 estados + LN87/LN88 en eje Y, default Todos")
  message("   O2: calor Padrón vs LNE — absoluto (subplots) | dif. (Padrón−LNE)")
  message("   O3: evolución semanal LNE/Padrón por origen × receptor (v3.0)")
}
