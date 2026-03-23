# modules/lista_nominal_graficas/graficas_semanal.R
# Orquestador de la Vista Semanal
# Versión: 3.6 — Fix definitivo: Nacional=suma 71k secciones; Extranjero=suma 32 EXT
#
# CAMBIOS vs v3.2:
#   - fila_ext(): ahora detecta filas de RESIDENTES EXTRANJERO también por firma
#     estructural (cve_distrito=0, cve_municipio=0, seccion=0) cuando el campo
#     nombre_entidad contiene el nombre de la entidad en lugar del texto especial.
#   - df_nacional(): usa nuevo helper es_fila_extranjero() que combina detección
#     por nombre Y por firma. Elimina el doble conteo que causaba discrepancias
#     entre E2/E4/DataTable y E1/E3 para consultas a nivel entidad o con ámbito nacional.
#
# CAMBIOS vs v3.1:
#   - datos_dt_edad_r: columnas simplificadas → Rango de Edad, Padrón Total,
#     LNE Total, Tasa de Inclusión (%). Sin columnas de sexo (corresponden a
#     la vista Sexo, no a Edad).
#   - output$semanal_dt_edad: dom='lfrtip', language en español completo,
#     formateo de miles y decimales igual al histórico.
#   - output$semanal_dt_edad_header: nuevo renderUI con header ámbito + alcance
#     (patrón idéntico a main-table_header del histórico).
#   - output$semanal_dt_edad_descarga: downloadHandler movido aquí (era solo UI
#     en graficas_ui_render.R); nombre de archivo descriptivo.

graficas_semanal <- function(input, output, session,
                             datos_semanal_edad,
                             datos_semanal_sexo,
                             datos_semanal_origen,
                             datos_semanal_serie_edad,
                             datos_semanal_serie_sexo,
                             datos_semanal_serie_origen,
                             anio_semanal,
                             fecha_semanal_efectiva,
                             texto_alcance,
                             ambito_reactivo,
                             estado_app) {
  
  message("📊 Inicializando graficas_semanal v3.1")
  
  # ════════════════════════════════════════════════════════════════════════════
  # CONSTANTES
  # ════════════════════════════════════════════════════════════════════════════
  
  COLORES <- list(
    nac_padron  = "#003E66", nac_lista   = "#AE0E35",
    nac_hombres = "#44559B", nac_mujeres = "#C0311A",
    ext_padron  = "#FFD14A", ext_lista   = "#71A251",
    ext_hombres = "#D4A500", ext_mujeres = "#8FB369"
  )
  FUENTE_INE <- "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado"
  ORDEN_EDAD <- c("18","19","20_24","25_29","30_34","35_39",
                  "40_44","45_49","50_54","55_59","60_64","65_y_mas")
  
  # ════════════════════════════════════════════════════════════════════════════
  # HELPERS COMPARTIDOS
  # (disponibles para graficas_semanal_edad.R y graficas_semanal_sexo.R
  #  a través del entorno padre de la función)
  # ════════════════════════════════════════════════════════════════════════════
  
  fmt_num <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE)
  fmt_pct <- function(x) paste0(sprintf("%.2f", round(as.numeric(x), 2)), "%")
  
  color_padron <- function(a) if (a == "extranjero") COLORES$ext_padron  else COLORES$nac_padron
  color_lista  <- function(a) if (a == "extranjero") COLORES$ext_lista   else COLORES$nac_lista
  color_h      <- function(a) if (a == "extranjero") COLORES$ext_hombres else COLORES$nac_hombres
  color_m      <- function(a) if (a == "extranjero") COLORES$ext_mujeres else COLORES$nac_mujeres
  etiq_ambito  <- function(a) if (a == "extranjero") "Extranjero" else "Nacional"
  
  ann_fuente <- function(y_pos = -0.18) list(
    text = FUENTE_INE, x = 0.5, y = y_pos, xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "top", showarrow = FALSE,
    font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
    align = "center"
  )
  ann_alcance <- function(texto, y_pos = 1.10) list(
    text = texto, x = 0.5, y = y_pos, xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "top", showarrow = FALSE,
    font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
    align = "center"
  )
  plot_vacio <- function(msg = "No hay datos disponibles") {
    plot_ly() %>% layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      annotations = list(list(
        text = msg, xref = "paper", yref = "paper",
        x = 0.5, y = 0.5, xanchor = "center", yanchor = "middle",
        showarrow = FALSE, font = list(size = 14, color = "#888")
      ))
    )
  }
  etiqueta_edad <- function(g) gsub("_", "-", gsub("_y_mas", "+", g))
  
  es_historico    <- function() { tc <- input$tipo_corte %||% "historico"; tc != "semanal" }
  desglose_activo <- function() input$desglose %||% "edad"
  
  # ── HELPERS ARITMÉTICOS ────────────────────────────────────────────────────
  #
  # ESTRUCTURA DEL CSV (verificada contra derfe_pdln_20251016_edad.csv):
  #
  #   71,632 filas de secciones nacionales  → Vista Nacional
  #      32 filas RESIDENTES EXTRANJERO     → Vista Extranjero
  #         (cabecera_distrital == "RESIDENTES EXTRANJERO")
  #       1 fila TOTALES (cve_entidad = NA) → solo secciones nac (NO incluye EXT)
  #
  #   cargar_lne() extrae y elimina la fila TOTALES de res$datos antes de devolver.
  #   El df recibido tiene 71,664 filas: 71,632 secciones nac + 32 EXT.
  #
  #   NACIONAL   = sumar las 71,632 secciones (excluir EXT y TOTALES si existiera)
  #   EXTRANJERO = sumar las 32 filas EXT
  #
  #   Totales correctos para 20251016:
  #     Nacional:   Padrón=99,939,828  LNE=99,071,246
  #     Extranjero: Padrón= 1,646,050  LNE=   769,903
  # ────────────────────────────────────────────────────────────────────────────
  
  # Identificador canónico de filas EXT
  es_fila_extranjero <- function(df) {
    if (!"cabecera_distrital" %in% colnames(df)) return(rep(FALSE, nrow(df)))
    grepl("RESIDENTES EXTRANJERO", toupper(trimws(df$cabecera_distrital)), fixed = TRUE)
  }
  
  # Las 32 filas EXT
  filas_ext <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df[es_fila_extranjero(df), , drop = FALSE]
  }
  
  # Las 71,632 secciones nacionales (sin EXT, sin fila TOTALES)
  filas_nacionales <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    mask_excl <- es_fila_extranjero(df)
    if ("cve_entidad" %in% colnames(df)) mask_excl <- mask_excl | is.na(df$cve_entidad)
    df[!mask_excl, , drop = FALSE]
  }
  
  # Alias de compatibilidad con código heredado
  df_nacional      <- filas_nacionales
  fila_totales_nac <- function(df) NULL  # obsoleto
  fila_totales_csv <- function(df) NULL  # obsoleto
  fila_ext <- function(df) {
    filas <- filas_ext(df)
    if (is.null(filas) || nrow(filas) == 0) return(NULL)
    cols_id  <- c("cve_entidad","nombre_entidad","cve_distrito","cabecera_distrital",
                  "cve_municipio","nombre_municipio","seccion")
    cols_num <- setdiff(colnames(filas), cols_id)
    cols_num <- cols_num[sapply(filas[, cols_num, drop = FALSE], is.numeric)]
    ag <- as.data.frame(
      lapply(filas[, cols_num, drop = FALSE], function(x) sum(as.numeric(x), na.rm = TRUE)),
      stringsAsFactors = FALSE)
    ag$nombre_entidad <- "RESIDENTES EXTRANJERO"
    ag$cabecera_distrital <- "RESIDENTES EXTRANJERO"
    ag
  }
  
  # Valor de un rango de edad (h+m+nb) en una fila o conjunto de filas
  sumar_rango <- function(df, grupo, sexo, tipo) {
    col <- grep(paste0("^", tipo, "_", grupo, "_", sexo, "$"),
                colnames(df), value = TRUE, ignore.case = TRUE)
    if (length(col) == 0) return(0L)
    sum(as.numeric(df[[col[1]]]), na.rm = TRUE)
  }
  
  # Valor de un rango (h+m+nb sumados) en una sola fila o conjunto
  sumar_rango_total <- function(df, grupo, tipo) {
    sumar_rango(df, grupo, "hombres",    tipo) +
      sumar_rango(df, grupo, "mujeres",    tipo) +
      sumar_rango(df, grupo, "no_binario", tipo)
  }
  
  # construir_df_edad(): una fila por rango con columnas h/m/nb
  #   Nacional   → sumar las 71,632 secciones (sin EXT, sin TOTALES)
  #   Extranjero → sumar las 32 filas EXT
  construir_df_edad <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    filas <- if (ambito == "extranjero") filas_ext(df) else filas_nacionales(df)
    if (is.null(filas) || nrow(filas) == 0) return(NULL)
    do.call(rbind, lapply(ORDEN_EDAD, function(g) data.frame(
      grupo             = etiqueta_edad(g),
      padron_hombres    = sumar_rango(filas, g, "hombres",    "padron"),
      padron_mujeres    = sumar_rango(filas, g, "mujeres",    "padron"),
      padron_no_binario = sumar_rango(filas, g, "no_binario", "padron"),
      lista_hombres     = sumar_rango(filas, g, "hombres",    "lista"),
      lista_mujeres     = sumar_rango(filas, g, "mujeres",    "lista"),
      lista_no_binario  = sumar_rango(filas, g, "no_binario", "lista"),
      stringsAsFactors = FALSE
    )))
  }
  # extraer_totales_sexo(): totales h/m/nb para E2-sexo y DataTable
  #   Nacional   → sumar las 71,632 secciones (sin EXT)
  #   Extranjero → sumar las 32 filas EXT
  extraer_totales_sexo <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    cols_sex <- c("padron_hombres","padron_mujeres","padron_no_binario",
                  "lista_hombres","lista_mujeres","lista_no_binario")
    filas <- if (ambito == "extranjero") filas_ext(df) else filas_nacionales(df)
    if (is.null(filas) || nrow(filas) == 0) return(NULL)
    cols_num <- intersect(cols_sex, colnames(filas))
    as.data.frame(
      lapply(filas[, cols_num, drop = FALSE], function(x) sum(as.numeric(x), na.rm = TRUE)),
      stringsAsFactors = FALSE)
  }
  NOM_ORIGEN <- c(
    "01"="AGUASCALIENTES","02"="BAJA CALIFORNIA","03"="BAJA CALIFORNIA SUR",
    "04"="CAMPECHE","05"="COAHUILA","06"="COLIMA","07"="CHIAPAS",
    "08"="CHIHUAHUA","09"="CIUDAD DE MEXICO","10"="DURANGO",
    "11"="GUANAJUATO","12"="GUERRERO","13"="HIDALGO","14"="JALISCO",
    "15"="MEXICO","16"="MICHOACAN","17"="MORELOS","18"="NAYARIT",
    "19"="NUEVO LEON","20"="OAXACA","21"="PUEBLA","22"="QUERETARO",
    "23"="QUINTANA ROO","24"="SAN LUIS POTOSI","25"="SINALOA","26"="SONORA",
    "27"="TABASCO","28"="TAMAULIPAS","29"="TLAXCALA","30"="VERACRUZ",
    "31"="YUCATAN","32"="ZACATECAS",
    "87"="Mexicanos nacidos en el extranjero",
    "88"="Ciudadanos naturalizados"
  )
  # Mapa sufijo de columna → nombre completo (espejo de MAPA_NOMBRE en graficas_semanal_origen.R)
  .NOM_ESTADO_ORIGEN <- c(
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

  construir_tabla_origen <- function(df, ambito) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df_uso <- if (ambito == "extranjero")
      df[es_fila_extranjero(df), , drop = FALSE]
    else df_nacional(df)
    if (is.null(df_uso) || nrow(df_uso) == 0) return(NULL)

    # Mismo patrón que detectar_cols_origen() en graficas_semanal_origen.R:
    # columnas con nombre de estado completo (ln_aguascalientes) + extranjero (ln87/ln88)
    cols_ln <- grep("^ln_[a-z]|^ln87$|^ln88$", colnames(df_uso),
                    value = TRUE, ignore.case = TRUE)
    if (length(cols_ln) == 0) {
      message("\u26a0\ufe0f [origen] Sin columnas de LN detectadas"); return(NULL)
    }

    res <- do.call(rbind, lapply(cols_ln, function(cl) {
      col_pad <- gsub("^ln_", "pad_",
                      gsub("^ln(8[78])$", "pad\\1", cl, ignore.case = TRUE),
                      ignore.case = TRUE)
      suf <- gsub("^ln_|^ln", "", cl, ignore.case = TRUE)
      nombre <- if (suf == "87") "Mexicanos nacidos en el extranjero"
                else if (suf == "88") "Ciudadanos naturalizados"
                else .NOM_ESTADO_ORIGEN[suf] %||% paste0("(", suf, ")")
      data.frame(
        entidad_origen = nombre,
        padron         = if (col_pad %in% colnames(df_uso))
          sum(as.numeric(df_uso[[col_pad]]), na.rm = TRUE) else NA_real_,
        lista_nominal  = sum(as.numeric(df_uso[[cl]]), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    res[order(res$lista_nominal, decreasing = TRUE, na.last = TRUE), ]
  }
  
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
  
  texto_subtitulo <- function() {
    if (estado_app() == "restablecido")
      return("Vista: Nacional — sin filtros aplicados")
    gsub(" - ", " – ", isolate(texto_alcance()))
  }
  
  css_h4 <- "margin:22px 0 6px 0;font-size:17px;color:#2c3e50;font-weight:600;border-bottom:1px solid #e0e0e0;padding-bottom:4px;"
  css_p  <- "margin:0 0 8px 0;font-size:15px;line-height:1.6;color:#333;"
  css_na <- "text-align:center;color:#999;padding:10px;font-style:italic;font-size:14px;"
  
  # ════════════════════════════════════════════════════════════════════════════
  # SUB-MÓDULOS: cargar e inicializar gráficas por categoría
  # Los source() van AQUÍ (dentro de graficas_semanal) para que los sub-módulos
  # hereden el entorno de esta función y accedan a los helpers compartidos:
  # es_historico(), desglose_activo(), construir_df_edad(), COLORES, etc.
  # ════════════════════════════════════════════════════════════════════════════
  
  source("modules/lista_nominal_graficas/graficas_semanal_edad.R", local = TRUE)
  source("modules/lista_nominal_graficas/graficas_semanal_sexo.R", local = TRUE)
  source("modules/lista_nominal_graficas/graficas_semanal_origen.R", local = TRUE)
  
  graficas_semanal_edad(
    input                    = input,
    output                   = output,
    session                  = session,
    datos_semanal_edad       = datos_semanal_edad,
    datos_semanal_serie_edad = datos_semanal_serie_edad,
    anio_semanal             = anio_semanal,
    fecha_semanal_efectiva   = fecha_semanal_efectiva,
    texto_alcance            = texto_alcance,
    ambito_reactivo          = ambito_reactivo,
    estado_app               = estado_app
  )
  
  graficas_semanal_sexo(
    input                    = input,
    output                   = output,
    session                  = session,
    datos_semanal_edad       = datos_semanal_edad,
    datos_semanal_sexo       = datos_semanal_sexo,
    datos_semanal_serie_sexo = datos_semanal_serie_sexo,
    anio_semanal             = anio_semanal,
    texto_alcance            = texto_alcance,
    ambito_reactivo          = ambito_reactivo,
    estado_app               = estado_app
  )
  
  o3_mod <- graficas_semanal_origen(
    input                       = input,
    output                      = output,
    session                     = session,
    datos_semanal_origen        = datos_semanal_origen,
    datos_semanal_serie_origen  = datos_semanal_serie_origen,
    anio_semanal                = anio_semanal,
    texto_alcance               = texto_alcance,
    ambito_reactivo             = ambito_reactivo,
    estado_app                  = estado_app
  )
  
  # ════════════════════════════════════════════════════════════════════════════
  # UI: TÍTULO Y SUBTÍTULOS
  # ════════════════════════════════════════════════════════════════════════════
  
  output$semanal_titulo_principal <- renderUI({
    if (es_historico()) return(NULL)
    div(
      style = "text-align:center;margin-bottom:10px;",
      h3(paste0(anio_semanal(), " - Padrón y Lista Nominal Electoral - ",
                etiq_ambito(ambito_reactivo())),
         style = "color:#2c3e50;font-family:Arial,sans-serif;font-weight:700;")
    )
  })
  output$semanal_subtitulo_edad <- renderUI({
    # Eliminado en v3.1: el texto de alcance aparece como anotación
    # dentro de cada gráfica (ann_alcance). No se duplica en el encabezado.
    return(NULL)
  })
  output$semanal_subtitulo_sexo <- renderUI({
    if (es_historico()) return(NULL)
    p(class = "text-muted",
      style = "font-size:13px;text-align:center;margin-bottom:6px;",
      texto_subtitulo())
  })
  output$semanal_subtitulo_origen <- renderUI({
    if (es_historico()) return(NULL)
    p(class = "text-muted",
      style = "font-size:13px;text-align:center;margin-bottom:6px;",
      texto_subtitulo())
  })
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # GRÁFICAS DE ORIGEN → graficas_semanal_origen.R
  # (semanal_o1_calor y semanal_o2_proyeccion — ver sub-módulo)
  # ════════════════════════════════════════════════════════════════════════════
  
  # ════════════════════════════════════════════════════════════════════════════
  # DATATABLES
  # ════════════════════════════════════════════════════════════════════════════
  
  datos_dt_edad_r <- reactive({
    if (es_historico()) return(NULL)
    df <- construir_df_edad(datos_semanal_edad(), ambito_reactivo())
    if (is.null(df)) return(NULL)
    # Incluir no_binario para que los totales coincidan con E1/E3
    nb_p <- if ("padron_no_binario" %in% colnames(df)) df$padron_no_binario else 0
    nb_l <- if ("lista_no_binario"  %in% colnames(df)) df$lista_no_binario  else 0
    df$padron_total <- df$padron_hombres + df$padron_mujeres + nb_p
    df$lista_total  <- df$lista_hombres  + df$lista_mujeres  + nb_l
    df$tasa         <- round(df$lista_total / df$padron_total * 100, 2)
    # Solo columnas relevantes para la vista Edad (sin desglose por sexo)
    data.frame(
      `Rango de Edad`        = df$grupo,
      `Padrón Total`         = df$padron_total,
      `LNE Total`            = df$lista_total,
      `Tasa de Inclusión (%)` = df$tasa,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  
  # Header: ámbito + alcance (patrón idéntico a main-table_header histórico)
  output$semanal_dt_edad_header <- renderUI({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    df <- datos_dt_edad_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    ambito_display <- etiq_ambito(ambito_reactivo())
    alcance_texto  <- isolate(texto_alcance())
    tags$div(
      class = "datatable-header-content",
      style = "text-align:center;margin-bottom:15px;padding:10px;background-color:#f8f9fa;border-radius:6px;border:1px solid #e9ecef;",
      tags$div(
        style = "font-size:15px;font-weight:700;color:#006988;margin-bottom:5px;",
        paste0("Ámbito: ", ambito_display)
      ),
      tags$div(
        style = "font-size:12px;color:#555555;line-height:1.4;",
        alcance_texto
      )
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  output$semanal_dt_edad <- DT::renderDataTable({
    df <- datos_dt_edad_r()
    if (is.null(df))
      return(DT::datatable(data.frame(Mensaje = "Sin datos"),
                           options = list(dom = "t")))
    DT::datatable(
      df,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        lengthMenu = list(c(10, 15, 25, -1), c("10", "15", "25", "Todos")),
        dom        = "lfrtip",
        autoWidth  = FALSE,
        language   = list(
          search     = "Buscar:",
          lengthMenu = "Mostrar _MENU_",
          info       = "Mostrando _START_ a _END_ de _TOTAL_",
          infoEmpty  = "Sin registros",
          infoFiltered = "(de _MAX_ totales)",
          paginate   = list(
            first    = "«",
            last     = "»",
            `next`   = "›",
            previous = "‹"
          )
        )
      )
    ) %>%
      DT::formatRound(c("Padrón Total", "LNE Total"), digits = 0) %>%
      DT::formatRound("Tasa de Inclusión (%)", digits = 2)
  })
  
  output$semanal_dt_edad_descarga <- downloadHandler(
    filename = function() {
      paste0("sefix_semanal_edad_",
             etiq_ambito(ambito_reactivo()), "_",
             anio_semanal(), "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- datos_dt_edad_r()
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )
  
  datos_dt_sexo_r <- reactive({
    if (es_historico()) return(NULL)
    tot <- extraer_totales_sexo(datos_semanal_sexo(), ambito_reactivo())
    if (is.null(tot)) return(NULL)
    ph <- tot$padron_hombres; pm <- tot$padron_mujeres
    lh <- tot$lista_hombres;  lm <- tot$lista_mujeres
    nb_p <- tot$padron_no_binario %||% 0
    nb_l <- tot$lista_no_binario  %||% 0
    data.frame(
      Sexo = c("Hombres", "Mujeres", "No Binario"),
      `Padrón Electoral`      = c(ph, pm, nb_p),
      `Lista Nominal`         = c(lh, lm, nb_l),
      `Tasa de Inclusión (%)` = c(
        if (!is.na(ph) && ph > 0) round(lh / ph * 100, 2) else NA,
        if (!is.na(pm) && pm > 0) round(lm / pm * 100, 2) else NA,
        if (!is.na(nb_p) && nb_p > 0) round(nb_l / nb_p * 100, 2) else NA
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })

  # Header: ámbito + alcance (patrón idéntico a semanal_dt_edad_header)
  output$semanal_dt_sexo_header <- renderUI({
    if (es_historico() || desglose_activo() != "sexo") return(NULL)
    df <- datos_dt_sexo_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    ambito_display <- etiq_ambito(ambito_reactivo())
    alcance_texto  <- isolate(texto_alcance())
    tags$div(
      class = "datatable-header-content",
      style = "text-align:center;margin-bottom:15px;padding:10px;background-color:#f8f9fa;border-radius:6px;border:1px solid #e9ecef;",
      tags$div(
        style = "font-size:15px;font-weight:700;color:#006988;margin-bottom:5px;",
        paste0("\u00c1mbito: ", ambito_display)
      ),
      tags$div(
        style = "font-size:12px;color:#555555;line-height:1.4;",
        alcance_texto
      )
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  output$semanal_dt_sexo <- DT::renderDataTable({
    df <- datos_dt_sexo_r()
    if (is.null(df))
      return(DT::datatable(data.frame(Mensaje = "Sin datos"),
                           options = list(dom = "t")))
    DT::datatable(
      df,
      rownames = FALSE,
      options  = list(
        pageLength = 10,
        lengthMenu = list(c(10, -1), c("10", "Todos")),
        dom        = "lfrtip",
        autoWidth  = FALSE,
        language   = list(
          search       = "Buscar:",
          lengthMenu   = "Mostrar _MENU_",
          info         = "Mostrando _START_ a _END_ de _TOTAL_",
          infoEmpty    = "Sin registros",
          infoFiltered = "(de _MAX_ totales)",
          paginate     = list(
            first    = "\u00ab",
            last     = "\u00bb",
            `next`   = "\u203a",
            previous = "\u2039"
          )
        )
      )
    ) %>%
      DT::formatRound(c("Padr\u00f3n Electoral", "Lista Nominal"), digits = 0) %>%
      DT::formatRound("Tasa de Inclusi\u00f3n (%)", digits = 2)
  })

  output$semanal_dt_sexo_descarga <- downloadHandler(
    filename = function() {
      paste0("sefix_semanal_sexo_",
             etiq_ambito(ambito_reactivo()), "_",
             anio_semanal(), "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content  = function(file) {
      df <- datos_dt_sexo_r()
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )
  
  datos_dt_origen_r <- reactive({
    if (es_historico()) return(NULL)
    tabla <- construir_tabla_origen(datos_semanal_origen(), ambito_reactivo())
    if (is.null(tabla)) return(NULL)
    tabla$tasa <- round(tabla$lista_nominal / tabla$padron * 100, 2)
    colnames(tabla) <- c("Estado de Origen","Padrón Electoral",
                         "Lista Nominal","Tasa Inclusión (%)")
    tabla
  })
  output$semanal_dt_origen_header <- renderUI({
    if (es_historico() || desglose_activo() != "origen") return(NULL)
    df <- datos_dt_origen_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    ambito_display <- etiq_ambito(ambito_reactivo())
    alcance_texto  <- isolate(texto_alcance())
    tags$div(
      class = "datatable-header-content",
      style = "text-align:center;margin-bottom:15px;padding:10px;background-color:#f8f9fa;border-radius:6px;border:1px solid #e9ecef;",
      tags$div(
        style = "font-size:15px;font-weight:700;color:#006988;margin-bottom:5px;",
        paste0("Ámbito: ", ambito_display)
      ),
      tags$div(
        style = "font-size:12px;color:#555555;line-height:1.4;",
        alcance_texto
      )
    )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

  output$semanal_dt_origen <- DT::renderDataTable({
    df <- datos_dt_origen_r()
    if (is.null(df))
      return(DT::datatable(data.frame(Mensaje = "Sin datos"),
                           options = list(dom = "t")))
    DT::datatable(
      df,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        lengthMenu = list(c(10, 15, 25, -1), c("10", "15", "25", "Todos")),
        dom        = "lfrtip",
        autoWidth  = FALSE,
        language   = list(
          search       = "Buscar:",
          lengthMenu   = "Mostrar _MENU_",
          info         = "Mostrando _START_ a _END_ de _TOTAL_",
          infoEmpty    = "Sin registros",
          infoFiltered = "(de _MAX_ totales)",
          paginate     = list(
            first    = "«",
            last     = "»",
            `next`   = "›",
            previous = "‹"
          )
        )
      )
    ) %>%
      DT::formatRound(c("Padrón Electoral", "Lista Nominal"), digits = 0) %>%
      DT::formatRound("Tasa Inclusión (%)", digits = 2)
  })
  output$semanal_dt_origen_descarga <- downloadHandler(
    filename = function() paste0("sefix_origen_", etiq_ambito(ambito_reactivo()),
                                 "_", anio_semanal(), "_",
                                 format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      df <- datos_dt_origen_r()
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ════════════════════════════════════════════════════════════════════════════
  # SIDEBAR DERECHO — análisis textual condicional por desglose
  # Sin cambios vs v2.3
  # ════════════════════════════════════════════════════════════════════════════
  
  output$semanal_texto_titulo <- renderUI({
    if (es_historico()) return(NULL)
    anio     <- anio_semanal()
    desglose <- desglose_activo()
    subtitulo <- switch(desglose,
      "edad"   = "Rangos de Edad",
      "sexo"   = "Distribuci\u00f3n por Sexo",
      "origen" = "Entidad de Origen",
      "Rangos de Edad"
    )
    HTML(paste0(
      "<div style='font-size:16px;line-height:1.6;color:#333;'>",
      "<h3 style='text-align:center;margin:0 0 2px 0;font-size:18px;",
      "color:#2c3e50;font-weight:600;line-height:1.4;'>",
      "An\u00e1lisis de la evoluci\u00f3n semanal del Padr\u00f3n y Lista Nominal Electoral",
      "<span style='display:block;text-align:center;color:#1a5276;",
      "font-weight:700;font-size:20px;margin-top:4px;margin-bottom:6px;'>",
      anio, "</span></h3>",
      "<p style='text-align:center;margin:0 0 12px 0;font-size:16px;",
      "color:#1a5276;font-weight:600;'>", subtitulo, "</p>",
      "<p style='margin:0 0 2px 0;font-size:14px;color:#555;",
      "font-weight:600;text-align:left;'>Alcance del an\u00e1lisis:</p>",
      "<p style='text-align:left;margin:0;font-size:14px;color:#777;line-height:1.4;'>",
      gsub(" - ", " \u2013 ", texto_alcance()), "</p></div>"
    ))
  })
  
  output$semanal_texto_analisis <- renderUI({
    if (es_historico()) return(NULL)
    ambito   <- ambito_reactivo()
    etiq     <- etiq_ambito(ambito)
    desglose <- desglose_activo()
    
    contenido <- switch(desglose,
                        
                        "edad" = tryCatch({
                          df <- construir_df_edad(datos_semanal_edad(), ambito)
                          if (is.null(df) || nrow(df) == 0)
                            return(HTML(paste0("<p style='", css_na, "'>Sin datos de edad disponibles.</p>")))

                          df$lst <- df$lista_hombres + df$lista_mujeres + df$lista_no_binario
                          df$pad <- df$padron_hombres + df$padron_mujeres + df$padron_no_binario
                          tot_lne  <- sum(df$lst, na.rm = TRUE)
                          tot_pad  <- sum(df$pad, na.rm = TRUE)
                          tasa_inc <- if (tot_pad > 0) round(tot_lne / tot_pad * 100, 2) else NA

                          # ── Comparación primera / última fecha via serie ──────────────
                          meses_es <- c("enero","febrero","marzo","abril","mayo","junio",
                                        "julio","agosto","septiembre","octubre","noviembre","diciembre")
                          fmt_fecha_es <- function(d) paste(
                            as.integer(format(d, "%d")),
                            meses_es[as.integer(format(d, "%m"))],
                            format(d, "%Y"))

                          txt_variacion <- tryCatch({
                            serie <- datos_semanal_serie_edad()
                            if (is.null(serie) || nrow(serie) < 2 || !"fecha" %in% colnames(serie))
                              return("")
                            serie <- serie[order(serie$fecha), ]
                            fecha_ini <- serie$fecha[1]
                            pad_ini <- as.numeric(serie$padron_total[1])
                            lst_ini <- as.numeric(serie$lista_total[1])
                            pad_fin <- as.numeric(serie$padron_total[nrow(serie)])
                            lst_fin <- as.numeric(serie$lista_total[nrow(serie)])
                            var_pad <- if (!is.na(pad_ini) && pad_ini > 0)
                              round((pad_fin - pad_ini) / abs(pad_ini) * 100, 2) else NA
                            var_lst <- if (!is.na(lst_ini) && lst_ini > 0)
                              round((lst_fin - lst_ini) / abs(lst_ini) * 100, 2) else NA
                            if (is.na(var_pad) || is.na(var_lst)) return("")
                            verbo_pad <- if (var_pad > 0) "aumentado" else if (var_pad < 0) "disminuido" else "se ha mantenido igual"
                            verbo_lst <- if (var_lst > 0) "aumentado" else if (var_lst < 0) "disminuido" else "se ha mantenido igual"
                            txt_p <- if (var_pad == 0) paste0("el padr\u00f3n ", verbo_pad)
                                     else paste0("el padr\u00f3n ha ", verbo_pad, " en <strong>",
                                                 fmt_pct(abs(var_pad)), "</strong>")
                            txt_l <- if (var_lst == 0) paste0("la LNE ", verbo_lst)
                                     else paste0("la LNE ha ", verbo_lst, " en <strong>",
                                                 fmt_pct(abs(var_lst)), "</strong>")
                            paste0(" Respecto al <strong>", fmt_fecha_es(fecha_ini), "</strong>, ",
                                   txt_p, ", en tanto que ", txt_l, ".")
                          }, error = function(e) "")

                          # Fecha final (de la serie o del catálogo)
                          fecha_fin_txt <- tryCatch({
                            serie <- datos_semanal_serie_edad()
                            if (!is.null(serie) && nrow(serie) > 0 && "fecha" %in% colnames(serie))
                              fmt_fecha_es(max(serie$fecha, na.rm = TRUE))
                            else as.character(anio_semanal())
                          }, error = function(e) as.character(anio_semanal()))

                          # ── Párrafo 1: totales + variación ────────────────────────────
                          parr1 <- paste0(
                            "<p style='", css_p, "'>Al <strong>", fecha_fin_txt,
                            "</strong>, el Padr\u00f3n Electoral totaliza <strong>", fmt_num(tot_pad),
                            "</strong> ciudadanos, de los cuales, <strong>", fmt_num(tot_lne),
                            "</strong> est\u00e1n incluidos en la Lista Nominal Electoral (LNE), ",
                            "lo que representa una tasa de inclusi\u00f3n de <strong>",
                            fmt_pct(tasa_inc), "</strong>.", txt_variacion, "</p>"
                          )

                          # ── Párrafo 2: grupos etarios macro ───────────────────────────
                          RANGOS_JOV <- c("18","19","20-24","25-29")
                          RANGOS_ADU <- c("30-34","35-39","40-44","45-49","50-54","55-59")
                          RANGOS_MAY <- c("60-64","65+")

                          lne_jov <- sum(df$lst[df$grupo %in% RANGOS_JOV], na.rm = TRUE)
                          lne_adu <- sum(df$lst[df$grupo %in% RANGOS_ADU], na.rm = TRUE)
                          lne_may <- sum(df$lst[df$grupo %in% RANGOS_MAY], na.rm = TRUE)

                          pct_jov <- if (tot_lne > 0) round(lne_jov / tot_lne * 100, 2) else NA
                          pct_adu <- if (tot_lne > 0) round(lne_adu / tot_lne * 100, 2) else NA
                          pct_may <- if (tot_lne > 0) round(lne_may / tot_lne * 100, 2) else NA

                          cmp_may_jov <- if (!is.na(pct_may) && !is.na(pct_jov)) {
                            if (pct_may > pct_jov) "mayor" else if (pct_may < pct_jov) "menor" else "igual"
                          } else "N/D"

                          parr2 <- paste0(
                            "<p style='", css_p, "'>Observando los datos por grupo etario, tanto el ",
                            "Padr\u00f3n como la LNE han mantenido una tendencia uniforme en <strong>",
                            anio_semanal(), "</strong>. La LNE de j\u00f3venes entre 18 y 29 a\u00f1os suma el ",
                            "<strong>", fmt_pct(pct_jov), "</strong>, los adultos entre 30 y 59 a\u00f1os ",
                            "acumulan el mayor porcentaje con <strong>", fmt_pct(pct_adu), "</strong>; y el ",
                            "porcentaje de las personas adultas mayores (60 y m\u00e1s a\u00f1os), es ",
                            "<strong>", cmp_may_jov, "</strong> que el de j\u00f3venes con <strong>",
                            fmt_pct(pct_may), "</strong>.</p>"
                          )

                          # ── Párrafo 3: top 3 rangos ───────────────────────────────────
                          df_ord <- df[order(df$lst, decreasing = TRUE), ]
                          top3   <- head(df_ord, 3)
                          p3_pct <- function(i) if (tot_lne > 0) round(top3$lst[i] / tot_lne * 100, 2) else NA

                          parr3 <- paste0(
                            "<p style='", css_p, "'>El rango de edad con mayor cantidad de ciudadanos ",
                            "en LNE es el formado por personas entre <strong>", top3$grupo[1],
                            " a\u00f1os</strong> con <strong>", fmt_pct(p3_pct(1)), "</strong>, seguido por ",
                            "<strong>", top3$grupo[2], " a\u00f1os</strong> con <strong>",
                            fmt_pct(p3_pct(2)), "</strong> y <strong>", top3$grupo[3],
                            " a\u00f1os</strong> con <strong>", fmt_pct(p3_pct(3)), "</strong>.</p>"
                          )

                          paste0(
                            "<h4 style='", css_h4, "'>An\u00e1lisis General</h4>",
                            parr1, parr2, parr3
                          )
                        }, error = function(e)
                          paste0("<p style='", css_na, "'>Error al procesar datos de edad.</p>")),
                        
                        "sexo" = tryCatch({
                          tot <- extraer_totales_sexo(datos_semanal_sexo(), ambito)
                          if (is.null(tot))
                            return(HTML(paste0("<p style='", css_na,
                                              "'>Sin datos de sexo disponibles.</p>")))

                          ph   <- tot$padron_hombres %||% 0
                          pm   <- tot$padron_mujeres %||% 0
                          pn   <- tot$padron_no_binario %||% 0
                          lh   <- tot$lista_hombres %||% 0
                          lm   <- tot$lista_mujeres %||% 0
                          ln_n <- tot$lista_no_binario %||% 0
                          ti_h <- if (ph > 0) round(lh / ph * 100, 2) else NA
                          ti_m <- if (pm > 0) round(lm / pm * 100, 2) else NA
                          ti_n <- if (pn > 0) round(ln_n / pn * 100, 2) else NA

                          # ── Fecha última vía serie ─────────────────────────
                          meses_es <- c("enero","febrero","marzo","abril",
                                        "mayo","junio","julio","agosto",
                                        "septiembre","octubre","noviembre",
                                        "diciembre")
                          fmt_f <- function(d) paste(
                            as.integer(format(d, "%d")),
                            meses_es[as.integer(format(d, "%m"))],
                            format(d, "%Y"))

                          fecha_fin_txt <- tryCatch({
                            s <- datos_semanal_serie_sexo()
                            if (!is.null(s) && nrow(s) > 0 && "fecha" %in% colnames(s))
                              fmt_f(max(s$fecha, na.rm = TRUE))
                            else as.character(anio_semanal())
                          }, error = function(e) as.character(anio_semanal()))

                          # ── Párrafo 1: totales + tasas de inclusión ────────
                          txt_nb_ti <- if (!is.na(ti_n) && pn > 0)
                            paste0(" y <strong>", fmt_pct(ti_n),
                                   "</strong> de personas no binarias")
                          else ""

                          parr1 <- paste0(
                            "<p style='", css_p, "'>Al <strong>", fecha_fin_txt,
                            "</strong>, el Padr\u00f3n Electoral totaliza <strong>",
                            fmt_num(ph), "</strong> hombres y <strong>",
                            fmt_num(pm), "</strong> mujeres; en tanto que en la LNE",
                            " se registran <strong>", fmt_num(lh),
                            "</strong> hombres y <strong>", fmt_num(lm),
                            "</strong> mujeres. La tasa de inclusi\u00f3n entre el",
                            " Padr\u00f3n y la LNE para la categor\u00eda \u2018Sexo\u2019",
                            " muestra <strong>", fmt_pct(ti_m), "</strong> para mujeres,",
                            " <strong>", fmt_pct(ti_h), "</strong> para hombres",
                            txt_nb_ti, ".</p>"
                          )

                          # ── Párrafos 2 y 3: desglose etario por sexo ──────
                          parr2 <- ""
                          parr3 <- ""
                          df_ed <- construir_df_edad(datos_semanal_edad(), ambito)

                          if (!is.null(df_ed) && nrow(df_ed) > 0) {
                            JOV <- c("18","19","20-24","25-29")
                            ADU <- c("30-34","35-39","40-44","45-49","50-54","55-59")
                            MAY <- c("60-64","65+")

                            tot_lm <- sum(df_ed$lista_mujeres,    na.rm = TRUE)
                            tot_lh <- sum(df_ed$lista_hombres,    na.rm = TRUE)
                            tot_ln <- sum(df_ed$lista_no_binario, na.rm = TRUE)

                            pct_m <- function(r) {
                              v <- sum(df_ed$lista_mujeres[df_ed$grupo %in% r],
                                       na.rm = TRUE)
                              if (tot_lm > 0) round(v / tot_lm * 100, 2) else NA
                            }
                            pct_h <- function(r) {
                              v <- sum(df_ed$lista_hombres[df_ed$grupo %in% r],
                                       na.rm = TRUE)
                              if (tot_lh > 0) round(v / tot_lh * 100, 2) else NA
                            }
                            pct_n <- function(r) {
                              v <- sum(df_ed$lista_no_binario[df_ed$grupo %in% r],
                                       na.rm = TRUE)
                              if (tot_ln > 0) round(v / tot_ln * 100, 2) else NA
                            }

                            txt_nb_p2 <- if (tot_ln > 0)
                              paste0(" Las personas no binarias registran <strong>",
                                     fmt_pct(pct_n(JOV)), "</strong> entre j\u00f3venes,",
                                     " <strong>", fmt_pct(pct_n(ADU)),
                                     "</strong> en adultos y <strong>",
                                     fmt_pct(pct_n(MAY)), "</strong> en adultos mayores.")
                            else ""

                            parr2 <- paste0(
                              "<p style='", css_p,
                              "'>Del total de mujeres en la LNE, <strong>",
                              fmt_pct(pct_m(JOV)),
                              "</strong> son ciudadanas entre 18 y 29 a\u00f1os; <strong>",
                              fmt_pct(pct_m(ADU)), "</strong> entre 30 y 59; y <strong>",
                              fmt_pct(pct_m(MAY)),
                              "</strong> son mayores de 60 a\u00f1os. En cuanto a los hombres,",
                              " <strong>", fmt_pct(pct_h(JOV)),
                              "</strong> son j\u00f3venes (18\u201329 a\u00f1os), <strong>",
                              fmt_pct(pct_h(ADU)),
                              "</strong> son adultos (30 y 59 a\u00f1os), y <strong>",
                              fmt_pct(pct_h(MAY)),
                              "</strong> son adultos mayores.", txt_nb_p2, "</p>"
                            )

                            # Top 3 rangos por sexo
                            top3_m <- head(df_ed[order(df_ed$lista_mujeres,
                                                       decreasing = TRUE), ], 3)
                            top3_h <- head(df_ed[order(df_ed$lista_hombres,
                                                       decreasing = TRUE), ], 3)
                            top3_n <- head(df_ed[order(df_ed$lista_no_binario,
                                                       decreasing = TRUE), ], 3)
                            pm3 <- function(i)
                              if (tot_lm > 0)
                                round(top3_m$lista_mujeres[i] / tot_lm * 100, 2)
                              else NA
                            ph3 <- function(i)
                              if (tot_lh > 0)
                                round(top3_h$lista_hombres[i] / tot_lh * 100, 2)
                              else NA
                            pn3 <- function(i)
                              if (tot_ln > 0)
                                round(top3_n$lista_no_binario[i] / tot_ln * 100, 2)
                              else NA

                            txt_nb_p3 <- if (tot_ln > 0)
                              paste0(
                                " El mayor porcentaje de personas no binarias en la",
                                " LNE se encuentra en el rango de <strong>",
                                top3_n$grupo[1], " a\u00f1os</strong>, con <strong>",
                                fmt_pct(pn3(1)), "</strong>; seguido por <strong>",
                                top3_n$grupo[2], " a\u00f1os</strong>, (<strong>",
                                fmt_pct(pn3(2)), "</strong>), y <strong>",
                                top3_n$grupo[3], " a\u00f1os</strong>, con <strong>",
                                fmt_pct(pn3(3)), "</strong>."
                              )
                            else ""

                            parr3 <- paste0(
                              "<p style='", css_p,
                              "'>Por rangos de edad, la mayor cantidad de mujeres en",
                              " LNE tiene entre <strong>", top3_m$grupo[1],
                              " a\u00f1os</strong>, con <strong>", fmt_pct(pm3(1)),
                              "</strong>, seguido por <strong>", top3_m$grupo[2],
                              " a\u00f1os</strong> (<strong>", fmt_pct(pm3(2)),
                              "</strong>) y <strong>", top3_m$grupo[3],
                              " a\u00f1os</strong>, con <strong>", fmt_pct(pm3(3)),
                              "</strong>. En cuanto a los hombres, en la LNE,",
                              " la mayor\u00eda est\u00e1 en el rango de <strong>",
                              top3_h$grupo[1], " a\u00f1os</strong>, con <strong>",
                              fmt_pct(ph3(1)), "</strong>, seguido por <strong>",
                              top3_h$grupo[2], " a\u00f1os</strong>, (<strong>",
                              fmt_pct(ph3(2)), "</strong>) y <strong>",
                              top3_h$grupo[3], " a\u00f1os</strong>, con <strong>",
                              fmt_pct(ph3(3)), "</strong>.", txt_nb_p3, "</p>"
                            )
                          }

                          paste0(
                            "<h4 style='", css_h4, "'>An\u00e1lisis General</h4>",
                            parr1, parr2, parr3
                          )
                        }, error = function(e)
                          paste0("<p style='", css_na,
                                 "'>Error al procesar datos de sexo.</p>")),
                        
                        "origen" = tryCatch({
                          tabla <- construir_tabla_origen(datos_semanal_origen(), ambito)
                          if (is.null(tabla) || nrow(tabla) < 5)
                            return(HTML(paste0("<p style='", css_na,
                                               "'>Sin datos suficientes de origen.</p>")))

                          # ── Fecha final vía serie ──────────────────────────────────────
                          meses_es_o <- c("enero","febrero","marzo","abril","mayo","junio",
                                          "julio","agosto","septiembre","octubre","noviembre","diciembre")
                          fmt_f_o <- function(d) paste(
                            as.integer(format(d, "%d")),
                            meses_es_o[as.integer(format(d, "%m"))],
                            format(d, "%Y"))

                          fecha_fin_o <- tryCatch({
                            s <- datos_semanal_serie_origen()
                            if (!is.null(s) && nrow(s) > 0 && "fecha" %in% colnames(s))
                              fmt_f_o(max(s$fecha, na.rm = TRUE))
                            else as.character(anio_semanal())
                          }, error = function(e) as.character(anio_semanal()))

                          # ── Top 5 entidades de origen por LNE ─────────────────────────
                          top5 <- head(tabla, 5)
                          noms <- top5$entidad_origen
                          txt_top5 <- paste0(
                            paste(noms[1:4], collapse = ", "),
                            " y ", noms[5]
                          )

                          parr1 <- paste0(
                            "<p style='", css_p, "'>Al <strong>", fecha_fin_o,
                            "</strong>, las 5 entidades de origen con mayor n\u00famero de ciudadanos ",
                            "registrados en la LNE son ",
                            txt_top5, ".</p>"
                          )

                          # ── Posición de LN87 (nacidos extranjero) y LN88 (naturalizados) ─
                          idx_87 <- which(grepl("nacidos en el extranjero", tabla$entidad_origen,
                                                ignore.case = TRUE))
                          idx_88 <- which(grepl("naturaliz", tabla$entidad_origen,
                                                ignore.case = TRUE))
                          pos_87 <- if (length(idx_87) > 0) idx_87[1] else NA
                          pos_88 <- if (length(idx_88) > 0) idx_88[1] else NA

                          txt_pos87 <- if (!is.na(pos_87))
                            paste0("el lugar <strong>", pos_87, "</strong>")
                          else "un lugar no determinado"
                          txt_pos88 <- if (!is.na(pos_88))
                            paste0("el lugar <strong>", pos_88, "</strong>")
                          else "un lugar no determinado"

                          parr2 <- paste0(
                            "<p style='", css_p,
                            "'>Las personas ciudadanas naturalizadas mexicanas ocupan ",
                            txt_pos88, " en la LNE; en tanto que las personas mexicanas ",
                            "nacidas en el extranjero ocupan ", txt_pos87, ".</p>"
                          )

                          # ── Variación temporal ligada a filtros de O3 ────────────────────
                          parr3 <- tryCatch({
                            # Serie reactiva a los filtros de O3 (entidad receptora + btn Consultar)
                            serie_o <- o3_mod$serie_o3_r()
                            if (is.null(serie_o) || nrow(serie_o) < 2 ||
                                !"fecha" %in% colnames(serie_o)) return("")
                            serie_o      <- serie_o[order(serie_o$fecha), ]
                            fecha_ini_o  <- fmt_f_o(serie_o$fecha[1])
                            fecha_fin_o3 <- fmt_f_o(serie_o$fecha[nrow(serie_o)])

                            # Etiquetas de entidad receptora y origen seleccionadas en O3
                            rec_lbl    <- o3_mod$label_receptora(
                              input$semanal_o3_entidad_rec %||% "Nacional")
                            origen_sel <- input$semanal_o3_origen %||% "todas"
                            ori_lbl    <- if (origen_sel == "todas") "Todas"
                                          else o3_mod$nombre_origen(origen_sel)

                            # Filtrar columnas según entidad de origen seleccionada
                            if (origen_sel == "todas") {
                              cols_pad_o <- grep("^pad_[a-z]|^pad87$|^pad88$", colnames(serie_o),
                                                 value = TRUE, ignore.case = TRUE)
                              cols_ln_o  <- grep("^ln_[a-z]|^ln87$|^ln88$",   colnames(serie_o),
                                                 value = TRUE, ignore.case = TRUE)
                            } else {
                              col_ln_sel  <- origen_sel
                              col_pad_sel <- gsub("^ln_", "pad_",
                                                  gsub("^ln(8[78])$", "pad\\1",
                                                       origen_sel, ignore.case = TRUE),
                                                  ignore.case = TRUE)
                              cols_ln_o  <- intersect(col_ln_sel,  colnames(serie_o))
                              cols_pad_o <- intersect(col_pad_sel, colnames(serie_o))
                            }

                            pad_ini_o  <- sum(as.numeric(serie_o[1,             cols_pad_o]), na.rm = TRUE)
                            lst_ini_o  <- sum(as.numeric(serie_o[1,             cols_ln_o]),  na.rm = TRUE)
                            pad_fin_o2 <- sum(as.numeric(serie_o[nrow(serie_o), cols_pad_o]), na.rm = TRUE)
                            lst_fin_o2 <- sum(as.numeric(serie_o[nrow(serie_o), cols_ln_o]),  na.rm = TRUE)
                            var_pad_o  <- if (!is.na(pad_ini_o) && pad_ini_o > 0)
                              round((pad_fin_o2 - pad_ini_o) / abs(pad_ini_o) * 100, 2) else NA
                            var_lst_o  <- if (!is.na(lst_ini_o) && lst_ini_o > 0)
                              round((lst_fin_o2 - lst_ini_o) / abs(lst_ini_o) * 100, 2) else NA
                            if (is.na(var_pad_o) || is.na(var_lst_o)) return("")
                            v_pad <- if (var_pad_o > 0) "aumentado" else if (var_pad_o < 0) "disminuido" else "se ha mantenido igual"
                            v_lst <- if (var_lst_o > 0) "aumentado" else if (var_lst_o < 0) "disminuido" else "se ha mantenido igual"
                            txt_p_o <- if (var_pad_o == 0) paste0("ha ", v_pad)
                                       else paste0("ha ", v_pad, " en <strong>",
                                                   fmt_pct(abs(var_pad_o)), "</strong>")
                            txt_l_o <- if (var_lst_o == 0) paste0("ha ", v_lst)
                                       else paste0("ha ", v_lst, " en <strong>",
                                                   fmt_pct(abs(var_lst_o)), "</strong>")
                            paste0(
                              "<h4 style='", css_h4,
                              "'>An\u00e1lisis puntual entre \u2018entidad de origen\u2019 ",
                              "y \u2018entidad receptora\u2019</h4>",
                              "<p style='", css_p,
                              "'>Considerando los datos de la consulta en la \u00faltima gr\u00e1fica",
                              " (Entidad receptora: <strong>", rec_lbl,
                              "</strong> y Entidad de origen: <strong>", ori_lbl, "</strong>), ",
                              "entre el <strong>", fecha_ini_o, "</strong> y el <strong>",
                              fecha_fin_o3, "</strong>, la LNE ha pasado de ",
                              "<strong>", fmt_num(lst_ini_o), "</strong> a ",
                              "<strong>", fmt_num(lst_fin_o2), "</strong>, por lo que ",
                              txt_l_o, ". Respecto al Padr\u00f3n Electoral, ha pasado de ",
                              "<strong>", fmt_num(pad_ini_o), "</strong> a ",
                              "<strong>", fmt_num(pad_fin_o2), "</strong>, por lo que ",
                              txt_p_o, ".</p>"
                            )
                          }, error = function(e) "")

                          paste0(
                            "<h4 style='", css_h4, "'>An\u00e1lisis General</h4>",
                            parr1, parr2, parr3
                          )
                        }, error = function(e)
                          paste0("<p style='", css_na, "'>Error al procesar datos de origen.</p>")),
                        
                        paste0("<p style='", css_na, "'>Selecciona un desglose para ver el análisis.</p>")
    )
    
    HTML(paste0(
      "<div style='font-size:16px;line-height:1.6;color:#333;'>",
      contenido, "</div>"
    ))
  })
  
  message("✅ graficas_semanal v3.2 inicializado")
  message("   ✅ semanal_dt_edad: columnas simplificadas (Rango/Padrón/LNE/Tasa)")
  message("   ✅ semanal_dt_edad: dom=lfrtip, language español, header ámbito+alcance")
  message("   ✅ Sub-módulos: graficas_semanal_edad, graficas_semanal_sexo, graficas_semanal_origen")
}
