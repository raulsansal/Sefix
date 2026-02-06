# utils_responsive.R
# Utilidades para gráficas responsivas en Shiny/Plotly
# Versión: 2.1
# Fecha: 06 de febrero de 2026
# Cambios v2.1:
#   - Líneas 50% más delgadas (0.75px en móvil)
#   - Marcadores 50% más pequeños
#   - Mejor distribución vertical de elementos
#   - Card NB 50% más pequeña

# ============================================================
# CONFIGURACIÓN RESPONSIVE PARA PLOTLY
# ============================================================

get_plotly_config <- function(screen_width = 1200) {
  
  is_mobile <- screen_width <= 768
  is_tablet <- screen_width > 768 && screen_width <= 1024
  
  if (is_mobile) {
    # ========== CONFIGURACIÓN MÓVIL - v2.1 OPTIMIZADA ==========
    config <- list(
      # Tamaños de fuente (proporcionales al viewport)
      title_size = 10,
      subtitle_size = 8,
      axis_title_size = 7,
      axis_tick_size = 6,
      legend_size = 6,
      annotation_size = 6,
      source_size = 5,
      
      # ========== MÁRGENES - DISTRIBUIR MEJOR EL ESPACIO ==========
      # Reducidos para dar más espacio al área de la gráfica
      # y distribuir mejor los elementos
      margin_top = 55,         # Espacio para título + subtítulo
      margin_bottom = 70,      # Espacio para leyenda + fuente
      margin_left = 40,        # Espacio para eje Y
      margin_right = 10,       # Mínimo a la derecha
      
      # Altura sugerida (se sobrescribe por CSS, pero útil para cálculos)
      height = 280,
      
      # ========== POSICIONAMIENTO VERTICAL ==========
      # Título en y=1.0 (arriba del plot area)
      # Subtítulo inmediatamente debajo del título
      subtitle_y = 1.04,       # Cerca del título
      
      # Leyenda debajo del área de gráfica
      legend_y = -0.18,        # Reducido para acercar al gráfico
      legend_orientation = "h",
      
      # Fuente con espaciado uniforme debajo de leyenda
      source_y = -0.32,        # ~0.14 debajo de la leyenda (mismo espacio)
      
      # ========== ✅ LÍNEAS Y MARCADORES - 50% MÁS DELGADOS ==========
      line_width = 0.75,       # Era 1.5, ahora 50% más delgado
      marker_size = 2.5,       # Era 4-5, ahora 50% más pequeño
      
      # Proyecciones (líneas punteadas) aún más delgadas
      projection_line_width = 0.5,
      projection_marker_size = 2,
      
      # ========== ✅ CARD NB - 50% MÁS PEQUEÑA ==========
      card_nb_font_size = 5,           # Era 7, ahora más pequeño
      card_nb_title_size = 6,          # Era 8
      card_nb_border_width = 1,        # Era 1.5
      card_nb_padding = 2,             # Era 4
      card_nb_scale = 0.5,             # 50% del tamaño original
      
      # Elementos opcionales
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE,
      
      # Hover/Tooltip
      hoverlabel_font_size = 8
    )
    
  } else if (is_tablet) {
    # ========== CONFIGURACIÓN TABLET ==========
    config <- list(
      title_size = 14,
      subtitle_size = 11,
      axis_title_size = 10,
      axis_tick_size = 9,
      legend_size = 9,
      annotation_size = 9,
      source_size = 8,
      
      margin_top = 85,
      margin_bottom = 100,
      margin_left = 60,
      margin_right = 30,
      
      height = 350,
      
      subtitle_y = 1.07,
      legend_y = -0.20,
      legend_orientation = "h",
      source_y = -0.35,
      
      line_width = 1.5,
      marker_size = 5,
      projection_line_width = 1.0,
      projection_marker_size = 4,
      
      card_nb_font_size = 8,
      card_nb_title_size = 9,
      card_nb_border_width = 1.5,
      card_nb_padding = 5,
      card_nb_scale = 0.7,
      
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE,
      hoverlabel_font_size = 10
    )
    
  } else {
    # ========== CONFIGURACIÓN DESKTOP ==========
    config <- list(
      title_size = 18,
      subtitle_size = 13,
      axis_title_size = 12,
      axis_tick_size = 11,
      legend_size = 11,
      annotation_size = 11,
      source_size = 10,
      
      margin_top = 120,
      margin_bottom = 140,
      margin_left = 90,
      margin_right = 50,
      
      height = 450,
      
      subtitle_y = 1.12,
      legend_y = -0.20,
      legend_orientation = "h",
      source_y = -0.35,
      
      line_width = 3,
      marker_size = 8,
      projection_line_width = 2,
      projection_marker_size = 6,
      
      card_nb_font_size = 10,
      card_nb_title_size = 12,
      card_nb_border_width = 2.5,
      card_nb_padding = 8,
      card_nb_scale = 1.0,
      
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE,
      hoverlabel_font_size = 12
    )
  }
  
  # Información de dispositivo
  config$is_mobile <- is_mobile
  config$is_tablet <- is_tablet
  config$is_desktop <- !is_mobile && !is_tablet
  config$screen_width <- screen_width
  
  return(config)
}


# ============================================================
# FUNCIÓN HELPER PARA CREAR LAYOUT RESPONSIVO
# ============================================================

create_responsive_layout <- function(p, config, title, subtitle = NULL, show_source = TRUE, 
                                     source_text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
                                     xaxis_config = list(), yaxis_config = list(),
                                     extra_annotations = list()) {
  
  annotations_list <- list()
  
  # Subtítulo (alcance) - cerca del título
  if (!is.null(subtitle) && nchar(subtitle) > 0) {
    annotations_list[[length(annotations_list) + 1]] <- list(
      text = subtitle,
      x = 0.5, 
      y = config$subtitle_y,
      xref = "paper", 
      yref = "paper",
      xanchor = "center", 
      yanchor = "top",
      showarrow = FALSE,
      font = list(
        size = config$subtitle_size, 
        color = "#555555", 
        family = "Arial, sans-serif"
      ),
      align = "center"
    )
  }
  
  # Fuente - espaciado uniforme respecto a leyenda
  if (show_source) {
    annotations_list[[length(annotations_list) + 1]] <- list(
      text = source_text,
      x = 0.5, 
      y = config$source_y,
      xref = "paper", 
      yref = "paper",
      xanchor = "center", 
      yanchor = "top",
      showarrow = FALSE,
      font = list(
        size = config$source_size, 
        color = "#666666", 
        family = "Arial, sans-serif"
      ),
      align = "center"
    )
  }
  
  # Anotaciones extra (card NB, etc.)
  if (length(extra_annotations) > 0) {
    for (ann in extra_annotations) {
      if (!is.null(ann)) {
        if (!is.null(ann$font)) {
          ann$font$size <- config$annotation_size
        }
        annotations_list[[length(annotations_list) + 1]] <- ann
      }
    }
  }
  
  # Eje X
  xaxis_base <- list(
    title = list(
      text = xaxis_config$title %||% "",
      font = list(size = config$axis_title_size)
    ),
    tickfont = list(size = config$axis_tick_size)
  )
  xaxis_final <- modifyList(xaxis_base, xaxis_config)
  
  # Eje Y
  yaxis_base <- list(
    title = list(
      text = yaxis_config$title %||% "Número de Electores",
      font = list(size = config$axis_title_size)
    ),
    tickfont = list(size = config$axis_tick_size),
    separatethousands = TRUE
  )
  yaxis_final <- modifyList(yaxis_base, yaxis_config)
  
  # Aplicar layout
  p <- p %>% layout(
    title = list(
      text = title,
      font = list(
        size = config$title_size, 
        color = "#333", 
        family = "Arial, sans-serif"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = xaxis_final,
    yaxis = yaxis_final,
    legend = list(
      orientation = config$legend_orientation, 
      xanchor = "center", 
      x = 0.5, 
      y = config$legend_y,
      font = list(size = config$legend_size),
      traceorder = "normal"
    ),
    margin = list(
      t = config$margin_top, 
      b = config$margin_bottom, 
      l = config$margin_left, 
      r = config$margin_right
    ),
    hovermode = 'x unified',
    hoverlabel = list(
      font = list(size = config$hoverlabel_font_size)
    ),
    annotations = annotations_list,
    autosize = TRUE
  )
  
  # Configuración de Plotly según dispositivo
  if (config$is_mobile) {
    p <- p %>% plotly::config(
      displayModeBar = FALSE,
      responsive = TRUE
    )
  } else {
    p <- p %>% plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      responsive = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
  }
  
  return(p)
}


# ============================================================
# FUNCIÓN PARA CREAR TRAZAS CON ESTILO RESPONSIVO
# ============================================================

add_responsive_trace <- function(p, data, x, y, name, color, config, 
                                 line_dash = "solid", marker_symbol = "circle",
                                 is_projection = FALSE) {
  
  lw <- if (is_projection) config$projection_line_width else config$line_width
  ms <- if (is_projection) config$projection_marker_size else config$marker_size
  
  # En móvil, usar solo líneas (sin marcadores) para mayor claridad
  mode <- if (config$is_mobile && !is_projection) "lines" else "lines+markers"
  
  p %>% add_trace(
    data = data,
    x = x,
    y = y,
    type = 'scatter',
    mode = mode,
    name = name,
    line = list(
      color = color, 
      width = lw,
      dash = line_dash
    ),
    marker = list(
      size = ms, 
      color = color,
      symbol = marker_symbol
    ),
    hovertemplate = paste0(
      '<b>%{x}</b><br>',
      name, ': %{y:,.0f}<extra></extra>'
    )
  )
}


# ============================================================
# JAVASCRIPT PARA DETECTAR ANCHO DE PANTALLA
# ============================================================

get_screen_width_js <- function() {
  tags$script(HTML("
    $(document).ready(function() {
      function sendScreenWidth() {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue('screen_width', window.innerWidth, {priority: 'event'});
        }
      }
      
      setTimeout(sendScreenWidth, 500);
      $(document).on('shiny:connected', sendScreenWidth);
      
      var resizeTimer;
      $(window).on('resize', function() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(sendScreenWidth, 250);
      });
      
      $(window).on('orientationchange', function() {
        setTimeout(sendScreenWidth, 100);
      });
    });
  "))
}


message("✅ utils_responsive.R v2.1 cargado")
message("   📱 Ajustes móvil v2.1:")
message("      - Líneas 50% más delgadas (0.75px)")
message("      - Marcadores 50% más pequeños (2.5px)")
message("      - Mejor distribución vertical")
message("      - Card NB 50% más pequeña")
message("      - Márgenes optimizados para aprovechar espacio")
