# utils_responsive.R
# Utilidades para gráficas responsivas en Shiny/Plotly
# Versión: 2.3
# Cambios v2.3:
#   - Card NB más pequeña con textos reducidos
#   - Posición de fuente dinámica según número de leyendas
#   - hovermode = 'closest' en móvil para tap

# ============================================================
# CONFIGURACIÓN RESPONSIVE PARA PLOTLY
# ============================================================

get_plotly_config <- function(screen_width = 1200, num_legend_items = 2) {
  
  is_mobile <- screen_width <= 768
  is_tablet <- screen_width > 768 && screen_width <= 1024
  
  # Calcular offset para leyendas adicionales
  # Cada par de leyendas extra necesita más espacio
  legend_rows <- ceiling(num_legend_items / 2)
  legend_offset <- max(0, (legend_rows - 1) * 0.06)
  
  if (is_mobile) {
    config <- list(
      # Tamaños de fuente
      title_size = 10,
      subtitle_size = 8,
      axis_title_size = 7,
      axis_tick_size = 6,
      legend_size = 6,
      annotation_size = 6,
      source_size = 5,
      
      # Márgenes - ajustados dinámicamente
      margin_top = 55,
      margin_bottom = 75 + (legend_offset * 60),  # Más espacio para leyendas
      margin_left = 40,
      margin_right = 10,
      
      height = 280,
      
      # Posiciones verticales - dinámicas según leyendas
      subtitle_y = 1.04,
      legend_y = -0.18 - (legend_offset * 0.3),
      legend_orientation = "h",
      source_y = -0.34 - (legend_offset * 1.2),  # ✅ Siempre debajo de leyendas
      
      # Líneas y marcadores delgados
      line_width = 0.75,
      marker_size = 2.5,
      projection_line_width = 0.5,
      projection_marker_size = 2,
      
      # ✅ Card NB más pequeña
      card_nb_font_size = 3,
      card_nb_title_size = 4,
      card_nb_border_width = 0.5,
      card_nb_padding = 1,
      
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE,
      hoverlabel_font_size = 7,
      
      # ✅ En móvil usar 'closest' para mejor interacción táctil
      hovermode = 'closest'
    )
    
  } else if (is_tablet) {
    
    legend_offset_tablet <- max(0, (legend_rows - 1) * 0.05)
    
    config <- list(
      title_size = 14,
      subtitle_size = 11,
      axis_title_size = 10,
      axis_tick_size = 9,
      legend_size = 9,
      annotation_size = 9,
      source_size = 8,
      
      margin_top = 85,
      margin_bottom = 110 + (legend_offset_tablet * 50),
      margin_left = 60,
      margin_right = 30,
      
      height = 350,
      
      subtitle_y = 1.07,
      legend_y = -0.20 - (legend_offset_tablet * 0.25),
      legend_orientation = "h",
      source_y = -0.38 - (legend_offset_tablet * 0.8),
      
      line_width = 1.5,
      marker_size = 5,
      projection_line_width = 1.0,
      projection_marker_size = 4,
      
      card_nb_font_size = 6,
      card_nb_title_size = 7,
      card_nb_border_width = 1,
      card_nb_padding = 3,
      
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE,
      hoverlabel_font_size = 9,
      hovermode = 'x unified'
    )
    
  } else {
    # Desktop
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
      
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE,
      hoverlabel_font_size = 12,
      hovermode = 'x unified'
    )
  }
  
  config$is_mobile <- is_mobile
  config$is_tablet <- is_tablet
  config$is_desktop <- !is_mobile && !is_tablet
  config$screen_width <- screen_width
  config$num_legend_items <- num_legend_items
  
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
  
  # Subtítulo
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
  
  # Fuente
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
  
  # Anotaciones extra (incluyendo card NB)
  if (length(extra_annotations) > 0) {
    for (ann in extra_annotations) {
      if (!is.null(ann)) {
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
    hovermode = config$hovermode,
    hoverlabel = list(
      font = list(size = config$hoverlabel_font_size)
    ),
    annotations = annotations_list,
    autosize = TRUE
  )
  
  # Configuración según dispositivo
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
# FUNCIÓN PARA CREAR TRAZAS RESPONSIVAS
# ============================================================

add_responsive_trace <- function(p, data, x, y, name, color, config, 
                                 line_dash = "solid", marker_symbol = "circle",
                                 is_projection = FALSE) {
  
  lw <- if (is_projection) config$projection_line_width else config$line_width
  ms <- if (is_projection) config$projection_marker_size else config$marker_size
  
  # En móvil, solo líneas para mayor claridad
  mode <- if (config$is_mobile) "lines" else "lines+markers"
  
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
# JAVASCRIPT PARA DETECTAR ANCHO
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


message("✅ utils_responsive.R v2.3 cargado")
message("   📱 Ajustes:")
message("      - source_y dinámico según número de leyendas")
message("      - Card NB más pequeña")
message("      - hovermode='closest' en móvil")
