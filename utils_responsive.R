# utils_responsive.R
# Utilidades para gráficas responsivas en Shiny/Plotly
# Versión: 1.0
# Fecha: 05 de febrero de 2026

# ============================================================
# CONFIGURACIÓN RESPONSIVE PARA PLOTLY
# ============================================================

# Función que retorna configuración de layout según el ancho de pantalla
# Esta función debe llamarse en cada renderPlotly con el ancho detectado

get_plotly_config <- function(screen_width = 1200) {
  
  # Determinar si es móvil, tablet o desktop
  is_mobile <- screen_width <= 768
  is_tablet <- screen_width > 768 && screen_width <= 1024
  
  if (is_mobile) {
    # ========== CONFIGURACIÓN MÓVIL ==========
    config <- list(
      # Tamaños de fuente
      title_size = 12,
      subtitle_size = 10,
      axis_title_size = 9,
      axis_tick_size = 8,
      legend_size = 8,
      annotation_size = 8,
      source_size = 7,
      
      # Márgenes
      margin_top = 80,
      margin_bottom = 100,
      margin_left = 50,
      margin_right = 20,
      
      # Altura sugerida
      height = 280,
      
      # Leyenda
      legend_y = -0.35,
      legend_orientation = "h",
      
      # Fuente posición Y
      source_y = -0.50,
      
      # Subtítulo posición Y
      subtitle_y = 1.08,
      
      # Marcadores
      marker_size = 5,
      line_width = 2,
      
      # Mostrar elementos opcionales
      show_metodologia_btn = FALSE,
      show_card_nb = TRUE
    )
    
  } else if (is_tablet) {
    # ========== CONFIGURACIÓN TABLET ==========
    config <- list(
      title_size = 15,
      subtitle_size = 12,
      axis_title_size = 11,
      axis_tick_size = 10,
      legend_size = 10,
      annotation_size = 10,
      source_size = 9,
      
      margin_top = 100,
      margin_bottom = 120,
      margin_left = 70,
      margin_right = 40,
      
      height = 380,
      
      legend_y = -0.25,
      legend_orientation = "h",
      
      source_y = -0.40,
      subtitle_y = 1.10,
      
      marker_size = 7,
      line_width = 2.5,
      
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE
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
      
      legend_y = -0.20,
      legend_orientation = "h",
      
      source_y = -0.35,
      subtitle_y = 1.12,
      
      marker_size = 8,
      line_width = 3,
      
      show_metodologia_btn = TRUE,
      show_card_nb = TRUE
    )
  }
  
  # Agregar información de dispositivo
  config$is_mobile <- is_mobile
  config$is_tablet <- is_tablet
  config$is_desktop <- !is_mobile && !is_tablet
  config$screen_width <- screen_width
  
  return(config)
}


# ============================================================
# FUNCIÓN HELPER PARA CREAR LAYOUT RESPONSIVO
# ============================================================

# Crea el layout de Plotly con parámetros responsivos
create_responsive_layout <- function(p, config, title, subtitle = NULL, show_source = TRUE, 
                                     source_text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
                                     xaxis_config = list(), yaxis_config = list(),
                                     extra_annotations = list()) {
  
  # ========== ANNOTATIONS BASE ==========
  annotations_list <- list()
  
  # Subtítulo (alcance)
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
  
  # Agregar anotaciones extra (como card NB)
  if (length(extra_annotations) > 0) {
    for (ann in extra_annotations) {
      if (!is.null(ann)) {
        # Ajustar tamaño de fuente de anotaciones extra
        if (!is.null(ann$font)) {
          ann$font$size <- config$annotation_size
        }
        annotations_list[[length(annotations_list) + 1]] <- ann
      }
    }
  }
  
  # ========== CONFIGURACIÓN DE EJES ==========
  
  # Eje X base
  xaxis_base <- list(
    title = list(
      text = xaxis_config$title %||% "",
      font = list(size = config$axis_title_size)
    ),
    tickfont = list(size = config$axis_tick_size)
  )
  
  # Merge con configuración personalizada
  xaxis_final <- modifyList(xaxis_base, xaxis_config)
  
  # Eje Y base
  yaxis_base <- list(
    title = list(
      text = yaxis_config$title %||% "Número de Electores",
      font = list(size = config$axis_title_size)
    ),
    tickfont = list(size = config$axis_tick_size),
    separatethousands = TRUE
  )
  
  # Merge con configuración personalizada
  yaxis_final <- modifyList(yaxis_base, yaxis_config)
  
  # ========== APLICAR LAYOUT ==========
  
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
    annotations = annotations_list,
    autosize = TRUE
  )
  
  # Configuración adicional para móvil
  if (config$is_mobile) {
    p <- p %>% config(
      displayModeBar = FALSE,  # Ocultar barra de herramientas en móvil
      responsive = TRUE
    )
  } else {
    p <- p %>% config(
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
                                  line_dash = "solid", marker_symbol = "circle") {
  
  p %>% add_trace(
    data = data,
    x = x,
    y = y,
    type = 'scatter',
    mode = 'lines+markers',
    name = name,
    line = list(
      color = color, 
      width = config$line_width,
      dash = line_dash
    ),
    marker = list(
      size = config$marker_size, 
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
# SHINY INPUT PARA DETECTAR ANCHO DE PANTALLA
# ============================================================

# JavaScript que debe incluirse en la UI para detectar el ancho
# Este código envía el ancho de pantalla a Shiny como input$screen_width

get_screen_width_js <- function() {
  tags$script(HTML("
    // Detectar ancho de pantalla y enviarlo a Shiny
    $(document).ready(function() {
      // Función para enviar ancho
      function sendScreenWidth() {
        Shiny.setInputValue('screen_width', window.innerWidth, {priority: 'event'});
      }
      
      // Enviar al cargar
      sendScreenWidth();
      
      // Enviar al redimensionar (con debounce)
      var resizeTimer;
      $(window).on('resize', function() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(function() {
          sendScreenWidth();
        }, 250);
      });
      
      // Enviar al cambiar orientación
      $(window).on('orientationchange', function() {
        setTimeout(sendScreenWidth, 100);
      });
    });
  "))
}


# ============================================================
# REACTIVE PARA OBTENER CONFIGURACIÓN EN SERVER
# ============================================================

# Ejemplo de uso en server:
# 
# # En el server principal o módulo:
# plotly_config <- reactive({
#   width <- input$screen_width %||% 1200
#   get_plotly_config(width)
# })
#
# # En cada renderPlotly:
# output$mi_grafico <- renderPlotly({
#   config <- plotly_config()
#   
#   p <- plot_ly() %>%
#     add_responsive_trace(datos, ~x, ~y, "Serie 1", "#003E66", config)
#   
#   create_responsive_layout(p, config, 
#                            title = "Mi Título",
#                            subtitle = texto_alcance())
# })


message("✅ utils_responsive.R v1.0 cargado")
message("   📱 Funciones disponibles:")
message("      - get_plotly_config(screen_width)")
message("      - create_responsive_layout(p, config, title, ...)")
message("      - add_responsive_trace(p, data, x, y, name, color, config, ...)")
message("      - get_screen_width_js()")
