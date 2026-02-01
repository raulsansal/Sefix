# modules/lista_nominal_graficas/graficas_main.R
# Función principal que orquesta todos los módulos de gráficas
# Versión: 1.5 - INTEGRACIÓN: Retorna core_reactives y data_reactives para text_analysis
# CAMBIOS vs v1.4:
#   1. Retorna lista con core y data reactives para que lista_nominal_server.R
#      pueda pasarlos al módulo de análisis textual

lista_nominal_server_graficas <- function(input, output, session, datos_columnas, combinacion_valida, estado_app) {
  
  message("🚀 Iniciando módulo lista_nominal_server_graficas v1.5 (modularizado)")
  
  # ========== CARGAR SUBMÓDULOS ==========
  
  # 1. CORE: Reactives base (caché, filtros, año, estado)
  source("modules/lista_nominal_graficas/graficas_core.R", local = TRUE)
  core_reactives <- graficas_core(input, output, session, estado_app)
  
  # 2. HELPERS: Funciones auxiliares (proyección, etc.)
  source("modules/lista_nominal_graficas/graficas_helpers.R", local = TRUE)
  # Las funciones helper se cargan globalmente en el entorno local
  
  # 3. DATA LOADERS: Carga de datos (year_actual, year_consulta, anuales)
  source("modules/lista_nominal_graficas/graficas_data_loaders.R", local = TRUE)
  data_reactives <- graficas_data_loaders(
    input, output, session, 
    core_reactives$anio_actual, 
    core_reactives$anio_consultado,
    core_reactives$filtros_usuario,
    estado_app
  )
  
  # 4. GRÁFICAS 1 Y 2 (Histórico - Proyección + Evolución Anual)
  source("modules/lista_nominal_graficas/graficas_historico_1_2.R", local = TRUE)
  graficas_historico_1_2_module <- graficas_historico_1_2(
    input, output, session,
    data_reactives$datos_year_actual,
    data_reactives$datos_anuales_completos,
    core_reactives$anio_actual,
    core_reactives$texto_alcance,
    estado_app,
    core_reactives$mostrar_graficas_anuales,
    core_reactives$ambito_reactivo
  )
  
  # 5. GRÁFICA 3 (Histórico - Evolución Anual por Sexo)
  source("modules/lista_nominal_graficas/graficas_historico_3.R", local = TRUE)
  graficas_historico_3_module <- graficas_historico_3(
    input, output, session,
    data_reactives$datos_anuales_completos,
    core_reactives$anio_actual,
    core_reactives$texto_alcance,
    estado_app,
    core_reactives$mostrar_graficas_anuales,
    core_reactives$ambito_reactivo
  )
  
  # 6. GRÁFICAS 4 Y 5 (Consultado - Evolución Mensual)
  source("modules/lista_nominal_graficas/graficas_consultado_4_5.R", local = TRUE)
  graficas_consultado_4_5_module <- graficas_consultado_4_5(
    input, output, session,
    data_reactives$datos_year_consulta,
    core_reactives$anio_consultado,
    core_reactives$texto_alcance,
    estado_app,
    core_reactives$mostrar_graficas_consultadas,
    core_reactives$ambito_reactivo
  )
  
  # 7. GRÁFICAS SEMANALES (Barras + Tasa Inclusión)
  source("modules/lista_nominal_graficas/graficas_semanal.R", local = TRUE)
  graficas_semanal_module <- graficas_semanal(
    input, output, session,
    datos_columnas,
    combinacion_valida,
    core_reactives$texto_alcance,
    core_reactives$ambito_reactivo  # ✅ v1.4: CORRECCIÓN - Parámetro agregado
  )
  
  # 8. RENDERIZADO DINÁMICO DE UI
  source("modules/lista_nominal_graficas/graficas_ui_render.R", local = TRUE)
  graficas_ui_render_module <- graficas_ui_render(
    input, output, session,
    estado_app,
    core_reactives$mostrar_graficas_anuales,
    core_reactives$mostrar_graficas_consultadas,
    core_reactives$ambito_reactivo
  )
  
  message("✅ Módulo lista_nominal_server_graficas v1.5 inicializado correctamente")
  message("   ✅ MANTIENE v1.4: ambito_reactivo pasado a graficas_semanal")
  message("   ✅ NUEVO v1.5: Retorna core_reactives y data_reactives para text_analysis")
  
  # ✅ v1.5: RETORNAR REACTIVES para que lista_nominal_server.R los pase al módulo de texto
  return(list(
    core = core_reactives,
    data = data_reactives
  ))
}
