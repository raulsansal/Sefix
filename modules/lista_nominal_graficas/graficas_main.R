# modules/lista_nominal_graficas/graficas_main.R
# Función principal que orquesta todos los módulos de gráficas
# Versión: 1.6 - INTEGRACIÓN: Vista Semanal con data loader propio
#
# CAMBIOS vs v1.5:
#   1. Carga graficas_semanal_data_loaders.R como submódulo nuevo (paso 7)
#   2. graficas_semanal recibe datos de semanal_data_loaders (no datos_columnas)
#   3. Firma de graficas_semanal actualizada con los nuevos parámetros
#   4. Mantiene retorno de core_reactives y data_reactives para text_analysis

lista_nominal_server_graficas <- function(input, output, session, datos_columnas, combinacion_valida, estado_app) {
  
  message("🚀 Iniciando módulo lista_nominal_server_graficas v1.6 (modularizado)")
  
  # ── 1. CORE: Reactives base (caché, filtros, año, estado) ──────────────────
  source("modules/lista_nominal_graficas/graficas_core.R", local = TRUE)
  core_reactives <- graficas_core(input, output, session, estado_app)
  
  # ── 2. HELPERS: Funciones auxiliares (proyección, etc.) ───────────────────
  source("modules/lista_nominal_graficas/graficas_helpers.R", local = TRUE)
  
  # ── 3. DATA LOADERS HISTÓRICO: año_actual, año_consulta, anuales ──────────
  source("modules/lista_nominal_graficas/graficas_data_loaders.R", local = TRUE)
  data_reactives <- graficas_data_loaders(
    input, output, session,
    core_reactives$anio_actual,
    core_reactives$anio_consultado,
    core_reactives$filtros_usuario,
    estado_app
  )
  
  # ── 4. GRÁFICAS 1 Y 2 (Histórico – Proyección + Evolución Anual) ──────────
  source("modules/lista_nominal_graficas/graficas_historico_1_2.R", local = TRUE)
  graficas_historico_1_2(
    input, output, session,
    data_reactives$datos_year_actual,
    data_reactives$datos_anuales_completos,
    core_reactives$anio_actual,
    core_reactives$texto_alcance,
    estado_app,
    core_reactives$mostrar_graficas_anuales,
    core_reactives$ambito_reactivo
  )
  
  # ── 5. GRÁFICA 3 (Histórico – Evolución Anual por Sexo) ───────────────────
  source("modules/lista_nominal_graficas/graficas_historico_3.R", local = TRUE)
  graficas_historico_3(
    input, output, session,
    data_reactives$datos_anuales_completos,
    core_reactives$anio_actual,
    core_reactives$texto_alcance,
    estado_app,
    core_reactives$mostrar_graficas_anuales,
    core_reactives$ambito_reactivo
  )
  
  # ── 6. GRÁFICAS 4 Y 5 (Consultado – Evolución Mensual) ────────────────────
  source("modules/lista_nominal_graficas/graficas_consultado_4_5.R", local = TRUE)
  graficas_consultado_4_5(
    input, output, session,
    data_reactives$datos_year_consulta,
    core_reactives$anio_consultado,
    core_reactives$texto_alcance,
    estado_app,
    core_reactives$mostrar_graficas_consultadas,
    core_reactives$ambito_reactivo
  )
  
  # ── 7. DATA LOADERS SEMANAL: edad, sexo, origen (NUEVO v1.6) ──────────────
  source("modules/lista_nominal_graficas/graficas_semanal_data_loaders.R", local = TRUE)
  semanal_reactives <- graficas_semanal_data_loaders(
    input, output, session,
    estado_app,
    core_reactives$filtros_usuario,
    core_reactives$ambito_reactivo
  )
  
  # ── 8. GRÁFICAS SEMANALES (REESCRITURA v2.0) ──────────────────────────────
  source("modules/lista_nominal_graficas/graficas_semanal.R", local = TRUE)
  graficas_semanal(
    input, output, session,
    datos_semanal_edad      = semanal_reactives$datos_semanal_edad,
    datos_semanal_sexo      = semanal_reactives$datos_semanal_sexo,
    datos_semanal_origen    = semanal_reactives$datos_semanal_origen,
    anio_semanal            = semanal_reactives$anio_semanal,
    fecha_semanal_efectiva  = semanal_reactives$fecha_semanal_efectiva,
    texto_alcance           = core_reactives$texto_alcance,
    ambito_reactivo         = core_reactives$ambito_reactivo,
    estado_app              = estado_app
  )
  
  # ── 9. RENDERIZADO DINÁMICO DE UI ─────────────────────────────────────────
  source("modules/lista_nominal_graficas/graficas_ui_render.R", local = TRUE)
  graficas_ui_render(
    input, output, session,
    estado_app,
    core_reactives$mostrar_graficas_anuales,
    core_reactives$mostrar_graficas_consultadas,
    core_reactives$ambito_reactivo
  )
  
  message("✅ Módulo lista_nominal_server_graficas v1.6 inicializado correctamente")
  message("   ✅ v1.6: graficas_semanal v2.0 con data loader propio")
  message("   ✅ MANTIENE v1.5: core_reactives y data_reactives retornados para text_analysis")
  
  return(list(
    core = core_reactives,
    data = data_reactives
  ))
}
