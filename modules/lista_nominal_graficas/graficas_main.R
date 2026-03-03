# modules/lista_nominal_graficas/graficas_main.R
# Función principal que orquesta todos los módulos de gráficas
# Versión: 1.8 — Pasa datos_semanal_serie_origen a graficas_semanal()
#
# CAMBIOS vs v1.7:
#   1. graficas_semanal_edad.R y graficas_semanal_sexo.R se cargan DENTRO de
#      graficas_semanal() para heredar su entorno (helpers, COLORES, etc.)
#   2. graficas_semanal() recibe tres argumentos de series temporales:
#        datos_semanal_serie_edad ← serie temporal para proyección E1
#        datos_semanal_serie_sexo ← serie temporal para proyección S7
#        datos_semanal_serie_origen ← serie temporal para proyección O2 (NUEVO)
#   4. graficas_semanal_data_loaders() ya expone ambos reactives en v2.0

lista_nominal_server_graficas <- function(input, output, session,
                                          datos_columnas,
                                          combinacion_valida,
                                          estado_app) {
  
  message("🚀 Iniciando módulo lista_nominal_server_graficas v1.8 (modularizado)")
  
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
  
  # ── 7. DATA LOADERS SEMANAL: corte único + series temporales ──────────────
  source("modules/lista_nominal_graficas/graficas_semanal_data_loaders.R", local = TRUE)
  semanal_reactives <- graficas_semanal_data_loaders(
    input, output, session,
    estado_app,
    core_reactives$filtros_usuario,
    core_reactives$ambito_reactivo
  )
  
  # ── 8. ORQUESTADOR SEMANAL: carga sub-módulos y gestiona origen ───────────
  # NOTA: graficas_semanal_edad.R y graficas_semanal_sexo.R se cargan
  # DENTRO de graficas_semanal() para heredar su entorno (es_historico,
  # desglose_activo, helpers compartidos, COLORES, ORDEN_EDAD, etc.)
  source("modules/lista_nominal_graficas/graficas_semanal.R", local = TRUE)
  graficas_semanal(
    input, output, session,
    datos_semanal_edad           = semanal_reactives$datos_semanal_edad,
    datos_semanal_sexo           = semanal_reactives$datos_semanal_sexo,
    datos_semanal_origen         = semanal_reactives$datos_semanal_origen,
    datos_semanal_serie_edad     = semanal_reactives$datos_semanal_serie_edad,
    datos_semanal_serie_sexo     = semanal_reactives$datos_semanal_serie_sexo,
    datos_semanal_serie_origen   = semanal_reactives$datos_semanal_serie_origen,  # NUEVO
    anio_semanal                 = semanal_reactives$anio_semanal,
    fecha_semanal_efectiva       = semanal_reactives$fecha_semanal_efectiva,
    texto_alcance                = core_reactives$texto_alcance,
    ambito_reactivo              = core_reactives$ambito_reactivo,
    estado_app                   = estado_app
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
  
  message("✅ Módulo lista_nominal_server_graficas v1.8 inicializado correctamente")
  message("   ✅ v1.8: sub-módulos edad/sexo/origen cargados dentro de graficas_semanal()")
  message("   ✅ v1.7: graficas_semanal_data_loaders v2.0 con series temporales")
  message("   ✅ MANTIENE: core_reactives y data_reactives retornados para text_analysis")
  
  return(list(
    core = core_reactives,
    data = data_reactives
  ))
}
