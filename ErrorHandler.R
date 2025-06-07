################################################################################
# errorHandler.R – логирование и защита от ошибок Shiny-приложения
################################################################################
# VKR: «Разработка программного модуля для системы анализа данных мониторинга
#      оборудования»
# Зависимости: logger, shiny.
# Опционально: shinylogs (трекинг действий), R.utils (тайм-ауты).
################################################################################

# ---- Пакеты ----
library(logger)
library(shiny)

################################################################################
# 1. ИНИЦИАЛИЗАЦИЯ ЛОГА --------------------------------------------------------
################################################################################
log_init <- function(log_dir = "logs", level = INFO) {
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  log_file <- file.path(log_dir, paste0(Sys.Date(), ".log"))
  log_appender(appender_tee(log_file))
  log_layout(layout_glue_generator(format = "[{format(Sys.time(), '%H:%M:%S')}] {level}: {msg}"))
  log_threshold(level)
  log_info("===== Logger initialized – {log_file} =====")
}

################################################################################
# 2. БЕЗОПАСНОЕ ВЫПОЛНЕНИЕ ВЫРАЖЕНИЯ -----------------------------------------
################################################################################
# safe_exec() – «универсальный предохранитель» для всех опасных операций.
#
# ▸ expr           – выражение (в фигурных скобках) для выполнения.
# ▸ context        – строка-метка для логов/уведомлений.
# ▸ default_value  – что вернуть при ошибке (NULL по умолчанию).
# ▸ retries        – число повторов при ошибке (≥1).
# ▸ retry_delay    – пауза (сек) между повторами.
# ▸ timeout_ms     – тайм-аут (мс) на выполнение expr; требует пакета R.utils.
# ▸ on_success     – callback(result) при успешном выполнении.
# ▸ on_error       – callback(err)    при ошибке после всех попыток.
# ▸ log_time       – фиксировать длительность выполнения (TRUE).
################################################################################

safe_exec <- function(expr,
                      context       = "generic",
                      default_value = NULL,
                      retries       = 1,
                      retry_delay   = 0.5,
                      timeout_ms    = NULL,
                      on_success    = NULL,
                      on_error      = NULL,
                      log_time      = TRUE) {
  
  expr_sub   <- substitute(expr)      # сохраняем выражение
  caller_env <- parent.frame()        # окружение вызова (важно для shiny input/output)
  
  # внутренний исполнитель с учётом тайм‑аута ---------------------------------
  eval_once <- function() {
    eval(expr_sub, envir = caller_env)
  }
  
  exec_with_timeout <- function() {
    if (is.null(timeout_ms)) return(eval_once())
    if (!requireNamespace("R.utils", quietly = TRUE)) {
      log_warn("R.utils не найден – тайм‑аут не применяется (context: {context})")
      return(eval_once())
    }
    R.utils::withTimeout(eval_once(), timeout = timeout_ms / 1000, onTimeout = "error")
  }
  
  attempt <- 1L
  repeat {
    start_time <- Sys.time()
    
    res <- tryCatch(exec_with_timeout(),
                    warning = function(w) {
                      log_warn("Предупреждение ({context}): {w$message}")
                      invokeRestart("muffleWarning")
                    }, error = function(e) e)
    
    # успех ------------------------------------------------------------------
    if (!inherits(res, "error")) {
      if (isTRUE(log_time)) {
        dt <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
        log_debug("{context} завершено за {dt} с")
      }
      if (is.function(on_success)) safe_exec(on_success(res), context = paste0(context, "::on_success"))
      return(res)
    }
    
    # неудача ---------------------------------------------------------------
    log_error("Ошибка ({context}, попытка {attempt}/{retries}): {res$message}")
    if (!is.null(getDefaultReactiveDomain()))
      showNotification(sprintf("Ошибка (%s): %s", context, res$message),
                       type = "error", duration = 6)
    
    if (attempt < retries) {
      Sys.sleep(retry_delay)
      attempt <- attempt + 1L
      next
    }
    
    # все попытки исчерпаны --------------------------------------------------
    if (is.function(on_error)) safe_exec(on_error(res), context = paste0(context, "::on_error"))
    return(default_value)
  }
}

################################################################################
# 2a. SAFE_QUICK – УПРОЩЁННЫЙ ВАРИАНТ ----------------------------------------
################################################################################
# safe_quick(expr, context = "generic", default = NULL)
# ▸ tryCatch вокруг expr
# ▸ Нет повторов, тайм‑аута, измерения времени, колбеков
# ▸ Минимальный overhead (≈0.03мс)
################################################################################

safe_quick <- function(expr, context = "generic", default = NULL) {
  tryCatch(expr,
           warning = function(w) { log_warn("Предупреждение ({context}): {w$message}"); invokeRestart("muffleWarning") },
           error   = function(e) {
             log_error("Ошибка ({context}): {e$message}")
             if (!is.null(getDefaultReactiveDomain()))
               showNotification(sprintf("Ошибка (%s): %s", context, e$message), type = "error", duration = 6)
             default
           }
  )
}

################################################################################
# 3. ГЛОБАЛЬНЫЙ ОБРАБОТЧИК НЕПЕРЕХВАЧЕННЫХ ОШИБОК ---------------------------
################################################################################
set_global_error_handler <- function() {
  options(shiny.error = function() {
    err_txt <- geterrmessage()
    log_fatal("НЕПЕРЕХВАЧЕННОЕ исключение: {err_txt}")
    if (!is.null(getDefaultReactiveDomain()))
      showModal(modalDialog(title = "Критическая ошибка",
                            paste("Приложение остановлено:", err_txt),
                            easyClose = TRUE, footer = NULL))
  })
}

################################################################################
# 4. ТРЕКАП ДЕЙСТВИЙ ПОЛЬЗОВАТЕЛЯ (shinylogs) --------------------------------
################################################################################
start_usage_tracking <- function(app_name = "MonitoringApp", dir = "logs") {
  if (!requireNamespace("shinylogs", quietly = TRUE)) {
    log_warn("Пакет shinylogs не установлен – трекинг не активирован")
    return(invisible(FALSE))
  }
  shinylogs::track_usage(storage_mode = "json", app_name = app_name, path = dir)
  log_info("shinylogs enabled (каталог: {dir})")
  invisible(TRUE)
}

################################################################################
# ПРИМЕР ПОДКЛЮЧЕНИЯ -----------------------------------------------------------
# source("errorHandler.R")
# log_init(level = INFO)
# set_global_error_handler()  # сессия завершится при критической ошибке
# start_usage_tracking()
#
# safe_exec({ read.csv(input$file$datapath) }, context = "Чтение CSV",
#           retries = 3, retry_delay = 1, timeout_ms = 5000)
################################################################################
