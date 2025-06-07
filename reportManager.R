# ──────────────────────────────────────────────────────────────
# reportManager.R  —  глобальный «реестр» блоков отчёта
# ──────────────────────────────────────────────────────────────
library(shiny)

# reactives видны всем модулям — кладём в глобал
reportManager <- reactiveValues(
  report_blocks = list()   # id  -> list(title, content_expr)
)

# ───── обновленный конструктор ─────
add_report_block <- function(id, title, preview_ui, content_fun,
                             caption  = "",
                             enabled  = TRUE) {
  reportManager$report_blocks[[id]] <- list(
    title       = title,
    caption     = caption,
    enabled     = enabled,
    preview_ui  = preview_ui,
    content_fun = content_fun
  )
}


# вернуть список блоков
get_report_blocks <- function() {
  reportManager$report_blocks
}
# вспомогательные функции (добавьте в том же файле)
set_block_caption <- function(id, txt)
  reportManager$report_blocks[[id]]$caption <- txt

toggle_block <- function(id)
  reportManager$report_blocks[[id]]$enabled <-
  !isTRUE(reportManager$report_blocks[[id]]$enabled)