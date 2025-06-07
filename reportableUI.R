# ──────────────────────────────────────────────────────────────
# reportableUI.R  –  helper, позволяющий добавлять любой output
#                   в конструктор отчётов одним кликом «плюса».
# ──────────────────────────────────────────────────────────────
library(shiny)

# output_ui    – готовый UI‑объект (plotOutput / DTOutput / …)
# block_id     – уникальный id блока
# block_title  – заголовок, который увидит пользователь в отчёте
# content_fun  – функция без аргументов, возвращающая UI для отчёта
reportable <- function(output_ui, block_id, block_title, content_fun) {
  
  ns <- NS(block_id)   # нужен для уникальности id в DOM
  
  # плавающий значок «+»
  plus_icon <- actionLink(
    ns("add"), NULL,
    icon("plus-circle", style = "color:#337ab7;font-size:20px;"),
    style = "position:absolute; top:4px; right:4px;"
  )
  
  # обёртка с output + иконка
  ui <- tagList(
    div(style = "position:relative; display:inline-block;",
        output_ui, plus_icon),
    tags$script(HTML(sprintf(
      "
      $('#%s').on('click', function(){
        Shiny.setInputValue('%s_add_click', +new Date(), {priority: 'event'});
      });
      ",
      ns("add"), ns("")
    )))
  )
  
  # серверная часть «плюсика»
  moduleServer(block_id, function(input, output, session) {
    observeEvent(input$add_click, {
      add_report_block(block_id, block_title, content_fun)
    })
  })
  
  ui
}
snapshot_png <- function(plot_expr, width = 1600, height = 1200, res = 150){
  file <- tempfile(fileext = ".png")
  png(file, width = width, height = height, res = res)
  on.exit(dev.off())
  plot_expr()            # вызываем переданную функцию‑построитель
  file                   # возвращаем путь
}
library(shiny)

# ------------------------ UI-генератор ---------------------------------------
reportablePlotUI <- function(id, height = NULL, width = "100%") {
  tagList(
    plotOutput(outputId = paste0(id, "_plot")),
    div(style = "position:relative; display:inline-block;",
        actionLink(inputId = paste0(id, "_add"), label = NULL,
                   icon("plus-circle", style = "color:#337ab7;font-size:20px;"),
                   style = "position:absolute; top:4px; right:4px;")
    )
  )
}

# ------------------------ Привязка логики ------------------------------------
reportablePlotAttach <- function(id, plot_expr, title = "Plot",
                                 input, output, session) {
  print("plot works1")
  output[[paste0(id, "_plot")]] <- renderPlot({
    plot_expr()
    # если пользователь возвращает ggplot – печатаем
    #if (inherits(res, c("gg", "ggplot"))) print(res)
  })
  
  observeEvent(input[[paste0(id, "_add")]], {
    print("plot works2")
    img_path <- snapshot_png(plot_expr)
    b64      <- base64enc::dataURI(file = img_path, mime = "image/png")
    preview  <- tags$img(src = b64, style = "width:100%;")
    
    block_id <- paste0("plot_", id, "_", as.integer(Sys.time()))
    add_report_block(
      id          = block_id,
      title       = title,
      preview_ui  = preview,
      content_fun = function(){ knitr::include_graphics(img_path) }
    )
    
    showNotification(sprintf("%s добавлен в отчёт", title),
                     type = "message", duration = 2)
  }, ignoreInit = TRUE)
}




# ---------------------------------------------------------------
# reportable_plot_ui(id, title)
# Возвращает готовый UI-контейнер (plotOutput + «плюс»)
# ---------------------------------------------------------------
reportable_plot_ui <- function(id, title) {
  reportable(
    plotOutput(id, height = "430px"),
    block_id    = id,          # идентификатор блока в отчёте
    block_title = title,
    content_fun = NULL         # knit-функция добавится позже
  )
}

# ------------------------------------------------------------
# helper: reportable_plot()
# ------------------------------------------------------------
#  ▸ id, title          – как раньше;
#  ▸ build_fun          – функция без аргументов, которая РИСУЕТ график
#                         (ggplot, base, lattice, grid, plotly::plotly_BUILD …);
#  ▸ w, h, res          – параметры для snapshot_png (можно не менять).
# ------------------------------------------------------------
reportable_plot <- function(id, title, build_fun,
                            w = 1600, h = 1200, res = 150) {
  
  ## 1) UI-контейнер с «плюсом»
  plt_out <- plotOutput(NS(id, "plot"))
  ui <- reportable(plt_out, block_id = id, block_title = title,
                   content_fun = build_fun)          # уже для knit()
  
  ## 2) server-часть
  moduleServer(id, function(input, output, session) {
    
    ## рендерим график «вживую» для пользователя
    output$plot <- renderPlot({ build_fun() })
    
    ## при клике «+» автоматически формируем PNG и регистрируем блок
    observeEvent(input$add_click, {
      img <- snapshot_png(build_fun, width = w, height = h, res = res)
      preview <- tags$img(src = base64enc::dataURI(file = img, mime = "image/png"),
                          style = "width:100%;")
      add_report_block(id, title, preview_ui = preview,
                       content_fun = function() knitr::include_graphics(img))
      showNotification(paste(title, "добавлен в отчёт"), type = "message")
    }, ignoreInit = TRUE)
  })
  
  ui   # ← возвращаем готовый UI-фрагмент
}
## для интерактивных
reportable_plot_html <- function(id, title, widget_fun) {
  ui <- htmlOutput(NS(id, "html"))
  reportable(ui, id, title, content_fun = function() htmltools::tagList(widget_fun()))
  moduleServer(id, function(input, output, session) {
    output$html <- renderUI({ widget_fun() })
    observeEvent(input$add_click, {
      add_report_block(id, title,
                       preview_ui  = widget_fun(),        # интерактив в корзине
                       content_fun = widget_fun)          # интерактив в knit
    })
  })
}

