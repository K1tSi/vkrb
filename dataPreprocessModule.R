# -------- dataPreprocessModule.R (расширенный) ----------------
library(shiny)
library(DT)
library(dplyr)
library(janitor)
library(naniar)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)
library(corrplot)
library(GGally)
source("reportableUI.R", local = TRUE)
# -------------------------- UI --------------------------------
dataPreprocessUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Предобработка данных (временные ряды)"),
    sidebarLayout(
      sidebarPanel(
        ## -- выбор числовых переменных для очистки --
        uiOutput(ns("var_select_ui")),
        checkboxInput(ns("snake"),    "Переименовать колонки (snake_case)", FALSE),
        checkboxInput(ns("empty"),    "Удалить пустые столбцы",            FALSE),
        checkboxInput(ns("constant"), "Удалить константные столбцы",       FALSE),
        
        hr(), tags$h4("Блок времени"),
        uiOutput(ns("time_col_ui")),
        textInput(ns("time_fmt"), "Формат (strftime) или '' для auto", ""),
        selectInput(ns("tz"), "TZ", choices = OlsonNames(), selected = "UTC"),
        selectInput(ns("freq"), "Частота",
                    choices = c("1 min","5 min","15 min","1 hour","1 day"),
                    selected = "1 hour"),
        selectInput(ns("agg_fun"), "Если сжимаем → как агрегировать?",
                    choices = c("mean", "sum", "last")),
        selectInput(ns("fill_method"), "Заполнение NA",
                    c("none","locf","linear","spline","const")),
        numericInput(ns("fill_const"), "Const (если 'const')", 0),
        
        hr(), tags$h4("Выбросы и лаги"),
        selectInput(ns("out_method"), "Выбросы",
                    c("None","IQR","Hampel")),
        numericInput(ns("lag_n"), "Сколько лагов добавить", value = 0, min = 0),
        uiOutput(ns("lag_cols_ui")),
        
        hr(), tags$h4("Скользящий расчёт"),
        numericInput(ns("roll_n"),  "Окно N", value = 0, min = 0),
        selectInput(ns("roll_fun"), "Функция", c("mean","median","sd")),
        
        hr(), tags$h4("Масштабирование"),
        selectInput(ns("scale_method"), "Способ",
                    c("None","Z-score","Min-max")),
        
        hr(),
        actionButton(ns("run_clean"), "Запустить", class = "btn-primary"),
        downloadButton(ns("download"), "Скачать")
        
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Исходные",  DTOutput(ns("raw_data"))),
          tabPanel("Очищенные", DTOutput(ns("clean_data"))),
          tabPanel("Пропуски",  
                   #plotOutput(ns("missing_plot"))
                   reportablePlotUI(ns("missing_plot"))
                   ),
          tabPanel("Лин.график",plotlyOutput(ns("ts_plot"))),
          tabPanel("Корреляция",
                   uiOutput(ns("corr_vars")),
                   checkboxInput(ns("corr_style"), "GGpairs", FALSE),
                   actionButton(ns("update_corr"), "Перерисовать"),
                   # контейнер, где лежит сам plot + плавающий плюс
                   #####
                   #plotOutput(ns("corr_plot")),
                   #div(
                   #  style = "position:relative; height:0; margin-top:-390px;",
                   #  actionLink(
                   #    ns("corr_add"), NULL,
                   #    icon("plus-circle", style = "color:#337ab7;font-size:20px;"),
                   #    style = "position:absolute; top:10px; right:10px;"
                   #  )
                   #),
                   
                   # >>> новый универсальный виджет
                   reportablePlotUI(ns("corrr"))   # ← плюс-иконка встроена
                   #uiOutput(ns("corr_wrap"))
                   ##plotOutput(ns("corr_plot"))
          )
                   
        )
      )
    )
  )
}

# ------------------------- SERVER -----------------------------
dataPreprocessServer <- function(id, dataReactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## -------- raw_data: что пришло извне ----------------------
    raw_data <- reactive({
      req(dataReactive$raw_data())
    })
    
    output$raw_data <- renderDT({
      datatable(raw_data(), options = list(scrollX = TRUE))
    })
    
    ## -------- UI: выбор переменных/времени/лагов --------------
    output$var_select_ui <- renderUI({
      selectInput(ns("num_var"), "Числовые колонки:",
                  choices = names(raw_data()), multiple = TRUE)
    })
    output$time_col_ui <- renderUI({
      selectInput(ns("time_col"), "Колонка-время:",
                  choices = names(raw_data()))
    })
    output$lag_cols_ui <- renderUI({
      selectInput(ns("lag_cols"), "Для лагов:",
                  choices = names(raw_data())[sapply(raw_data(), is.numeric)],
                  multiple = TRUE)
    })
    
    ## -------- основная обработка ------------------------------
    cleaned_data <- reactiveVal(NULL)
    
    observeEvent(input$run_clean, {
      df <- raw_data()
      req(input$num_var, input$time_col)
      
      ## 1. базовая очистка (NA, snake, empty, constant) ---------
      df[input$num_var] <- lapply(df[input$num_var], as.numeric)
      df <- df %>% na.omit()
      if (isTRUE(input$snake))    names(df) <- make_clean_names(names(df))
      if (isTRUE(input$empty))    df <- remove_empty(df, "cols")
      if (isTRUE(input$constant)) df <- remove_constant(df)
      
      ## 2. парсим время, регуляризуем ---------------------------
      fmt <- trimws(input$time_fmt)
      df$.__time <- if (fmt == "")
        as_datetime(df[[input$time_col]], tz = input$tz)
      else
        parse_date_time(df[[input$time_col]], orders = fmt, tz = input$tz)
      df <- arrange(df, .__time)
      
      agg_fun <- switch(input$agg_fun,
                        mean = \(x) mean(x, na.rm = TRUE),
                        sum  = \(x) sum(x,  na.rm = TRUE),
                        last = \(x) dplyr::last(x))
      
      # 2.1 кладём всё в «корзины» нужной частоты
      df <- df |>
        mutate(.bucket = lubridate::floor_date(.__time, unit = input$freq)) |>
        group_by(.bucket) |>
        summarise(across(where(is.numeric), agg_fun),
                  .groups = "drop") |>
        rename(.__time = .bucket) |>
        arrange(.__time)
      
      # 2.2 формируем регулярную сетку и join
      full_seq <- seq(min(df$.__time), max(df$.__time), by = input$freq)
      df <- full_join(data.frame(.__time = full_seq), df, by = ".__time") |>
        arrange(.__time)
      
      ## 3. заполняем пропуски -----------------------------------
      if (input$fill_method != "none") {
        num_cols <- names(df)[sapply(df, is.numeric)]
        for (cl in num_cols) {
          v <- df[[cl]]
          if (input$fill_method == "locf")   v <- zoo::na.locf(v, na.rm = FALSE)
          if (input$fill_method == "linear") v <- zoo::na.approx(v, na.rm = FALSE)
          if (input$fill_method == "spline") v <- zoo::na.spline(v, na.rm = FALSE)
          if (input$fill_method == "const")  v[is.na(v)] <- input$fill_const
          df[[cl]] <- v
        }
      }
      
      ## 4. выбросы ---------------------------------------------
      if (input$out_method != "None") {
        num_cols <- names(df)[sapply(df, is.numeric)]
        for (cl in num_cols) {
          x <- df[[cl]]
          if (input$out_method == "IQR") {
            q <- quantile(x, c(.25,.75), na.rm = TRUE)
            iqr <- diff(q); lo <- q[1]-1.5*iqr; hi <- q[2]+1.5*iqr
            x[x<lo | x>hi] <- NA
          } else if (input$out_method == "Hampel") {
            med <- median(x,na.rm=TRUE); s <- mad(x,na.rm=TRUE)
            x[abs(x-med) > 3*s] <- NA
          }
          df[[cl]] <- x
        }
      }
      
      ## 5. скользящее окно -------------------------------------
      if (input$roll_n > 1) {
        roll_fun <- switch(input$roll_fun,
                           mean   = rollmean,
                           median = function(z,n) rollapply(z,n,median,fill=NA,align="right"),
                           sd     = function(z,n) rollapply(z,n,sd,    fill=NA,align="right"))
        for (cl in names(df)[sapply(df, is.numeric)]) {
          df[[paste0(cl,"_roll_",input$roll_fun,input$roll_n)]] <-
            roll_fun(df[[cl]], input$roll_n, fill = NA, align = "right")
        }
      }
      
      ## 6. лаги по календарю ------------------------------------
      if (input$lag_n > 0 && length(input$lag_cols) > 0) {
        for (cl in input$lag_cols) {
          for (k in seq_len(input$lag_n)) {
            df[[paste0(cl,"_lag",k)]] <- dplyr::lag(df[[cl]], k)
          }
        }
      }
      
      ## 7. масштабирование -------------------------------------
      if (input$scale_method != "None") {
        num_cols <- names(df)[sapply(df, is.numeric)]
        for (cl in num_cols) {
          x <- df[[cl]]
          if (input$scale_method == "Z-score") x <- scale(x)
          if (input$scale_method == "Min-max")
            x <- (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
          df[[cl]] <- x
        }
      }
      
      cleaned_data(df)
    })
    
    ## ----------- выводы ---------------------------------------
    output$clean_data <- renderDT({
      req(cleaned_data())
      datatable(cleaned_data(), options = list(scrollX = TRUE))
    })
    
    #output$missing_plot <- renderPlot({
    #  req(cleaned_data())
    #  gg_miss_var(cleaned_data()) + theme_minimal()
    #})
    
    reportablePlotAttach(
      id    = "missing_plot",                         # без ns() в server-части
      title = "Количество пропусков",
      plot_expr = function(){
        req(cleaned_data())
        p <- gg_miss_var(cleaned_data()) + theme_minimal()
        print(p)
      },
      input   = input,
      output  = output,
      session = session
    )
    
    
    output$ts_plot <- renderPlotly({
      req(cleaned_data())
      num <- names(cleaned_data())[sapply(cleaned_data(), is.numeric)]
      if (length(num) == 0) return(NULL)
      
      plot_ly(cleaned_data(), x = ~`.__time`) |>
        add_lines(y = as.formula(paste0("~`", num[1], "`")),
                  name = num[1])
    })
    
    ## ------- корреляция ---------------------------------------
    output$corr_vars <- renderUI({
      req(cleaned_data())
      selectizeInput(ns("corr_sel"), "Переменные:",
                     choices = names(cleaned_data())[sapply(cleaned_data(), is.numeric)],
                     multiple = TRUE)
    })
    output$corr_plot <- renderPlot({
      req(input$corr_sel,input$update_corr, cleaned_data())
      m <- cleaned_data()[, input$corr_sel, drop = FALSE]
      if (input$corr_style) ggpairs(m) else
        corrplot(cor(m,use="pair"), method="color", type="upper",
                 addCoef.col="black", diag=FALSE)
    })
    
    # ───── корретляционный график + отчёт ─────────────────────────
    reportablePlotAttach(
      id    = "corrr",                         # без ns() в server-части
      title = "Матрица корреляций",
      plot_expr = function(){
        req(cleaned_data(), input$corr_sel,input$update_corr)
        m <- cleaned_data()[ , input$corr_sel, drop = FALSE]
        if (input$corr_style) {
          p <- GGally::ggpairs(m, progress = FALSE)
          print(p)
        } else {
          corrplot::corrplot(cor(m, use = "pair"),
                             method = "color", type = "upper",
                             addCoef.col = "black", diag = FALSE)
        }
      },
      input   = input,
      output  = output,
      session = session
    )
    
    
    # (2) при клике на плюс добавляем блок в отчёт
    observeEvent(input$corr_add, {
      # ------- 1) формируем функцию, которая строит ГРАФИК ------------------
      plot_builder <- function(){
        m <- cleaned_data()[ , input$corr_sel, drop = FALSE]
        if (input$corr_style) {
          p <- GGally::ggpairs(m, progress = FALSE)
          print(p)
        } else {
          corrplot::corrplot(cor(m, use = "pair"), method = "color",
                             type = "upper", addCoef.col = "black", diag = FALSE)
        }
      }
      
      # ------- 2) «фотографируем» её в PNG ---------------------------------
      img_path <- snapshot_png(plot_builder)
      
      # ------- 3) превью для Shiny‑корзины (base64) -------------------------
      #  (используем dataURI, чтобы не возиться с www/)
      b64 <- base64enc::dataURI(file = img_path, mime = "image/png")
      preview_ui <- tags$img(src = b64, style = "width:100%;")
      
      # ------- 4) функция для knit — include_graphics ----------------------
      knit_fun <- function(){
        knitr::include_graphics(img_path)
      }
      
      block_id <- paste0("corr_", session$ns(""))
      add_report_block(
        id          = block_id,
        title       = "Матрица корреляций",
        preview_ui  = preview_ui,
        content_fun = knit_fun          # <‑‑ будет вставлять ИМЕННО этот PNG
      )
      
      showNotification("Снимок матрицы корреляций добавлен в отчёт",
                       type = "message", duration = 2)
    })
    
    #observeEvent(input$update_corr, { output$corr_plot <- output$corr_plot })
    
    ## -------- download ----------------------------------------
    output$download <- downloadHandler(
      filename = function() "final_data.csv",
      content = function(file) {
        req(cleaned_data()); write.csv(cleaned_data(), file, row.names = FALSE)
      })
    
    ## -------- возврат наружу ----------------------------------
    final_data <- reactive({
      if (is.null(cleaned_data())) raw_data() else cleaned_data()
    })
    
    return(list(
      raw_data   = raw_data,
      final_data = final_data
    ))
  })
}
