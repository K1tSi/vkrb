  
  PATH <- "W:/Diplom/3"
  setwd(PATH)
  # app.R
  library(shiny)
  library(shinythemes)
  source("errorHandler.R")          
  log_init(level = DEBUG)            # каталоги logs/2025-06-01.log …
  set_global_error_handler()        # ловец неперехваченных ошибок
  
  # Подключаем модули
  #source("mapModule_look.R")
  source("reportManager.R")
  source("reportModule.R")
  
  source("dataLoadModule.R")
  source("dataPreprocessModule.R")
  source("kmeansModule.R")
  source("dbscanModule.R")
  
  
  source("mapModule.R")
  source("dataSourceModule.R")
  # при необходимости: другие файлы
  
  source("regressionModule.R")     # regress
  
  source("classificationModule.R")
  
  #source("vizModule.R")
  
  conflicted::conflicts_prefer(shiny::observe)
  conflicted::conflicts_prefer(shiny::validate)
  
  ui <- fluidPage(    theme = shinytheme("flatly"), # красивая тема
    navbarPage("Приложение анализа данных",
               
               tabPanel("Загрузка",
                        dataSourceUI("ds_module")  # вызываем UI модуля
               ),
               tabPanel("Предобработка",
                        dataPreprocessUI("prep")  # вызываем UI модуля
               ),
               
               tabPanel("Кластеризация",
                        navbarPage(
                          
                          "Методы кластеризации",
                          tabPanel("K-means", kmeansUI("kmeans")),
                          tabPanel("DBSCAN", dbscanUI("dbscan"))
                        )
               ),
               tabPanel("Регрессия",  regressionUI("reg")),
               
               tabPanel("Классификация",
                        classificationUI("classif")
               ), 
               tabPanel("Отчёт", reportUI("rep")),
               #tabPanel("Визуализация", vizUI("viz_g lobal"))
    ),
    # ----- «Летающая» корзина отчёта ---------------------------------------
    absolutePanel(id = "reportTray", right = 0, top = "30%",
                  draggable = TRUE, width = 300,
                  style = "opacity:0.92; background:#f7f7f7; padding:10px;",
                  h4("Отчёт"),
                  uiOutput("tray_blocks"),           # сюда будем выводить выбранные блоки
                  actionButton("openReport", "Сформировать", class = "btn-primary")
    )
  )
  
  server <- function(input, output, session) {
    # 1) Запускаем модуль загрузки/предобработки
    #    Возвращает список reactive(), например: data_reactive$final_data()
    reportServer("rep")
    data_reactive <- dataSourceServer("ds_module")
    observe({
      df <- data_reactive$final_data()
      if(!is.null(df) && !is.na(df)) {
        cat("Загружено строк:", nrow(df), "\n")
        # Тут можно выполнять дальнейшую обработку df
      }
    })
    data_clean <- dataPreprocessServer("prep", data_reactive)
    
    #vizServer("viz_global", reactive({ data_clean$final_data() }))
    # 2) Запускаем модуль K-means, передаём ему данные из data_reactive
    data_klaster <- kmeansServer("kmeans", data_clean)
    
    # 3) Запускаем модуль DBSCAN
    dbscanServer("dbscan", data_clean)
    
    reg <- regressionServer("reg", data_clean)
    classificationServer("classif", data_klaster)
    
    ##отчетность--------------------------------------------------------
    
    output$tray_blocks <- renderUI({
      blk <- get_report_blocks()      # из reportManager.R
      if (length(blk) == 0)
        return(em("Ничего не выбрано"))
      
      tags$ul(lapply(blk, function(b) tags$li(b$title)))
    })
    
    observeEvent(input$openReport, {
      showModal(
        modalDialog(
          title = "Формирование отчёта",
          size  = "l",
          easyClose = TRUE,
          footer = NULL,
          reportUI("rep_modal")        # отчёт внутри модального окна
        )
      )
      reportServer("rep_modal")       # запускаем модуль отчёта
    })
    
    
    # 4) Запускаем модуль карты
    #mapServer("map_mod")
    
    # 5)  можно читать, что вернул mapServer, если вы внутри (апример, facilityId через return(list(...)) нужно сохранить в переменную:
    # map_data <- mapServer("map_mod")
    # observe({
    #   fid <- map_data$facilityId()
    #   if (!is.null(fid)) {
    #     cat("Выбран объект:", fid, "\n")
    #   }
    # })
  }
  
  shinyApp(ui, server)
  
