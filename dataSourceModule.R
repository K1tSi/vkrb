#---------------------------------------
# Модуль выбора источника данных
#---------------------------------------
library(shiny)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(shinythemes)
library(fuzzyjoin)
# source("mapModule.R")  # если нужно подключать отдельно

dataSourceUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    theme = shinytheme("flatly"), # красивая тема
    titlePanel("Загрузка данных"), # Заголовок
    # Радиокнопки: "Файл" или "Карта"
    
    radioButtons(
      inputId = ns("source_type"),
      label = "Выберите источник данных:",
      choices = c("Файл", "Карта"),
      inline = TRUE
    ),
    
    # ============ Если выбран Файл ============
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Файл'", ns("source_type")),
      fileInput(ns("file_upload"), "Загрузите CSV", accept = c(".csv", ".txt")),
      helpText("Настройки чтения CSV можно дополнять: сепаратор, заголовки и т.д.")
    ),
    
    # ============ Если выбрана Карта ============
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Карта'", ns("source_type")),
      # UI модуля карты
      mapUI(ns("map_module")),
      
      # Период (даты)
      dateInput(ns("startDate"), "Начальная дата:", value = "2023-12-10"),
      dateInput(ns("endDate"),   "Конечная дата:", value = "2024-01-20"),
      
      # Кнопка (активно тянем данные из API)
      actionButton(ns("load_facility_data"), "Загрузить данные по объекту"),
      
      helpText("После выбора объекта на карте и указания периода, нажмите кнопку.")
    ),
    
    hr(),
    
    # Предпросмотр итогового DataFrame (или сообщение)
    h4("Результат загрузки данных"),
    DT::dataTableOutput(ns("preview_data"))
  )
}

dataSourceServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Хранилище для загруженных данных (из файла или с карты)
      finalData <- reactiveVal(NULL)
      
      # 1) Загрузка из файла
      observeEvent(input$file_upload, {
        req(input$file_upload)
        df <- tryCatch({
          read.csv(input$file_upload$datapath, header = TRUE, sep = ",")
        }, error = function(e) {
          showNotification(paste("Ошибка чтения файла:", e$message), type = "error")
          NULL
        })
        finalData(df)
      })
      
      # 2) Загрузка через карту
      #    Подключаем сервер модуля карты
      mapResult <- mapServer("map_module")
      
      # Функция для получения списка тегов (tagId) у объекта
      getFacilityTags <- function(facility_id) {
        if (is.null(facility_id)) return(NULL)
        url_tags <- paste0("https://tes.cloud.nstu.ru/proxy/tags/getFacilityTagList?facilityId=", facility_id)
        tryCatch({
          fromJSON(url_tags)
        }, error = function(e) NULL)
      }
      
      # Функция для подгрузки временного ряда одного tagId
      getTagData <- function(tag_id, start_time, end_time) {
        url_get_values <- paste0(
          "https://tes.cloud.nstu.ru/proxy/tags/GetTagValues",
          "?tagId=", tag_id,
          "&startTime=", start_time,
          "&endTime=", end_time
        )
        df <- tryCatch({
          fromJSON(url_get_values)
        }, error = function(e) NULL)
        
        if (!is.null(df) && !is.null(nrow(df)) && nrow(df) > 0) {
          # Преобразуем timestamp в POSIXct (при желании)
          df$timestamp <- as.POSIXct(df$timestamp, origin = "1970-01-01", tz = "UTC")
          print("Преобразование")
        }
        df
      }
      
      observeEvent(input$load_facility_data, {
        # Получаем ID объекта из модуля карты
        fId <- mapResult$facilityId()
        req(fId)
        
        # Загружаем список тегов для объекта
        tags_info <- getFacilityTags(fId)
        if (is.null(tags_info) || is.null(nrow(tags_info)) || nrow(tags_info) == 0) {
          showNotification("У объекта нет доступных тегов (tagId) или ошибка загрузки.", type = "warning")
          finalData(NULL)
          return()
        }
        print("MARKER")
        # Границы периода
        start_date <- input$startDate
        end_date   <- input$endDate
        
        withProgress(message = "Загрузка данных...", value = 0, {
        
        # Перебираем все теги, подгружаем их данные, складываем в список
        list_of_dfs <- list()
        n_tags <- nrow(tags_info)  # общее число тегов
        
        for (i in seq_len(nrow(tags_info))) {
          # 1) Заголовок прогресса будет меняться при каждой итерации
          incProgress(
            amount = 1 / n_tags,
            detail = paste("Тег", i, "из", n_tags)
          )
          
          #2 загрузка одного tag
          tag_id   <- tags_info$id[i]
          tag_name <- tags_info$name[i]
          
          df_tag <- getTagData(tag_id, start_date, end_date)
          if (!is.null(df_tag) && !is.null(nrow(df_tag)) && nrow(df_tag) > 0) {
            # Переименуем value в безопасное имя на базе tag_name
            safe_col <- make.names(tag_name)
            df_tag <- df_tag %>%
              select(timestamp, value) %>%
              rename(!!safe_col := value)
            
            list_of_dfs[[length(list_of_dfs) + 1]] <- df_tag
          }
        }
        print("MARKER")
        if (length(list_of_dfs) == 0) {
          print("MARKER")
          showNotification("Не удалось загрузить данные ни по одному параметру (возможно, нет измерений за этот период).",
                           type = "warning")
          finalData(NULL)
          return()
        }
        
        # Объединяем все фреймы по timestamp (full join)
        #combined_df <- purrr::reduce(list_of_dfs, dplyr::full_join, by = "timestamp")
        
        # задаём допуск (в секундах)
        tol <- 3      
        
        list_of_dfs2 <- map(list_of_dfs, \(df) {
          df %>% 
            mutate(
              ts_group = as.POSIXct(
                round(as.numeric(timestamp) / tol) * tol,  # «округляем» в сторону ближайшего tol
                origin = "1970-01-01", tz   = attr(timestamp, "tzone")
              )
            )
        })
        
        combined_df <- reduce(list_of_dfs2, full_join, by = "ts_group") %>% 
          ## 1. собрать все столбцы с именем timestamp*
          mutate(
            timestamp = coalesce(!!!select(., matches("^timestamp")))  # !!! распаковывает список колонок
          ) %>% 
          ## 2. навести порядок: убрать старые timestamp.* и ts_group
          select(timestamp, everything(), -matches("^timestamp\\..*$"), -ts_group) 
        
        
        
        finalData(combined_df)
      })# конец withProgress
      })  
      
      # 3) Предпросмотр данных
      output$preview_data <- DT::renderDataTable({
        df <- finalData()
        if (is.null(df) || is.null(nrow(df)) || nrow(df) == 0) {
          return(NULL)
        }
        df
      }, options = list(pageLength = 10, scrollX = TRUE,  language = list(url = "https://cdn.datatables.net/plug-ins/1.13.4/i18n/ru.json")))
      
      final_data <- reactive({
        finalData()
      })
      
      # Возвращаем реактив, чтобы в дальнейшем использовать загруженные данные
      return(
        list(
        raw_data = final_data,
        final_data = reactive({NA})
        )
      )
    }
  )
}
