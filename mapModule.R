#---------------------------------------
# Модуль для работы с картой
#---------------------------------------
library(shiny)
library(leaflet)
library(jsonlite)
library(shinyjs)
library(dplyr)

# UI-функция
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Кнопка скрыть/показать карту — опционально
    checkboxInput("show_map", "Показать карту", value = TRUE),
    
    # Контейнер с картой
    conditionalPanel(
      condition = "input.show_map == true",
      div(
        id = ns("mapPanel"),
        leafletOutput(ns("map"), width = "100%", height = "400px")
      )
    ),
    
    # Выбор объекта
    selectInput(ns("facilitySelect"), "Выберите объект:", choices = NULL),
    
    # Таблица с тегами (или краткими характеристиками)
    tableOutput(ns("facility_tags"))
  )
}

# Server-функция
mapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      #-------------------------------------------------------------------
      # 1) Загрузка списков объектов (через API)
      #-------------------------------------------------------------------
      url_facilities <- "https://tes.cloud.nstu.ru/proxy/Facility/GetFacilityList"
      df_facilities <- tryCatch({
        fromJSON(url_facilities)
      }, error = function(e) {
        # Если не получилось загрузить с API:
        data.frame(id = integer(0),
                   name = character(0),
                   lat = numeric(0),
                   lng = numeric(0),
                   address = character(0))
      })
      
      # Заполняем selectInput
      observe({
        if (nrow(df_facilities) > 0) {
          updateSelectInput(
            session, "facilitySelect",
            choices = setNames(df_facilities$id, paste0(df_facilities$name, " [", df_facilities$id, "]")),
            selected = df_facilities$id[1]
          )
        }
      })
      
      #-------------------------------------------------------------------
      # 2) Рендер карты с маркерами
      #-------------------------------------------------------------------
      output$map <- renderLeaflet({
        req(nrow(df_facilities) > 0)
        leaflet(data = df_facilities) %>%
          addTiles() %>%
          addMarkers(
            lng = ~lng, lat = ~lat,
            layerId = ~as.character(id),
            popup = ~paste0("<b>", name, "</b><br/>", address)
          )
      })
      
      
      #-------------------------------------------------------------------
      # 4) Подгрузка тегов/параметров по выбранному объекту (для таблицы)
      #-------------------------------------------------------------------
      getFacilityTags <- function(facility_id) {
        tags_url <- paste0("https://tes.cloud.nstu.ru/proxy/tags/getFacilityTagList?facilityId=", facility_id)
        df_tags <- tryCatch(fromJSON(tags_url), error = function(e) NULL)
        df_tags
      }
      
      # Реактивное хранилище для выбранного ID (facilityId)
      facilityId <- reactiveVal(NULL)
      
      # При смене в selectInput
      observeEvent(input$facilitySelect, {
        selected_id <- input$facilitySelect
        facilityId(as.numeric(selected_id))
        
        # Обновляем таблицу тегов
        tags_data <- getFacilityTags(selected_id)
        output$facility_tags <- renderTable({
          if (is.null(tags_data) || is.null(nrow(tags_data)) || nrow(tags_data) == 0) {
            data.frame(Сообщение = paste("Нет параметров для объекта ID =", selected_id))
          } else {
            tags_data[, c("id", "name", "uom", "description")]
          }
        })
        
        # Центрируем карту на объект
        coords <- df_facilities %>% filter(id == as.numeric(selected_id))
        if (nrow(coords) == 1 && coords$lat != 0 && coords$lng != 0) {
          leafletProxy(ns("map")) %>%
            setView(lng = coords$lng, lat = coords$lat, zoom = 14)
        }
      })
      
      # При клике на маркер (синхронизируем с selectInput)
      observeEvent(input$map_marker_click, {
        click <- input$map_marker_click
        if (!is.null(click)) {
          clicked_id <- click$id
          facilityId(as.numeric(clicked_id))
          
          tags_data <- getFacilityTags(clicked_id)
          output$facility_tags <- renderTable({
            if (is.null(tags_data) || nrow(tags_data) == 0) {
              data.frame(Сообщение = paste("Нет параметров для объекта ID =", clicked_id))
            } else {
              tags_data[, c("id", "name", "uom", "description")]
            }
          })
          
          # Синхронизация списка
          updateSelectInput(session, "facilitySelect", selected = clicked_id)
        }
      })
      
      #-------------------------------------------------------------------
      # 5) Возвращаем reactive-значение facilityId(),
      #    чтобы родительский модуль/приложение знал, какой объект выбран
      #-------------------------------------------------------------------
      return(
        list(
          facilityId = facilityId
        )
      )
    }
  )
}
