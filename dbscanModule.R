# file: dbscanModule.R
library(shiny)
library(dbscan)
library(factoextra)
library(ggplot2)

dbscanUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Метод DBSCAN кластеризации"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("dbscan_vars"), "Выберите переменные для кластеризации", choices = NULL, multiple = TRUE),
        sliderInput(ns("eps_slider"), "Значения eps", min = 0.01, max = 1, value = c(0.1, 0.5), step = 0.01),
        numericInput(ns("eps_k"), "Шаг eps", min = 0.01, max = 0.1, value = 0.05),
        sliderInput(ns("minPts_slider"), "Значения minPts", min = 3, max = 9, value = c(4, 6), step = 1),
        actionButton(ns("run_heatmap"), "Построить тепловые карты"), hr(),
        
        numericInput(ns("dbscan_eps"), "Радиус окрестности (eps)", min = 0.1, max = 10, value = 0.5),
        numericInput(ns("dbscan_minPts"), "Минимальное количество точек (minPts)", min = 1, max = 10, value = 5),
        actionButton(ns("run_dbscan"), "Запустить DBSCAN")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Выбор оптимальных параметров", 
                   plotOutput(ns("heatmap_plot1")), 
                   plotOutput(ns("heatmap_plot2"))),
          tabPanel("Результаты кластеризации", 
                   plotOutput(ns("dbscan_plot")),
                   verbatimTextOutput(ns("dbscan_res")),
                   verbatimTextOutput(ns("dbscan_res1"))),
          tabPanel("Таблица кластеров", DT::DTOutput(ns("dbscan_table")))
        )
      )
    )
  )
}

dbscanServer <- function(id, data_reactive) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Обновляем список числовых переменных
      observe({
        df <- data_reactive$final_data()
        req(df)
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        updateSelectInput(session, "dbscan_vars", choices = numeric_cols)
      })
      
      # 1) Построение тепловых карт
      observeEvent(input$run_heatmap, {
        req(input$dbscan_vars)
        df <- data_reactive$final_data()
        req(df)
        raw <- df[, input$dbscan_vars, drop = FALSE]
        scaled_data <- scale(raw)
        
        eps_values <- seq(input$eps_slider[1], input$eps_slider[2], by = input$eps_k)
        minPts_values <- seq(input$minPts_slider[1], input$minPts_slider[2], by = 1)
        
        results <- expand.grid(eps = eps_values, minPts = minPts_values)
        results$silhouette <- NA
        results$n_clusters <- NA
        
        for (i in seq_len(nrow(results))) {
          e <- results$eps[i]
          m <- results$minPts[i]
          db <- dbscan(scaled_data, eps = e, minPts = m)
          # Сила кластера
          valid_clusters <- db$cluster[db$cluster != 0]
          if (length(unique(valid_clusters)) > 1) {
            sil <- cluster::silhouette(db$cluster, dist(scaled_data))
            results$silhouette[i] <- mean(sil[, 3])
          }
          results$n_clusters[i] <- length(unique(valid_clusters))
        }
        
        output$heatmap_plot1 <- renderPlot({
          ggplot(results, aes(x = eps, y = minPts, fill = n_clusters)) +
            geom_tile() +
            geom_text(aes(label = round(n_clusters, 2)), color = "black", size = 3) +
            scale_fill_viridis_c(option = "plasma", na.value = "white") +
            labs(
              title = "Число кластеров при разных eps/minPts",
              x = "eps", y = "minPts", fill = "Число кластеров"
            ) +
            theme_minimal()
        })
        output$heatmap_plot2 <- renderPlot({
          ggplot(results, aes(x = eps, y = minPts, fill = silhouette)) +
            geom_tile() +
            geom_text(aes(label = round(silhouette, 2)), color = "black", size = 3) +
            scale_fill_viridis_c(option = "plasma", na.value = "white") +
            labs(
              title = "Средняя оценка силуэта",
              x = "eps", y = "minPts", fill = "Силуэт"
            ) +
            theme_minimal()
        })
      })
      
      # 2) Запуск DBSCAN
      observeEvent(input$run_dbscan, {
        req(input$dbscan_vars)
        df <- data_reactive$final_data()
        req(df)
        raw <- df[, input$dbscan_vars, drop = FALSE]
        scaled_data <- scale(raw)
        
        db_res <- dbscan(scaled_data, eps = input$dbscan_eps, minPts = input$dbscan_minPts)
        
        output$dbscan_plot <- renderPlot({
          fviz_cluster(db_res, data = scaled_data, geom = "point", stand = FALSE) +
            ggtitle("Результаты DBSCAN")
        })
        
        clustered_data <- data.frame(raw, Cluster = as.factor(db_res$cluster))
        output$dbscan_table <- DT::renderDT({
          DT::datatable(clustered_data, options = list(scrollX = TRUE, autoWidth = TRUE))
        })
        
        output$dbscan_res <- renderPrint({
          table(clustered_data$Cluster)
        })
        output$dbscan_res1 <- renderPrint({
          cluster_stats <- clustered_data %>%
            group_by(Cluster) %>%
            summarise(across(where(is.numeric),
                             list(
                               Mean = ~mean(.x, na.rm = TRUE),
                               Min = ~min(.x, na.rm = TRUE),
                               Max = ~max(.x, na.rm = TRUE),
                               SD = ~sd(.x, na.rm = TRUE)
                             )))
          cluster_stats
        })
      })
    }
  )
}
