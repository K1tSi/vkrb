# --------------------  kmeansModule.R --------------------------
library(shiny)
library(ggplot2)
library(factoextra)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

# -------------------- UI --------------------------------------
kmeansUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Метод K-means кластеризации"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("kmeans_vars"), "Переменные для кластеризации",
                    choices = NULL, multiple = TRUE),
        selectInput(ns("clust_method"), "Метод подбора k",
                    c("Метод локтя" = "wss", "Силуэт" = "silhouette")),
        selectInput(ns("normalization_method"), "Нормализация",
                    c("Z-Score" = "zscore", "Min-Max" = "minmax")),
        actionButton(ns("update_clust"), "Обновить график"), hr(),
        
        numericInput(ns("kmeans_k"), "Число кластеров (k)", 3, 1, 10),
        actionButton(ns("run_kmeans"), "Запустить K-means"), hr(),
        
        uiOutput(ns("d3_plot_var")), hr(),
        uiOutput(ns("kruskal_ui")),  hr(),
        
        ## --- блок временного графика ------------------------------------
        uiOutput(ns("time_plot_ui")),
        actionButton(ns("plot_ts"), "Показать график")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Выбор k",        plotOutput(ns("optimal_clusters_plot"))),
          tabPanel("Результаты",     plotOutput(ns("kmeans_scatter")),
                   plotOutput(ns("kmeans_hist")),
                   plotOutput(ns("kmeans_dim"))),
          tabPanel("Таблица",        DTOutput(ns("kmeans_table"))),
          tabPanel("Анализ",         DTOutput(ns("cluster_stats")),
                   plotOutput(ns("cluster_plot"))),
          tabPanel("3D-график",      plotlyOutput(ns("plot3d_kmean"))),
          tabPanel("Крускал-Уоллис", verbatimTextOutput(ns("kruskal_results"))),
          tabPanel("Временной график", plotlyOutput(ns("ts_plot")))
        )
      )
    )
  )
}

# -------------------- SERVER ----------------------------------
kmeansServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## ---------- 1. подготовка переменных ----------------------
    observe({
      df <- data_reactive$final_data()
      req(df)
      num <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "kmeans_vars", choices = num)
    })
    
    ## ---------- 2. реактивы -----------------------------------
    kmeans_result <- reactiveVal(NULL)   # сам объект kmeans
    labeled_data  <- reactiveVal(NULL)   # df + Cluster
    
    raw_data <- reactive({ data_reactive$final_data() })
    
    ## ---------- 3. график для подбора k -----------------------
    observeEvent(input$update_clust, {
      req(input$kmeans_vars)
      df  <- raw_data()[, input$kmeans_vars, drop = FALSE]
      
      norm <- switch(input$normalization_method,
                     zscore = scale(df),
                     minmax = as.data.frame(lapply(df, \(x)
                                                   (x-min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE)-min(x,na.rm=TRUE)))))
      output$optimal_clusters_plot <- renderPlot({
        fviz_nbclust(norm, kmeans, method = input$clust_method) +
          theme_minimal() + ggtitle(paste("Метод", input$clust_method))
      })
    })
    
    ## ---------- 4. запуск K-means -----------------------------
    observeEvent(input$run_kmeans, {
      req(input$kmeans_vars)
      df  <- raw_data()
      raw <- df[, input$kmeans_vars, drop = FALSE]
      
      norm <- switch(input$normalization_method,
                     zscore = scale(raw),
                     minmax = as.data.frame(lapply(raw, \(x)
                                                   (x-min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE)-min(x,na.rm=TRUE)))))
      
      km <- kmeans(norm, centers = input$kmeans_k)
      kmeans_result(km)
      
      # ----- размечаем данные ---------------------------------
      df_out <- df
      df_out$Cluster <- factor(km$cluster)
      labeled_data(df_out)
      
      ## --- гистограмма распределения --------------------------
      output$kmeans_hist <- renderPlot({
        hist(km$cluster, breaks = input$kmeans_k,
             main = "Распределение по кластерам", xlab = "Кластеры")
      })
      
      ## --- scatter-matrix -------------------------------------
      output$kmeans_scatter <- renderPlot({
        pairs(raw, col = km$cluster, pch = 19,
              main = "Матрица рассеяния")
      })
      
      ## --- PCA-визуализация -----------------------------------
      output$kmeans_dim <- renderPlot({
        fviz_cluster(km, data = raw, geom = "point") +
          ggtitle("PCA-проекция")
      })
      
      ## --- таблица --------------------------------------------
      output$kmeans_table <- renderDT({
        datatable(head(df_out, 500), options = list(scrollX = TRUE))
      })
      
      ## --- описательная статистика ----------------------------
      clust_stats <- df_out |>
        group_by(Cluster) |>
        summarise(across(where(is.numeric),
                         list(mean = ~mean(.x, na.rm = TRUE),
                              sd   = ~sd(.x,   na.rm = TRUE),
                              min  = ~min(.x,  na.rm = TRUE),
                              max  = ~max(.x,  na.rm = TRUE)),
                         .names = "{.col}_{.fn}"),
                  .groups = "drop")
      
      output$cluster_stats <- renderDT(datatable(clust_stats))
      
      output$cluster_plot <- renderPlot({
        long <- pivot_longer(df_out, cols = all_of(input$kmeans_vars),
                             names_to = "Var", values_to = "Val")
        ggplot(long, aes(Var, Val, colour = Cluster)) +
          geom_jitter(alpha = .4) +
          theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      ## --- выбор переменных для 3D -----------------------------
      output$d3_plot_var <- renderUI({
        tagList(
          selectInput(ns("var_x_kmean"), "X:", input$kmeans_vars),
          selectInput(ns("var_y_kmean"), "Y:", input$kmeans_vars),
          selectInput(ns("var_z_kmean"), "Z:", input$kmeans_vars)
        )
      })
      
      ## --- 3D-Plotly ------------------------------------------
      observeEvent(
        {input$var_x_kmean; input$var_y_kmean; input$var_z_kmean},
        {
          req(input$var_x_kmean, input$var_y_kmean, input$var_z_kmean)
          output$plot3d_kmean <- renderPlotly({
            plot_ly(df_out,
                    x = ~get(input$var_x_kmean),
                    y = ~get(input$var_y_kmean),
                    z = ~get(input$var_z_kmean),
                    color = ~Cluster,
                    type = "scatter3d", mode = "markers")
          })
        }, ignoreInit = TRUE)
      
      ## --- Kruskal --------------------------------------------
      output$kruskal_ui <- renderUI({
        selectInput(ns("kruskal_var"), "Переменная:",
                    choices = input$kmeans_vars)
      })
    })
    
    ## ------- кнопка Kruskal-Wallis ----------------------------
    observeEvent(input$run_kruskal, {
      req(kmeans_result(), input$kruskal_var)
      df <- labeled_data()
      res <- kruskal.test(as.formula(paste(input$kruskal_var,"~ Cluster")),
                          data = df)
      output$kruskal_results <- renderText({
        paste0("H = ", round(res$statistic,3),
               ",  p = ", format(res$p.value, digits = 3))
      })
    })
    
    ## ---------- 5. UI и логика для временного графика ----------
    output$time_plot_ui <- renderUI({
      df <- raw_data()
      time_cols <- names(df)[sapply(df, inherits, what = c("POSIXct","Date"))]
      num_cols  <- names(df)[sapply(df, is.numeric)]
      
      if (length(time_cols) == 0 || length(num_cols) == 0)
        return(helpText("Нет временных или числовых колонок"))
      
      tagList(
        selectInput(ns("time_col"), "Столбец-время",  choices = time_cols),
        selectInput(ns("y_col"),    "Y-переменная",   choices = num_cols)
      )
    })
    
    # ---------- временной график с корректными подписями ----------
    observeEvent(input$plot_ts, {
      req(labeled_data(), input$time_col, input$y_col)
      df <- labeled_data()
      
      # удобные подписи
      x_lab <- input$time_col
      y_lab <- input$y_col
      ttl   <- paste(y_lab, "во времени")
      
      output$ts_plot <- renderPlotly({
        ## 1) общая серая линия (убираем из легенды)
        p <- plot_ly(
          data  = df,
          x     = ~get(x_lab),
          y     = ~get(y_lab),
          type  = "scatter",
          mode  = "lines",
          line  = list(color = "rgba(150,150,150,0.6)", width = 1),
          name  = "Линия",
          showlegend = FALSE            # не выводим в легенду
        )
        
        ## 2) цветные точки + легенда по кластерам
        p <- add_trace(
          p,
          x      = ~get(x_lab),
          y      = ~get(y_lab),
          type   = "scatter",
          mode   = "markers",
          marker = list(size = 6),
          color  = ~Cluster,            # легенда = значения Cluster
          colors = "Set1",
          line   = list(width = 0),
          name   = ~paste0("Кластер: ",Cluster),
          customdata = ~Cluster,
          hovertemplate = paste(
            x_lab, ": %{x}<br>",
            y_lab, ": %{y:.3f}<br>",
            "Cluster: %{customdata}<extra></extra>"
          ),
          inherit = FALSE
        )
        
        p |>
          layout(
            title = ttl,
            xaxis = list(title = x_lab),
            yaxis = list(title = y_lab)
          )
      })
    })
    
    
    ## ---------- 6. возврат результата наружу ------------------
    final_data <- reactive({
      if (is.null(labeled_data())) raw_data() else labeled_data()
    })
    
    return(list(
      raw_data   = raw_data,
      final_data = final_data
    ))
  })
}
