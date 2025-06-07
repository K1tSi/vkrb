# classificationModule.R

# ─── ПАКЕТЫ ──────────────────────────────────────────────────────────────────────
library(shiny)
library(DT)
library(tidymodels)
library(dplyr)
library(rlang)
library(ggplot2)
library(rpart.plot)
library(themis)       # для балансировки апсемплингом
library(yardstick)    # метрики и матрица ошибок

tidymodels_prefer()

# ─── UI ─────────────────────────────────────────────────────────────────────────
classificationUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Классификация кластерных меток — Дерево решений"),
    sidebarLayout(
      sidebarPanel(
        h4("1. Переменная кластеризации"),
        uiOutput(ns("cluster_var_ui")),
        hr(),
        h4("2. Признаки для обучения"),
        uiOutput(ns("feature_ui")),
        hr(),
        h4("3. Балансировка классов"),
        radioButtons(ns("bal_method"), "Метод балансировки",
                     choices = c("Нет"="none",
                                 "Апсемплинг"="upsample",
                                 "Веса"="weights"),
                     selected = "none"),
        hr(),
        h4("4. Настройки дерева решений"),
        numericInput(ns("tree_depth"), "Макс. глубина дерева", value = 5, min = 1),
        numericInput(ns("min_n"), "Мин. число в узле",    value = 20, min = 1),
        hr(),
        checkboxInput(ns("scale"), "Z-score масштабирование", TRUE),
        sliderInput(ns("split_ratio"), "Доля Train", 0.7, min = 0.5, max = 0.9, step = 0.05),
        actionButton(ns("train"), "Обучить", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Split", DTOutput(ns("split_tbl"))),
          tabPanel("Метрики",
                   verbatimTextOutput(ns("metrics_txt")),
                   plotOutput(ns("roc_plot")),
                   DTOutput(ns("conf_mat_plot"))  # заменить на DTOutput
          ),
          tabPanel("Дерево", plotOutput(ns("tree_plot"))),
          tabPanel("Предсказания",
                   DTOutput(ns("pred_tbl")),
                   downloadButton(ns("dl_pred"), "Скачать"))
        )
      )
    )
  )
}

# ─── SERVER ─────────────────────────────────────────────────────────────────────
classificationServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    raw_data <- reactive({ data_reactive$final_data() })
    
    # UI: выбор переменной кластеризации
    output$cluster_var_ui <- renderUI({
      df <- raw_data(); req(df)
      selectInput(session$ns("cluster_var"), "Cluster var", choices = names(df))
    })
    # UI: выбор признаков
    output$feature_ui <- renderUI({
      req(input$cluster_var)
      numeric_feats <- names(raw_data())[sapply(raw_data(), is.numeric)]
      feats <- setdiff(numeric_feats, input$cluster_var)
      selectInput(session$ns("features"), "Features", choices = feats,
                  multiple = TRUE, selected = feats)
    })
    
    # Подготовка данных
    class_data <- reactive({
      req(input$cluster_var, input$features, input$bal_method)
      df <- raw_data() %>%
        select(all_of(input$cluster_var), all_of(input$features)) %>%
        drop_na() %>%
        rename(.class = !!sym(input$cluster_var)) %>%
        mutate(.class = factor(.class))
      if(input$bal_method == "weights"){
        freq <- df %>% count(.class)
        maxn <- max(freq$n)
        df <- df %>% left_join(freq, by = ".class") %>%
          mutate(weights = maxn / n) %>% select(-n)
      }
      df
    })
    
    # Split Train/Test
    split_rs <- eventReactive(input$train, {
      initial_split(class_data(), prop = input$split_ratio, strata = .class)
    })
    output$split_tbl <- renderDT({
      req(split_rs())
      tibble(Set=c("Train","Test"),
             N  = c(nrow(training(split_rs())), nrow(testing(split_rs())))) %>%
        datatable(options=list(dom='t'))
    })
    
    # Рецепт
    rec <- reactive({
      r <- recipe(.class ~ ., data = training(split_rs()))
      if(input$bal_method == "upsample")   r <- r %>% step_upsample(.class)
      if(input$scale)                       r <- r %>% step_normalize(all_numeric_predictors())
      r
    })
    
    # Спецификация дерева
    model_spec <- reactive({
      spec <- decision_tree(tree_depth=input$tree_depth, min_n=input$min_n)
      if(input$bal_method == "weights"){
        spec <- spec %>% set_engine("rpart", weights=weights)
      } else {
        spec <- spec %>% set_engine("rpart")
      }
      spec %>% set_mode("classification")
    })
    
    # Workflow + обучение
    wflow  <- reactive({ workflow() %>% add_recipe(rec()) %>% add_model(model_spec()) })
    fitted <- eventReactive(input$train, { fit(wflow(), data = training(split_rs())) })
    
    # Метрики
    output$metrics_txt <- renderPrint({
      req(fitted())
      test  <- testing(split_rs())
      truth <- test$.class
      p_cl  <- predict(fitted(), test)
      est   <- p_cl$.pred_class
      
      acc <- accuracy_vec(truth, est)
      rec <- recall_vec(truth, est)
      kap <- kap_vec(truth, est)
      
      cat(sprintf("Accuracy: %.3f\nRecall: %.3f\nKappa: %.3f\n", acc, rec, kap))
      if(nlevels(truth) == 2) {
        p_pr <- predict(fitted(), test, type = "prob")
        pos  <- levels(truth)[2]
        prob <- p_pr[[paste0(".pred_", pos)]]
        auc  <- roc_auc_vec(truth, prob)
        cat(sprintf("AUC: %.3f\n", auc))
      }
    })
    
    # ROC-кривая
    output$roc_plot <- renderPlot({
      req(fitted())
      roc_data <- predict(fitted(), testing(split_rs()), type = "prob") %>%
        bind_cols(testing(split_rs()) %>% select(.class)) %>%
        roc_curve(truth = .class, .pred_critical)
      ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
        geom_line() + geom_abline(lty = 2) + theme_minimal() +
        labs(x = "1 - FP Rate", y = "TP Rate")
    })
    
    # Матрица ошибок
    output$conf_mat_plot <- renderDT({
      req(fitted())
      test <- testing(split_rs())
      preds <- predict(fitted(), test)
      df_cm <- bind_cols(test, preds)
      cm <- conf_mat(df_cm, truth = .class, estimate = .pred_class)
      cm_tbl <- as_tibble(cm$table)
      datatable(cm_tbl, rownames = FALSE, options = list(dom = 't', pageLength = 10))
    })
    
    # Визуализация дерева
    output$tree_plot <- renderPlot({
      req(fitted())
      tree_fit <- extract_fit_parsnip(fitted())$fit
      rpart.plot(tree_fit, type = 4, extra = 104, fallen.leaves = TRUE, roundint = FALSE)
    })
    
    # Предсказания
    pred_tbl <- reactive({
      req(fitted())
      df    <- class_data()
      preds <- bind_cols(df,
                         predict(fitted(), df),
                         predict(fitted(), df, type = "prob"))
      preds %>% select(-any_of("weights"))
    })
    output$pred_tbl <- renderDT({ req(pred_tbl()); datatable(head(pred_tbl(), 500), options = list(scrollX = TRUE)) })
    output$dl_pred <- downloadHandler(
      filename = function() "predictions.csv",
      content = function(file) write.csv(pred_tbl(), file, row.names = FALSE)
    )
    
    # Возвращаем
    return(list(
      raw_data   = raw_data,
      final_data = pred_tbl,
      model      = reactive(fitted())
    ))
  })
}
