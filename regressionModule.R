# =============================================================
# regressionModule.R     (самодостаточный Shiny-модуль)
# =============================================================
#
# ▸ UI  : regressionUI(id)
# ▸ srv : regressionServer(id, data_reactive)
#
# data_reactive — список, который возвращают ваши предыдущие
#                 модули (обязательно содержит $final_data()).
#
# Возвращает:
#   list(
#     raw_data   = raw_data,          # исходный df
#     final_data = final_data,        # df + .pred (если модель обучена)
#     model      = model_r            # reactive(model)  (NULL до обучения)
#   )
#
# Использует tidy-мир: recipes / parsnip / workflows / yardstick / tune.
# =============================================================

# ---------- пакеты ----------
library(shiny)
library(DT)
library(tidymodels)
library(plotly)
library(janitor)
#library(xgboost)
tidymodels::tidymodels_prefer()

# ---------- UI ----------
regressionUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Модуль регрессии"),
    sidebarLayout(
      sidebarPanel(
        ## 1. выбор целевой и фичей -----------------------------
        h4("Данные"),
        uiOutput(ns("target_ui")),
        uiOutput(ns("feature_ui")),
        sliderInput(ns("split_ratio"), "Train доля", 0.7, min = 0.5, max = 0.9, step = 0.05),
        actionButton(ns("do_split"), "Разделить"),
        
        ## 2. алгоритм и гиперы ---------------------------------
        hr(), h4("Алгоритм"),
        selectInput(ns("algo"), "Метод",
                    c("Linear", "Ridge", "Lasso",
                      "ElasticNet", "RandomForest",
                      "XGBoost")),
        conditionalPanel(
          sprintf("input['%s'] == 'RandomForest'", ns("algo")),
          numericInput(ns("rf_trees"), "n_trees", 500, 100, 2000, 50)
        ),
        conditionalPanel(
          sprintf("input['%s'] == 'XGBoost'", ns("algo")),
          numericInput(ns("xgb_trees"), "trees",      500, 100, 2000, 50),
          numericInput(ns("xgb_depth"), "max_depth",  6,   2,   15,   1),
          numericInput(ns("xgb_eta"),   "learn_rate", 0.1, 0.01, 0.5, 0.01)
        ),
        
        ## 3. препроцессинг -------------------------------------
        hr(), h4("Препроцессинг"),
        checkboxInput(ns("scale"), "Z-score масштабирование", TRUE),
        numericInput(ns("poly_deg"), "Полиномиальная степень (0 = нет)", 0, 0, 3, 1),
        
        ## 4. обучение и сохранение -----------------------------
        hr(),
        actionButton(ns("train"),  "Обучить",  class = "btn-primary"),
        downloadButton(ns("save_model"), "Сохранить модель")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Train/Test split", DTOutput(ns("split_tbl"))),
          tabPanel("Метрики", tableOutput(ns("metrics_tbl")),
                   plotOutput(ns("resid_plot"))),
          tabPanel("Важность", plotOutput(ns("vip_plot"))),
          tabPanel("Предсказания", DTOutput(ns("pred_tbl")),
                   downloadButton(ns("dl_pred"), "Скачать .pred")),
          tabPanel("Прогноз на файле",
                   fileInput(ns("new_file"), "CSV с новыми наблюдениями"),
                   actionButton(ns("predict_new"), "Предсказать"),
                   DTOutput(ns("new_pred_tbl")),
                   downloadButton(ns("dl_new_pred"), "Скачать"))
        )
      )
    )
  )
}

# ---------- SERVER ----------
regressionServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## -----------------------------------------------------------------
    ## 0  исходный дата-фрейм
    ## -----------------------------------------------------------------
    raw_data <- reactive({ data_reactive$final_data() })
    
    ## UI выбора переменных -------------------------------------------
    output$target_ui <- renderUI({
      num <- names(raw_data())[sapply(raw_data(), is.numeric)]
      selectInput(ns("target"), "Целевая переменная", num)
    })
    output$feature_ui <- renderUI({
      num <- names(raw_data())[sapply(raw_data(), is.numeric)]
      selectInput(ns("features"), "Признаки", num, multiple = TRUE,
                  selected = setdiff(num, input$target))
    })
    
    ## -----------------------------------------------------------------
    ## 1  train/test split
    ## -----------------------------------------------------------------
    split_rs <- eventReactive(input$do_split, {
      req(input$target, input$features)
      initial_split(raw_data(), prop = input$split_ratio, strata = NULL)
    })
    
    output$split_tbl <- renderDT({
      req(split_rs())
      tibble(Set = c("Train","Test"),
             N   = c(nrow(training(split_rs())),
                     nrow(testing(split_rs())))) |>
        datatable(options = list(dom='t'))
    })
    
    ## -----------------------------------------------------------------
    ## 2  recipe
    ## -----------------------------------------------------------------
    rec <- reactive({
      req(split_rs())                       # есть Train/Test
      req(input$target)                     # выбрана цель
      validate(
        need(length(input$features) > 0,
             "Выберите хотя бы один предиктор")
      )
      
      ## базовый рецепт -------------------------------------------------
      r <- recipe(
        as.formula(
          paste(input$target, "~", paste(input$features, collapse = "+"))
        ),
        data = training(split_rs())
      )
      
      ## масштабирование, если есть числовые предикторы ----------------
      num_preds <- training(split_rs()) |>
        select(all_of(input$features)) |>
        select(where(is.numeric)) |>
        names()
      
      if (isTRUE(input$scale) && length(num_preds) > 0)
        r <- r |> step_normalize(all_of(num_preds))
      
      ## полиномиальные признаки ---------------------------------------
      if (input$poly_deg > 0 && length(num_preds) > 0)
        r <- r |> step_poly(all_of(num_preds), degree = input$poly_deg)
      
      r
    })
    
    
    ## -----------------------------------------------------------------
    ## 3  модель (parsnip) + workflow
    ## -----------------------------------------------------------------
    model_spec <- reactive({
      switch(input$algo,
             "Linear"       = linear_reg()          |> set_engine("lm"),
             "Ridge"        = linear_reg(penalty = 0.1, mixture = 0) |> set_engine("glmnet"),
             "Lasso"        = linear_reg(penalty = 0.1, mixture = 1) |> set_engine("glmnet"),
             "ElasticNet"   = linear_reg(penalty = 0.1, mixture = 0.5) |> set_engine("glmnet"),
             "RandomForest" = rand_forest(mtry = floor(length(input$features)/3),
                                          trees = input$rf_trees, min_n = 5) |> set_engine("ranger"),
             "XGBoost"      = boost_tree(trees = input$xgb_trees,
                                         tree_depth = input$xgb_depth,
                                         learn_rate = input$xgb_eta) |> set_engine("xgboost")
      ) |> set_mode("regression")
    })
    
    wflow <- reactive({
      workflow() |> add_recipe(rec()) |> add_model(model_spec())
    })
    
    ## -----------------------------------------------------------------
    ## 4  обучение
    ## -----------------------------------------------------------------
    fitted_wf <- eventReactive(input$train, {
      req(wflow())
      fit(wflow(), data = training(split_rs()))
    })
    
    ## модель наружу
    model_r <- reactive({ fitted_wf() })
    
    ## -----------------------------------------------------------------
    ## 5  метрики и графики
    ## -----------------------------------------------------------------
    metrics_df <- reactive({
      req(fitted_wf())
      preds <- predict(fitted_wf(), testing(split_rs())) |>
        bind_cols(testing(split_rs()) %>% select(!!input$target))
      yardstick::metrics(preds, truth = !!input$target, estimate = .pred)
    })
    
    output$metrics_tbl <- renderTable({
      req(metrics_df())
      metrics_df()
    })
    
    output$resid_plot <- renderPlot({
      req(fitted_wf())
      preds <- predict(fitted_wf(), testing(split_rs())) |>
        bind_cols(testing(split_rs()))
      ggplot(preds, aes(.pred, .pred - !!sym(input$target))) +
        geom_hline(yintercept = 0, colour = "grey60") +
        geom_point(alpha = .6) +
        theme_minimal() +
        labs(x = "Предсказание", y = "Resid")
    })
    
    ## Variable importance (rudimentary) ------------------------------
    output$vip_plot <- renderPlot({
      req(fitted_wf())
      mod <- extract_fit_parsnip(fitted_wf())$fit
      if (inherits(mod, "lm")){
        imp <- broom::tidy(mod) |>
          filter(term != "(Intercept)") |>
          mutate(imp = abs(estimate)) |>
          arrange(desc(imp))
        ggplot(imp, aes(reorder(term, imp), imp)) +
          geom_col(fill = "steelblue") +
          coord_flip() + theme_minimal() +
          labs(x = "", y = "abs(coef)")
      } else if (inherits(mod, "xgb.Booster")){
        vip::vip(mod, num_features = 15)
      } else if (inherits(mod, "ranger")){
        vip::vip(mod)
      }
    })
    
    ## -----------------------------------------------------------------
    ## 6  предсказания на test и скачивание
    ## -----------------------------------------------------------------
    pred_tbl <- reactive({
      req(fitted_wf())
      predict(fitted_wf(), raw_data()) |>
        bind_cols(raw_data())
    })
    
    output$pred_tbl <- renderDT({
      req(pred_tbl())
      datatable(head(pred_tbl(), 500), options = list(scrollX = TRUE))
    })
    
    output$dl_pred <- downloadHandler(
      filename = function() "predictions.csv",
      content  = function(file) write.csv(pred_tbl(), file, row.names = FALSE)
    )
    
    ## -----------------------------------------------------------------
    ## 7  прогноз на новом файле
    ## -----------------------------------------------------------------
    new_pred <- eventReactive(input$predict_new, {
      req(input$new_file, fitted_wf())
      new_df <- read.csv(input$new_file$datapath, check.names = FALSE)
      predict(fitted_wf(), new_df) |> bind_cols(new_df)
    })
    
    output$new_pred_tbl <- renderDT({
      req(new_pred())
      datatable(head(new_pred(), 500), options = list(scrollX = TRUE))
    })
    
    output$dl_new_pred <- downloadHandler(
      filename = function() "new_pred.csv",
      content  = function(file) write.csv(new_pred(), file, row.names = FALSE)
    )
    
    ## -----------------------------------------------------------------
    ## 8  сохранение модели
    ## -----------------------------------------------------------------
    output$save_model <- downloadHandler(
      filename = function() paste0("model_", Sys.Date(), ".rds"),
      content  = function(file) {
        req(fitted_wf()); saveRDS(fitted_wf(), file)
      }
    )
    
    ## -----------------------------------------------------------------
    ## 9  возвращаем наружу
    ## -----------------------------------------------------------------
    final_data <- reactive({
      if (is.null(pred_tbl())) raw_data() else pred_tbl()
    })
    
    return(list(
      raw_data   = raw_data,
      final_data = final_data,
      model      = model_r
    ))
  })
}
