# ──────────────────────────────────────────────────────────────
# reportModule.R   v2 – подписи к блокам
# ──────────────────────────────────────────────────────────────
library(shiny)
library(rmarkdown)
library(knitr)

# ---------- UI ----------
reportUI <- function(id){
  ns <- NS(id)
  fluidPage(
    titlePanel("Конструктор отчётов"),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("block_selector")),   # выбор + подписи
        radioButtons(ns("fmt"), "Формат",
                     c("PDF"="pdf","HTML"="html")),
        textInput(ns("rep_title"), "Заголовок отчёта",
                  "Отчёт анализа ИТП"),
        downloadButton(ns("gen_report"), "Сформировать")
      ),
      mainPanel(
        h4("Предпросмотр"),
        uiOutput(ns("preview"))
      )
    )
  )
}

# ---------- Server ----------
reportServer <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ── 1. выбор блоков + поля для подписи ─────────────────────
    output$block_selector <- renderUI({
      blocks <- get_report_blocks()
      if (length(blocks) == 0)
        return(helpText("Пока нет ни одного блока"))
      
      tagList(
        checkboxGroupInput(
          ns("blocks"),
          "Включить в отчёт:",
          choices  = setNames(names(blocks),
                              vapply(blocks, `[[`, "", "title")),
          selected = names(blocks)
        ),
        # динамические textInput'ы для подписей
        lapply(names(blocks), function(id){
          textInput(
            ns(paste0("cap_", id)),
            label = paste("Подпись к —", blocks[[id]]$title),
            value = ""
          )
        })
      )
    })
    
    # ── 2. предпросмотр ────────────────────────────────────────
    output$preview <- renderUI({
      req(input$blocks)
      blocks <- get_report_blocks()[input$blocks]
      tagList(
        lapply(names(blocks), function(id){
          bl <- blocks[[id]]
          caption <- input[[paste0("cap_", id)]]
          tagList(
            tags$h4(bl$title),
            bl$preview_ui,
            if (nzchar(caption)) tags$em(caption)  # предварительный вывод подписи
          )
        })
      )
    })
    
    # ── 3. генерация отчёта ------------------------------------
    output$gen_report <- downloadHandler(
      filename = function(){
        paste0("report_", Sys.Date(), ".", input$fmt)
      },
      content = function(file_path){
        req(input$blocks)
        blocks <- get_report_blocks()[input$blocks]
        
        # 1) создаём временный Rmd
        tmp_rmd <- tempfile(fileext = ".Rmd")
        hdr <- paste0(
          "---\n",
          "title: \"", input$rep_title, "\"\n",
          "output: ", if (input$fmt == "pdf") "pdf_document" else "html_document", "\n",
          "---\n\n"
        )
        writeLines(hdr, tmp_rmd)
        
        # 2) добавляем блоки
        for (id in names(blocks)){
          bl <- blocks[[id]]
          cat("## ", bl$title, "\n", file = tmp_rmd, append = TRUE)
          
          # код-чанк (PNG / plot и т.д.)
          chunk <- paste0(
            "```{r ", id, ", echo=FALSE, fig.width=6, fig.height=4}\n",
            "reportManager$report_blocks[['", id, "']]$content_fun()\n",
            "```\n\n"
          )
          cat(chunk, file = tmp_rmd, append = TRUE)
          
          # подпись, если непустая
          cap <- input[[paste0("cap_", id)]]
          if (nzchar(cap)) {
            cat("*", cap, "*\n\n", file = tmp_rmd, append = TRUE, sep = "")
          }
        }
        
        # 3) knit → PDF/HTML
        render(tmp_rmd, output_file = file_path,
               quiet = TRUE, envir = globalenv())
      }
    )
  })
}
