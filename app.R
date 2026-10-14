library(shiny)
library(ggplot2)
library(readr)
library(rvest)
library(stringr)

data_url <- "https://raw.githubusercontent.com/Disco-Dip/SPI-Birds_repo_update/main/data.csv"

scrape_n_datasets <- function() {
  tryCatch({
    read_html("https://repository.spibirds.org/") |>
      html_element("span.facetTypeDataset") |>
      html_text2() |>
      str_extract("(?<=\\()\\d+(?=\\))") |>
      as.integer()
  }, error = function(e) NA_integer_)
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-size: 20px; }
    .centered { max-width: 900px; margin: 0 auto; text-align: center; }
    .centered h1 { font-size: 40px; }
  "))),
  
  div(class = "centered",
      h1("SPI-Birds Repository metadata and dataset population"),
      plotOutput("plot", width = "100%", height = "500px")
  )
)

server <- function(input, output, session) {
  
  # stored history (cached per session)
  history <- reactive({
    read_csv(
      data_url,
      col_types = cols(
        datasets = col_integer(),
        date = col_date(format = "%Y-%m-%d")
      )
    )
  })
  
  # retrieve current number of datasets
  n <- reactive({n <- scrape_n_datasets()}) 
  
  # live value, scraped once per visit, never saved
  live <- reactive({
    if (is.na(n())) return(NULL)
    data.frame(
      datasets = n(),
      date = as.Date(format(Sys.time(), "%Y-%m-%d", tz = "Europe/Amsterdam"))
    )
  })
  
  output$plot <- renderPlot({
    dat <- history()
    lv <- live()
    if (!is.null(lv)) {
      dat <- rbind(dat[dat$date != lv$date, ], lv)
    }
    
    ggplot() +
      geom_line(data = dat, aes(x = date, y = datasets), col = "#004A83") +
      scale_x_date(date_labels = "%d-%m") +
      geom_hline(yintercept = 180, col = "#91191C") +
      annotate("text",
               x = min(dat$date), y = 180,
               label = "Target: migrate 180 SPI-Birds (metadata) datasets",
               hjust = 0, vjust = -0.5,
               colour = "#91191C", size = 5) +
      annotate("text",
               x = max(dat$date), y = n(),
               label = paste(n(), "datasets migrated"),
               hjust = 1, vjust = -0.5,
               colour = "#004A83", size = 5) +
      theme_classic()
  })
}

shinyApp(ui, server)

