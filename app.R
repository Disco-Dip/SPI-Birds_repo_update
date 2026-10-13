library(shiny)
library(ggplot2)
library(readr)
library(rvest)
library(stringr)

data_url <- "https://raw.githubusercontent.com/YOUR_USER/YOUR_REPO/main/data.csv"

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
  "SPI-Birds Repository population",
  plotOutput("plot", width = "100%")
)

server <- function(input, output, session) {
  
  # stored history (cached per session)
  history <- reactive({
    read_csv(data_url, show_col_types = FALSE) |>
      transform(type = "stored")
  })
  
  # live value, scraped once per visit, never saved
  live <- reactive({
    n <- scrape_n_datasets()
    if (is.na(n)) return(NULL)
    data.frame(
      datasets = n,
      date = as.Date(format(Sys.time(), "%Y-%m-%d", tz = "Europe/Amsterdam")),
      type = "live"
    )
  })
  
  output$plot <- renderPlot({
    dat <- history()
    lv <- live()
    if (!is.null(lv)) {
      dat <- rbind(dat[dat$date != lv$date, ], lv)
    }
    
    ggplot(dat, aes(x = date, y = datasets)) +
      geom_line() +
      geom_point(aes(colour = type), size = 2) +
      scale_colour_manual(values = c(stored = "black", live = "steelblue"),
                          name = NULL) +
      geom_hline(yintercept = 186, col = "red") +
      theme_classic()
  })
}

shinyApp(ui, server)