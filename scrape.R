library(rvest)
library(stringr)

now_ams <- as.POSIXlt(Sys.time(), tz = "Europe/Amsterdam")

# On scheduled runs, only continue if it's actually 23:xx in Amsterdam.
# Manual runs (workflow_dispatch) always go through.
if (Sys.getenv("GITHUB_EVENT_NAME") == "schedule" && now_ams$hour != 23) {
  message("Not 23:xx in Amsterdam, skipping.")
  quit(save = "no", status = 0)
}

page <- read_html("https://repository.spibirds.org/")

n_datasets <- page |>
  html_element("span.facetTypeDataset") |>
  html_text2() |>
  str_extract("(?<=\\()\\d+(?=\\))") |>
  as.integer()

stopifnot(length(n_datasets) == 1, !is.na(n_datasets))

new_row <- data.frame(
  datasets = n_datasets,
  date = as.Date(format(now_ams, "%Y-%m-%d"))
)

if (file.exists("data.csv")) {
  dat <- read.csv("data.csv")
  dat$date <- as.Date(dat$date)
  dat <- dat[dat$date != new_row$date, ]   # replace today's row if it exists
  dat <- rbind(dat, new_row)
} else {
  dat <- new_row
}

write.csv(dat[order(dat$date), ], "data.csv", row.names = FALSE)