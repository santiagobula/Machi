library("shiny")

APP_TITLE <- "Machi portal web"
APP_SLOGAN <- "Somos tus panas"
APP_VERSION <- "BETA v1.0"

appsilon_website <- "https://github.com/santiagobula"
marketplace_website <- "https://github.com/santiagobula"
marketplace_name <- "Pardo Bula Github"

COLORS <- list(
  white = "#FFF",
  black = "#0a1e2b",
  primary = "#A4E99F",
  secondary = "#49CDDD",
  ash = "#D7EEF9",
  ash_light = "#6B7171"
)


machi_logo <- tags$a(
  href = appsilon_website,
  target = "_blank",
  rel = "nofollow noreferrer",
  class = "logo-link",
  img(src = "images/logo-machi.png", class = "logo-img", alt = "Machi Logo")
)

