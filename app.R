library(shiny)
library(leaflet)
library(leaflet.extras)
library(rnaturalearth)
library(dplyr)
library(lubridate)
library(shinyjs)
library(bslib)

# Proovi app launchida browseris
options(shiny.launch.browser = TRUE)

# Keela RStudiot kasutamast sisseehitatud 'Run App' valikuid
options(viewer = NULL)

source("R/functions.R")
source("R/data_cleaning.R")

# Loe sisse puhastatud andmed
andmed_sf <- readRDS("data/cleaned_data.rds")

ui <- page_fillable(
  shinyjs::useShinyjs(),
  theme = bs_theme(version = version_default()), 
  
  titlePanel("Liiklusõnnetuste interaktiivne kaart"),
  tags$head(
    # Vaikimisi loetakse styles.css sisse www kaustast, nt projekt/www/styles.css
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Font Awesome ikoon filter nupu jaoks
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.2/css/all.min.css")
  ),
  
  navset_card_tab(
    nav_panel(
      title = "Kaart",
      tags$div(style = "position: relative; width: 100%; height: 100%;",
               leafletOutput("map", width = "100%", height = "100%"),
               div(id = "map-overlay",
                   sliderInput("mapAjavahemik",
                               "Vali aastad:",
                               min = min(year(andmed_sf$Toimumisaeg)),
                               max = max(year(andmed_sf$Toimumisaeg)),
                               value = c(min(year(andmed_sf$Toimumisaeg)), max(year(andmed_sf$Toimumisaeg))),
                               step = 1,
                               sep = ""),
                   checkboxInput("mapHukkunuid", "Näita hukkunutega õnnetusi", FALSE),
                   checkboxInput("mapJoobes", "Näita joobes juhi osalusega õnnetusi", FALSE),
                   checkboxInput("eakasJuht", "Näita eaka (65+) juhi osalusel õnnetusi", FALSE),
                   checkboxInput("ilmaTurvavarustuseta", "Näita turvavarustuseta osalusega õnnetusi", FALSE),
                   checkboxGroupInput("soidukid_group", "Õnnetuses olnud autode arv:",
                                      choices = c("1", "2-3", "4+"), selected= c("1", "2-3", "4+"), inline = TRUE),
                   checkboxGroupInput("isikud_group", "Õnnetuses olnud isikute arv:",
                                      choices = c("1", "2-3", "4+"), selected= c("1", "2-3", "4+"), inline = TRUE),
                   selectInput("maakond", "Vali maakond:",
                               choices = c("Kõik", sort(unique(andmed_sf$Maakond))),
                               selected = "Kõik"),
                   selectInput("ilmastik", "Ilmastik:",
                               choices = c("Kõik", sort(unique(andmed_sf$Ilmastik)))),
                   selectInput("valgustus", "Valgustus:",
                               choices = c("Kõik", sort(unique(andmed_sf$Valgustus))))
               )
      )
    ),
    nav_panel(
      title = "Projekti kirjeldus",
      includeHTML("www/project_description.html")
    ),
  )
)

server <- function(input, output, session) {
  # Rakendus sulgub kui sessioon lõppeb
  # Vajalik kui ilma IDE-ta jooksutada
  session$onSessionEnded(stopApp)
  
  maakonnad_sf <- ne_states(country = "Estonia", returnclass = "sf")

  uuenda_maakonnad("map", session, maakonnad_sf)
  
  filtered_data <- reactive({
    vali_andmed(andmed_sf, 
                input$mapAjavahemik[1], 
                input$mapAjavahemik[2], 
                input$mapHukkunuid, 
                input$mapJoobes,
                isikute_grupid = input$isikud_group,
                soidukite_grupid = input$soidukid_group, 
                ilmastik_valik = input$ilmastik, 
                valgustus_valik = input$valgustus,
                maakond_valik = input$maakond, 
                eakas_juht = input$eakasJuht, 
                ilma_turvavarustuseta = input$ilmaTurvavarustuseta)
  })
  
  output$map <- renderLeaflet({
    joonista_kaart()
  })
  
  observe({
    uuenda_markerid("map", session, filtered_data())
  })
  
  observe({
    uuenda_layerid("map", session)
  })
  
  # Salvesta kaardi keskpunkt ja zoomi tase
  observe({
    if (!is.null(input$map_center) && !is.null(input$map_zoom)) {
      salvesta_keskosa_zoom("map", session, input$map_center, input$map_zoom)
    }
  })
  
  # Uuenda overlay staatust vastavalt toggle_button muutustele
  observeEvent(input$toggle_button, {
    shinyjs::toggle(id = "map-overlay", anim = TRUE)
  })
}

shinyApp(ui = ui, server = server)