filterButtonHTML <- actionButton("toggle_button", "", icon("filter"))

vali_andmed <- function(data, 
                        aasta_min, 
                        aasta_max, 
                        hukkunud, 
                        joobes, 
                        isikute_grupid = c("1", "2-3", "4+"), 
                        soidukite_grupid = c("1", "2-3", "4+"), 
                        ilmastik_valik = "Kõik", 
                        valgustus_valik = "Kõik", 
                        maakond_valik = "Kõik", 
                        eakas_juht = FALSE, 
                        ilma_turvavarustuseta = FALSE) {
  filtered_data <- data
  
  # Kontrolli, kas aasta_min ja aasta_max on nullid.
  if (!is.null(aasta_min) & !is.null(aasta_max)) {
    # Näita õnnetusi aastavahemikust
    filtered_data <- filtered_data %>%
      filter(year(Toimumisaeg) >= aasta_min & year(Toimumisaeg) <= aasta_max)
  }
  
  # Kui parameeter hukkunud on TRUE siis näidatakse ainult hukkunutega õnnetusi
  if (hukkunud){
    filtered_data <- filtered_data %>%
      filter(Hukkunuid > 0)
  }
  
  # Kui parameeter joobes on TRUE siis näidatakse ainult joobes juhi osalusega õnnetusi
  if (joobes){
    filtered_data <- filtered_data %>%
      filter(`Joobes mootorsõidukijuhi osalusel` > 0)
  }
  
  # Isikute arv gruppidena
  filtered_data <- filtered_data %>%
    filter(
      (("1" %in% isikute_grupid & Isikuid == 1) |
         ("2-3" %in% isikute_grupid & Isikuid >= 2 & Isikuid <= 3) |
         ("4+" %in% isikute_grupid & Isikuid >= 4))
    )
  
  # Sõidukite arv gruppidena
  filtered_data <- filtered_data %>%
    filter(
      (("1" %in% soidukite_grupid & Sõidukeid == 1) |
         ("2-3" %in% soidukite_grupid & Sõidukeid >= 2 & Sõidukeid <= 3) |
         ("4+" %in% soidukite_grupid & Sõidukeid >= 4))
    )
  
  # Eakas juht
  if (eakas_juht) {
    filtered_data <- filtered_data %>%
      filter(`Eaka (65+) mootorsõidukijuhi osalusel` == 1)
  }
  
  # Ilma turvavarustuseta
  if (ilma_turvavarustuseta) {
    filtered_data <- filtered_data %>%
      filter(`Turvavarustust mitte kasutanud isiku osalusel` == 1)
  }
  
  # Ilmastik
  if (ilmastik_valik != "Kõik") {
    filtered_data <- filtered_data %>%
      filter(Ilmastik == ilmastik_valik)
  }
  
  # Valgustus
  if (valgustus_valik != "Kõik") {
    filtered_data <- filtered_data %>%
      filter(Valgustus == valgustus_valik)
  }
  
  # Maakond
  if (maakond_valik != "Kõik") {
    filtered_data <- filtered_data %>%
      filter(Maakond == maakond_valik)
  }
  
  return (filtered_data)
}

joonista_kaart <- function() {
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
    # Eesti keskpunkt
    setView(lng = 25, lat = 58.6, zoom = 8) %>%
    addControl(html = filterButtonHTML, position = "topleft", className = "leaflet-control") %>%
    addScaleBar(
      position = "bottomright",
      options = scaleBarOptions(metric = TRUE, imperial = FALSE)
    ) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Esri.WorldImagery")
    )
}

uuenda_markerid <- function(map, session, data) {
  leafletProxy(map, session) %>%
    clearGroup("markers") %>%
    addAwesomeMarkers(
      data = data,
      group = "markers",
      clusterOptions = markerClusterOptions(),
      icon = awesomeIcons(icon = 'car', library = 'fa', markerColor =
                            ifelse(data$Hukkunuid > 0, 'red', ifelse(data$Vigastatuid > 1, 'orange', 'blue'))
      ),
      popup = ~paste(
        "Liiklusõnnetus: ", Liiklusõnnetuse.liik..detailne.,
        "<br>Isikuid: ", Isikuid,
        "<br>Sõidukeid: ", Sõidukeid,
        "<br>Vigastatuid: ", Vigastatuid,
        "<br>Hukkunuid: ", Hukkunuid,
        "<br>Ilmastik: ", Ilmastik,
        "<br>Valgustus: ", Valgustus,
        "<br>Maakond: ", Maakond,
        "<br>Toimumisaeg: ", Toimumisaeg
      )
    )
}

uuenda_layerid <- function(map, session) {
  leafletProxy(map, session) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
      options = layersControlOptions(collapsed = TRUE)
    )
}

uuenda_maakonnad <- function(map, session, maakonnad_sf) {
  leafletProxy(map, session) %>%
    clearGroup("maakonnad") %>%
    addPolygons(data = maakonnad_sf,
                fill = FALSE,
                color = "green",
                weight = 2,
                group = "maakonnad")
}

salvesta_keskosa_zoom <- function(map, session, map_center, map_zoom) {
  leafletProxy(map, session) %>%
    setView(lng = map_center$lng, lat = map_center$lat, zoom = map_zoom)
}