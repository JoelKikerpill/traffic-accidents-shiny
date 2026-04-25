library(readr)
library(dplyr)
library(sf)
library(stringr)

# Lokaalne sisselugemine
# Peidab warningud andmete sisse lugemisel
# Juhtumi number puudu ridadel 8654 ja 20902
andmed <- suppressWarnings(read.csv("data/lo_2011_2026.csv", sep = ";"))

# Andmed otse veebist
# https://andmed.eesti.ee/datasets/inimkannatanutega-liiklusonnetuste-andmed
# andmed <- read.delim("https://pilv.transpordiamet.ee/s/Iiee4OAYFq4lT1v/download?path=%2F&files=lo_2011_2026.csv", sep = ";")

# Juhtumi nr tulba jaoks
options(scipen = 999)

# Vali sobivad tulbad
andmed_subset <- andmed %>%
  select(-c(9:16, 20:23, 25:33, 35:48))

# Eemalda NA väärtused koordinaatide hulgast
andmed_subset <- andmed_subset %>%
  filter(!is.na(X.koordinaat),
         !is.na(Y.koordinaat))

# Eemalda koordinaadid, mis on meres:
andmed_subset <- andmed_subset %>%
  filter(!(Juhtumi.nr %in% c(
    "2502120071378",
    "3100220604407",
    "2780150005556",
    "3120160068577",
    "3100180414557",
    "3100220671959",
    "2780220000564",
    "3100200274793",
    "3100220670499",
    "3100220461765",
    "3100140060352",
    "2302120249909",
    "2502120103071",
    "2502120024189",
    "2502120036809",
    "2602120118692",
    "2402120037516",
    "2302120050318",
    "2602120080813"
    )))

# Koordinaatide teisendamine leafleti jaoks
# EPSG:3301 -> EPSG:4326
andmed_sf <- st_as_sf(andmed_subset, coords = c("Y.koordinaat", "X.koordinaat"), crs = 3301)
andmed_sf <- st_transform(andmed_sf, crs = 4326)

# Maakondade extractimine
andmed_sf$Maakond <- str_extract(andmed_sf$Aadress, "^[^,]*maakond")

# Uus R objekt Shiny jaoks
saveRDS(andmed_sf, "data/cleaned_data.rds")