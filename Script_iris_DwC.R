#instalar paquetes
install.packages("validate")

#cargar librerias
library(tidyverse)
library(validate)

#cargar base de datos
iris

#estructura data.frame_iris
str(iris)

#identifica y devuelve las filas sin duplicados
unique(iris)

#muestra los resultados en lista de cada variable
lapply(iris, unique)

#agregar columna site a iris

set.seed(123) # Para que la aleatoriedad sea reproducible

iris_con_site <- iris %>% 
  mutate(site = sample(c("site1", "site2", "site3"), size = n(), replace = TRUE))

#agregar columna de coordenadas
iris_con_coordenadas <- iris_con_site %>% 
  mutate(
    coordenadas = case_when(
      site == "site1" ~ "-21.7538, -41.3239",
      site == "site2" ~ "-22.9068, -43.1729",
      site == "site3" ~ "-23.5505, -46.6333"
    )

## Separar la columna 'coordenadas' en 'lat' y 'lon' dentro de 'iris'
iris <- iris_con_coordenadas %>% 
  separate(
    col = coordenadas, 
    into = c("lat", "lon"), 
    sep = ",", 
    convert = TRUE # Convierte automáticamente los valores a tipo numérico
  )

#grafica de distribucion de valores númericos
iris %>% 
  select(Species, Sepal.Length:Petal.Width) %>% 
  pivot_longer(cols = -Species, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(x = valores, fill = Species)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (cm)") +
  scale_fill_discrete(
    name = expression(bold("Species:")),
    labels = c(
      expression(italic("Iris setosa")), 
      expression(italic("Iris versicolor")), 
      expression(italic("Iris virginica"))
    )
  )

#poner limites a variables

library(validate)

rules <- validator(
  in_range(lat, min = -90, max = 90),
  in_range(lon , min = -180, max = 180), # Corregido de lat a lon
  is.character(site),
  is.numeric(date),
  all_complete(iris)
)

out   <- confront(iris, rules)
summary(out)
plot(out)

# 1. Obtener los nombres de las especies y formatear para ITIS ("Iris setosa", etc.)
especies_df <- iris %>% 
  distinct(Species) %>% 
  mutate(scientificName_query = paste("Iris", as.character(Species)))

# 2. Consultar ITIS
itis_res <- filter_name(especies_df$scientificName_query, provider = "itis")

# 3. Unir para conservar la columna 'Species' original de iris
species <- especies_df %>% 
  left_join(itis_res, by = c("scientificName_query" = "scientificName"))

# 1. Asegurar que las columnas date y amostra existan
iris_preparado <- iris %>% 
  mutate(
    date = Sys.Date(),
    amostra = row_number()
  )

# 2. Pipeline Darwin Core
iris_1 <- iris_preparado %>% 
  mutate(
    eventID = paste(site, date, sep = "_"),
    occurrenceID = paste(site, date, amostra, sep = "_")
  ) %>% 
  left_join(
    species %>% select(Species, any_of(c("acceptedNameUsageID", "scientificName"))),
    by = "Species"
  ) %>% 
  rename(
    decimalLongitude = lon,
    decimalLatitude = lat,
    eventDate = date
  ) %>% 
  mutate(
    geodeticDatum = "WGS84",
    verbatimCoordinateSystem = "decimal degrees",
    georeferenceProtocol = "Random coordinates obtained from Google Earth",
    locality = "Gaspe Peninsula",
    recordedBy = "Juaninho",
    taxonRank = "Species",
    organismQuantityType = "individuals",
    basisOfRecord = "Human observation"
  )

# Ver el resultado final
head(iris_1)

#cambiar nombre de species o una columna por otro
library(dplyr)

iris_1 <- iris_1 %>% 
  rename(scientificName = Species)

## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, acceptedNameUsageID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))

#control de calidad
# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(occurrences$eventID, eMOF$eventID)
eMOF %>%
  filter(is.na(eventID))
occurrences %>%
  filter(is.na(eventID))

#exportar matrices
rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}