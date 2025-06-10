##EAS 548
##Lab 3 R Code
##Name: Sarah Bassiouni, MPH
##Date: September-October 2024

###############################################################################
#################### Preamble / Package Downloading ###########################
###############################################################################

#Installing packages (only once)
install.packages("tidyverse")
install.packages("readr")
install.packages("readxl")
install.packages("stringr")

install.packages("ggplot2")
install.packages("tidycensus")
install.packages("tmap")
install.packages("tmaptools")
install.packages("dplyr")
install.packages("sf")
install.packages("RColorBrewer")
install.packages("classInt")
install.packages("cartography")
install.packages("maptiles")
install.packages("SpatialPosition")

#Package libraries (run each time there's a new R session)
# For plotting
library(ggplot2)
# For downloading Census data
library(tidycensus)
# For creating tmap
library(tmap)
# For reading and processing spatial data related to tmap      
library(tmaptools)
# For data wrangling
library(dplyr)        
# For reading, writing and working with spatial objects
library(sf)      
#library(rgeos) # Package ‘rgeos’ was removed from the CRAN repository.
#library(rgdal) # Package ‘rgdal’ was removed from the CRAN repository.
# library(maptools) # Package ‘maptools’ was removed from the CRAN repository.    
# also loads sp()
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables
library(tidyverse)
library(cartography)
library(maptiles)
library(SpatialPosition) 

#to search for help, use help.search("function")


###############################################################################
###############################################################################
###############################################################################

#census_api_key(place your api key here)

### fron the
v2017 <- load_variables(2017, "acs5", cache = FALSE)
View(v2017)

###Grab data from Michigan at the census tract level of the total population 
### include the vector or shapefile
Michigan <- get_acs(state = "MI", geography = "tract", variables = "B19013_001", geometry = TRUE, cb = FALSE)

### We can examine what is being loaded
head(Michigan)

#format can be quickly visualized using the geom_sf functionality 
Michigan %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  coord_sf(crs = 4326) +
  scale_fill_viridis_c(option = "magma")

###
racevars <- c(White = "P005003",
              Black = "P005004",
              Asian = "P005006",
              Hispanic = "P004003",
              HouseUnits = "H001001",
              Rent = "H004004" )

# race_vars <- c(
#   Hispanic = "P2_002N",
#   White = "P2_005N",
#   Black = "P2_006N",
#   Asian = "P2_008N"
# )

Wayne <- get_decennial(geography = "tract", variables = racevars,
                       state = "MI", county = "Wayne County", geometry = TRUE,
                       summary_var = "P001001", year = 2010)

head(Wayne)

## Here we change our variable to a factor to actaully see the different factors
summary(as.factor(Wayne$variable))

## here we use the 'key' for our variable: Asian, Black Hispanic and White,  
## and their values to be summarized
Wayne %>%
  spread(variable, value)

##rename the variable for the population, summary_value, to Pop100 for readability.
Wayne %>% rename("Pop2010" = "summary_value")  

##replace the old Wayne SF data with the reorganized data.
Wayne <- Wayne %>% 
  spread(variable, value) %>%
  rename("Pop2010" = "summary_value") 
  Wayne

#### Here we will need the NAD27 / Michigan South, which is 6202
Wayne <- st_transform(Wayne, crs = 6498)
  Wayne
  
#constructing a chloropleth

  ## first we will calculate population density (inhab./km2) 
  ## to do this we calculate the area of the tracts using sf::st_area()
  ## we use this this to normalize the population. Since area is 
  ## meters we need to multiply by 1,000,000 (1e6)
  Wayne$PopDens <- 1e6 * Wayne$Pop2010 / st_area(Wayne)
  # plot municipalities (only the backgroung color is plotted)
  plot(st_geometry(Wayne), col = NA, border = NA, bg = "gray")
  # plot population density
  choroLayer(
    x = Wayne, 
    var = "PopDens",
    method = "quantile",
    nclass=5,
    col = carto.pal(pal1 = "sand.pal", n1 = 5),
    border = "white", 
    lwd = 0.5,
    legend.pos = "topright", 
    legend.title.txt = "Population Density\n(people per km2)",
    add = TRUE
  ) 
  # layout
  layoutLayer(title = "Population Distribution in Wayne County", 
              sources = "Sources: US Census, 2010",
              author = paste0("cartography ", packageVersion("cartography")), 
              frame = FALSE, north = FALSE, tabtitle = TRUE, theme= "sand.pal") 
  # north arrow
  north(pos = "topleft")
  
  
  ## Here we are calculating the proportation of the white population
  ## as a product of total population
  Wayne$PropWhite <-  Wayne$White / Wayne$Pop2010 * 100
  ## We will use the county tracts as a base layer
  ## Feel free to alter these options
  plot(st_geometry(Wayne), col = "white", border = "lightgray", bg = "gray")
  ## Now we call the  function using the new variable
  propSymbolsLayer(
    x = Wayne, 
    var = "PropWhite",
    inches = 0.075,
    col = "lightblue",
    border = "blue",
    legend.title.txt = "Population")
  
  # Create a provider from a custom url
  osm_tiles <- create_provider(
    name = "osm_tiles",
    url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    citation = "© OpenStreetMap contributors."
  )
  
  orl.osm <- get_tiles(
    x = Wayne,
    provider = osm_tiles,
    zoom = 11,
    crop = TRUE
  )
  ## Data and map tiles sources:
  ## © OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright.
  # plot osm tiles
  plot_tiles(x = orl.osm)
  
  
  plot_tiles(x = orl.osm)
  # plot municipalities (only borders are plotted)
  plot(st_geometry(Wayne), col = NA, border = "grey", add=TRUE)
  # plot population
  propSymbolsLayer(
    x = Wayne, 
    var = "PropWhite", 
    inches = 0.1, 
    col = "brown4",
    legend.pos = "topright",  
    legend.title.txt = "Total population"
  )
  
  
  ## Let's first calculate the proportion of rental homes 
  Wayne$PropTenure <- Wayne$Rent / Wayne$HouseUnits * 100
  ## Add a base layer of the tracts
  plot(st_geometry(Wayne), col="darkseagreen3", border="darkseagreen4",  
       bg = "lightgray", lwd = 0.5)
  # Plot symbols with choropleth coloration
  propSymbolsChoroLayer(
    x = Wayne, 
    var = "Black", 
    inches = 0.05,
    border = "grey50",
    lwd = 1,
    legend.var.pos = "topright", 
    legend.var.title.txt = "Black Population",
    var2 = "PropTenure",
    method = "equal", 
    nclass = 4, 
    col = carto.pal(pal1 = "sand.pal", n1 = 4),
    legend.var2.values.rnd = 1,
    legend.var2.pos = "right", 
    legend.var2.title.txt = "Proportion Rental"
  ) 
  # layout
  layoutLayer(title="Black Population & Housing Rentals, 2010", 
              author = "D. Van Berkel", 
              sources = "Sources: US Census, 2010", 
              scale = 5, tabtitle = TRUE, frame = FALSE)
  # north arrow
  north(pos = "topleft")
  
  
  plot(st_geometry(Wayne), col="darkseagreen3", border="darkseagreen4",  
       bg = "lightgray", lwd = 0.5)
  # plot isopleth map
  smoothLayer(
    x = Wayne, 
    var = 'Pop2010',
    typefct = "exponential",
    span = 4000,
    beta = 2,
    nclass = 12,
    col = carto.pal(pal1 = 'wine.pal', n1 = 12),
    border = "grey",
    lwd = 0.1, 
    mask = Wayne, 
    legend.values.rnd = -3,
    legend.title.txt = "Population",
    legend.pos = "topright", 
    add=TRUE
  )
  
  # annotation on the map
  text(x = 692582, y = 1611478, cex = 0.8, adj = 0, font = 3,  labels = 
         "Distance function:\n- type = exponential\n- beta = 2\n- span = 4 km")
  # layout
  layoutLayer(title = "Population Distribution Wayne County",
              sources = "Sources: US Census 2010",
              author = paste0("cartography ", packageVersion("cartography")),
              frame = FALSE, north = FALSE, tabtitle = TRUE, theme = "brown.pal")
  # north arrow
  north(pos = "topleft")
  
  
  plot(st_geometry(Wayne), col="darkseagreen3", border="darkseagreen4",  
       bg = "lightgray", lwd = 0.5)
  # plot the density map
  dotDensityLayer(
    x = Wayne,  
    var="Pop2010", 
    pch=20, 
    col = "red4", 
    n = 250)
  layoutLayer(title = "Population Distribution Wayne County, MI, 2010")
  
  plot(st_geometry(Wayne), col="darkseagreen3", border="darkseagreen4",  
       bg = "lightgray", lwd = 0.5)
  # plot the density map
  dotDensityLayer(
    x = Wayne,  
    var="PropTenure", 
    pch=20, 
    col = "red4", 
    n = 10)
  layoutLayer(title = "Population Distribution Wayne County, MI, 2010")
  
  
  ## First we build the regular grid
  mygrid <- getGridLayer(
    x = na.omit(Wayne), 
    cellsize = median(as.numeric(st_area(Wayne))), 
    var = "Pop2010",
    type = "hexagonal"
  )
  ## Now we compute population density in people per km2
  mygrid$PopDens <- 1e6 * mygrid$Pop2010 / mygrid$gridarea
  plot(st_geometry(Wayne), col = NA, border = NA, bg = "#deffff")
  # Plot the population density for the grid
  choroLayer(x = mygrid, var = "PopDens", method = "fisher-jenks", nclass=10, 
             col = carto.pal(pal1 = "turquoise.pal", n1 = 10), border = "grey80", 
             lwd = 0.5, legend.pos = "right", add = TRUE,
             legend.title.txt = "Population Density\n(people per km2)") 
  layoutLayer(title = "Population Distribution in Wayne County, MI", 
              sources = "Sources: US Census, 2010",
              author = paste0("cartography ", packageVersion("cartography")), 
              frame = FALSE, north = FALSE, tabtitle = TRUE,
              theme = "turquoise.pal")
  # north arrow
  north(pos = "topleft")
  
  #la dee da dee da dee daaaa (a change for git) 
  #another change for merge conflict