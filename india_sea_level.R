library(elevatr)
library(raster)
library(ggplot2)
library(leafem)
library(leaflet)

rasterOptions(maxmemory = 1.5e+10, 
              chunksize = 1e+10, 
              memfrac = 0.8)

loc_df <- data.frame(x = c(66,90),
                     y = c(5,35))

test <- get_elev_raster(loc_df,
                        z = 8,
                        prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                        neg_to_na = TRUE, 
                        src = "aws")
land <- test
land[land <= 0] <- NA

#pop <- raster("population/ind_ppp_2020_constrained.tiff") #from https://www.worldpop.org/geodata/summary?id=49804
if(!dir.exists("population")){
  dir.create("population")
  if(!file.exists("population/ind_ppp_2020_1km_Aggregated.tiff")){
    download.file("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/IND/ind_ppp_2020_1km_Aggregated.tif", "population/ind_ppp_2020_1km_Aggregated.tiff")
  }
}

pop <- raster("population/ind_ppp_2020_1km_Aggregated.tiff")# https://www.worldpop.org/geodata/summary?id=31771
land_res <- resample(land, pop, method = "bilinear")
land_res <- mask(land_res, pop)

land_1m <- land_res <= 1
land_2m <- land_res <= 2
land_3m <- land_res <= 3
land_4m <- land_res <= 4
land_5m <- land_res <= 5
land_6m <- land_res <= 6


pop_1m <- pop*land_1m
pop_2m <- pop*land_2m
pop_3m <- pop*land_3m
pop_4m <- pop*land_4m
pop_5m <- pop*land_5m
pop_6m <- pop*land_6m

displaced_pop_1m <- cellStats(pop_1m, sum)
displaced_pop_2m <- cellStats(pop_2m, sum)
displaced_pop_3m <- cellStats(pop_3m, sum)
displaced_pop_4m <- cellStats(pop_4m, sum)
displaced_pop_5m <- cellStats(pop_5m, sum)
displaced_pop_6m <- cellStats(pop_6m, sum)

dp <- data.frame(sea_level_rise = 1:6, displaced_population = c(displaced_pop_1m, displaced_pop_2m, displaced_pop_3m,
        displaced_pop_4m, displaced_pop_5m, displaced_pop_6m))


ggplot(dp, aes(x = sea_level_rise, y = displaced_population/1000000))+
  geom_point()+
 # geom_smooth()+
  labs(x = "Sea Level Rise (m)", y = "Displaced Population (millions)")+
  theme_bw()

land_6m[land_6m < 1] <- NA

writeRaster(land_6m, "sea_level_rise/india_six_metres_1km.tif", overwrite = TRUE)

#test <- crop(land_6m, extent(c(87, 88, 21,23)))
#writeRaster(test, "sea_level_rise/india_six_metres.tif", overwrite = TRUE)

tst <- read_stars("sea_level_rise/india_six_metres_1km.tif", proxy = TRUE)


leaflet() %>%
  setView(lng = 88.5, lat = 22, zoom = 08) %>%
  #addTiles() %>% 
   addProviderTiles("CartoDB.DarkMatter") %>%
  addGeoRaster(
    x = tst,
    autozoom = FALSE,
    resolution = 100,
    opacity = 1, colorOptions = colorOptions(
      palette = grey.colors(256), na.color = "transparent" 
    ))


