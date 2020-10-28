loadd(bounding_box)
loadd(berlin_polygons)
loadd(full_data_set_clean)
loadd(baumscheiben_in_lists)


tifpath <- "./analysis/data/raw_data/WUDAPT_LCZ.geotiff"

rgdal::GDALinfo(tifpath)


lcz_europe <- raster::raster(tifpath)

raster::extent(lcz_europe)
lcz_proj <- raster::crs(lcz_europe)

berlin_polygons <- sf::st_transform(berlin_polygons,
                                    crs = lcz_proj)
berlin_polygons %>% raster::extent()

# gdaluti::gdalbuildvrt(gdalfile = "yourraster.tif",
#              output.vrt = "tmp.vrt",
#              te = st_bbox(shp_layerbuffer1))
#
# tempraster <- raster("tmp.vrt")



crop_raster <- raster::crop(lcz_europe,
                            sf::st_union(berlin_polygons) %>% sf::st_centroid() %>%
                                sf::st_buffer(35000)  %>% sf::st_as_sf())


lcz_stars <- stars::st_as_stars(crop_raster)

colourCount = length(unique(raster::values(crop_raster)))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

plot(lcz_stars, col = viridisLite::cividis(14))
plot(lcz_stars, col =getPalette(colourCount))

map_grid <- sf::st_graticule(berlin_polygons)
ggplot() +
    stars::geom_stars(data = lcz_stars,
                      aes(x = x,
                          y = y,
                          fill = as.factor(WUDAPT_LCZ))) +
    geom_sf(data = berlin_polygons,
            color = "black",
            fill = NA,
            size = 0.5) +
    geom_sf(data = map_grid, col = "gray20", alpha = .4) +
    theme(panel.background = element_rect(fill = NA))



# lcz_values_by_district <- raster::extract(crop_raster,
#                                           sf::st_make_valid(berlin_polygons)) %>%
# lapply(., function(x)prop.table(table(x))) %>%
#     setNames(berlin_polygons$NAMGEM) %>% dplyr::bind_rows(.id = "bezirk")


lcz_values_by_district <- raster::extract(crop_raster,
                                          berlin_polygons) %>%
    lapply(., function(x)prop.table(table(x))) %>%
    setNames(berlin_polygons$NAMGEM) %>% dplyr::bind_rows(.id = "bezirk")

lcz_values_by_district %>%
    tidyr::pivot_longer(cols = tidyselect::matches("[0-9]"),
                        names_to = "LCZ",
                        values_to = "proportion_by_district") %>%
    dplyr::mutate(LCZ = as.factor(as.numeric(LCZ))) %>%
    ggplot() +
        geom_col(aes(x = LCZ,
                     y = as.numeric(proportion_by_district),
                     fill = bezirk),
                 show.legend = FALSE)  +
    facet_wrap(~bezirk)


lcz_values_by_district %>%
    tidyr::pivot_longer(cols = tidyselect::matches("[0-9]"),
                        names_to = "LCZ",
                        values_to = "proportion_by_district") %>%
    dplyr::mutate(LCZ = as.factor(as.numeric(LCZ))) %>%
    ggplot() +
    geom_col(aes(x =  as.numeric(proportion_by_district),
                 y = bezirk,
                 fill = LCZ),
             show.legend = FALSE)  +
    facet_wrap(~LCZ)




crop_raster <- raster::as.factor(crop_raster)

raster_value_map <- data.frame(raster_value = 1:17,
                               lcz_class = c(1:10, LETTERS[1:7]))




levels(crop_raster) <- dplyr::left_join(rat, raster_value_map, by = c("ID" = "raster_value"))

crop_raster_rat <- raster::crop(lcz_europe,
                            sf::st_union(berlin_polygons) %>% sf::st_centroid() %>%
                                sf::st_buffer(35000)  %>% sf::st_as_sf())

test_raster <- raster::ratify(crop_raster_rat, "test.tiff")
