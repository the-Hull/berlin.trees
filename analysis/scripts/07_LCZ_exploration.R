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

gdaluti::gdalbuildvrt(gdalfile = "yourraster.tif",
             output.vrt = "tmp.vrt",
             te = st_bbox(shp_layerbuffer1))

tempraster <- raster("tmp.vrt")



crop_raster <- raster::crop(lcz_europe,
                            sf::st_union(berlin_polygons) %>% sf::st_centroid() %>%
                                sf::st_buffer(35000)  %>% sf::st_as_sf())


lcz_stars <- stars::st_as_stars(crop_raster)

colourCount = length(unique(raster::values(crop_raster)))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

plot(lcz_stars, col = viridisLite::cividis(14))
plot(lcz_stars, col =getPalette(colourCount))

ggplot() +
    stars::geom_stars(data = lcz_stars,
                      aes(x = x,
                          y = y,
                          fill = as.factor(WUDAPT_LCZ))) +
    geom_sf(data = berlin_polygons %>% dplyr::filter(NAMGEM == "Pankow"),
            color = "black",
            fill = NA,
            size = 2)


lcz_values_by_district <- raster::extract(crop_raster,
                                          sf::st_make_valid(berlin_polygons)) %>%
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



