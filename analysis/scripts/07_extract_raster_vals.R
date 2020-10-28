
#' Extract values from raster based on (point) sf
#'
#' @param sf_data sf dframe, e.g. berlin trees
#' @param lcz_raster, raster
#' @param buff_dist, buffer around points / polygons
#' @param method character, "simple" (for categorical) or "bilinear"
#'
#' @return dframe, 1 row per sf geometry, with proportions of coverage
#' @export
#'
assess_relative_cover <- function(sf_data, lcz_raster, buff_dist = 100, method = "simple"){

    # ensure both objects have same projection

    if(!sf::st_crs(sf_data)$wkt ==
       raster::wkt(lcz_raster)){

        #
        #
        # lcz_raster <- raster::projectRaster(lcz_raster,
        #                       crs = sf::st_crs(full_data_set_clean)$wkt %>% raster::crs())
        #
        sf_data <- sf::st_transform(sf_data,
                                    crs = raster::crs(lcz_raster))
        message("adjusted CRS")
    }

    plot(lcz_raster)
    plot(sf_data, add = TRUE, cex = 1, col = "black")


    # extract values (proportionally) by tree buffer
    # raster::beginCluster(n = 10, type='SOCK')

    # extracted_vals <- raster::extract(lcz_raster, sf_data, method = "simple", buffer = buff_dist)
    extracted_vals <- dplyr::bind_rows(lapply(raster::extract(lcz_raster, sf_data, method = method, buffer = buff_dist),
                             function(x)prop.table(table(x))))
    # raster::endCluster()

}

#' Split df in chunks for mapping/looping
#'
#' @param dframe df, nrow > 0
#' @param cut_size numeric, number of rows in eaech chunk
#'
#' @return list of dfs with nrow <= cut_size
#' @export
#'
split_by_n <- function(dframe, cut_size){

    if(cut_size >= nrow(dframe)){
        message("no splitting done")
        return(dframe)
    }

    # make unique id
    cuts <- base::cut(1:nrow(dframe),
                      breaks = seq(from = 0, to = nrow(dframe) +1,
                                   by = cut_size))
    # make lists
    dframe_lists <- base::split(dframe, f = as.factor(cuts))

    return(dframe_lists)


}

library(future)
future::plan(multisession)


library(microbenchmark)


chunk_size_test <- microbenchmark(test_5000 =- furrr::future_map_dfr(split_by_n(full_data_set_clean[1:200000, ],
                                                                                5000),
                                                                     ~assess_relative_cover(.x, wudapt_lcz, 150),
                                                                     .progress = FALSE),
                                  test_20000 = furrr::future_map_dfr(split_by_n(full_data_set_clean[1:200000, ],
                                                                                 20000),
                                                                      ~assess_relative_cover(.x, wudapt_lcz, 150),
                                                                      .progress = FALSE),
                                  times = 10)





