# Functions for reading, accessing and wrangling data --------------------


#' Download Trees from daten.berlin.de
#'
#'
#' @details Downloads tree data from WFS Service for all packages containing 'baum',
#' excluding the Baumscheiben data set.
#'
#' @export
#' @import ckanr
#'

download_berlin_trees <- function(){


    # test



    # set base url
    ckanr_setup(url = "https://datenregister.berlin.de")

    # pull all packages from api, limit to 2500 (currentl ~ 2100 pckgs)
    p_list <- package_list(as = "table", url = "https://datenregister.berlin.de/", limit = 2500)


    # get tree-related data sets
    baum_index <- grep("baum", p_list)
    baum_packages <- p_list[baum_index]

    package_drop <- grep("baumscheibe", baum_packages)
    baum_packages <- baum_packages[-package_drop]

    # download meta-data for resources in each package
    package_results <- lapply(baum_packages, package_show)

    # extract only resources with WFS Service data
    wfs_subset <- purrr::map(package_results, "resources") %>%
        purrr::map_depth(2, ~purrr::has_element(.x, "WFS Service"))

    wfs_baum_resources <- purrr::map(package_results, "resources") %>%
        purrr::flatten() %>%
        purrr::keep(.p = unlist(wfs_subset))


    # get names from pertinent packages,
    # could call API again, but potentially costly
    wfs_baum_packages <- baum_packages[unlist(
        purrr::map(wfs_subset,
                   function(x) any(x == TRUE)))]


    # TEST IF COLLECTING RIGHT DATA
    # identical(wfs_baum_packages %>%
    #     purrr::map(package_show) %>%
    #     purrr::map("id"),
    #
    #     wfs_baum_resources %>%
    #         purrr::map("package_id") )
    # TEST TRUE



    # get urls for WFS service
    wfs_urls <- wfs_baum_resources %>%
        purrr::map("id") %>%
        purrr::map(resource_show) %>%
        purrr::map_chr("url")


    # set up paths to write results to disk
    json_paths <- wfs_baum_packages %>%
        purrr::map(package_show) %>%
        purrr::map_chr("title") %>%
        purrr::map_chr(function(x){

            file.path(here::here(),
                      "analysis",
                      "data",
                      "raw_data",
                      paste0(x,
                             ".json"))
        })


    # get feature names for GET request via WFS urls
    feature_names <- purrr::map(wfs_urls, function(x){

        capabilities <- httr::GET(x)
        xml_capabilities <- httr::content(capabilities, type = "text/xml")

        feature_name <- xml2::xml_contents(
            xml2::xml_child(
                xml2::xml_child(
                    xml_capabilities, 4),
                1))[1] %>%
            xml2::xml_text() %>% stringr::str_remove("fis:")

    }) %>%
        as.character()








    # extract content

    purrr::pwalk(list(wfs_urls, json_paths, feature_names),
                 function(u,jp,fnames){

                     query <- list(service = "WFS",
                                   request = "GetFeature",
                                   version = "2.0.0",
                                   TypeNames = fnames,
                                   # count = 10,
                                   outputFormat = 'application/geo+json')

                     # request data via GET (REST API and save to disk)
                     httr::GET(u,
                               query = query,
                               httr::write_disk(jp,
                                                overwrite = TRUE))
                 })




}



#' Load spatial-features data sets of all Berlin trees
#'
#' @param download_data Character, file paths for downloaded .geojson files from WFS service
#'
#' @return list of sf-tibbles
#' @export
#'
load_downloaded_data_to_lists <- function(download_data){

    json_paths <- download_data[[2]] %>%
        as.character()

    feature_names <- download_data[[3]] %>%
        as.character()

    sf_list <- purrr::map(json_paths,
                          ~sf::read_sf(.x)) %>%
        purrr::set_names(feature_names) %>%
        purrr::map2(names(.), ~dplyr::mutate(.x, provenance = .y))

    return(sf_list)

}


#' Bind rows of sf tibbles to single data set
#'
#' @param sf_list list containing simple feature tibbles
#'
#' @return single sf-tibble
#' @export
#'
bind_rows_sf <- function(sf_list){
    # bind data without geometry
    sf_df  <- lapply(sf_list, function(x) sf::st_set_geometry(x, NULL)) %>%
        dplyr::bind_rows()

    # make data frame and set geometry based on original sfcs
    d_sf  <-  sf::st_sf(sf_df, geometry = purrr::map(sf_list, "geometry") %>%
                            do.call(c, .))

    return(d_sf)

}


#' Clean Feature Meta Data
#'
#' @param sf_data sf-tibble of Berlin City trees
#'
#' @description Formats upper/lower case and removes "" as genera; still need to check if other
#' problematic values exist or can be recovered from other meta data.
#'
#' @return sf tibble with cleaned genus names
#' @export
#'
clean_data <- function(sf_data){

    sf_data$GATTUNG <- tools::toTitleCase(tolower(sf_data$GATTUNG))
    sf_data$GATTUNG[sf_data$GATTUNG==""] <- NA


    too_old_idx <- which(as.numeric(sf_data$STANDALTER) > 1500)


    sf_data[too_old_idx, c("STANDALTER", "PFLANZJAHR")] <-
        sf_data[too_old_idx, rev(c("STANDALTER", "PFLANZJAHR"))]


    sf_data <- sf_data %>%
        dplyr::filter(!is.na(GATTUNG)) %>%
        dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
                      # GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),


                      gattung_short = forcats::fct_lump(as.factor(GATTUNG),11, other_level = "Other") %>%
                          as.character(),
                      gattung_short =  ifelse(GATTUNG == "Pinus", "Pinus", gattung_short),
                      gattung_short = forcats::fct_infreq(as.factor(gattung_short)),
                      gattung_short = forcats::fct_relevel(gattung_short, "Other", after = Inf),
                      krone_m = as.numeric(ifelse(!is.na(KRONEDURCH), KRONEDURCH, KRONENDURC)),
                      dbh_cm = as.numeric(ifelse(!is.na(STAMMUMFAN), STAMMUMFAN, STAMMUMFG)) /
                                              pi,
                      species_corrected = ifelse(!is.na(ART_BOT),
                                                 ART_BOT,
                                                 paste(gattung_short, "spec.")),


                      bezirk_num = as.numeric(as.factor(BEZIRK)))

    return(sf_data)

}



# Spatial -----------------------------------------------------------------




#' Define bounding box as area of interest
#'
#' @param lat_min numeric
#' @param lat_max numeric
#' @param lon_min numeric
#' @param lon_max numeric
#' @param feature_name character, name of sf feature
#' @param crs numeric, defaults to 4326 (lat-lon Mercator, WGS84)
#'
#' @return sf polygon
#' @export
#'
make_bbox <- function(lat_min, lat_max, lon_min, lon_max, feature_name, crs = 4326){

    lat_b_left <- lat_min
    lon_b_left <- lon_min


    lat_t_left <- lat_max
    lon_t_left <- lon_min

    lat_t_right <- lat_max
    lon_t_right <- lon_max

    lat_b_right <- lat_min
    lon_b_right <- lon_max

    x <- c(lon_b_left, lon_t_left, lon_t_right, lon_b_right, lon_b_left)
    y <- c(lat_b_left, lat_t_left, lat_t_right, lat_b_right, lat_b_left)

    bbox <- sf::st_sf(data.frame(bbox = feature_name,
                                 geometry = sf::st_sfc(sf::st_polygon(list(cbind(x, y)),
                                                                      dim = "XY"),
                                                       crs = crs)))

    return(bbox)
}





#' Access Berlin district polygon from external source
#'
#' @return sf-tibble with polygons of Berlin districts
#' @export
#'
get_berlin_polygons_as_sf <- function(){
    cat("Accessing Berlin District polygon via GitHub/m-hoerz \n")

    berlin_poly <- sf::read_sf("https://raw.githubusercontent.com/m-hoerz/berlin-shapes/master/berliner-bezirke.geojson")
    return(berlin_poly)
}

#' Crop Spatial data with bbox
#'
#' @param sf_data_list list containing sf tibbles
#' @param bbox sf tibble, polygon used for cropping
#'
#' @return sf tibble, cropped with specified bbox
#' @export
#'
crop_data_with_bbox <- function(sf_data_list, bbox){

    # extract crs from list element,
    # transform crs of bbox accordingly
    # crop data

    furrr::future_map(sf_data_list,
                      function(x){

                          b <- sf::st_transform(bbox, crs = sf::st_crs(x))

                          cropped_sf <- sf::st_crop(x, b)

                          return(cropped_sf)
                      }
    )
}




#' Extract UHI rasters from tifs
#'
#' @description Extracts summer and night time UHI gridded data from tifs.
#' Data was accessed via the UHI Explorer App (2020-01-08;
#' \url{https://yceo.users.earthengine.app/view/uhimap}) for the Berlin Urban Cluster.
#'
#' @references Chakraborty, T., and X. Lee.
#' A Simplified Urban-Extent Algorithm to Characterize Surface Urban Heat Islands
#' on a Global Scale and Examine Vegetation Control on Their Spatiotemporal Variability’.
#' International Journal of Applied Earth Observation and Geoinformation 74 (February 2019):
#' 269–80. \url{https://doi.org/10.1016/j.jag.2018.09.015}.

#'
#' @param path Character, path to folder containing UHI zip archives.
#'
#' @return A nested list containing RasterLayers with the following levels:
#' \enumerate{
#' \item{Time of year}{Summer / Winter}
#' \item{Time of day}{Night / Day}
#' }
#' For the Years 2003 to 2017.
#'
#' @export
#'
get_uhi_rasters <- function(path){



    folder_path <- path
    grid_zips <- file.path(folder_path) %>%
        list.files()


    uhi_stacks <- furrr::future_map(
        grid_zips,
        function(x){


            # grab all tif files
            all_files <- utils::unzip(zipfile = file.path(folder_path, x), list = TRUE) %>%
                dplyr::filter(grepl(pattern = "tif", x = .$Name)) %>%
                dplyr::select(Name)

            # if(nrow(all_files) == 0) stop("No UHI .tif(s) found.")

            day_night_list <- furrr::future_map(
                c("night", "day"),
                function(time_of_day) {


                    uhi_grid_stack <- raster::stack(x = utils::unzip(zipfile = file.path(folder_path, x),
                                                                     files = dplyr::filter(all_files,
                                                                                           grepl(pattern = time_of_day,
                                                                                                 x = Name))$Name,
                                                                     exdir = tempdir()))


                    uhi_grid_stack[uhi_grid_stack < -100] <- NA


                    names(uhi_grid_stack) <- paste0(time_of_day,
                                                    "_",
                                                    sub(pattern = ".*([0-9]{4})$",
                                                        replacement = "\\1",
                                                        x = names(uhi_grid_stack)))


                    return(uhi_grid_stack)



                }) %>%
                purrr::set_names(c("night", "day"))

            return(day_night_list)

                                    }) %>%
        purrr::set_names(tools::file_path_sans_ext(grid_zips))


    return(uhi_stacks)
}







#' Add UHI data from RasterLayer stack to sf data frame
#'
#' @param uhi_stack_list List, nested on time of year, time of day
#' @param sf_data sf points, data frame with Berlin tree locations and meta data
#'
#' @return A nested list containing RasterLayers with the following levels:
#' \enumerate{
#' \item{Time of year}{Summer / Winter}
#' \item{Time of day}{Night / Day}
#' }
#' Containing sf point objects (multiple copies of full data set - careful! \strong{needs improvement!})
#' @importFrom methods "is"
#'
#' @export
#'
add_uhi_hist_data <- function(uhi_stack_list, sf_data){


    # brief checks
    if (!methods::is(sf_data, "sf")) {
        stop("input data is not in sf")
    }
    # if (!is(uhi_stack_list, "RasterLayer")) {
    #     stop("input raster is not a RasterLayer")
    # }


    # LOGIC



    ## tree uhi exposure

    ### prepare tree data

    # sf_data <- sf_data %>%
    #     dplyr::filter(!is.na(GATTUNG)) %>%
    #     dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
    #                   # GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),
    #
    #
    #                   gattung_short = forcats::fct_lump(as.factor(GATTUNG),11, other_level = "Other") %>%
    #                       as.character(),
    #                   gattung_short =   ifelse(GATTUNG == "Pinus", "Pinus", gattung_short),
    #                   gattung_short = forcats::fct_infreq(as.factor(gattung_short)),
    #                   gattung_short = forcats::fct_relevel(gattung_short, "Other", after = Inf),
    #
    #
    #                   bezirk_num = as.numeric(as.factor(BEZIRK)))



    ### re-project raster to UTM of sf data

    uhi_stack_list <- lapply(uhi_stack_list,
                             function(stack)
                                 lapply(stack, function(x)  {

                                     raster::projectRaster(x,
                                                           crs =  sf::st_crs(sf_data)$proj4string)

                                 }  ))


    extract_vals <- lapply(uhi_stack_list,
                           function(stacks){
                               lapply(stacks,
                                      raster::extract,
                                      sf_data,
                                      # df = TRUE,
                                      sp = FALSE)
                           })



    return(extract_vals)



}


# Stats -------------------------------------------------------------------

### UHI


#' Calculate descriptive statistics for a UHI stacks (in lists)
#'
#' @param uhi_stack_list A two-level list (time of year, time of day) of UHI data
#'
#' @return A list containing Median, Mean and StDev of UHI intensities by RasterLayer (years)
#' @export
#'
calc_uhi_stats <- function(uhi_stack_list){


        ## raster stats

        stat_funs <- c("median", "mean", "sd")

        ### cycle into list containing the 4 (total) stacks (2x summer, 2x winter)
        raster_stats <- lapply(stat_funs,
                               function(stats) {

                                   # into list (summer vs. winter)
                                   lapply(uhi_stack_list,
                                          function(stacks) {
                                              # into stacks (day vs. night)
                                              lapply(stacks,

                                                     function(stack){
                                                         stat_result <- raster::cellStats(stack, stats) %>%
                                                             data.frame(val = .,
                                                                        year = sub(pattern = ".*([0-9]{4})$",
                                                                                   replacement = "\\1",
                                                                                   x = names(.)),
                                                                        stringsAsFactors = FALSE)
                                                     })
                                          })

                               }) %>%
            stats::setNames(stat_funs)


        return(raster_stats)


}





#' Generate data set for stat-models
#'
#' @param full_df cleaned data.frame of berlin trees
#' @param extract_uhi list of sf data frames with UHI values
#'
#' @return a data frame for use with \code{\link{apply_models}}
#' @export
#' @importFrom magrittr "%>%"
# #'
make_test_data_set <- function(full_df = full_data_set_clean,
                               extract_uhi = extract_uhi_values_to_list){



    test_set <- dplyr::mutate(cbind(as.data.frame(full_df),
                            extract_uhi$Summertime_gridded_UHI_data$day),

                      STANDALTER = as.numeric(STANDALTER),
                      age_group = cut(STANDALTER, breaks = seq(0, 280, 40)),
                      ART_BOT = ifelse(is.na(ART_BOT),
                                       paste(gattung_short, "spec."),
                                       ART_BOT))


    return(test_set)
}










#' Apply models (lme4)
#'
#'
#'
#' @param df data.frame containing extracted uhi values and tree data
#' @param model_list list of models to apply
#' @param n_top_species numeric, number of top species to consider
#' @param min_individuals numeric, min. individuals per species required
#'   for inclusion
#'
#' @return a list containing lme4 model outputs
#' @export
#'
#' @import furrr
#' @import dplyr
#' @import lme4
#' @import rlang
#' @importFrom magrittr "%>%"
#'
#'
# apply_models <- function(df,
#                          model_list,
#                          n_top_species = 6,
#                          min_individuals = 150){
# #
# #     filt_args <- rlang::enquos(filter_args)
#
#     test_set <- df
#
#
#
#
#     apply_model_full <- function(.model, .df){
#         .model(.df)
#     }
#
#     top_species <- test_set %>%
#         group_by(gattung_short) %>%
#         count(ART_BOT) %>%
#         arrange(desc(n), .by_group = TRUE) %>%
#         top_n(n_top_species)
#
#     test_set <- test_set %>%
#         filter(ART_BOT %in% top_species$ART_BOT[top_species$n > min_individuals])
#
#
#     # future::plan(future::multiprocess)
#     model_out <- furrr::future_map(model_list,
#                                    apply_model_full,
#                                    .df = test_set)
#
#
#     return(model_out)
#
#
#
# }
apply_models <- function(df = test_df,
                         model_list = model_list,
                         n_top_species = 6,
                         min_individuals = 150){


    test_set <- df




    apply_model_full <- function(.model, .data){
        .model(.data)
    }

    top_species <- test_set %>%
        dplyr::group_by(gattung_short) %>%
        dplyr::count(ART_BOT) %>%
        dplyr::arrange(desc(n), .by_group = TRUE) %>%
        dplyr::top_n(n_top_species)


    test_set <- test_set %>%
        dplyr::filter(ART_BOT %in% top_species$ART_BOT[top_species$n > min_individuals])


    future::plan(future::multiprocess)
    model_out <- furrr::future_map(model_list,
                                   apply_model_full,
                                   .data = test_set)


    return(model_out)



}









# PLOTS -------------------------------------------------------------------




#' Create overview-map of all data sets
#'
#' @param sf_data sf-tibble of Berlin trees
#' @param poly sf-tibble, polygons of Berlin districts
#'
#' @return ggplot object
#' @import ggplot2
#' @import sf
#' @export
make_overview_map <- function(sf_data, poly){

    # extrafont::loadfonts("win", quiet = TRUE)


    gplot <- sf_data %>%
        dplyr::group_by(provenance) %>%
        dplyr::sample_n(7000) %>%
        dplyr::ungroup() %>% {
        ggplot() +



        geom_sf(inherit.aes = FALSE,
                aes(geometry = geometry),
                data = poly,
                fill = "gray80",
                color = "white",
                show.legend = FALSE,
                size = 0.7) +


        geom_sf(color = "black",
                inherit.aes = FALSE,
                data = .,
                aes(geometry = geometry),
                size = 0.2,
                show.legend = FALSE,
                alpha = 0.3) +


        # geom_sf(inherit.aes = FALSE,
        #         data = poly,
        #         fill = "transparent",
        #         color = "white",
        #         show.legend = FALSE,
        #         size = 0.75,
        #         alpha = 0.5) +

        facet_wrap(~provenance) +

        scale_x_continuous(breaks = seq(13, 14, 0.2)) +
        scale_y_continuous(breaks = seq(52.3, 53.7, 0.1)) +

        theme_minimal(base_size = 18
                      # base_family = "Roboto Condensed"
        ) +

        labs(caption = paste0("Data source: daten.berlin.de; WFS Service, accessed: ",
                              "2019-12-13"))
        }

    return(gplot)

}



#' Generate overview of records (bar plot)
#'
#' @param sf_data sf-tibble of Berlin City trees
#'
#' @return ggplot bar graph
#' @export
#'
#' @import ggplot2
tree_sums_bar_plot <- function(sf_data){



    sf_data %>%
        # dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
        #        GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),
        #        gattung_short = forcats::fct_lump(GATTUNG,11),
        #        bezirk_num = as.numeric(as.factor(BEZIRK))) %>%


        ggplot() +

        coord_flip() +

        geom_bar(aes(x = gattung_short,
                              fill = provenance),
                          show.legend = TRUE,
                          color = "gray10") +


        labs(subtitle =  paste0("total records: ",
                                nrow(sf_data),
                                "\n",
                                "total genera: ",
                                sf_data$GATTUNG %>%
                                    unique() %>% length()),

             caption = paste0("Data source: daten.berlin.de; WFS Service, accessed: ",
                              Sys.Date()),
             y = "Count",
             x = "Genus",
             fill = NULL) +


        theme_minimal(base_size = 18) +
        theme(legend.position = c(0.8,0.5),
              axis.text.y = element_text(face = "italic")) +


        scale_fill_brewer(type = "seq", palette = "Greens")

}




#' Spatially-binned tree counts
#'
#' @param sf_data sf-tibble of Berlin City trees
#' @param poly sf-tibble of Berlin Districts
#'
#' @return ggplot map
#' @export
#'
#' @import ggplot2
tree_count_map <- function(sf_data, poly){




    sf_plot <- sf_data %>%
        # dplyr::filter(!is.na(GATTUNG)) %>%
        # dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
        #               # GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),
        #
        #
        #               gattung_short = forcats::fct_lump(as.factor(GATTUNG),11, other_level = "Other") %>%
        #                   as.character(),
        #               gattung_short =   ifelse(GATTUNG == "Pinus", "Pinus", gattung_short),
        #               gattung_short = forcats::fct_infreq(as.factor(gattung_short)),
        #               gattung_short = forcats::fct_relevel(gattung_short, "Other", after = Inf),
        #
        #
        #               bezirk_num = as.numeric(as.factor(BEZIRK))) %>%
        # dplyr::add_count(gattung_short) %>%
        # dplyr::filter(!is.na(gattung_short)) %>%
        dplyr::filter(STAMMUMFG > 62.83185) %>% {
            ggplot(.) +

                geom_sf(inherit.aes = FALSE,
                        aes(geometry = geometry),
                        data = poly,
                        fill = "gray80",
                        color = "white",
                        show.legend = FALSE,
                        size = 0.7) +


                stat_bin2d(aes(x = sf::st_coordinates(.)[,"X"],
                               y = sf::st_coordinates(.)[,"Y"],
                               group = gattung_short,
                               fill = stat(ncount)),
                           alpha = 0.8,
                           bins = 25,
                           binwidth = c(1500,1500)) +



                geom_sf(inherit.aes = FALSE,
                        aes(geometry = geometry),
                        data = poly,
                        fill = "transparent",
                        color = "gray80",
                        show.legend = FALSE,
                        size = 0.5) +




                facet_wrap(~gattung_short) +

                labs(caption = paste0("Data source: daten.berlin.de; WFS Service, accessed: ",
                                      "2019-12-15"),
                     fill = "normalized Count")+



                scale_x_continuous(breaks = seq(13, 14, 0.2)) +
                scale_y_continuous(breaks = seq(52.3, 53.7, 0.1)) +
                scale_fill_viridis_c() +
                guides(fill = guide_colorbar(barwidth = grid::unit(10, units = "cm"),
                                             barheight = grid::unit(.45, units = "cm"))) +


                theme_minimal(base_size = 18) +

                theme(axis.title = element_blank(),
                      strip.text = element_text(face = c("bold.italic")),
                      legend.position = "bottom",
                      legend.direction = "horizontal",
                      legend.title = element_text(vjust = 1))

        }

    return(sf_plot)


}


#' Density plot overview
#'
#' This function is hard-codes use of 2007 Summer, Day UHI data.
#'
#' @param sf_data sf, data set containing Berlin trees
#' @param extracted_uhi List, contains UHI intensities for Summer/Winter and day/night.
#' @param position_stack Character, either "stack" or "dodge".
#' @param ymin Numeric, coordinate for inset plot.
#' @param ymax Numeric, coordinate for inset plot.
#' @param xmin Numeric, coordinate for inset plot.
#' @param xmax Numeric, coordinate for inset plot.
#'
#' @usage dens_plot_trees(sf_data,
#'   extracted_uhi,
#'   position_stack = "stack",
#'   ymin = 0.4,
#'   ymax=1.4,
#'   xmin=-5,
#'   xmax=-0.5)
#'
#' @return A ggplot2 plot object
#' @export
#'
#' @import ggplot2
dens_plot_trees <- function(sf_data,
                            extracted_uhi,
                            position_stack = "stack",
                            ymin = 0.4,
                            ymax=1.4,
                            xmin=-5,
                            xmax=-0.5){



    sf_extracted_uhi <- cbind(sf_data, extracted_uhi$Summertime_gridded_UHI_data$day)

    # helper function to add panel-dependent insets
    annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data)
    {
        layer(data = data, stat = StatIdentity, position = PositionIdentity,
              geom = ggplot2:::GeomCustomAnn,
              inherit.aes = TRUE, params = list(grob = grob,
                                                xmin = xmin, xmax = xmax,
                                                ymin = ymin, ymax = ymax))
    }


    # density plots with panels
    dens_plot <- sf_extracted_uhi %>%

        ggplot() +
        # geoms by tree type
        geom_density(aes(x = day_2007, fill = provenance),
                     alpha =  0.2,
                     col = "transparent",
                     size = 1,
                     position = position_stack) +
        stat_density(geom = "line",
                     aes(x = day_2007,
                         col = provenance,
                         fill = provenance),
                     alpha =  0.8,
                     size = 1.2,
                     position = position_stack) +

        # full data set
        stat_density(geom = "line",
                     aes(x = day_2007),
                     col = "gray10",
                     alpha =  0.8,
                     size = 0.8) +

        geom_vline(xintercept = 0, linetype = 2) +

        facet_wrap(~gattung_short) +

        # scales
        xlim(c(-5, 6.5)) +
        scale_color_brewer(type = "qual", palette = "Set2") +
        scale_fill_brewer(type = "qual", palette = "Set2") +

        # theming
        guides(color = FALSE) +
        theme_bw(base_size = 16) +
        theme(legend.position = c(0.6, 0.1),
              legend.direction = "horizontal") +
        labs(x = "Summer day-time UHI magnitude (C)", y = "Density", fill = "Type")






    bar_plots <- purrr::map(levels(sf_extracted_uhi$gattung_short),

                            function(gattung){


                                annotation_custom2(
                                    grob = ggplotGrob(
                                        sf_extracted_uhi %>%

                                            ggplot() +

                                            coord_flip() +

                                            geom_bar(aes(x = gattung_short,
                                                         fill = provenance,
                                                         alpha = gattung_short == gattung),
                                                     color = "transparent",
                                                     show.legend = FALSE) +



                                            scale_alpha_manual(values = c(0.3,
                                                                          0.95)) +
                                            scale_fill_brewer(type = "qual", palette = "Set2") +


                                            scale_y_continuous(breaks = c(0, 150000),
                                                               labels = c("0", "150k")) +


                                            labs(fill = NULL) +
                                            theme_minimal(base_size = 9) +
                                            theme(axis.text.y = element_blank(),
                                                  axis.title = element_blank(),
                                                  plot.margin = margin(),
                                                  panel.grid = element_blank(),
                                                  axis.ticks.length = unit(2, "mm"),
                                                  axis.ticks = element_line(color = "gray10"),
                                                  axis.ticks.y = element_blank())
                                    ),


                                    data = data.frame(gattung_short=gattung),
                                    ymin = ymin, ymax=ymax, xmin=xmin, xmax=xmax)






                            })


    return(dens_plot + bar_plots)


}





#' Make Random-effects effect-size plot
#'
#' @param model_out output list with models
#' @param model_name Character, model name
#' @param df data.frame with all trees
#' @param n_top_species Numeric, max number of species to inspect
#'
#' @return ggplot2 object and plot
#' @export
#' @import ggplot2
#' @import dplyr
#'
#'
make_ranef_plot <- function(model_out,
                            model_name = "heat_RIspecies_RSspecies_RIprovenance",
                            df,
                            n_top_species = 6){





    top_species <- df %>%
        as.data.frame() %>%
        dplyr::group_by(gattung_short) %>%
        dplyr::count(ART_BOT) %>%
        dplyr::arrange(desc(n), .by_group = TRUE) %>%
        dplyr::top_n(n_top_species)


    ranef_slopes <- model_out[[model_name]] %>%
        lme4::ranef() %>%
        as.data.frame() %>%
        dplyr::filter(term == "day_2007") %>%
        dplyr::mutate(grp = as.character(grp)) %>%
        dplyr::arrange(as.character(grp), as.numeric(condval)) %>%
        dplyr::mutate(gattung = sub("(.*:)(\\w+)(.*)", replacement = "\\2", x = as.character(grp), perl = FALSE),
               species = sub("(.*:)(.*$)", replacement = "\\2", x = as.character(grp), perl = FALSE),
               species_short = paste0(substr(gattung, 1, 1),
                                      ". ",
                                      sub("(^\\w+)( )(.*)", replacement = "\\3", x = as.character(species), perl = TRUE)),
               provenance = sub("(^\\w+):(.*)", replacement = "\\1", x = as.character(grp), perl = FALSE))


    p <- ranef_slopes %>%
        dplyr::left_join(top_species, by = c("species" = "ART_BOT")) %>%
        dplyr::arrange(gattung,condval ) %>%
        dplyr::mutate(grp = factor(grp,levels = grp),
               species_short = forcats::fct_reorder(species_short, condval),
               gattung = forcats::fct_reorder(gattung, n, .fun = sum, .desc = TRUE)) %>%
        ggplot2::ggplot(ggplot2::aes(x = species_short, y = condval, ymin = condval - condsd, ymax = condval + condsd)) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_point(ggplot2::aes(color = provenance, size = n,  group = provenance),
                   position = position_dodge(width = 0.2),
                   alpha = 0.8) +
        ggplot2::geom_linerange(ggplot2::aes(group = provenance, color = provenance),
                       position = position_dodge(width = 0.2),
                       alpha = 0.8) +
        ggplot2::facet_wrap(~gattung, scales = "free_y") +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::scale_color_brewer(type = "qual", palette = "Set2", direction = +1) +
        ggplot2::coord_flip() +
        ggplot2::theme(panel.border = element_rect(fill = "transparent"))

    return(p)



}







# helpers -----------------------------------------------------------------



#' Adjusted filtering with ...
#'
#' @param df data frame / tibble for filtering
#' @param ... unquoted expression for filtering
#'
#' @return filtered df
#' @export
#'
filter_maybe <- function(df, fargs){

    if(length(fargs) > 0){


        df <- df %>%
            dplyr::filter(!!!fargs)

        return(df)
    }

    else{return(df)}

}







