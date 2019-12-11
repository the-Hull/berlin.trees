## Functions for reading/accessing data


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
#' @param sf_data sf tibble of Berlin City trees
#'
#' @return sf tibble with cleaned genus names
#' @export
#'
clean_genus <- function(sf_data){

    sf_data$GATTUNG <- tools::toTitleCase(tolower(sf_data$GATTUNG))
    sf_data$GATTUNG[sf_data$GATTUNG==""] <- NA

    return(sf_data)

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



#' Create overview-map of all data sets
#'
#' @param sf_data sf-tibble of Berlin trees
#' @param poly sf-tibble, polygons of Berlin districts
#'
#' @return ggplot object
#' @export
#' @import ggplot2
make_overview_map <- function(sf_data, poly){

    # extrafont::loadfonts("win", quiet = TRUE)

    gplot <- sf_data %>%
        dplyr::group_by(provenance) %>%
        dplyr::sample_n(5000) %>%
        dplyr::ungroup() %>%
        ggplot() +
        geom_sf(aes(color = GATTUNG),
                show.legend = FALSE,
                alpha = 0.3) +

        geom_sf(inherit.aes = FALSE,
                data = poly,
                fill = "transparent",
                show.legend = FALSE,
                color = "gray20",
                size = 0.5) +

        facet_wrap(~provenance) +

        scale_x_continuous(breaks = seq(13, 14, 0.2)) +
        scale_y_continuous(breaks = seq(52.3, 53.7, 0.1)) +

        theme_minimal(base_size = 16
                      # base_family = "Roboto Condensed"
        )

    return(gplot)

}
