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


#' Download baumscheiben from Berlin daten WFS
#'
#' @return Baumscheiben JSON
#' @import ckanr
#' @export
#'
download_berlin_baumscheiben <- function(){


    # test



    # set base url
    ckanr_setup(url = "https://datenregister.berlin.de")

    # pull all packages from api, limit to 2500 (currentl ~ 2100 pckgs)
    p_list <- package_list(as = "table", url = "https://datenregister.berlin.de/", limit = 2500)


    # get tree-related data sets
    baum_index <- grep("baumscheibe", p_list)
    baum_packages <- p_list[baum_index]


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


#' Download Building and Veg Height from Berlin Umweltatlas
#'
#' @param fpath character, file path for saving
#' @source https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=a_06_10gebveghoeh2010_geb1@senstadt&type=FEED
#'
#' @return nothing
#' @examples
download_building_height <- function(fpath = "./analysis/data/raw_data/spatial_ancillary/berlin_building_veg_height"){

    gebveg_xml <- httr::GET(url = "https://fbinter.stadt-berlin.de/fb/feed/senstadt/a_06_10gebveghoeh2010_geb1/0") %>%
        xml2::read_xml() %>%
        xml2::as_list()

    gebveg_xml <- gebveg_xml$feed$entry[3:6] %>% lapply(attr, "href") %>% unlist()

    # only dl gebaeude
    gebveg_xml <- gebveg_xml[grepl("gebaeude", x=gebveg_xml, ignore.case = TRUE)]




    fnames <- sapply(gebveg_xml, function(x){

        fname <- fs::path_file(x)

        # check if files already downloaded



        if(!file.exists(file.path(fpath, fname))){

            print("downloading building heights")
            httr::GET(url = x,
                      httr::write_disk(file.path(fpath, fname),
                                       overwrite = TRUE))
        }


        return(fname)

    },
    simplify = TRUE)

    zips <- file.path(fpath, fnames)


    # check if already unzipped
    if(length(

        list.files(path = fpath,
                   pattern = ".shp$",
                   recursive = TRUE)) == 0) {

        print("unzipping building heights")

        lapply(seq_along(zips),
               function(x){

                   unzip(zips[x],
                         exdir = fs::path_ext_remove(zips[x]))
               })

    }

    # read shp files, some manual work needed

    shps <- list.files(path = fpath,
                       pattern = ".shp$",
                       full.names = TRUE,
                       recursive = TRUE)



    ph1_idx <- grep(pattern ="phase1",
                    x = shps)


    building_height1 <- sf::read_sf(shps[ph1_idx]) %>%
        setNames(tolower(names(.))) %>%
        dplyr::select(c("os", "mean_ndom", "max_ndom", "min_ndom", "bezirk"))
    building_height2 <- sf::read_sf(shps[-ph1_idx]) %>%
        setNames(tolower(names(.))) %>%
        dplyr::select(c("os", "mean_ndom", "max_ndom", "min_ndom", "bezirk_kla")) %>%
        dplyr::rename(bezirk = bezirk_kla)

    # clean up
    building_full <- rbind(building_height1, building_height2)

    rm(building_height1, building_height2)

    building_full_raster <- raster::raster(building_full, resolution=10)

    building_rasterized <- raster::rasterize(building_full,
                                             building_full_raster,
                                             field = "mean_ndom")




    ## stars alternative:
    # building_full_raster_sf <- stars::st_rasterize(sf = building_full,
    #                                                template = stars::st_as_stars(st_bbox(building_full),
    #                                                                       dx = 10,
    #                                                                       dy = 10,
    #                                                                       values = NA_real_))

    return(building_rasterized)


}



#' Download Soil type classification for Berlin
#'
#' @param path character, path to save file (with .geojson extension)
#'
#' @return sf dframe
download_soil_types <- function(path = "./analysis/data/raw_data/spatial_ancillary/soil_type.geojson"){

    wfs_soil <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_boden_wfs1_2015"
    query <- list(service = "WFS",
                  request = "GetFeature",
                  version = "2.0.0",
                  TypeNames = "fis:s_boden_wfs1_2015",
                  # count = 10,
                  outputFormat = 'application/geo+json')

    httr::GET(wfs_soil,
              query = query,
              httr::write_disk(path = path,
                               overwrite = TRUE))


    # request data via GET (REST API and save to disk)
    soil_sf <- sf::st_read(path,
                           quiet = TRUE)

    return(soil_sf)
}



#' Access Berlin district polygon from external source
#'
#' @return sf-tibble with polygons of Berlin districts
#' @import httr
#' @export
#'
download_berlin_polygons_as_sf <- function(path = "./analysis/data/raw_data/spatial_ancillary/berlin_polygons.geojson"){
    # cat("Accessing Berlin District polygon via GitHub/m-hoerz \n")

    # berlin_poly <- sf::read_sf("https://raw.githubusercontent.com/m-hoerz/berlin-shapes/master/berliner-bezirke.geojson")
    # return(berlin_poly)
    #
    #
    #
    wfs_bez <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_wfs_alkis_bezirk"


    query <- list(service = "WFS",
                  request = "GetFeature",
                  version = "2.0.0",
                  TypeNames = "fis:s_wfs_alkis_bezirk",
                  # count = 10,
                  outputFormat = 'application/geo+json')

    # request data via GET (REST API and save to disk)

    httr::GET(wfs_bez,
              query = query,
              httr::write_disk(path = path, overwrite = TRUE)

    )

    berlin_polygons <- sf::st_read(path, quiet = TRUE) %>%
        sf::st_make_valid()

    return(berlin_polygons)
}

#' Crop Spatial data with bbox
#'
#' @param sf_data_list list containing sf tibbles
#' @param bbox sf tibble, polygon used for cropping
#'
#' @return sf tibble, cropped with specified bbox
#' @export
#'


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

#'
#' #' Add tree info to Baumscheiben data set
#' #'
#' #' Grabs the closest tree (within max_dist_m) to a Baumscheibe, and adds the
#' #' corresponding index from the tree data set as a column to baumscheiben
#' #'
#' #' @param bms list, contains an sf_df with baumscheiben polygons
#' #' @param fulldf sf_df with all berlin trees
#' #' @param max_dist_m numeric, max distance to buffer around a baumscheiben centroid to look for trees
#' #'
#' #' @return baumscheiben_in_lists, with modified element (added column of tree indices)
#' #'
#' #' @import dplyr
#' #'  sf
#' #'  furrr
#' #'
#' #' @export
#' add_full_df_idx_to_baumscheiben <- function(bms,
#'                                             fulldf,
#'                                             max_dist_m = 15){
#'
#'     # make unique id
#'     bms$s_Baumscheibe$split_id <- seq_len(nrow(bms$s_Baumscheibe))
#'     # make unique labels
#'     bms$s_Baumscheibe$split_label <- base::cut(
#'         x = bms$s_Baumscheibe$split_id,
#'         breaks = seq(0,
#'                      max(bms$s_Baumscheibe$split_id) + 1,
#'                      by = 3000),
#'         labels = FALSE)
#'
#'
#'
#'     # make temp baumscheiben object
#'     bms$s_Baumscheibe <-  sf::st_transform(bms$s_Baumscheibe,
#'                                st_crs(fulldf))
#'
#'     # generate buffer and adjust crs
#'     baumsch_buff <- sf::st_buffer(x = sf::st_centroid(bms$s_Baumscheibe),
#'                                   dist = max_dist_m)
#'     # assess which trees fall into specific buffer
#'     # this outputs a list of indices in the y object (fulldf)
#'     trees_in_buffs <- sf::st_intersects(baumsch_buff, fulldf)
#'
#'     # calc centroid for later use
#'     baumsch_centroid <- sf::st_centroid(bms$s_Baumscheibe)
#'
#'
#'
#'
#'     closest_tree_idx <- unlist(
#'         furrr::future_map(
#'             base::split(bms$s_Baumscheibe$split_id,
#'                         f = bms$s_Baumscheibe$split_label),
#'             function(spidx){
#'
#'                 spidx <- as.numeric(spidx)
#'
#'                 baumsch <- bms$s_Baumscheibe[spidx, ]
#'
#'                 tree_idx <- lapply(
#'                     seq_along(trees_in_buffs[spidx]),
#'                     function(x){
#'
#'                         # grab the closest tree idx to the centroid
#'                         idx <- which.min(st_distance(fulldf[trees_in_buffs[spidx][[x]], ],
#'                                                      baumsch_centroid[spidx[x], ]))
#'                         # subset the potential trees to the closests one
#'                         out <- trees_in_buffs[spidx][[x]][idx]
#'
#'                         # clean up
#'                         out <- ifelse(length(out) == 0,
#'                                       NA,
#'                                       out)
#'
#'
#'                     })
#'
#'             return(tree_idx)
#'
#'         })
#'     )
#'
#'     bms$s_Baumscheibe$closest_tree_idx <- closest_tree_idx
#'
#'     return(bms)
#'
#' }
#'


#' Prepare Baumscheiben Area to Tree Data set
#'
#' @param fulldf sf_df, all berlin trees
#' @param bms sf_df, Baumscheiben polygons and their area
#' @param max_dist_m numeric, how far from polygon centroid can tree lie and still be assigned?
#'
#' @return fulldf with additional columns: "baumsch_dist_m",
#'  "baumsch_elem_nr",
#'  "baumsch_gis_id",
#'   "baumsch_flaeche_m2"
#' @export
#'
#' @import sf
#' units
#'
add_baumscheiben_flaeche <- function(fulldf, bms, max_dist_m = 10){

    nearest_idx <- sf::st_nearest_feature(fulldf, bms)

    # calculate pairwise distances
    fulldf$baumsch_dist_m <- sf::st_distance(fulldf, bms[nearest_idx, ], by_element = TRUE)

    bms <- sf::st_drop_geometry(bms[nearest_idx, c("elem_nr", "gis_id", "flaeche"), ])
    names(bms) <- c("baumsch_elem_nr", "baumsch_gis_id", "baumsch_flaeche_m2")

    # join manually
    fulldf <- cbind(fulldf,
                    bms)

    fulldf$baumsch_flaeche_m2 <- ifelse(fulldf$baumsch_dist <= units::set_units(max_dist_m, m),
                                        fulldf$baumsch_flaeche_m2,
                                        NA)

    return(fulldf)

}


#' Prepare Soil Type for each Location
#'
#' @param fulldf sf_df, all berlin trees
#' @param bms sf_df, Baumscheiben polygons and their area
#' @param max_dist_m numeric, how far from polygon centroid can tree lie and still be assigned?
#'
#' @return fulldf with additional columns: "baumsch_dist_m",
#'  "baumsch_elem_nr",
#'  "baumsch_gis_id",
#'   "baumsch_flaeche_m2"
#' @export
#'
#' @import sf
#' units
#'
prep_soil_type <- function(fulldf, sty, max_dist_m = 50){

    nearest_idx <- sf::st_nearest_feature(fulldf, bms)

    # calculate pairwise distances
    fulldf$baumsch_dist_m <- sf::st_distance(fulldf, bms[nearest_idx, ], by_element = TRUE)

    bms <- sf::st_drop_geometry(bms[nearest_idx, c("elem_nr", "gis_id", "flaeche"), ])
    names(bms) <- c("baumsch_elem_nr", "baumsch_gis_id", "baumsch_flaeche_m2")

    # join manually
    fulldf <- cbind(fulldf,
                    bms)

    fulldf$baumsch_flaeche_m2 <- ifelse(fulldf$baumsch_dist <= units::set_units(max_dist_m, m),
                                        fulldf$baumsch_flaeche_m2,
                                        NA)

    return(fulldf)

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
    sf_df  <- lapply(sf_list, function(x){

        sf::st_set_geometry(x, NULL) %>%
           setNames(toupper(colnames(x)))

    }) %>%
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

    # colnames(sf_data)[which(colnames(sf_data) != "geometry")] <- toupper(colnames(sf_data)[which(colnames(sf_data) != "geometry")])

    # sf_data$GATTUNG <- ifelse(!is.na(sf_data$GATTUNG),
    #                           sf_data$GATTUNG,
    #                           sf_data$gattung)

    sf_data$GATTUNG <- tools::toTitleCase(tolower(sf_data$GATTUNG))
    sf_data$GATTUNG[sf_data$GATTUNG==""] <- NA


    too_old_idx <- which(as.numeric(sf_data$STANDALTER) > 1500)


    sf_data[too_old_idx, c("STANDALTER", "PFLANZJAHR")] <-
        sf_data[too_old_idx, rev(c("STANDALTER", "PFLANZJAHR")), drop = TRUE]


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


                      bezirk_num = as.numeric(as.factor(BEZIRK))) %>%
        dplyr::rename(provenance = PROVENANCE)

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


#' crop raster to (buffered) shp file extent (using centroid)
#'
#' This function factorized the raster and adds an LCZ value table with
#' 1:10 and A:G to classes present in the cropped raster
#'
#' @param rasterpath character, path to e.g. LCZ tif file
#' @param sf_data sf dframe, e.g. berlin polygons
#' @param buffer_dist numeric, meters (around unioned sf_data)
#'
#' @return
#' @export
#'
crop_raster <- function(rasterpath, sf_data, buffer_dist = 35000){

    # read raster and extract crs
    rasterdat <- raster::raster(rasterpath)
    raster_crs <- raster::crs(rasterdat)

    # adjust projections
    sf_data <- sf::st_transform(sf_data,
                                crs = raster_crs)

    # cropped raster
    crop_raster <- raster::crop(rasterdat,
                                sf::st_union(sf_data) %>%
                                    sf::st_buffer(buffer_dist)  %>% sf::st_as_sf())
    # crop_raster <- raster::crop(rasterdat,
    #                             sf::st_union(sf_data) %>% sf::st_centroid() %>%
    #                                 sf::st_buffer(buffer_dist)  %>% sf::st_as_sf())

    crop_raster <- raster::as.factor(crop_raster)

    raster_value_map <- data.frame(raster_value = 1:17,
                                   lcz_class = c(1:10, LETTERS[1:7]))

    rat <- levels(crop_raster) %>% as.data.frame()


    levels(crop_raster) <- dplyr::left_join(rat, raster_value_map, by = c("ID" = "raster_value"))

    return(crop_raster)
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
assess_relative_lcz_cover <- function(sf_data, lcz_raster, buff_dist = 100, method = "simple"){

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

    # plot(lcz_raster)
    # plot(sf_data, add = TRUE, cex = 1, col = "black")


    # extract values (proportionally) by tree buffer
    # raster::beginCluster(n = 10, type='SOCK')

    # extracted_vals <- raster::extract(lcz_raster, sf_data, method = "simple", buffer = buff_dist)
    # extracted_vals <- dplyr::bind_rows(lapply(raster::extract(lcz_raster, sf_data, method = method, buffer = buff_dist),
    #                                           function(x)prop.table(table(x))))


     extracted_vals <- raster::extract(lcz_raster,
                                       sf_data,
                                       method = method,
                                       buffer = buff_dist,
                                       df = TRUE,
                                       factors = TRUE)

     extracted_vals <- data.table::as.data.table(extracted_vals)
     extracted_vals <- extracted_vals[,.N, by = c("ID", "lcz_class")]
     extracted_vals[ , `:=`(prop_class = prop.table(N)), by = "ID"]

     extracted_vals <- data.table::dcast(extracted_vals, ID ~ lcz_class, value.var = "prop_class")

    # raster::endCluster()

     return(extracted_vals)

}


#' Extract values from raster based on (point) sf
#'
#' @param sf_data sf dframe, e.g. berlin trees
#' @param bh_raster, raster, building height (or other)
#' @param buff_dist, buffer around points / polygons
#' @param method character, "simple" (for categorical) or "bilinear"
#'
#' @return dframe, 1 row per sf geometry, with proportions of coverage
#' @export
assess_relative_building_height <- function(sf_data, bh_raster, buff_dist = 100, method = "simple"){

    # ensure both objects have same projection

    if(!sf::st_crs(sf_data)$wkt ==
       raster::wkt(bh_raster)){

        #
        #
        # bh_raster <- raster::projectRaster(bh_raster,
        #                       crs = sf::st_crs(full_data_set_clean)$wkt %>% raster::crs())
        #
        sf_data <- sf::st_transform(sf_data,
                                    crs = raster::crs(bh_raster))
        message("adjusted CRS")
    }

    # plot(bh_raster)
    # plot(sf_data, add = TRUE, cex = 1, col = "black")


    # extract values (proportionally) by tree buffer
    # raster::beginCluster(n = 10, type='SOCK')

    # extracted_vals <- raster::extract(bh_raster, sf_data, method = "simple", buffer = buff_dist)
    # extracted_vals <- dplyr::bind_rows(lapply(raster::extract(bh_raster, sf_data, method = method, buffer = buff_dist),
    #                                           function(x)prop.table(table(x))))


    extracted_vals <- raster::extract(bh_raster,
                                      sf_data,
                                      method = method,
                                      buffer = buff_dist,
                                      fun = mean,
                                      na.rm = TRUE,
                                      df = FALSE,
                                      factors = FALSE)
    return(extracted_vals)

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


    # future::plan(future::multiprocess)
    model_out <- furrr::future_map(model_list,
                                   apply_model_full,
                                   .data = test_set)


    return(list(model = model_out, test_data = test_set))



}









# PLOTS -------------------------------------------------------------------




#' Create overview-map of all data sets
#'
#' @param sf_data sf-tibble of Berlin trees
#' @param poly sf-tibble, polygons of Berlin districts
#' @param file character, file path (use with \code{\link{file_out}}), must include file ending
#' @param height numeric, height in inches
#' @param width numeric, width in inches
#' @param dpi numeric, dpi of output
#'
#' @return ggplot object
#' @import ggplot2
#' @import sf
#' @export
make_overview_map <- function(sf_data,
                              poly,
                              base_size = 18,
                              file,
                              height,
                              width,
                              dpi){

    # extrafont::loadfonts(device = "win", quiet = TRUE)
    extrafont::loadfonts(device = "pdf", quiet = TRUE)


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

                theme_minimal(base_size = base_size,
                              base_family = "Roboto Condensed"
                ) +

                # labs(caption = paste0("Data source: daten.berlin.de; WFS Service, accessed: ",
                #                       "2019-12-13"))

                facet_wrap(~provenance,
                           labeller = labeller(provenance = c("s_uferbaeume" = "Riparian",
                                                              "s_wfs_baumbestand" = "Street",
                                                              "s_wfs_baumbestand_an" = "Park")))
        }


    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

    # return(gplot)

}



#' Generate overview of records (bar plot)
#'
#' @param sf_data sf-tibble of Berlin City trees
#' @param file character, file path (use with \code{\link{file_out}}), must include file ending
#' @param height numeric, height in inches
#' @param width numeric, width in inches
#' @param dpi numeric, dpi of output
#'
#' @return ggplot bar graph
#' @export
#'
#' @import ggplot2
tree_sums_bar_plot <- function(sf_data,
                               base_size = 18,
                               file,
                               height,
                               width,
                               dpi){

    # extrafont::loadfonts("win", quiet = TRUE)
    extrafont::loadfonts("pdf", quiet = TRUE)


    gplot <- sf_data %>%
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
             #
             #              caption = paste0("Data source: daten.berlin.de; WFS Service, accessed: ",
             #                               Sys.Date()),
             y = "Count",
             x = "Genus",
             fill = NULL) +


        theme_minimal(base_size = base_size, base_family = "Roboto Condensed") +
        theme(legend.position = c(0.8,0.5),
              axis.text.y = element_text(face = "italic")) +


        ggplot2::scale_fill_brewer(type = "qual",
                                   palette = "Set2",
                                   direction = +1,
                                   labels = c("Riparian",
                                              "Street",
                                              "Park"))


    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

}




#' Spatially-binned tree counts
#'
#' @param sf_data sf-tibble of Berlin City trees
#' @param poly sf-tibble of Berlin Districts
#' @param file character, file path (use with \code{\link{file_out}}), must include file ending
#' @param height numeric, height in inches
#' @param width numeric, width in inches
#' @param dpi numeric, dpi of output
#'
#' @return ggplot map
#' @export
#'
#' @import ggplot2
tree_count_map <- function(sf_data,
                           poly,
                           base_size = 18,
                           file,
                           height,
                           width,
                           dpi){


    # extrafont::loadfonts("win", quiet = TRUE)
    extrafont::loadfonts("pdf", quiet = TRUE)


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
            guides(fill = guide_colorbar(barwidth = grid::unit(6, units = "cm"),
                                         barheight = grid::unit(.45, units = "cm"),
                                         ticks.colour = "gray80")) +


            theme_minimal(base_size = base_size, base_family = "Roboto Condensed") +

            theme(axis.title = element_blank(),
                  strip.text = element_text(face = c("bold.italic")),
                  legend.position = c(0.6, 0.11),
                  legend.direction = "horizontal",
                  legend.title = element_text(vjust = 1))

    }

    ggplot2::ggsave(filename = file,
                    plot = sf_plot,
                    dpi = dpi,
                    height = height,
                    width = width)

    # return(sf_plot)


}





#' Plot UHI with Berlin districts
#'
#' @param uhi_stacks Rasterstack lists for UHI
#' @param berlin_poly Berlin District Polygon
#' @param base_size Numeric, base char size for ggplot
#' @param file Character, output file path
#' @param height Numeric, for plot output (cm)
#' @param width Numeric, for plot output (cm)
#' @param dpi Numeric
#' @import raster
#'
#' @return
#' @export
#'
make_uhi_plot <- function(uhi_stacks,
                          berlin_poly,
                          base_size = 18,
                          file,
                          height,
                          width,
                          dpi){

    # extrafont::loadfonts(device = "win",quiet = TRUE)
    extrafont::loadfonts(device = "pdf",quiet = TRUE)


    berlin_poly <- sf::st_transform(berlin_poly,
                                    crs = raster::crs(uhi_stacks$Summertime_gridded_UHI_data$day))

    mid_rescaler <- function(mid = 0) {
        function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
            scales::rescale_mid(x, to, from, mid)
        }
    }


    p <- ggplot2::ggplot() +


        ggplot2::geom_sf(data = berlin_poly,
                         color = "gray60",
                         fill = "gray80",
                         size = 1,
                         show.legend = FALSE) +


        stars::geom_stars(data = stars::st_as_stars(uhi_stacks$Summertime_gridded_UHI_data$day$day_2007),
                          na.rm = TRUE) +


        ggplot2::geom_sf(data = berlin_poly,
                         color = "gray60",
                         fill = "transparent",
                         size = 1,
                         show.legend = FALSE) +

        # scale_fill_viridis_c(na.value = "transparent") +
        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      rescaler = mid_rescaler(),
                                      na.value = "transparent")   +

        ggplot2::labs(fill = expression(atop(Summer~day-time,
                                             UHI~(degree*C))),
                      x = NULL,
                      # title = "Estimate of Urban Heat Loading",
                      y = NULL) +


        ggplot2::theme_minimal(base_family = "Roboto Condensed",
                               base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = guide_colorbar(barwidth = unit(3.5, "cm"),
                                              title.vjust = 1,
                                              ticks.colour = "gray30"))


    ggplot2::ggsave(filename = file,
                    plot = p,
                    dpi = dpi,
                    height = height,
                    width = width)


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
#' @param file character, file path (use with \code{\link{file_out}}), must include file ending
#' @param height numeric, height in inches
#' @param width numeric, width in inches
#' @param dpi numeric, dpi of output
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
                            xmax=-0.5,
                            base_size = 18,
                            file,
                            width,
                            height,
                            dpi){

    # extrafont::loadfonts(device = "win",quiet = TRUE)
    extrafont::loadfonts(device = "pdf",quiet = TRUE)


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
        scale_color_brewer(type = "qual",
                           palette = "Set2",
                           labels = c("Riparian",
                                      "Street",
                                      "Park"),
                           direction = +1) +
        scale_fill_brewer(type = "qual",
                          palette = "Set2",
                          labels = c("Riparian",
                                     "Street",
                                     "Park"),
                          direction = +1) +

        # theming
        guides(color = FALSE) +
        theme_bw(base_size = base_size, base_family = "Roboto Condensed") +
        theme(legend.position = c(0.6, 0.1),
              legend.direction = "horizontal",
              strip.text = element_text(face = "italic")) +
        labs(x = expression(Summer~day-time~UHI~magnitude~(degree*C)), y = "Density", fill = "Type")






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
                                            scale_fill_brewer(type = "qual", palette = "Set2",
                                                              labels = c("Riparian",
                                                                         "Street",
                                                                         "Park")) +


                                            scale_y_continuous(breaks = c(0, 150000),
                                                               labels = c("0", "150k")) +


                                            labs(fill = NULL) +
                                            theme_minimal(base_size = 11, base_family = "Roboto Condensed") +
                                            theme(axis.text.y = element_blank(),
                                                  axis.title = element_blank(),
                                                  plot.margin = margin(),
                                                  panel.grid = element_blank(),
                                                  axis.ticks.length = unit(2, "mm"),
                                                  axis.ticks = element_line(color = "gray10"),
                                                  axis.ticks.y = element_blank(),
                                                  strip.text = element_text(face = "italic"))
                                    ),


                                    data = data.frame(gattung_short=gattung),
                                    ymin = ymin, ymax=ymax, xmin=xmin, xmax=xmax)






                            })

    gplot <- dens_plot + bar_plots


    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

    # return(dens_plot + bar_plots)


}





#' Make Random-effects effect-size plot
#'
#' @param model_out output list with models
#' @param model_name Character, model name
#' @param df data.frame with all trees
#' @param n_top_species Numeric, max number of species to inspect
#' @param file character, file path (use with \code{\link{file_out}}), must include file ending
#' @param height numeric, height in inches
#' @param width numeric, width in inches
#' @param dpi numeric, dpi of output
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
                            n_top_species = 6,
                            base_size = 18,
                            file,
                            height,
                            width,
                            dpi){



    # extrafont::loadfonts(device = "win", quiet = TRUE)
    extrafont::loadfonts(device = "pdf", quiet = TRUE)


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


        ggplot2::theme_bw(base_size = base_size, base_family = "Roboto Condensed") +
        ggplot2::theme(panel.border = element_rect(fill = "transparent"),
                       strip.text = element_text(face = "italic"),
                       legend.position = c(0.9,0.11)) +

        ggplot2::scale_color_brewer(type = "qual",
                                    palette = "Set2",
                                    direction = +1,
                                    labels = c("Street",
                                               "Park")) +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(~gattung, scales = "free_y") +

        ggplot2::scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) +

        ggplot2::labs(y = expression(Effect~Size~(cm/degree*C)),
                      x = NULL,
                      color = NULL)

    ggplot2::ggsave(filename = file,
                    plot = p,
                    dpi = dpi,
                    height = height,
                    width = width)


    # return(p)



}


# Tables ------------------------------------------------------------------

#' Make overview table
#'
#' @param df berlin trees df
#'
#' @return data frame ready for kabling
#' @export
#' @import data.table
#'
make_overview_table <- function(df){

    full_data_set_clean <- data.table(df)
    counts_full <- full_data_set_clean[,.N, by = provenance]
    # counts_with_age_size <- full_data_set_clean[!is.na(dbh_cm) & !is.na(STANDALTER),
    #                                             .N,
    #                                             by = provenance]



    counts_with_age_size <- full_data_set_clean[,full := {!is.na(STANDALTER) &
            STANDALTER > 0 &
            !is.na(dbh_cm)}]


    counts_with_age_size <- counts_with_age_size[ , .(n_available = sum(full)), by = provenance]




    counts_full <- cbind(counts_full, counts_with_age_size[,2])
    counts_full[,1] <- c("Park", "Street", "Riparian")
    colnames(counts_full) <- c("Category", "n", "n$_{full}$")
    counts_full[order(-n)]

    return(counts_full)
}



#' Generate table of genera age distribution
#'
#' @param df Berlin trees data frame
#' @param max_age numeric, last cut
#' @param break_interval numeric, size of intervals, note, max_age/break_intervals must be an integer.
#'
#' @return
#' @export
#'
#' @import data.table
#'
make_age_table <- function(df, max_age = 120, break_interval = 20){


    # make df with age breaks
    age_df <- data.table(df)[ , .(STANDALTER, gattung_short)][
        , age_group := cut(STANDALTER, breaks = c(seq(0, max_age, break_interval), max(STANDALTER, na.rm = TRUE)), right = TRUE)]

    species_totals <- age_df[ ,.(n_total = .N), by = gattung_short]

    cols <- c(1,3:(4+ max_age/break_interval),2)


    age_df_class <- dcast(age_df,
                          gattung_short ~ age_group,
                          value.var = "STANDALTER",
                          fun.aggregate = length)[species_totals,
                                                  on = "gattung_short"][ ,..cols]

    setnames(age_df_class, old = "NA", new = "missing")




    # age_df_perc <- age_df_class[, 2:8 ]/species_totals[,n_total]
    # age_df_perc[, gattung_short := as.character(age_df$gattung_short)]

    return(age_df_class)

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
                      breaks = c(seq(from = 0,
                                     to = nrow(dframe),
                                     by = cut_size),
                                 nrow(dframe) +1))
    # make lists
    dframe_lists <- base::split(dframe, f = as.factor(cuts))

    return(dframe_lists)


}


#' add prefix to object names
#'
#' @param x df or vector
#' @param prefix character, prefix to add, always uses "_" as seperator
#'
#' @return x, with adjusted names
#'
prefix_names <- function(x, prefix){

    if(is.null(names(x))){

        return(x)
    }

    # handle sf columns
    if(rlang::inherits_any(x, "sf")){

        geometry_col <- which(names(x) == "geometry")
        names(x)[-geometry_col] <- paste0(prefix, "_", names(x)[-geometry_col])

    } else {
        names(x) <- paste0(prefix, "_", names(x))

    }



    return(x)
}


