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

    building_full_raster <- raster::raster(building_full, resolution=5)

    # building_rasterized <- raster::rasterize(building_full,
    #                                          building_full_raster,
    #                                          field = "mean_ndom")

    building_rasterized <- fasterize::fasterize(building_full,
                         building_full_raster,
                         field = "mean_ndom",
                         fun = "max")




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

#' Download Soil Nutrient data and classification for Berlin
#'
#' swertstu is the "Cumulative S-Value", used to determine cation exchange capacity
#'
#' @param path character, path to save file (with .geojson extension)
#'
#' @return sf dframe
download_soil_nutrients <- function(path = "./analysis/data/raw_data/spatial_ancillary/soil_nutrients.geojson"){


    # soil nutrient classes 1 - 3
    # wfs_soil <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_boden_wfs3_2015"
    # query <- list(service = "WFS",
    #               request = "GetFeature",
    #               version = "2.0.0",
    #               TypeNames = "fis:s_boden_wfs3_2015",
    #               # count = 10,
    #               outputFormat = 'application/geo+json')
    #
    # out <- httr::GET(wfs_soil,
    #           query = query)
    #
    #
    # # request data via GET (REST API and save to disk)
    # soil_sf <- sf::st_read(out,
    #                        quiet = TRUE)
    #
    #



    # cation exchange capacity 1-10
    wfs_soil_cat <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_boden_wfs2_2015"
    query <- list(service = "WFS",
                  request = "GetFeature",
                  version = "2.0.0",
                  TypeNames = "fis:s_boden_wfs2_2015",
                  # count = 10,
                  outputFormat = 'application/geo+json')

    out <- httr::GET(wfs_soil_cat,
                     query = query)


    # request data via GET (REST API and save to disk)
    soil_cat_sf <- sf::st_read(out,
                               quiet = TRUE)



    # return(list(soil_sf, soil_cat_sf))
    return(soil_cat_sf)
}


#' Download Klimamodell 2015 heat data for Berlin
#'
#' Heat data (PET, 2 m..) at multiple time points from a climate model
#' @return heats raster dframe
download_heat_data <- function(){



    wfs_heat_area <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_0410_analysedaten2015"
    wfs_heat_street <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_0410_analysedaten_strasse_2015"

    wfss <- list(block = wfs_heat_area, street = wfs_heat_street)

    # cation exchange capacity 1-10


    out <- lapply(wfss,
                  function(x){

                      q <- list(service = "WFS",
                                request = "GetFeature",
                                version = "2.0.0",
                                TypeNames = sprintf("fis:%s", fs::path_file(x)),
                                outputFormat = 'application/geo+json')

                      return(httr::GET(x,
                                       query = q))
                  })





    sfs <- lapply(out, sf::st_read, quiet = TRUE)


    sfs[[1]] <- sfs[[1]] %>%
        mutate_at(.vars = 2:16,
                  .funs = function(x){as.numeric(as.character(x))}) %>%
        mutate_at(.vars = 2:16,
                  .funs = function(x){ifelse(x == 0, NA, x)})
    sfs[[2]] <- sfs[[2]] %>%
        mutate_at(.vars = 2:13,
                  .funs = function(x){as.numeric(as.character(x))}) %>%
        mutate_at(.vars = 2:13,
                  .funs = function(x){ifelse(x == 0, NA, x)})
    names(sfs) <- names(wfss)

    sfs <- purrr::map2(sfs, names(sfs),
                       function(x, y){
                           x$type <- y
                           return(x)
                       })

    sfs <- bind_rows(sfs)
    sfs_raster <- raster::raster(sfs, resolution = 5)

    fields <- c("T2M04HMEA",  "T2M14HMEA",  "T2M22HMEA")

    heat_rasterized <- lapply(fields,
                              function(x){
                                  fasterize::fasterize(sfs,
                                                       sfs_raster,
                                                       field = x,
                                                       fun = "max")
                              }) %>%
        setNames(nm = fields)
    heat_rasterized <- raster::stack(heat_rasterized)

    return(heat_rasterized)
}




#' Download UrbClim data from ecmwf CDS
#'
#' Download and unzip data set, file format is .zip/.nc;
#' creates a directory "unzipped" in `path_dir`, sets correct CRS
#' and averages rasterlayers across hour_av elements.
#' Note, function creates and writes a raster brick into the unzipped folder.
#'
#' @param month character, defaults to 06
#' @param year character, 2008 - 2017, defaults to 2010
#' @param var character, defaults to 'air_temperature'
#' @param path_dir character, folder to download and unzip directory
#' @param hour_av named list, with day-hour ranges for averaging rasterlayers (e.g. `list(afternoon = c(13,15))`)
#'
#' @source https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-urban-climate-cities
#'
#' @return rasterStack of UrbClim data
download_urbclim_uhi <- function(month = "06", year = "2010", var = 'air_temperature', path_dir,
                                 hour_av = list(morning = c(3,5), afternoon = c(13,15), night = c(21,23))){

    if(!dir.exists(path_dir)){
        dir.create(path_dir)
        cat(sprintf("Created download dir in %s \n", path_dir))
    }

    if(!nchar(month) == 2 &&
       !class(month) == "character"){
        stop("Provide month as a character string with length 0; e.g. '01' for January")
    }

    if(!nchar(year) == 4 &&
       !class(month) == "character"){
        stop("Provide year as a character string with length 4; e.g. '2012'")
    }


    #

    fname <- sprintf("berlin_%s_%s_%s.zip", year, month, var)

    request <- list(
        format = "zip",
        variable = var,
        city = "berlin",
        year = year,
        month = month,
        dataset_short_name = "sis-urban-climate-cities",
        target = fname
    )

    # options(keyring_backend = "file")
    # set a key to the keychain

    options(keyring_backend = "file")
    # If you have stored your user login information
    # in the keyring by calling cds_set_key you can


    if(!file.exists( normalizePath(file.path(path_dir, fname)))){


        # call:
        file <- ecmwfr::wf_request(
            user     = "92658",   # user ID (for authentification)
            request  = request,  # the request
            transfer = TRUE,     # download the file
            path     = path_dir)      # store data in current working directory
    } else {
        cat(sprintf("File %s already exists", fname))
    }


    unzip_dir <- normalizePath(file.path(path_dir, "unzipped"))

    if(!dir.exists(unzip_dir)){
        dir.create(unzip_dir)
    }
    unzip(normalizePath(file.path(path_dir, fname)), exdir = unzip_dir)

    path_file <- normalizePath(list.files(unzip_dir, pattern = "nc", full.names = TRUE)[grep(paste(year, month, sep = "_"), list.files(unzip_dir, pattern = "nc"))])

    out <- raster::brick(path_file)
    raster::crs(out) <- "+init=EPSG:3035"


    raster_mean_across_hours <- function(heat_raster, hour_av){

        hour_idxs <- lapply(hour_av,
                            function(x){
                                out <- which(dplyr::between(as.numeric(format(as.POSIXct(heat_raster@z[[1]]), "%H")),
                                                            x[1], x[2]))

                                return(out)

                            })


        raster_mean_hours <- lapply(seq_along(hour_idxs),
                                    function(x){
                                        out <- raster::calc(subset(heat_raster, hour_idxs[[x]]), mean) - 273.15
                                        return(raster::stack(out))
                                    }
        )

        return(raster_mean_hours)
    }

    out <- raster_mean_across_hours(heat_raster = out,
                                    hour_av = hour_av)

    out <- raster::stack(out)



    raster::writeRaster(x = out,
                        filename = fs::path(fs::path_ext_remove(path_file),ext = "grd"),
                        overwrite = TRUE)

    out <- raster::stack(fs::path(fs::path_ext_remove(path_file),ext = "grd"))

    # saveRDS(object = out,
    #         file = fs::path(fs::path_ext_remove(path_file),ext = "Rds"))

    cat(sprintf("raster written to %s \n\n", fs::path(fs::path_ext_remove(path_file),ext = "grd")))

    out <- raster::unstack(out)
    names(out) <- paste(names(hour_av),
                        sapply(hour_av, paste, collapse = "_"),
                        sep = "_")
    return(out)

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




#' Download  Berlin Land use to green, water and urban
#'
#' @param path character, path to read file from
#'
#' @return sf-tibble with polygons of Berlin landuse
#' @import httr
#' dplyr
#'
download_berlin_lu <- function(path = "./analysis/data/raw_data/spatial_ancillary/berlin_landuse.geojson"){

    # blu <- sf::st_read(path)

    wfs_lu <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/sach_nutz2015_nutzsa"


    query <- list(service = "WFS",
                  request = "GetFeature",
                  version = "2.0.0",
                  TypeNames = "fis:sach_nutz2015_nutzsa",
                  # count = 10,
                  outputFormat = 'application/geo+json')

    # request data via GET (REST API and save to disk)

    httr::GET(wfs_lu,
              query = query,
              httr::write_disk(path = path, overwrite = TRUE)

    )



    return(path)
}

#' Download Precip and Temp from KNMI Climate Explorer
#'
#' This downloads all station data (>1750 for temp, >1950 for precip) from Berlin Tegel
#'
#' @return tibble with climate data
download_berlin_climate_data <- function(){

    berlin_precip <- read.table(file = "https://climexp.knmi.nl/data/pa10384.dat") %>%
        setNames(c("year", month.abb)) %>%
        tidyr::pivot_longer(cols = -year, names_to = "month", values_to = "vals")

    berlin_mean_temp <- read.table(file = "https://climexp.knmi.nl/data/ta10384.dat") %>%
        setNames(c("year", month.abb)) %>%
        tidyr::pivot_longer(cols = -year, names_to = "month", values_to = "vals")

    berlin_climate <- dplyr::left_join(berlin_precip, berlin_mean_temp, by = c("year", "month"), suffix = c("_prec", "_temp"))

    return(berlin_climate)
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
#' @return a df of baumscheiben data: "baumsch_dist_m",
#'  "baumsch_elem_nr",
#'  "baumsch_gis_id",
#'   "baumsch_flaeche_m2"
#' @export
#'
#' @import sf
#' units
#'
prep_baumscheiben_flaeche <- function(fulldf, bms, max_dist_m = 10){

    nearest_idx <- sf::st_nearest_feature(fulldf, bms)

    # calculate pairwise distances
    baumsch_dist_m <- sf::st_distance(fulldf, bms[nearest_idx, ], by_element = TRUE)

    # bms <- sf::st_drop_geometry(bms[nearest_idx, c("elem_nr", "gis_id", "flaeche"), ])
    bms <- bms[nearest_idx, c("elem_nr", "gis_id", "flaeche", "geometry")]
    names(bms) <- c("baumsch_elem_nr", "baumsch_gis_id", "baumsch_flaeche_m2", "geometry")

    # bms$baumsch_flaeche_m2 <- ifelse(baumsch_dist_m <= units::set_units(max_dist_m, m),
    #                                  bms$baumsch_flaeche_m2,
    #                                  NA)

    bms[baumsch_dist_m <= units::set_units(max_dist_m, m), ] <- NA
    bms$baumsch_flaeche_m2 <- as.numeric(bms$baumsch_flaeche_m2)
    #
    # # join manually
    # fulldf <- cbind(fulldf,
    #                 bms)
    #
    # fulldf$baumsch_flaeche_m2 <- ifelse(fulldf$baumsch_dist <= units::set_units(max_dist_m, m),
    #                                     fulldf$baumsch_flaeche_m2,
    #                                     NA)

    return(bms)

}


#' Prepare Soil Type for each Location
#'
#' @param fulldf sf_df, all berlin trees
#' @param sty sf_df, Baumscheiben polygons and their area
#' @param max_dist_m numeric, how far from polygon centroid can tree lie and still be assigned?
#'
#' @return a df of sty ordrered / subsetted for closest tree (following fulldf)
#' @export
#'
#' @import sf
#' units
#'
prep_soil_type <- function(fulldf, sty, max_dist_m = 50){


    nearest_idx <- sf::st_nearest_feature(fulldf, sty)

    # calculate pairwise distances
    soil_poly_dist <- sf::st_distance(fulldf, sty[nearest_idx, ], by_element = TRUE)

    # sty <- sf::st_drop_geometry(sty[nearest_idx, ])
    sty <- sty[nearest_idx, ]
    # names(sty) <- c("baumsch_elem_nr", "baumsch_gis_id", "baumsch_flaeche_m2")

    sty[soil_poly_dist > units::set_units(max_dist_m, m), ] <- NA
    #
    # # join manually
    # fulldf <- cbind(fulldf,
    #                 sty)
    #
    # fulldf$baumsch_flaeche_m2 <- ifelse(fulldf$baumsch_dist <= units::set_units(max_dist_m, m),
    #                                     fulldf$baumsch_flaeche_m2,
    #                                     NA)

    return(sty)
}


#' Prepare Soil Nutrients for each Location
#'
#' @param fulldf sf_df, all berlin trees
#' @param nuts sf_df, Baumscheiben polygons and their area
#' @param max_dist_m numeric, how far from polygon centroid can tree lie and still be assigned?
#'
#' @return a df of nuts ordrered / subsetted for closest tree (following fulldf)
#' @export
#'
#' @import sf
#' units
#'
prep_soil_nutrients <- function(fulldf, nuts, max_dist_m = 50){


    nearest_idx <- sf::st_nearest_feature(fulldf, nuts)

    # calculate pairwise distances
    soil_poly_dist <- sf::st_distance(fulldf, nuts[nearest_idx, ], by_element = TRUE)

    # nuts <- sf::st_drop_geometry(nuts[nearest_idx, ])
    nuts <- nuts[nearest_idx, ]
    # names(nuts) <- c("baumsch_elem_nr", "baumsch_gis_id", "baumsch_flaeche_m2")

    nuts[soil_poly_dist > units::set_units(max_dist_m, m), ] <- NA
    #
    # # join manually
    # fulldf <- cbind(fulldf,
    #                 nuts)
    #
    # fulldf$baumsch_flaeche_m2 <- ifelse(fulldf$baumsch_dist <= units::set_units(max_dist_m, m),
    #                                     fulldf$baumsch_flaeche_m2,
    #                                     NA)

    return(nuts)
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


# Cleaning -------------------------------------------

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


                      bezirk_num = as.numeric(as.factor(BEZIRK)),
                      BEZIRK = as.factor(BEZIRK)) %>%
        dplyr::rename(provenance = PROVENANCE)

    return(sf_data)

}




#' Split data sets and save to file
#'
#' Datasets are split by district and saved to individual files of form
#' \code{berlin_trees_subset-GENUS.Rds}
#'
#' @param sfdf data.frame, full berlin tree data set
#' @param save_dir character, path to save folder.
#'
#' @return nothing
#' @export
split_df <- function(sfdf, save_dir = "./analysis/data/raw_data/tree_splits"){
    # make a unique id for re-joining at later stage
    sfdf$berlin_id <- seq_len(nrow(sfdf))

    # make "backup" geometry data set - needs unique id and geometry column
    sfdf_geometry <- sfdf[ , c("berlin_id", "geometry")]

    # drop geometry from df
    # sdfdf <- sf::st_drop_geometry(sfdf)
    sfdf <- sfdf %>%
        dplyr::select(-geometry)

    # split by district + gattung_short
    splits <- split(sfdf, f = list(sfdf$gattung_short))

    # save_dir <- "./analysis/data/raw_data/tree_splits"

    # save into individual files with reasonable naming structure
    if(!fs::dir_exists(save_dir)){
        fs::dir_create(save_dir)
    }

    purrr::iwalk(splits,
                 ~saveRDS(.x,
                          file = fs::path(
                              save_dir,
                              paste0(
                                  "berlin_trees_subset_",
                                  gsub("[-]",
                                       "_",
                                       .y,)),
                              ext = "RDS")))


    saveRDS(sfdf_geometry,
            file = fs::path(save_dir, "berlin_trees_GEOBACKUP", ext = "RDS"))

}





#' Join dcr cleaned data together and sanity check
#'
#' @param origdf full df
#' @param georecover_path path to RDS of georecover file
#' @param cleaned_path folder path to cleaned data
#'
#' @return cleaned and joined df
#' @export
combine_split_clean_data <- function(
    origdf,
    georecover_path = "./analysis/data/raw_data/tree_splits/berlin_trees_GEOBACKUP.RDS",
    cleaned_path = "./analysis/data/raw_data/tree_splits/"
){


    geo_recover <- readRDS(georecover_path)

    # make id for sanity check
    origdf <- cbind(origdf, berlin_id = geo_recover$berlin_id) %>%
        tidyr::unite("old_id", KENNZEICH, GISID)

    split_files <- list.files(path = cleaned_path,
                              pattern = "cleaned.Rds$",
                              full.names = TRUE)


    cleaned_data_join <- lapply(split_files,
                                readRDS) %>%
        dplyr::bind_rows() %>%
        dplyr::ungroup() %>%
        tidyr::unite("check_id", KENNZEICH, GISID) %>%
        dplyr::filter(is.na(.annotation)) %>%
        dplyr::select(berlin_id, check_id) %>%
        dplyr::left_join(origdf, by = "berlin_id") %>%
        tidyr::drop_na(STANDALTER, dbh_cm) %>%
        sf::st_set_geometry(.$geometry)

    cleaned_data_join <- cbind(cleaned_data_join, sf::st_coordinates(cleaned_data_join))


    if(
        !identical(
            cleaned_data_join$check_id,
            cleaned_data_join$old_id,
        )
    ){
        stop("Something went wrong while merging the cleaned data. Please check")
    }


    return(cleaned_data_join)
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



#' Derive CORINE Urban-Rural Mask
#'
#' @param path character, path to CORINE high-level zip
#' @param berlin_bbox sf df, bounding box of greater berlin area
#'
#' @details See https://datastore.copernicus-climate.eu/documents/sis-european-health/C3S_422_Lot2.VITO.3.1_201909_Demo_Web_Application_URBAN.1_v4.pdf
#' for calculation of UHI
#'
#' @return list with adjusted CLC raster, original land cover legend, and value table
make_corine_urban_rural_mask <- function(path, berlin_bbox){

    # nested zip files..
    unzip(path, exdir = fs::path_dir(path))

    path_nested_zip <- list.files(fs::path_dir(path), pattern = "clc2018.*zip$", full.names = TRUE, include.dirs = FALSE)

    unzip(zipfile = path_nested_zip, exdir = fs::path_dir(path))

    # read tif
    path_data <- list.files(fs::path(fs::path_ext_remove(path_nested_zip), "DATA"),
                            pattern = "tif$",
                            full.names = TRUE)


    # meta data

    path_desc <- list.files(
        fs::path(
            fs::path_ext_remove(
                path_nested_zip),
            "Legend"),
        pattern = "txt$",
        full.names = TRUE)


    clc_desc <- read.csv(path_desc, header = FALSE, stringsAsFactors = FALSE) %>%
        setNames(nm = c("class", "r", "g", "b", "alpha", "name")) %>%
        dplyr::mutate(class = as.factor(class))

    clc_rast <- raster::raster(path_data)

    # ensure same projection
    berlin_bbox <- sf::st_transform(berlin_bbox, crs = raster::crs(clc_rast))


    clc_rast <- raster::crop(clc_rast, berlin_bbox)

    clc_rast[clc_rast >= 37] <- 100 # water
    clc_rast[clc_rast < 37 & clc_rast > 21] <- 50 # natural
    clc_rast[clc_rast <= 21 & clc_rast > 11] <- 25 # agricultural
    clc_rast[clc_rast <= 11] <- 1 # urban



    # rural land classes:
    # CORINE covering grassland, cropland, shrubland, woodland, broadleaf forest and needleleaf forest

    return(list(clc = clc_rast,
                clc_legend = clc_desc,
                clc_legend_adjust = c(water = 100, natural = 50, agricultural = 25, urban = 1)))
}

#' Calculate UHI for Berlin based on CLC land cover and UrbClim data
#'
#' @param urbclim raster(layer) of (averaged) urbclim data
#' @param clc named list, contains clc, clc_legend and clc_legend_adjust
#'
#' @returns UHI raster
#'
#' @examples
calc_urbclim_uhi_with_corine <- function(urbclim, clc, natural_cover_val = 50, make_plot = FALSE){



    clc_crop <- crop(resample(clc, urbclim[[1]]), urbclim[[1]])

    if(make_plot){
        raster::plot(mask(stack(urbclim), clc_crop == natural_cover_val, maskvalue = 0))
    }

    urbclim_uhi <- lapply(urbclim,
                          function(x){

                              mean_temp_natural <- cellStats(mask(x, clc_crop == natural_cover_val, maskvalue = 0), mean)

                              urbclim_uhi <- x - mean_temp_natural

                          }) %>%
        setNames(nm = names(urbclim))



    return(urbclim_uhi)
}

#' Add UHI data from RasterLayer stack to sf data frame
#'
#' @param uhi_stack_list List, nested on time of year, time of day
#' @param sf_data sf points, data frame with Berlin tree locations and meta data
#' @param buff_dist numeric, buffer distance for data extraction
#' @param method character, should be `simple`
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
add_uhi_hist_data <- function(
    uhi_stack_list,
    sf_data,
    buff_dist = 100,
    method = "simple"){


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


                                      function(x){
                                          raster::extract(x,
                                                          sf_data,
                                                          method = method,
                                                          buffer = buff_dist,
                                                          fun = mean,
                                                          na.rm = TRUE,
                                                          df = FALSE,
                                                          sp = FALSE,
                                                          factors = FALSE)

                                      })
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


#' Extract values from raster based on (point) sf
#'
#' @param sf_data sf dframe, e.g. berlin trees
#' @param heat_raster, raster, berlin klimamodell temps (or other)
#' @param buff_dist, buffer around points / polygons
#' @param method character, "simple" (for categorical) or "bilinear"
#'
#' @return dframe, 1 row per sf geometry, with proportions of coverage
#' @export
assess_mean_temps <- function(sf_data, heat_raster, buff_dist = 100, method = "simple"){

    # ensure both objects have same projection

    if(!sf::st_crs(sf_data)$wkt ==
       raster::wkt(heat_raster)){

        sf_data <- sf::st_transform(sf_data,
                                    crs = raster::crs(heat_raster))
        message("adjusted CRS")
    }


    extracted_vals <- raster::extract(heat_raster,
                                      sf_data,
                                      method = method,
                                      buffer = buff_dist,
                                      fun = mean,
                                      na.rm = TRUE,
                                      df = FALSE,
                                      factors = FALSE)
    return(extracted_vals)

}


#' Extract values from raster based on (point) sf
#'
#' @param sf_data sf dframe, e.g. berlin trees
#' @param heat_raster, list with rasters, berlin urbclim data temps (or other)
#' @param buff_dist, buffer around points / polygons
#' @param method character, "simple" (for categorical) or "bilinear"
#'
#' @return dframe/matrix, 1 row per sf geometry, with proportions of coverage
assess_mean_temps_urbclim <- function(sf_data,
                                      heat_raster,
                                      buff_dist = 100,
                                      method = "simple"){

    # ensure both objects have same projection

    if(!sf::st_crs(sf_data)$wkt ==
       raster::wkt(heat_raster[[1]])){

        # sf_data <- sf::st_transform(sf_data,
        # crs = raster::crs(heat_raster[[1]]))
        heat_raster <- purrr::modify(heat_raster,
                                     ~raster::projectRaster(
                                         from = .x,
                                         crs = sf::st_crs(sf_data)$wkt))

        message("adjusted CRS")
    }

    # raster_mean_across_hours <- function(heat_raster, hour_av){
    #
    #     hour_idxs <- lapply(hour_av,
    #                         function(x){
    #                             out <- which(dplyr::between(as.numeric(format(as.POSIXct(heat_raster@z[[1]]), "%H")),
    #                                                         x[1], x[2]))
    #
    #                             return(out)
    #
    #                         })
    #
    #
    #     raster_mean_hours <- lapply(seq_along(hour_idxs),
    #                                 function(x){
    #                                     out <- raster::calc(subset(heat_raster, hour_idxs[[x]]), mean)
    #                                     return(out)
    #                                 }
    #     )
    #
    #     return(raster_mean_hours)
    # }
    #
    # heat_raster_means <- raster_mean_across_hours(heat_raster = heat_raster,
    #                                               hour_av = hour_av)



    extracted_vals <- sapply(heat_raster,
                             function(x){

                                 raster::extract(x,
                                                 sf_data,
                                                 method = method,
                                                 buffer = buff_dist,
                                                 fun = mean,
                                                 na.rm = TRUE,
                                                 df = FALSE,
                                                 factors = FALSE)})

    colnames(extracted_vals) <- names(heat_raster)

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
                                    extract_uhi$Summertime_gridded_UHI_data$day,
                                    extract_uhi$Summertime_gridded_UHI_data$night),

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


#' Prep model data frame
#'
#' Defines df with top species by abundance, max age and the median absolute
#' deviance score used to remove gross outliers.
#'
#' @param dset
#' @param model_params
#' @param plot
#' @param path
#' @param stat_filter logical, drop observations outside of median absolute dev threshold; uses `model_params$mad_factor`
#'
#' @return dset filtered according to the model params and has two additional columns.
#' One of these is `mad_select`, (logical), which can be used to subset the df.
#' @export
#'
#' @examples
prep_model_df <- function(dset,
                          model_params,
                          plot = TRUE,
                          stat_filter,
                          path = "./analysis/figures/diagnostic_01_model_data.png"){


    dset <- sf::st_drop_geometry(dset)
    # %>%
        # mutate(species_corrected = as.factor(gsub("Tilia intermedia.*", "Tilia intermedia", x = species_corrected)))

    #
    top_species <- dset %>%
        dplyr::group_by(species_corrected) %>%
        dplyr::tally(sort = TRUE) %>%
        dplyr::top_n(model_params$n_max_species) %>%
        dplyr::filter(n > model_params$n_min_abundance)



    df_nest <- dset %>%
        dplyr::filter(species_corrected %in% top_species$species_corrected,
                      STANDALTER <= model_params$age_cutoff,
                      dbh_cm <= model_params$dbh_cutoff) %>%
        tidyr::nest(cols = -species_corrected) %>%
        dplyr::mutate(
            mod = purrr::map(
                cols,
                function(df){
                    glm(dbh_cm ~ STANDALTER, family = Gamma(link = "identity"), data = df)
                }),
            cols = purrr::modify2(
                cols,
                mod,
                ~dplyr::mutate(.x,
                               diag_fitted_dat = predict(.y, .x, type = "response"),
                               diag_resid_val = resid(.y),
                               diag_mad_cutoff = +1 * model_params$mad_factor * mad(resid(.y)),
                               diag_mad_select = abs(diag_resid_val) <= diag_mad_cutoff
                )
            )
        ) %>%
        dplyr::select(-mod) %>%
        tidyr::unnest(cols = cols)


    if(plot == TRUE){



        p <- df_nest %>%
            dplyr::arrange(desc(diag_mad_select)) %>%
            ggplot(aes(x = STANDALTER,
                       y = dbh_cm,
                       color = diag_mad_select,
                       group = species_corrected)) +
            geom_point() +
            geom_point(aes(y = diag_fitted_dat), col = "pink", size = 0.5) +
            labs(color = "Retained point",
                 subtitle = sprintf("dbh ~ STANDALTER^1; MAD Cutoff factor +/- %i x MAD", model_params$mad_factor)) +
            # geom_line(color = "pink", aes(group = species_corrected)) +
            facet_wrap(~species_corrected, scales = "free")

        if(file.exists(path)){
            cat("Plot already exists. Overwriting. \n\n")
        }


        ggsave(filename = path,
               plot = p,
               width = 35,
               height = 30,
               units = "cm")

        if(file.exists(path)){
            cat(sprintf("Created diagnostic plot successfully in %s \n\n", path))
        }

    }

    df_nest$species_corrected <- as.factor(df_nest$species_corrected)


    if(stat_filter){
        df_nest <- df_nest[df_nest$diag_mad_select, ]



    }

    return(df_nest)

}





#' Apply GAM models across pre-defined grid (formula, family)
#'
#' Take care to manage overwrite properly - if model grid changes, might require re-running with "overwrite" set to TRUE
#'
#' @param model_grid df, predefined model grid from `make_model_grid()`
#' @param dat df to run with model
#' @param path character, output path
#' @param overwrite logical, defaults to FALSE, i.e. don't overwrite existing models
#' @param n_cl numeric, number of clusters for parallel run, defaults to `NULL`
#'
#' @return df with status overview
#' @export
#'
#' @examples
apply_gam_mod <- function(model_grid, dat, path = "./analysis/data/models/stat/", overwrite = TRUE, n_cl = NULL){


    safe_bam <- purrr::possibly(mgcv::bam, otherwise = NULL, quiet = FALSE)



    model_grid_list <- split(model_grid,
                             seq_len(nrow(model_grid)))


    future::plan(future::multisession(workers = 4))
    # out <- purrr::map_dfr(
    out <- furrr::future_map_dfr(
        model_grid_list,
        function(x){


            out_file_path <- fs::path(path, x$mod_names, ext = "Rds")


            if(file.exists(out_file_path) && !overwrite){


                generated <- "old"
                status <- "success"

                cat(sprintf("Model: %s \n exists in  %s \n\n Skipping! \n \n", x$mod_names, out_file_path))

            } else {



                mod <- safe_bam(
                    formula = x$forms[[1]],
                    family = eval(x$fams[[1]]),
                    data = dat,
                    discrete = TRUE,
                    cl = n_cl)


                if(!is.null(mod)){
                    # out_file_path <- fs::path(path, x$mod_names, ext = "Rds")



                    saveRDS(mod, out_file_path)
                    cat(sprintf("Model: %s \n Writing to: %s \n\n", x$mod_names, out_file_path))
                    status <- "success"
                    generated <- "new"
                    rm(mod)


                } else {
                    status <- "fail"
                    out_file_path <- NA
                    cat(sprintf("Model: %s \n Failed.\n\n", x$mod_names))
                    generated <- NA
                }

            }

            status_df <- data.frame(model = x$mod_names,
                                    model_file_path = out_file_path,
                                    state = status,
                                    generated_in_run = generated,
                                    stringsAsFactors = FALSE)


            return(status_df)
        },
        .options = furrr::furrr_options(seed = 123)
    )

    future::plan(future::sequential())
    return(out)
}



#' Make df with model formulas and families
#'
#' @return tibble with 3 colums  `forms` = formulas, `fams` = family function, `mod_names` = id
make_model_grid <- function(){


    # k_uni <- 30
    # k_te <- c(5, 15)

    model_params <- new.env()

    model_params$k_uni <- 7
    model_params$k_age <- 3
    # model_params$k_te <- c(7, 20)
    # model_params$k_te <- c(5, 15)
    model_params$k_te <- c(3, 7)
    model_params$k_spatial <- 400
    model_params$k_spatial_soilnutmodel <- 250
    model_params$k_soilnut <- 25

    tempvars <- list('mod2015_T2M04HMEA',
                     'mod2015_T2M14HMEA',
                     'mod2015_T2M22HMEA',

                     'urbclim_mod_morning_3_5',
                     'urbclim_mod_afternoon_13_15',
                     'urbclim_mod_night_21_23',

                     'day_2007',
                     'night_2007')
    # tempvars <- list('mod2015_T2M04HMEA', 'mod2015_T2M14HMEA')

    #
    # tensor_mods <- list("mI_age_x_temp_by_species_reBEZIRK" =
    # "dbh_cm ~ te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = 're')")
    #     tensor_mods_spatial <- list("mI_spatial_age_x_temp_by_species_reBEZIRK" =
    #                             "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = 're')")
    #     tensor_mods_spatial_full <- list("mI_spatial_age_x_temp_by_species_reBEZIRK_full" =
    #                             "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = 're') + s(soil_nutrients_swert, k = k_uni) + s(building_height_m,  k = k_uni)")




    mods <- list(
        list("mI_age_x_temp_by_species_reBEZIRK" =
                 "dbh_cm ~ te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_building_height_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + +s(building_height_m, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_building_height300_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + +s(building_height_m300, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_lcz6_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + +s(lcz_prop_6, k = k_uni) + s(BEZIRK, bs = 're')"),
        # list("mI_spatial_age_x_temp_by_species_lcz6_30_reBEZIRK" =
        #          "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + +s(lcz_prop30_6, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_lcz6_300_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + +s(lcz_prop300_6, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_soil_nutrients_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial_soilnutmodel, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + +s(log10(soil_nutrients_swert), k = k_soilnut) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_baumsch_flaeche_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial_soilnutmodel, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + +s(log10(baumsch_flaeche_m2), k = k_soilnut) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_x_temp_by_species_reBEZIRK_full" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + te(STANDALTER, %s, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = 're') + s(log10(soil_nutrients_swert), k = k_soilnut) + s(building_height_m,  k = k_uni) + +s(log10(baumsch_flaeche_m2), k = k_soilnut)"),



        list("mI_age_ADD_temp_by_species_reBEZIRK" =
                 "dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_building_height_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + +s(building_height_m, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_building_height300_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + +s(building_height_m300, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_lcz6_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + +s(lcz_prop_6, k = k_uni) + s(BEZIRK, bs = 're')"),
        # list("mI_spatial_age_ADD_temp_by_species_lcz6_30_reBEZIRK" =
        #          "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + +s(lcz_prop30_6, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_lcz6_300_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + +s(lcz_prop300_6, k = k_uni) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_soil_nutrients_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial_soilnutmodel, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + +s(log10(soil_nutrients_swert), k = k_soilnut) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_baumsch_flaeche_reBEZIRK" =
                 "dbh_cm ~  s(X,Y, k = k_spatial_soilnutmodel, bs = 'gp', m = 3) +s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni)  + species_corrected + +s(log10(baumsch_flaeche_m2), k = k_soilnut) + s(BEZIRK, bs = 're')"),
        list("mI_spatial_age_ADD_temp_by_species_reBEZIRK_full" =
                 "dbh_cm ~  s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER, by = species_corrected, k = k_age) +s(%s, by = species_corrected, k = k_uni) + species_corrected + s(BEZIRK, bs = 're') + s(log10(soil_nutrients_swert), k = k_soilnut) + s(building_height_m,  k = k_uni) + +s(log10(baumsch_flaeche_m2), k = k_soilnut)")
    )







    forms <- purrr::map(mods,
                        function(x){
                            make_formula(main_body = x,
                                         placeholders = tempvars,
                                         n_depth = 2)

                        }) %>%
        purrr::flatten()

    # print(forms)

    forms$`mI_age_by_species_NOTEMP_var-nullmodel` <-
        "dbh_cm ~ s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_age_by_species_NOTEMP_var-soil_nutrients` <-
        "dbh_cm ~ s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(log10(soil_nutrients_swert),  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_age_by_species_NOTEMP_var-building_height` <-
        "dbh_cm ~ s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(building_height_m,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_age_by_species_NOTEMP_var-building_height300` <-
        "dbh_cm ~ s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(building_height_m300,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_age_by_species_NOTEMP_var-baumsch_flaeche` <-
        "dbh_cm ~ s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(log10(baumsch_flaeche_m2),  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_age_by_species_NOTEMP_var-lcz6` <-
        "dbh_cm ~ s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(lcz_prop_6,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_age_by_species_NOTEMP_var-lcz6_300` <-
        "dbh_cm ~ s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(lcz_prop300_6,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"


    forms$`mI_spatial_age_by_species_NOTEMP_var-nullmodel` <-
        "dbh_cm ~ s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_spatial_age_by_species_NOTEMP_var-soil_nutrients` <-
        "dbh_cm ~ s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(log10(soil_nutrients_swert),  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_spatial_age_by_species_NOTEMP_var-building_height` <-
        "dbh_cm ~ s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(building_height_m,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_spatial_age_by_species_NOTEMP_var-building_height300` <-
        "dbh_cm ~ s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(building_height_m300,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_spatial_age_by_species_NOTEMP_var-baumsch_flaeche` <-
        "dbh_cm ~ s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(log10(baumsch_flaeche_m2),  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_spatial_age_by_species_NOTEMP_var-lcz6` <-
        "dbh_cm ~ s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(lcz_prop_6,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"
    forms$`mI_spatial_age_by_species_NOTEMP_var-lcz6_300` <-
        "dbh_cm ~ s(X,Y, k = k_spatial, bs = 'gp', m = 3) + s(STANDALTER,  by = species_corrected, m = 1, k = k_age) + s(lcz_prop300_6,  by = species_corrected, m = 1, k = k_uni) + species_corrected + s(BEZIRK, bs = 're')"





    forms <- lapply(forms, formula, env = model_params)






    # forms <- list(
    #     "mI_age_by_species" =
    #         dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re"),
    #     "mI_age_by_species_RE-district" =
    #         dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re") +s(BEZIRK, bs = "re"),
    #
    #               # AGE + Heat by Species
    #               "mI_age_by_species_ADD_heat14_by_species" =
    #                   dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(mod2015_T2M14HMEA, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re"),
    #               "mI_age_by_species_ADD_heat04_by_species" =
    #                   dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(mod2015_T2M04HMEA, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re"),
    #               "mI_age_by_species_ADD_heat22_by_species" =
    #                   dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(mod2015_T2M22HMEA, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re"),
    #
    #               # AGE x HEAT by Species
    #               "mI_age_x_heat14_by_species" =
    #                   dbh_cm ~ te(STANDALTER, mod2015_T2M14HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected,
    #               "mI_age_x_heat04_by_species" =
    #                   dbh_cm ~ te(STANDALTER, mod2015_T2M04HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected,
    #               "mI_age_x_heat22_by_species" =
    #                   dbh_cm ~ te(STANDALTER, mod2015_T2M22HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected,
    #
    #               "mI_age_x_heat04_by_species_reBEZIRK" =
    #                   dbh_cm ~ te(STANDALTER, mod2015_T2M04HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = "re"),
    #               "mI_age_x_heat14_by_species_reBEZIRK" =
    #                   dbh_cm ~ te(STANDALTER, mod2015_T2M14HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = "re"),
    #               "mI_age_x_heat22_by_species_reBEZIRK" =
    #                   dbh_cm ~ te(STANDALTER, mod2015_T2M22HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = "re"),
    #
    #               # SPATIAL + AGE x HEAT by Species
    #               "mI_spatial_age_x_heat14_by_species" =
    #                   dbh_cm ~ s(X,Y, k = k_spatial, bs = "ds") + te(STANDALTER, mod2015_T2M14HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected
    #               # SPATIAL + AGE x HEAT by Species
    #               "mI_spatial_age_x_heat22_by_species" =
    #                   dbh_cm ~ s(X,Y, k = k_spatial, bs = "ds") + te(STANDALTER, mod2015_T2M22HMEA, by = species_corrected, m = 1, k = k_te) + species_corrected
    #
    #
    #
    #
    #
    #
    #               # AGE + Heat by Species
    #               "mI_age_by_species_ADD_urbclim13-15_by_species" =
    #                   dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(urbclim_mod_afternoon_13_15, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re"),
    #               "mI_age_by_species_ADD_urbclim03-05_by_species" =
    #                   dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(urbclim_mod_morning_3_5, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re"),
    #               "mI_age_by_species_ADD_urbclim21-23_by_species" =
    #                   dbh_cm ~ s(STANDALTER, by = species_corrected, k = k_uni) + s(urbclim_mod_night_21_23, by = species_corrected, k = k_uni) + s(species_corrected, bs = "re"),
    #
    #               # AGE x HEAT by Species
    #               "mI_age_x_urbclim13-15_by_species" =
    #                   dbh_cm ~ te(STANDALTER, urbclim_mod_afternoon_13_15, by = species_corrected, m = 1, k = k_te) + species_corrected,
    #               "mI_age_x_urbclim03-05_by_species" =
    #                   dbh_cm ~ te(STANDALTER, urbclim_mod_morning_3_5, by = species_corrected, m = 1, k = k_te) + species_corrected,
    #               "mI_age_x_urbclim21-23_by_species" =
    #                   dbh_cm ~ te(STANDALTER, urbclim_mod_night_21_23, by = species_corrected, m = 1, k = k_te) + species_corrected,
    #
    #               "mI_age_x_urbclim03-05_by_species_reBEZIRK" =
    #                   dbh_cm ~ te(STANDALTER, urbclim_mod_morning_3_5, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = "re"),
    #               "mI_age_x_urbclim13-15_by_species_reBEZIRK" =
    #                   dbh_cm ~ te(STANDALTER, urbclim_mod_afternoon_13_15, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = "re"),
    #               "mI_age_x_urbclim21-23_by_species_reBEZIRK" =
    #                   dbh_cm ~ te(STANDALTER, urbclim_mod_night_21_23, by = species_corrected, m = 1, k = k_te) + species_corrected + s(BEZIRK, bs = "re"),
    #
    #               # SPATIAL + AGE x HEAT by Species
    #               "mI_spatial_age_x_urbclim13-15_by_species" =
    #                   dbh_cm ~ s(X,Y, k = k_spatial, bs = "ds") + te(STANDALTER, urbclim_mod_afternoon_13_15, by = species_corrected, m = 1, k = k_te) + species_corrected
    #               # SPATIAL + AGE x HEAT by Species
    #               "mI_spatial_age_x_urbclim21-23_by_species" =
    #                   dbh_cm ~ s(X,Y, k = k_spatial, bs = "ds") + te(STANDALTER, urbclim_mod_night_21_23, by = species_corrected, m = 1, k = k_te) + species_corrected
    # )



    fams <- rep(expression(Gamma(link = "log")), length(forms))


    model_grid <- dplyr::tibble(forms, fams)  %>%
        mutate(mod_names = names(forms))

    return(model_grid)

}


make_model_prediction_df <- function(
    path_model, model_df,
    fixed_vars = list(X = X, Y = Y, STANDALTER = STANDALTER),
    crit_val = 1.96){


    future::plan(future::multisession(workers = 4))


    # extract model meta from file path
    mod_names <- gsub(
        pattern = "(.*[_]var[-])(.*)([.]Rds$)",
        x =  path_model,
        replacement = "\\2",
        perl = TRUE)

    # only grab TEMP models!
    mod_names <- mod_names[!grepl(pattern = "NOTEMP", x = mod_names)]



    # load model
    mod_list <- purrr::map(path_model, ~readRDS(.x)) %>%
        setNames(mod_names)


    # set up grid variables



    # bheight
    # check which variable is present
    if(!is.null(fixed_vars[['building_height_m']])){

        bheight <- fixed_vars[['building_height_m']]
        bheight_name <- 'building_height_m'
    } else if(!is.null(fixed_vars[['building_height_m300']])){
        bheight <- fixed_vars[['building_height_m300']]
        bheight_name <- 'building_height_m300'

    } else if(is.null(fixed_vars[['building_height_m']]) &
              is.null(fixed_vars[['building_height_m300']])){
        bheight <- median(model_df[['building_height_m']], na.rm = TRUE)
        bheight_name <- 'building_height_m'

    }

    # lcz prop
    if(!is.null(fixed_vars[['lcz_prop_6']])){

        lcz <- fixed_vars[['lcz_prop_6']]
        lcz_name <- 'lcz_prop_6'
    } else if(!is.null(fixed_vars[['lcz_prop30_6']])){
        lcz <- fixed_vars[['lcz_prop30_6']]
        lcz_name <- 'lcz_prop30_6'
    } else if(!is.null(fixed_vars[['lcz_prop300_6']])){
        lcz <- fixed_vars[['lcz_prop300_6']]
        lcz_name <- 'lcz_prop300_6'

    } else if(is.null(fixed_vars[['lcz_prop_6']]) &
              is.null(fixed_vars[['lcz_prop30_6']]) &
              is.null(fixed_vars[['lcz_prop300_6']])){
        lcz <- median(model_df[['lcz_prop_6']], na.rm = TRUE)
        lcz_name <- 'lcz_prop_6'

    }

#
#     lcz <- {if(!is.null(fixed_vars$lcz_prop_6)){
#         fixed_vars$lcz_prop_6
#     } else {
#         median(model_df[['lcz_prop_6']], na.rm = TRUE)
#     }}

#
#
#     if(!is.null(fixed_vars[['building_height_m']])){
#         bheight <- fixed_vars$building_height_m
#             # else {
#         #     median(model_df[['building_height_m']], na.rm = TRUE)
#         # }
#
#         bheight_name <- "building_height_m"
#
#     } else if(is.null(fixed_vars[['building_height_m']]) & is.null(fixed_vars[['building_height_m300']]) ){
#         bheight <- median(model_df[['building_height_m']], na.rm = TRUE)
#         bheight_name <- "building_height_m"
#
#
#
#     } else {
#
#         bheight <- {if(!is.null(fixed_vars[['building_height_m300']]) & is.null(fixed_vars[['building_height_m']])){
#             fixed_vars$building_height_m300
#         }
#         bheight_name <- "building_height_m300"
#
#
#     }}
#






    snut <- {if(!is.null(fixed_vars$soil_nutrients_swert)){
        fixed_vars$soil_nutrients_swert
    } else {
        median(model_df[['soil_nutrients_swert']], na.rm = TRUE)
    }}


    baumsch <- {if(!is.null(fixed_vars$baumsch_flaeche_m2)){
                      fixed_vars$baumsch_flaeche_m2
    } else {
                      median(model_df[['baumsch_flaeche_m2']], na.rm = TRUE)
    }}

    # lcz <- {if(!is.null(fixed_vars$lcz_prop_6)){
    #     fixed_vars$lcz_prop_6
    # } else {
    #     median(model_df[['lcz_prop_6']], na.rm = TRUE)
    # }}



    # make prediction df with each model var
    # pdata <- purrr::map(
    pdata <- furrr::future_map(
        mod_names,
        function(mn){

            # mn <- rlang::sym(mn)

            temps <- {if(!is.null(fixed_vars$tempvar)){
                fixed_vars$tempvar
            } else {
                seq(min(model_df[[mn]], na.rm = TRUE),
                    max(model_df[[mn]], na.rm = TRUE), length.out = 100)
            }}


            args_list <- list(
                # temp var
                # seq(min(model_df[[mn]], na.rm = TRUE),
                #     max(model_df[[mn]], na.rm = TRUE), length.out = 100),
                temps,
                # X
                fixed_vars$X,
                # Y
                fixed_vars$Y,
                # STANDALTER
                fixed_vars$STANDALTER,
                # species_corrected
                as.factor(unique(model_df[["species_corrected"]])),
                # building_height_m
                # median(model_df[['building_height_m']], na.rm = TRUE),
                bheight,
                # soil_nutrients_swert
                # median(model_df[['soil_nutrients_swert']], na.rm = TRUE),
                snut,
                # baumsch
                baumsch,
                #lcz,
                lcz,
                # BEZIRK
                as.factor(unique(model_df[['BEZIRK']]))
            )

            names(args_list) <- c(
                mn,
                "X",
                "Y",
                "STANDALTER",
                "species_corrected",
                bheight_name,
                "soil_nutrients_swert",
                "baumsch_flaeche_m2",
                lcz_name,
                "BEZIRK"
            )

            tmp <- do.call(expand.grid, args_list)



            #
            # tmp <- with(model_df,
            #     # tidyr::expand_grid({{mn}} := seq(min({{mn}}, na.rm = TRUE),
            #                                         # max({{mn}}, na.rm = TRUE), length.out = 200),
            #     tidyr::expand_grid(!!mn := seq(min(!!mn, na.rm = TRUE),
            #                                         max(!!mn, na.rm = TRUE), length.out = 200),
            #                 X = X,
            #                 Y = Y,
            #                 # STANDALTER = c(30, 50, 80),
            #                 STANDALTER = STANDALTER,
            #                 species_corrected = as.factor(unique(species_corrected)),
            #                 building_height_m = median(building_height_m, na.rm = TRUE),
            #                 soil_nutrients_swert = median(soil_nutrients_swert, na.rm = TRUE),
            #                 BEZIRK = as.factor(unique(BEZIRK)))
            #  )
            #


            return(tmp)




        }) %>%
        setNames(mod_names)




    # predict for each model
    # preds <- purrr::map2(
    preds <- furrr::future_map2(
        mod_list,
        pdata,
        function(mod, pd){

            ifun <- family(mod)$linkinv



            p <- mgcv::predict.bam(
                object = mod,
                newdata = pd,
                se.fit = TRUE,
                exclude = "s(BEZIRK)")


            pd <- cbind(pd, pred = p) %>%
                mutate(fit.low = pred.fit - crit_val * pred.se.fit,
                       fit.high = pred.fit + crit_val * pred.se.fit,
                       response = ifun(pred.fit),
                       response.low = ifun(fit.low),
                       response.high = ifun(fit.high)
                )
            return(as.data.frame(pd))
        },
        .options = furrr::furrr_options(seed=TRUE))


    # define prediction groups, summarize

    # combine results

    # generate plot
    future::plan(future::sequential())

    return(preds)


}



#' Get GAMM and Spatial (variogram) summaries
#'
#' Hard-coded 8 parallel sessions to summarise models
#'
#' @param mgroups factor, model subgroups
#' @param bam_df data.frame, output from apply gam models (for dependency only)
#' @param path_model_dir character, path to model dir (filtered/nonfiltered)
#' @param path_model_files character, path to model files
#' @param path_out character, path to save RDS of model summary on disk
#'
#'
#' @return list of model summaries
model_summarize <- function(mgroups, bam_df, path_model_dir, path_model_files,path_out){





    future::plan(future::multisession(workers = 4))
    mod_summary_list <-
        furrr::future_map(
            levels(mgroups),
            function(mg){

                idx <- which(mgroups == mg)

                mods <- lapply(path_model_files[idx],
                               function(x){
                                   m <- readRDS(x)

                                   s <- m %>%
                                       summary()
                                   g <- broom::glance(m)

                                   m <- broom::augment(m,
                                                       type.residuals = "response",
                                                       type.response = "response")



                                   v <- {
                                       if(grepl("mI_spatial", x = x)){
                                           gstat::variogram(.resid ~ 1, locations = ~ X + Y, width = 50, cutoff = 5000, data = m)
                                       } else {
                                           NULL
                                       }}

                                   return(
                                       list(summary = s,
                                            summary_broom = g,
                                            variogramm = v,
                                            fitted_response = m[ , c("dbh_cm", ".fitted", ".resid")]))

                               }) %>%
                    setNames(
                        fs::path_ext_remove(
                            list.files(path_model_dir)[idx]
                        )  %>%
                            gsub(pattern = ".*var-",
                                 replacement = "",
                                 x = .)
                    )
            }, .options = furrr::furrr_options(seed = TRUE)) %>%
        setNames(levels(mgroups))
    future::plan(future::sequential())


    saveRDS(mod_summary_list, file = path_out)

    return(path_out)

}


#' Predict temp models
#'
#' @param path_model character, path to single model
#' @param model_df dframe, used for fitting path model
#' @param fixed_vars list, provide variables used in model
#' @param age_expression expression, used to define age groups
#' @param group_vars character, name of variables (next to temp) for grouping
#'
#' @return list with raw predictions and summarized by age group
pred_dbh_temp_single_var <- function(path_model,
                                     model_df,
                                     fixed_vars,
                                     age_expression,
                                     group_vars
){



    # `%nin%` <- Negate(`%in%`)



    pred_var <- make_model_prediction_df(
        path_model = path_model,
        model_df = model_df %>%
            mutate(species_corrected = droplevels(species_corrected)),
        fixed_vars = fixed_vars
    )




    pred_groups <- purrr::map2_dfr(
        pred_var,
        names(pred_var),
        function(x,y){
            summarize_age_groups(
                x,
                model_df %>%
                    mutate(species_corrected = droplevels(species_corrected)),
                c(y, group_vars),
                age_break_expr = age_expression)
        },
        .id = "tempvar"
    )


    return(list(pred_var = pred_var, pred_groups = pred_groups))


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


#' Plot UHI with Berlin districts
#'
#' @param uhi_rast Raster(layer) UHI
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
make_uhi_urbclim_plot <- function(uhi_rast,
                                  berlin_poly,
                                  legend_label = expression(atop(Summer~3-5~AM,
                                                                 UHI~(degree*C))),
                                  base_size = 18,
                                  file,
                                  height,
                                  width,
                                  dpi){

    # extrafont::loadfonts(device = "win",quiet = TRUE)
    # extrafont::loadfonts(device = "pdf",quiet = TRUE)


    # uhi_stacks <- purrr::modify(uhi_stacks, raster::raster)

    # berlin_poly <- sf::st_transform(berlin_poly,
    # crs = raster::crs(uhi_rast))

    uhi_rast <- raster::projectRaster(from = uhi_rast,
                                      crs = sf::st_crs(berlin_poly)$wkt)

    mid_rescaler <- function(mid = 0) {
        function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
            scales::rescale_mid(x, to, from, mid)
        }
    }





    p <- ggplot2::ggplot() +


        ggplot2::geom_sf(data = berlin_poly,
                         color = "gray60",
                         fill = "gray80",
                         size = 0.75,
                         show.legend = FALSE) +


        stars::geom_stars(data = stars::st_as_stars(uhi_rast)[berlin_poly]) +

        # %>%
        #     dplyr::slice(band, 1),
        # na.rm = TRUE) +
        #

        ggplot2::geom_sf(data = berlin_poly,
                         color = "gray20",
                         fill = "transparent",
                         size = 0.25,
                         show.legend = FALSE) +

        ggplot2::scale_y_continuous(breaks = seq(52.35, 52.65, by = 0.1)) +
        ggplot2::scale_x_continuous(breaks = seq(13.1, 13.7, by = 0.2)) +


        # ggplot2::scale_fill_viridis_c(na.value = "transparent", option = "inferno") +
        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      rescaler = mid_rescaler(),
                                      na.value = "transparent")   +

        ggplot2::labs(fill = legend_label,
                      x = NULL,
                      # title = "Estimate of Urban Heat Loading",
                      y = NULL) +


        # ggplot2::theme_minimal(base_family = "Roboto Condensed",
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = ggplot2::unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))

    ggplot2::ggsave(filename = file,
                    plot = p,
                    dpi = dpi,
                    height = height,
                    width = width)

    # print(p)


}

#' Make temp overview plot
#'
#' @param landsat list, uhi stacks
#' @param envat rasterstack, berlin heat model
#' @param urbclim list, urbclim rasters
#' @param berlin_poly sf, outlines
#' @param ylim numeric, UTM coordinates
#' @param xlim numeric, UTM coordinates
#' @param base_size
#' @param file
#' @param height
#' @param width
#' @param dpi
plot_temp_maps <- function(landsat,
                           envat,
                           urbclim,
                           berlin_poly,
                           ylim = c(5789783, 5853683),
                           xlim = c(360062, 424844),
                           # ylim = c(52.275, 52.85),
                           base_size = 18,
                           file,
                           height,
                           width,
                           dpi){

    # extrafont::loadfonts(device = "win",quiet = TRUE)
    extrafont::loadfonts(device = "pdf",quiet = TRUE)

    mid_rescaler <- function(mid = 0) {
        function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
            scales::rescale_mid(x, to, from, mid)
        }
    }

    # main_crs <-  raster::crs(landsat$Summertime_gridded_UHI_data$day)
    main_crs <- berlin_poly %>% sf::st_crs()


    landsat$Summertime_gridded_UHI_data$day <-
        raster::projectRaster(
            landsat$Summertime_gridded_UHI_data$day,
            crs = main_crs$wkt
        )
    landsat$Summertime_gridded_UHI_data$night <-
        raster::projectRaster(
            landsat$Summertime_gridded_UHI_data$night,
            crs = main_crs$wkt
        )



    # ensure all assets have same proj
    berlin_poly <- sf::st_transform(berlin_poly,
                                    crs = main_crs)
    urbclim <- raster::projectRaster(from = urbclim,
                                     crs = main_crs$wkt)  %>%
        stars::st_as_stars(proxy = FALSE)
    urbclim <- urbclim[berlin_poly]
    envat <- raster::projectRaster(from = envat,
                                   crs = main_crs$wkt) %>%
        stars::st_as_stars(proxy = FALSE)
    #


    # plotting parameters
    labs_uhi <- expression(UHI~(degree*C))
    labs_temp <- expression(Temp.~(degree*C))


    # LANDSAT DAY -------------------------------------------------------------


    p_landsat_day <- ggplot2::ggplot() +


        ggplot2::geom_sf(data = berlin_poly,
                         color = "gray60",
                         fill = "gray80",
                         size = 0.75,
                         show.legend = FALSE) +


        stars::geom_stars(data = stars::st_as_stars(landsat$Summertime_gridded_UHI_data$day$day_2007),
                          na.rm = TRUE,
                          downsample = 2) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      rescaler = mid_rescaler(),
                                      na.value = "transparent")   +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            # UHI~(degree*C))),
            fill = labs_uhi,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))

    # LANDSAT NIGHT -----------------------------------------------------------

    p_landsat_night <- ggplot2::ggplot() +

        ggplot2::geom_sf(data = berlin_poly,
                         color = "gray60",
                         fill = "gray80",
                         size = 0.75,
                         show.legend = FALSE) +


        stars::geom_stars(data = stars::st_as_stars(landsat$Summertime_gridded_UHI_data$night$night_2007),
                          na.rm = TRUE,
                          downsample = 2) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      rescaler = mid_rescaler(),
                                      na.value = "transparent")   +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            #                                  UHI~(degree*C))),
            fill = labs_uhi,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))



    # ENVAT Morning -----------------------------------------------------------


    rn <- envat %>% dplyr::slice(band, 1) %>% as.data.frame() %>% dplyr::select(3) %>% range(na.rm = TRUE)

    rn[1] <- floor(rn[1])
    rn[2] <- ceiling(rn[2])

    p_envat_morning  <- ggplot2::ggplot() +

        #
        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "gray80",
        #                  size = 0.75,
        #                  show.legend = FALSE) +


        stars::geom_stars(data = envat %>% slice(band, 1),
                          na.rm = TRUE,
                          downsample = 5) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        # ggplot2::scale_fill_distiller(palette = "RdBu",
        #                               # rescaler = mid_rescaler(),
        #                               na.value = "transparent")   +


        ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "transparent",
                                      limits = c(10,22), breaks = c(10,22)) +
        # limits = rn, breaks = rn) +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            # UHI~(degree*C))),
            fill = labs_temp,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))




    # ENVAT AFTERNOON -----------------------------------------------------------
    rn <- envat %>% dplyr::slice(band, 2) %>% as.data.frame() %>% dplyr::select(3) %>% range(na.rm = TRUE)
    rn[1] <- floor(rn[1])
    rn[2] <- ceiling(rn[2])

    p_envat_afternoon  <- ggplot2::ggplot() +

        #
        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "gray80",
        #                  size = 0.75,
        #                  show.legend = FALSE) +


        stars::geom_stars(data = envat %>% slice(band, 2),
                          na.rm = TRUE,
                          downsample = 5) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        # ggplot2::scale_fill_distiller(palette = "RdBu",
        #                              # rescaler = mid_rescaler(),
        #                               na.value = "transparent")   +

        ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "transparent",
                                      limits = rn, breaks = rn) +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            # UHI~(degree*C))),
            fill = labs_temp,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))



    # ENVAT NIGHT -----------------------------------------------------------
    rn <- envat %>% dplyr::slice(band, 3) %>% as.data.frame() %>% dplyr::select(3) %>% range(na.rm = TRUE)
    rn[1] <- floor(rn[1])
    rn[2] <- ceiling(rn[2])

    p_envat_night  <- ggplot2::ggplot() +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "gray80",
        #                  size = 0.75,
        #                  show.legend = FALSE) +


        stars::geom_stars(data = envat %>% slice(band, 3),
                          na.rm = TRUE,
                          downsample = 5) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        # ggplot2::scale_fill_distiller(palette = "RdBu",
        #                              # rescaler = mid_rescaler(),
        #                               na.value = "transparent")   +

        ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "transparent",
                                      limits = rn, breaks = rn) +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            # UHI~(degree*C))),
            fill = labs_temp,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))




    # DUP ---------------------------------------------------------------------


    # urbclim Morning -----------------------------------------------------------


    rn <- urbclim %>% dplyr::slice(band, 1) %>% as.data.frame() %>% dplyr::select(3) %>% range(na.rm = TRUE)

    rn[1] <- floor(rn[1])
    rn[2] <- ceiling(rn[2])

    p_urbclim_morning  <- ggplot2::ggplot() +

        #
        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "gray80",
        #                  size = 0.75,
        #                  show.legend = FALSE) +


        stars::geom_stars(data = urbclim %>% slice(band, 1),
                          na.rm = TRUE,
                          downsample = 2) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      # rescaler = mid_rescaler(),
                                      na.value = "transparent")   +


        # ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "transparent",
        #                               limits = c(10,22), breaks = c(10,22)) +
        # limits = rn, breaks = rn) +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            # UHI~(degree*C))),
            fill = labs_uhi,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))




    # urbclim AFTERNOON -----------------------------------------------------------
    rn <- urbclim %>% dplyr::slice(band, 2) %>% as.data.frame() %>% dplyr::select(3) %>% range(na.rm = TRUE)
    rn[1] <- floor(rn[1])
    rn[2] <- ceiling(rn[2])

    p_urbclim_afternoon  <- ggplot2::ggplot() +

        #
        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "gray80",
        #                  size = 0.75,
        #                  show.legend = FALSE) +


        stars::geom_stars(data = urbclim %>% slice(band, 2),
                          na.rm = TRUE,
                          downsample = 2) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      # rescaler = mid_rescaler(),
                                      na.value = "transparent")   +

        # ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "transparent",
        #                               limits = rn, breaks = rn) +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            # UHI~(degree*C))),
            fill = labs_uhi,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))



    # urbclim NIGHT -----------------------------------------------------------
    rn <- urbclim %>% dplyr::slice(band, 3) %>% as.data.frame() %>% dplyr::select(3) %>% range(na.rm = TRUE)
    rn[1] <- floor(rn[1])
    rn[2] <- ceiling(rn[2])

    p_urbclim_night  <- ggplot2::ggplot() +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "gray80",
        #                  size = 0.75,
        #                  show.legend = FALSE) +


        stars::geom_stars(data = urbclim %>% slice(band, 3),
                          na.rm = TRUE,
                          downsample = 2) +


        # ggplot2::geom_sf(data = berlin_poly,
        #                  color = "gray60",
        #                  fill = "transparent",
        #                  size = 0.75,
        #                  show.legend = FALSE) +

        ggplot2::coord_sf(xlim = xlim, ylim = ylim) +

        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      # rescaler = mid_rescaler(),
                                      na.value = "transparent")   +

        # ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "transparent",
        #                               limits = rn, breaks = rn) +

        ggplot2::labs(
            # fill = expression(atop(Summer~day-time,
            # UHI~(degree*C))),
            fill = labs_uhi,
            x = NULL,
            # title = "Estimate of Urban Heat Loading",
            y = NULL) +


        ggplot2::theme_void(base_family = "Roboto Condensed",
                            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.6, 0.95)) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = unit(3.5, "cm"),
                                                       title.vjust = 1,
                                                       ticks.colour = "gray30"))




    p_list <- list(p_landsat_day = p_landsat_day,
                   p_landsat_night = p_landsat_night,
                   p_envat_morning = p_envat_morning,
                   p_envat_afternoon = p_envat_afternoon,
                   p_envat_night = p_envat_night,
                   p_urbclim_morning = p_urbclim_morning,
                   p_urbclim_afternoon = p_urbclim_afternoon,
                   p_urbclim_night = p_urbclim_night
    )




    library(patchwork)

    layout <- "
##AABB
CCDDEE
FFGGHH
"
    p_map <-
        p_list$p_landsat_day +
        p_list$p_landsat_night +

        p_list$p_envat_morning +
        p_list$p_envat_afternoon +
        p_list$p_envat_night +

        p_list$p_urbclim_morning +
        p_list$p_urbclim_afternoon +
        p_list$p_urbclim_night +


        plot_layout(design = layout, heights = 1/3)


    png(filename = file, width = width, height = height, res = dpi, units = "in")

    gp <- grid::gpar(fontfamily = "Roboto Condensed", fontsize = base_size)

    print(p_map)
    grid::grid.text(gp = gp, "UrbClim", x = 0.03, y = 1/3 * 0.5, rot = 90)
    grid::grid.text(gp = gp, "Berlin EnvAT", x = 0.03, y = 1/3 * 0.5 + 1/3, rot = 90)
    grid::grid.text(gp = gp, "LandSat", x = 0.03, y = 1/3 * 0.5 + 2 * 1/3, rot = 90)

    grid::grid.text(gp = gp, "Morning (0400)", y = 0.03, x = 1/3 * 0.5)
    grid::grid.text(gp = gp, "Day / Afternoon (1400)", y = 0.03, x = 1/3 * 0.5 + 1/3)
    grid::grid.text(gp = gp, "Night (2200)", y = 0.03, x = 1/3 * 0.5 + 2*  1/3)


    dev.off()

    #
    # ggplot2::ggsave(filename = file,
    #                 plot = p_map,
    #                 dpi = dpi,
    #                 height = height,
    #                 width = width)

    return(p_map)


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


#' Generate study site area
#'
#' @param blu character, path to berlin land use
#' @param berlin_poly character, path to berlin poly
#'
#' @return ggplot object
#' @export
#'
#' @examples
make_map_study_area <- function(blu, berlin_poly, path_out, height, width, dpi){



    blu <-  sf::st_read(blu, quiet = TRUE) %>%
        sf::st_make_valid()

    blu <- blu %>%
        dplyr::mutate(lu_adjusted = dplyr::case_when(
            GRZ_NAME %in% c("Baumschule / Gartenbau",
                            "Park / Grünfläche",
                            "Friedhof",
                            "Brachfläche, wiesenartiger Vegetationsbestand",
                            "Brachfläche, Mischbestand aus Wiesen, Gebüschen und Bäumen",
                            "Kleingartenanlage",
                            "Baumschule / Gartenbau",
                            "Wald",
                            "Grünland",
                            "Ackerland") ~ "greenspace",
            GRZ_NAME == "Gewässer" ~ "water",
            TRUE ~ "urbanized"))





    base_size <- 16



    # countries <- rnaturalearth::ne_countries(returnclass = "sf", continent = "Europe", scale = 50)
    countries <- rnaturalearth::ne_countries(returnclass = "sf", continent = "Europe", scale = 110)



    europe_box <- data.frame(lon = c(-12, 45),
                             lat = c(71, 34)) %>%
        sf::st_as_sf(coords = c('lon', 'lat'), crs=4326, remove = FALSE) %>%
        sf::st_bbox() %>%
        sf::st_as_sfc()



    berlin_loc <- blu %>%
        sf::st_bbox() %>%
        sf::st_as_sfc() %>%
        sf::st_centroid() %>%
        sf::st_transform(crs = sf::st_crs(countries))

    # countries <- sf::st_crop(countries, europe_box)


    map_europe<- ggplot2::ggplot() +
        ggplot2::geom_sf(data = countries,
                         # geom_sf(data = countries  %>%
                         #             dplyr::filter(admin != 'Russia'),
                         ggplot2::aes(fill = admin == 'Germany'),
                         color = "gray20", show.legend = FALSE) +
        ggplot2::geom_sf(data = berlin_loc, fill = "firebrick1", shape = 23, size = 2) +
        ggplot2::coord_sf(xlim = c(-10,45), ylim = c(34,71), crs = 4326) +
        # geom_sf(data = countries  %>%
        #             dplyr::filter(admin == 'Russia'),
        #         fill = "gray90",
        #         color = "gray90") +
        # geom_sf(data = europe_box, color = "black", fill = "transparent") +
        ggplot2::scale_fill_manual(values = c("gray95", "gray30")) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"),
                       panel.border = ggplot2::element_rect(color = "black", fill = "transparent"))



    berlin_poly <- berlin_poly %>%
        sf::st_make_valid() %>%
        sf::st_union()






    plt <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = blu,
                         ggplot2::aes(fill = lu_adjusted,
                                      color = lu_adjusted)) +
        ggplot2::geom_sf(data = berlin_poly, color = "black", fill = "transparent") +
        ggplot2::scale_fill_manual(values = c("greenspace" = "seagreen4",
                                              "urbanized" = "beige",
                                              "water" = "steelblue1")) +
        ggplot2::scale_color_manual(values = c("greenspace" = "seagreen4",
                                               "urbanized" = "beige",
                                               "water" = "steelblue1")) +
        ggplot2::coord_sf(xlim = c(370000, 425000.2), ylim = c(5795000    , 5837259.4 )) +
        ggplot2::scale_y_continuous(breaks = seq(52.35, 52.65, by = 0.1)) +
        ggplot2::theme_minimal(
            # base_family = "Roboto Condensed",
            base_size = base_size) +
        ggplot2::theme(legend.direction = "horizontal",
                       legend.position = c(0.4, 0.075),
                       # legend.position = "top",
                       legend.background = ggplot2::element_rect(fill = "white", color = "transparent"),
                       # legend.key = element_rect(color = "black", fill = "transparent"),
                       legend.title = ggplot2::element_blank()) +
        ggplot2::annotation_custom(grob = ggplot2::ggplotGrob(map_europe),
                                   xmin = 410000 ,
                                   xmax = 430000.2 ,
                                   ymin = 5820000 ,
                                   ymax = 5837259.4)

    ggplot2::ggsave(path_out,
                    plot = plt,
                    height = height,
                    width = width,
                    dpi = dpi)



}



#' Climate overview plot
#'
#' @param clim tibble, from `download_berlin_climate_data()`
#' @param col_temp character, color val for temperature
#' @param col_prec character, color val for precip mean
#' @param col_prec_secondary character, color val for precip range
#' @param base_size numeric, 18
#' @param file character, save height
#' @param height numeric,
#' @param width numeric
#' @param dpi numeric
#'
make_berlin_climate_plot <- function(
    clim,

    col_temp = "#ed6a1f",
    col_prec = "#3dd2e3",
    col_prec_secondary = "#93d2d9",

    base_size = 18,
    file,
    height,
    width,
    dpi
){

    extrafont::loadfonts(device = "pdf", quiet = TRUE)



    temp_plot <- clim %>%
        mutate(across(where(is.numeric), ~ifelse(.x < -100, NA, .x))) %>%
        mutate(month = forcats::fct_relevel(month, month.abb)) %>%

        ggplot(aes(x = month)) +

        geom_line(aes(y = vals_temp, group = year), col = col_temp, alpha = 0.2) +

        stat_summary(aes(x = month, y = vals_temp, group = 1),
                     geom = "ribbon",
                     fill = col_temp,
                     color = "transparent",
                     alpha = 0.5,
                     fun.data = "mean_cl_boot") +

        stat_summary(aes(x = month,
                         y = vals_temp,
                         group = 1),
                     geom = "line",
                     color = col_temp,
                     size = 1,
                     fun = mean) +

        theme_bw(base_size = base_size, base_family = "Roboto Condensed") +
        theme(axis.line.x.top = element_line(color = "white")) +
        guides(x.sec = guide_axis(NULL),
               x = guide_axis(n.dodge = 2)) +
        # scale_x_discrete(guide = guide_axis(n.dodge = 2))+
        theme(axis.text.x.top = element_blank(),
              axis.ticks.length.x.top = unit(0, "npc")) +

        labs(x = NULL, y = expression(Temperature~(degree*C)))

    prec_plot <- clim %>%
        mutate(across(where(is.numeric), ~ifelse(.x < -100, NA, .x))) %>%
        mutate(month = forcats::fct_relevel(month, month.abb)) %>%

        ggplot(aes(x = month)) +

        # geom_col(aes(y = vals_prec, group = year), col = col_temp, alpha = 0.2) +

        stat_summary(aes(x = month, y = vals_prec, group = 1),
                     geom = "linerange",
                     # color = "gray60",
                     color = col_prec_secondary,
                     size = 3,
                     alpha = 0.25,
                     fun = mean,
                     fun.max = max,
                     fun.min = min) +
        stat_summary(aes(x = month, y = vals_prec, group = 1),
                     geom = "linerange",
                     color = col_prec,
                     size = 2,
                     alpha = 1,
                     fun.data = "mean_cl_boot") +
        stat_summary(aes(x = month, y = vals_prec, group = 1),
                     geom = "line",
                     color = col_prec,
                     size = 1,
                     alpha = 1,
                     fun = "mean") +
        stat_summary(aes(x = month, y = vals_prec, group = 1),
                     geom = "point",
                     color = "white",
                     size = 0.75,
                     alpha = 1,
                     fun = "mean") +
        scale_y_reverse() +

        theme_bw(base_size = base_size, base_family = "Roboto Condensed") +
        theme(
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            # axis.line = element_line(),
            # axis.line.y = element_line(),
            # axis.line.x = element_line(),
            axis.line.x.top = element_line(color = "black"),
            axis.line.x.bottom = element_line(color = "white")) +
        labs(y = expression(Precipitation~(mm)))


    # library(patchwork)


    patch_plot <- prec_plot + temp_plot  +
        patchwork::plot_layout(ncol = 1) +
        patchwork::plot_annotation(tag_levels = "A", tag_suffix = ")") &
        theme(text = element_text(size = 16),
              plot.tag.position = "topright",
              plot.tag = element_text(margin = margin(l = 10)))

    ggplot2::ggsave(filename = file,
                    plot = patch_plot,
                    dpi = dpi,
                    height = height,
                    width = width)

}

#' Create model overview for SI
#'
#' @param file character, file path (use with \code{\link{file_out}}), must include file ending
#' @param height numeric, height in inches
#' @param width numeric, width in inches
#' @param dpi numeric, dpi of output
#' @param deviance_list list from deviance summary
#' @param base_size
#'
#' @return ggplot object
#' @import ggplot2
make_deviance_plot <- function(deviance_list,
                               base_size = 18,
                               file,
                               height,
                               width,
                               dpi){


    # # grab sample size per model
    # mod_n <- mod_summary_list %>%
    #     purrr::map_depth(2, "summary") %>%
    #     purrr::map_depth(2, "n") %>%
    #     purrr::map_depth(2, .f = as.data.frame) %>%
    #     purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
    #     purrr::map2(names(.),
    #                 ~mutate(.x, mod_group = .y)) %>%
    #     do.call(rbind, .) %>%
    #     `rownames<-`(NULL) %>%
    #     rename(n_sample = 2)
    #
    #
    #
    # # extract deviance and add sample sizes
    # mod_dev <- mod_summary_list %>%
    #     purrr::map_depth(2, "summary") %>%
    #     purrr::map_depth(2, "dev.expl") %>%
    #     purrr::map_depth(2, .f = as.data.frame) %>%
    #     purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
    #     purrr::map2(names(.),
    #                 ~mutate(.x, mod_group = .y)) %>%
    #     do.call(rbind, .) %>%
    #     `rownames<-`(NULL) %>%
    #     dplyr::rename(deviance_explained = 2) %>%
    #     dplyr::left_join(mod_n, by = c("mod_group", 'expvar'))
    #
    # # calc data for points over box plot
    # mod_means <- mod_dev %>%
    #     dplyr::group_by(mod_group) %>%
    #     dplyr::summarise(ggplot2:::mean_se(deviance_explained)) %>%
    #     dplyr::arrange(desc(y))
    #
    # mod_dev$mod_group <- forcats::fct_relevel(mod_dev$mod_group, mod_means$mod_group)
    # mod_means$mod_group <- forcats::fct_relevel(mod_means$mod_group, mod_means$mod_group)

    # ggplot(mod_dev,
    #        aes(y = mod_group,
    #            x = deviance_explained)) +
    #     geom_line(data = mod_means, aes(x = y),  color = "black", group = 1, alpha = 0.3) +
    #     geom_jitter( shape = 21,height = .1, aes(size = n_sample,
    #                                              fill = expvar), alpha = 0.8) +
    #     # geom_jitter( shape = 21, color = "white", fill = "gray60",height = .1, aes(size = n_sample), alpha = 0.5) +
    #     geom_linerange(data = mod_means, aes(x = y, xmin = ymin, xmax = ymax), color = "black") +
    #     geom_point(data = mod_means, aes(x = y), size = 3, shape = 21, color = "white", fill = "black") +
    #     guides(fill = guide_legend(override.aes = list(size = 5))) +
    #     theme_minimal() +
    #     scale_fill_brewer(type = "qual", palette = "Set3")
    #

    gplot <- ggplot(
        deviance_list$deviances %>%
            dplyr::mutate(is_spatial = grepl("spatial", mod_group),
                          is_tensor = grepl("_x_", fixed = TRUE, ignore.case = FALSE, mod_group),
                          mod_class = sub("(.*[_]species[_])(.*)", replacement = "\\2", mod_group),
                          mod_group = forcats::fct_relevel(mod_group, stringr::str_sort(levels(mod_group))),
                          expvar = forcats::fct_relevel(
                              expvar,
                              "nullmodel",
                              "soil_nutrients",
                              "baumsch_flaeche",
                              "building_height",
                              "building_height300",
                              "lcz6",
                              "lcz6_300",
                              "urbclim_mod_morning_3_5",
                              "urbclim_mod_afternoon_13_15",
                              "urbclim_mod_night_21_23",
                              "day_2007",
                              "night_2007") %>%
                              forcats::fct_relabel(., .fun = ~gsub("urbclim_mod_", "", x = .x)) %>%
                              forcats::fct_relabel(., .fun = ~gsub("mod2015_T2M", "", x = .x)) %>%
                              forcats::fct_relabel(., .fun = ~gsub("HMEA", " H", x = .x))),
        aes(x = expvar,
            y = deviance_explained * 100)) +
        # geom_line(data = mod_means, aes(x = y),  color = "black", group = 1, alpha = 0.3) +
        # annotate("text", x = c(2.5, 5.5, 8, 10), y = rep(0.82, 4), label = c("no temp", "Urbclim", "Landsat", "Berlin UA")) +
        # annotate("text", x = c(1:4), y = rep(0.82, 4), label = c("bla", "blu", "bli", "kli")) +
        annotate("text", x = c(4), y = rep(80, 1), label = "bold(no~temp)", parse = TRUE) +
        annotate("text", x = c(9),   y = rep(80, 1), label = "bold(Urbclim)", parse = TRUE) +
        annotate("text", x = c(11.5),   y = rep(80, 1), label = "bold(Landsat)", parse = TRUE) +
        annotate("text", x = c(14),  y = rep(80, 1), label = "bold(Berlin~EnvAt)", parse = TRUE) +
        annotate(geom = "rect", xmin = 0.5, xmax = 7.5, ymin = -Inf, ymax = Inf, alpha = 0.3) +
        annotate(geom = "rect", xmin = 7.5, xmax = 10.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "steelblue1") +
        annotate(geom = "rect", xmin = 10.5, xmax = 12.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "darkorange") +
        annotate(geom = "rect", xmin = 12.5, xmax = 15.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "seagreen4") +

        geom_boxplot() +
        geom_jitter(width = .25,
                    aes(size = n_sample,
                        shape = is_tensor,
                        # fill = mod_group),
                        fill = mod_class),
                    alpha = 0.8) +
        # geom_linerange(data = mod_means, aes(x = y, xmin = ymin, xmax = ymax), color = "black") +
        # geom_point(data = mod_means, aes(y = y), size = 3, shape = 21, color = "white", fill = "black") +

        guides(fill = guide_legend(
            override.aes = list(
                size = 5,
                shape = 21
                # shape = c(rep(21, 2),
                #           23,
                #           rep(21, 9),
                #           rep(23, 8))
                ),
            ncol = 2),
            shape = guide_legend(override.aes = list(size = 5))) +
        theme_minimal() +
        theme(legend.position = "top",
              legend.direction = "vertical",
              strip.text = element_text(size = 12)) +
        # scale_fill_brewer(type = "qual", palette = "Set3") +
        scale_shape_manual(values = c("TRUE" = 23, "FALSE" = 21), labels = c("Interaction", "Additive")) +
        scale_fill_manual(values = pals::kelly(n = n_distinct(sub("(.*[_]species[_])(.*)", replacement = "\\2", deviance_list$deviances$mod_group))),
        # scale_fill_manual(values = pals::kelly(n = n_distinct(deviance_list$deviances$mod_group)),
                          aesthetics = c("fill")) +
        scale_x_discrete(guide = guide_axis(n.dodge = 3) ) +
        facet_wrap(~is_spatial, ncol = 1, labeller = labeller(is_spatial = c('TRUE' = "spatial - f(x,y)", 'FALSE' = 'non-spatial')))  +
        labs(x = NULL, y = "Explained Deviance (%)", shape = "Temperature structure")

    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

    return(gplot)
}






#' Create model overview for SI
#'
#' @param file character, file path (use with \code{\link{file_out}}), must include file ending
#' @param height numeric, height in inches
#' @param width numeric, width in inches
#' @param dpi numeric, dpi of output
#' @param deviance_list list from deviance summary
#' @param base_size
#'
#' @return ggplot object
#' @import ggplot2
make_aic_plot <- function(aic_list,
                               base_size = 18,
                               file,
                               height,
                               width,
                               dpi){


    # # grab sample size per model
    # mod_n <- mod_summary_list %>%
    #     purrr::map_depth(2, "summary") %>%
    #     purrr::map_depth(2, "n") %>%
    #     purrr::map_depth(2, .f = as.data.frame) %>%
    #     purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
    #     purrr::map2(names(.),
    #                 ~mutate(.x, mod_group = .y)) %>%
    #     do.call(rbind, .) %>%
    #     `rownames<-`(NULL) %>%
    #     rename(n_sample = 2)
    #
    #
    #
    # # extract deviance and add sample sizes
    # mod_dev <- mod_summary_list %>%
    #     purrr::map_depth(2, "summary") %>%
    #     purrr::map_depth(2, "dev.expl") %>%
    #     purrr::map_depth(2, .f = as.data.frame) %>%
    #     purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
    #     purrr::map2(names(.),
    #                 ~mutate(.x, mod_group = .y)) %>%
    #     do.call(rbind, .) %>%
    #     `rownames<-`(NULL) %>%
    #     dplyr::rename(deviance_explained = 2) %>%
    #     dplyr::left_join(mod_n, by = c("mod_group", 'expvar'))
    #
    # # calc data for points over box plot
    # mod_means <- mod_dev %>%
    #     dplyr::group_by(mod_group) %>%
    #     dplyr::summarise(ggplot2:::mean_se(deviance_explained)) %>%
    #     dplyr::arrange(desc(y))
    #
    # mod_dev$mod_group <- forcats::fct_relevel(mod_dev$mod_group, mod_means$mod_group)
    # mod_means$mod_group <- forcats::fct_relevel(mod_means$mod_group, mod_means$mod_group)

    # ggplot(mod_dev,
    #        aes(y = mod_group,
    #            x = deviance_explained)) +
    #     geom_line(data = mod_means, aes(x = y),  color = "black", group = 1, alpha = 0.3) +
    #     geom_jitter( shape = 21,height = .1, aes(size = n_sample,
    #                                              fill = expvar), alpha = 0.8) +
    #     # geom_jitter( shape = 21, color = "white", fill = "gray60",height = .1, aes(size = n_sample), alpha = 0.5) +
    #     geom_linerange(data = mod_means, aes(x = y, xmin = ymin, xmax = ymax), color = "black") +
    #     geom_point(data = mod_means, aes(x = y), size = 3, shape = 21, color = "white", fill = "black") +
    #     guides(fill = guide_legend(override.aes = list(size = 5))) +
    #     theme_minimal() +
    #     scale_fill_brewer(type = "qual", palette = "Set3")
    #

    gplot <- ggplot(
        aic_list$AIC %>%
            dplyr::mutate(is_spatial = grepl("spatial", mod_group),
                          is_tensor = grepl("_x_", fixed = TRUE, ignore.case = FALSE, mod_group),
                          mod_class = sub("(.*[_]species[_])(.*)", replacement = "\\2", mod_group),
                          mod_group = forcats::fct_relevel(mod_group, stringr::str_sort(levels(mod_group))),
                          expvar = forcats::fct_relevel(
                              expvar,
                              "nullmodel",
                              "soil_nutrients",
                              "baumsch_flaeche",
                              "building_height",
                              "building_height300",
                              "lcz6",
                              "lcz6_300",
                              "urbclim_mod_morning_3_5",
                              "urbclim_mod_afternoon_13_15",
                              "urbclim_mod_night_21_23",
                              "day_2007",
                              "night_2007") %>%
                              forcats::fct_relabel(., .fun = ~gsub("urbclim_mod_", "", x = .x)) %>%
                              forcats::fct_relabel(., .fun = ~gsub("mod2015_T2M", "", x = .x)) %>%
                              forcats::fct_relabel(., .fun = ~gsub("HMEA", " H", x = .x))),
        aes(x = expvar,
            y = AIC)) +
        # geom_line(data = mod_means, aes(x = y),  color = "black", group = 1, alpha = 0.3) +
        # annotate("text", x = c(2.5, 5.5, 8, 10), y = rep(0.82, 4), label = c("no temp", "Urbclim", "Landsat", "Berlin UA")) +
        # annotate("text", x = c(1:4), y = rep(0.82, 4), label = c("bla", "blu", "bli", "kli")) +
        annotate("text", x = c(4), y = rep(80, 1), label = "bold(no~temp)", parse = TRUE) +
        annotate("text", x = c(9),   y = rep(80, 1), label = "bold(Urbclim)", parse = TRUE) +
        annotate("text", x = c(11.5),   y = rep(80, 1), label = "bold(Landsat)", parse = TRUE) +
        annotate("text", x = c(14),  y = rep(80, 1), label = "bold(Berlin~EnvAt)", parse = TRUE) +
        annotate(geom = "rect", xmin = 0.5, xmax = 7.5, ymin = -Inf, ymax = Inf, alpha = 0.3) +
        annotate(geom = "rect", xmin = 7.5, xmax = 10.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "steelblue1") +
        annotate(geom = "rect", xmin = 10.5, xmax = 12.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "darkorange") +
        annotate(geom = "rect", xmin = 12.5, xmax = 15.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "seagreen4") +

        geom_boxplot() +
        geom_jitter(aes(size = nobs,
                        shape = is_tensor,
                        # fill = mod_group),
                        fill = mod_class),
                    position = position_jitter(width = .25, seed = 1),
                    alpha = 0.6) +
        geom_jitter(aes(color = mod_class),
                    size = 0.3,
                    position = position_jitter(width = .25, seed = 1),
                    alpha = 1) +
        # geom_linerange(data = mod_means, aes(x = y, xmin = ymin, xmax = ymax), color = "black") +
        # geom_point(data = mod_means, aes(y = y), size = 3, shape = 21, color = "white", fill = "black") +

        guides(fill = guide_legend(
            override.aes = list(
                size = 5,
                shape = 21
                # shape = c(rep(21, 2),
                #           23,
                #           rep(21, 9),
                #           rep(23, 8))
            ),
            ncol = 2),
            shape = guide_legend(override.aes = list(size = 5))) +
        theme_minimal() +
        theme(legend.position = "top",
              legend.direction = "vertical",
              strip.text = element_text(size = 12)) +
        # scale_fill_brewer(type = "qual", palette = "Set3") +
        scale_shape_manual(values = c("TRUE" = 23, "FALSE" = 21), labels = c("Interaction", "Additive")) +
        scale_fill_manual(values = pals::kelly(n = n_distinct(sub("(.*[_]species[_])(.*)", replacement = "\\2", aic_list$AIC$mod_group))),
                          # scale_fill_manual(values = pals::kelly(n = n_distinct(deviance_list$deviances$mod_group)),
                          aesthetics = c("fill", 'color')) +
        scale_x_discrete(guide = guide_axis(n.dodge = 3) ) +
        facet_wrap(~is_spatial, ncol = 1, labeller = labeller(is_spatial = c('TRUE' = "spatial - f(x,y)", 'FALSE' = 'non-spatial')))  +
        labs(x = NULL, y = "AIC", shape = "Temperature structure")

    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

    return(gplot)
}



#' Make single-temp var plot
#'
#' @param pred_list list, output from `pred_dbh_temp_single_var`
#' @param model_df dframe used for predictions in pred_list
#' @param age_filter character, age groups to exclude
#' @param age_expression expression, used to define age groups
#' @param prediction_range character, within or full?
#' @param base_size numeric
#' @param file path, save plot
#' @param height, width numeric, inches
#' @param dpi numeric
#'
#' @return
#' @export
#'
#' @examples
plot_dbh_temp_single_var <- function(pred_list,
                                     model_df,
                                     age_filter,
                                     age_expression,
                                     base_size = 18,
                                     prediction_range = "full",
                                     file,
                                     height,
                                     width,
                                     dpi){

    `%nin%` <- Negate(`%in%`)

    model_df <- model_df %>%
        mutate(age_group = eval(age_expression),
               species_corrected = shorten_species(species_corrected)) %>%
        tidyr::drop_na(age_group)



    plot_data <- pred_list$pred_groups %>%
        # dplyr::select(-tempvar) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(names(pred_list$pred_var)),
                            names_to = "uhi_tempvar",
                            values_to = "temp_degc") %>%
        # dplyr::filter(prediction_range == prediction_range) %>%
        dplyr::filter(age_group %nin% age_filter) %>%
        dplyr::arrange(uhi_tempvar) %>%
        tidyr::drop_na(temp_degc) %>%
        dplyr::group_by(species_corrected, uhi_tempvar) %>%
        dplyr::mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)),
                      species_corrected = shorten_species(species_corrected))




    gplot <- ggplot(data = plot_data %>%
                      ungroup(),
                  aes(y = response.fit.mean,
                      x = temp_degc,
                      colour = as.factor(age_group),
                      fill =  as.factor(age_group),
                      ymin = response.low.mean,
                      ymax = response.high.mean)) +

        geom_vline(xintercept = 0, linetype = 2, color = "black", alpha = 3/4) +


        stat_density(inherit.aes = FALSE,
                     data = model_df %>%
                         dplyr::filter(age_group %nin% age_filter),
                     aes(x = day_2007,
                         y = ..scaled..*100,
                         fill = as.factor(age_group),),
                     position = "dodge",
                     geom = "area",
                     alpha = 0.4) +


        geom_ribbon(data = plot_data %>% filter(prediction_range == "within"),
                    alpha = 0.5, color = "transparent") +
        # geom_line(color = "black")  +

        geom_smooth(aes(group = 1),
                    formula = y ~ x,
                    method = "lm", fill = "transparent",
                    color = "gray50",
                    data = plot_data %>% filter(prediction_range == "within")) +
        # geom_ribbon(linetype = 1) +


        # geom_line(linetype = 1)  +
        geom_line(linetype = 3, color = "black", size = 0.35)  +
        geom_line(data = plot_data %>% filter(prediction_range == "within")) +



        facet_grid(age_group~species_corrected,
                   scales = "free_y",
                   labeller = label_wrap_gen(width = 3,
                                             multi_line = FALSE)) +

        theme_minimal(base_size = base_size) +
        theme(legend.position = 'top',
              legend.direction = "horizontal",
              panel.spacing = unit(1.25, "lines"),
              strip.text = element_text(size = 12),
              strip.text.y = element_blank()) +

        scale_color_brewer(palette = 2, type = "qual") +
        scale_fill_brewer(palette = 2, type = "qual") +

        labs(color = "Age Group",
             fill = "Age Group",
             x = expression(UHI~Magnitude~(degree*C)),
             y = expression(bar(DBH)~(cm)))


    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)


}





#' Make single-temp var plot for individual species
#'
#' @param pred_list list, output from `pred_dbh_temp_single_var`
#' @param model_df dframe used for predictions in pred_list
#' @param age_filter character, age groups to exclude, using `%nin%`
#' @param species_filter character, age groups to exclude, using `%in%`
#' @param age_expression expression, used to define age groups
#' @param prediction_range character, within or full?
#' @param base_size numeric
#' @param file
#' @param height
#' @param width
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
plot_dbh_temp_single_var_single_species <- function(pred_list,
                                                    model_df,
                                                    age_filter,
                                                    species_filter,
                                                    age_expression,
                                                    base_size = 18,
                                                    prediction_range = "full",
                                                    file,
                                                    height,
                                                    width,
                                                    dpi
){

    if(is.null(species_filter)){
        species_filter <- shorten_species(unique(model_df$species_corrected))
    }


    `%nin%` <- Negate(`%in%`)

    model_df <- model_df %>%
        mutate(age_group = eval(age_expression),
               species_corrected = shorten_species(species_corrected)) %>%
        dplyr::filter(
            age_group %nin% age_filter,
            species_corrected %in% species_filter) %>%
        tidyr::drop_na(age_group)



    plot_data <- pred_list$pred_groups %>%
        # dplyr::select(-tempvar) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(names(pred_list$pred_var)),
                            names_to = "uhi_tempvar",
                            values_to = "temp_degc") %>%
        dplyr::mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)),
                      species_corrected = shorten_species(species_corrected)) %>%
        dplyr::filter(prediction_range == prediction_range) %>%
        dplyr::filter(age_group %nin% age_filter) %>%
        dplyr::filter(species_corrected %in% shorten_species(species_filter)) %>%
        dplyr::arrange(uhi_tempvar) %>%
        tidyr::drop_na(temp_degc) %>%
        dplyr::group_by(species_corrected, age_group)
    # %>%
    #     mutate(
    #         response.fit.mean = response.fit.mean - mean(response.fit.mean),
    #         response.low.mean = response.low.mean - mean(response.low.mean),
    #         response.high.mean = response.high.mean - mean(response.high.mean)
    #         )



    gplot <- ggplot(data = plot_data %>%
                        ungroup(),
                    aes(y = response.fit.mean,
                        x = temp_degc,
                        colour = as.factor(species_corrected),
                        fill =  as.factor(species_corrected),
                        ymin = response.low.mean,
                        ymax = response.high.mean)) +


        # stat_density(inherit.aes = FALSE,
        #              data = model_df,
        #              aes(x = day_2007,
        #                  y = ..scaled..*100,
        #                  fill = as.factor(species_corrected)),
        #              position = "dodge",
        #              geom = "area",
        #              alpha = 0.2) +


        geom_ribbon(data = plot_data %>% filter(prediction_range == "within"),
                    alpha = 0.2, color = "transparent") +
        # geom_line(color = "black")  +

        geom_smooth(aes(group = species_corrected),
                    formula = y ~ x,
                    method = "lm",
                    # fill = "transparent",
                    # color = "gray50",
                    data = plot_data %>% filter(prediction_range == "within")) +
        # geom_ribbon(linetype = 1) +


        # geom_line(linetype = 3, color = "black", size = 0.35, alpha = 0.2)  +
        # geom_line(data = plot_data %>% filter(prediction_range == "within"),
        #           alpha = 0.2) +


        #
        facet_grid(~age_group) +

        theme_minimal(base_size = base_size) +
        theme(legend.position = 'top',
              legend.direction = "horizontal",
              panel.spacing = unit(2, "lines"),
              strip.text = element_text(size = 12)) +

        # scale_color_brewer(palette = 2, type = "qual") +
        # scale_fill_brewer(palette = 2, type = "qual") +

        scale_fill_manual(
            values = pals::tol(n = n_distinct(plot_data$species_corrected)),
            aesthetics = c("fill", "color")) +


        labs(color = "Species",
             fill = "Species",
             x = expression(UHI~Magnitude~(degree*C)),
             y = expression(bar(DBH)~(cm)))



    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)


}


#' Make single-temp var plot for individual species
#'
#' @param pred_list list, output from `pred_dbh_temp_single_var`
#' @param model_df dframe used for predictions in pred_list
#' @param age_filter character, age groups to exclude, using `%nin%`
#' @param species_filter character, age groups to exclude, using `%in%`
#' @param age_expression expression, used to define age groups
#' @param prediction_range character, within or full?
#' @param base_size numeric
#' @param file
#' @param height
#' @param width
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
plot_dbh_temp_single_var_flex <- function(pred_list,
                                          model_df,
                                          var,
                                          age_filter,
                                          species_filter,
                                          age_expression,
                                          base_size = 18,
                                          prediction_range = "full",
                                          file,
                                          x_label,
                                          height,
                                          width,
                                          dpi
){

    `%nin%` <- Negate(`%in%`)
    var <- rlang::sym(var)

    if(is.null(species_filter)){
        species_filter <- unique(model_df$species_corrected)
    }


    model_df <- model_df %>%
        dplyr::filter(age_group %nin% age_filter,
                      species_corrected %in% species_filter) %>%
        mutate(age_group = eval(age_expression),
               species_corrected = shorten_species(species_corrected)) %>%
        tidyr::drop_na(age_group)


    plot_data <- pred_list$pred_groups %>%
        # dplyr::select(-tempvar) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(names(pred_list$pred_var)),
                            names_to = "uhi_tempvar",
                            values_to = "temp_degc") %>%
        dplyr::filter(prediction_range == prediction_range) %>%
        dplyr::filter(age_group %nin% age_filter) %>%
        dplyr::filter(species_corrected %in% species_filter) %>%
        dplyr::arrange(uhi_tempvar) %>%
        tidyr::drop_na(temp_degc) %>%
        dplyr::group_by(species_corrected, uhi_tempvar) %>%
        dplyr::mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)),
                      species_corrected = shorten_species(species_corrected))



    #
    gplot <- ggplot(data = plot_data %>%
                        ungroup(),
                    aes(y = response.fit.mean,
                        x = !!var,
                        colour = as.factor(species_corrected),
                        fill =  as.factor(species_corrected),
                        ymin = response.low.mean,
                        ymax = response.high.mean)) +


        # stat_density(inherit.aes = FALSE,
        #              data = model_df,
        #              aes(x = day_2007,
        #                  y = ..scaled..*100,
        #                  fill = as.factor(species_corrected)),
        #              position = "dodge",
        #              geom = "area",
        #              alpha = 0.2) +


    geom_ribbon( alpha = 0.5, color = "transparent") +
        # geom_line(color = "black")  +

        geom_smooth(aes(group = species_corrected),
                    formula = y ~ x,
                    method = "lm", fill = "transparent",
                    color = "gray50") +
        # geom_ribbon(linetype = 1) +


        # geom_line(linetype = 1)  +
        geom_line()  +

        #
        facet_grid(~age_group) +

        lims(y = c(20, 35)) +

        theme_minimal(base_size = base_size) +
        theme(legend.position = 'top',
              legend.direction = "horizontal",
              panel.spacing = unit(2, "lines"),
              strip.text = element_text(size = 12)) +

        scale_color_brewer(palette = 2, type = "qual") +
        scale_fill_brewer(palette = 2, type = "qual") +

        labs(color = "Species",
             fill = "Species",
             x = x_label,
             y = expression(bar(DBH)~(cm)))



    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)


}



#' Title
#'
#' @param moran_summary
#' @param file
#' @param height
#' @param width
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
plot_moran_comparison <- function(moran_summary,
                                  base_size = 18,
                                  file,
                                  height,
                                  width,
                                  dpi){


    gplot <- moran_summary %>%
        mutate(spatial_model = dplyr::case_when(spatial_model == "spat" ~ "spatial - f(x,y)",
                                                spatial_model == "non_spat" ~ "non-spatial",
                                                TRUE ~ NA_character_)) %>%
        dplyr::filter(vars %in% c('observed', 'expected')) %>%
        ggplot(aes(x = grid_area , y = value, fill = spatial_model)) +
        geom_bar(position = "dodge", stat = "identity") +
        facet_wrap(~model) +
        labs(y = "Moran's I", x = "Grid Area", fill = "Model type") +
        scale_fill_brewer(type = "qual", palette = 2) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        theme_minimal(base_size = base_size)



    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

}



#' Observed vs. Predicted for given model
#'
#' @param path_model
#' @param base_size
#' @param file
#' @param height
#' @param width
#' @param dpi
#'
plot_obs_predicted_model <- function(path_model,
                                     base_size = 18,
                                     file,
                                     height,
                                     width,
                                     dpi){

    mod <- readRDS(path_model)


    mod_broomed <- broom::augment(mod) %>%
        dplyr::mutate(predicted = mod$family$linkinv(.fitted))

    mod_preds <- lm(dbh_cm ~ predicted, data = mod_broomed)

    adjr <- format(summary(mod_preds)$adj.r.squared, digits = 3)

    mod_rmse <- mod_preds %>%
        broom::augment() %>%
        dplyr::summarise(sqrt(sum(.fitted - dbh_cm)^2/dplyr::n())) %>%
        as.numeric() %>%
        format(digits = 2)

    mod_mae <- mod_preds %>%
        broom::augment() %>%
        dplyr::summarise(mean(abs(.fitted - dbh_cm))) %>%
        as.numeric() %>%
        round(2)

    ifun <- Gamma(link = "log")$linkinv

    gplot <- mod_broomed %>%
        ggplot(aes(x = predicted, y = dbh_cm)) +
        geom_hex(bins = 75, color = "transparent") +
        geom_smooth(method = "lm", formula = "y ~ x", color = "red") +
        geom_abline(slope = 1, intercept = 0, linetype = 2, color = "gray40") +
        scale_fill_viridis_c(option = "plasma") +
        theme_minimal(base_size = 18) +
        labs(x = "Predicted", y = "Observed", fill = "N") +
        annotate("text",
                 x = 50,
                 y = 200,
                 label = as.expression(bquote(R[adj]^2~'='~.(adjr)~~~~RMSE~'='~.(mod_rmse)~~~~MAE~'='~.(mod_mae))))


    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

    return(gplot)


}



#' Make single-temp var plot for individual species
#'
#' @param pred_list list, output from `pred_dbh_temp_single_var`
#' @param model_df dframe used for predictions in pred_list
#' @param age_filter character, age groups to exclude, using `%nin%`
#' @param species_filter character, age groups to exclude, using `%in%`
#' @param age_expression expression, used to define age groups
#' @param prediction_range character, within or full?
#' @param base_size numeric
#'
#' @return list with plot and lms of slopes for temp sensitivities
#' @export
#'
#' @examples
plot_dbh_temp_comparison <- function(
    pred_list,
    model_df,
    age_filter,
    species_filter,
    age_expression,
    base_size = 18,
    prediction_range = "full"
){

    if(is.null(species_filter)){
        species_filter <- shorten_species(unique(model_df$species_corrected))
    }


    `%nin%` <- Negate(`%in%`)

    model_df <- model_df %>%
        mutate(age_group = eval(age_expression),
               species_corrected = shorten_species(species_corrected) ) %>%
        dplyr::filter(
            age_group %nin% age_filter,
            species_corrected %in% species_filter) %>%
        tidyr::drop_na(age_group)



    plot_data <- pred_list$pred_groups %>%
        filter(prediction_range == "within") %>%
        # dplyr::select(-tempvar) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(names(pred_list$pred_var)),
                            names_to = "uhi_tempvar",
                            values_to = "temp_degc") %>%
        dplyr::mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)),
                      species_corrected = shorten_species(species_corrected)) %>%
        dplyr::filter(prediction_range == prediction_range) %>%
        dplyr::filter(age_group %nin% age_filter) %>%
        dplyr::filter(species_corrected %in% shorten_species(species_filter)) %>%
        dplyr::mutate(species_corrected = species_corrected %>%
                          as.character(),
                      species_corrected = ifelse(
                          species_corrected == "A. pseudo- platanus",
                          "A. pseudoplatanus",
                          species_corrected)) %>%
        dplyr::arrange(uhi_tempvar) %>%
        tidyr::drop_na(temp_degc) %>%
        dplyr::group_by(species_corrected, age_group) %>%
        mutate(age_species_group_mean = mean(response.fit.mean),
             response.mean.unadj = response.fit.mean,
            response.fit.mean = response.fit.mean - mean(response.fit.mean),
            response.low.mean = response.low.mean - mean(response.low.mean),
            response.high.mean = response.high.mean - mean(response.high.mean)
        )




    lms <- plot_data %>%
        tidyr::nest(data = -c(species_corrected, age_group)) %>%
        mutate(models = purrr::map(data,
                                   ~lm(response.fit.mean ~ temp_degc,
                                       data = .)),
               estimates = purrr::map(models,
                                      broom::tidy),
               goodness = purrr::map(models,
                                     broom::glance)
               )


    zero_val <- lms %>%
        dplyr::select(species_corrected, age_group, estimates) %>%
        tidyr::unnest(cols = estimates) %>%
        dplyr::select(species_corrected, age_group, term, estimate) %>%
        tidyr::pivot_wider(names_from = term,
                           values_from = estimate) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(zero_threshold_degc = (0 - `(Intercept)`) / temp_degc) %>%
        dplyr::select(-c(`(Intercept)`, zero_threshold_degc)) %>%
        tidyr::pivot_wider(names_from = age_group, values_from = temp_degc)





    gplot <- ggplot(data = plot_data %>%
                        ungroup(),
                    aes(y = response.fit.mean,
                        x = temp_degc,
                        colour = as.factor(species_corrected),
                        fill =  as.factor(species_corrected),
                        ymin = response.low.mean,
                        ymax = response.high.mean)) +


    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +

        geom_smooth(aes(group = species_corrected),
                    formula = y ~ x,
                    method = "lm",
                    # fill = "transparent",
                    # color = "gray50",
                    data = plot_data %>% filter(prediction_range == "within"),
                    alpha = 0.15, size = 0) +
        geom_line(stat = "smooth",
                  aes(group = species_corrected),
                  formula = y ~ x,
                  method = "lm",
                  # fill = "transparent",
                  # color = "gray50",
                  data = plot_data %>% filter(prediction_range == "within"),
                  alpha = 0.75) +


        # geom_ribbon(linetype = 1) +


        # geom_line(linetype = 3, color = "black", size = 0.35, alpha = 0.2)  +
        # geom_line(data = plot_data %>% filter(prediction_range == "within"),
        #           alpha = 0.2) +


        #
    facet_wrap(~age_group, scales = "free_y", nrow = 1) +

        theme_minimal(base_size = base_size) +
        theme(legend.position = 'top',
              legend.direction = "horizontal",
              panel.spacing = unit(2, "lines"),
              strip.text = element_text(size = 12)) +

        # scale_color_brewer(palette = 2, type = "qual") +
        # scale_fill_brewer(palette = 2, type = "qual") +

        scale_fill_manual(
            values = pals::tol(n = n_distinct(plot_data$species_corrected)),
            aesthetics = c("fill", "color")) +


        labs(color = "Species",
             fill = "Species",
             x = expression(UHI~Magnitude~(degree*C)),
             y = expression(bar(DBH)[centered]~(cm)))



    zero_val_long <- zero_val %>%
        # dplyr::mutate(rown = row_number()) %>%
        dplyr::mutate(rown = 1) %>%
        tidyr::pivot_longer(cols = -c(species_corrected, rown), names_to = 'age', values_to = "estimate")

    estimate_plot <- ggplot(zero_val_long) +
        geom_tile(aes(y = species_corrected,
                      x = rown,
                      fill = estimate),
                  show.legend = FALSE) +
        geom_text(aes(y = species_corrected,
                      x = rown,
                      label = round(estimate, 2))) +
        facet_wrap(~age) +
        ggplot2::scale_fill_distiller(palette = "RdBu",
                                      rescaler = mid_rescaler(),
                                      na.value = "transparent") +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::labs(fill = "Slope Estimate") +
        ggplot2::theme(legend.position = "bottom",
              legend.dir = "horizontal",
              axis.title = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(face = "italic")) +
        ggplot2::guides(fill = guide_colorbar(barwidth = unit(3.5, "cm"),
                                              title.vjust = 1,
                                              ticks.colour = "gray30"))


    # plot_dbh_sens_lms$plot + plot_dbh_sens_lms$est_plot + plot_layout(guides = "collect", widths = c(0.7, 0.3)) & theme(legend.position='bottom', legend.box = 'vertical')

    return(list(plot = gplot, est_plot = estimate_plot, lms = lms, df = plot_data, estimates = zero_val))

}











#' Pairs Plot
#'
#' @param dframe data.frame, model df
#' @param var_names character, names of variables to include in pairs plot
#' @param ... additional arguments to pair plot
#'
#' @return NULL
plot_pairs <- function(dframe,
                       var_names,
                       ...){


    usr <- par("usr"); on.exit(par(usr))
    # define lm function with R2
    panel.lm <- function(x, y, digits = 2, prefix = "", cex.r2)
    {
        usr <- par("usr"); on.exit(par(usr))
        lms <- lm(y ~ x)
        txt <- round(summary(lms)$r.squared, 3)

        # if(missing(cex.r2)) cex.r2 <- 0.8/strwidth(txt)
        points(y ~ x)
        abline(lms, col = "red")
        par(usr = c(0, 1, 0, 1))
        text(0.55, 0.25, bquote(R^2~'='~.(txt)), cex = 2, col = "red")
    }

    # label
    panel.text <- function(x, y, labels, cex, font)
    {
        usr <- par("usr"); on.exit(par(usr))

        # if(missing(cex.r2)) cex.r2 <- 0.8/strwidth(txt)
        par(usr = c(0, 1, 0, 1))
        text(0.5, 0.5, labels, col = "gray40", cex = 0.75)
    }


    dframe %>%
        dplyr::select(dplyr::contains(var_names)) %>%
        pairs(lower.panel = panel.lm,
              upper.panel = NULL,
              text.panel = panel.text,
              ...)

}


#' Make and save pair plots
#'
#' @param dframe data.frame, model df
#' @param varlabs, named list of lists with items var and main, i.e. list(morning = list(var = 'x', main = "y"))
#' @param ... additional arguments to plot pairs
#' @param res,width,height numeric, details for plotting
#' @param units character, centimeter?
#' @param path_out character, path to dir
#'

#' @examples
make_plot_pairs <- function(dframe,
                            varlabs =  list(afternoon = list(var = c("T2M14", "mod_afternoon", "day_2007"),
                                                             main = "Air Temperature / UHI Measure: \n Afternoon"),
                                            night = list(var = c("T2M22", "mod_night", "night_2007"),
                                                         main = "Air Temperature / UHI Measure: \n Night"),
                                            morning = list(var = c("T2M04", "mod_afternoon"),
                                                           main = "Air Temperature / UHI Measure: \n Morning")),
                            ...,
                            res = 300,
                            width = 12,
                            height = 12,
                            units = "cm",
                            path_out){



    purrr::map2(varlabs,
                names(varlabs),
                function(x,y){

                    path_out <- fs::path(path_out, sprintf("fig-tempvar_comparison-%s.png", y))

                    png(filename = path_out,
                        width = width,
                        height = height,
                        units = units,
                        res = res)
                    plot_pairs(dframe,
                               var_names = x$var,
                               main = x$main,
                               oma = c(2,2,7,2))
                    dev.off()

                })

}




#' BIWI Generate growth ~ age plot
#'
#' @param biwi_preds predictions from biwi gam
#' @param base_size
#' @param file
#' @param height
#' @param width
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
make_biwi_plot <- function(biwi_preds,
                           base_size = 18,
                           file,
                           height,
                           width,
                           dpi){




    # helper for error propagation
    serror <- function(x){

        return(sqrt(sum(x^2)/length(x)))


        # sqrt(sum(pred.se.fit^2))/n())

    }

    means <- biwi_preds %>%
        mutate(year_break = as.factor(ifelse(year <= 1960, "1920 - 1960", "1961 - 2001"))) %>%
        group_by(cambial_age, year_break) %>%
        summarize(mean_link = mean(fit),
                  mean_fit_response = Gamma(link="log")$linkinv(mean_link),
                  mean_se_response_low = Gamma(link="log")$linkinv(mean_link - 1.96 * serror(se.fit)),
                  mean_se_response_high = Gamma(link="log")$linkinv(mean_link + 1.96 * serror(se.fit)))


    gplot <-     ggplot(biwi_preds,
                    aes(x = cambial_age,
                        y = Gamma(link="log")$linkinv(fit),
                        color = year_break,
                        group = year)) +
        geom_line(alpha = 0.2) +
        geom_ribbon(data = means,
                    inherit.aes = FALSE,
                    aes(x = cambial_age, y = mean_fit_response,
                        ymin = mean_se_response_low,
                        ymax = mean_se_response_high,
                        color = year_break,
                        fill = year_break,
                        group = year_break),
                    alpha = 0.35,
                    show.legend = FALSE) +
        geom_line(data = means,
                  inherit.aes = FALSE,
                  aes(x = cambial_age,
                      y = mean_fit_response,
                      group = year_break,
                      color = year_break),
                  size = 1) +
        labs(x = "Cambial Age (a)",
             y = "Annual Growth (mm)",
             color = NULL,
             fill = NULL) +
        # lims(y = c(0, 7)) +
        # scale_fill_brewer(type = "qual", palette = 2) +
        # scale_color_brewer(type = "qual", palette = 2) +
        scale_color_manual(values = pals::tol(2), aesthetics = c("fill", "color")) +
        theme_minimal(base_size = 18) +
        theme(legend.position = c(0.75, y = 0.75))




    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

    return(file)

}




## HICAM report ---------------------------------------------------------



plot_dbh_temp_single_var_GG <- function(pred_list,
                                     model_df,
                                     age_filter,
                                     age_expression,
                                     base_size = 18,
                                     prediction_range = "full"){

    `%nin%` <- Negate(`%in%`)

    model_df <- model_df %>%
        mutate(age_group = eval(age_expression),
               species_corrected = shorten_species(species_corrected) %>%
                   as.character()) %>%
        tidyr::drop_na(age_group) %>%
        dplyr::mutate(species_corrected =
                          dplyr::case_when(
                              species_corrected == "A. hippocastanum" ~ "A. hippo- castanum",
                              species_corrected == "A. platanoides" ~ "A. plata- noides",
                              species_corrected == "A. pseudoplatanus" ~ "A. pseudopla- tanus",
                              species_corrected == "T. platyphyllos" ~ "T. platy- phyllos",
                              TRUE ~ species_corrected))




    plot_data <- pred_list$pred_groups %>%
        # dplyr::select(-tempvar) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(names(pred_list$pred_var)),
                            names_to = "uhi_tempvar",
                            values_to = "temp_degc") %>%
        # dplyr::filter(prediction_range == prediction_range) %>%
        dplyr::filter(age_group %nin% age_filter) %>%
        dplyr::arrange(uhi_tempvar) %>%
        tidyr::drop_na(temp_degc) %>%
        dplyr::group_by(species_corrected, uhi_tempvar) %>%
        dplyr::mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)),
                      species_corrected = shorten_species(species_corrected) %>%
                          as.character(),
                      species_corrected =
                          dplyr::case_when(
                              species_corrected == "A. hippocastanum" ~ "A. hippo- castanum",
                              species_corrected == "A. platanoides" ~ "A. plata- noides",
                              species_corrected == "A. pseudoplatanus" ~ "A. pseudopla- tanus",
                              species_corrected == "T. platyphyllos" ~ "T. platy- phyllos",
                              TRUE ~ species_corrected))






    gplot <- ggplot(data = plot_data %>%
                      ungroup(),
                  aes(y = response.fit.mean,
                      x = temp_degc,
                      colour = as.factor(age_group),
                      fill =  as.factor(age_group),
                      ymin = response.low.mean,
                      ymax = response.high.mean)) +


        stat_density(inherit.aes = FALSE,
                     data = model_df %>%
                         dplyr::filter(age_group %nin% age_filter),
                     aes(x = day_2007,
                         y = ..scaled..*100,
                         fill = as.factor(age_group),),
                     position = "dodge",
                     geom = "area",
                     alpha = 0.4) +


        geom_ribbon(data = plot_data %>% filter(prediction_range == "within"),
                    alpha = 0.5, color = "transparent") +
        # geom_line(color = "black")  +

        geom_smooth(aes(group = 1),
                    formula = y ~ x,
                    method = "lm", fill = "transparent",
                    color = "gray50",
                    data = plot_data %>% filter(prediction_range == "within")) +
        # geom_ribbon(linetype = 1) +


        # geom_line(linetype = 1)  +
        geom_line(linetype = 3, color = "black", size = 0.35)  +
        geom_line(data = plot_data %>% filter(prediction_range == "within")) +



        facet_grid(age_group~species_corrected,
                   scales = "free_y",
                   labeller = label_wrap_gen(width = 10,
                                             multi_line = FALSE)) +

        scale_x_continuous(breaks = c(-4, 0, 5)) +

        theme_minimal(base_size = base_size) +
        theme(legend.position = 'top',
              legend.direction = "horizontal",
              panel.spacing = unit(0.1, "in"),
              strip.text = element_text(size = 12)) +

        scale_color_brewer(palette = 2, type = "qual", guide = NULL) +
        scale_fill_brewer(palette = 2, type = "qual", guide = NULL) +

        labs(color = "Age Group",
             fill = "Age Group",
             x = expression(UHI~Magnitude~(degree*C)),
             y = expression(bar(DBH)~(cm)))


    return(gplot)


}

#' Make single-temp var plot for individual species
#'
#' @param pred_list list, output from `pred_dbh_temp_single_var`
#' @param model_df dframe used for predictions in pred_list
#' @param age_filter character, age groups to exclude, using `%nin%`
#' @param species_filter character, age groups to exclude, using `%in%`
#' @param age_expression expression, used to define age groups
#' @param prediction_range character, within or full?
#' @param base_size numeric
#' @param file
#' @param height
#' @param width
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
plot_dbh_temp_single_var_single_species_GG <- function(pred_list,
                                                    model_df,
                                                    age_filter,
                                                    species_filter,
                                                    age_expression,
                                                    base_size = 18,
                                                    prediction_range = "full"
){

    if(is.null(species_filter)){
        species_filter <- shorten_species(unique(model_df$species_corrected))
    }


    `%nin%` <- Negate(`%in%`)

    model_df <- model_df %>%
        mutate(age_group = eval(age_expression),
               species_corrected = shorten_species(species_corrected) ) %>%
        dplyr::filter(
            age_group %nin% age_filter,
            species_corrected %in% species_filter) %>%
        tidyr::drop_na(age_group)



    plot_data <- pred_list$pred_groups %>%
        # dplyr::select(-tempvar) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(names(pred_list$pred_var)),
                            names_to = "uhi_tempvar",
                            values_to = "temp_degc") %>%
        dplyr::mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)),
                      species_corrected = shorten_species(species_corrected)) %>%
        dplyr::filter(prediction_range == prediction_range) %>%
        dplyr::filter(age_group %nin% age_filter) %>%
        dplyr::filter(species_corrected %in% shorten_species(species_filter)) %>%
        dplyr::mutate(species_corrected = species_corrected %>%
                          as.character(),
                      species_corrected = ifelse(
            species_corrected == "A. pseudo- platanus",
            "A. pseudoplatanus",
            species_corrected)) %>%
        dplyr::arrange(uhi_tempvar) %>%
        tidyr::drop_na(temp_degc) %>%
        dplyr::group_by(species_corrected, age_group) %>%
        mutate(
            response.fit.mean = response.fit.mean - mean(response.fit.mean),
            response.low.mean = response.low.mean - mean(response.low.mean),
            response.high.mean = response.high.mean - mean(response.high.mean)
            )



    gplot <- ggplot(data = plot_data %>%
                        ungroup(),
                    aes(y = response.fit.mean,
                        x = temp_degc,
                        colour = as.factor(species_corrected),
                        fill =  as.factor(species_corrected),
                        ymin = response.low.mean,
                        ymax = response.high.mean)) +


        # stat_density(inherit.aes = FALSE,
        #              data = model_df,
        #              aes(x = day_2007,
        #                  y = ..scaled..*100,
        #                  fill = as.factor(species_corrected)),
        #              position = "dodge",
        #              geom = "area",
        #              alpha = 0.2) +


    # geom_ribbon(data = plot_data %>% filter(prediction_range == "within"),
    #             alpha = 0.2, color = "transparent") +
        # geom_line(color = "black")  +


    geom_vline(xintercept = 0, linetype = 2) +

        geom_smooth(aes(group = species_corrected),
                    formula = y ~ x,
                    method = "lm",
                    # fill = "transparent",
                    # color = "gray50",
                    data = plot_data %>% filter(prediction_range == "within"),
                    alpha = 0.15, size = 0) +
        geom_line(stat = "smooth",
                  aes(group = species_corrected),
                  formula = y ~ x,
                  method = "lm",
                  # fill = "transparent",
                  # color = "gray50",
                  data = plot_data %>% filter(prediction_range == "within"),
                  alpha = 0.75) +


        # geom_ribbon(linetype = 1) +


        # geom_line(linetype = 3, color = "black", size = 0.35, alpha = 0.2)  +
        # geom_line(data = plot_data %>% filter(prediction_range == "within"),
        #           alpha = 0.2) +


        #
        facet_wrap(~age_group, scales = "free_y", nrow = 1) +

        theme_minimal(base_size = base_size) +
        theme(legend.position = 'top',
              legend.direction = "horizontal",
              panel.spacing = unit(2, "lines"),
              strip.text = element_text(size = 12)) +

        # scale_color_brewer(palette = 2, type = "qual") +
        # scale_fill_brewer(palette = 2, type = "qual") +

        scale_fill_manual(
            values = pals::tol(n = n_distinct(plot_data$species_corrected)),
            aesthetics = c("fill", "color")) +


        labs(color = "Species",
             fill = "Species",
             x = expression(UHI~Magnitude~(degree*C)),
             y = expression(bar(DBH)[centered]~(cm)))



    return(gplot)

}


plot_hicam_species_overview <- function(plot_ages,
                                   plot_comparison,
                                   path_out,
                                   width,
                                   height,
                                   dpi){
    gplot <- plot_ages / plot_comparison +
        theme(legend.margin = margin(r = 5),
              legend.title = element_blank()) +
        plot_layout(heights = c(8,2)) +
        plot_annotation(tag_levels = "A")


    ggsave(filename = path_out,
           plot = gplot,
           width = width, height = height, units = "in", dpi = dpi)
    # ggsave(path_out, width = 12, height = 15, units = "in", dpi = 300)

}


#' Plot UHI with Berlin districts
#'
#' @param uhi_stacks Rasterstack lists for UHI
#' @param berlin_poly Berlin District Polygon
#' @param dsf sf df, Berlin trees
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
make_uhi_plot_hicam <- function(uhi_stacks,
                          berlin_poly,
                          dsf,
                          base_size){

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

        ggplot2::geom_sf(data = dsf %>%
                             dplyr::filter(provenance == "s_wfs_baumbestand") %>%
                             dplyr::slice_sample(prop = 0.05),
                         color = "black",
                         size = 0.05,
                         alpha = 0.1) +

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



    return(p)


}




#' Make single-temp var plot for individual species
#'
#' @param pred_list list, output from `pred_dbh_temp_single_var`
#' @param model_df dframe used for predictions in pred_list
#' @param age_filter character, age groups to exclude, using `%nin%`
#' @param species_filter character, age groups to exclude, using `%in%`
#' @param age_expression expression, used to define age groups
#' @param prediction_range character, within or full?
#' @param base_size numeric
#' @param file
#' @param height
#' @param width
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
plot_dbh_temp_single_var_flex_hicam <- function(pred_list,
                                          model_df,
                                          var,
                                          age_filter,
                                          species_filter,
                                          age_expression,
                                          base_size = 18,
                                          prediction_range = "full",
                                          x_label
){

    `%nin%` <- Negate(`%in%`)
    var <- rlang::sym(var)

    if(is.null(species_filter)){
        species_filter <- unique(model_df$species_corrected)
    }


    model_df <- model_df %>%
        dplyr::filter(age_group %nin% age_filter,
                      species_corrected %in% species_filter) %>%
        mutate(age_group = eval(age_expression),
               species_corrected = shorten_species(species_corrected)) %>%
        tidyr::drop_na(age_group)


    plot_data <- pred_list$pred_groups %>%
        # dplyr::select(-tempvar) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(names(pred_list$pred_var)),
                            names_to = "uhi_tempvar",
                            values_to = "temp_degc") %>%
        dplyr::filter(prediction_range == prediction_range) %>%
        dplyr::filter(age_group %nin% age_filter) %>%
        dplyr::filter(species_corrected %in% species_filter) %>%
        dplyr::arrange(uhi_tempvar) %>%
        tidyr::drop_na(temp_degc) %>%
        dplyr::group_by(species_corrected, uhi_tempvar) %>%
        dplyr::mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)),
                      species_corrected = shorten_species(species_corrected))



    #
    gplot <- ggplot(data = plot_data %>%
                        ungroup(),
                    aes(y = response.fit.mean,
                        x = !!var,
                        # colour = as.factor(species_corrected),
                        # fill =  as.factor(species_corrected),
                        ymin = response.low.mean,
                        ymax = response.high.mean)) +


        # stat_density(inherit.aes = FALSE,
        #              data = model_df,
        #              aes(x = day_2007,
        #                  y = ..scaled..*100,
        #                  fill = as.factor(species_corrected)),
        #              position = "dodge",
        #              geom = "area",
        #              alpha = 0.2) +


    geom_ribbon( alpha = 0.5, color = "transparent") +
        # geom_line(color = "black")  +

        geom_smooth(aes(group = species_corrected),
                    formula = y ~ x,
                    method = "lm", fill = "transparent",
                    color = "gray50") +
        # geom_ribbon(linetype = 1) +


        # geom_line(linetype = 1)  +
        geom_line()  +

        #
        # facet_grid(~age_group) +

        lims(y = c(20, 35)) +

        theme_minimal(base_size = base_size) +
        theme(legend.position = 'top',
              legend.direction = "horizontal",
              panel.spacing = unit(2, "lines"),
              strip.text = element_text(size = 12)) +

        # scale_color_brewer(palette = 2, type = "qual") +
        # scale_fill_brewer(palette = 2, type = "qual") +

        labs(color = "Species",
             fill = "Species",
             x = x_label,
             y = expression(bar(DBH)~(cm)))

    return(gplot)


}



plot_general_results <- function(uhi_map,
                                 obs_pred,
                                 env_pred,
                                 width,
                                 height,
                                 dpi,
                                 file
){



    p <- uhi_map + theme(panel.grid = element_blank(),
                         axis.text = element_blank()) +
        obs_pred +
        env_pred +
        annotate("text", x = 0.2, y = 34.6,label =  "[30 - 35]", size = 4) +
        plot_annotation(tag_levels = "A") +
        plot_layout(nrow = 1)


    ggsave(file, plot = p, width = width, height = height, units = 'in')

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
    age_df <- data.table(df)[provenance == "s_wfs_baumbestand" , .(STANDALTER, gattung_short)][
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


#' Returns df table of proportionial LCZ cover
#'
#' @param wudapt_path
#' @param wudapt_desc_path
#' @param berlin_poly_path
#'
#' @return df with covers
make_wudapt_landcover_table <- function(wudapt_path, wudapt_desc_path, berlin_poly_path){

    wudapt <- terra::rast(wudapt_path)
    berlin_poly <- terra::vect(berlin_poly_path) %>%
        terra::project(wudapt)

    wudapt_desc <- read.csv(wudapt_desc_path, header = FALSE, skip = 2, stringsAsFactors = FALSE) %>%
        setNames(nm = c("class", "r", "g", "b", "alpha", "name")) %>%
        dplyr::mutate(class = as.factor(class))

    lcz_berlin <- wudapt %>%
        terra::crop(berlin_poly) %>%
        terra::mask(berlin_poly)


    lcz_prop <- lcz_berlin %>%
        terra::values() %>%
        table() %>%
        prop.table() %>%
        round(3) %>%
        as.data.frame() %>%
        setNames(nm = c("class", "freq")) %>%
        mutate(class = as.factor(class)) %>%
        dplyr::left_join(x = wudapt_desc, y = .) %>%
        dplyr::arrange(dplyr::desc(freq))


    return(lcz_prop)

}




# helpers -----------------------------------------------------------------


#' Rescale color range
#'
#' @param mid
mid_rescaler <- function(mid = 0) {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
        scales::rescale_mid(x, to, from, mid)
    }
}


#' Extract relevant metrics from model summary list
#'
#' @param mod_summary_list list of model summaries
#'
#' @return list, contains
#' @export
extract_mod_deviance <- function(mod_summary_list){
    # grab sample size per model

    message('extracting deviance \n')

    mod_n <- mod_summary_list %>%
        purrr::map_depth(2, "summary") %>%
        purrr::map_depth(2, "n") %>%
        purrr::map_depth(2, .f = as.data.frame) %>%
        purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
        purrr::map2(names(.),
                    ~mutate(.x, mod_group = .y)) %>%
        do.call(rbind, .) %>%
        `rownames<-`(NULL) %>%
        rename(n_sample = 2)



    # extract deviance and add sample sizes
    mod_dev <- mod_summary_list %>%
        purrr::map_depth(2, "summary") %>%
        purrr::map_depth(2, "dev.expl") %>%
        purrr::map_depth(2, .f = as.data.frame) %>%
        purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
        purrr::map2(names(.),
                    ~mutate(.x, mod_group = .y)) %>%
        do.call(rbind, .) %>%
        `rownames<-`(NULL) %>%
        dplyr::rename(deviance_explained = 2) %>%
        dplyr::left_join(mod_n, by = c("mod_group", 'expvar'))

    # calc data for points over box plot
    mod_means <- mod_dev %>%
        dplyr::group_by(mod_group) %>%
        dplyr::summarise(ggplot2:::mean_se(deviance_explained)) %>%
        dplyr::arrange(desc(y))

    mod_dev$mod_group <- forcats::fct_relevel(mod_dev$mod_group, mod_means$mod_group)
    mod_means$mod_group <- forcats::fct_relevel(mod_means$mod_group, mod_means$mod_group)

    return(list(deviances = mod_dev, variables_means = mod_means))


}




#' Extract relevant metrics from model summary list
#'
#' @param mod_summary_list list of model summaries
#'
#' @return list, contains
#' @export
extract_mod_AIC <- function(mod_summary_list){




    # extract AIC and add sample sizes
    mod_aic <- mod_summary_list %>%
        purrr::map_depth(2, "summary_broom") %>%
        purrr::map_depth(2, .f = as.data.frame) %>%
        purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
        purrr::map2(names(.),
                    ~mutate(.x, mod_group = .y)) %>%
        do.call(rbind, .) %>%
        `rownames<-`(NULL)

    # calc data for points over box plot
    mod_means <- mod_aic %>%
        dplyr::group_by(mod_group) %>%
        dplyr::summarise(ggplot2:::mean_se(AIC)) %>%
        dplyr::arrange(desc(y))

    mod_aic$mod_group <- forcats::fct_relevel(mod_aic$mod_group, mod_means$mod_group)
    mod_means$mod_group <- forcats::fct_relevel(mod_means$mod_group, mod_means$mod_group)

    return(list(AIC = mod_aic, variables_means = mod_means))


}


#' Shorten species label to X. yyyy
#'
#' @param species vector, character/factor
#'
#' @return factor vector, same length as species
shorten_species <- function(species){
    spec_list <- species %>%
        # droplevels() %>%
        as.character() %>%
        strsplit(" ")


    genus_short <- spec_list %>%
        purrr::map_chr(1)
    genus_short <- paste0(stringr::str_sub(genus_short, 1,1), ".")
    #

    species <- spec_list %>%
        purrr::map_chr(function(x){

            spec <- paste(x[2:length(x)], collapse = " ")
            spec <- ifelse(spec == "pseudoplatanus", "pseudo- platanus", spec)

            return(spec)
        })

    species_short <- paste(genus_short,
                           species)


    return(as.factor(species_short))
}


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


#' Return boolean index for filtering
#'
#' Top species selected based on `model_params$n_max_species`
#'
#' @param dset
#' @param model_params
#'
#' @return lgl
filter_top_species_idx <- function(dset, model_params){


    top_species <- dset %>%
        dplyr::group_by(species_corrected) %>%
        dplyr::tally(sort = TRUE) %>%
        dplyr::top_n(model_params$n_max_species) %>%
        dplyr::filter(n > model_params$n_min_abundance)

    idx <- dset$species_corrected %in% top_species$species_corrected

    return(idx)

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
    break_sequence <- seq(from = 0,
                          to = nrow(dframe),
                          by = cut_size)
    if(tail(break_sequence, 1) < nrow(dframe)){

        break_sequence <- c(break_sequence, nrow(dframe))

    }
    cuts <- base::cut(1:nrow(dframe),
                      breaks = break_sequence)
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

    # if(is.null(names(x))){
    #
    #     return(x)
    # }

    # handle sf columns
    if(rlang::inherits_any(x, "sf")){

        geometry_col <- which(names(x) == "geometry")
        names(x)[-geometry_col] <- paste0(prefix, "_", names(x)[-geometry_col])

    } else {
        x <- as.data.frame(x)
        colnames(x) <- paste0(prefix, "_", names(x))

    }



    return(x)
}



#' Make df with ancillary covariates for modelling
#'
#' @param data_list list with dfs/vectors, names must be specified as in function
#'
#' @return df with name prefixes for each data set; adjust function if additional data is used
#' @export
combine_covariates <- function(data_list){


    # expected names are:
    data_names <- c("baumsch_data",
                    "soil_type",
                    "soil_nutrients",
                    "lcz_cover_prop",
                    # "lcz_cover_prop30",
                    "lcz_cover_prop300",
                    "building_height_mean_m",
                    "building_height_mean_m_300",
                    "berlin_heat_model",
                    "berlin_urbclim_heat_model")

    if(!all(names(data_list) %in% data_names)){
        stop("expecting different data set inputs. Check or change function to accommodate")
    }


    soil_nutrient_cols <- c("nfkdur", "swert", "swertstu")
    soil_type_cols <- c("NUTZ", "NUTZ_BEZ", "BOGES_NEU5", "BTYP", "BTYP_KA4", "BTYP_KA4_LINK")


    covariate_df <- cbind(sf::st_drop_geometry(data_list$baumsch_data),
                          prefix_names(sf::st_drop_geometry(data_list$soil_type)[, soil_type_cols ], "soil_type"),
                          prefix_names(apply(sf::st_drop_geometry(data_list$soil_nutrients)[, soil_nutrient_cols], MAR = 2, as.numeric), "soil_nutrients"),
                          building_height_m = data_list$building_height_mean_m,
                          building_height_m300 = data_list$building_height_mean_m_300,
                          prefix_names(data_list$lcz_cover_prop, "lcz_prop"),
                          # prefix_names(data_list$lcz_cover_prop30, "lcz_prop30"),
                          prefix_names(data_list$lcz_cover_prop300, "lcz_prop300"),
                          prefix_names(data_list$berlin_heat_model, "mod2015"),
                          prefix_names(data_list$berlin_urbclim_heat_model, "urbclim_mod")
    )


    return(covariate_df)



}





#' Formula factory
#'
#' @param main_body named character, string that can be parsed to formula with %s wildcards,
#' name is base model name, to which placeholder var is appended
#' @param placeholders list, values = variables spliced into main body via %s
#' @param n_depth numeric, layers to search up for vars in formula
#'
#' @return
#' @export
#'
#' @examples
make_formula <- function(main_body, placeholders, n_depth){

    if(is.null(names(main_body))){
        stop("Main body must be named character vector or list")
    }


    # consider adding checks for formula syntax

    placeholder_counts_in_body <- stringr::str_count(main_body, "%s")



    unique_counts_in_placeholders <- length(unique(lengths(placeholders)))

    if(unique_counts_in_placeholders != 1){
        stop("Fix your placeholders - all items should have the same length.")
    }

    if(unique(lengths(placeholders)) != placeholder_counts_in_body){
        stop("Mismatch of wildcards and number of placeholders")
    }




    forms_out <- lapply(seq_along(placeholders),

                        function(x){

                            pl <- placeholders[[x]]




                            formula(do.call(sprintf, c(fmt = unname(main_body), as.list(pl))), n = n_depth)
                            do.call(sprintf, c(fmt = unname(main_body), as.list(pl)))
                        })



    model_name_strings <- sapply(seq_along(placeholders),
                                 function(x){

                                     # grab name
                                     n_placeholders <- length(placeholders[[x]])

                                     wildcard_string <- paste0(names(main_body), "_var-", paste0(rep("%s", n_placeholders), collapse = "-"))

                                     pl <- placeholders[[x]]
                                     model_name_string <-do.call(sprintf, c(fmt = wildcard_string, as.list(pl)))


                                     # make sprintf string with %s times length of placeholder


                                 })


    names(forms_out) <- model_name_strings

    return(forms_out)

}

#' Generate indicator for within-group variable ranges of a coarse prediction grid
#'
#' @param prediction_df data.frame, prediction grid across full variable range and groups
#' @param model_df data.frame, original model data frame for all groups and a given focal variable
#' @param group_var character, column name of the grouping variable of interest (e.g., species)
#' @param range_var character, column name of the numeric variable for which predictions are made (e.g., temperature)
#' @param qtl numeric, one-tail percentile at which within-group variables should be truncated to (e.g., 0.95 for 0.0275 and 0.975 percentiles)
#'
#' @return prediction_df with additional variable "prediction_range"
#' @export
#'
#' @examples
augment_prediction_range <- function(prediction_df, model_df, group_var, range_var, qtl = 1){



    if(any(!c(group_var, range_var) %in% colnames(prediction_df)) |
       any(!c(group_var, range_var) %in% colnames(model_df))){

        stop("Variables not in dfs")
    }


    # process original data to get ranges
    # split into groups

    split_df <- split(as.data.frame(model_df), model_df[, group_var])


    # get group variable range
    group_range <- lapply(split_df,
                          function(dset){



                              r <- range(
                                  quantile(
                                      dset[ ,range_var],
                                      probs = c(1 - qtl, qtl),
                                      na.rm = TRUE),
                                  na.rm = TRUE)


                              return(r)
                          })


    # set indicators on prediction df

    split_prediction_df <- split(prediction_df, prediction_df[, group_var])

    prediction_df_adjusted <- purrr::map2_dfr(split_prediction_df,group_range,
                                              function(x, y){

                                                  x$prediction_range <- "full"
                                                  within_idx <- dplyr::between(
                                                      x[ ,range_var],
                                                      y[1], y[2])

                                                  x$prediction_range[within_idx] <- "within"

                                                  return(x)

                                              })

    return(prediction_df_adjusted)


}


#' Summarize model predictions across age groups
#'
#' @param dset data.frame, predictions from
#' @param groupvars character, temp var and other variable(s) used in model; first needs to be temp var
#' @param age_breaks numeric, breaks for age variable
#' @param age_break_expr expression
#'
#' @return data.frame, containing mean responses and pooled standard errors for each age group
summarize_age_groups <- function(dset,model_df, groupvars, age_break_expr){

    groupvars_sym <- rlang::syms(groupvars)

    ifun <- Gamma(link = "log")$linkinv


    pred_groups <- dset %>%
        # mutate(age_group = cut(STANDALTER, age_breaks)) %>%
        mutate(age_group = eval(age_break_expr)) %>%
        # filter(STANDALTER < 100) %>%
        group_by(age_group, !!! groupvars_sym, species_corrected) %>%
        summarise(mean_dbh = mean(pred.fit, na.rm = TRUE),
                  mean_se = sqrt(sum(pred.se.fit^2)/n())) %>%
        mutate(response.fit.mean = ifun(mean_dbh),
               response.low.mean = ifun(mean_dbh - 1.96 * mean_se),
               response.high.mean = ifun(mean_dbh + 1.96 * mean_se)) %>%
        ungroup()

    pred_groups <- augment_prediction_range(prediction_df = as.data.frame(pred_groups),
                                            model_df = model_df,
                                            group_var = "species_corrected",
                                            range_var = groupvars[1],
                                            qtl = 1)

    return(pred_groups)
}




#' Calculate Moran's I for spatial subset
#'
#' @param dframe sf data frame
#' @param grid sf polygon to subset dframe
#' @param min_obs numeric, fail-safe to ensure enough data is assessed
#' @param var character, variable name in dframe to be assessed
#'
#' @return list, output from `ape::Moran.I()`
check_moran <- function(dframe, grid, min_obs = 1000, var){

    if(!{var %in% colnames(dframe)}){
        stop(sprintf("%s is not a column in dframe.", var))
    }

    subdf <- dframe[grid, ]
    subdf <- tidyr::drop_na(subdf, tidyselect::all_of(var))


    if(nrow(subdf) < min_obs){
        message("not enough observations, inrease min_obs")
        return(NULL)
    }
    if(length(unique(diff(as.numeric(subdf[ , var, drop = TRUE])))) > 1 &&
       unique(diff(as.numeric(subdf[ , var, drop = TRUE]))) == 0){
        message("all obs are equal")
        return(NULL)
    }

    # calc inverse distance
    dists <- 1/as.matrix(dist(cbind(subdf$X, subdf$Y)))
    diag(dists) <- 0
    dists[is.infinite(dists)] <- 0


    calc_weights <- ape::Moran.I(x = subdf[ , var, drop = TRUE], weight = dists, scaled = TRUE)

    return(calc_weights)
}


#' Compare Variable and residuals
#'
#' @param mod_list list, mod_group_list
#' @param bam_df data.frame, for dependency management only
#' @param var_response character, column name of response variable, e.g. dbh_cm, must be used in model from `mod_list`
#' @param var_resid character, column name of residuals, typically `.resid` from `broom::augment`
#' @param gridp sf df, 2x2 grid squares to subset data - currently split by column `grid_region`
#'
#' @return list with nesting from mod_list, with ouput from `ape::Moran.I()`
assess_morans_spatialmod <- function(mod_list,
                                     gridp,
                                     bam_df,
                                     var_response,
                                     var_resid){

    invisible(is.null(bam_df))

    # cycle through
    moran_mods <- purrr::map2(mod_list,
                              names(mod_list),
                              function(x,y){



                                  # get to individual models, carry over temp var
                                  morans <- purrr::map2(x,names(x),
                                                        function(mod, varname){

                                                            message(sprintf("current mod is: %s\n",y))
                                                            message(sprintf("current var is: %s\n",varname))
                                                            message(sprintf("current path is: %s\n",mod))

                                                            temp_mod <- readRDS(mod)
                                                            temp_mod <- broom::augment(temp_mod)
                                                            temp_mod <- sf::st_as_sf(temp_mod, coords = c("X", "Y"), crs = sf::st_crs(gridp))
                                                            temp_mod <- cbind(temp_mod, sf::st_coordinates(temp_mod))

                                                            moran_temp <- purrr::map(.x = unique(gridp$grid_region),
                                                                                     .f = function(gr){

                                                                                         if(gr %in% c("Mitte", "Sued")){
                                                                                         # if(gr %in% c("Mitte", "Nord", "Sued")){

                                                                                             message(sprintf("Skipping %s", gr))
                                                                                             return(list(non_spat = NULL,
                                                                                                         spat = NULL,
                                                                                                         var = varname,
                                                                                                         grid_area = gr))
                                                                                         } else {


                                                                                             mino <- 500
                                                                                             moran_non_spat <- check_moran(df = temp_mod,
                                                                                                                           grid = sf::st_union(gridp[gridp$grid_region==unlist(gr), ]),
                                                                                                                           min_obs = mino,
                                                                                                                           var = var_response)
                                                                                             moran_spat <- check_moran(df = temp_mod,
                                                                                                                       grid = sf::st_union(gridp[gridp$grid_region==unlist(gr), ]),
                                                                                                                       min_obs = mino,
                                                                                                                       var = var_resid)

                                                                                             return(list(non_spat = moran_non_spat,
                                                                                                         spat = moran_spat,
                                                                                                         var = varname,
                                                                                                         grid_area = gr))
                                                                                         }
                                                                                     })
                                                            rm(temp_mod)
                                                            gc()

                                                            # add grid names
                                                            names(moran_temp) <- purrr::map_chr(moran_temp, "grid_area")

                                                            return(moran_temp)
                                                        })
                                  gc()
                                  # print()
                                  # names(morans) <- purrr::map_chr(morans, "grid_area")
                                  return(morans)
                              })

    return(moran_mods)

}

#' Wrangle nested Moran's output list
#'
#' @param moran_list output from `assess_morans_spatialmod`
#'
#' @return tibble with all values from assessment
summarize_moran <- function(moran_list){

    res_list <- list()

    for(mod in names(moran_list)){


        current_mod <- moran_list[[mod]]



        for(var in names(current_mod)){
            current_var <- current_mod[[var]]


            tibble_out <- purrr::map(current_var, bind_rows) %>%
                purrr::discard(.p = ~nrow(.x) == 1) %>%
                purrr::map(~bind_cols(.x, tibble::tibble(vars = c("observed", "expected", "sd", "p.value")))) %>%
                purrr::map(~tidyr::pivot_longer(.x, cols = c("non_spat", "spat"), names_to = "spatial_model")) %>%
                purrr::map(tidyr::unnest, cols = "value") %>%
                dplyr::bind_rows()




            res_list[[mod]][[var]] <- tibble_out
        }


    }

    res <- purrr::map(res_list, dplyr::bind_rows) %>%
        dplyr::bind_rows(.id = "model")

    return(res)
}


# BIWI analyses -----------------------------------------------------------

#' Moving or sliding windows for rwl time series
#'
#' @param df data.frame, long format with "rwl_mm" and "year" columns
#' @param window_n numeric, n years for moving or sliding window
#' @param type character, either slide or moving
#'
#' @return
#' @export
#'
#' @examples
make_rwl_windows <- function(df, window_n = 5, type = "slide"){


    # checks
    if(! type %in% c("move", "slide")){
        stop("Please provide type as either 'move' or 'slide.'")
    }

    # define inner fns
    #' Calculate sliding window means for growth time series
    #'
    #' @param df data.frame in long containing colum "rwl_mm", "year"
    #' @param window_size
    slide_mean <- function(df, window_size = 5){

        rws <- nrow(df)

        window_mean_mm <- numeric(rws)
        window_nas <- numeric(rws)
        window_center <- numeric(rws)

        for(j in 3:(rws-3)){



            window <- (j-2):(j+2)



            window_mean_mm[j] <- mean(df[window, "rwl_mm", drop = TRUE], na.rm = TRUE)
            window_nas[j] <- sum(is.na(df[window, "rwl_mm", drop = TRUE]))
            window_center[j] <- df[j, "year"]


        }

        window_mean_mm[window_nas > 0 | window_mean_mm == 0] <- NA
        window_center [window_nas > 0 |  window_center == 0] <- NA

        cbind(df,data.frame(window_mean_mm,
                            window_nas,
                            window_center,
                            stringsAsFactors = FALSE))



    }


    #' Calculate moving window means for growth time series
    #'
    #' @param df data.frame in long containing colum "rwl_mm", "year"
    #' @param window_size
    move_mean <- function(df, window_size = 5){

        rws <- nrow(df)

        window_mean_mm <- numeric(rws)
        window_nas <- numeric(rws)
        window_center <- numeric(rws)

        # find first non-NA entry in rwl
        first_val <- min(which(!is.na(df[ , "rwl_mm"])))

        n_windows <- (rws - first_val + 1) %/% window_size


        for(j in seq_len(n_windows)){




            window <- seq(from = (j - 1) * window_size, length.out = window_size) + (first_val)



            window_mean_mm[window] <- mean(df[window, "rwl_mm", drop = TRUE], na.rm = TRUE)
            window_nas[window] <- sum(is.na(df[window, "rwl_mm", drop = TRUE]))
            window_center[window] <- df[median(window), "year", drop  = TRUE]


        }


        window_mean_mm[window_nas > 0 | window_mean_mm == 0] <- NA
        window_center [window_nas > 0 |  window_center == 0] <- NA

        cbind(df,data.frame(window_mean_mm,
                            window_nas,
                            window_center,
                            stringsAsFactors = FALSE))



    }




    if(type == "slide"){

        out <- slide_mean(df = df, window_size = window_n)

    } else {

        out <- move_mean(df = df, window_size = window_n)

    }

    return(out)

}


















#' Makes df of rwl time series, custom for BIWI data structure
#'
#' @param path_meta_cores  character, inventory file
#' @param path_meta_trees character, inventory file
#' @param path_meta_sites character, inventory file
#' @param path_dir_fh character, path to raw rwl folders
#'
#' @return
#' @export
#'
#' @examples
prep_rwl_data <- function(path_meta_cores,
                          path_meta_trees,
                          path_meta_sites,
                          path_dir_fh){

    # define inner funcs

    #' Load al .fh files in a directory
    #'
    #' @param path_dir_raw char, path to folder
    #' @param code_def numeric, vector of length 3, with specification of site and tree codes
    #' @param meta data.frame, contains meta info on trees
    #'
    #' @return list with all .fhs
    #'
    #' @examples
    rw_load_raw <- function(
        path_dir_raw,
        code_def = c(5,2,1),
        meta
    ){

        path_fh <- fs::path_norm(path_dir_raw)
        fhs <- list.files(
            path = path_fh,
            pattern = "*.fh",
            full.names = TRUE,
            recursive = TRUE
        )

        ## CUSTOM CLEAN
        # drop duplicated, keep "den15"
        fhs <- fhs[!grepl("den15o/", fhs, fixed = TRUE)]




        rws <- lapply(fhs,
                      function(path){

                          rw <- dplR::read.fh(fname = path)

                          # grab meta rows
                          meta <- meta[meta$Ident. %in% names(rw) & meta$G %in% c(1:4, 7,8, NA), ]


                          names_old <- names(rw)


                          # rw <- rw[ , meta$Ident.]
                          rw <- rw[ , meta$Ident., drop = FALSE]


                          cat(sprintf("Did previious and new rw series match? %i \n \n", identical(names_old, names(rw))))

                          if(is.numeric(rw)){
                              return(rw)
                          } else {


                              rw_mean <- dplR::treeMean(
                                  rwl = rw,
                                  ids = dplR::read.ids(rw, stc = code_def))

                              return(rw_mean)
                          }

                      })

        names(rws) <- fs::path_ext_remove(fs::path_file(fhs))

        return(rws)


    }



    # prep meta data ----------------------------------------------------------


    meta_cores <- readxl::read_xlsx(path_meta_cores,
                                    sheet = "Cores")
    meta_sites <- readxl::read_xlsx(path_meta_sites,
                                    sheet = "Plots")
    meta_trees <- readxl::read_xlsx(path_meta_trees,
                                    sheet = "Trees")



    meta_sites <- meta_sites %>%
        mutate(
            site_short = dplyr::case_when(
                `District                               Kreis` == "Müggelheimer Damm 141; Frau Silvia Knöfel-Mosch (Tel.:030 6540083)" ~ "natural_teufelsee",
                `District                               Kreis` == "Kolonie Heimaterde Alpenrose" ~ "urban-gardens_alpenrose",
                `District                               Kreis` == "Gutschmidtstraße" ~ "urban-park_britz-sued",
                `District                               Kreis` == "Hasenheide" ~ "urban-park_hasenheide",
                `District                               Kreis` == "Weigandufer 23" ~ "urban-smallpark_weigandufer",
                `District                               Kreis` == "Werrastraße" ~ "urban-smallpark_weigandufer",
                `District                               Kreis` == "Mecklenburgische Seenplatte" ~ "natural_mueritz-np",
                `District                               Kreis` == "Potsdam" ~ "rural_telegrafenberg",
                TRUE ~ NA_character_),
            site_type = sub("(.+(?=_))(_)(.+)",
                            "\\1",
                            x = site_short,
                            perl = TRUE),
            location_short = sub("(.+(?=_))(_)(.+)",
                                 "\\3",
                                 x = site_short,
                                 perl = TRUE)
        )


    # join with inventory meta
    meta_trees$tree_id <- substr(meta_trees$Ident., 1, 7)
    meta_trees$plot_id <- substr(meta_trees$Ident., 1,5)

    meta_full <- dplyr::left_join(meta_trees, meta_sites, by = c("plot_id" = "Key-Code"))

    # check that years in meta match series
    # series_lengths <- purrr::map_dfr(all_series, function(x){as.data.frame(nrow(x))}, .id = "tree_id")

    # series_checks <- dplyr::left_join(series_lengths,
    # meta_full[ , c("tree_id", "Anzahl JR")], by = "tree_id")

    # series_checks[!series_checks$`nrow(x)` == series_checks$`Anzahl JR`, ]
    # ddp0212 has bad meta val
    # all others are considered okay


    # adjust for missing rings (pith and bark)
    # calculate average ring width for first/last 15 years of series, to estimate radius offsets)

    # meta_full$tree_id[ meta_full$`mR to pith` > 0 & !is.na(meta_full$`mR to pith`)]
    # meta_full$tree_id[meta_full$`mR to bark` > 0  & !is.na(meta_full$`mR to bark`)]



    # read in data ------------------------------------------------------------

    all_series <- rw_load_raw(path_dir_raw = path_dir_fh,
                              meta = meta_cores)


    all_series <- purrr::discard(all_series, ~length(.x) == 0)


    all_series <- purrr::modify2(all_series, names(all_series),
                                 function(x,y){
                                     colnames(x) <- y
                                     return(x)
                                 })

    all_series_rwl <- dplR::combine.rwl(all_series)

    all_series_rwl$year <- as.numeric(row.names(all_series_rwl))


    # make long and add meta columns
    all_series_rwl_long <- tidyr::pivot_longer(data = all_series_rwl,
                                               cols = -year,
                                               names_to = "tree_id",
                                               values_to = "rwl_mm")

    all_series_rwl_long <- dplyr::left_join(all_series_rwl_long,
                                            dplyr::select(
                                                meta_full,
                                                height = `Höhe [m]`,
                                                circ_cm = `BH-Umfang`,
                                                tree_id,
                                                plot_id,
                                                species = `Species Art`,
                                                sample_location = `Location                                   Standortname`,
                                                sample_location_desc = `District                               Kreis`,
                                                quality = `G`,
                                                mr_pith = `mR to pith`,
                                                mr_bark = `mR to bark`,
                                                age = Alter,
                                                pith = Mark,
                                                first_ring = `erster JR`,
                                                date_sample = `Datum`,
                                                site_short,
                                                site_type,
                                                location_short),
                                            by = "tree_id"
    )

    # Tue Jun 29 11:26:44 2021 ------------------------------
    all_series_rwl_long$age <- all_series_rwl_long$age + 3 # data sampled in 2018;

    # adjust factors
    all_series_rwl_long$species <- as.factor(all_series_rwl_long$species)
    all_series_rwl_long$plot_id <- as.factor(all_series_rwl_long$plot_id)
    all_series_rwl_long$tree_id <- as.factor(all_series_rwl_long$tree_id)
    all_series_rwl_long$year_sampled <- as.numeric(format(all_series_rwl_long$date_sample, "%Y"))

    # drop empyt rows
    drop_rows <- which.max(apply(all_series_rwl[ ,-grep("year", colnames(all_series_rwl))], MARGIN = 1, function(x){any(!is.na(x))})) - 1
    # all_series_rwl_long <- all_series_rwl_long
    all_series_rwl_long <- all_series_rwl_long[-drop_rows, ] %>%
        mutate(cambial_age = case_when(
            !is.na(pith) ~ year - pith,
            !is.na(pith) & !is.finite(mr_pith) ~ year - pith,
            !is.na(pith) & is.finite(mr_pith) ~ year - pith,


            is.na(pith) & is.finite(mr_pith)  & is.finite(first_ring) ~ year - first_ring - mr_pith,
            is.na(age) & !is.finite(mr_pith) ~ year - first_ring,
            TRUE ~ NA_real_
        ))


    return(all_series_rwl_long)


}



#' Estimates time and age trend in BIWI data
#'
#' @param df data.frame, either all series raw or moving data
#' @return mgcv::gamm output
apply_gam_biwi <- function(df){

    # define inner fncs
    make_urban_df <- function(df, year_min, year_max,year_break, cambial_age_max){
        urban_rwl <- df %>%
            mutate(age_group = cut(cambial_age, seq(from = 0, to = 120, by = 5)),
                   site_global = case_when(
                       site_type %in% c("natural", "rural") ~ "forest",
                       TRUE ~ "urban"),
                   year_break = as.factor(
                       ifelse(
                           year <= year_break,
                           sprintf("<=%s", year_break),
                           sprintf(">%s", year_break))),
                   location_short = as.factor(location_short)) %>%
            filter(site_global == "urban",
                   year >= year_min,
                   year <= year_max,
                   cambial_age <= cambial_age_max,
                   cambial_age >= 0,
                   !is.na(rwl_mm)) %>%
            arrange(tree_id, year)

        return(urban_rwl)
    }

    urban_rwl <- make_urban_df(df,
                               year_min = 1920,
                               year_max = 2001,
                               year_break = 1960,
                               cambial_age_max = 100)


    # library(mgcv)
    ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

    # mod <- gamm(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 6, by = year_break) + year_break,
    #
    #                     data = urban_rwl,
    #                     family = Gamma(link = "log"),
    #                     correlation = corARMA(form = ~ year | tree_id, p = 3),
    #                     random = list(tree_id = ~1,
    #                                   species = ~ 1),
    # niterPQL = 50)


    mod <- mgcv::gamm(rwl_mm ~ s(year, k = 15) + s(cambial_age, k = 6, by = year_break) + year_break + s(species, bs = "re"),
               data = urban_rwl,
               family = Gamma(link ="log"),
               correlation = nlme::corARMA(form = ~ year | tree_id, p = 3),
               # correlation = corAR1(form = ~ year | tree_id),
               # random = list(tree_id = ~1,
               # species = ~ 1),
               niterPQL = 50)
    # mod <- gam(list(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 6, by = year_break) + year_break,
    #                 ~ s(cambial_age, k = 6, by = year_break) + year_break),
    #            data = urban_rwl,
    #            family = mgcv::gammals(link = list("identity", "log")),
    #            correlation = corARMA(form = ~ year | tree_id, p = 15, q = 3),
    #            # correlation = corAR1(form = ~ year | tree_id),
    #            # random = list(tree_id = ~1,
    #            # species = ~ 1),
    #            niterPQL = 50)



    mod$df <- urban_rwl


    return(mod)


}


#' Estimate model and generate predictions for BIWI mod
#'
#' @param dframe data.frame from `prep_rwl_data`
#'
#' @return
biwi_mod_predict <- function(dframe){

    biwimod <- apply_gam_biwi(df = dframe)



    new_dat <- expand.grid(cambial_age = c(0:80),
                           year = c(1920:2001),
                           species = unique(biwimod$df$species)) %>%
        mutate(year_break = as.factor(ifelse(year <= 1960, "<=1960", ">1960")))
    preds <- predict(biwimod$gam, newdata = new_dat, type = "link", se.fit = TRUE, exclude = "s(species)")


    new_dat <- cbind(new_dat, preds) %>%
        mutate(year_group = as.factor(year <= 1950)) %>%
        mutate(year_break = as.factor(ifelse(year <= 1960, "1920 - 1960", "1961 - 2001")))
    return(new_dat)

}

