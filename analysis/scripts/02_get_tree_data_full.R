


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
    xml_capabilities <- content(capabilities, type = "text/xml")

    feature_name <- xml_contents(
        xml_child(
            xml_child(
                xml_capabilities, 4)
            , 1))[1] %>%
        xml_text() %>% stringr::str_remove("fis:")

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
          httr::GET(u, query = query, httr::write_disk(jp, overwrite = TRUE))
      })







