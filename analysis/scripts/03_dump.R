get_uhi_hist_data <- function(uhi_stack_list, sf_data){


    # brief checks
    if (!is(sf_data, "sf")) {
        stop("input data is not in sf")
    }
    # if (!is(uhi_stack_list, "RasterLayer")) {
    #     stop("input raster is not a RasterLayer")
    # }


    # LOGIC



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
                                                 raster::cellStats, stats)
                                      })
                           }) %>%
        setNames(stat_funs)


    ## tree uhi exposure

    ### prepare tree data

    sf_data <- sf_data %>%
        dplyr::filter(!is.na(GATTUNG)) %>%
        dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
                      # GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),


                      gattung_short = forcats::fct_lump(as.factor(GATTUNG),11, other_level = "Other") %>%
                          as.character(),
                      gattung_short =   ifelse(GATTUNG == "Pinus", "Pinus", gattung_short),
                      gattung_short = forcats::fct_infreq(as.factor(gattung_short)),
                      gattung_short = forcats::fct_relevel(gattung_short, "Other", after = Inf),


                      bezirk_num = as.numeric(as.factor(BEZIRK)))



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
                                      sf::as_Spatial(sf_data),
                                      # df = TRUE,
                                      sp = TRUE)
                           }) %>%
        # transform back to sf
        lapply(.,
               function(stacks){
                   lapply(stacks,
                          sf::st_as_sf)
               })







}


loadd(full_data_set_clean)



test <- berlin.trees::get_uhi_rasters(file.path(here::here(), "analysis", "data", "raw_data", "UHI_explorer"))





plot(test$Summertime_gridded_UHI_data$day$day_2013)




extract_values <- get_uhi_hist_data(uhi_stack_list = test,
                                    sf_data = full_data_set_clean[, ])




extract_values$Summertime_gridded_UHI_data$day %>%
    dplyr::filter(!is.na(GATTUNG)) %>%
    dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
                  # GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),


                  gattung_short = forcats::fct_lump(as.factor(GATTUNG),11, other_level = "Other") %>%
                      as.character(),
                  gattung_short =   ifelse(GATTUNG == "Pinus", "Pinus", gattung_short),
                  gattung_short =   ifelse(GATTUNG == "Fagus", "Fagus", gattung_short),
                  gattung_short = forcats::fct_infreq(as.factor(gattung_short)),
                  gattung_short = forcats::fct_relevel(gattung_short, "Other", after = Inf),


                  bezirk_num = as.numeric(as.factor(BEZIRK))) %>%
    ggplot(aes(group = gattung_short)) +
    geom_density(aes(x = day_2003, col = "2003", fill = "2003"), alpha =  0.5) +
    geom_density(aes(x = day_2007, col = "2007", fill = "2007"), alpha =  0.5) +
    geom_density(aes(x = day_2017, col = "2017", fill = "2017"), alpha =  0.5) +
    # geom_freqpoly(aes(x = day_2013)) +
    # geom_freqpoly(aes(x = day_2003), col = "red") +
    facet_wrap(~gattung_short) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_color_manual(values = c(RColorBrewer::brewer.pal(3, "Set2"))) +
    scale_fill_manual(values = c(RColorBrewer::brewer.pal(3, "Set2"))) +
    guides(color = FALSE) +
    theme_bw() +
    xlim(c(-5, 6.5))





stat_funs <- c("median", "mean", "sd")

### cycle into list containing the 4 (total) stacks (2x summer, 2x winter)
raster_stats <- lapply(stat_funs,
                       function(stats) {

                           # into list (summer vs. winter)
                           lapply(test,
                                  function(stacks) {
                                      # into stacks (day vs. night)
                                      lapply(stacks,
                                             raster::cellStats, stats)
                                  })
                       }) %>%
    setNames(stat_funs)



plot(x = 2003:2017, raster_stats$mean$Summertime_gridded_UHI_data$day, type = "b")
