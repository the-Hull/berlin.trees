
loadd(full_data_set_clean)



uhi_list <- berlin.trees::get_uhi_rasters(file.path(here::here(), "analysis", "data", "raw_data", "UHI_explorer"))
raster_stats <- berlin.trees::calc_uhi_stats(uhi_list)








extract_values <- berlin.trees::add_uhi_hist_data(uhi_stack_list = uhi_list,
                                    sf_data = full_data_set_clean[, ])




# plot_uhi_dist <- function(uhi_stack_list, uhi_stats){
#
#
# }


extract_values[["Summertime_gridded_UHI_data"]][["day"]] %>%
    filter(provenance == "s_wfs_baumbestand") %>%
    mutate(STANDALTER = as.numeric(STANDALTER),
        age_group = cut((STANDALTER), breaks = seq(20, 340, 20))) %>%
    ggplot(aes(x = as.numeric(STAMMUMFG), group = age_group)) +
    geom_histogram() +
    facet_wrap(~gattung_short)




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


#
#
#
# stat_funs <- c("median", "mean", "sd")
#
# ### cycle into list containing the 4 (total) stacks (2x summer, 2x winter)
# raster_stats <- lapply(stat_funs,
#                        function(stats) {
#
#                            # into list (summer vs. winter)
#                            lapply(test,
#                                   function(stacks) {
#                                       # into stacks (day vs. night)
#                                       lapply(stacks,
#                                              raster::cellStats, stats)
#                                   })
#                        }) %>%
#     setNames(stat_funs)
#
#
