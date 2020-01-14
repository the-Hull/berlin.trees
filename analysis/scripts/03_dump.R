
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
    dplyr::filter(provenance == "s_wfs_baumbestand") %>%
    dplyr::mutate(STANDALTER = as.numeric(STANDALTER),
        age_group = cut(STANDALTER, breaks = seq(20, 340, 20))) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(dbh_cm), fill = age_group)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~gattung_short)


extract_values[["Summertime_gridded_UHI_data"]][["day"]] %>%
    dplyr::filter(provenance == "s_wfs_baumbestand") %>%
    dplyr::mutate(STANDALTER = as.numeric(STANDALTER),
                  age_group = cut(STANDALTER, breaks = seq(20, 340, 20))) %>%
    dplyr::pull(dbh_cm) %>%
    plot()



extract_values[["Summertime_gridded_UHI_data"]][["day"]] %>%
    dplyr::filter(provenance == "s_wfs_baumbestand",
                  dbh_cm < 300) %>%
    dplyr::mutate(STANDALTER = as.numeric(STANDALTER),
                  age_group = cut(STANDALTER, breaks = seq(0, 320, 40))) %>%
    ggplot2::ggplot(ggplot2::aes(x = day_2013,
                                 y = as.numeric(dbh_cm), fill = age_group, col = age_group)) +
    # ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "gam") +
    ggplot2::facet_wrap(~gattung_short) +
    scale_color_viridis_d() +
    scale_fill_viridis_d()


annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data)
{
    layer(data = data, stat = StatIdentity, position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob,
                                            xmin = xmin, xmax = xmax,
                                            ymin = ymin, ymax = ymax))
}

dens_plot <- extract_values$Summertime_gridded_UHI_data$day %>%
    # dplyr::filter(!is.na(GATTUNG)) %>%
    # dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
    #               # GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),
    #
    #
    #               gattung_short = forcats::fct_lump(as.factor(GATTUNG),11, other_level = "Other") %>%
    #                   as.character(),
    #               gattung_short =   ifelse(GATTUNG == "Pinus", "Pinus", gattung_short),
    #               gattung_short =   ifelse(GATTUNG == "Fagus", "Fagus", gattung_short),
    #               gattung_short = forcats::fct_infreq(as.factor(gattung_short)),
    #               gattung_short = forcats::fct_relevel(gattung_short, "Other", after = Inf),
    #
    #
    #               bezirk_num = as.numeric(as.factor(BEZIRK))) %>%
    ggplot() +
    # geom_density(aes(x = day_2003, col = "2003", fill = "2003"), alpha =  0.5) +
    # geom_density(aes(x = day_2007, col = "2007", fill = "2007"), alpha =  0.5) +

    geom_density(aes(x = day_2007, fill = provenance),
                 alpha =  0.2,
                 col = "transparent",
                 size = 1) +
    stat_density(geom = "line", aes(x = day_2007, col = provenance, fill = provenance),
                 alpha =  0.8, size = 1,
                 position = "dodge") +
    # geom_density(aes(x = day_2007, col = provenance, fill = provenance), alpha =  0.5) +
    # geom_density(aes(x = day_2017, col = "2017", fill = "2017"), alpha =  0.5) +
    # geom_freqpoly(aes(x = day_2013)) +
    # geom_freqpoly(aes(x = day_2003), col = "red") +
    facet_wrap(~gattung_short) +
    geom_vline(xintercept = 0, linetype = 2) +
    # scale_color_manual(values = c(RColorBrewer::brewer.pal(3, "Set2"))) +
    # scale_fill_manual(values = c(RColorBrewer::brewer.pal(3, "Set2"))) +
    guides(color = FALSE) +
    theme_bw(base_size = 16) +
    xlim(c(-5, 6.5)) +
    theme(legend.position = c(0.6, 0.1),
          legend.direction = "horizontal")





bar_plots <- purrr::map(levels(extract_values$Summertime_gridded_UHI_data$day$gattung_short),

                        function(gattung){


                            annotation_custom2(
                                grob = ggplotGrob(extract_values$Summertime_gridded_UHI_data$day %>%
                                                      # dplyr::mutate(STAMMUMFG = as.numeric(STAMMUMFG),
                                                      #        GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),
                                                      #        gattung_short = forcats::fct_lump(GATTUNG,11),
                                                      #        bezirk_num = as.numeric(as.factor(BEZIRK))) %>%


                                                      ggplot() +

                                                      coord_flip() +

                                                      geom_bar(aes(x = gattung_short,
                                                                   fill = {gattung_short == gattung}),
                                                               show.legend = FALSE,
                                                               color = "gray10") +


                                                      labs(
                                                          # subtitle =  paste0("total records: ",
                                                          #                     nrow(sf_data),
                                                          #                     "\n",
                                                          #                     "total genera: ",
                                                          #                     sf_data$GATTUNG %>%
                                                          #                         unique() %>% length()),
                                                          #
                                                          #  caption = paste0("Data source: daten.berlin.de; WFS Service, accessed: ",
                                                          #                   Sys.Date()),
                                                          y = "Count",
                                                          x = "Genus",
                                                          fill = NULL) +


                                                      # theme_minimal(base_size = 18) +
                                                      scale_y_continuous(breaks = c(0, 150000),
                                                                         labels = c("0", "150k")) +
                                                      theme_minimal(base_size = 9) +
                                                      # theme(legend.position = c(0.8,0.5),
                                                      #       axis.text.y = element_text(face = "italic")) +
                                                      #

                                                      # scale_fill_brewer(type = "qual", palette = "Set2") +
                                                      scale_fill_manual(values = c(`FALSE` = "gray90",
                                                                                   `TRUE` = "gray10")) +

                                                      theme(axis.text.y = element_blank(),
                                                            axis.title = element_blank(),
                                                            plot.margin = margin(),
                                                            panel.grid = element_blank(),
                                                            axis.ticks.length = unit(2, "mm"),
                                                            plot.background = element_rect(color = "transparent"))),
                                data = data.frame(gattung_short=gattung),
                                ymin = 0.4, ymax=1.4, xmin=-5, xmax=-0.5)






                        })





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
