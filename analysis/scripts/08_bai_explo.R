# process BIWI dendro data
# data from: Prof Christoph Schneider; Sebastian Schneider (MSc)

# libraries ---------------------------------------------------------------

library(dplR)
library(dplyr)



# functions ---------------------------------------------------------------



predict_rw_to_bark <- function(meta, series_names, rwl){

    `%nin%` <- Negate(`%in%`)

    rwl$year <- as.numeric(row.names(rwl))

    out_list <- list()

    for(sn in series_names){
        # grab plot id and subset data
        current_plot <- substr(sn, 1, 5)



        model_df <- rwl[ ,grepl(pattern = sprintf("(%s)|(%s)", "year", current_plot), x = colnames(rwl))]
        # remove year and response for averaging
        year_col <- which(colnames(model_df) == "year")
        drop_columns <- c(year_col,
                          grep(pattern = sn, x = colnames(model_df)))

        print(drop_columns)

        model_df <- data.frame(year = as.numeric(model_df$year),
                               mean_rwl = rowMeans(
                                   model_df[ , -drop_columns],
                                   na.rm = TRUE),
                               response = model_df[ , sn],
                               # define weights based on non-NA entries per plot
                               weights = 1 + ncol( model_df[ , -drop_columns]) - (apply(model_df[ , -drop_columns],
                                                                                        MARGIN = 1,
                                                                                        function(x){sum(is.na(x))})
                               ))

        print(colnames(model_df))
        print(model_df$weights)


        # form <- paste(sprintf("%s ~ year + ", sn), paste(colnames(model_df)[colnames(model_df) %nin% c(sn, "year") ], collapse = " + "))
        # form <- sprintf("%s ~ year + mean_rwl", sn)
        form <- as.formula("response ~ year + mean_rwl")
        # print(form)

        mod <- glm(formula = form,
                   data = model_df,
                   na.action = "na.omit",
                   family = gaussian(link = "log"),
                   weights = as.numeric(model_df$weights)
        )

        # Get the coefficient matrix
        # coefs <- summary(mod)$coefficients
        # sig variables
        # vars <- rownames(coefs)[which(coefs[, 4] < 0.05)]


        # estimate
        max_year <- max(rwl[!is.na(rwl[ ,sn]) ,c("year", sn)]$year, na.rm = TRUE)
        print(max_year)

        year_col_inner <- which(colnames(model_df) == "year")

        # any data available beyond the max year of target series IN THE OTHER series?
        checks <- any(!is.na(model_df[model_df$year >= max_year, -year_col_inner]))
        print(checks)
        # make_grid <- expand.grid()

        # sn_col <- grep(sn, colnames(model_df))
        sn_col <- grep("response", colnames(model_df))
        new_dat <- model_df[model_df$year > max_year, - c(sn_col)]

        print(new_dat)

        model_df <- broom::augment(mod, type.predict = c( "response"))


        # clean_df <- tidyr::drop_na(model_df, tidyselect::any_of(sn))
        clean_df <- tidyr::drop_na(model_df, tidyselect::any_of("response"))
        # plot(clean_df[ , c("year", sn)])
        plot(clean_df[ , c("year", "response")], xlim = c(min(model_df$year), 2020))

        # plot(clean_df$year, clean_df[, sn])
        lines(model_df$year, model_df$.fitted, col = "red")



        final_year_estimates <- broom::augment(mod, newdata = new_dat, type.predict = "response")
        points(final_year_estimates$year, final_year_estimates$mean_rwl, pch = 16, col = "darkorange")
        lines(final_year_estimates$year, final_year_estimates$.fitted, col = "dodgerblue1")


        out_list[[sn]] <- list(mod = mod,
                               est = final_year_estimates,
                               est_rad_missing_mm = round(sum(final_year_estimates$mean_rwl, na.rm = TRUE),3),
                               year_start = min(final_year_estimates$year),
                               year_end = max(final_year_estimates$year[is.finite(final_year_estimates$mean_rwl)]))
    }

    return(out_list)


}


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


# read data ---------------------------------------------------------------


# path_fh <- "C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/SMC/"
path_fh <- "C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/RAW/"
# fhs <- list.files(path = path_fh, pattern = "*.fh", full.names = TRUE, recursive = TRUE)

# drop duplicated fhs
# fhs <- fhs[!grepl("den15/", fhs, fixed = TRUE)]

meta_cores <- readxl::read_xlsx("C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/BIWi_INV_20190123.xlsx",
                          sheet = "Cores")
meta_sites <- readxl::read_xlsx("C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/BIWi_INV_20190123.xlsx",
                                sheet = "Plots")
meta_trees <- readxl::read_xlsx("C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/BIWi_INV_20190123.xlsx",
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

# dplyr::distinct(meta_sites[ ,c("Province                        Region",
#          "Location                                   Standortname",
#          "District                               Kreis",
#          "Laenge",
#          "Breite",
#          "site_short",
#          "site_type")])
#

# rws <- dplR::read.fh(fname = fhs[1])
# rws <- dplR::read.fh(fname = fhs[524])
# mshort <- meta[meta$Ident. %in% names(rws) & meta$G %in% c(1:4, 7,8, NA), ]
# mshort$Ident.
# rws[ , mshort$Ident.]



all_series <- rw_load_raw(path_dir_raw = path_fh,
                    meta = meta_cores)


all_series <- purrr::discard(all_series, ~length(.x) == 0)


all_series <- purrr::modify2(all_series, names(all_series),
              function(x,y){
                  colnames(x) <- y
                  return(x)

              })



# Meta Adjustments ---------------------------------------------------

# join with inventory meta
meta_trees$tree_id <- substr(meta_trees$Ident., 1, 7)
meta_trees$plot_id <- substr(meta_trees$Ident., 1,5)

meta_full <- dplyr::left_join(meta_trees, meta_sites, by = c("plot_id" = "Key-Code"))

# check that years in meta match series
series_lengths <- purrr::map_dfr(all_series, function(x){as.data.frame(nrow(x))}, .id = "tree_id")

series_checks <- dplyr::left_join(series_lengths,
                 meta_full[ , c("tree_id", "Anzahl JR")], by = "tree_id")

series_checks[!series_checks$`nrow(x)` == series_checks$`Anzahl JR`, ]
# ddp0212 has bad meta val
# all others are considered okay


# adjust for missing rings (pith and bark)
# calculate average ring width for first/last 15 years of series, to estimate radius offsets)

meta_full$tree_id[ meta_full$`mR to pith` > 0 & !is.na(meta_full$`mR to pith`)]
meta_full$tree_id[meta_full$`mR to bark` > 0  & !is.na(meta_full$`mR to bark`)]






# dplR::read.rwl(fhs[grepl("den1003", fhs)])
all_series_rwl <- dplR::combine.rwl(all_series)

dplR::spag.plot(all_series_rwl,zfac = 3, useRaster = FALSE)

meta_order <- match(colnames(all_series_rwl), meta_full$tree_id, )

# radii <- meta_full[meta_order, c("tree_id", "BH-Umfang", "Bark [mm]")]
# radii$radius_mm <- radii$`BH-Umfang`*10/ (pi) - radii$`Bark [mm]`
# radii$diam_mm <- radii$radius_mm * 2
#
# radii <- radii[!is.na(radii$radius_mm), ]
#
#
# all_series_bai <- dplR::bai.out(
#     all_series_rwl[, colnames(all_series_rwl) %in% radii$tree_id],
#     as.data.frame(radii[ , c("tree_id", "diam_mm")]))
#
#
# dplR::spag.plot(all_series_bai[ ,])


# dplR::plot.rwl(all_series_bai[ ,1:10], plot.type = "spag")



# check if can impute missing values
# baddies <- meta_full$tree_id[meta_full$`mR to bark` > 0  & !is.na(meta_full$`mR to bark`)]


# out <- predict_rw_to_bark(series_names = "dek0215", rwl = all_series_rwl)
# out <- predict_rw_to_bark(series_names = baddies, rwl = all_series_rwl)
# summary(out$mod)






# Chrono EDA -------------------------------------------------------

# all_series_bai$year <- as.numeric(row.names(all_series_bai))
#
# all_series_bai_long <- tidyr::pivot_longer(data = all_series_bai,
#                     cols = -year,
#                     names_to = "tree_id",
#                     values_to = "bai_mm2")
#
# all_series_bai_long <- dplyr::left_join(all_series_bai_long,
#                                         dplyr::select(
#                                             meta_full,
#                                             `Höhe [m]`,
#                                             `BH-Umfang`,
#                                             tree_id,
#                                             plot_id,
#                                             `Species Art`,
#                                             `Location                                   Standortname`,
#                                             `G`)
#                                         )
# all_series_bai_long$cambial_age <- 2018 - as.numeric(all_series_bai_long$year)
# all_series_bai_long$species <- as.factor(all_series_bai_long$`Species Art`)
# all_series_bai_long$plot_id <- as.factor(all_series_bai_long$plot_id)
# all_series_bai_long$tree_id <- as.factor(all_series_bai_long$tree_id)
#
# library(mgcv)
#
#
# all_series_bai_long <- all_series_bai_long[!is.na(all_series_bai_long$bai_mm2), ]
# tree_ids <- sample(unique(all_series_bai_long$tree_id), 25, replace = FALSE)
#
#
# all_series_bai_test <- all_series_bai_long[all_series_bai_long$tree_id %in% tree_ids, ]
#
#
# # lapply(split(all_series_bai_test, all_series_bai_long$tree_id),
# #        function(srs){
# #            bai_detrend <- mgcv::bam(bai_mm2 ~  s(year, bs = "tp", k = 24),
# #                                     discrete = FALSE,
# #                                     data = srs)
# #            #
# #        })
#
# bai_detrend <- mgcv::bam(bai_mm2 ~ tree_id + s(year, by = tree_id, bs = "tp", k = 24),
#                          discrete = FALSE,
#                          data = all_series_bai_test)
# #
# # bai_detrend <- mgcv::bam(bai_mm2 ~ species + s(year, by = species, bs = "tp", k = 24) + s(cambial_age, by = species, bs = "tp", k = 24),
# #                          discrete = TRUE,
# #                          data = all_series_bai_long)
# mgcv::gam.check(bai_detrend)
#
# mgcv::concurvity(bai_detrend)
#
#
# mgcv::plot.gam(bai_detrend, pages = 1, scheme = 2, seWithMean = TRUE, rug = FALSE)
#
# library(ggplot2)
#
# ggplot(data = all_series_bai_test) +
#     geom_line(aes(x = year, y = bai_mm2, color = `BH-Umfang`)) +
#     facet_wrap(~tree_id)
#
#
# (300^2 * pi) - 298^2 * pi
#



# cambial expo ------------------------------------------------------------

all_series_rwl$year <- as.numeric(row.names(all_series_rwl))

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
all_series_rwl_long$age <- all_series_rwl_long$age + 2
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

library(ggplot2)

all_series_rwl_long %>%
    ggplot(aes(group = tree_id)) +
    geom_line(aes(x = cambial_age,
                  y = rwl_mm,
                  color = species)) +
    # facet_wrap(~sample_location_desc)
    facet_wrap(~sample_location)








library(furrr)
future::plan(future::multisession(workers = future::availableCores()-1))

rwl_windows <- furrr::future_map_dfr(
    split(all_series_rwl_long,
          all_series_rwl_long$tree_id),
    function(x){
        slide_mean(x)
    })



rwl_move <- furrr::future_map_dfr(
    split(all_series_rwl_long,
          all_series_rwl_long$tree_id),
    function(x){
        move_mean(x)
    })


future::plan(sequential)



# rwl_windows %>%
#     filter(between(cambial_age, 50, 60)) %>%
#     ggplot(aes(group = tree_id,
#                x = cambial_age,
#                y = window_mean_mm,
#                color = window_center)) +
#     geom_line() +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~sample_location) +
#     geom_smooth(method = "lm", aes(group = 1),
#                 color = "darkorange",
#                 size = 1.5) +
#     scale_color_viridis_c()
#
#
#
# rwl_move %>%
#     filter(between(cambial_age, 20, 40)) %>%
#     ggplot(aes(group = tree_id,
#                x = cambial_age,
#                y = window_mean_mm,
#                color = window_center)) +
#     geom_line() +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~sample_location) +
#     geom_smooth(method = "lm", aes(group = 1),
#                 color = "darkorange",
#                 size = 1.5) +
#     scale_color_viridis_c()
#
#
#
#
# rwl_windows %>%
#     filter(between(cambial_age, 30, 35)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                color = species,
#                group = species)) +
#     geom_point() +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~sample_location, scales = "free_x") +
#     geom_smooth(method = "lm")
#     # lims(xlim = c(1940, 2020))
#
#
# rwl_windows %>%
#     filter(between(cambial_age, 30, 40)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                color = species,
#                group = species)) +
#     geom_point() +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~sample_location, scales = "free_x") +
#     geom_smooth(method = "lm")
# # lims(xlim = c(1940, 2020))
#
#
#
#
# rwl_windows %>%
#     filter(between(cambial_age, 60, 70)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                color = species,
#                group = species)) +
#     geom_point() +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~sample_location, scales = "free_x") +
#     geom_smooth(method = "lm")
# lims(xlim = c(1940, 2020))





# library(furrr)
# future::plan(future::multisession(workers = future::availableCores()-1))
#
# rwl_move <- furrr::future_map_dfr(
#     split(all_series_rwl_long,
#           all_series_rwl_long$tree_id),
#     function(x){
#         move_mean(x)
#     })
#
#
# future::plan(sequential)
#


rwl_move %>%
    mutate(age_group = cut(cambial_age, seq(from = 0, to = 120, by = 5)),
           site_global = case_when(
               site_type %in% c("natural", "rural") ~ "forest",
               TRUE ~ "urban")) %>%
    # mutate(age_group_young = ifelse(between(age_group, 10, 25), "young", NA),
           # age_group_old = ifelse(between(age_group, 55, 70), "old", "NA")) %>%
    filter(between(cambial_age, 21, 25) |
               between(cambial_age, 51, 55)) %>%
    ggplot(aes(x = window_center,
               y = window_mean_mm,
               linetype = age_group,
               color = site_global,
               group = site_global,
               shape = location_short)) +
    geom_point() +
    # facet_wrap(~sample_location_desc)
    facet_grid(age_group~species, scales = "free_x") +
    geom_smooth(method = "lm")



# rwl_move %>%
#     filter(between(cambial_age, 25, 25)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                color = site_type,
#                group = site_type,
#                shape = location_short)) +
#     geom_point() +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~species, scales = "free_x") +
#     geom_smooth(method = "lm")
#
#
# rwl_move %>%
#     filter(between(cambial_age, 50, 55)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                color = site_type,
#                group = site_type,
#                shape = location_short)) +
#     geom_point() +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~species, scales = "free_x") +
#     geom_smooth(method = "lm")
# # lims(xlim = c(1940, 2020))
# rwl_move %>%
#     filter(between(cambial_age, 10, 25)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                color = sample_location)) +
#     geom_point(alpha = 0.3) +
#     # facet_wrap(~sample_location_desc)
#     facet_wrap(~species) +
#     geom_smooth(method = "lm") +
#     guides(color = FALSE)
# # lims(xlim = c(1940, 2020))
#
#
#
#
#
# rwl_move %>%
#     filter(sample_location == "Teufelssee-Kie",
#            between(cambial_age, 60, 70)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                color = species,
#                group = species)) +
#     geom_point() +
#     # facet_wrap(~sample_location_desc)
#     # facet_wrap(~sample_location, scales = "free_x") +
#     geom_smooth(method = "lm")
# # lims(xlim = c(1940, 2020))
#
#
#
# rwl_move %>%
#     filter(sample_location == "Teufelssee-Kie", between(cambial_age, 60, 70)) %>%
#     ggplot(aes(x = pith,
#                y = tree_id,
#                color = species,
#                group = tree_id)) +
#     geom_point()
#     # facet_wrap(~sample_location_desc)
#     # geom_smooth(method = "lm")
# # lims(xlim = c(1940, 2020))
#
#
#
# rwl_move %>%
#     mutate(age_group = cut(cambial_age, seq(from = 0, to = 120, by = 5)),
#            site_global = case_when(
#                site_type %in% c("natural", "rural") ~ "forest",
#                TRUE ~ "urban")) %>%
#     filter(between(cambial_age, 25, 25) |
#                between(cambial_age, 55, 55)) %>%
#     ggplot(aes(x = window_center,
#                y = window_mean_mm,
#                linetype = age_group,
#                color = site_global,
#                group = location_short,
#                shape = location_short)) +
#     geom_point() +
#     # facet_wrap(~sample_location_desc)
#     facet_grid(age_group~species, scales = "free_x") +
#     geom_smooth(method = "lm")




rwl_move %>%
    mutate(age_group = cut(cambial_age, seq(from = 0, to = 120, by = 5)),
           site_global = case_when(
               site_type %in% c("natural", "rural") ~ "forest",
               TRUE ~ "urban")) %>%
    filter(cambial_age >= 0, cambial_age < 150) %>%
    # filter(between(cambial_age, 31, 35) |
               # between(cambial_age, 61, 65)) %>%
    ggplot(aes(x = cambial_age,
               y = rwl_mm,
               # color = species,
               group = species)) +
    geom_point(alpha = 0.3) +
    # facet_wrap(~sample_location_desc)
    facet_grid(species~site_global, scales = "free_x") +
    # geom_smooth(method = "lm") +
    geom_smooth(aes(group = 1), color = "purple") +
    geom_smooth(aes(group = year >= 1960, color  = year >= 1960))




# rwl_move %>%
#     mutate(age_group = cut(cambial_age, seq(from = 0, to = 120, by = 5)),
#            site_global = case_when(
#                site_type %in% c("natural", "rural") ~ "forest",
#                TRUE ~ "urban")) %>%
#     filter(!is.na(rwl_mm)) %>%
#     # filter(between(cambial_age, 31, 35) |
#     # between(cambial_age, 61, 65)) %>%
#     ggplot(aes(x = year)) +
#     geom_histogram(alpha = 0.3) +
#     # facet_wrap(~sample_location_desc)
#     facet_grid(species~site_global, scales = "free_x")
#     # geom_smooth(method = "lm") +
#
#

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
               cambial_age <= cambial_age_max) %>%
        arrange(tree_id, year)

    return(urban_rwl)
}

urban_rwl <- make_urban_df(rwl_move,
                           year_min = 1920,
                           year_max = 2000,
                           year_break = 1960,
                           cambial_age_max = 100)



urban_rwl %>%
    group_by(tree_id,
             year_break) %>%
    tally() %>%
    group_by(year_break) %>%
    tally()



library(mgcv)
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
# ctrl <- list(niterPQL = 30, msVerbose = TRUE, optimMethod="L-BFGS-B")
# mod_growth1 <- gamm(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 40),
#                    data = urban_rwl,
#                   family = Gamma(link = "log"),
#                   correlation = corARMA(form = ~ year | tree_id, p = 1),
#                   random = list(species = ~1,
#                                 location_short = ~1))
#
#
# mod_growth0 <- gamm(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 40),
#                    data = urban_rwl,
#                    family = Gamma(link = "log"),
#                    correlation = corARMA(form = ~ year | tree_id, p = 0),
#                    random = list(species = ~1,
#                                  location_short = ~1))
#
#
# mod_growth2a <- gamm(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 40),
#                     data = urban_rwl,
#                     family = Gamma(link = "log"),
#                     correlation = corARMA(form = ~ year | tree_id, p = 2),
#                     random = list(species = ~1,
#                                   location_short = ~1))
# mod_growth2b <- gamm(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 40),
#                     data = urban_rwl,
#                     family = Gamma(link = "log"),
#                     correlation = corARMA(form = ~ year | tree_id, p = 2),
#                     random = list(tree_id = ~1,
#                                   species = ~1,
#                                   location_short = ~1))
mod_growth3 <- gamm(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 10),
                    data = urban_rwl,
                    family = Gamma(link = "log"),
                    correlation = corARMA(form = ~ year | tree_id, p = 2),
                    random = list(tree_id = ~1,
                                  species = ~1,
                                  location_short = ~1))
mod_growth4 <- gamm(rwl_mm ~ s(year, k = 40) + s(cambial_age, k = 40),
                    data = urban_rwl,
                    family = Gamma(link = "log"),
                    correlation = corARMA(form = ~ year | tree_id, p = 3),
                    random = list(tree_id = ~1),
                    niterPQL = 30)

mod_growth5 <- gamm(rwl_mm ~ s(year, k = 50) + s(cambial_age, k = 7),
                    data = urban_rwl,
                    family = Gamma(link = "log"),
                    correlation = corARMA(form = ~ year | tree_id, p = 3),
                    random = list(tree_id = ~1),
                    niterPQL = 50)

mod_growth6 <- gamm(rwl_mm ~ s(year, k = 70) + s(cambial_age, k = 10),
                    data = urban_rwl,
                    family = Gamma(link = "log"),
                    correlation = corARMA(form = ~ year | tree_id, p = 4),
                    random = list(tree_id = ~1),
                    niterPQL = 50)
                    # control = ctrl)

#
# mod_growth3 <- gamm(rwl_mm ~ s(year, k = 40) + s(cambial_age, k = 40),
#                     data = urban_rwl,
#                     family = Gamma(link = "log"),
#                     correlation = corARMA(form = ~ year | tree_id, p = 3),
#                     random = list(species = ~1,
#                                   location_short = ~1),
#                   control = ctrl)
# mod_growth <- gam(rwl_mm ~ s(year, k = 65) + s(cambial_age, k = 30)  +  s(location_short, species, bs = "re"), data = urban_rwl,
#                   family = Gamma(link = "log"))



layout(matrix(1:2, ncol = 2))
res <- resid(mod_growth3$lme, type = "normalized")
res <- resid(mod_growth5$lme, type = "normalized")
acf(res, lag.max = 10, main = "ACF - AR(2) errors")
pacf(res, lag.max = 10, main = "pACF- AR(2) errors")
layout(1)




# gam.check(mod_growth$gam)
# gam.check(mod_growth0$gam)
# gam.check(mod_growth2$gam)
# gam.check(mod_growth2b$gam)
# gam.check(mod_growth3$gam)
gam.check(mod_growth4$gam)
gam.check(mod_growth5$gam)

# plot(mod_growth0$gam,scheme = 2,pages = 1)
# plot(mod_growth2$gam,scheme = 2,pages = 1)
# plot(mod_growth2b$gam,scheme = 2,pages = 1)
# plot(mod_growth3$gam,scheme = 2,pages = 1)
plot(mod_growth4$gam,scheme = 2,pages = 1)

# new_dat <- expand.grid(cambial_age = c(0:100), year = c(1900:2000) ,
                       # location_short = unique(urban_rwl$location_short), species = unique(urban_rwl$species))
new_dat <- expand.grid(cambial_age = c(0:100), year = c(1900:2000))
preds <- predict(mod_growth4$gam, newdata = new_dat, type = "link", se.fit = TRUE)
# preds <- predict(mod_growth$gam, newdata = new_dat, type = "terms", exclude = "s(location_short,species)")

# sapply(mod_growth$smooth, '[[',  'label')
# mod_fixed <- rowSums(preds) + attr(preds, "constant")
# new_dat$fitted <- family(mod_growth)$linkinv(mod_fixed)
# new_dat$fitted <- preds


new_dat <- cbind(new_dat, preds) %>%
    mutate(year_group = as.factor(year <= 1950))


serror <- function(x){

    return(sqrt(sum(x^2))/length(x))

}


means <- new_dat %>%
    group_by(cambial_age, year_group) %>%
    summarize(mean_link = mean(fit),
              mean_fit_response = Gamma(link="log")$linkinv(mean_link),
              mean_se_response_low = Gamma(link="log")$linkinv(mean_link - 1.96 * serror(se.fit)),
              mean_se_response_high = Gamma(link="log")$linkinv(mean_link + 1.96 * serror(se.fit)))


# ggplot(new_dat, aes(x = cambial_age, y = fitted, color = year <= 1950, group = year)) +
#     geom_line(alpha = 0.3) +
#     stat_summary(geom = "ribbon", fun.data = "mean_cl_boot", aes(group  = year <= 1950, fill = year <= 1950), alpha = 0.6) +
#     theme_minimal()
ggplot(new_dat, aes(x = cambial_age, y = Gamma(link="log")$linkinv(fit), color = year_group, group = year)) +
    geom_line(alpha = 0.2) +
    geom_ribbon(data = means,
                inherit.aes = FALSE,
                aes(x = cambial_age, y = mean_fit_response,
                    ymin = mean_se_response_low,
                    ymax = mean_se_response_high,
                    color = year_group,
                    fill = year_group,
                    group = year_group),
                alpha = 0.35) +
    geom_line(data = means,
                inherit.aes = FALSE,
                aes(x = cambial_age,
                    y = mean_fit_response,
                    group = year_group,
                    color = year_group),
              size = 1) +
    # lims(y = c(0, 7)) +
    scale_fill_brewer(type = "qual", palette = 2) +
    scale_color_brewer(type = "qual", palette = 2) +
    theme_test(base_family = "serif")

overview <- all_series_rwl_long %>%
    filter(!site_type %in% c("rural", "natural")) %>%
    tidyr::drop_na(rwl_mm) %>%
        group_by(tree_id) %>%
    summarise(ymin = min(year),
              ymax = max(year),
              n_years = n()) %>%
    arrange(ymin) %>%
    mutate(tree_id = forcats::fct_reorder(tree_id, ymin))
overview <- urban_rwl %>%
    filter(site_global == "urban") %>%
    tidyr::drop_na(rwl_mm) %>%
        group_by(tree_id) %>%
    summarise(ymin = min(year),
              ymax = max(year),
              n_years = n()) %>%
    arrange(ymin) %>%
    mutate(tree_id = forcats::fct_reorder(tree_id, ymin))




ggplot(overview) +
    geom_segment(aes(x = ymin, xend = ymax, y = tree_id, yend = tree_id, group = tree_id))




# new_dat <- expand.grid(cambial_age = c(25, 50, 75), year = c(1940:2010) ,
#                        location_short = unique(urban_rwl$location_short), species = unique(urban_rwl$species))
#
# preds <- predict(mod_growth, newdata = new_dat, type = "terms", exclude = "s(location_short,species)")
#
# sapply(mod_growth$smooth, '[[',  'label')
#
#
# mod_fixed <- rowSums(preds) + attr(preds, "constant")
#
#
# new_dat$fitted <- family(mod_growth)$linkinv(mod_fixed)
#
#
# ggplot(new_dat, aes(x = year, y = fitted, color =as.factor(cambial_age), group = cambial_age)) +
#     geom_line(alpha = 0.3) +
#     # stat_summary(geom = "ribbon", fun.data = "mean_cl_boot"), alpha = 0.6) +
#     # theme_minimal() +
#     theme_test() +
#     geom_smooth(method = "lm")
#
#
#
#
#
#
# preds_full <- predict(mod_growth$gam, newdata = new_dat)
# new_dat$fitted <- preds_full
#
# ggplot(new_dat, aes(x = cambial_age, y = fitted, color = year <= 1970, group = year)) +
#     geom_line(alpha = 0.4) +
#     facet_grid(location_short~ species)





# application -------------------------------------------------------------

series_long <- prep_rwl_data(path_meta_cores = "C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/BIWi_INV_20190123.xlsx",
                             path_meta_trees = "C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/BIWi_INV_20190123.xlsx",
                             path_meta_sites = "C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/BIWi_INV_20190123.xlsx",
                             path_dir_fh = "C:/Users/ahurl/Documents/_work/p024_gfz_berlin-trees/ext_data/BIWI/dendro_da/Dendro/RAW/")


slide_rwl <- make_rwl_windows(df = series_long,
                              window_n = 5,
                              type = "slide")

move_rwl <- make_rwl_windows(df = series_long,
                             window_n = 5,
                             type = "move")



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
                               year_max = 2000,
                               year_break = 1960,
                               cambial_age_max = 100)


    library(mgcv)
    ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

    # mod <- gamm(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 6, by = year_break) + year_break,
    #
    #                     data = urban_rwl,
    #                     family = Gamma(link = "log"),
    #                     correlation = corARMA(form = ~ year | tree_id, p = 3),
    #                     random = list(tree_id = ~1,
    #                                   species = ~ 1),
    # niterPQL = 50)


    mod <- gam(list(rwl_mm ~ s(year, k = 30) + s(cambial_age, k = 6, by = year_break) + year_break,
                    ~ s(cambial_age, k = 6, by = year_break) + year_break),
               data = urban_rwl,
               family = mgcv::gammals(link = list("identity", "log")),
               correlation = corARMA(form = ~ year | tree_id, p = 3),
               # random = list(tree_id = ~1,
               # species = ~ 1),
               niterPQL = 50)



    mod$df <- urban_rwl


    return(mod)


}



mod6 <- apply_gam_biwi(df = series_long)
mod7 <- apply_gam_biwi(df = series_long)


layout(matrix(1:2, ncol = 2))
res <- resid(mod6$lme, type = "normalized")
res <- resid(mod7, type = "deviance")
acf(res, lag.max = 10, main = "ACF - AR(2) errors")
pacf(res, lag.max = 10, main = "pACF- AR(2) errors")
layout(1)







new_dat <- expand.grid(cambial_age = c(0:80), year = c(1920:2000)) %>%
    mutate(year_break = as.factor(ifelse(year <= 1960, "<=1960", ">1960")))
preds <- predict(mod6$gam, newdata = new_dat, type = "link", se.fit = TRUE)
# preds <- predict(mod_growth$gam, newdata = new_dat, type = "terms", exclude = "s(location_short,species)")

# sapply(mod_growth$smooth, '[[',  'label')
# mod_fixed <- rowSums(preds) + attr(preds, "constant")
# new_dat$fitted <- family(mod_growth)$linkinv(mod_fixed)
# new_dat$fitted <- preds


new_dat <- cbind(new_dat, preds) %>%
    mutate(year_group = as.factor(year <= 1950))


serror <- function(x){

    return(sqrt(sum(x^2))/length(x))

}


means <- new_dat %>%
    group_by(cambial_age, year_break) %>%
    summarize(mean_link = mean(fit),
              mean_fit_response = Gamma(link="log")$linkinv(mean_link),
              mean_se_response_low = Gamma(link="log")$linkinv(mean_link - 1.96 * serror(se.fit)),
              mean_se_response_high = Gamma(link="log")$linkinv(mean_link + 1.96 * serror(se.fit)))


# ggplot(new_dat, aes(x = cambial_age, y = fitted, color = year <= 1950, group = year)) +
#     geom_line(alpha = 0.3) +
#     stat_summary(geom = "ribbon", fun.data = "mean_cl_boot", aes(group  = year <= 1950, fill = year <= 1950), alpha = 0.6) +
#     theme_minimal()
ggplot(new_dat, aes(x = cambial_age, y = Gamma(link="log")$linkinv(fit), color = year_break, group = year)) +
    geom_line(alpha = 0.2) +
    geom_ribbon(data = means,
                inherit.aes = FALSE,
                aes(x = cambial_age, y = mean_fit_response,
                    ymin = mean_se_response_low,
                    ymax = mean_se_response_high,
                    color = year_break,
                    fill = year_break,
                    group = year_break),
                alpha = 0.35) +
    geom_line(data = means,
              inherit.aes = FALSE,
              aes(x = cambial_age,
                  y = mean_fit_response,
                  group = year_break,
                  color = year_break),
              size = 1) +
    # lims(y = c(0, 7)) +
    scale_fill_brewer(type = "qual", palette = 2) +
    scale_color_brewer(type = "qual", palette = 2) +
    theme_test(base_family = "serif")




ggplot(mod6$df,
       aes(x = cambial_age,
           y = rwl_mm,
           color = year_break,
           group = tree_id)) +
    geom_line()

