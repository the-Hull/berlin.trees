library(drake)
library(ggplot2)
library(dplyr)
loadd(full_data_set_clean)
loadd(extract_uhi_values_to_list)



#' Title
#'
#' @param full_df
#' @param extract_uhi
#'
#' @return
#' @export
#'
#' @examples
make_test_data_set <- function(full_df = full_data_set_clean,
                               extract_uhi = extract_uhi_values_to_list){


    test_set <- cbind(as.data.frame(full_df),
                      extract_uhi$Summertime_gridded_UHI_data$day)


    test_set <- test_set %>%
        # dplyr::filter(provenance == "s_wfs_baumbestand") %>%
        dplyr::mutate(STANDALTER = as.numeric(STANDALTER),
                      age_group = cut(STANDALTER, breaks = seq(0, 280, 40))) %>%
        mutate(ART_BOT = ifelse(is.na(ART_BOT),
                                paste(gattung_short, "spec."),
                                ART_BOT))


    return(test_set)
}



test_set <- berlin.trees::make_test_data_set(full_df = full_data_set_clean,
                                             extract_uhi = extract_uhi_values_to_list)


library(berlin.trees)


#' Apply models (lme4)
#'
#' @param full_df cleaned data set
#' @param extract_uhi extracted UHI data
#' @param model_list list of models to apply
#'
#' @return a list containing lme4 model outputs
#' @export
#'
#' @import furrr
#' @import dplyr
#' @import lme4
#'
apply_models <- function(test_df,
         model_list = model_list,
         n_top_species = 6){




    apply_model_full <- function(.model, .data){
        .model(.data)
    }

    top_species <- test_df %>%
        group_by(gattung_short) %>%
        count(ART_BOT) %>%
        arrange(desc(n), .by_group = TRUE) %>%
        top_n(n_top_species)


    future::plan(future::multiprocess)
    model_out <- furrr::future_map(model_list,
                                   apply_model_full,
                                   .data = test_df %>%
                                       filter(STANDALTER < 350,
                                              krone_m < 50,
                                              dbh_cm < 600,
                                              ART_BOT %in% top_species$ART_BOT[top_species$n > 150]))


    return(model_out)



}



test_out_model <- apply_models(test_df = test_set,
                               model_list = model_list)


#' Make Random-effects effect-size plot
#'
#' @param model_out output list with models
#' @param model_name Character, model name
#'
#' @return ggplot2 object and plot
#' @export
#' @import ggplot2
#' @import dplyr
#'
#'
make_ranef_plot <- function(model_out,
                            model_name = "heat_RIspecies_RSspecies_RIprovenance",
                            n_top_species = 6,
                            full_df = full_data_set_clean,
                            extract_uhi = extract_uhi_values_to_list){


    test_set <- cbind(as.data.frame(full_data_set_clean),
                      extract_uhi_values_to_list$Summertime_gridded_UHI_data$day)




    top_species <- test_set %>%
        group_by(gattung_short) %>%
        count(ART_BOT) %>%
        arrange(desc(n), .by_group = TRUE) %>%
        top_n(n_top_species)


    ranef_slopes <- model_out[[model_name]] %>%
        lme4::ranef() %>%
        as.data.frame() %>%
        filter(term == "day_2007") %>%
        mutate(grp = as.character(grp)) %>%
        arrange(as.character(grp), as.numeric(condval)) %>%
        mutate(gattung = sub("(.*:)(\\w+)(.*)", replacement = "\\2", x = as.character(grp), perl = FALSE),
               species = sub("(.*:)(.*$)", replacement = "\\2", x = as.character(grp), perl = FALSE),
               species_short = paste0(substr(gattung, 1, 1),
                                      ". ",
                                      sub("(^\\w+)( )(.*)", replacement = "\\3", x = as.character(species), perl = TRUE)),
               provenance = sub("(^\\w+):(.*)", replacement = "\\1", x = as.character(grp), perl = FALSE))


    p <- ranef_slopes %>%
        left_join(top_species, by = c("species" = "ART_BOT")) %>%
        arrange(gattung,condval ) %>%
        mutate(grp = factor(grp,levels = grp),
               species_short = forcats::fct_reorder(species_short, condval),
               gattung = forcats::fct_reorder(gattung, n, .fun = sum, .desc = TRUE)) %>%
        ggplot(aes(x = species_short, y = condval, ymin = condval - condsd, ymax = condval + condsd)) +
        geom_hline(yintercept = 0) +
        geom_point(aes(color = provenance, size = n,  group = provenance),
                   position = position_dodge(width = 0.2),
                   alpha = 0.8) +
        geom_linerange(aes(group = provenance, color = provenance),
                       position = position_dodge(width = 0.2),
                       alpha = 0.8) +
        facet_wrap(~gattung, scales = "free_y") +
        theme_bw(base_size = 16) +
        scale_color_brewer(type = "qual", palette = "Set2", direction = +1) +
        coord_flip() +
        theme(panel.border = element_rect(fill = "transparent"))

    return(p)



}




make_ranef_plot(model_out = test_out_model,
                model_name = "heat_RIspecies_RSspecies_RIprovenance")


make_ranef_plot(model_out = test_out_model,
                model_name = "heat_age_RIspecies_RSspecies_RIprovenance")






# plot_uhi_dist <- function(uhi_stack_list, uhi_stats){
#
#
# }

test_set <- cbind(as.data.frame(full_data_set_clean), extract_uhi_values_to_list$Summertime_gridded_UHI_data$day)




bad_years <- c(1989, 1962,1961, 1945, 1914, 1871, 1923)

# maybe_years <- c(1990, 1970)



test_set <- test_set %>%
    # dplyr::filter(provenance == "s_wfs_baumbestand") %>%
    dplyr::mutate(STANDALTER = as.numeric(STANDALTER),
                  age_group = cut(STANDALTER, breaks = seq(0, 280, 40))) %>%
    mutate(ART_BOT = ifelse(is.na(ART_BOT),
                            paste(gattung_short, "spec."),
                            ART_BOT))

nrow(filter(test_set, !is.na(STANDALTER), STANDALTER < 200)) / nrow(filter(test_set, !is.na(STANDALTER)))
nrow(filter(test_set, !is.na(STANDALTER), STANDALTER < 100)) / nrow(filter(test_set, !is.na(STANDALTER)))


test_set %>%
    group_by(gattung_short) %>%
    count(ART_BOT) %>%
    dplyr::top_n(10) %>%
    arrange(desc(n), .by_group = TRUE) %>%
    View()

test_set %>%
    # dplyr::filter(provenance == "s_wfs_baumbestand") %>%
    dplyr::mutate(STANDALTER = as.numeric(STANDALTER),
                  age_group = cut(STANDALTER, breaks = seq(0, 280, 40))) %>%
    filter(STANDALTER < 200) %>%
    ggplot2::ggplot(ggplot2::aes(x = STANDALTER)) +
    # ggplot2::geom_freqpoly(bins = 100) +
    ggplot2::geom_vline(xintercept = 2018 - bad_years) +
    ggplot2::geom_density(aes(fill = provenance),
                          alpha = 0.5) +
    ggplot2::facet_wrap(~gattung_short) +
    theme_bw(base_size = 16)



top_species <- test_set %>%
    group_by(gattung_short) %>%
    count(ART_BOT) %>%
    arrange(desc(n), .by_group = TRUE) %>%
    top_n(6)

test_set_nested <- test_set[,] %>%
    filter(ART_BOT %in% top_species$ART_BOT) %>%
    as.data.frame() %>%
    select(-geometry) %>%
    mutate(pca_m2 = krone_m^2 * pi) %>%
    filter(STANDALTER < 350,
           dbh_cm < 600,
           krone_m < 40) %>%
    group_by(gattung_short) %>%
    tidyr::nest()


# model_formula <- formula(pca_m2 ~ day_2007 + ART_BOT + dbh_cm)


# model_formula <- formula(pca_m2 ~ day_2007 + ART_BOT)

#
# test_set_nested <- test_set_nested %>%
#     mutate(model = lapply(data,
#                           function(df) lm(model_formula, data = df)),
#            model_stats = lapply(model,
#                                 broom::glance),
#            model_coefs = lapply(model,
#                                 broom::tidy))
# test_set_nested %>% select(gattung_short, model_stats) %>% tidyr::unnest()





#
#
# test_out <- model_list %>%
#     purrr::map(function(mod){
#
#         lapply(test_set_nested$data,
#                function(x) {
#
#                    lm(mod, data = x)
#                })
#     })
#
#
#
#
#




apply_model <- function(.model, nested_df){

    # takes data column from nested df and "loops"/maps over it
    # to apply models then
    # adds a model column to the nested data frame
    nested_df$model <- purrr::map(nested_df$data, purrr::possibly(.model, otherwise = "failed"))


    # returns nested data frame
    return(nested_df)
}


# model_list <- list(null = function(x) lm(pca_m2 ~ 1, data = x),
#                    heat_only = function(x) lm(pca_m2 ~ day_2007, data = x)  ,
#                    heat_species = function(x) lm(pca_m2 ~ day_2007 + ART_BOT, data = x)  ,
#                    heat_dbh = function(x) lm(pca_m2 ~ day_2007 + dbh_cm, data = x)  ,
#                    heat_species_dbh = function(x) lm(pca_m2 ~ day_2007 + ART_BOT + dbh_cm, data = x),
#                    heat_species_dbh_X_age = function(x) lm(pca_m2 ~ day_2007 + ART_BOT + dbh_cm * STANDALTER, data = x),
#                    dbh_only = function(x) lm(pca_m2 ~ dbh_cm, data = x),
#                    dbh_age = function(x) lm(pca_m2 ~ dbh_cm + STANDALTER, data = x),
#                    dbh_X_age = function(x) lm(pca_m2 ~ dbh_cm * STANDALTER, data = x),
#                    dbh_species = function(x) lm(pca_m2 ~ dbh_cm + ART_BOT, data = x),
#                    dbh_X_age_species = function(x) lm(pca_m2 ~ dbh_cm* STANDALTER + ART_BOT, data = x),
#                    heat_dbh_X_age = function(x) lm(pca_m2 ~ day_2007 + dbh_cm * STANDALTER, data = x))


# model_list <- list(
#     # null = function(x) lme4::lmer(dbh_cm ~ 1, data = x),
#                    # heat_only = function(x) lme4::lmer(dbh_cm ~ day_2007, data = x)  ,
#                    heat_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) , data = x) ,
#                    heat_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) , data = x)  ,
#                    # heat_age = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER, data = x)  ,
#                    # heat_age_species = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + ART_BOT, data = x)  ,
#                    heat_age_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT), data = x)  ,
#                    heat_age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 + day_2007 | ART_BOT), data = x)  ,
#                    heat_age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | ART_BOT), data = x)  ,
#                    # age_only = function(x) lme4::lmer(dbh_cm ~ STANDALTER, data = x),
#                    age_RIspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT), data = x),
#                    age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT), data = x)
#                    # species_only = function(x) lme4::lmer(dbh_cm ~ ART_BOT, data = x)
#                    )


model_list <- list(
    # null = function(x) lme4::lmer(dbh_cm ~ 1, data = x),
                   # heat_only = function(x) lme4::lmer(dbh_cm ~ day_2007, data = x)  ,
                   heat_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) , data = x) ,
                   heat_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) + provenance , data = x) ,
                   heat_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) , data = x)  ,
                   heat_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) + provenance , data = x)  ,
                   heat_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + provenance + (1 + day_2007 | provenance : ART_BOT)  , data = x)  ,
                   # heat_age = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER, data = x)  ,
                   # heat_age_species = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + ART_BOT, data = x)  ,
                   heat_age_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT), data = x)  ,
                   heat_age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT) + provenance, data = x)  ,
                   heat_age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 + day_2007 | ART_BOT), data = x)  ,
                   heat_age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | ART_BOT), data = x)  ,
                   # age_only = function(x) lme4::lmer(dbh_cm ~ STANDALTER, data = x),
                   age_RIspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT), data = x),
                   age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT) + provenance, data = x),
                   age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT), data = x),
                   age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT) + provenance, data = x)
                   # species_only = function(x) lme4::lmer(dbh_cm ~ ART_BOT, data = x)
                   )



# top_4_species <- test_set %>%
#     mutate(ART_BOT = ifelse(is.na(ART_BOT),
#                             paste(gattung_short, "spec."),
#                             ART_BOT)) %>%
#     group_by(gattung_short) %>%
#     count(ART_BOT) %>%
#     arrange(desc(n), .by_group = TRUE) %>%
#     top_n(4)
#

model_list <- list(
    # null = function(x) lme4::lmer(dbh_cm ~ 1, data = x),
    # heat_only = function(x) lme4::lmer(dbh_cm ~ day_2007, data = x)  ,
    # heat_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) , data = x) ,
    # heat_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) + provenance , data = x) ,
    # heat_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) , data = x)  ,
    # heat_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) + provenance , data = x)  ,
    heat_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + provenance + (1 + day_2007 | provenance : ART_BOT)  , data = x)  ,
    # heat_age = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER, data = x)  ,
    # heat_age_species = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + ART_BOT, data = x)  ,
    # heat_age_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT), data = x)  ,
    # heat_age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT) + provenance, data = x)  ,
    # heat_age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 + day_2007 | ART_BOT), data = x)  ,
    heat_age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | ART_BOT), data = x)  ,
    heat_age_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | provenance : ART_BOT), data = x, control = lme4::lmerControl(optimizer ="Nelder_Mead"))  ,
    # age_only = function(x) lme4::lmer(dbh_cm ~ STANDALTER, data = x),
    # age_RIspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT), data = x),
    # age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT) + provenance, data = x),
    # age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT), data = x),
    age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT) + provenance, data = x),
    age_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | provenance : ART_BOT), data = x)
    # species_only = function(x) lme4::lmer(dbh_cm ~ ART_BOT, data = x)
)




# test_out <- model_list %>%
#     purrr::map_df(apply_model,
#                test_set_nested,
#                .id = "model_id") %>%
#     # select(-data) %>%
#     mutate(coefs = purrr::map(model, broom::tidy),
#            performance = purrr::map(model, broom::glance))
#
#
# bad_out <- test_out[test_out$model %>% purrr::map_lgl(lme4::isSingular), ]
# good_out <- test_out[!{test_out$model %>% purrr::map_lgl(lme4::isSingular)}, ]
#
#
#
#
# test_out %>%
#     select(model_id, gattung_short, coefs) %>%
#     tidyr::unnest(cols = c(coefs))
#
#
# test_out %>%
#     select(model_id, gattung_short, performance) %>%
#     tidyr::unnest(cols = c(performance)) %>%
#     View()



apply_model_full <- function(.model, .data){
    .model(.data)
}



future::plan(future::multiprocess)
model_out <- furrr::future_map(model_list,
                        apply_model_full,
                        .data = test_set %>%
                            filter(STANDALTER < 350,
                                   krone_m < 50,
                                   dbh_cm < 600,
                            ART_BOT %in% top_species$ART_BOT[top_species$n > 150]))


model_out %>%
    # purrr::map(broom::tidy)
    purrr::map(function(x){

        summary(x)

        # cat("\n_________\n")

        })

model_out$heat_RIspecies_RSspecies %>%
    ranef() %>%
    as.data.frame() %>%
    filter(term == "day_2007") %>%
    arrange(as.character(grp), as.numeric(condval))

model_out$heat_age_RIspecies_RSspecies_provenance %>%
    ranef() %>%
    as.data.frame() %>%
    filter(term == "day_2007") %>%
    arrange(as.character(grp), as.numeric(condval))



ranef_slopes <- model_out$heat_age_RIspecies_RSspecies_provenance %>%
    ranef() %>%
    as.data.frame() %>%
    filter(term == "day_2007") %>%
    mutate(grp = as.character(grp)) %>%
    arrange(as.character(grp), as.numeric(condval)) %>%
    mutate(gattung = sub("(^\\w+)(.*)", replacement = "\\1", x = as.character(grp), perl = FALSE))


ranef_slopes %>%
    left_join(top_species, by = c("grp" = "ART_BOT")) %>%
    arrange(gattung,condval ) %>%
    mutate(grp = factor(grp,levels = grp)) %>%
    ggplot(aes(x = grp, y = condval, ymin = condval - condsd, ymax = condval + condsd)) +
    geom_hline(yintercept = 0) +
    geom_point(aes(color = condval > 0, size = n)) +
    geom_linerange() +
    facet_wrap(~gattung, scales = "free_y") +
    theme_bw() +
    coord_flip()



test_set %>%
    filter(gattung_short == "Populus",
           STANDALTER < 350,
           krone_m < 50,
           dbh_cm < 600,
           ART_BOT %in% top_species$ART_BOT[top_species$n > 150]) %>%
    ggplot(aes(y = dbh_cm,
               x = day_2007)) +
    geom_smooth(method = "lm", size = 1.5, color = "black", linetype = 2, alpha = 0.5) +
    geom_point(aes(color = ART_BOT), alpha = 0.2) +
    geom_smooth(method = "lm", aes(color = ART_BOT)) +
    facet_wrap(~provenance)




test_set %>%
    filter(gattung_short == "Pinus",
           ART_BOT %in% top_species$ART_BOT[top_species$n > 150]) %>%
    ggplot(aes(y = dbh_cm,
               x = day_2007)) +
    # geom_point(aes(color = ART_BOT)) +
    geom_smooth(method = "lm", aes(color = ART_BOT))









ranef_slopes <- model_out$heat_RIspecies_RSspecies_RIprovenance %>%
    ranef() %>%
    as.data.frame() %>%
    filter(term == "day_2007") %>%
    mutate(grp = as.character(grp)) %>%
    arrange(as.character(grp), as.numeric(condval)) %>%
    mutate(gattung = sub("(.*:)(\\w+)(.*)", replacement = "\\2", x = as.character(grp), perl = FALSE),
           species = sub("(.*:)(.*$)", replacement = "\\2", x = as.character(grp), perl = FALSE),
           species_short = paste0(substr(gattung, 1, 1),
                                 ". ",
                                 sub("(^\\w+)( )(.*)", replacement = "\\3", x = as.character(species), perl = TRUE)),
           provenance = sub("(^\\w+):(.*)", replacement = "\\1", x = as.character(grp), perl = FALSE))


ranef_slopes %>%
    left_join(top_species, by = c("species" = "ART_BOT")) %>%
    arrange(gattung,condval ) %>%
    mutate(grp = factor(grp,levels = grp),
           species_short = forcats::fct_reorder(species_short, condval),
           gattung = forcats::fct_reorder(gattung, n, .fun = sum, .desc = TRUE)) %>%
    ggplot(aes(x = species_short, y = condval, ymin = condval - condsd, ymax = condval + condsd)) +
    geom_hline(yintercept = 0) +
    geom_point(aes(color = provenance, size = n,  group = provenance),
               position = position_dodge(width = 0.2),
               alpha = 0.8) +
    geom_linerange(aes(group = provenance, color = provenance),
                   position = position_dodge(width = 0.2),
                   alpha = 0.8) +
    facet_wrap(~gattung, scales = "free_y") +
    theme_minimal() +
    scale_color_brewer(type = "qual", palette = "Set2", direction = +1) +
    coord_flip() +
    theme(panel.border = element_rect(fill = "transparent"))



model_res <-  berlin.trees::apply_models(df = model_df %>%
                                           filter(STANDALTER < 350 &
                                                      krone_m < 50 &
                                                      dbh_cm < 600),
                                       model_list = model_list,
                                       n_top_species = 6,
                                       min_individuals = 150)

