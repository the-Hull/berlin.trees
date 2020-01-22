library(drake)
library(ggplot2)
library(dplyr)
loadd(full_data_set_clean)
loadd(extract_uhi_values_to_list)




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
    ggplot2::facet_wrap(~gattung_short)



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
                            filter(ART_BOT %in% top_species$ART_BOT[top_species$n > 150]))


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
    arrange(gattung,condval ) %>%
    mutate(grp = factor(grp,levels = grp)) %>%
    ggplot(aes(x = grp, y = condval, ymin = condval - condsd, ymax = condval + condsd)) +
    geom_hline(yintercept = 0) +
    geom_linerange() +
    geom_point(aes(color = condval > 0)) +
    facet_wrap(~gattung, scales = "free_y") +
    theme_bw() +
    coord_flip()
