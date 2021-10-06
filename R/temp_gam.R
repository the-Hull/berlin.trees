# exploratory GAM analyses


library(drake)
library(sf)
library(dplyr)
library(mgcv)

# load data ---------------------------------------------------------------

loadd(full_data_set_clean_with_UHI_covariates)
geo_recover <- readRDS("./analysis/data/raw_data/tree_splits/berlin_trees_GEOBACKUP.RDS")

full_data_set_clean_with_UHI_covariates <- cbind(full_data_set_clean_with_UHI_covariates, berlin_id = geo_recover$berlin_id) %>%
    tidyr::unite("old_id", KENNZEICH, GISID)



split_files <- list.files(path = "./analysis/data/raw_data/tree_splits/",
                          pattern = "cleaned.Rds$",
                          full.names = TRUE)


cleaned_data_join <- lapply(split_files,
                            readRDS) %>%
    dplyr::bind_rows() %>%
    dplyr::ungroup() %>%
    tidyr::unite("check_id", KENNZEICH, GISID) %>%
    dplyr::filter(is.na(.annotation)) %>%
    dplyr::select(berlin_id, check_id) %>%
    dplyr::left_join(full_data_set_clean_with_UHI_covariates, by = "berlin_id") %>%
    tidyr::drop_na(STANDALTER, dbh_cm)
# %>% sf::st_set_geometry(.$geometry)


# cleaned_data_join <- purrr::map2(
#     cleaned_data,
#     full_data_set_clean_with_UHI_covariates %>%
#         select(ID, STANDORTNR, KENNZEICH, geometry, match_id) %>%
#         split(., f = .$BEZIRK),
#     ~left_join(.x,
#                .y,
#                by = c(".dcrkey" = "match_id"))) %>%
#     dplyr::bind_rows()

# make model df -----------------------------------------------------------

mdf <- cleaned_data_join %>%
    dplyr::select(provenance,
                  ART_BOT,
                  BEZIRK = BEZIRK,
                  gattung_short,
                  STANDALTER,
                  dbh_cm,
                  day_2007,
                  T2M22HMEA,
                  T2M14HMEA,
                  T2M04HMEA,
                  geometry,
                  species_corrected,
                  baumsch_flaeche_m2,
                  soil_nutrients_swertstu,
                  lcz_prop_2,
                  lcz_prop_3,
                  lcz_prop_6,
                  lcz_prop_4) %>%
    sf::st_set_geometry(value = .$geometry) %>%
    mutate(provenance = as.factor(provenance),
           species_corrected = as.factor(species_corrected),
           soil_nutrients_swertstu = as.numeric(as.character(soil_nutrients_swertstu)),
           baumsch_flaeche_m2 = as.numeric(as.character(baumsch_flaeche_m2)))

mdf[ , c("x", "y")] <- sf::st_coordinates(mdf)

mdf <- mdf[-which(mdf$dbh_cm >30 & mdf$STANDALTER <= 7),]



# filter data
mdf_tilia <- mdf %>%
    dplyr::filter(STANDALTER >= 15,
                  STANDALTER <= 120,
                  dbh_cm >= 5,
                  dbh_cm <= 200,
                  gattung_short == "Tilia",
                  # species_corrected == "Acer platanoides",
                  # provenance != "s_uferbaeume")
                  provenance == "s_wfs_baumbestand")

species_top <- mdf_tilia %>%
    group_by(species_corrected) %>%
    tally(sort = TRUE) %>%
    slice_head(n = 2)

mdf_tilia <- mdf_tilia %>%
    filter(species_corrected %in% species_top$species_corrected)


# top_species <- mdf %>%
#     group_by(species_corrected) %>%
#     tally(sort = TRUE) %>%
#     slice_head(n = 1) %>%
#     pull(species_corrected)

# mdf <- mdf %>%
#     dplyr::filter(species_corrected %in% top_species)

# model data exclusion


model_basic <- lm(dbh_cm ~ poly(STANDALTER, 2):species_corrected, data = mdf_tilia)

plot(resid(model_basic))
hist(resid(model_basic))

mad_cutoff <- 0 + c(-1,+1) * 6 * mad(resid(model_basic))

mad_select <- which(resid(model_basic) >= mad_cutoff[1] & resid(model_basic) <= mad_cutoff[2])

model_mad <- lm(dbh_cm ~ poly(STANDALTER, 2), data = mdf_tilia[mad_select, ])
plot(resid(model_mad), col = as.numeric(as.factor(mdf_tilia[mad_select, "species_corrected", drop = TRUE])))
hist(resid(model_mad))

newdata = data.frame( rs = resid(model_mad),
                      spec = mdf_tilia[mad_select, "species_corrected", drop = TRUE])


ggplot(newdata, aes(x = seq_len(NROW(newdata)), y = rs, color = spec)) +
    geom_point(alpha = 0.2, show.legend = FALSE) +
    facet_wrap(~spec)

ggplot(mdf_tilia, aes(x = STANDALTER, y = dbh_cm)) +
    geom_point(alpha = 0.2) +
    labs(x = "age", y = "Diameter (cm)") +
    geom_point(inherit.aes = FALSE,
               data = mdf_tilia[mad_select, ] ,
               color = "pink",
               aes(x = STANDALTER, y = dbh_cm)) +
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 2),
                color = "black") +
    geom_smooth(inherit.aes = FALSE,
                data = mdf_tilia[mad_select, ],
                aes(x = STANDALTER, y = dbh_cm),
                method = "lm",
                formula = y ~ poly(x, 2),
                color = "red") +
    geom_smooth(inherit.aes = FALSE,
                data = mdf_tilia[mad_select, ],
                aes(x = STANDALTER, y = dbh_cm),
                method = "lm",
                formula = y ~ poly(x, 4),
                color = "steelblue1")


mod1 <- lm(dbh_cm ~ STANDALTER, mdf_tilia[mad_select, ])
mod2 <- lm(dbh_cm ~ poly(STANDALTER, 2), mdf_tilia[mad_select, ])
mod3 <- lm(dbh_cm ~ poly(STANDALTER, 4), mdf_tilia[mad_select, ])
mod1 <- lm(dbh_cm ~ STANDALTER, mdf_tilia[, ])
mod2 <- lm(dbh_cm ~ poly(STANDALTER, 2), mdf_tilia[, ])
mod3 <- lm(dbh_cm ~ poly(STANDALTER, 4), mdf_tilia[, ])

anova(mod1, mod2, mod3)
AIC(mod1, mod2, mod3)


# gam testing -------------------------------------------------------------
# simple x,y

set.seed(123)
simple_spatial <- mgcv::bam(dbh_cm  ~
                                species_corrected +
                                s(STANDALTER, bs = "tp", k = 80, by = species_corrected, m = 2),
                            # s(as.numeric(baumsch_flaeche_m2), bs = "tp", k = 100),
                            data = mdf_tilia[mad_select, ],
                            family = Gamma(link = "log"),
                            discrete = TRUE,
                            nthreads = c(10, 1))



gam.check(simple_spatial, k.sample = 40000)
summary(simple_spatial)





simple_spatial <- mgcv::gam(
    list(
        dbh_cm ~ ti(day_2007, STANDALTER, bs = "tp", k = 30) +
            s(day_2007, bs = "tp", k = 20) +
            s(STANDALTER, bs = "tp", k = 20) +
            s(species_corrected, bs = "fs") +
            species_corrected,
        # ~ STANDALTER
        ~ s(STANDALTER, k = 20)
    ),
    family = gammals(link = list("identity", "log") ),
    data = mdf_tilia[mad_select, ] %>%
        mutate(species_corrected = as.factor(species_corrected)))


gam.check(simple_spatial)
summary(simple_spatial)
mgcv::plot.gam(simple_spatial, pages = 1, scheme = 2)


# variable smooths and intercepts for species
simple_spatial <- mgcv::gam(
    list(
        dbh_cm ~
            ti(day_2007, STANDALTER, bs = "tp", k = 20, m = 2) +
            s(day_2007, bs = "tp", k = 10) +
            s(STANDALTER, bs = "tp", k = 10) +
            # s(species_corrected, by = "re") +
            s(STANDALTER, day_2007, species_corrected, bs = "re", k = 5),
        # ~ STANDALTER
        ~ s(STANDALTER, k = 20)
    ),
    family = gammals(link = list("identity", "log") ),
    method = "ML",
    data = mdf_tilia[mad_select, ] %>%
        mutate(species_corrected = as.factor(species_corrected)))


gam.check(simple_spatial)
summary(simple_spatial)
mgcv::plot.gam(simple_spatial, pages = 1, scheme = 2)




#
#
# simple_spatial <- lme4::lmer(dbh_cm ~ day_2007*poly(STANDALTER,2) + (day_2007-1|species_corrected),
#                       # family = Gamma(),
#                       # family = Gamma(link = "identity"),
#                       # family = inverse.gaussian(),
#     data = mdf_tilia[mad_select, ])
#
#
# summary(simple_spatial)
#
# visreg::visreg2d(simple_spatial, xvar = "STANDALTER", yvar = "day_2007",
#                  type = "conditional",
#                  scale = "response",
#                  plot.type = "persp")
#
# visreg::visreg2d(simple_spatial, xvar = "STANDALTER", yvar = "day_2007",
#                  type = "conditional",
#                  scale = "response",
#                  plot.type = "image")

# simple_spatial <- lme4::glmer(dbh_cm ~ day_2007*poly(STANDALTER,2) + (1|BEZIRK),
#                       # family = Gamma(link = "log"),
#                       # family = Gamma(link = "identity"),
#                       # family = inverse.gaussian(),
#     data = mdf[mad_select, ])
#

# summary(simple_spatial)

# visreg::visreg2d(simple_spatial, xvar = "STANDALTER", yvar = "day_2007",
#                  type = "conditional",
#                  scale = "response",
#                  plot.type = "persp")

plot(simple_spatial)






library(DHARMa)

simout  <-  simulateResiduals(simple_spatial,  n=500, plot = TRUE)
# simout  <-  simulateResiduals(model_mad,  n=250, plot = TRUE)
# simout  <-  simulateResiduals(mod2,  n=250, plot = TRUE)
plot(simout)
testResiduals(simout)
plotResiduals(simout,quantreg = TRUE)

plotResiduals(simout, simple_spatial@frame$day_2007, quantreg = TRUE)



# Testing -----------------------------------------------------------------

set.seed(123)
simple_spatial <- mgcv::bam(dbh_cm ~
                                s(STANDALTER, bs = "tp", k = 75) +
                                s(STANDALTER, bs = "tp", k = 75, by = species_corrected, m = 1) +
                                s(day_2007, bs = "tp", k = 200) +
                                s(day_2007, bs = "tp", k = 200, by = species_corrected, m = 1) +
                                s(species_corrected, bs = "re"),
                            # s(as.numeric(baumsch_flaeche_m2), bs = "tp", k = 100),
                            data = mdf_tilia %>%
                                mutate(species_corrected = as.factor(species_corrected)),
                            family = Gamma(link = "log"),
                            discrete = TRUE,
                            nthreads = c(10, 1))



gam.check(simple_spatial, k.sample = 40000)
summary(simple_spatial)



# trial 2 -----------------------------------------------------------------

# filter data
mdf_tilia <- mdf %>%
    dplyr::filter(STANDALTER >= 15,
                  STANDALTER <= 100,
                  dbh_cm >= 5,
                  dbh_cm <= 200,
                  gattung_short == "Tilia",
                  # species_corrected == "Acer platanoides",
                  # provenance != "s_uferbaeume")
                  provenance == "s_wfs_baumbestand")

species_top <- mdf_tilia %>%
    group_by(species_corrected) %>%
    tally(sort = TRUE) %>%
    slice_head(n = 2)

mdf_tilia <- mdf_tilia %>%
    filter(species_corrected %in% species_top$species_corrected)

mdf_tilia <- mdf_tilia %>%
    mutate(species_corrected = as.factor(species_corrected),
           BEZIRK = as.factor(BEZIRK),
           baumsch_flaeche_m2 = as.numeric(baumsch_flaeche_m2)) %>%
    tidyr::drop_na(dbh_cm, STANDALTER, day_2007)

set.seed(123)

simple_tilia_base <- mgcv::bam(dbh_cm ~
                                   s(x,y, k = 1400) +
                                   species_corrected +
                                   s(STANDALTER, by = species_corrected, m = 1, bs = "tp"),
                               # te(baumsch_flaeche_m2, STANDALTER, by = species_corrected, k = 30, m = 2),
                               select = TRUE,
                               #,
                               method = "fREML",
                               # data = mdf_tilia,
                               data = mdf_tilia,
                               family = Gamma(link = "log"),
                               # family = quasi(link = "identity", variance = "mu^2"),
                               discrete = FALSE,
                               nthreads = c(4, 1))


simple_tilia <- mgcv::bam(dbh_cm ~
                              s(x,y, k = 200) +
                              species_corrected +
                              s(day_2007, by = species_corrected) +
                              s(STANDALTER, by = species_corrected) +
                              ti(day_2007, STANDALTER, by = species_corrected, k = 40, m = 2, bs = "tp"),
                          # te(baumsch_flaeche_m2, STANDALTER, by = species_corrected, k = 30, m = 2),
                          select = TRUE,
                          #,
                          method = "fREML",
                          # data = mdf_tilia,
                          data = mdf_tilia[mad_select, ],
                          family = Gamma(link = "log"),
                          # family = quasi(link = "identity", variance = "mu^2"),
                          discrete = TRUE,
                          nthreads = c(4, 1))

simple_tilia_indi <- mgcv::bam(dbh_cm ~
                                   s(x,y, k = 200) +
                                   species_corrected +
                                   s(day_2007, by = species_corrected) +
                                   s(STANDALTER, by = species_corrected),
                               # te(baumsch_flaeche_m2, STANDALTER, by = species_corrected, k = 30, m = 2),
                               select = TRUE,
                               #,
                               method = "fREML",
                               # data = mdf_tilia,
                               data = mdf_tilia[mad_select, ],
                               family = tw(link = "log"),
                               # family = Gamma(link = "log"),
                               # family = quasi(link = "identity", variance = "mu^2"),
                               discrete = TRUE,
                               nthreads = c(4, 1))


simple_tilia_GS <- mgcv::bam(dbh_cm ~
                                 # s(x,y, k = 400) +
                                 te(STANDALTER, T2M14HMEA, k = 40, m = 1) +
                                 t2(STANDALTER, T2M14HMEA, species_corrected, bs = c("tp", "tp", "re"), m = 2, full = TRUE, k = 60) +
                                 s(species_corrected, BEZIRK, bs = "re"),
                             # te(baumsch_flaeche_m2, STANDALTER, by = species_corrected, k = 30, m = 2),
                             #,
                             method = "fREML",
                             # method = "fREML",
                             # data = mdf_tilia,
                             data = mdf_tilia,
                             family = Gamma(link = "log"))
# discrete = FALSE,

simple_tilia_GI <- mgcv::bam(dbh_cm ~
                                 # species_corrected +
                                 s(species_corrected, bs = "re") +
                                 s(STANDALTER, bs = "tp", k = 20) +
                                 s(STANDALTER, bs = "tp", by = species_corrected, m = 1, k = 20) +
                                 te(STANDALTER, T2M14HMEA, m = 2, k = 40) +
                                 te(STANDALTER, T2M14HMEA, by = species_corrected, m = 1, k = 50) +
                                 s(lcz_prop_6, bs = "tp"),

                             # te(baumsch_flaeche_m2, STANDALTER, by = species_corrected, k = 30, m = 2),
                             #,
                             method = "fREML",
                             # discrete = TRUE,
                             # method = "fREML",
                             # data = mdf_tilia,
                             data = mdf_tilia,
                             family = Gamma(link = "log"))
# discrete = FALSE,
# nthreads = c(4, 1))
simple_tilia_GSm <- mgcv::gam(dbh_cm ~
                                  # s(x,y, k = 400) +
                                  te(STANDALTER, day_2007, k = 40, m = 1) +
                                  t2(STANDALTER, day_2007, species_corrected, bs = c("tp", "tp", "re"), m = 2, full = TRUE, k = 60) +
                                  s(species_corrected, BEZIRK, bs = "re"),
                              # te(baumsch_flaeche_m2, STANDALTER, by = species_corrected, k = 30, m = 2),
                              #,
                              # method = "fREML",
                              method = "REML",
                              # data = mdf_tilia,
                              data = mdf_tilia[mad_select, ])
# family = Gamma(link = "log"),
# discrete = FALSE,
# nthreads = c(4, 1))

# simple_tilia <- mgcv::bam(dbh_cm ~
#                                 # s(STANDALTER) +
#                                 s(x, y, k = 200),
#                             # s(as.numeric(baumsch_flaeche_m2), bs = "tp", k = 100),
#                             data = mdf_tilia %>%
#                                 mutate(species_corrected = as.factor(species_corrected)) %>%
#                                 tidyr::drop_na(STANDALTER, day_2007),
#                             family = Gamma(link = "log"),
#                             discrete = FALSE,
#                             nthreads = c(10, 1))



gam.check(simple_tilia)
summary(simple_tilia)
mgcv::plot.gam(simple_tilia, pages = 1, scheme = 2, seWithMean = TRUE)
vis.gam(simple_tilia, plot.type = "persp", theta = 310, phi = 12)

plot(resid(simple_tilia, type = "deviance")~ mdf_tilia$day_2007)


simple_spatial_tilia <- mgcv::bam(dbh_cm ~ te(STANDALTER, day_2007, k = 60),
                                  # s(as.numeric(baumsch_flaeche_m2), bs = "tp", k = 100),
                                  data = mdf_tilia %>%
                                      mutate(species_corrected = as.factor(species_corrected)),
                                  family = Gamma(link = "log"),
                                  discrete = FALSE,
                                  nthreads = c(10, 1))



gam.check(simple_spatial_tilia, k.sample = 40000)
summary(simple_spatial_tilia)
mgcv::plot.gam(simple_spatial_tilia, pages = 1, scheme = 2)



library(DHARMa)

simout  <-  simulateResiduals(simple_tilia_GI,  n=500, plot = TRUE)
# simout  <-  simulateResiduals(model_mad,  n=250, plot = TRUE)
# simout  <-  simulateResiduals(mod2,  n=250, plot = TRUE)
plot(simout)
testResiduals(simout)
plotResiduals(simout,quantreg = TRUE)

plotResiduals(simout, simple_spatial@frame$day_2007, quantreg = TRUE)




# x = day 2007 ------------------------------------------------------------

pdata <- with(mdf_tilia %>%
                  mutate(species_corrected = as.factor(species_corrected)),
              expand.grid(T2M14HMEA = seq(26.73466 , 32.39471, length.out = 200),
                          x = 385785,
                          y = 5816681,
                          STANDALTER = c(30, 50, 80),
                          BEZIRK = as.factor(unique(mdf_tilia$BEZIRK)),
                          species_corrected = as.factor(unique(mdf_tilia$species_corrected))
              ))
fit <- predict(simple_tilia_GS , pdata, type = "response")
# ind <- mgcv::exclude.too.far(pdata$day_2007, pdata$STANDALTER,
#                              mdf_tilia[mad_select, ]$day_2007, mdf_tilia[mad_select, ]$STANDALTER, dist = 0.1)
# fit[ind] <- NA
pred <- cbind(pdata, Fitted = fit)

pdata <- with(mdf_tilia %>%
                  mutate(species_corrected = as.factor(species_corrected)),
              expand.grid(day_2007 = c(-3, 0, 3, 6),
                          baumsch_flaeche_m2 = c(2,4,8),
                          x = 385785,
                          y = 5816681,
                          STANDALTER = 15:100,
                          species_corrected = as.factor(unique(mdf_tilia$species_corrected))
              ))
fit <- predict(simple_tilia, pdata, type = "response")
# ind <- mgcv::exclude.too.far(pdata$day_2007, pdata$STANDALTER,
#                              mdf_tilia[mad_select, ]$day_2007, mdf_tilia[mad_select, ]$STANDALTER, dist = 0.1)
# fit[ind] <- NA
pred <- cbind(pdata, Fitted = fit)




plt <- ggplot(pred, aes(linetype = as.factor(BEZIRK), y = Fitted, x = (T2M14HMEA), color = species_corrected)) +
    geom_line()+
    # facet_wrap(~ species_corrected, ncol = 2) +
    # scale_color_brewer(type = "qual", palette = "Set2") +
    theme(legend.position = 'right') +
    # facet_wrap(~species_corrected) +
    # geom_smooth(method = "lm")
    # geom_smooth() +
    theme_minimal(base_size = 16) +
    facet_wrap(~STANDALTER+species_corrected) +
    labs(linetype = "Age Class", color = "Species", x = expression(UHI~Magnitude~(degree~C)), y = "Mean DBH (cm)")
plt


plt <- ggplot(pred, aes(linetype = as.factor(baumsch_flaeche_m2),
                        y = Fitted, x = day_2007, color = as.factor(day_2007))) +
    geom_line()+
    # facet_wrap(~ species_corrected, ncol = 2) +
    # scale_color_brewer(type = "qual", palette = "Set2") +
    theme(legend.position = 'right') +
    scale_color_viridis_d(option = "inferno", end = 0.9) +
    # facet_wrap(~species_corrected+as.factor(baumsch_flaeche_m2), ncol = 3) +
    facet_wrap(~species_corrected, ncol = 3) +
    # geom_smooth(method = "lm")
    # geom_smooth() +
    theme_minimal(base_size = 16) +
    labs(linetype = "Baumsch (m2)", color = "UHI Magnitude", x = "Age", y = "Mean DBH (cm)") +
    geom_vline(xintercept = 0)
plt


# df prep -----------------------------------------------------------------


model_params <- list(n_max_species = 10,
                     age_cutoff = 150,
                     mad_factor = 7)
out_test

#' Prep model data frame
#'
#' Defines df with top species by abundance, max age and the median absolute
#' deviance score used to remove gross outliers.
#'
#' @param dset
#' @param model_params
#' @param plot
#' @param path
#'
#' @return dset filtered according to the model params and has two additional columns.
#' One of these is `mad_select`, (logical), which can be used to subset the df.
#' @export
#'
#' @examples
prep_model_df <- function(dset,
                          model_params,
                          plot = TRUE,
                          path = "./analysis/figures/diagnostic_01_model_data.png"){



    top_species <- dset %>%
        group_by(species_corrected) %>%
        tally(sort = TRUE) %>%
        top_n(model_params$n_max_species)



    df_nest <- dset %>%
        dplyr::filter(species_corrected %in% top_species$species_corrected,
                      STANDALTER <= model_params$age_cutoff) %>%
        tidyr::nest(cols = -species_corrected) %>%
        mutate(
            mod = purrr::map(
                cols,
                function(df){
                    # lm(dbh_cm ~ poly(STANDALTER, degree = 1, raw = TRUE), data = df)
                    # glm(dbh_cm ~ STANDALTER, family = Gamma(link = "log"), data = df)
                    glm(dbh_cm ~ poly(STANDALTER, 2), family = Gamma(link = "log"), data = df)
                }),
            cols = purrr::modify2(
                cols,
                mod,
                ~mutate(.x,
                        fitted_dat = predict(.y, .x, type = "response"),
                        resid_val = resid(.y),
                        mad_cutoff = +1 * model_params$mad_factor * mad(resid(.y)),
                        mad_select = abs(resid_val) <= mad_cutoff
                )
            )
        ) %>%
        select(-mod) %>%
        tidyr::unnest(cols = cols)


    if(plot == TRUE){



        p <- df_nest %>%
            arrange(desc(mad_select)) %>%
            ggplot(aes(x = STANDALTER,
                       y = dbh_cm,
                       color = mad_select,
                       group = species_corrected)) +
            geom_point() +
            labs(color = "Retained point",
                 subtitle = sprintf("dbh ~ STANDALTER^2; MAD Cutoff factor +/- %i x MAD", model_params$mad_factor)) +
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

    return(df_nest)

}



tt <- prep_model_df(dset = out_test, model_params = model_params, plot = TRUE)

model_params$mad_factor <- 6

tt2 <- prep_model_df(dset = tt[tt$mad_select, ], model_params = model_params, plot = TRUE,
                     path = "./analysis/figures/diagnostic_01_model_data_iter2.png")







forms <- list("mI_age_by_species" =
                  dbh_cm ~ s(STANDALTER, by = species_corrected) + s(species_corrected, bs = "re"),

              # AGE + Heat by Species
              "mI_age_by_species_ADD_heat14_by_species" =
                  dbh_cm ~ s(STANDALTER, by = species_corrected) + s(T2M14HMEA, by = species_corrected) + s(species_corrected, bs = "re"),
              "mI_age_by_species_ADD_heat04_by_species" =
                  dbh_cm ~ s(STANDALTER, by = species_corrected) + s(T2M04HMEA, by = species_corrected) + s(species_corrected, bs = "re"),
              "mI_age_by_species_ADD_heat22_by_species" =
                  dbh_cm ~ s(STANDALTER, by = species_corrected) + s(T2M04HMEA, by = species_corrected) + s(species_corrected, bs = "re"),

              # AGE x HEAT by Species
              "mI_age_x_heat14_by_species" =
                  dbh_cm ~ te(STANDALTER, T2M14HMEA, by = species_corrected, m = 2) + species_corrected,
              "mI_age_x_heat04_by_species" =
                  dbh_cm ~ te(STANDALTER, T2M04HMEA, by = species_corrected, m = 2) + species_corrected,
              "mI_age_x_heat22_by_species" =
                  dbh_cm ~ te(STANDALTER, T2M22HMEA, by = species_corrected, m = 2) + species_corrected)


fams <- list(fams = rep(c(expression(Gamma(link = "identity"))), length(forms)))


model_grid <- dplyr::tibble(forms, fams)  %>%
    mutate(mod_names = names(forms))




test <- apply_gam_mod(model_grid = model_grid[1:3, ], dat = model_df[model_df$mad_select,])

model_grid_bad <- model_grid
model_grid_bad[2,1:2] <- NA
test <- apply_gam_mod(model_grid = model_grid_bad[1:3, ], dat = model_df[model_df$mad_select,])



# explore spatial ---------------------------------------------------------




apply_gam_mod(path = "./analysis/data/models/stat/filtered/test",
              model_grid = model_grid, dat = model_df[model_df$diag_mad_select,],
              overwrite = FALSE)


dir.create("./analysis/data/models/stat/filtered/test")


library(parallel)

###see if you have multiple cores

detectCores()

###indicate number of cores used for parallel processing
if (detectCores()>1) {
    cl <- makeCluster(detectCores()-1)
} else cl <- NULL

cl



model_df$soil_nutrients_swert <- as.numeric(model_df$soil_nutrients_swert)


apply_gam_mod(path = "./analysis/data/models/stat/filtered/test",
              model_grid = model_grid[9,], dat = model_df[model_df$diag_mad_select,],
              overwrite = FALSE)



mod9 <- mgcv::bam(formula = model_grid[9,]$forms[[1]],
                  data = model_df[model_df$diag_mad_select,],
                  family = Gamma(link = "log"),
                  # cl = cl
                  discrete = TRUE
)


mod10 <- mgcv::bam(formula = model_grid[9,]$forms[[1]],
                  data = model_df[model_df$diag_mad_select & model_df$provenance == "s_wfs_baumbestand",],
                  family = Gamma(link = "log"),
                  # cl = cl
                  discrete = TRUE
)

mod11 <- mgcv::bam(dbh_cm ~ s(X, Y, k = 200, bs = "ds") +
                       te(STANDALTER, T2M14HMEA, by = species_corrected, m = 1, k = c(10, 30)) + species_corrected,
                   data = model_df[model_df$diag_mad_select & model_df$provenance == "s_wfs_baumbestand",],
                   family = Gamma(link = "log"),
                   # cl = cl
                   discrete = TRUE)

mod11 <- mgcv::bam(dbh_cm ~ s(X, Y, k = 200, bs = "ds") +
                       te(STANDALTER, T2M14HMEA, by = species_corrected, m = 1, k = c(5, 15)) + species_corrected,
                   data = model_df[model_df$provenance == "s_wfs_baumbestand",],
                   family = Gamma(link = "log"),
                   # cl = cl
                   discrete = TRUE)

model_df$BEZIRK <- as.factor(model_df$BEZIRK)
mod12 <- mgcv::bam(dbh_cm ~ s(X, Y, k = 200, bs = "ds") +
                       te(STANDALTER, T2M14HMEA, by = species_corrected, m = 1, k = c(5, 15)) + species_corrected + s(BEZIRK, bs = "re"),
                   data = model_df[model_df$provenance == "s_wfs_baumbestand",],
                   family = Gamma(link = "log"),
                   # cl = cl
                   discrete = TRUE)
mod13 <- mgcv::bam(dbh_cm ~ s(X, Y, k = 200, bs = "ds") +
                       te(STANDALTER, T2M14HMEA, by = species_corrected, m = 1, k = c(5, 15)) +
                       species_corrected +
                       s(BEZIRK, bs = "re") +
                       s(soil_nutrients_swert, k = 20) +
                       s(building_heigt_m,  k = 20),
                   data = model_df[model_df$provenance == "s_wfs_baumbestand",],
                   family = Gamma(link = "log"),
                   # cl = cl
                   discrete = TRUE)

mod14 <- readRDS("analysis/data/models/stat/fulldf/mI_age_x_temp_by_species_reBEZIRK_var-mod2015_T2M14HMEA.Rds")

mgcv::gam.check(mod9)
mgcv::gam.check(mod10)
mgcv::gam.check(mod11)
mgcv::gam.check(mod12)
mgcv::gam.check(mod13)
mgcv::gam.check(mod14)

pdata <- with(model_df[model_df$provenance == "s_wfs_baumbestand",] %>%
# pdata <- with(model_df[model_df$diag_mad_select & model_df$provenance == "s_wfs_baumbestand",, ] %>%
                  mutate(species_corrected = as.factor(species_corrected)),
              expand.grid(mod2015_T2M14HMEA = seq(min(mod2015_T2M14HMEA, na.rm = TRUE),
                                          max(mod2015_T2M14HMEA, na.rm = TRUE), length.out = 200),
                          X = 385785,
                          Y = 5816681,
                          # STANDALTER = c(30, 50, 80),
                          STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
                          species_corrected = as.factor(unique(species_corrected)),
                          building_heigt_m = median(building_heigt_m, na.rm = TRUE),
                          # soil_nutrients_swert = median(soil_nutrients_swert, na.rm = TRUE),
                          BEZIRK = as.factor(unique(BEZIRK))
              ))



# fit <- predict(mod9 , pdata, type = "response", se.fit = TRUE)
fit <- predict(mod9 , pdata, se.fit = TRUE)
fit10 <- predict(mod10 , pdata, se.fit = TRUE)
fit11 <- predict(mod10 , pdata, se.fit = TRUE)
fit12 <- predict(mod12 , pdata, se.fit = TRUE, exclude = "s(BEZIRK)")
fit12 <- predict(mod12 , pdata, se.fit = TRUE, exclude = "s(BEZIRK)")
fit13 <- predict(mod13 , pdata, se.fit = TRUE, exclude = "s(BEZIRK)")
fit14 <- predict(mod14 , pdata, se.fit = TRUE, exclude = "s(BEZIRK)")
fit15 <- predict(mod15 , pdata, se.fit = TRUE, exclude = "s(BEZIRK)")
# ind <- mgcv::exclude.too.far(pdata$day_2007, pdata$STANDALTER,
#                              mdf_tilia[mad_select, ]$day_2007, mdf_tilia[mad_select, ]$STANDALTER, dist = 0.1)
# fit[ind] <- NA
pred <- cbind(pdata, Fitted = fit14)
pred$se.low <- pred$Fitted.fit - 1.96 * pred$Fitted.se.fit
pred$se.high <- pred$Fitted.fit + 1.96 * pred$Fitted.se.fit


ifun <- family(mod9)$linkinv
ifun <- family(mod10)$linkinv
ifun <- family(mod11)$linkinv
ifun <- family(mod15)$linkinv

pred$response.fit <- ifun(pred$Fitted.fit)
pred$response.low <- ifun(pred$se.low)
pred$response.high <- ifun(pred$se.high)



pred_groups <- pred %>%
    mutate(age_group = cut(STANDALTER, c(26, 36, 50, 66, 81, 126))) %>%
    # filter(STANDALTER < 100) %>%
    group_by(age_group, mod2015_T2M14HMEA, species_corrected) %>%
    summarise(mean_dbh = mean(Fitted.fit, na.rm = TRUE),
              mean_se = sqrt(sum(Fitted.se.fit))/n()) %>%
    mutate(response.fit = ifun(mean_dbh),
           response.low = ifun(mean_dbh - 1.96 * mean_se),
           response.high = ifun(mean_dbh + 1.96 * mean_se),) %>%
    ungroup()

pred_groups <- augment_prediction_range(prediction_df = as.data.frame(pred_groups),
                                  model_df = model_df[model_df$provenance == "s_wfs_baumbestand",],
                                  group_var = "species_corrected",
                                  range_var = "mod2015_T2M14HMEA",
                                  qtl = 1)




plt <- ggplot(pred, aes(y = response.fit, x = (T2M14HMEA), color = as.factor(STANDALTER)), fill =  as.factor(STANDALTER)) +

    geom_ribbon(aes(ymin = response.low, ymax = response.high,  color = as.factor(STANDALTER), fill =  as.factor(STANDALTER)), alpha = 0.4) +
    geom_line()+
    # facet_wrap(~ species_corrected, ncol = 2) +
    # scale_color_brewer(type = "qual", palette = "Set2") +
    theme(legend.position = 'right') +
    # facet_wrap(~species_corrected) +
    # geom_smooth(method = "lm")
    # geom_smooth() +
    theme_minimal(base_size = 16) +
    facet_wrap(~species_corrected, scales = "free_y") +
    # scale_color_brewer(palette = 1) +
    labs(linetype = "Age Class", color = "Age", fill = "Age", x = expression(UHI~Magnitude~(degree~C)), y = "Mean DBH (cm)")
plt




plt <- ggplot(data = data.frame(), aes(y = response.fit, x = (mod2015_T2M14HMEA), colour = as.factor(age_group), fill =  as.factor(age_group), group = as.factor(age_group))) +

    geom_ribbon(data = pred_groups %>%
                    filter(prediction_range =="within"),
                alpha = 0.2,  aes(ymin = response.low, ymax = response.high), color = "transparent") +
    # geom_ribbon( aes(ymin = se.low, ymax = se.high), alpha = 0.4) +
    # geom_line(aes(size = prediction_range))+
    geom_line(data = pred_groups %>%
                  filter(prediction_range =="within"))+
    geom_ribbon(data = pred_groups,
                alpha = 0.2,  aes(ymin = response.low, ymax = response.high), color = "transparent",
                linetype = 2) +
    # geom_ribbon( aes(ymin = se.low, ymax = se.high), alpha = 0.4) +
    # geom_line(aes(size = prediction_range))+
    geom_line(data = pred_groups,
              linetype = 2)+
    # facet_wrap(~ species_corrected, ncol = 2) +
    # scale_color_brewer(type = "qual", palette = "Set2") +
    theme(legend.position = 'right') +
    # facet_wrap(~species_corrected) +
    # geom_smooth(method = "lm")
    # geom_smooth() +
    theme_minimal(base_size = 16) +
    facet_wrap(~species_corrected, scales = "free_y") +
    scale_color_brewer(palette = 2, type = "qual") +
    scale_fill_brewer(palette = 2, type = "qual") +
    labs(color = "Age", fill = "Age", x = expression(UHI~Magnitude~(degree~C)), y = "Mean DBH (cm)")
plt



library(DHARMa)

simout  <-  simulateResiduals(mod11,  n=500, plot = TRUE)
# simout  <-  simulateResiduals(model_mad,  n=250, plot = TRUE)
# simout  <-  simulateResiduals(mod2,  n=250, plot = TRUE)
plot(simout)
testResiduals(simout)
plotResiduals(simout,quantreg = TRUE)

plotResiduals(simout, simple_spatial@frame$day_2007, quantreg = TRUE)





# plotting multimodel ---------------------------------------------------------------

drake::loadd(bam_dbh_fulldf)
drake::loadd(model_df_full)
drake::loadd(model_df_stat_filtered)
drake::loadd(bam_dbh_filtered)



# bam_dbh_fulldf[18:21, "model_file_path"]


bam_dbh_filtered[
    bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_reBEZIRK_var-day_2007', 'model_file_path']


testvar <- make_model_prediction_df(
    # path_model = bam_dbh_fulldf[8:14, "model_file_path"],
    path_model = bam_dbh_filtered[
        # bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_reBEZIRK_var-day_2007', 'model_file_path'],
        bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_building_height_reBEZIRK_var-day_2007', 'model_file_path'],
    model_df = model_df_stat_filtered %>%
        mutate(species_corrected = droplevels(species_corrected)),
    fixed_vars = list(X = 385785,
                      Y = 5816681,
                      STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
                      building_height_m = c(7.5, 25))
)


age_expr <- expression(dplyr::case_when(
    dplyr::between(STANDALTER, 30, 35) ~ "[30 - 35]",
    dplyr::between(STANDALTER, 45, 50) ~ "[45 - 50]",
    dplyr::between(STANDALTER, 60, 65) ~ "[60 - 65]",
    dplyr::between(STANDALTER, 75, 80) ~ "[75 - 80]",
    dplyr::between(STANDALTER, 90, 95) ~ "[90 - 95]",
    TRUE ~ NA_character_
))

# saveRDS(testvar, "testvar.Rds")
# testvar <- readRDS("testvar.Rds")


# age_breaks <- c(29, 35, 44,50,59,65,74,80,89,95)

# cut(1:100, age_breaks)



pred_groups <- purrr::map2_dfr(
    testvar,
    names(testvar),
    function(x,y){
        summarize_age_groups(
            x,
            model_df_stat_filtered %>%
                mutate(species_corrected = droplevels(species_corrected)),
            c(y, "building_height_m"),
            age_break_expr = age_expr)
    },
    .id = "tempvar"
)

#
# plt <- ggplot(data = data.frame(), aes(y = response.fit.mean, x = (urbclim_mod_morning_3_5), colour = as.factor(age_group), fill =  as.factor(age_group), group = as.factor(age_group))) +
#
#     geom_ribbon(data = pred_groups %>%
#                     filter(prediction_range =="within"),
#                 alpha = 0.2,  aes(ymin = response.low.mean, ymax = response.high.mean), color = "transparent") +
#     # geom_ribbon( aes(ymin = se.low, ymax = se.high), alpha = 0.4) +
#     # geom_line(aes(size = prediction_range))+
#     geom_line(data = pred_groups %>%
#                   filter(prediction_range =="within"))+
#     geom_ribbon(data = pred_groups,
#                 alpha = 0.2,  aes(ymin = response.low.mean, ymax = response.high.mean), color = "transparent",
#                 linetype = 2) +
#     # geom_ribbon( aes(ymin = se.low, ymax = se.high), alpha = 0.4) +
#     # geom_line(aes(size = prediction_range))+
#     geom_line(data = pred_groups,
#               linetype = 2)+
#     # facet_wrap(~ species_corrected, ncol = 2) +
#     # scale_color_brewer(type = "qual", palette = "Set2") +
#     theme(legend.position = 'right') +
#     # facet_wrap(~species_corrected) +
#     # geom_smooth(method = "lm")
#     # geom_smooth() +
#     theme_minimal(base_size = 16) +
#     facet_wrap(~species_corrected, scales = "free_y") +
#     scale_color_brewer(palette = 2, type = "qual") +
#     scale_fill_brewer(palette = 2, type = "qual") +
#     labs(color = "Age", fill = "Age", x = expression(UHI~Magnitude~(degree~C)), y = "Mean DBH (cm)")
# plt
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

plot_data <- pred_groups %>%
    select(-tempvar) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(names(testvar)),
                        names_to = "uhi_tempvar",
                        values_to = "temp_degc") %>%
    dplyr::filter(age_group != "[60 - 65]", prediction_range == "within") %>%
    dplyr::filter(age_group != "[75 - 80]", prediction_range == "within") %>%
    # dplyr::filter(age_group != "[90 - 95]", prediction_range == "within") %>%
    dplyr::filter() %>%
    dplyr::arrange(uhi_tempvar) %>%
    tidyr::drop_na(temp_degc) %>%
    group_by(species_corrected, uhi_tempvar) %>%
    mutate(temp_degc_scaled = scales::rescale(temp_degc, to = c(0,1)))

grand_means <- plot_data %>%
    dplyr::filter(grepl("day_2007", uhi_tempvar)) %>%
    dplyr::group_by(species_corrected, temp_degc_scaled) %>%
    dplyr::summarise(grand_mean = mean(response.fit.mean, na.rm = TRUE),
              n = dplyr::n())

model_df_stat_filtered <- model_df_stat_filtered %>%
    mutate(age_group = eval(age_expr))

plt <- ggplot(data = plot_data %>%
                  filter(prediction_range == "within",
                         grepl("day_2007", uhi_tempvar)) %>%
                  # filter(prediction_range == "within",
                  #        grepl("mod2015", uhi_tempvar)) %>%
                  ungroup(),
              aes(y = response.fit.mean,
                  x = temp_degc,
                  # x = temp_degc_scaled,
                  colour = as.factor(age_group),
                  fill =  as.factor(age_group),
                  linetype = as.factor(building_height_m),
                  # group = as.factor(age_group),
                  # colour = as.factor(uhi_tempvar),
                  # fill =  as.factor(uhi_tempvar),
                  # group = as.factor(uhi_tempvar),
                  ymin = response.low.mean,
                  ymax = response.high.mean)) +


    stat_density(inherit.aes = FALSE,
               data = model_df_stat_filtered %>%
                   dplyr::filter(age_group != "[60 - 65]") %>%
                   dplyr::filter(age_group != "[75 - 80]") ,
               aes(x = day_2007,
                   y = ..scaled..*100,
                   fill = as.factor(age_group),),
               position = "dodge",
               geom = "area",
               alpha = 0.4) +


    # geom_ribbon(color = "transparent", alpha = 0.2) +
    # geom_line(color = "black")  +

    # geom_ribbon(linetype = 1) +


    # geom_line(linetype = 1)  +
    geom_line()  +

    # geom_line(inherit.aes = FALSE,
    #              data = grand_means,
    #              aes(y = grand_mean,
    #                  # x = temp_degc,
    #                  # x = temp_degc_scaled
    #                  )) +

    # geom_smooth(aes(group = 1), fill = "transparent", color = "black") +
    geom_smooth(aes(group = 1), formula = y ~ x, method = "lm", fill = "transparent", color = "black") +

#
#     geom_ribbon(data = pred_groups %>%
#                     filter(prediction_range =="within"),
#                 alpha = 0.2,
#                 color = "transparent") +
#
#     geom_line(data = pred_groups %>%
#                   filter(prediction_range =="within"))+
    #
# geom_ribbon(data = pred_groups,
#             alpha = 0.2,  aes(ymin = response.low.mean, ymax = response.high.mean), color = "transparent",
#             linetype = 2) +
#
# geom_line(data = pred_groups,
#           linetype = 2) +
#

theme(legend.position = 'right') +
    theme_minimal(base_size = 16) +
    # facet_grid(uhi_tempvar ~species_corrected, scales = "free_y") +
    facet_grid(age_group~species_corrected, scales = "free_y") +
    scale_color_brewer(palette = 2, type = "qual") +
    scale_fill_brewer(palette = 2, type = "qual") +
    labs(color = "Model", fill = "Model", x = "Standardized UHI Magnitude", y = "Mean DBH (cm)")
plt


# multi model summarz -----------------------------------------------------

drake::loadd(model_df_stat_filtered)
mod_df <-model_df_stat_filtered

# Identify model groups ---------------------------------------------------
path_model_dir <- "analysis/data/models/stat/filtered/"

path_files <- list.files(path_model_dir, full.names = TRUE)

mod_groups <- list.files(path_model_dir) %>%
    gsub("_var-.*Rds$", "", .) %>%
    as.factor()

# get model summaries -------------------------------------------------------


future::plan(future::multisession(workers = 8))
mod_summary_list <-
    furrr::future_map(
        levels(mod_groups),
        function(mg){

            idx <- which(mod_groups == mg)

            mods <- lapply(path_files[idx],
                           function(x){
                               m <- readRDS(x)

                               s <- m %>%
                                  summary()

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
    setNames(levels(mod_groups))
future::plan(future::sequential())

# saveRDS(mod_summary_list, "TEMP_mod_summary_list.Rds")
# readRDS("TEMP_mod_summary_list.Rds")

# grab model paths --------------------------------------------------------


mod_group_list <-
    purrr::map(
        levels(mod_groups),
        function(mg){

            idx <- which(mod_groups == mg)

            mods <- lapply(path_files[idx],
                           function(x){
                               return(x)
                           }) %>%
                setNames(
                    fs::path_ext_remove(
                        list.files(path_model_dir)[idx]
                    )  %>%
                        gsub(pattern = ".*var-",
                             replacement = "",
                             x = .)
                )
        }) %>%
    setNames(levels(mod_groups))


# Predict for each model --------------------------------------------------
# mod_test_list <- mod_group_list[1]
# mod_test_list[[1]] <- mod_group_list[[1]][1:3]


mod_prediction_list <- purrr::map_depth(
    mod_test_list,
    .depth = 1,
    .f = function(mod){
        make_model_prediction_df(
            path_model = mod,
            model_df = mod_df,
            fixed_vars = list(X = 385785,
                              Y = 5816681,
                              STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95))
        )


    })



# Scale model predictions [0,1] -------------------------------------------


# Average across model groups ---------------------------------------------


age_expr <- expression(dplyr::case_when(
    dplyr::between(STANDALTER, 30, 35) ~ "[30 - 35]",
    dplyr::between(STANDALTER, 45, 50) ~ "[45 - 50]",
    dplyr::between(STANDALTER, 60, 65) ~ "[60 - 65]",
    dplyr::between(STANDALTER, 75, 80) ~ "[75 - 80]",
    dplyr::between(STANDALTER, 90, 95) ~ "[90 - 95]",
    TRUE ~ NA_character_
))




pred_groups <- purrr::map_depth(
    .x = mod_test_list,
    .depth = 1,
    ... = names(mod_test_list),
    .f = function(x,y){
        summarize_age_groups(
            x,
            mod_df,
            y,
            age_break_expr = age_expr)
    })





# Average within models ---------------------------------------------------


# Plot for each species, across age groups --------------------------------


# Plot for each species, one age group and all models ---------------------




# Supplemental ------------------------------------------------------------

## semivariograms ---------------------------------


vgram_fits <- purrr::map_depth(mod_summary_list[], .depth = 2, "variogramm") %>%
    purrr::map_depth(.depth = 1, purrr::compact) %>%
    purrr::compact() %>%
# vgram_fits <- purrr::map(mod_summary_list[[1]], "variogramm") %>%
    purrr::map_depth(.depth = 2, ~dplyr::mutate(.x, gamma = gamma / max(gamma))) %>%
    purrr::map_depth(.depth = 2, ~gstat::fit.variogram(.x, gstat::vgm(psill = 1, "Exp", range = 1000, nugget = 0.2))) %>%
    purrr::map(~purrr::map_dfr(.x, function(x){gstat::variogramLine(x, maxdist = 5000, n = 100)},
                   .id = "tempvar")) %>%
    bind_rows(.id = "mod_group")

# %>%
    # dplyr::group_by(tempvar) %>%
    # dplyr::mutate(gamma_adj = gamma / max(gamma))


var_spat <- gstat::variogram(dbh_cm ~ 1, locations = ~ X + Y, data =  model_df_stat_filtered, width = 50, cutoff = 5000)
var_spat_new <- var_spat
var_spat_new$gamma <- var_spat_new$gamma / max(var_spat_new$gamma)


vgram_raw <- purrr::map_depth(mod_summary_list[], .depth = 2, "variogramm") %>%
    purrr::map_depth(.depth = 1, purrr::compact) %>%
    purrr::compact() %>%
    # vgram_fits <- purrr::map(mod_summary_list[[1]], "variogramm") %>%
    purrr::map_depth(.depth = 2, ~dplyr::mutate(.x, gamma = gamma / max(gamma))) %>%
    purrr::map(~bind_rows(.x, .id = "tempvar")) %>%
    bind_rows(.id = "mod_group")


vgram_raw %>%
    # dplyr::mutate(gamma_adj = gamma / max(gamma)) %>%
    ggplot(aes(x = dist, y = gamma, color = tempvar)) +
    geom_point() +
    geom_line(data = vgram_fits,
              aes(x = dist, y = gamma, color = tempvar)) +
    lims(y = c(0, 1.1)) +
    facet_wrap(~mod_group)+
    geom_point(data = var_spat_new, aes(x = dist, y = gamma, color = "raw"))




# extract smooth p-vals ---------------------------------------------------

extract_smooth_pval <- function(mlist){

    # mod_vars <- purrr::map_depth(mlist, 1, names) %>%
    #     unlist() %>%
    #     as.character()


    pvals <- purrr::map_depth(mlist, 2, "summary") %>%
        purrr::map_depth(2, "s.table") %>%
        # purrr::map2(mod_vars, function(mod, vn){
        purrr::map(function(mod){

            purrr::map2(mod, names(mod), function(m, vn){
                # idx <- grepl(vn, rownames(m))


                m <- cbind(smooth = rownames(m),
                           as.data.frame(m))
                # m$smooth <-
                m$model <- vn
                m$species_corrected <- gsub(pattern = "(.*species_corrected)(.*$)",
                                            replacement = "\\2",
                                            m$smooth,
                                            perl = TRUE)
                m$smooth <- gsub("(.*)([:].*)", "\\1", m$smooth, perl = TRUE)
                m <- m[!grepl("BEZIRK", m$smooth), ]

                # if(!any(idx)){
                # m$model <- vn
                # idx <- 0
                # }
                # print(idx)



                # return(m[idx, ])
                return(m)

            }
            )


        })

    return(pvals)
}


smooth_vals <- extract_smooth_pval(mod_summary_list) %>%
    purrr::map(purrr::compact) %>%
    purrr::map(~dplyr::bind_rows(.x, .id = "model_var")) %>%
    bind_rows(.id = "model_group") %>%
    mutate(model_id = paste0(model_group, "_", model),
           expvar = case_when(
               species_corrected %in% c('s(X,Y)',
                                        's(log10(baumsch_flaeche_m2))',
                                        's(log10(soil_nutrients_swert))',
                                        's(building_height_m)))',
                                        's(building_height_m)') ~ "var",
               TRUE ~ species_corrected),
           expvar = forcats::fct_relevel(as.factor(expvar),
                                         "var",
                                         ),
           # expvar = forcats::fct_relevel(as.factor(species_corrected),
           #                               's(X,Y)',
           #                               's(log10(baumsch_flaeche_m2))',
           #                               's(log10(soil_nutrients_swert))',
           #                               's(building_height_m)',
           #                               ),
           smooth = forcats::fct_relevel(as.factor(smooth),
                                         's(X,Y)',
                                         's(log10(baumsch_flaeche_m2))',
                                         's(log10(soil_nutrients_swert))',
                                         's(building_height_m)',
           ))





mod_plot <- ggplot(smooth_vals, aes(x = expvar, y = smooth, fill = `p-value`)) +
    geom_tile(color = "white") +
    geom_text(color = "white", aes(label =round(`p-value`,2))) +
    facet_wrap(~model_id, scales = "free_y", ncol = 1) +
    scale_fill_viridis_c(option = "inferno") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))


ggsave(plot = mod_plot, filename = "./analysis/figures/eda/model_sign_smooth.png",
       width = 45,
       height = 200,
       units = "cm",
       dpi = 300,
       limitsize = FALSE)



# extract mod R2 ----------------------------------------------------------

mod_summary_list <- mod_summaries_filtered
## NEXT
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




mod_dev <- mod_summary_list %>%
    purrr::map_depth(2, "summary") %>%
    purrr::map_depth(2, "dev.expl") %>%
    purrr::map_depth(2, .f = as.data.frame) %>%
    purrr::map(dplyr::bind_rows, .id = "expvar")  %>%
    purrr::map2(names(.),
                ~mutate(.x, mod_group = .y)) %>%
    do.call(rbind, .) %>%
    `rownames<-`(NULL) %>%
    rename(deviance_explained = 2) %>%
    left_join(mod_n, by = c("mod_group", 'expvar'))

mod_means <- mod_dev %>%
    group_by(mod_group) %>%
    summarise(ggplot2:::mean_se(deviance_explained)) %>%
    arrange(desc(y))

mod_dev$mod_group <- forcats::fct_relevel(mod_dev$mod_group, mod_means$mod_group)
mod_means$mod_group <- forcats::fct_relevel(mod_means$mod_group, mod_means$mod_group)

ggplot(mod_dev,
       aes(y = mod_group,
           x = deviance_explained)) +
    geom_line(data = mod_means, aes(x = y),  color = "black", group = 1, alpha = 0.3) +
    geom_jitter( shape = 21,height = .1, aes(size = n_sample,
                                             fill = expvar), alpha = 0.8) +
    # geom_jitter( shape = 21, color = "white", fill = "gray60",height = .1, aes(size = n_sample), alpha = 0.5) +
    geom_linerange(data = mod_means, aes(x = y, xmin = ymin, xmax = ymax), color = "black") +
    geom_point(data = mod_means, aes(x = y), size = 3, shape = 21, color = "white", fill = "black") +
    guides(fill = guide_legend(override.aes = list(size = 5))) +
    theme_minimal() +
    scale_fill_brewer(type = "qual", palette = "Set3")






ggplot(mod_dev %>%
           mutate(is_spatial = grepl("spatial", mod_group),
                  mod_group = forcats::fct_relevel(mod_group, stringr::str_sort(levels(mod_group))),
                  expvar = forcats::fct_relevel(
                      expvar,
                      "nullmodel",
                      "baumsch_flaeche",
                      "building_height",
                      "soil_nutrients",
                      "urbclim_mod_morning_3_5",
                      "urbclim_mod_afternoon_13_15",
                      "urbclim_mod_night_21_23")),
       aes(x = expvar,
           y = deviance_explained * 100)) +
    # geom_line(data = mod_means, aes(x = y),  color = "black", group = 1, alpha = 0.3) +
    # annotate("text", x = c(2.5, 5.5, 8, 10), y = rep(0.82, 4), label = c("no temp", "Urbclim", "Landsat", "Berlin UA")) +
    # annotate("text", x = c(1:4), y = rep(0.82, 4), label = c("bla", "blu", "bli", "kli")) +
    annotate("text", x = c(2.5), y = rep(80, 1), label = "bold(no~temp)", parse = TRUE) +
    annotate("text", x = c(6),   y = rep(80, 1), label = "bold(Urbclim)", parse = TRUE) +
    annotate("text", x = c(8),   y = rep(80, 1), label = "bold(Landsat)", parse = TRUE) +
    annotate("text", x = c(10),  y = rep(80, 1), label = "bold(Berlin~EnvAt)", parse = TRUE) +
    annotate(geom = "rect", xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.3) +
    annotate(geom = "rect", xmin = 4.5, xmax = 7.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "steelblue1") +
    annotate(geom = "rect", xmin = 7.5, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "darkorange") +
    annotate(geom = "rect", xmin = 8.5, xmax = 11.5, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "seagreen4") +
    geom_boxplot() +
    geom_jitter( shape = 21,width = .25, aes(size = n_sample,
                                             fill = mod_group), alpha = 0.8) +
    # geom_linerange(data = mod_means, aes(x = y, xmin = ymin, xmax = ymax), color = "black") +
    # geom_point(data = mod_means, aes(y = y), size = 3, shape = 21, color = "white", fill = "black") +
    guides(fill = guide_legend(override.aes = list(size = 5),
                               ncol = 2)) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.direction = "vertical",
          strip.text = element_text(size = 12)) +
    # scale_fill_brewer(type = "qual", palette = "Set3") +
    scale_fill_manual(values = pals::kelly(n = n_distinct(mod_dev$mod_group))) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2) ) +
    facet_wrap(~is_spatial, ncol = 1, labeller = labeller(is_spatial = c('TRUE' = "spatial - f(x,y)", 'FALSE' = 'non-spatial')))  +
    labs(x = NULL, y = "Explained Deviance (%)")





# extract Moran's I -------------------------------------------------------
gridp <- sf::st_read("analysis/data/raw_data/spatial_ancillary/grid_2x2.geojson")

has_spatial <- grepl("spatial", names(mod_group_list))

short_mod <- mod_group_list[has_spatial][1]
short_mod[[1]] <- short_mod[[1]][1:2]

# cycle through model groups
# future::plan(future::multiprocess(workers = 3))

#' Compare Variable and residuals
#'
#' @param mod_list list, mod_group_list
#' @param var_response character, column name of response variable, e.g. dbh_cm, must be used in model from `mod_list`
#' @param var_resid character, column name of residuals, typically `.resid` from `broom::augment`
#' @param gridp sf df, 2x2 grid squares to subset data - currently split by column `grid_region`
#'
#' @return list with nesting from mod_list, with ouput from `ape::Moran.I()`
assess_morans_spatialmod <- function(mod_list,
                                     gridp,
                                     var_response,
                                     var_resid){


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

                                                                                         moran_non_spat <- check_moran(df = temp_mod,
                                                                                                                       grid = sf::st_union(gridp[gridp$grid_region==unlist(gr), ]),
                                                                                                                       min_obs = 1000,
                                                                                                                       var = var_response)
                                                                                         moran_spat <- check_moran(df = temp_mod,
                                                                                                                   grid = sf::st_union(gridp[gridp$grid_region==unlist(gr), ]),
                                                                                                                   min_obs = 1000,
                                                                                                                   var = var_resid)

                                                                                         return(list(non_spat = moran_non_spat,
                                                                                                     spat = moran_spat,
                                                                                                     var = varname,
                                                                                                     grid_area = gr))
                                                                                     })
                                                            rm(temp_mod)

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


# purrr::keep(.p = function(x) grepl("spatial", x = names(.)))



moran_comparison = assess_morans_spatialmod(
    mod_list = mod_group_list_filtered[grepl(
        pattern = ("mI_spatial_age_ADD_temp_by_species_reBEZIRK$"),
        x = names(mod_group_list_filtered))],
    gridp = gridp,
    var_response = "dbh_cm",
    var_resid = ".resid"
)






# plot morans -------------------------------------------------------------



library(ggplot2)

plot_moran_comparison <- function(moran_summary,
                                  prediction_range = "full",
                                  file,
                                  height,
                                  width,
                                  dpi){


    moran_summary %>%
        dplyr::filter(vars %in% c('observed', 'expected')) %>%
        ggplot(aes(x = grid_area , y = value, fill = spatial_model)) +
        geom_bar(position = "dodge", stat = "identity") +
        facet_wrap(~model) +
        scale_fill_brewer(type = "qual", palette = 2) +
        theme_minimal(base_size = base_size)



    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)

}


# plot resid vs. pred -----------------------------------------------------



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

    mod_broomed <- broom::augment(mod)

    ifun <- Gamma(link = "log")$linkinv

    gplot <- mod_broomed %>%
        ggplot(aes(x = mod$family$linkinv(.fitted), y = dbh_cm)) +
        geom_hex(bins = 75, color = "transparent") +
        geom_smooth(method = "lm", color = "red") +
        scale_fill_viridis_c() +
        theme_minimal(base_size = 18) +
        labs(x = "Predicted", y = "Observed", fill = "N")


    ggplot2::ggsave(filename = file,
                    plot = gplot,
                    dpi = dpi,
                    height = height,
                    width = width)


}
