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

mgcv::gam.check(mod9)
mgcv::gam.check(mod10)
mgcv::gam.check(mod11)
mgcv::gam.check(mod12)
mgcv::gam.check(mod13)

pdata <- with(model_df[model_df$provenance == "s_wfs_baumbestand",] %>%
# pdata <- with(model_df[model_df$diag_mad_select & model_df$provenance == "s_wfs_baumbestand",, ] %>%
                  mutate(species_corrected = as.factor(species_corrected)),
              expand.grid(T2M14HMEA = seq(min(T2M14HMEA, na.rm = TRUE),
                                          max(T2M14HMEA, na.rm = TRUE), length.out = 200),
                          X = 385785,
                          Y = 5816681,
                          # STANDALTER = c(30, 50, 80),
                          STANDALTER = c(30:35, 45:50, 60:65, 75:80),
                          species_corrected = as.factor(unique(species_corrected)),
                          BEZIRK = as.factor(unique(BEZIRK))
              ))
# fit <- predict(mod9 , pdata, type = "response", se.fit = TRUE)
fit <- predict(mod9 , pdata, se.fit = TRUE)
fit10 <- predict(mod10 , pdata, se.fit = TRUE)
fit11 <- predict(mod10 , pdata, se.fit = TRUE)
fit12 <- predict(mod12 , pdata, se.fit = TRUE, exclude = "s(BEZIRK)")
# ind <- mgcv::exclude.too.far(pdata$day_2007, pdata$STANDALTER,
#                              mdf_tilia[mad_select, ]$day_2007, mdf_tilia[mad_select, ]$STANDALTER, dist = 0.1)
# fit[ind] <- NA
pred <- cbind(pdata, Fitted = fit12)
pred$se.low <- pred$Fitted.fit - 1.96 * pred$Fitted.se.fit
pred$se.high <- pred$Fitted.fit + 1.96 * pred$Fitted.se.fit


ifun <- family(mod9)$linkinv
ifun <- family(mod10)$linkinv
ifun <- family(mod11)$linkinv

pred$response.fit <- ifun(pred$Fitted.fit)
pred$response.low <- ifun(pred$se.low)
pred$response.high <- ifun(pred$se.high)



pred_groups <- pred %>%
    mutate(age_group = cut(STANDALTER, c(26, 36, 50, 66, 80))) %>%
    filter(STANDALTER < 75) %>%
    group_by(age_group, T2M14HMEA, species_corrected) %>%
    summarise(mean_dbh = mean(Fitted.fit, na.rm = TRUE),
              mean_se = sqrt(sum(Fitted.se.fit))/n()) %>%
    mutate(response.fit = ifun(mean_dbh),
           response.low = ifun(mean_dbh - 1.96 * mean_se),
           response.high = ifun(mean_dbh + 1.96 * mean_se))





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




plt <- ggplot(pred_groups, aes(y = response.fit, x = (T2M14HMEA), colour = as.factor(age_group), fill =  as.factor(age_group), group = as.factor(age_group))) +

    geom_ribbon(alpha = 0.2,  aes(ymin = response.low, ymax = response.high), color = "transparent") +
    # geom_ribbon( aes(ymin = se.low, ymax = se.high), alpha = 0.4) +
    geom_line()+
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
