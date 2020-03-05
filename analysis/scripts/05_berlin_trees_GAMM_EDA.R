
# EDA for GAMM application ------------------------------------------------


library(dplyr)
library(drake)
library(data.table)
library(mgcv)

loadd(model_df)
# loadd(full_data_set_clean)

proj <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

tilia <- data.table(model_df)[gattung_short == "Tilia", .(id,
                                              provenance,
                                              gattung_short,
                                              ART_BOT,
                                              BAUMHOEHE,
                                              STANDALTER,
                                              dbh_cm,
                                              geometry,
                                              day_2017)
                              ][ , ":="(x_coord = sf::st_coordinates(geometry)[ ,1],
                                    y_coord = sf::st_coordinates(geometry)[ ,2],
                                    age = as.numeric(STANDALTER))]


top_spec <- tilia[order(ART_BOT),.N , by = ART_BOT
                  ][order(-N)][c(1,2,3,5,6), ART_BOT]

tilia <- tilia[ART_BOT %in% top_spec & age < 200 & dbh_cm < 200
               ][, `:=`(ART_BOT = as.factor(ART_BOT),
                        provenance = as.factor(provenance))]



tilia_gam <- mgcv::bam(dbh_cm ~ s(x_coord, y_coord, k = 12) +
                           # s(day_2017, as.factor(ART_BOT), k = c(30, 6),  bs = "fs") +
                           s(age, k  = 60) +
                           provenance,
                       data = tilia,
                       method = "REML")


tilia_gam <- mgcv::bam(dbh_cm ~
                           # provenance +
                           ART_BOT +
                           s(day_2017, k = 120, by = ART_BOT) +
                           s(age, k  = 120, by = ART_BOT),
                       data = tilia[provenance == "s_wfs_baumbestand"],
                       method = "REML",
                       nthreads = 4)

mgcv::gam.check(tilia_gam)
summary(tilia_gam)


