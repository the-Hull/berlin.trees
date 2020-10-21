library(future)
library(dplyr)
library(ggplot2)
library(sf)

drake::loadd(full_data_set_clean)
drake::loadd(baumscheiben_in_lists)







# check baumscheiben ------------------------------------------------------
glimpse(baumscheiben_in_lists)


# rough plot
baumscheiben_in_lists$s_Baumscheibe[1:5000, ] %>%
    ggplot() +
    geom_sf(color = "gray20")




# test on subset

baumsch <- baumscheiben_in_lists$s_Baumscheibe[1:15000, ] %>%
    sf::st_set_crs(st_crs(full_data_set_clean))


baumsch_buff <- sf::st_buffer(x = sf::st_centroid(baumsch),
                              dist = 15)%>%
    sf::st_set_crs(st_crs(full_data_set_clean))


baumsch_centroid <- sf::st_centroid(baumsch)
trees_in_buffs <- sf::st_intersects(baumsch_buff, full_data_set_clean)


plan(multiprocess(workers = availableCores() - 1))


options(future.globals.maxSize = 600 * 1024 ^ 2)

tree_idx <- furrr::future_map(seq_along(trees_in_buffs),
                              function(x){

                                  idx <- which.min(st_distance(full_data_set_clean[trees_in_buffs[[x]], ],
                                                               baumsch_centroid[x, ]))
                                  out <- trees_in_buffs[[x]][idx]
                                  out <- ifelse(length(out) == 0,
                                                NA,
                                                out)
                              })


baumsch$tree_idx <- tree_idx



baumsch <- baumscheiben_in_lists$s_Baumscheibe[1:1000, ] %>%
    sf::st_set_crs(st_crs(full_data_set_clean))


baumsch_buff <- sf::st_buffer(x = sf::st_centroid(baumsch),
                              dist = 15)%>%
    sf::st_set_crs(st_crs(full_data_set_clean))


baumsch_centroid <- sf::st_centroid(baumsch)
trees_in_buffs <- sf::st_intersects(baumsch_buff, full_data_set_clean)



tree_idx <- sapply(seq_along(trees_in_buffs),
                   function(x){

                       out <- st_distance(full_data_set_clean[trees_in_buffs[[x]], ], baumsch_centroid[x, ])


                   }, simplify = TRUE)


baumsch$tree_idx <- tree_idx





test <- st_distance(full_data_set_clean[trees_in_buffs[1], ], baumsch_centroid)



# full  -------------------------------------------------------------------

library(future)
library(dplyr)
library(ggplot2)
library(sf)

drake::loadd(full_data_set_clean)
drake::loadd(baumscheiben_in_lists)




#' Add tree info to Baumscheiben data set
#'
#' Grabs the closest tree (within max_dist_m) to a Baumscheibe, and adds the
#' corresponding index from the tree data set as a column to baumscheiben
#'
#' @param baumscheiben_in_lists list, contains an sf_df with baumscheiben polygons
#' @param full_data_set_clean sf_df with all berlin trees
#' @param max_dist_m numeric, max distance to buffer around a baumscheiben centroid to look for trees
#'
#' @return baumscheiben_in_lists, with modified element (added column of tree indices)
#' @export
#'
#' @import dplyr
#'  sf
#'  furrr
#'
#' @examples
add_full_df_idx_to_baumscheiben <- function(baumscheiben_in_lists,
                                            full_data_set_clean,
                                            max_dist_m = 15){

# make unique id
baumscheiben_in_lists$s_Baumscheibe$split_id <- seq_len(nrow(baumscheiben_in_lists$s_Baumscheibe))
# make unique labels
baumscheiben_in_lists$s_Baumscheibe$split_label <- base::cut(x = baumscheiben_in_lists$s_Baumscheibe$split_id,
                                                             breaks = seq(0,
                                                                          max(baumscheiben_in_lists$s_Baumscheibe$split_id),
                                                                          by = 2000),
                                                             labels = FALSE)







closest_tree_idx <- furrr::future_map(baumscheiben_in_lists$s_Baumscheibe %>%
                                          split(., f = .$split_label),
                              function(baumsplit){

                                  # make temp baumscheiben object
                                  baumsch <-  sf::st_set_crs(baumsplit,
                                                             st_crs(full_data_set_clean))

                                  # generate buffer and adjust crs
                                  baumsch_buff <- sf::st_buffer(x = sf::st_centroid(baumsch),
                                                                dist = max_dist_m)
                                  # assess which trees fall into specific buffer
                                  # this outputs a list of indices in the y object (full_data_set_clean)
                                  trees_in_buffs <- sf::st_intersects(baumsch_buff, full_data_set_clean)



                                  # calc centroid for later use
                                  baumsch_centroid <- sf::st_centroid(baumsch)



                                  tree_idx <- sapply(seq_along(trees_in_buffs),
                                                     function(x){

                                                         # grab the closest tree idx to the centroid
                                                         idx <- which.min(st_distance(full_data_set_clean[trees_in_buffs[[x]], ],
                                                                            baumsch_centroid[x, ]))

                                                         # subset the potential trees to the closests one
                                                         out <- trees_in_buffs[[x]][idx]

                                                         # clean up
                                                         out <- ifelse(length(out) == 0,
                                                                       NA,
                                                                       out)


                                                     }, simplify = TRUE)

                                  return(tree_idx)

                              }) %>%
    dplyr::bind_rows()

baumscheiben_in_lists$s_Baumscheibe$closest_tree_idx <- closest_tree_idx



}

## Validate


baumscheiben_in_lists$s_Baumscheibe[1:4, ] %>%
    ggplot() +
    geom_sf(color = "gray20") +
    geom_sf(data = full_data_set_clean[closest_tree_idx[[1]][1:4], ],
            color = "red")



plot(baumscheiben_in_lists$s_Baumscheibe[1:4, ], max.plot = 1, col = "black")
plot(full_data_set_clean[closest_tree_idx[[1]][1:4], ], col = "red", add = TRUE, cex = 10)




## testing stuff
future::plan(multisession(workers = availableCores() - 1))

short <- list()
short$s_Baumscheibe <- baumscheiben_in_lists$s_Baumscheibe[1:45000, ]
test <- add_full_df_idx_to_baumscheiben(bms = short,
                                   fulldf = full_data_set_clean)




test$s_Baumscheibe[2000:2010, ] %>%
    ggplot() +
    geom_sf(color = "gray20") +
    geom_sf(data = full_data_set_clean[test$s_Baumscheibe[2000:2010, "closest_tree_idx", drop = TRUE], ],
            color = "red",
            alpha = 0.1)

idcs <- 35000:35050

test$s_Baumscheibe[idcs, ] %>%
    ggplot() +
    geom_sf(color = "gray20") +
    geom_sf(data = full_data_set_clean[test$s_Baumscheibe[idcs, "closest_tree_idx", drop = TRUE], ],
            color = "red",
            alpha = 0.1)




test_join <- sf::st_join(full_data_set_clean, baumscheiben_in_lists$s_Baumscheibe, st_join = st_nearest_feature )



test_nearest <- sf::st_nearest_feature(full_data_set_clean, baumscheiben_in_lists$s_Baumscheibe)

test_dist <- sf::st_distance(full_data_set_clean, baumscheiben_in_lists$s_Baumscheibe[test_nearest, ], by_element = TRUE)

test_join_man <- cbind(full_data_set_clean, sf::st_drop_geometry(baumscheiben_in_lists$s_Baumscheibe[test_nearest, ]))
test_join_man$dist_flaeche <- test_dist

test_join_man$flaeche_adj <- ifelse(test_join_man$dist_flaeche <= units::set_units(10, m), test_join_man$dist_flaeche, NA)



#' Add Baumscheiben Area to Tree Data set
#'
#' @param fulldf sf_df, all berlin trees
#' @param bms sf_df, Baumscheiben polygons and their area
#' @param max_dist numeric, how far from polygon centroid can tree lie and still be assigned?
#'
#' @return fulldf with additional columns: "baumsch_dist_m",
#'  "baumsch_elem_nr",
#'  "baumsch_gis_id",
#'   "baumsch_flaeche_m2"
#' @export
#'
#' @import sf
#' units
#'
add_baumscheiben_flaeche <- function(fulldf, bms, max_dist = 10){

    nearest_idx <- sf::st_nearest_feature(fulldf, bms)

    # calculate pairwise distances
    fulldf$baumsch_dist_m <- sf::st_distance(fulldf, bms[nearest_idx, ], by_element = TRUE)

    bms <- sf::st_drop_geometry(bms[nearest_idx, c("elem_nr", "gis_id", "flaeche"), ])
    names(bms) <- c("baumsch_elem_nr", "baumsch_gis_id", "baumsch_flaeche_m2")

    # join manually
    fulldf <- cbind(fulldf,
                    bms)

    fulldf$baumsch_flaeche_m2 <- ifelse(fulldf$baumsch_dist <= units::set_units(max_dist, m),
                                        fulldf$baumsch_flaeche_m2,
                                        NA)

    return(full_df)

}
