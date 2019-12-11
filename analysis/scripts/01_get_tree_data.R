library(httr)
library(xml2)
library(dplyr)
library(ckanr)
library(ggplot2)



# set base url
ckanr_setup(url = "https://datenregister.berlin.de")

# pull all packages from api, limit to 2500 (currentl ~ 2100 pckgs)
p_list <- package_list(as = "table", url = "https://datenregister.berlin.de/", limit = 2500)


# check for tree-related data sets
-

# extract target urls for first packages (street trees)
anlagen_id <- package_results[[1]]$resources[[2]]$id
anlagen_url <- resource_show(id = anlagen_id)$url

# request data via GET (REST API)


json_path <- file.path(here::here(),
                       "analysis",
                       "data",
                       "raw_data",
                       paste0(package_results[[1]]$title,
                              ".json"))


# n <- 10
query <- list(service = "WFS",
              request = "GetFeature",
              version = "2.0.0",
              TypeNames = "s_wfs_baumbestand_an",
              # count = n,
              outputFormat = 'application/geo+json')


# extract content

httr::GET(anlagen_url, query = query, httr::write_disk(json_path, overwrite = TRUE))
trees_sf <- sf::read_sf(json_path)

## Alternative approach via loading data as geojson string directly into memory
# result <- httr::GET(anlagen_url, query = query, httr::write_disk(json_path, overwrite = TRUE))
# tree_data <- httr::content(result, type = 'text')
# trees_sf <- sf::read_sf(tree_data)







# get berlin district shapes
berlin_poly <-  sf::read_sf("https://raw.githubusercontent.com/m-hoerz/berlin-shapes/master/berliner-bezirke.geojson")


many_trees.plot <- trees_sf %>%

    mutate(STAMMUMFG = as.numeric(STAMMUMFG),
           GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)) ,
           gattung_short = forcats::fct_lump(GATTUNG,10) %>% forcats::fct_rev(),
           bezirk_num = as.numeric(as.factor(BEZIRK))) %>%
    filter(STAMMUMFG > 62.83185) %>%
    ggplot() +
    ggplot2::geom_sf(inherit.aes = FALSE,
                     data = berlin_poly,
                     fill = "transparent",
                     show.legend = FALSE) +
    ggplot2::geom_sf(aes(color = gattung_short),
                     shape = 3,
                     show.legend = FALSE,
                     alpha = 0.1) +
    facet_wrap(~gattung_short) +
    theme_minimal()




ggsave(filename = "./analysis/figures/01_tree_overiview.pdf",
       plot = many_trees.plot,
       height = 20,
       width = 20,
       units = "cm",
       dpi = 300)




extrafont::loadfonts("win", quiet = TRUE)


dens.plot <- trees_sf %>%

    mutate(STAMMUMFG = as.numeric(STAMMUMFG),
           GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)) ,
           gattung_short = forcats::fct_lump(GATTUNG,11),
           bezirk_num = as.numeric(as.factor(BEZIRK))) %>%
    filter(STAMMUMFG > 62.83185) %>% {
        ggplot(.) +



            # stat_density2d(aes(x = sf::st_coordinates(.)[,"X"],
            #                    y = sf::st_coordinates(.)[,"Y"],
            #                    fill = stat(nlevel)),
            #                alpha = 0.3,
            #                # alpha = stat(nlevel)),
            #                size = 0.01,
            #                bins = 15,
            #                geom = 'polygon') +
        stat_bin2d(aes(x = sf::st_coordinates(.)[,"X"],
                       y = sf::st_coordinates(.)[,"Y"],
                       group = gattung_short,
                       fill = stat(ncount)),
                   alpha = 0.8,
                   # alpha = stat(nlevel)),
                   # size = 0.01,
                   bins = 25,
                   binwidth = c(1500,1500)) +

            ggplot2::geom_sf(inherit.aes = FALSE,
                             data = berlin_poly,
                             fill = "transparent",
                             show.legend = FALSE,
                             color = "gray20",
                             size = 0.5) +





            facet_wrap(~gattung_short) +

            labs(fill = "normalized Count") +



            scale_x_continuous(breaks = seq(13, 14, 0.2)) +
            scale_y_continuous(breaks = seq(52.3, 53.7, 0.1)) +
            scale_fill_viridis_c() +
            guides(fill = guide_colorbar(barwidth = grid::unit(10, units = "cm"),
                                         barheight = grid::unit(.45, units = "cm"))) +


            theme_minimal(base_family = "Roboto Condensed",
                          base_size = 14) +

            theme(axis.title = element_blank(),
                  strip.text = element_text(face = "italic"),
                  legend.position = "bottom",
                  legend.dir = "horizontal",
                  legend.title = element_text(vjust = 1))

    }



dens.plot



tree_sums.plot <- trees_sf %>%
    # filter(STAMMUMFG > 400) %>%
    mutate(STAMMUMFG = as.numeric(STAMMUMFG),
           GATTUNG = forcats::fct_infreq(as.factor(GATTUNG)),
           gattung_short = forcats::fct_lump(GATTUNG,11),
           bezirk_num = as.numeric(as.factor(BEZIRK))) %>%
    # filter(bezirk_num < 5) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(aes(x = gattung_short, fill = gattung_short), show.legend = FALSE) +
    # ggplot2::geom_bar(aes(x = GATTUNG, fill = GATTUNG), show.legend = FALSE) +
    theme_minimal() +
    coord_flip()


tree_sums.plot



