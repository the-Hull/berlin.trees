library(ggplot2)
library(drake)
library(data.table)
loadd(full_data_set_clean)

full_data_set_clean <- full_data_set_clean %>%
    mutate(STANDALTER = as.numeric(as.character(STANDALTER)))

min_dbh <- 1
max_dbh <- 400
max_age <- 350
min_age <- 15

filter_expression <- expression(dbh_cm > min_dbh &
                                    dbh_cm < max_dbh &
                                    STANDALTER < max_age &
                                    STANDALTER > min_age)

top_spec <- full_data_set_clean %>%
    as.data.frame() %>%
    select(ART_BOT, gattung_short, dbh_cm, STANDALTER) %>%
    filter(eval(filter_expression)) %>%
    dplyr::count(ART_BOT) %>%

    arrange(-n) %>%
    filter(n > 1000) %>%
    mutate(gattung_short = stringr::str_extract(ART_BOT, "^([\\w]+)"))


bezirke <- unique(full_data_set_clean$BEZIRK)


purrr::walk(bezirke,
            function(bz){

                g_plot <- full_data_set_clean %>%
                    filter(eval(filter_expression),
                           ART_BOT %in% top_spec$ART_BOT,
                           BEZIRK == bz) %>%
                    ggplot(aes(x = STANDALTER,
                               y = dbh_cm,
                               color = NAMENR)) +
                    geom_point(alpha = 0.3) +
                    geom_smooth(method = "lm") +

                    # ggforce::facet_grid_paginate(~ART_BOT, ncol = 5, nrow = 5)
                    facet_wrap(~ART_BOT, nrow = 10, ncol = 6, scales = "free")

                # np <- ggforce::n_pages(g_plot)

                ggsave(paste0("./analysis/figures/eda/00_eda_dbh_age_", bz, ".png"),
                       g_plot,
                       width = 100,
                       height = 100,
                       units = "cm",
                       dpi = 300)



            })

g_plot <- full_data_set_clean %>%
    filter(eval(filter_expression),
           ART_BOT %in% top_spec$ART_BOT) %>%
    ggplot(aes(x = STANDALTER,
               y = dbh_cm,
               color = BEZIRK)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +

    # ggforce::facet_grid_paginate(~ART_BOT, ncol = 5, nrow = 5)
    facet_wrap(~ART_BOT, nrow = 10, ncol = 6, scales = "free")

# np <- ggforce::n_pages(g_plot)

ggsave("./analysis/figures/00_eda_dbh_age.png",
       g_plot,
       width = 100,
       height = 100,
       units = "cm",
       dpi = 300)

# pdf("./analysis/figures/00_eda_dbh_age.pdf", width = 50, height = 50)
# for(page in seq_len(np)){
#     print({full_data_set_clean %>%
#         filter(eval(filter_expression),
#                ART_BOT %in% top_spec$ART_BOT) %>%
#         ggplot(aes(x = STANDALTER,
#                    y = dbh_cm,
#                    color = BEZIRK)) +
#         geom_point(alpha = 0.2) +
#         facet_wrap(~ART_BOT) +
#         geom_smooth(method = "lm") +
#             scale_color_brewer(palette = 2) +
#
#         ggforce::facet_grid_paginate(~ART_BOT, ncol = 5, nrow = 5, page = page)})
#

# print(g_plot)



# dev.off()
