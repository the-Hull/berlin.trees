library(drake)
library(future.callr)
library(dplyr)
future::plan(future.callr::callr)

plan <- drake_plan(

    # getting data



    ## Spatial Ancillary

    ### Berlin districts and BBoxx
    berlin_polygons = target(berlin.trees::get_berlin_polygons_as_sf()),

    bounding_box = berlin.trees::make_bbox(52.083962, 52.847599,
                                           12.712024, 14.238359,
                                           "greater_berlin",
                                           crs = 4326),

    ### Berlin UHI gridded data



    uhi_stacks = berlin.trees::get_uhi_rasters(file.path(here::here(), "analysis", "data", "raw_data", "UHI_explorer")),




    ## Berlin trees
    download_data = target(berlin.trees::download_berlin_trees()),

    ### Cleaning
    tree_data_in_lists = berlin.trees::load_downloaded_data_to_lists(download_data),


    cropped_data_set = berlin.trees::crop_data_with_bbox(tree_data_in_lists,
                                                         bounding_box),
    full_data_set = berlin.trees::bind_rows_sf(cropped_data_set),

    full_data_set_clean = target(berlin.trees::clean_data(full_data_set),
                                 format = "rds"),


    extract_uhi_values_to_list = berlin.trees::add_uhi_hist_data(uhi_stack_list = uhi_stacks,
                                                                 sf_data = full_data_set_clean[, ]),





    model_list = list(
        # null = function(x) lme4::lmer(dbh_cm ~ 1, data = x),
        # heat_only = function(x) lme4::lmer(dbh_cm ~ day_2007, data = x)  ,
        # heat_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) , data = x) ,
        # heat_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) + provenance , data = x) ,
        # heat_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) , data = x)  ,
        # heat_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) + provenance , data = x)  ,
        # heat_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + provenance + (1 + day_2007 | provenance : ART_BOT)  , data = x)  ,
        # heat_age = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER, data = x)  ,
        # heat_age_species = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + ART_BOT, data = x)  ,
        # heat_age_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT), data = x)  ,
        # heat_age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT) + provenance, data = x)  ,
        # heat_age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 + day_2007 | ART_BOT), data = x)  ,
        heat_age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | ART_BOT), data = x, control = lme4::lmerControl(optimizer ="Nelder_Mead"))  ,
        heat_age_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | provenance : ART_BOT), data = x, control = lme4::lmerControl(optimizer ="Nelder_Mead"))
        # age_only = function(x) lme4::lmer(dbh_cm ~ STANDALTER, data = x),
        # age_RIspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT), data = x),
        # age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT) + provenance, data = x),
        # age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT), data = x),
        # age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT) + provenance, data = x),
        # age_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | provenance : ART_BOT), data = x)
        # species_only = function(x) lme4::lmer(dbh_cm ~ ART_BOT, data = x)
    ),


    model_df = make_test_data_set(full_df = full_data_set_clean,
                                  extract_uhi = extract_uhi_values_to_list),





    model_res = berlin.trees::apply_models(df = model_df %>%
                                               dplyr::filter(STANDALTER < 350 &
                                                                 krone_m < 50 &
                                                                 dbh_cm < 600),
                                           model_list = model_list,
                                           n_top_species = 3,
                                           min_individuals = 150),




    # Plotting
    plot_overview_map = berlin.trees::make_overview_map(full_data_set_clean,
                                                        berlin_polygons,
                                                        file = drake::file_out("./analysis/figures/map_01_overview.png"),
                                                        height = 3.5,
                                                        width = 10.5,
                                                        dpi = 300),



    plot_count_map = berlin.trees::tree_count_map(full_data_set_clean,
                                                  berlin_polygons,file = drake::file_out("./analysis/figures/map_02_tree_sums_standardized.png"),
                                                  height = 12,
                                                  width = 12,
                                                  dpi = 300),

    plot_uhi_map = berlin.trees::make_uhi_plot(uhi_stacks = uhi_stacks,
                                               berlin_poly = berlin_polygons,
                                               base_size = 18,
                                               file = drake::file_out("./analysis/figures/map_03_uhi.png"),
                                               height = 6,
                                               width = 6,
                                               dpi = 300),





    plot_tree_sums_bar = berlin.trees::tree_sums_bar_plot(full_data_set_clean,
                                                          file = drake::file_out("./analysis/figures/plot_01_tree_sums_bar.png"),
                                                          base_size = 18,

                                                          height = 12,
                                                          width = 12,
                                                          dpi = 300),

    plot_density = berlin.trees::dens_plot_trees(sf_data = full_data_set_clean,
                                                 extracted_uhi = extract_uhi_values_to_list,
                                                 position_stack = "stack",
                                                 base_size = 18,
                                                 file = drake::file_out("./analysis/figures/plot_02_genus_UHI_dens.png"),
                                                 height = 12,
                                                 width = 12,
                                                 dpi = 300),


    # plot_LME_no_age = berlin.trees::make_ranef_plot(model_out = model_res,
    #                 model_name = "heat_RIspecies_RSspecies_RIprovenance",
    #                 df = full_data_set_clean,
    #                 n_top_species = 3
    #                 ),


    plot_LME_age = berlin.trees::make_ranef_plot(model_out = model_res,
                                                 model_name = "heat_age_RIspecies_RSspecies_RIprovenance",
                                                 df = full_data_set_clean,
                                                 n_top_species = 3,
                                                 base_size = 18,
                                                 file = drake::file_out("./analysis/figures/plot_03_ranef_species_dbh_uhi.png"),
                                                 height = 12,
                                                 width = 12,
                                                 dpi = 300),


    # tables


    age_tables = berlin.trees::make_age_table(df = model_df,
                                              max_age = 150,
                                              break_interval = 30),










    # Reporting
    paper_html = rmarkdown::render(
        knitr_in("./analysis/paper/paper.Rmd"),
        output_file = file_out(file.path(here::here(), "paper_knit.html")),
        # output_file = file_out(file.path("./paper_knit.html")),
        output_format = bookdown::html_document2(),
        quiet = TRUE
    ),
    # paper_word = rmarkdown::render(
    #     knitr_in("./analysis/paper/paper.Rmd"),
    #     output_file = file_out(file.path(here::here(), "paper_knit.docx")),
    #     # output_file = file_out(file.path("./paper_knit.html")),
    #     output_format = bookdown::word_document2(),
    #     quiet = TRUE
    # ),
    paper_pdf = rmarkdown::render(
        knitr_in("./analysis/paper/paper.Rmd"),
        output_file = file_out(file.path(here::here(), "paper_knit.pdf")),
        # output_file = file_out(file.path("./paper_knit.html")),
        output_format = bookdown::pdf_document2(),
        quiet = TRUE
    )

)


