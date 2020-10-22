library(drake)
# library(future.callr)
library(dplyr)
library(future)
future::plan(future.callr::callr)
# future::plan(future::multiprocess)
# options(future.globals.maxSize = 600 * 1024 ^ 2)


plan <- drake_plan(

    # getting data



    ## Spatial Ancillary -----------------------------------

    ### Berlin districts and BBoxx
    berlin_polygons = target(get_berlin_polygons_as_sf()),

    bounding_box = make_bbox(52.083962, 52.847599,
                                           12.712024, 14.238359,
                                           "greater_berlin",
                                           crs = 4326),

    ### Berlin UHI gridded data -------------------------------



    # loads the raster data from file (not uploaded to github due to size,
    # but see source in manuscript)

    uhi_stacks = get_uhi_rasters(file.path(here::here(), "analysis", "data", "raw_data", "UHI_explorer")),



    # Berlin trees data -------------------------------------------------------



    ## Berlin trees (download Senate tree data set from WFS using API)
    download_data = target(download_berlin_trees()),

    # Berlin Baumscheiben
    download_data_baumscheiben = download_berlin_baumscheiben(),

    ### Cleaning

    # Load spatial-features data sets of all Berlin trees
    tree_data_in_lists = load_downloaded_data_to_lists(download_data),


    # Load spatial-features data sets of all Berlin baumscheiben
    baumscheiben_in_lists = load_downloaded_data_to_lists(download_data_baumscheiben),


    cropped_data_set = crop_data_with_bbox(tree_data_in_lists,
                                                         bounding_box),

    # Bind rows of sf tibbles to single data set (currently in lists)
    full_data_set = bind_rows_sf(cropped_data_set),

    # Clean Feature Meta Data
    full_data_set_prep = clean_data(full_data_set),

    # add baumscheiben area to full_data
    full_data_set_clean = add_baumscheiben_flaeche(full_data_set_prep,
                                                                 baumscheiben_in_lists$s_Baumscheibe,
                                                                 max_dist_m = 10),

    # Add UHI data from RasterLayer stack to sf data frame
    extract_uhi_values_to_list = add_uhi_hist_data(uhi_stack_list = uhi_stacks,
                                                                 sf_data = full_data_set_clean[, ]),

    # steps to add baumscheiben to data set
    # baumscheiben_with_tree_idx = add_full_df_idx_to_baumscheiben(bms = baumscheiben_in_lists,
    #                                                                         fulldf = full_data_set_clean,
    #                                                                         max_dist_m = 15),


     # Model -------------------------------------------------------------------



    # This is a list of models I apply simulatenously for later evaluation
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


    # Generate data set for stat-models
    # This step just adds the UHI temperature data to the data set
    # and adjusts a bit
    model_df = make_test_data_set(full_df = full_data_set_clean,
                                  extract_uhi = extract_uhi_values_to_list),




   # here I apply all lme4 models not commented-out above,
   # and also specify the data which I want to exclude
    model_res = apply_models(df = model_df %>%
                                               dplyr::filter(STANDALTER < 350 &
                                                                 krone_m < 50 &
                                                                 dbh_cm < 300,
                                                             dbh_cm > 20),
                                           model_list = model_list,
                                           n_top_species = 3,
                                           min_individuals = 1000),




    # Plotting --------------------------------

   # Create overview-map of all data sets

    plot_overview_map = make_overview_map(full_data_set_clean,
                                                        berlin_polygons,
                                                        file = drake::file_out("./analysis/figures/map_01_overview.png"),
                                                        height = 3.5,
                                                        width = 10.5,
                                                        dpi = 300),



   # Spatially-binned tree counts

    plot_count_map = tree_count_map(full_data_set_clean,
                                                  berlin_polygons,file = drake::file_out("./analysis/figures/map_02_tree_sums_standardized.png"),
                                                  height = 12,
                                                  width = 12,
                                                  dpi = 300),

   # Plot UHI with Berlin districts

    plot_uhi_map = make_uhi_plot(uhi_stacks = uhi_stacks,
                                               berlin_poly = berlin_polygons,
                                               base_size = 18,
                                               file = drake::file_out("./analysis/figures/map_03_uhi.png"),
                                               height = 6,
                                               width = 6,
                                               dpi = 300),




   # Generate overview of records (bar plot)

    plot_tree_sums_bar = tree_sums_bar_plot(full_data_set_clean,
                                                          file = drake::file_out("./analysis/figures/plot_01_tree_sums_bar.png"),
                                                          base_size = 18,

                                                          height = 12,
                                                          width = 12,
                                                          dpi = 300),
   # Density plot overview

    plot_density = dens_plot_trees(sf_data = full_data_set_clean,
                                                 extracted_uhi = extract_uhi_values_to_list,
                                                 position_stack = "stack",
                                                 base_size = 18,
                                                 file = drake::file_out("./analysis/figures/plot_02_genus_UHI_dens.png"),
                                                 height = 12,
                                                 width = 12,
                                                 dpi = 300),


    # plot_LME_no_age = make_ranef_plot(model_out = model_res,
    #                 model_name = "heat_RIspecies_RSspecies_RIprovenance",
    #                 df = full_data_set_clean,
    #                 n_top_species = 3
    #                 ),


   # Make Random-effects effect-size plot

    plot_LME_age = make_ranef_plot(model_out = model_res$model,
                                                 model_name = "heat_age_RIspecies_RSspecies_RIprovenance",
                                                 df = model_res$test_data,
                                                 n_top_species = 3,
                                                 base_size = 18,
                                                 file = drake::file_out("./analysis/figures/plot_03_ranef_species_dbh_uhi.png"),
                                                 height = 12,
                                                 width = 12,
                                                 dpi = 300),


    # tables ------------------------------------------

    overview_table = make_overview_table(full_data_set_clean),


   # Generate table of genera age distribution

    age_tables = make_age_table(df = model_df,
                                              max_age = 150,
                                              break_interval = 30),










    # Reporting ------------------------------
    paper_html = rmarkdown::render(
        knitr_in("./analysis/paper/paper.Rmd"),
        # output_file = file_out(file.path(here::here(), "paper_knit.html")),
        output_file = file_out("./paper_knit.html"),
        output_format = bookdown::html_document2(),
        quiet = TRUE
    ),
    paper_word = rmarkdown::render(
        knitr_in("./analysis/paper/paper.Rmd"),
        output_file = file_out("./paper_knit.docx"),
        # output_file = "./paper_knit.html",
        output_format = bookdown::word_document2(),
        quiet = TRUE
    ),
    paper_pdf = rmarkdown::render(
        knitr_in("./analysis/paper/paper.Rmd"),
        output_file = file_out("./paper_knit.pdf"),
        # output_file = "./paper_knit.html"),
        output_format = bookdown::pdf_document2(),
        quiet = TRUE
    )

)


