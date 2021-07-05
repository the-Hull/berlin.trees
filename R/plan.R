library(drake)
# library(future.callr)
library(dplyr)
# library(future)
# future::plan(future.callr::callr)
# future::plan(future::multisession)
# options(future.globals.maxSize = 5000 * 1024 ^ 2)

redownload <- FALSE


plan <- drake_plan(

    # getting data



    ## Spatial Ancillary -----------------------------------



    ### Berlin districts and BBoxx
    berlin_polygons = target(download_berlin_polygons_as_sf(),
                             trigger = trigger(condition = redownload)),

    bounding_box = make_bbox(52.083962, 52.847599,
                                           12.712024, 14.238359,
                                           "greater_berlin",
                                           crs = 4326),

    # obtained from http://www.wudapt.org/continental-lcz-maps/
    # downloaded with wget in Ubuntu shell
    wudapt_lcz = crop_raster("./analysis/data/raw_data/spatial_ancillary/WUDAPT_LCZ.geotiff",
                              berlin_polygons,
                              buffer_dist = 10000),

    wudapt_lcz_by_district = raster::extract(wudapt_lcz,
                                     berlin_polygons) %>%
        lapply(., function(x)prop.table(table(x))) %>%
        setNames(berlin_polygons$NAMGEM) %>% dplyr::bind_rows(.id = "bezirk"),

    berlin_soil = target(download_soil_types(),
                         trigger = trigger(condition = redownload)),

    berlin_soil_nutrients = target(download_soil_nutrients(),
                                   trigger = trigger(condition = redownload)),

    berlin_building_height = target(download_building_height(),
                                    trigger = trigger(condition = redownload)),

    berlin_heat_model_2015 = target(download_heat_data(),
                                    trigger = trigger(condition = redownload)),


    ### Berlin UHI gridded data -------------------------------



    # loads the raster data from file (not uploaded to github due to size,
    # but see source in manuscript)

    uhi_stacks = get_uhi_rasters(file.path(here::here(), "analysis", "data", "raw_data", "UHI_explorer")),



    # Berlin trees data -------------------------------------------------------



    ## Berlin trees (download Senate tree data set from WFS using API)
    download_data = target(download_berlin_trees(),
                           trigger = trigger(condition = redownload)),

    # Berlin Baumscheiben
    download_data_baumscheiben = target(download_berlin_baumscheiben(),
                                        trigger = trigger(condition = redownload)),

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
    full_data_set_clean = clean_data(full_data_set),

    # Spatial ancillary extract/join ---------------------------

    # add baumscheiben area and soil type to full_data
    # polygon / non-raster operations
    baumsch_data = prep_baumscheiben_flaeche(full_data_set_clean,
                                                                 baumscheiben_in_lists$s_Baumscheibe,
                                                                 max_dist_m = 10),

    soil_type_data = prep_soil_type(full_data_set_clean,
                                    berlin_soil,
                                    max_dist_m = 25),

    soil_nutrient_data = prep_soil_nutrients(full_data_set_clean,
                                             berlin_soil_nutrients,
                                             max_dist_m = 25),


    # %>%
    #     sf::st_join(x = .,
    #                 y = berlin_soil[, c("id", "NUTZ_BEZ", "BOGES_NEU5", "BTYP", "BTYP_KA4", "FLUR")] %>%
    #                     prefix_names(prefix = "soil"),
    #                 join = sf::st_nearest_feature),


    lcz_cover_prop = furrr::future_map_dfr(split_by_n(full_data_set_clean,
                                                      5000),
                                           ~assess_relative_lcz_cover(.x, wudapt_lcz, 150),
                                           .progress = FALSE),

    building_height_mean_m = furrr::future_map(split_by_n(full_data_set_clean,
                                     75000),
                          ~assess_relative_building_height(sf_data =  .x,
                                                           bh_raster = berlin_building_height,
                                                           buff_dist = 150),
                          .progress = FALSE) %>%
        unlist() %>%
        unname(),


    berlin_heat_model = assess_mean_temps(full_data_set_clean,
                                          berlin_heat_model_2015,
                                          20),

    # Add UHI data from RasterLayer stack to sf data frame
    extract_uhi_values_to_list = add_uhi_hist_data(uhi_stack_list = uhi_stacks,
                                                                 sf_data = full_data_set_clean[, ]),


    # add lcz, soil, building height, mineral content to data set
    covariate_df = combine_covariates(list(baumsch_data = baumsch_data,
                            soil_type = soil_type_data,
                            soil_nutrients = soil_nutrient_data,
                            building_height_mean_m = building_height_mean_m,
                            lcz_cover_prop = lcz_cover_prop,
                            berlin_heat_model = berlin_heat_model)),



    # Generate data set for stat-models
    # This step just adds the UHI temperature data to the data set
    # and adjusts a bit
    full_data_set_clean_with_UHI = make_test_data_set(full_df = full_data_set_clean,
                                  extract_uhi = extract_uhi_values_to_list),


    full_data_set_clean_with_UHI_covariates = cbind(full_data_set_clean_with_UHI,
                                                    covariate_df),



    # Data cleaning (datacleanr) ------------------------

    split_and_save = split_df(sfdf = full_data_set_clean_with_UHI_covariates,
                        save_dir = drake::file_out("./analysis/data/raw_data/tree_splits")),


    combined_data = combine_split_clean_data(full_data_set_clean_with_UHI_covariates),


     # Model -------------------------------------------------------------------


    model_params = list(n_max_species = 10,
                                        age_cutoff = 150,
                                        mad_factor = 7),

    model_df = prep_model_df(
        dset = combined_data,
        model_params = model_params,
        plot = TRUE,
        path = "./analysis/figures/diagnostic_01_model_data.png"),



    model_grid = make_model_grid(),

    # bam_dbh_fulldf = apply_gam_mod(path = "./analysis/data/models/stat/fulldf/",
    #                                model_grid = model_grid, dat = model_df,
    #                                overwrite = FALSE),
    #
    #
    # bam_dbh_filtered = apply_gam_mod(path = "./analysis/data/models/stat/filtered/",
    #     model_grid = model_grid, dat = model_df[model_df$diag_mad_select,],
    #     overwrite = FALSE),




    # This is a list of models I apply simultaneously for later evaluation
    model_list = list(
        # null = function(x) lme4::lmer(dbh_cm ~ 1, data = x),
        # heat_only = function(x) lme4::lmer(dbh_cm ~ day_2007, data = x)  ,
        # heat_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) , data = x) ,
        # heat_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 | ART_BOT) + provenance , data = x) ,
        # heat_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) , data = x)  ,
        # heat_RIspecies_RSspecies_provenance = function(x) dralme4::lmer(dbh_cm ~ day_2007 + (1 + day_2007 | ART_BOT) + provenance , data = x)  ,
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




   # here I apply all lme4 models not commented-out above,
   # and also specify the data which I want to exclude
    model_res = apply_models(df = full_data_set_clean_with_UHI_covariates %>%
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

    age_tables = make_age_table(df = full_data_set_clean_with_UHI_covariates,
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


