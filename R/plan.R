library(drake)
# library(future.callr)
library(dplyr)
# library(future)
# future::plan(future.callr::callr)
# future::plan(future::multisession)
# options(future.globals.maxSize = 5000 * 1024 ^ 2)

redownload <- FALSE


plan <- drake::drake_plan(

    # getting data


    ## Climate time series data ---------------------------------------


    berlin_climate = download_berlin_climate_data(),


    ## Spatial Ancillary -----------------------------------


    ### Berlin grid
    gridp = sf::st_read("analysis/data/raw_data/spatial_ancillary/grid_2x2.geojson"),


    ### Berlin districts and BBoxx
    berlin_polygons = target(download_berlin_polygons_as_sf(),
                             trigger = trigger(condition = redownload)),

    bounding_box = make_bbox(52.083962, 52.847599,
                                           12.712024, 14.238359,
                                           "greater_berlin",
                                           crs = 4326),

    ### Berlin land-use from senate

    berlin_lu = download_berlin_lu(),


    # obtained from http://www.wudapt.org/continental-lcz-maps/
    # downloaded with wget in Ubuntu shell
    wudapt_lcz = crop_raster("./analysis/data/raw_data/spatial_ancillary/WUDAPT_LCZ.geotiff",
                              berlin_polygons,
                              buffer_dist = 10000),

    wudapt_lcz_by_district = raster::extract(wudapt_lcz,
                                     berlin_polygons) %>%
        lapply(., function(x)prop.table(table(x))) %>%
        setNames(berlin_polygons$NAMGEM) %>% dplyr::bind_rows(.id = "bezirk"),

    corine_landcover_mask = make_corine_urban_rural_mask(path = "analysis/data/raw_data/spatial_ancillary/CORINE_CLC.zip",
                                                          berlin_bbox = bounding_box),

    berlin_soil = target(download_soil_types(),
                         trigger = trigger(condition = redownload)),

    berlin_soil_nutrients = target(download_soil_nutrients(),
                                   trigger = trigger(condition = redownload)),

    berlin_building_height = target(download_building_height(),
                                    trigger = trigger(condition = redownload)),

    berlin_heat_model_2015 = target(download_heat_data(),
                                    trigger = trigger(condition = redownload)),


    urbclim = download_urbclim_uhi(month = "08",
                                   year = "2015",
                                   path_dir = "./analysis/data/raw_data/spatial_ancillary/ecmwfr_urbclim"),

    uhi_urbclim = calc_urbclim_uhi_with_corine(urbclim, clc = corine_landcover_mask$clc ,natural_cover_val = 50, make_plot = FALSE),


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
    # lcz_cover_prop30 = furrr::future_map_dfr(split_by_n(full_data_set_clean,
    #                                                   5000),
    #                                        ~assess_relative_lcz_cover(.x, wudapt_lcz, 30),
    #                                        .progress = FALSE),
    lcz_cover_prop300 = furrr::future_map_dfr(split_by_n(full_data_set_clean,
                                                      5000),
                                           ~assess_relative_lcz_cover(.x, wudapt_lcz, 300),
                                           .progress = FALSE),

    building_height_mean_m = furrr::future_map(split_by_n(full_data_set_clean,
                                     75000),
                          ~assess_relative_building_height(sf_data =  .x,
                                                           bh_raster = berlin_building_height,
                                                           buff_dist = 150),
                          .progress = FALSE) %>%
        unlist() %>%
        unname(),



    building_height_mean_m_300 = furrr::future_map(split_by_n(full_data_set_clean,
                                                          75000),
                                               ~assess_relative_building_height(sf_data =  .x,
                                                                                bh_raster = berlin_building_height,
                                                                                buff_dist = 300),
                                               .progress = FALSE) %>%
        unlist() %>%
        unname(),


    # heat polygons based on block level - 20 m captures canopy
    berlin_heat_model = assess_mean_temps(full_data_set_clean,
                                          berlin_heat_model_2015,
                                          buff_dist = 150),

    # raster has 100 m resolution
    berlin_urbclim_heat_model = assess_mean_temps_urbclim(full_data_set_clean,
                                                          uhi_urbclim,
                                                          buff_dist = 150),



    # Add UHI data from RasterLayer stack to sf data frame
    extract_uhi_values_to_list = add_uhi_hist_data(
        uhi_stack_list = uhi_stacks,
        sf_data = full_data_set_clean[, ],
        buff_dist = 150),


    # add lcz, soil, building height, mineral content to data set
    covariate_df = combine_covariates(list(baumsch_data = baumsch_data,
                            soil_type = soil_type_data,
                            soil_nutrients = soil_nutrient_data,
                            building_height_mean_m = building_height_mean_m,
                            building_height_mean_m_300 = building_height_mean_m_300,
                            lcz_cover_prop = lcz_cover_prop,
                            # lcz_cover_prop30 = lcz_cover_prop30,
                            lcz_cover_prop300 = lcz_cover_prop300,
                            berlin_heat_model = berlin_heat_model,
                            berlin_urbclim_heat_model = berlin_urbclim_heat_model)),



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

    # Stat Descriptive -----------------------------------

    uhi_stack_stats = calc_uhi_stats(uhi_stacks),


     # Stat Model -------------------------------------------------------------------


    model_params = list(
        n_max_species = 10,
        n_min_abundance = 10000,
        age_cutoff = 125,
        dbh_cutoff = 350, # cm
        mad_factor = 7),

    model_df_full = prep_model_df(
        dset = combined_data[combined_data$provenance == "s_wfs_baumbestand",],
        model_params = model_params,
        plot = TRUE,
        path = "./analysis/figures/diagnostic_01_model_data.png",
        stat_filter = FALSE),

    model_df_stat_filtered = prep_model_df(
        dset = combined_data[combined_data$provenance == "s_wfs_baumbestand",],
        model_params = model_params,
        plot = TRUE,
        path = "./analysis/figures/diagnostic_01_model_data.png",
        stat_filter = TRUE),

    # full_df_street_trees_idx = filter_top_species_idx(
    #     model_df[model_df$provenance == "s_wfs_baumbestand",],
    #     model_params
    #     ),
    #
    # filtered_df_street_trees_idx = filter_top_species_idx(
    #     model_df[model_df$provenance == "s_wfs_baumbestand" & model_df$diag_mad_select,],
    #     model_params
    #     ),

    # model_df_full = model_df[model_df$provenance == "s_wfs_baumbestand" &
    #                              full_df_street_trees_idx, ],
    #
    # model_df_stat_filtered = model_df[model_df$provenance == "s_wfs_baumbestand" &
    #                                       model_df$diag_mad_select &
    #                                       filtered_df_street_trees_idx, ],




    model_grid = make_model_grid(),

    bam_dbh_fulldf = apply_gam_mod(path = drake::file_out("./analysis/data/models/stat/fulldf/"),
                                   model_grid = model_grid,
                                   dat = model_df_full,
                                   overwrite = TRUE),
    #
    #
    bam_dbh_filtered = apply_gam_mod(path = drake::file_out("./analysis/data/models/stat/filtered/"),
                                     model_grid = model_grid,
                                     dat = model_df_stat_filtered,
                                     overwrite = TRUE),




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


   # path_model_dir_filtered = drake::file_in("analysis/data/models/stat/filtered/"),
   path_model_files_filtered = {

       invisible(is.null(bam_dbh_filtered))
       list.files(drake::file_in("analysis/data/models/stat/filtered/"), full.names = TRUE)}
   ,



   mod_groups_filtered = list.files(path = drake::file_in("analysis/data/models/stat/filtered/"), ignore.case = TRUE) %>%
       gsub("_var-.*Rds$", "", .) %>%
       as.factor(),


   # grab model paths and format into list
   mod_group_list_filtered =
       purrr::map(
           levels(mod_groups_filtered),
           function(mg){

               idx <- which(mod_groups_filtered == mg)

               mods <- lapply(path_model_files_filtered[idx],
                              function(x){
                                  return(x)
                              }) %>%
                   setNames(
                       fs::path_ext_remove(
                           list.files(drake::file_in("analysis/data/models/stat/filtered/"))[idx]
                       )  %>%
                           gsub(pattern = ".*var-",
                                replacement = "",
                                x = .)
                   )
           }) %>%
       setNames(levels(mod_groups_filtered)),



   mod_summaries_filtered = drake::target(model_summarize(mgroups = mod_groups_filtered,
                                            bam_df = bam_dbh_filtered,
                                    path_model_dir = drake::file_in("analysis/data/models/stat/filtered/"),
                                    path_model_files = path_model_files_filtered,
                                    path_out = drake::file_out("./analysis/data/models/stat/summary/mod_filtered_summary.Rds")),
                                    format = "file"),


   ## predictions


   age_expr = expression(dplyr::case_when(
       dplyr::between(STANDALTER, 30, 35) ~ "[30 - 35]",
       dplyr::between(STANDALTER, 45, 50) ~ "[45 - 50]",
       dplyr::between(STANDALTER, 60, 65) ~ "[60 - 65]",
       dplyr::between(STANDALTER, 75, 80) ~ "[75 - 80]",
       dplyr::between(STANDALTER, 90, 95) ~ "[90 - 95]",
       TRUE ~ NA_character_
   )),


   # temp varying
   pred_data_single_tempvar =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(X = 388141,
                         Y = 5818534,
                         STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95)),
       age_expression = age_expr,
       group_vars = NULL),


   pred_data_single_tempvar_fixed_lcz6 =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_lcz6_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(X = 388141,
                         Y = 5818534,
                         STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
                         lcz_prop_6 = 0.5),
       age_expression = age_expr,
       group_vars = NULL),


   pred_data_single_tempvar_fullmodel =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_reBEZIRK_full_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(X = 388141,
                         Y = 5818534,
                         STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95)),
       age_expression = age_expr,
       group_vars = NULL),


   # temp fixed
   pred_data_single_tempvar_fixed_baumsch =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_baumsch_flaeche_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(
           tempvar = 3,
           X = 388141,
           Y = 5818534,
           STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
           baumsch_flaeche_m2 = seq(1, 12, by = 0.1)),
       age_expression = age_expr,
       group_vars = "baumsch_flaeche_m2"),


   pred_data_single_tempvar_fixed_build =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_building_height_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(
           tempvar = 3,
           X = 388141,
           Y = 5818534,
           STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
           building_height_m = seq(1, 25, by = 0.25)),
       age_expression = age_expr,
       group_vars = "building_height_m"),


   pred_data_single_tempvar_fixed_build300 =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_building_height300_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(
           tempvar = 3,
           X = 388141,
           Y = 5818534,
           STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
           building_height_m300 = seq(1, 25, by = 0.25)),
       age_expression = age_expr,
       group_vars = "building_height_m300"),

   pred_data_single_tempvar_fixed_build300_temp15 =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_building_height300_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(
           tempvar = 1.5,
           X = 388141,
           Y = 5818534,
           STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
           building_height_m300 = seq(1, 25, by = 0.25)),
       age_expression = age_expr,
       group_vars = "building_height_m300"),



   pred_data_single_tempvar_fixed_build300_temp_multi =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_building_height300_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(
           tempvar = c(-3, -1.5, 0, 1.5,3, 4.5, 6),
           X = 388141,
           Y = 5818534,
           STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
           building_height_m300 = seq(1, 25, by = 0.25)),
       age_expression = age_expr,
       group_vars = c("building_height_m300")),



   pred_data_single_tempvar_multi_lcz6 =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_lcz6_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(
           tempvar = 3,
           X = 388141,
           Y = 5818534,
           STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
           lcz_prop_6 = seq(0, 1, by = 0.025)),
       age_expression = age_expr,
       group_vars = "lcz_prop_6"),


   pred_data_single_tempvar_fixed_lcz6_300 =  pred_dbh_temp_single_var(
       path_model =    bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_lcz6_300_reBEZIRK_var-day_2007', 'model_file_path'],
       model_df = model_df_stat_filtered,
       fixed_vars = list(
           tempvar = 3,
           X = 388141,
           Y = 5818534,
           STANDALTER = c(30:35, 45:50, 60:65, 75:80, 90:95),
           lcz_prop300_6 = seq(0, 1, by = 0.025)),
       age_expression = age_expr,
       group_vars = "lcz_prop300_6"),


   # Stat Spatial ---------------------------

   ## autocorrelation ------------------


   moran_comparison = assess_morans_spatialmod(
       mod_list = mod_group_list_filtered[grepl(
           pattern = "(mI_spatial_age_ADD_temp_by_species_reBEZIRK$|mI_spatial_age_x_temp_by_species_reBEZIRK$)",
           x = names(mod_group_list_filtered))],
       bam_df = bam_dbh_filtered,
       gridp = gridp,
       var_response = "dbh_cm",
       var_resid = ".resid"
   ),


   moran_summary = summarize_moran(moran_comparison),



    # Plotting --------------------------------

   journal_fig_width_single = 8.2,
   journal_fig_width_double = 17,
   journal_fig_width_mid = 8.2 * 1.5,


   # UFUG Submission --------------------------

   base_path_fig = "./analysis/figures/ufug_submission/hurley_heinrich_",

   ### map: study area ------

   plot_study_area_map = make_map_study_area(blu = berlin_lu,
                                             berlin_poly = berlin_polygons,
                                             path_out = paste0(base_path_fig, "01_single_map_studyarea.tiff"),
                                             height = journal_fig_width_single * 0.7857143,
                                             width = journal_fig_width_single,
                                             dpi = 300),


   ### map: temp/UHI overview ----------------

   plot_uhi_overview_maps = plot_temp_maps(landsat = uhi_stacks,
                                           envat = berlin_heat_model_2015,
                                           urbclim = raster::stack(uhi_urbclim),
                                           berlin_poly = berlin_polygons,
                                           base_size = 8,
                                           file = paste0(base_path_fig, "02_double_map_temp.tiff"),
                                           dpi = 300,
                                           height = journal_fig_width_double,
                                           width = journal_fig_width_double),



   ### BIWI: cambial age vs. annual growth -------------------

   species_lookup = read.csv(drake::file_in("analysis/data/tables/species_lookup.csv"), header = TRUE),



   series_long = prep_rwl_data(path_meta_cores = drake::file_in("./analysis/data/raw_data/biwi/BIWi_INV_20190123.xlsx"),
                               path_meta_trees = drake::file_in("./analysis/data/raw_data/biwi/BIWi_INV_20190123.xlsx"),
                               path_meta_sites = drake::file_in("./analysis/data/raw_data/biwi/BIWi_INV_20190123.xlsx"),
                               path_dir_fh = drake::file_in("./analysis/data/raw_data/biwi/rwl/")),

   biwi_preds = biwi_mod_predict(series_long),

   biwi_plot = make_biwi_plot(biwi_preds,
                              file = paste0(base_path_fig, "03_single_biwi_annual_growth.tiff"),
                              height = journal_fig_width_single,
                              width = journal_fig_width_single,
                              dpi = 300,
                              base_size = 10),







   ### stat: GAMM deviance ------------------


   gam_deviances = extract_mod_deviance(readRDS(mod_summaries_filtered)),
   # gam_deviances = extract_mod_deviance(readRDS(drake::file_in("analysis/data/models/stat/summary/mod_filtered_summary.Rds"))),

   plot_gam_deviance = make_deviance_plot(
       deviance_list = gam_deviances,
       base_size = 10,
       file = paste0(base_path_fig, "04_double_model_deviance.tiff"),
       height =  0.8333333 * journal_fig_width_double,
       width = journal_fig_width_double,
       dpi = 300),


    # obs vs. pred day2007, lcz6
    plot_obs_pred_single_temp_lcz6 =
        plot_obs_predicted_model(path_model = bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_lcz6_reBEZIRK_var-day_2007', 'model_file_path'],
                                 file = paste0(base_path_fig, "05_single_gam-dbh_temp_day2007_lcz6_obs_pred.tiff"),
                                 height = 0.875 * journal_fig_width_single,
                                 width = journal_fig_width_single,
                                 base_size = 10,
                                 dpi = 300),


   ### stat: gamm predictions


   # uhi2007, lcz6
   plot_gam_temp_lcz6_prediction = plot_dbh_temp_single_var(
       pred_list = pred_data_single_tempvar_fixed_lcz6,
       model_df = model_df_stat_filtered,
       age_filter = NULL,
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 12,
       file = paste0(base_path_fig, "06_double_gam-dbh_temp-day2007_lcz6.png"),
       height = 0.75 * journal_fig_width_double,
       width = journal_fig_width_double,
       dpi = 300),







   ### stat: dbh temp sensitivites (plot and calc) --------------


   plot_dbh_sens_lms = plot_dbh_temp_comparison(pred_list = pred_data_single_tempvar_fixed_lcz6,
                                                model_df = model_df_stat_filtered,
                                                age_filter = c('[45 - 50]', '[75 - 80]', '[90 - 95]'),
                                                species_filter = NULL,
                                                # species_filter = c("Tilia cordata", "Tilia platyphyllos"),
                                                # species_filter = c("Tilia cordata","Platanus acerifolia"),
                                                age_expression = age_expr,
                                                prediction_range = "within",
                                                base_size = 10,
                                                file = paste0(base_path_fig, "07_double_day2007_lcz6_growth_sensitivity.tiff"),
                                                height = journal_fig_width_double * 0.5,
                                                width = journal_fig_width_double,
                                                dpi = 300),



   # tilia
   plot_gam_temp_prediction_single_genus_lcz6 =
       plot_dbh_temp_single_var_flex(pred_list = pred_data_single_tempvar_multi_lcz6,
                                     model_df = model_df_stat_filtered,
                                     var = "lcz_prop_6",
                                     age_filter = c("[45 - 50]", "[60 - 65]", '[75 - 80]', '[90 - 95]'),
                                     species_filter = c("Tilia cordata", "Tilia platyphyllos"),
                                     # species_filter = c("Tilia cordata","Platanus acerifolia"),
                                     age_expression = age_expr,
                                     prediction_range = "within",
                                     base_size = 10,
                                     x_label = expression('Proportional Cover - LCZ6'[bar(150~m)]),
                                     file = paste0(base_path_fig, "08_single_dbh_temp-day2007_xvar-lcz6_tilia.tiff"),
                                     height = 0.875 * journal_fig_width_single,
                                     width = journal_fig_width_single,
                                     dpi = 300),









   # Other plots --------------------------------









   ### map: Tree overview-----

    plot_overview_map = make_overview_map(full_data_set_clean,
                                                        berlin_polygons,
                                                        file = drake::file_out("./analysis/figures/map_01_overview.png"),
                                                        height = 3.5,
                                                        width = 10.5,
                                                        dpi = 300),



   ### map: Binned tree counts -----

    plot_count_map = tree_count_map(full_data_set_clean,
                                                  berlin_polygons,file = drake::file_out("./analysis/figures/map_02_tree_sums_standardized.png"),
                                                  height = 12,
                                                  width = 12,
                                                  dpi = 300),

   ### map: UHI Explorer ------------

    plot_uhi_map = make_uhi_plot(uhi_stacks = uhi_stacks,
                                               berlin_poly = berlin_polygons,
                                               base_size = 18,
                                               file = drake::file_out("./analysis/figures/map_03_uhi.png"),
                                               height = 6,
                                               width = 6,
                                               dpi = 300),



   ### map: UHI urbclim -------------

   plot_uhi_urbclim_map = make_uhi_urbclim_plot(uhi_rast = uhi_urbclim[[3]],
                                                berlin_poly = berlin_polygons,
                                                file = drake::file_out("./analysis/figures/map_03_uhi_urbclim.png"),
                                                height = 5.5,
                                                width = 6,
                                                dpi = 300,
                                                legend_label = expression(atop(Summer~21-23~hrs,
                                                                               UHI~(degree*C)))),




   ### bar: Tree counts ------------

    plot_tree_sums_bar = tree_sums_bar_plot(full_data_set_clean,
                                                          file = drake::file_out("./analysis/figures/plot_01_tree_sums_bar.png"),
                                                          base_size = 18,

                                                          height = 12,
                                                          width = 12,
                                                          dpi = 300),
   ### dens: trees by UHI -------------

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


   ### stat: Berlin Climate plot -------



   plot_berlin_climate = make_berlin_climate_plot(
       clim = berlin_climate,

       col_temp = "#ed6a1f",
       col_prec = "#3dd2e3",
       col_prec_secondary = "#93d2d9",

       base_size = 18,
       file = drake::file_out("./analysis/figures/fig-berlin-climate.png"),
       height = 8,
       width = 6,
       dpi = 300
   ),

   ### stat:  RanEf-size --------------

    plot_LME_age = make_ranef_plot(model_out = model_res$model,
                                                 model_name = "heat_age_RIspecies_RSspecies_RIprovenance",
                                                 df = model_res$test_data,
                                                 n_top_species = 3,
                                                 base_size = 18,
                                                 file = drake::file_out("./analysis/figures/plot_03_ranef_species_dbh_uhi.png"),
                                                 height = 12,
                                                 width = 12,
                                                 dpi = 300),





   ### stat: GAMM AIC ------------------
   gam_aic = extract_mod_AIC(readRDS(mod_summaries_filtered)),

   plot_gam_aic = make_aic_plot(
       aic_list = gam_aic,
       base_size = 18,
       file = drake::file_out("./analysis/figures/fig_model_aic.png"),
       height = 10,
       width = 12,
       dpi = 300),
   ### stat: GAMM day_2007 prediction ----------------

   plot_gam_temp_prediction = plot_dbh_temp_single_var(
       pred_list = pred_data_single_tempvar,
       model_df = model_df_stat_filtered,
       age_filter = NULL,
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 18,
       file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007.png"),
       height = 11,
       width = 18,
       dpi = 300),



   plot_gam_temp_prediction_fullmodel = plot_dbh_temp_single_var(
       pred_list = pred_data_single_tempvar_fullmodel,
       model_df = model_df_stat_filtered,
       age_filter = NULL,
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 18,
       file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007_fullmodel.png"),
       height = 11,
       width = 18,
       dpi = 300),





   plot_gam_temp_prediction_single_genus = plot_dbh_temp_single_var_single_species(
       pred_list = pred_data_single_tempvar_fixed_lcz6,
       model_df = model_df_stat_filtered,
       age_filter = NULL,
       species_filter = c("Tilia cordata", "Tilia platyphyllos"),
       # species_filter = c("Tilia cordata","Platanus acerifolia"),
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 18,
       file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007-lcz6_tilia.png"),
       height = 6,
       width = 12,
       dpi = 300),


   plot_gam_temp_prediction_genus_comparison = plot_dbh_temp_single_var_single_species(
       pred_list = pred_data_single_tempvar_fixed_lcz6,
       model_df = model_df_stat_filtered,
       age_filter = c('[45 - 50]', '[75 - 80]'),
       species_filter = NULL,
       # species_filter = c("Tilia cordata", "Tilia platyphyllos"),
       # species_filter = c("Tilia cordata","Platanus acerifolia"),
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 18,
       file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007-lcz6_all_genus.png"),
       height = 6,
       width = 12,
       dpi = 300),



   plot_gam_temp_prediction_single_genus_building_height = plot_dbh_temp_single_var_flex(pred_list = pred_data_single_tempvar_fixed_build,
                                 model_df = model_df_stat_filtered,
                                 var = "building_height_m",
                                 age_filter = c("[45 - 50]", "[60 - 65]", '[75 - 80]', '[90 - 95]'),
                                 species_filter = c("Tilia cordata", "Tilia platyphyllos"),
                                 # species_filter = c("Tilia cordata","Platanus acerifolia"),
                                 age_expression = age_expr,
                                 prediction_range = "within",
                                 base_size = 18,
                                 x_label = expression('Building height '[bar(150~m)]~(m)),
                                 file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007_building_height_tilia.png"),
                                 height = 7,
                                 width = 8,
                                 dpi = 300),


   plot_gam_temp_prediction_single_genus_building_height300 = plot_dbh_temp_single_var_flex(pred_list = pred_data_single_tempvar_fixed_build300,
                                 model_df = model_df_stat_filtered,
                                 var = "building_height_m300",
                                 age_filter = c("[45 - 50]", "[60 - 65]", '[75 - 80]', '[90 - 95]'),
                                 species_filter = c("Tilia cordata", "Tilia platyphyllos"),
                                 # species_filter = c("Tilia cordata","Platanus acerifolia"),
                                 age_expression = age_expr,
                                 prediction_range = "within",
                                 base_size = 18,
                                 x_label = expression('Building height '[bar(300~m)]~(m)),
                                 file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007_building_height300_tilia.png"),
                                 height = 7,
                                 width = 8,
                                 dpi = 300),
   plot_gam_temp_prediction_single_genus_building_height300_temp15 = plot_dbh_temp_single_var_flex(pred_list = pred_data_single_tempvar_fixed_build300_temp15,
                                 model_df = model_df_stat_filtered,
                                 var = "building_height_m300",
                                 age_filter = c("[45 - 50]", "[60 - 65]", '[75 - 80]', '[90 - 95]'),
                                 species_filter = c("Tilia cordata", "Tilia platyphyllos"),
                                 # species_filter = c("Tilia cordata","Platanus acerifolia"),
                                 age_expression = age_expr,
                                 prediction_range = "within",
                                 base_size = 18,
                                 x_label = expression('Building height '[bar(300~m)]~(m)),
                                 file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007_building_height30_temp1-5_tilia.png"),
                                 height = 7,
                                 width = 8,
                                 dpi = 300),





   plot_gam_temp_prediction_single_genus_lcz6_300 = plot_dbh_temp_single_var_flex(
       pred_list = pred_data_single_tempvar_fixed_lcz6_300,
       model_df = model_df_stat_filtered,
       var = "lcz_prop300_6",
       age_filter = c("[45 - 50]", "[60 - 65]", '[75 - 80]', '[90 - 95]'),
       species_filter = c("Tilia cordata", "Tilia platyphyllos"),
       # species_filter = c("Tilia cordata","Platanus acerifolia"),
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 18,
       x_label = expression('Proportional Cover - LCZ6'[bar(300~m)]),
       file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007_lcz6_300_tilia.png"),
       height = 7,
       width = 8,
       dpi = 300),


   ### stat: Obs vs. Pred --------------------------------------------------


   plot_obs_pred_single_temp = plot_obs_predicted_model(path_model = bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_reBEZIRK_var-day_2007', 'model_file_path'],
                                            file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007_obs_pred.png"),
                                            height = 7,
                                            width = 8,
                                            dpi = 300),







   ### stat: moran comparison ------------------



   plot_moran_blocks = plot_moran_comparison(moran_summary,
                         base_size = 18,
                         file = drake::file_out("./analysis/figures/fig-gam-spatial_moran_comparison_blocks.png"),
                         height = 8,
                         width = 14,
                         dpi = 300),




   ### SI: Heat comparison ------------------------------

   plot_heat_comparison = make_plot_pairs(dframe = model_df_stat_filtered,
                   varlabs = list(afternoon = list(var = c("T2M14", "mod_afternoon", "day_2007"),
                                                   main = "Air Temperature / UHI Measure: \n Afternoon"),
                                  night = list(var = c("T2M22", "mod_night", "night_2007"),
                                               main = "Air Temperature / UHI Measure: \n Night"),
                                  morning = list(var = c("T2M04", "mod_afternoon"),
                                                 main = "Air Temperature / UHI Measure: \n Morning")),
                   path_out = "./analysis/figures/"),





    ### HICAM Report --------------------------------------------------------

   # plot_modperformance_predictions <- make_plot_mod_res(
   #     path_model = bam_dbh_filtered[bam_dbh_filtered$model=='mI_spatial_age_x_temp_by_species_lcz6_reBEZIRK_var-day_2007', 'model_file_path'],
   #     file = drake::file_out("./analysis/figures/fig-gam-dbh_temp-day2007_lcz6_obs_pred.png"),
   #     height = 7,
   #     width = 8,
   #     dpi = 300
   # ),


   plot_gam_temp_prediction_genus_comparison_hicam = plot_dbh_temp_single_var_single_species_GG(
       pred_list = pred_data_single_tempvar_fixed_lcz6,
       model_df = model_df_stat_filtered,
       age_filter = c('[45 - 50]', '[75 - 80]', '[90 - 95]'),
       species_filter = NULL,
       # species_filter = c("Tilia cordata", "Tilia platyphyllos"),
       # species_filter = c("Tilia cordata","Platanus acerifolia"),
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 18),


   plot_gam_temp_lcz6_prediction_hicam = plot_dbh_temp_single_var_GG(
       pred_list = pred_data_single_tempvar_fixed_lcz6,
       model_df = model_df_stat_filtered,
       age_filter = NULL,
       age_expression = age_expr,
       prediction_range = "within",
       base_size = 18),



   plot_hicam_temp_effects = plot_hicam_species_overview(plot_ages = plot_gam_temp_lcz6_prediction_hicam,
                          plot_comparison = plot_gam_temp_prediction_genus_comparison_hicam,
                          path_out = drake::file_out("analysis/figures/hicam_report/temp_effects_species_overview.png"),
                          width = 12,
                          height = 15,
                          dpi = 300),



   plot_uhi_map_hicam = make_uhi_plot_hicam(uhi_stacks = uhi_stacks,
                                berlin_poly = berlin_polygons,
                                dsf = full_data_set_clean,
                                base_size = 18),



   plot_gam_temp_prediction_single_genus_lcz6_hicam = plot_dbh_temp_single_var_flex_hicam(pred_list = pred_data_single_tempvar_multi_lcz6,
                                                                              model_df = model_df_stat_filtered,
                                                                              var = "lcz_prop_6",
                                                                              age_filter = c("[45 - 50]", "[60 - 65]", '[75 - 80]', '[90 - 95]'),
                                                                              species_filter = c("Tilia cordata"),
                                                                              # species_filter = c("Tilia cordata","Platanus acerifolia"),
                                                                              age_expression = age_expr,
                                                                              prediction_range = "within",
                                                                              base_size = 18,
                                                                              x_label = expression('Proportional Cover - LCZ6'[bar(150~m)])),
   general_results_lcz_hicam = plot_general_results(uhi_map = plot_uhi_map_hicam,
                                                    obs_pred = plot_obs_pred_single_temp_lcz6,
                                                    env_pred = plot_gam_temp_prediction_single_genus_lcz6_hicam,
                                                    width = 18,
                                                    height = 6,
                                                    dpi = 300,
                                                    file = drake::file_out("analysis/figures/hicam_report/general_effects_overview.png")),


    # tables ------------------------------------------

    overview_table = make_overview_table(full_data_set_clean),

   n_total_obs = nrow(model_df_stat_filtered),

   n_total_species_obs = model_df_stat_filtered %>%
       dplyr::group_by(species_corrected) %>%
       dplyr::count(sort = TRUE) %>%
       dplyr::rename('Species' = 1) %>%
       mutate(Species = paste0('*',Species, '*')),


   # Generate table of genera age distribution

    age_tables = make_age_table(df = full_data_set_clean_with_UHI_covariates,
                                              max_age = 150,
                                              break_interval = 30),










    # Reporting ------------------------------
    paper_html = rmarkdown::render(
        knitr_in("./analysis/paper/paper.Rmd"),
        output_dir = "./analysis/paper/",
        output_file = file_out("paper_knit.html"),
        # output_format = bookdown::html_document2(),
        output_format = "bookdown::html_document2",
        quiet = TRUE
    ),
    paper_word = rmarkdown::render(
        knitr_in("./analysis/paper/paper.Rmd"),
        output_dir = "./analysis/paper/",
        output_file = file_out("Hurley-et-al_UFUG_berlin-trees.docx"),
        # output_file = "./paper_knit.html",
        output_format = "bookdown::word_document2",
        quiet = TRUE
    ),
    # paper_pdf = rmarkdown::render(
    #     knitr_in("./analysis/paper/paper.Rmd"),
    #     output_dir = "./analysis/paper/",
    #     output_file = file_out("paper_knit.pdf"),
    #     output_format = bookdown::pdf_document2(),
    #     quiet = TRUE
    # )


   si_html = rmarkdown::render(
       knitr_in("./analysis/paper/supplemental_information.Rmd"),
       output_dir = "./analysis/paper/",
       output_file = file_out("Hurley-et-al_UFUG_berlin-trees_SI.html"),
       output_format = "bookdown::html_document2",
       quiet = TRUE
   ),

   author_cred_word = rmarkdown::render(
       knitr_in("./analysis/paper/author_statement_credit.Rmd"),
       output_dir = "./analysis/paper/",
       output_file = file_out("author_statement_credit.docx"),
       output_format = "bookdown::word_document2",
       quiet = TRUE
   )



)


