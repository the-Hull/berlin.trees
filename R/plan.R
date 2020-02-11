library(drake)
library(future.callr)
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
        heat_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + provenance + (1 + day_2007 | provenance : ART_BOT)  , data = x)  ,
        # heat_age = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER, data = x)  ,
        # heat_age_species = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + ART_BOT, data = x)  ,
        # heat_age_RIspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT), data = x)  ,
        # heat_age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 | ART_BOT) + provenance, data = x)  ,
        # heat_age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + (1 + day_2007 | ART_BOT), data = x)  ,
        heat_age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | ART_BOT), data = x)  ,
        heat_age_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ day_2007 + STANDALTER + provenance + (1 + day_2007 | provenance : ART_BOT), data = x, control = lme4::lmerControl(optimizer ="Nelder_Mead"))  ,
        # age_only = function(x) lme4::lmer(dbh_cm ~ STANDALTER, data = x),
        # age_RIspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT), data = x),
        # age_RIspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1 | ART_BOT) + provenance, data = x),
        # age_RIspecies_RSspecies = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT), data = x),
        age_RIspecies_RSspecies_provenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | ART_BOT) + provenance, data = x),
        age_RIspecies_RSspecies_RIprovenance = function(x) lme4::lmer(dbh_cm ~ STANDALTER + (1  + day_2007 | provenance : ART_BOT), data = x)
        # species_only = function(x) lme4::lmer(dbh_cm ~ ART_BOT, data = x)
    ),


    model_df = make_test_data_set(full_df = full_data_set_clean,
                                  extract_uhi = extract_uhi_values_to_list),





    model_res = berlin.trees::apply_models(df = model_df,
                             model_list = model_list,
                             n_top_species = 6,
                             min_individuals = 150,
                             list(STANDALTER < 350,
                             krone_m < 50,
                             dbh_cm < 600)),




    # Plotting
    plot_overview_map = berlin.trees::make_overview_map(full_data_set_clean,
                                                        berlin_polygons),

    plot_tree_sums_bar = berlin.trees::tree_sums_bar_plot(full_data_set_clean),

    plot_count_map = berlin.trees::tree_count_map(full_data_set_clean,
                                                  berlin_polygons),


    plot_density = berlin.trees::dens_plot_trees(sf_data = full_data_set_clean,
                                                 extracted_uhi = extract_uhi_values_to_list,
                                                 position_stack = "stack"),


    plot_LME_no_age = make_ranef_plot(model_out = model_res,
                    model_name = "heat_RIspecies_RSspecies_RIprovenance"),


    plot_LME_age = make_ranef_plot(model_out = model_res,
                    model_name = "heat_age_RIspecies_RSspecies_RIprovenance")





# ,
# ART_BOT %in% top_species$ART_BOT[top_species$n > 150]
    # Reporting
    # paper = rmarkdown::render(
    #     knitr_in("./analysis/paper/paper.Rmd"),
    #     output_file = file_out(file.path(here::here(), "paper_knit.html")),
    #     # output_file = file_out(file.path("./paper_knit.html")),
    #     output_format = bookdown::html_document2(),
    #     quiet = TRUE
    # )

)


