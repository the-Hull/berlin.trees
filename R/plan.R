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





    # Plotting
    plot_overview_map = berlin.trees::make_overview_map(full_data_set_clean,
                                                        berlin_polygons),

    plot_tree_sums_bar = berlin.trees::tree_sums_bar_plot(full_data_set_clean),

    plot_count_map = berlin.trees::tree_count_map(full_data_set_clean,
                                                  berlin_polygons),


    plot_density = berlin.trees::dens_plot_trees(sf_data = full_data_set_clean,
                                                 extracted_uhi = extract_uhi_values_to_list,
                                                 position_stack = "stack")

    # Reporting
    # paper = rmarkdown::render(
    #     knitr_in("./analysis/paper/paper.Rmd"),
    #     output_file = file_out(file.path(here::here(), "paper_knit.html")),
    #     # output_file = file_out(file.path("./paper_knit.html")),
    #     output_format = bookdown::html_document2(),
    #     quiet = TRUE
    # )



)
