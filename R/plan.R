library(drake)


plan <- drake_plan(
    download_data = target(berlin.trees::download_berlin_trees()),

    berlin_polygons = target(berlin.trees::get_berlin_polygons_as_sf(),
                             trigger = trigger(condition = redownload)),

    tree_data_in_lists = berlin.trees::load_downloaded_data_to_lists(download_data),


    cropped_data_set = berlin.trees::crop_data_with_bbox(tree_data_in_lists,
                                                         bounding_box),


    bounding_box = berlin.trees::make_bbox(52.083962, 52.847599,
                                           12.712024, 14.238359,
                                           "greater_berlin",
                                           crs = 4326),

    full_data_set = berlin.trees::bind_rows_sf(cropped_data_set),

    full_data_set_cleaned = berlin.trees::clean_genus(full_data_set),


    overview_map = berlin.trees::make_overview_map(full_data_set_cleaned,
                                                   berlin_polygons)


    # data = raw_data %>%
    #     mutate(Species = forcats::fct_inorder(Species)),
    # hist = create_plot(data),
    # fit = lm(Sepal.Width ~ Petal.Width + Species, data),
    # report = rmarkdown::render(
    #     knitr_in("report.Rmd"),
    #     output_file = file_out("report.html"),
    #     quiet = TRUE
    # )
)
