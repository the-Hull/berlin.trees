# temp cleaning script

tree_by_dists <- list.files("./analysis/data/raw_data/tree_splits/",
                            pattern = "berlin_trees_subset",
                            full.names = TRUE)



# Charlottenburg -------------------------------------

file_index <- grep("Charlottenburg",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Fhain-Xberg -------------------------------------

file_index <- grep("Friedrichshain",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Lichtenberg -------------------------------------

file_index <- grep("Lichtenberg",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Marzahn -------------------------------------

file_index <- grep("Marzahn",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Mitte -------------------------------------

file_index <- grep("Mitte",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Neuk -------------------------------------

file_index <- grep("Neuk",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Pankow -------------------------------------

file_index <- grep("Pankow",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Reinick -------------------------------------

file_index <- grep("Reinick",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Spandau -------------------------------------

file_index <- grep("Spandau",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Stegl -------------------------------------

file_index <- grep("Stegl",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])




# Tempelhof -------------------------------------

file_index <- grep("Tempelhof",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])




# Trep -------------------------------------

file_index <- grep("Trep",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])

