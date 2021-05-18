library(dplyr
        )

# temp cleaning script

tree_by_dists <- list.files("./analysis/data/raw_data/tree_splits/",
                            pattern = "berlin_trees_subset",
                            full.names = TRUE)



# # Charlottenburg -------------------------------------
#
# file_index <- grep("Charlottenburg",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
# # Fhain-Xberg -------------------------------------
#
# file_index <- grep("Friedrichshain",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
# # Lichtenberg -------------------------------------
#
# file_index <- grep("Lichtenberg",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
# # Marzahn -------------------------------------
#
# file_index <- grep("Marzahn",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
#
# # Mitte -------------------------------------
#
# file_index <- grep("Mitte",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
#
# # Neuk -------------------------------------
#
# file_index <- grep("Neuk",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
#
# # Pankow -------------------------------------
#
# file_index <- grep("Pankow",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
# # Reinick -------------------------------------
#
# file_index <- grep("Reinick",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
# # Spandau -------------------------------------
#
# file_index <- grep("Spandau",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
# # Stegl -------------------------------------
#
# file_index <- grep("Stegl",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
#
#
# # Tempelhof -------------------------------------
#
# file_index <- grep("Tempelhof",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])
#
#
#
#
# # Trep -------------------------------------
#
# file_index <- grep("Trep",
#                    tree_by_dists)
#
#
# datacleanr::dcr_app(dframe = tree_by_dists[file_index])






# BY GENUS ----------------------------------------------------------------

# temp cleaning script

tree_by_dists <- list.files("./analysis/data/raw_data/tree_splits/",
                            pattern = "berlin_trees_subset",
                            full.names = TRUE)


# grab genera
genera <- sub(pattern = "(.*_)(\\w+[.]RDS$)",
    replacement = "\\2",
     x = tree_by_dists,
     perl = FALSE) %>%
    fs::path_ext_remove()



# Acer --------------------------------------------------------------------

genera[1]

file_index <- grep("Acer",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Aesculus --------------------------------------------------------------------

genera[2]

file_index <- grep("Aesculus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Betula --------------------------------------------------------------------

genera[3]

file_index <- grep("Betula",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Carpinus --------------------------------------------------------------------

genera[4]

file_index <- grep("Carpinus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Fraxinus --------------------------------------------------------------------

genera[5]

file_index <- grep("Fraxinus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Other --------------------------------------------------------------------

genera[6]

file_index <- grep("Other",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Pinus --------------------------------------------------------------------

genera[7]

file_index <- grep("Pinus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Platanus --------------------------------------------------------------------

genera[8]

file_index <- grep("Platanus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])



# Populus --------------------------------------------------------------------

genera[9]

file_index <- grep("Populus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Prunus --------------------------------------------------------------------

genera[10]

file_index <- grep("Prunus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Quercus --------------------------------------------------------------------

genera[11]

file_index <- grep("Quercus",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])


# Robinia --------------------------------------------------------------------

genera[12]

file_index <- grep("Robinia",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])

# Tilia --------------------------------------------------------------------

genera[13]

file_index <- grep("Tilia",
                   tree_by_dists)


datacleanr::dcr_app(dframe = tree_by_dists[file_index])

