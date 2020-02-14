# EDA for  Urban tree database -------------------------------------------

# # for source
# https://data.nal.usda.gov/dataset/urban-tree-database
# # for overview
# https://www.fs.usda.gov/rds/archive/catalog/RDS-2016-0005
# # for meta
# https://www.fs.usda.gov/rds/archive/products/RDS-2016-0005/_fileindex_RDS-2016-0005.html
# # for download
# https://www.fs.usda.gov/rds/archive/products/RDS-2016-0005/RDS-2016-0005.zip


# download ----------------------------------------------------------------



url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2016-0005/RDS-2016-0005.zip"


udb <- curl::curl_download(url,
    destfile = tempfile(fileext = ".zip")
)

td <- tempdir()

udb_unzip <- unzip(udb, exdir = td)


udb_trees <- udb_unzip[grepl("Raw_tree_data", x = udb_unzip)]
udb_data <- read.csv(udb_trees, header = TRUE, stringsAsFactors = FALSE)
