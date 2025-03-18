# TODO:
# - [ ] logic in server.R d_res seems right, 
#       but not matching https://calcofi.io/larvae-cinms/#prep_taxa_filter,
#       eg scientific: Abralia; itis_tsn: 82406; spccode: 9555; n: 44500
#  

# libraries ----
librarian::shelf(
  bslib, DBI, dplyr, glue, here, htmlwidgets, leaflet, listviewer, purrr, readr, 
  ritis, scales, shiny, 
  dreamRs/shinyWidgets, 
  plotly,
  stringr, 
  # taxadb, taxize, 
  tibble, tidyr,
  quiet = T)
options(readr.show_col_types = F)
# shinyTree::dfToTree()

source(here("libs/db.R")) # con: database connection 

# paths ----
# taxa_csv <- here("../workflows/data/taxa-tree.csv")
taxa_csv <- here("taxa/data/taxa_larvae_long_itis.csv")

# themes ----
light <- bs_theme(
  preset = "flatly",
  base_font = font_google("Playwright+MX"))
dark  <- bs_theme(
  preset = "darkly") |>
  bs_add_rules(
    list(
      ".treejs .treejs-switcher:before { border-top: 4px solid rgba(255, 255, 255, 0.6) !important }"))

# taxa ----

# OLD: taxadb -- download local copy of ITIS taxonomy
# dbpaths <- taxadb::tl_import(
#   provider = "itis",
#   schema   = "dwc")  # Darwin Core
# d_i <- taxadb::taxa_tbl("itis") |>
#   collect() # 899,937 × 15

if (!file.exists(taxa_csv)){
  d_taxa <- dbGetQuery(
    con, 
    "SELECT DISTINCT string_to_table(ltree2text(path), '.')::INT AS tsn FROM taxa_hierarchy") |>
    tibble() # |> 
  # OLD: taxadb
  # left_join(
  #   d_i,
  #   by   = "taxonID") |> 
  # filter(!is.na(kingdom)) |>  # TODO: update with taxize vs taxadb missing hierarchy
  
  print(glue("BEG id2name ~ {Sys.time()}"))
  d_taxa <- d_taxa |> 
    pull(tsn) |> 
    taxize::id2name(db = "itis")  |>   # 6 to 16 min
    "class<-"(NULL) |> 
    bind_rows()
  print(glue("END id2name ~ {Sys.time()}"))
  write_csv(d_taxa, taxa_csv)
}
d_taxa <- read_csv(taxa_csv) |> 
  # TODO: fix invalid
  # pull(status) |> table()
  # invalid   valid 
  #      36    1677
  filter(status == "valid") |> 
  tibble() |> 
  mutate(
    item = glue("<em>{name}</em> ({rank}, <samp>ITIS:{id}</samp>)")) |> 
  arrange(name, rank, id)
# TODO: try building whole tree with only those selected checked
v_taxa <- d_taxa |> 
  select(item, id) |>
  deframe()

# names(d_taxa) |> paste(collapse = ' > ')
# kingdom > phylum > class > order > family > genus > species
# table(d_taxa$kingdom, useNA = "ifany")
# kingdom , phylum <>, class <chr>, order <chr>,
#   family <chr>, genus

# OLD: shinyWidgets::treeInput() ----
# flds_taxa    <- c("kingdom", "subkingdom", "infrakingdom", "superphylum", "phylum", "subphylum", "infraphylum", "superclass", "class", "subclass", "infraclass", "superorder", "order", "suborder", "infraorder", "superfamily", "family", "subfamily", "tribe", "genus", "subgenus", "species", "subspecies", "scientificName")
# # flds_taxa_id <- c(glue('{setdiff(flds_taxa, "scientificName")}_id') |> as.vector(), "taxonID")
# flds_taxa_id <- c(setdiff(flds_taxa, "scientificName"), "taxonID")
# 
# d_taxa <- read_csv(taxa_csv, guess_max = Inf) |> 
#   arrange(!!! flds_taxa) |> 
#   mutate(across(is.character, ~replace_na(., "")))
#   # slice(1:10)
# # View(d_taxa)
# 
# tr_taxa <- create_tree(
#   d_taxa,
#   levels = flds_taxa)
#   # levels_id = flds_taxa_id)
# # names(tr_taxa[[1]])
# # names(tr_taxa[[2]])

# TODO: try this for 
# library(shinyTree)
# library(shiny)
# runApp(system.file("examples/06-search", package = "shinyTree"))
# runApp(here("taxa"))