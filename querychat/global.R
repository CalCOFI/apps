# global.R - querychat app for CalCOFI database ----

# libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  bslib, bsicons, DBI, dbplyr, digest, dplyr, DT, duckdb, ggplot2, glue,
  htmltools, leaflet, leaflet.extras, plotly, querychat, readr, shiny,
  shinyjs, stringr, tidyr,
  quiet = T)
options(readr.show_col_types = F)

# database connection ----
# connect to remote DuckDB database via httpfs
duckdb_url <- "https://file.calcofi.io/data/calcofi.duckdb"

# create a local connection with httpfs enabled for remote access
con <- dbConnect(
  duckdb(),
  dbdir = ":memory:")

# install and load httpfs extension for remote file access
dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

# attach the remote database as read-only
dbExecute(con, glue("ATTACH '{duckdb_url}' AS calcofi (READ_ONLY);"))

# get list of tables from the attached database
tbls <- dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_catalog = 'calcofi'")$table_name

# table descriptions ----
d_tbls <- tibble::tribble(
  ~table_name,       ~description,
  "species_summary", "** QUERYABLE ** species with aggregated larva/egg counts (used by chat)",
  "observations",    "joined view of species, nets, tows, sites, cruises (for advanced queries)",
  "cruise",          "unique cruises by ship and year-month",
  "egg",             "number of eggs from net tow by species",
  "egg_stage",       "egg stage from net tow by species",
  "larva",           "number of larvae from net tow by species",
  "larva_size",      "size of larvae from net tow by species",
  "larva_stage",     "stage of larvae from net tow by species",
  "net",             "individual net from given tow",
  "ship",            "unique ships with many cruises",
  "species",         "unique species (or taxa) found in egg or larva net tows",
  "site",            "unique location (longitude, latitude) and time along cruise",
  "tow",             "unique net tows",
  "tow_type",        "unique types of tows",
  "grid",            "CalCOFI grid cells for spatial aggregation",
  "site_seg",        "segments between consecutive sites during cruise",
  "cast",            "CTD cast metadata",
  "bottle",          "CTD bottle samples with oceanographic measurements")

# field definitions (for documentation) ----
d_flds <- tibble::tribble(
  ~table_name,   ~field_name,       ~type,       ~description,
  # species_summary (queryable view)
  "species_summary", "species_id",      "smallint",  "CalCOFI species identifier",
  "species_summary", "scientific_name", "varchar",   "scientific name of the species",
  "species_summary", "common_name",     "varchar",   "common name of the species",
  "species_summary", "itis_id",         "integer",   "ITIS taxonomic serial number",
  "species_summary", "worms_id",        "integer",   "WoRMS AphiaID",
  "species_summary", "total_larvae",    "bigint",    "total count of all larvae observations",
  "species_summary", "larva_records",   "bigint",    "number of larva observation records",
  "species_summary", "total_eggs",      "bigint",    "total count of all egg observations",
  "species_summary", "egg_records",     "bigint",    "number of egg observation records",
  # cruise
  "cruise",      "cruise_uuid",     "uuid",      "unique cruise identifier",
  "cruise",      "date_ym",         "date",      "year-month of cruise",
  "cruise",      "ship_key",        "varchar",   "ship identifier, foreign key to ship table",
  # egg
  "egg",         "net_uuid",        "uuid",      "net identifier, foreign key to net table",
  "egg",         "species_id",      "smallint",  "CalCOFI species identifier, foreign key to species table",
  "egg",         "tally",           "integer",   "raw count of eggs",
  # egg_stage
  "egg_stage",   "net_uuid",        "uuid",      "net identifier",
  "egg_stage",   "species_id",      "smallint",  "species identifier",
  "egg_stage",   "stage",           "smallint",  "developmental stage",
  "egg_stage",   "tally",           "integer",   "raw count of eggs by stage",
  # larva
  "larva",       "net_uuid",        "uuid",      "net identifier",
  "larva",       "species_id",      "smallint",  "species identifier",
  "larva",       "tally",           "integer",   "raw count of larvae",
  # larva_size
  "larva_size",  "net_uuid",        "uuid",      "net identifier",
  "larva_size",  "species_id",      "smallint",  "species identifier",
  "larva_size",  "length_mm",       "decimal",   "total length of the larvae (mm)",
  "larva_size",  "tally",           "integer",   "raw count of larvae at this size",
  # larva_stage
  "larva_stage", "net_uuid",        "uuid",      "net identifier",
  "larva_stage", "species_id",      "smallint",  "species identifier",
  "larva_stage", "stage",           "varchar",   "developmental stage: yolksac (YOLK), preflexion (PREF), flexion (FLEX), postflexion (POST), or transformation (TRNS)",
  "larva_stage", "tally",           "integer",   "raw count of larvae by developmental stage",
  # net
  "net",         "net_uuid",        "uuid",      "unique net identifier",
  "net",         "tow_uuid",        "uuid",      "tow identifier, foreign key to tow table",
  "net",         "side",            "varchar",   "net location side",
  "net",         "std_haul_factor", "decimal",   "standard haul factor (SHF) for abundance standardization",
  "net",         "vol_sampled_m3",  "decimal",   "volume of water sampled by the net (m³)",
  "net",         "prop_sorted",     "decimal",   "proportion of net sample sorted in laboratory",
  "net",         "smallplankton",   "decimal",   "small plankton volume (ml)",
  "net",         "totalplankton",   "decimal",   "total plankton volume (ml)",
  # ship
  "ship",        "ship_key",        "varchar",   "unique ship code",
  "ship",        "ship_name",       "varchar",   "ship name",
  "ship",        "ship_nodc",       "varchar",   "NODC ship code",
  # species
  "species",     "species_id",      "smallint",  "CalCOFI species identifier",
  "species",     "scientific_name", "varchar",   "scientific name",
  "species",     "itis_id",         "integer",   "ITIS taxonomic serial number",
  "species",     "worms_id",        "integer",   "WoRMS AphiaID",
  "species",     "common_name",     "varchar",   "common name",
  # site
  "site",        "site_uuid",       "uuid",      "unique site identifier",
  "site",        "cruise_uuid",     "uuid",      "cruise identifier",
  "site",        "orderocc",        "smallint",  "order of occurrence in cruise",
  "site",        "latitude",        "decimal",   "latitude in decimal degrees (WGS84)",
  "site",        "longitude",       "decimal",   "longitude in decimal degrees (WGS84)",
  "site",        "line",            "decimal",   "CalCOFI coordinate - alongshore component",
  "site",        "station",         "decimal",   "CalCOFI coordinate - offshore component",
  "site",        "grid_key",        "text",      "grid cell identifier",
  # tow
  "tow",         "tow_uuid",        "uuid",      "unique tow identifier",
  "tow",         "site_uuid",       "uuid",      "site identifier",
  "tow",         "tow_type_key",    "varchar",   "tow type code",
  "tow",         "tow_number",      "smallint",  "tow number at site",
  "tow",         "time_start",      "timestamp", "start time of tow",
  # tow_type
  "tow_type",    "tow_type_key",    "varchar",   "tow type code",
  "tow_type",    "description",     "varchar",   "description of tow type")

# theme ----
theme <- bs_theme(
  preset    = "flatly",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Roboto Slab")) |>
  bs_add_rules("
    .card-header { font-weight: 600; }
    .nav-pills .nav-link.active { background-color: #2C3E50; }
    pre.sql-display {
      background-color: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 0.25rem;
      padding: 1rem;
      font-size: 0.875rem;
    }
    .erd-container { padding: 1rem; }
    .table-info { margin-bottom: 0.5rem; }
  ")

# querychat configuration ----
# create views for remote tables in local connection for querychat
# use double quotes to handle reserved words like "cast"
for (tbl in tbls) {
  dbExecute(con, glue('CREATE OR REPLACE VIEW "{tbl}" AS SELECT * FROM calcofi.main."{tbl}"'))
}

# create a comprehensive view joining key tables for querychat
# this allows natural language queries across related data
dbExecute(con, '
  CREATE OR REPLACE VIEW observations AS
  SELECT
    -- species info
    sp.species_id,
    sp.scientific_name,
    sp.common_name,
    sp.itis_id,
    sp.worms_id,
    -- counts
    l.tally AS larva_count,
    e.tally AS egg_count,
    -- net info
    n.net_uuid,
    n.std_haul_factor,
    n.vol_sampled_m3,
    n.totalplankton,
    -- tow info
    t.tow_uuid,
    t.time_start,
    tt.description AS tow_type,
    -- site info
    s.site_uuid,
    s.latitude,
    s.longitude,
    s.line,
    s.station,
    -- cruise info
    c.cruise_uuid,
    c.date_ym AS cruise_date,
    sh.ship_name
  FROM "species" sp
  LEFT JOIN "larva" l ON sp.species_id = l.species_id
  LEFT JOIN "egg" e ON sp.species_id = e.species_id AND l.net_uuid = e.net_uuid
  LEFT JOIN "net" n ON COALESCE(l.net_uuid, e.net_uuid) = n.net_uuid
  LEFT JOIN "tow" t ON n.tow_uuid = t.tow_uuid
  LEFT JOIN "tow_type" tt ON t.tow_type_key = tt.tow_type_key
  LEFT JOIN "site" s ON t.site_uuid = s.site_uuid
  LEFT JOIN "cruise" c ON s.cruise_uuid = c.cruise_uuid
  LEFT JOIN "ship" sh ON c.ship_key = sh.ship_key
  WHERE l.tally IS NOT NULL OR e.tally IS NOT NULL
')

# also create a simpler species summary view
dbExecute(con, '
  CREATE OR REPLACE VIEW species_summary AS
  SELECT
    sp.species_id,
    sp.scientific_name,
    sp.common_name,
    sp.itis_id,
    sp.worms_id,
    COALESCE(l.total_larvae, 0) AS total_larvae,
    COALESCE(l.larva_records, 0) AS larva_records,
    COALESCE(e.total_eggs, 0) AS total_eggs,
    COALESCE(e.egg_records, 0) AS egg_records
  FROM "species" sp
  LEFT JOIN (
    SELECT species_id, SUM(tally) AS total_larvae, COUNT(*) AS larva_records
    FROM "larva"
    GROUP BY species_id
  ) l ON sp.species_id = l.species_id
  LEFT JOIN (
    SELECT species_id, SUM(tally) AS total_eggs, COUNT(*) AS egg_records
    FROM "egg"
    GROUP BY species_id
  ) e ON sp.species_id = e.species_id
')

# helper functions ----
get_erd_mermaid <- function() {
"erDiagram
    SHIP ||--o{ CRUISE : has
    CRUISE ||--o{ SITE : contains
    SITE ||--o{ TOW : has
    TOW }o--|| TOW_TYPE : type
    TOW ||--o{ NET : uses
    NET ||--o{ EGG : samples
    NET ||--o{ LARVA : samples
    EGG }o--|| SPECIES : identifies
    LARVA }o--|| SPECIES : identifies
    SITE }o--o| GRID : within

    SHIP {
        varchar ship_key PK
        varchar ship_name
    }
    CRUISE {
        uuid cruise_uuid PK
        date date_ym
        varchar ship_key FK
    }
    SITE {
        uuid site_uuid PK
        uuid cruise_uuid FK
        decimal latitude
        decimal longitude
    }
    TOW {
        uuid tow_uuid PK
        uuid site_uuid FK
        varchar tow_type_key FK
    }
    NET {
        uuid net_uuid PK
        uuid tow_uuid FK
    }
    EGG {
        uuid net_uuid FK
        smallint species_id FK
        integer tally
    }
    LARVA {
        uuid net_uuid FK
        smallint species_id FK
        integer tally
    }
    SPECIES {
        smallint species_id PK
        varchar scientific_name
        varchar common_name
    }
    GRID {
        text grid_key PK
    }"
}

# sample questions for the chat interface ----
sample_questions <- c(
  "How many unique species are in the database?",
  "What are the top 10 species by total larvae count?",
  "Show species that have both egg and larva observations",
  "What species have an ITIS ID but no WoRMS ID?",
  "List all species with 'anchovy' in the common name",
  "What are the top 5 species by total egg count?",
  "Show me species with more than 1000 larva records",
  "Which species have the highest ratio of eggs to larvae?")

# create QueryChat instance ----
# must be after views are created
qc <- QueryChat$new(
  con,
  "species_summary",
  client   = "anthropic/claude-sonnet-4-5",
  greeting = paste0(
    "Welcome to the CalCOFI Database Explorer! ",
    "Ask me questions about CalCOFI species data including larvae and egg counts. ",
    "I can help you find information like species names, common names, taxonomic IDs, and observation counts. ",
    "Try questions like: 'What are the top 10 species by total larvae count?' or 'Show species with both egg and larva observations.'"))

# cleanup on app stop ----
onStop(function() {
  if (exists("con") && !is.null(con)) {
    tryCatch({
      dbDisconnect(con, shutdown = TRUE)
    }, error = function(e) {
      message("Error disconnecting from database: ", e$message)
    })
  }
})
