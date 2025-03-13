library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  rx <- reactiveValues(
    d_taxa = NULL)

  output$json_tree <- renderJsonedit({

    req(input$sel_taxa)

    # flds_taxa_id <- c(glue('{setdiff(flds_taxa, "scientificName")}_id') |> as.vector(), "taxonID")
    # flds_taxa_id <- c(setdiff(flds_taxa, "scientificName"), "taxonID")
    # input <- list(sel_taxa = c(165547, 82696))
    # d_taxa |> 
    #   filter(id %in% input$sel_taxa) # Ablennes (Genus; ITIS:165547); Arthropoda (Phylum; ITIS:82696)

    # taxa_str <- paste(input$sel_taxa, collapse = "|")
    # d0 <- d <- dbGetQuery(
    #   con, glue("
    #     SELECT tsn::INT, scientific_name, taxon_rank, path::TEXT
    #       FROM taxa_hierarchy
    #       WHERE path ~ '*.{taxa_str}.*'")) |>
    #   tibble()
    
    get_taxa_nodes <- function(itis_tsn){
      dbGetQuery(
        con,
        # https://stackoverflow.com/questions/43510596/collect-all-leaf-nodes-with-postgres-ltree
        glue("WITH a AS (
           -- get all nodes containing taxa_str in their path
           SELECT tsn::INT, scientific_name, taxon_rank, path  -- path::TEXT
             FROM taxa_hierarchy
             WHERE path ~ '*.{itis_tsn}.*')
          -- subset to all leaf end nodes
           SELECT *
             FROM a AS a1
             WHERE NOT EXISTS (
               SELECT 1
                 FROM a AS a2
                 WHERE
                    a1.path <> a2.path AND
                    a1.path @> a2.path )")) |>
        tibble()
    }
    
    # get nodes for selected taxa
    rx$d_taxa <- tibble(
      sel_tsn = as.integer(input$sel_taxa)) |> 
      left_join(
        d_taxa |> 
          select(id, name, rank, status, item), 
        by = c("sel_tsn" = "id")) |>
      mutate(
        nodes = map(sel_tsn, get_taxa_nodes))
    
    # display taxa nodes per selected taxa
    rx$d_taxa |> 
      mutate(
        nodes = map(nodes, \(x){ 
          split(x, 1:nrow(x)) 
        })) |> 
      split(1:nrow(rx$d_taxa)) |> 
      jsonedit()
    # TODO: rename path to path_itis
    # TODO: add drop-down of taxonomic authority: ITIS, WoRMS, GBIF, COL, ...; add path_* to species table
    
    # if (nrow(d) == 0)
    #   stop("TODO: what does 0 rows mean?")
    # 
    # if (nrow(d) == 1) {
    # 
    #   d_tr <- d |>
    #     select(tsn, taxon_rank, scientific_name) |>
    #     mutate(
    #       level = glue("{scientific_name} ({taxon_rank}; {tsn})"))
    # 
    #   
    #   rx$sel_taxa <- list(
    #     tsn = d$tsn,
    #     level = d$level)
    #   browser()
    # 
    #   tsn_selected <- d$tsn
    #   tr_taxa <- create_tree(
    #     d_tr,
    #     levels    = "level",
    #     levels_id = "tsn")
    # } else {
    # 
    #   str_tsns <- paste(d$tsn, collapse = ",")
    #   # get least common ancestor (lca)
    #   root <- dbGetQuery(
    #     con,
    #     glue("SELECT lca(array_agg(path))::TEXT AS lca FROM taxa_hierarchy WHERE tsn::INT IN ({str_tsns})")) |>
    #     pull(lca)
    #   # lca() misses one extra level, per [Ltree.lca has confusing behavior wrt to the lca function provided by the ltree extension in postgres · Issue #468 · kvesteri/sqlalchemy-utils](https://github.com/kvesteri/sqlalchemy-utils/issues/468)
    #   # append one level deeper
    #   # root      = "202423.914154.914156.158852.331030.914179.161061.161105.166082.167640.172249"
    #   # d$path[1] = "202423.914154.914156.158852.331030.914179.161061.161105.166082.167640.172249.172250"
    #   # root <- as.character(d$path[1]) |> str_replace(glue("({root}\\.[0-9]+).*"), "\\1")
    # 
    #   ranks_keep <- c("kingdom", "phylum", "class", "order", "family", "genus", "species") # , "species")
    # 
    #   z <- d |>
    #     mutate(
    #       path = str_replace(path, glue("({root})\\.(.*)"), "\\2")) |>
    #     select(tsn_target = tsn, tsn = path) |>
    #     separate_longer_delim(tsn, delim = ".") |>  # 66 × 2
    #     distinct(tsn_target, tsn) |>
    #     mutate(
    #       tsn = as.integer(tsn)) |>
    #     left_join(
    #       d_taxa |>
    #         select(tsn = id, name, rank) |>
    #         mutate(rank = tolower(rank)),
    #       by = "tsn") |>
    #     filter(
    #       rank %in% ranks_keep |
    #         tsn_target == tsn) |>
    #     filter(
    #       !is.na(name),
    #       !is.na(rank)) |> # rm 98056; why name/rank == NA
    #     mutate(
    #       tsn_target = as.character(tsn_target),
    #       tsn        = as.character(tsn))
    # 
    #   ranks_tr <- intersect(ranks_keep, unique(z$rank)) # orde
    #   w <- z |>
    #     mutate(
    #       name = glue("{name} ({rank}; {tsn})")) |>
    #     pivot_wider(
    #       names_from  = rank,
    #       values_from = c(name, tsn),
    #       names_glue  = "{rank}_{.value}") |>
    #     select(-tsn_target)
    # 
    #   # i <- which(is.na(w$genus_tsn))
    #   # w$genus_tsn[i] <- glue("NAgenus{i}")
    #   # i <- which(is.na(w$species_tsn))
    #   # w$species_tsn[i] <- glue("NAspecies{i}")
    # 
    #   # browser()
    #   tr_taxa <- create_tree(
    #     w,
    #     levels    = glue("{ranks_tr}_name"),
    #     levels_id = glue("{ranks_tr}_tsn"))
    #   # listviewer::jsonedit(tr)
    # 
    #   # View(z)
    #   # d_tr <- z |>
    #   #   arrange(!!! flds_tr) |>
    #   #   mutate(across(is.character, ~replace_na(., "")))
    # 
    #   get_n <- function(x){length(unique(x))}
    #   n_d_tsn        <- get_n(d$tsn)
    #   n_z_tsn_target <- get_n(z$tsn_target)
    #   n_z_tsn        <- get_n(z$tsn)
    #   n_w_tsn_target <- get_n(w$tsn_target)
    #   message(glue("
    #     n_d_tsn:        {n_d_tsn}
    #     n_z_tsn_target: {n_d_tsn}
    #     n_z_tsn:        {n_z_tsn}
    #     n_w_tsn_target: {n_w_tsn_target}"))
    # 
    #   # Galatheidae (family)
    #   # 97964
    #   # 97964 %in% unique(z$tsn)
    #   # children are NA
    #   # z |> filter(tsn_target == 98056)
    #   #   tsn_target   tsn name         rank
    #   # 1      98056 89787 Malacostraca class
    #   # 2      98056 95599 Decapoda     order
    #   # 3      98056 97964 Galatheidae  family
    #   # 4      98056 98056 NA           NA
    #   # https://itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=98056
    #   # Pleuroncodes planipes (98056; invalid) -> Grimothea planipes (1202386; valid)
    #   # d_taxa |> filter(id == 98056)
    #   # d_taxa |> filter(status != "valid")
    # 
    #   tsn_selected <- unique(z$tsn_target)
    #   message(glue("selected: {paste(tsn_selected,collapse = ', ')}"))
    # 
    # }
    # 
    # treeInput(
    #   "sel_tree",
    #   label       = "Taxa",
    #   choices     = tr_taxa,
    #   selected    = tsn_selected,
    #   returnValue = "id",
    #   closeDepth  = 2)

  })
  
  # map ----
  output$map <- renderLeaflet({
    
    leaflet() |> 
      addProviderTiles(
        providers$Esri.OceanBasemap) |>
      setView(-122.4, 33.8, 4)
  })
  
  observe({
  
    req(rx$d_taxa)
    # TODO:
    # - table: species_codes_new -> species_codes
    # - column: species_codes_new.itis_tsn convert to integer
    
    # sel_tree: 165548 # species
    # sel_taxa: 165547 # genus
    # message(glue("sel_taxa: {paste(input$sel_taxa, collapse = ', ')}"))
    # message(glue("sel_tree: {paste(input$sel_tree, collapse = ', ')}"))
  
    # sel_tsns <- as.integer(input$sel_taxa)
    # sel_tsns <- input$sel_tree
    # q_tsns <- paste0("'", paste(input$sel_taxa, collapse = "','"), "'")
    # tbl(con, "species_codes_new") |> 
    #   filter(itis_tsn %in% !!sel_tsns) |>
    
    # saveRDS(rx$d_taxa,     "_d_taxa_debug.rds")
    # saveRDS(session$token, "_token_debug.rds")
    # rx <- list(d_taxa = readRDS("taxa/_d_taxa_debug.rds"))
    
    d <- rx$d_taxa |> 
      unnest(nodes) |> 
      mutate(
        tsn = as.character(tsn)) # TODO: fix db
    
    tbl_tmp_taxa <- glue("pg_temp.taxa_session_{session$token}")
    tbl_tmp_taxa_local <- glue("taxa_session_{session$token}")
    
    # tbl_tmp_taxa <- "pg_temp.taxa_session_be735dfcfab8f46c4ebce11c64e30e68" # DEBUG
    # session <- list(token = "be735dfcfab8f46c4ebce11c64e30e68")   # DEBUG
    
    if (dbExistsTable(con, tbl_tmp_taxa_local)){
      copy_to(con, d, name = tbl_tmp_taxa_local, overwrite = T)
    } else {
      copy_to(con, d, name = I(tbl_tmp_taxa))
    }
    
    d_res <- tbl(con, I(tbl_tmp_taxa)) |> 
      # collect() |> 
      # names() |> 
      # paste(collapse = ", ")
      # select(sel_tsn, name, rank, status, item, tsn, scientific_name, taxon_rank, path) |>
      select(
        sel_tsn, sel_scientific_name = name, sel_rank = rank, sel_item_html = item, 
        itis_tsn = tsn, taxon_rank) |>
      left_join(
        tbl(con, "species_codes_new") |> 
          # select(id, spccode, scientific_name, itis_tsn, common_name, taxon_rank)
          select(itis_tsn, spccode, scientific_name, common_name),
        by = "itis_tsn") |> 
      left_join(
        tbl(con, "larvae_counts") |> 
          # select(netid, spccode, tally),
          select(netid, spccode, n_larvae = tally),
        by = "spccode") |>
      left_join(
        tbl(con, "nets") |> 
          # select(towid, netid, netside, shf, volsampled, propsorted),
          # TODO: use fields (netside, shf?, volsampled, propsorted) to calculate density: n_larvae / liter
          select(towid, netid, volsampled, propsorted),
        by = "netid") |>
      left_join(
        tbl(con, "tows") |> 
          # select(towid, stationid, towtype, townumber, starttime),
          select(towid, stationid, starttime),
        by = "towid") |>
      inner_join(
        tbl(con, "stations") |> 
          # select(stationid, cruise_id, orderocc, latitude, longitude, line, station, gebco_depth, geom),
          select(stationid, latitude, longitude, line, station, geom),
        by = "stationid") |> 
      group_by(sel_scientific_name, sel_item_html, line, station) |> 
      summarize(
        latitude   = mean(latitude, na.rm=T),
        longitude  = mean(longitude, na.rm=T),
        n_larvae   = sum(n_larvae, na.rm=T),
        n_samples  = n(),
        volsorted  = sum(volsampled * propsorted, na.rm=T),
        date_beg   = min(as.Date(starttime), na.rm = T),
        date_end   = max(as.Date(starttime), na.rm = T),
        .groups = "drop") |> 
      collect()
    
    if (nrow(d_res) == 0){
      message(glue("{paste(rx$d_taxa$name, collapse=', ')}: 0 results"))
      return(NULL)
    }
    
    d_res <- d_res |> 
      mutate(
        radius = rescale(log10(n_larvae), c(5, 20)))
    # range(d_res$radius)
    
    # q = glue("SELECT * FROM species_codes_new WHERE itis_tsn IN ({q_tsns})") %>%
    #   DBI::dbGetQuery(con, .) |>
    # tbl(con, "species_new") |>
    # filter(itis_tsn %in% sel_tsns) |>
    #   collect()
      # larvae_counts.netid -> nets.netid -> nets.towid -> tows.towid
    
      # leafletProxy("map") |>
    
    # previewColors(colorFactor("Set3", d_res$sel_scientific_name), unique(d_res$sel_scientific_name))
    sci_names <- sort(unique(d_res$sel_scientific_name))
    pal <- colorFactor("Dark2", sci_names) # RColorBrewer::display.brewer.all()
    # previewColors(colorFactor("Dark2", sci_names), sci_names)
    
    # TODO: convert d_res to sf and zoom to extent
    leafletProxy("map") |> 
      clearShapes() |> 
      addCircleMarkers(
        data        = d_res,
        group       = "larvae",
        lng         = ~longitude,
        lat         = ~latitude,
        radius      = ~radius, # default: 10, ~n_larvae,
        color       = "black",
        opacity     = 0.6,
        weight      = 0.5,
        fillColor   = ~pal(sel_scientific_name),
        fillOpacity = 0.4,
        popup       = ~glue(
          "<b>{sel_scientific_name}</b>:<br>
          line, station: {line}, {station}<br>
          longitude, latitude: {longitude}, {latitude}<br>
          n_larvae: {n_larvae}<br>
          n_samples: {n_samples}<br>
          volsorted: {volsorted}<br>
          date range: {date_beg} to {date_end}")) |> 
      clearControls() |>
      addLegend(
        position = 'topright',
        colors   = pal(sci_names), 
        labels   = sci_names,
        group    = "larvae")
    
    # {table}.spccode:
    # - egg_counts.netid
    # - egg_species
    # - krill_abundances
    # - larvae_counts.netid -> nets.netid -> nets.towid -> tows.towid
    # - larvae_species
    # - species_codes
    # - species_groups
    # taxa_hierarchy.id
})

  # observe changes in selected taxa and send message to console
  # observeEvent(input$sel_tree, {
  #   message(glue("sel_tree: {paste(input$sel_tree, collapse = ', ')}"))
  # })

}
