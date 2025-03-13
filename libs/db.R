# packages ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  DBI, dbplyr, dplyr, DT, glue, here, markdown, purrr, RPostgres, stringr, tidyr,
  quiet = T)

is_server <- Sys.info()[["sysname"]] == "Linux"
host <- ifelse(
  is_server,
  "postgis",   # from rstudio to postgis docker container on server
  "localhost") # from laptop to locally tunneled connection to db container on server
# for localhost db, see: https://github.com/calcofi/server#ssh-tunnel-connection-to-postgis-db

# database connect ----
db_pass_txt <- ifelse(
  is_server,
  "/share/.calcofi_db_pass.txt",
  "~/.calcofi_db_pass.txt")
# sudo ln -s /home/bebest/.calcofi_db_pass.txt /root/.calcofi_db_pass.txt
# sudo ln -s /share/.calcofi_db_pass.txt /root/.calcofi_db_pass.txt
stopifnot(file.exists(db_pass_txt))


get_con <- function(schemas = "public"){

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "gis",
    host     = host, 
    port     = 5432,
    user     = "admin",
    password = readLines(db_pass_txt),
    options  = glue("-c search_path={paste(schemas, collapse = ',')}"))
}
  
con          <- get_con()
con_dev      <- get_con(c("dev","public"))
con_dev_only <- get_con("dev")

# test connection ----
# dbListTables(con)

# helper functions ----
glue2 <- function(x, null_str="", .envir = sys.frame(-3), ...){
  # handle NULLs inside glue string as empty character
  null_transformer <- function(str = "NULL") {
    function(text, envir) {
      out <- glue::identity_transformer(text, envir)
      if (is.null(out))
        return(str)
      out }}
  glue(x, .transformer = null_transformer(null_str), .envir = .envir, ...)
}

q <- function(sql){ dbSendQuery(con, sql) }

