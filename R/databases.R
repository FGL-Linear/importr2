#' Build connection string for Lida 500 database
#'
#' @param db_file_path path to database file (.mdb)
#'
#' @return A connection string for Lida 500 database.
#' @export
l500_con_strings <- function(db_file_path){

  keys <- keyring::key_list()

  if(!("L500" %in% keys$service)){
    keyring::key_set("L500", prompt = "Lida 500 database password:")
  }

  dbq_string <- paste0("DBQ=", db_file_path)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  cred_string <- paste0("Pwd=", keyring::key_get("L500"), ";")
  paste0(driver_string, cred_string, dbq_string)
}


#' Connect to an instrument database
#'
#' @param db_file_path path to database file (.mdb)
#'
#' @return An S4 object that inherits from DBI::DBIConnection-class.
#' @export
connect_to_l500_dbi <- function(db_file_path = "D:/Analyzer.mdb")  {
  # make sure that the file exists before attempting to connect
  if (!file.exists(db_file_path)) {
    stop("DB file does not exist at ", db_file_path)
  }

  myconn <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = l500_con_strings(db_file_path),
    timeout = 10
  )

  return(myconn)
}

#' @param user username (for now, "fguerrero")
#' @rdname connect_to_l500_dbi
connect_to_sk_dbi <- function( user = "fguerrero" ){

  keys <- keyring::key_list()

  if(!("sekisui" %in% keys$service)){
    keyring::key_set("sekisui", prompt = "Sekisui database password:")
  }

  DBI::dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-NVDH6AU\\SQLEXPRESS,1433",
                 Database = "48i",
                 UID = user,
                 PWD = keyring::key_get("sekisui"))
}

#' @rdname connect_to_l500_dbi
connect_to_kr_dbi <- function(db_file_path = "D:/OppLocal.mdb")  {
  # make sure that the file exists before attempting to connect
  if (!file.exists(db_file_path)) {
    stop("DB file does not exist at ", db_file_path)
  }

  dbq_string <- paste0("DBQ=", db_file_path)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)

  myconn <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = db_connect_string,
    timeout = 10
  )

  return(myconn)
}
