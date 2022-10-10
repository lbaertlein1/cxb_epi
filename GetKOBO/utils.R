pacman::p_load(httr, readr, tidyverse)

init_kobo_data_struc <- function(folder, base_url, username, password){
  #check to see if folder exists, if not create it
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  #check to see if cache directory exists, if not create it
  cache_dir <- file.path(folder, "cache_dir")
  if(!dir.exists(cache_dir)){
    dir.create(cache_dir)
  }
  #Get Token
  install.packages("remotes")
  remotes::install_gitlab("dickoa/robotoolbox")
  
  library("robotoolbox")
  token <- kobo_token(username = username, password = password, 
                      url = base_url)
  #create specs yaml
  specs_yaml <- file.path(folder,'cache_dir','specs.yaml')
  if(!file.exists(specs_yaml)){
    yaml_out <- list()
    yaml_out$kobo$base_url <- base_url
    yaml_out$kobo$username <- username
    yaml_out$kobo$password <- password
    yaml_out$kobo$token <- token
    yaml_out$kobo_folder <- folder
    write_yaml(yaml_out, specs_yaml)
  }
  Sys.setenv("kobo_folder" = folder)
  Sys.setenv("base_url" = base_url)
  
  kobo_settings()
  
}

## Load parameters from yaml
load_specs <- function(folder = Sys.getenv("kobo_folder")){
  specs <- read_yaml(file.path(folder,'cache_dir','specs.yaml'))
  return(specs)
}


