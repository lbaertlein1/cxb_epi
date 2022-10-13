pacman::p_load(httr, readr, tidyverse, lubridate, yaml, labelled)

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

#Select asset and download

getKOBO <- function(asset_id = NULL,
                    get_all_cebs = FALSE,
                    get_all = FALSE,
                    with_labels = TRUE){
  if(!is.null(asset_id)){
    l <- kobo_asset_list() %>%
      arrange(desc(as.Date(date_modified, origin=lubridate::origin))) %>%
      select(uid, name) %>%
      filter(!is.na(name) & name != "")
    selection <- l %>% 
      filter(uid %in% asset_id)
    asset_id <- selection$uid
    asset_name <- selection$name
  }
  if(is.null(asset_id) & get_all_cebs == FALSE & get_all == FALSE){
    l <- kobo_asset_list() %>%
      arrange(desc(as.Date(date_modified, origin=lubridate::origin))) %>%
      select(uid, name) %>%
      filter(!is.na(name) & name != "")
    selection <- l %>% 
      filter(name == l$name[utils::menu(l$name, title="Select an Asset:")])
    asset_id <- selection$uid
    asset_name <- selection$name
  }
  if(get_all_cebs == TRUE & get_all == FALSE){
    asset_id <- c("acfGrxnmkTaFvSTdHnrQ3x", "a9bYeVttuVuSzCcgmk9qpo", "a4V5jXFEHjwALe8ztWQNvX", "a6GKjVvR7dyEPeXDtQ9U37", "a5BCfuooic3qN8Y27pR44M")
    l <- kobo_asset_list() %>%
      arrange(desc(as.Date(date_modified, origin=lubridate::origin))) %>%
      select(uid, name) %>%
      filter(!is.na(name) & name != "")
    selection <- l %>% 
      filter(uid %in% asset_id)
    asset_id <- selection$uid
    asset_name <- selection$name
  }
  if(get_all == TRUE){
    selection <- kobo_asset_list() %>%
      arrange(desc(as.Date(date_modified, origin=lubridate::origin))) %>%
      select(uid, name) %>%
      filter(!is.na(name) & name != "")
    asset_id <- selection$uid
    asset_name <- selection$name
  }
  output_list <- list()
  for(i in 1:length(asset_id)){
    data <- kobo_data(kobo_asset(asset_id[i])) %>%
      as.data.frame(.) %>%
      janitor::clean_names(.)
    if(with_labels == FALSE){
      data <- labelled::remove_labels(data)
    }
    output_list <- append(output_list, list(new=data))
    names(output_list)[i] <- asset_name[i]
  }
  return(output_list)
}

