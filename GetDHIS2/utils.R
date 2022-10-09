

#' Check to see if cache exists, if not create it
#' @param folder A string, the location of the dhis2 data folder
#' @return A string describing the creation process or errors
init_dhis2_data_struc <- function(folder, base_url, username, password){
  #check to see if folder exists, if not create it
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  #check to see if cache directory exists, if not create it
  cache_dir <- file.path(folder, "cache_dir")
  if(!dir.exists(cache_dir)){
    dir.create(cache_dir)
  }
  #create specs yaml
  specs_yaml <- file.path(folder,'cache_dir','specs.yaml')
  if(!file.exists(specs_yaml)){
    yaml_out <- list()
    yaml_out$dhis2$base_url <- base_url
    yaml_out$dhis2$username <- username
    yaml_out$dhis2$password <- password
    yaml_out$dhis2_folder <- folder
    write_yaml(yaml_out, specs_yaml)
  }
  Sys.setenv("dhis2_folder" = folder)
  Sys.setenv("base_url" = base_url)
}

## Load parameters from yaml
load_specs <- function(folder = Sys.getenv("dhis2_folder")){
  specs <- read_yaml(file.path(folder,'cache_dir','specs.yaml'))
  return(specs)
}

## Get Metadata
  ### get metatdata
    getMetadata <- function(metadata_type,
                            base_url = load_specs()$dhis2$base_url,
                            username = load_specs()$dhis2$username,
                            password = load_specs()$dhis2$password){
      url<-paste0(base_url,"api/", metadata_type,"?paging=false&fields=*")
      r<-content(GET(url,authenticate(username,password)),as="parsed")
      return(r[[metadata_type]])
   }

  ### get list of metadata types
  getMetadata_all <- function(base_url = load_specs()$dhis2$base_url,
                              username = load_specs()$dhis2$username,
                              password = load_specs()$dhis2$password){
    
    metadata_types <- content(GET("https://his.oca.msf.org/api/resources", authenticate(username,password)), as="parsed") 
    metadata_types <- do.call(rbind.data.frame, metadata_types$resources) %>%
      filter(!(plural %in% c("oAuth2Clients", "relationships", "trackedEntityInstances", "dataStores", "dataElementOperands",
                           "metadataVersions", "externalFileResources", "users", "fileResources", "icons")))
  
    metadata <- list()
    for(i in 1:nrow(metadata_types)){
      new_metadata <- getMetadata(metadata_type = metadata_types$plural[i])
      metadata <- append(metadata, list(new = new_metadata))
      names(metadata)[i] <- metadata_types$plural[i]
      print(paste0(i, " of ",nrow(metadata_types),": ",metadata_types$plural[i]))
    }
    return(metadata)
  }

#convert time period to list
  datseq <- function(t1, t2) { 
    format(seq(as.Date(t1, "%Y-%m-%d"), 
               as.Date(t2, "%Y-%m-%d"),by="month"), 
           "%Y%m") 
  }
  
getData <- function(data_element_group = "OPD - New Consultations",
                    country = "Bangladesh",
                    projects = NULL,
                    time_period_type = "MONTHS",
                    start_date = "2020-01-01",
                    end_date = Sys.Date(),
                    metadata = metadata,
                    org_unit_hierarchy = 5,
                    relative_time_period = "LAST_12_MONTHS"){
    if(time_period_type == "MONTHS"){
      time_period <- datseq(t1 = start_date,
                             t2 = end_date)
    }
    if(time_period_type == "RELATIVE"){
      time_period <- relative_time_period
    }
  
  if(!is.null(data_element_group)){
  data_elements <-  rbindlist(map(metadata[["dataElementGroups"]], as.data.table), fill = TRUE, idcol = T) %>%
    filter(name == data_element_group) %>%
    select(dataElements) %>%
    unnest(cols=c(dataElements)) %>%
    unnest(cols=c(dataElements))
  }
  if(is.null(data_element_group)){
    data_elements <-  rbindlist(map(metadata[["dataElements"]], as.data.table), fill = TRUE, idcol = T) %>%
      filter(aggregationType == "SUM") %>%
      # filter(!(grepl(paste0(c("Geo-locations","EGEN_","GL_", "EVAC_", " location"), collapse="|"), name, ignore.case=TRUE))) %>%
      # filter((grepl(paste0(c("Geo-locations","EGEN_","GL_", "EVAC_", " location"), collapse="|"), name, ignore.case=TRUE))) %>%
      select(id, name, aggregationType) %>%
      unique() %>%
      # slice(1:5) %>%
      rename(dataElements = id)
  }

  country_id <- rbindlist(map(metadata[["organisationUnits"]], as.data.table), fill = TRUE, idcol = T) %>%
    filter(name == country & level == "3") %>%
    select(id) %>%
    unique()
  
  project_list <-  rbindlist(map(metadata[["organisationUnits"]], as.data.table), fill = TRUE, idcol = T) %>%
    filter(level == "4" & grepl(country_id$id[1], path) & name != "_Geolocations") %>%
    select(name, id) %>%
    unique() 
  
  if(!is.null(projects)){
    project_list <- project_list %>%
      filter(name %in% projects)
  }
  if(is.null(projects)){
    add_all <- project_list %>%
      bind_rows(c(name = "Select All"))
    selection <- add_all[utils::menu(add_all$name, title="Select a Project:")]
    if(selection$name[1] == "Select All"){
      project_list <- project_list
    }
    if(selection$name != "Select All"){
      project_list <- project_list %>%
        filter(name == selection$name[1])
    }
  }
  
  org_unit_list <- rbindlist(map(metadata[["organisationUnits"]], as.data.table), fill = TRUE, idcol = T) %>%
    filter(as.numeric(level) == org_unit_hierarchy & grepl(paste0(project_list$id, collapse="|"), path) & name != "_Geolocations") %>%
    select(name, id) %>%
    unique() 
  
  
   #login to DHIS2
  datimutils::loginToDATIM(
    base_url = load_specs()$dhis2$base_url,
    username = load_specs()$dhis2$username,
    password = load_specs()$dhis2$password
  )
  
  for(i in 1:length(data_elements$dataElements)){
    print(i)
    data <- tryCatch({
    data <- datimutils::getAnalytics(dx = data_elements$dataElements[i],
                                     ou = dput(org_unit_list$id),
                                     pe = dput(time_period),
                                     return_names = TRUE)
    }, error = function(e) {
      data <- NULL
    })
    
    if(i == 1){
      all_data <- data
    }
    if(i != 1){
      all_data <- all_data %>%
        bind_rows(data)
    }
  }
  # data <- datimutils::getAnalytics(dx = dput(data_elements$dataElements),
  #                                  ou = dput(org_unit_list$id),
  #                                  pe = dput(time_period),
  #                                 return_names = TRUE)
  return(all_data)
}

#Possible relative periods: 
# THIS_WEEK, LAST_WEEK, LAST_4_WEEKS, LAST_12_WEEKS, LAST_52_WEEKS,
# THIS_MONTH, LAST_MONTH, THIS_BIMONTH, LAST_BIMONTH, THIS_QUARTER, LAST_QUARTER,
# THIS_SIX_MONTH, LAST_SIX_MONTH, MONTHS_THIS_YEAR, QUARTERS_THIS_YEAR,
# THIS_YEAR, MONTHS_LAST_YEAR, QUARTERS_LAST_YEAR, LAST_YEAR, LAST_5_YEARS, LAST_10_YEARS, LAST_10_FINANCIAL_YEARS, LAST_12_MONTHS,
# LAST_3_MONTHS, LAST_6_BIMONTHS, LAST_4_QUARTERS, LAST_2_SIXMONTHS, THIS_FINANCIAL_YEAR,
# LAST_FINANCIAL_YEAR, LAST_5_FINANCIAL_YEARS