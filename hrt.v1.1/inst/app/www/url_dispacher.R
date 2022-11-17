# Define helper functions


get_url_parameters = function(){
  query <- isolate(getQueryString())

  #str(query_name) # Uncomment to debug code

  print(paste0("Myquery is ", query["id"], "\n"))

  fname = isolate(session$clientData$url_search)

  return(list(fname = fname, query = query))

}


#' Check url and return the view
#'
#'
#' @importFrom stringr
#'
#'




url2template = function(fname, query, main_template_path = "template/", temp_var = NULL) {

  #url_list = gsub(".html", "", list.files("template/")) # get list of html files

  query_name = names(query)
  #check if template exists in the main_template_path directory or in a sub-directory
  check_temp = file.exists(stringr::str_glue("template/{query['page']}.html"))


  print(length(query))

  print(paste0("queryurl is ", query["page"]))

  print(paste0("Check is ", check_temp))

  str(query)
  print(stringr::str_glue("Query names are : {query_name}"))
  if(nchar(fname) == 0 || query_name == "homePageGlobal") {

    path_2_template = stringr::str_glue("{main_template_path}index.html")

  } else if(length(query) == 1 & check_temp){

    path_2_template = stringr::str_glue("{main_template_path}{query['page']}.html")

  } else if (length(query) == 1 &
             file.exists(stringr::str_glue("template/{query_name[[1]]}.html")) &
             !(check_temp)) {

    query_name = query_name[[1]]


    path_2_template = stringr::str_glue("{main_template_path}{query_name}.html")

    if(query_name[[1]] == "human-housekeeping-gene/visualization"){
      temp_var = mod_visualization_ui("visualization_1")
    }

  } else {
    path_2_template = stringr::str_glue("{main_template_path}error.html")

  }


  return(tagList(
    # Add template and other variables for the template

    golem_add_external_resources(),

    htmlTemplate(path_2_template,
                 temp_var = mod_visualization_ui("visualization_1"))

  ))

}



# define the url of each page
get_url = function(){

# Get url parameters
  url_par = get_url_parameters()

# Check url and get template

  url2template(fname=url_par[["fname"]],
               query=url_par[["query"]])

}


