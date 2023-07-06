#' Function to display selectize input
#'
#' @param id is the id of each selectize
#' @param selectize.title string to provide a title
#' @param items vector of options. The first item will be used as default.
#' @import RSQLite
#' @export


display_selectize = function(id, items, seleted.gene = NULL, selectize.title = NULL) {

  if(is.null(seleted.gene)){
    seleted.gene = ""
  }

  selectInput(id, label = selectize.title, choices = items, selected = seleted.gene, selectize=TRUE)
}


#' Function to display a banner in small device
#'
#'

smDeviceBanner = function(message = "default") {
  # text to be shown
  if (message == "default") {
    message = "Please use this page on your desktop (at least 768px wide). You can dismiss this banner,
              but certain features might not work correctly."
  }


  div(class = "modal",
      tabindex = "-1",
      div(class = "modal-dialog",
          div(
            class = "modal-content",
            div(
              class = "modal-header",
              h5(class = "modal-title",
                 "Modal title"),
              tags$button(
                type = "button",
                class = "btn-close",
                "data-bs-dismiss" = "modal",
                "aria-label" = "Close"
              )
            ),
            div(class = "modal-body",
                p(message)),
            div(
              class = "modal-footer",
              tags$button(
                type = "button",
                class = "btn btn-secondary",
                "data-bs-dismiss" = "modal",
                "Close"
              ),
              tags$button(type = "button",
                     class = "btn btn-primary",
                     "Save changes")
            )
          )))

}

#' Fech visualization data
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr %>% filter pull tbl collect
#'


db_path = "/mnt/d/Repository_ubuntu/Housekeeping/HRT_Atlas_db/HRT_Atlas_human_mouse_v1_2022.sqlite"

get_HK_genes = function(spec){
  con <- dbConnect(RSQLite::SQLite(), db_path)

  tbl(con, "HK_gene_transcript_info") %>%
    filter(specie == spec) %>%
    pull(gene_name) %>%
    sort() %>% na.omit() %>% unique() -> gene
  dbDisconnect(con)
  return(gene)

}

#' @param top interger indicating the max rank of a transcript in a tissue
#' Only tissue in which the transcript have been ranked up to top will be listed as recommended tissue
#' See visualization


get_gene_info = function(spec, gene, top = 10){
  con <- dbConnect(RSQLite::SQLite(), db_path)
#get gene info
  geneI = tbl(con, "HK_gene_transcript_info") %>%
    filter(specie == spec, gene_name == gene) %>%
    select(gene_name, Name, Summary, Ortholog_symbol, Synonym, ens_transcript_id)# %>% collect()
  #Get transcript info to enable filtering based on the number of tissue in which
  #the transcripts have been identified as relevant reference transcript

  cellI = tbl(con, "Tissue_Info_ref_transcripts_qpcr") %>%
    filter(specie == spec, gene_name == gene) %>%
    select(cell_type, ens_transcript_id, mean, rank) #%>% collect()

  #merge information
  dplyr::left_join(x = geneI, y = cellI, by = "ens_transcript_id") %>%
    collect() -> gene_info


  dbDisconnect(con)

  Name = gene_info$Name[1]
  Suma = gene_info$Summary[1]
  Ortholog_symbol = gene_info$Ortholog_symbol[1]
  ens_transcript_id = unique(gene_info$ens_transcript_id)
  Synonym = gene_info$Synonym[1]

  results = list(
    Name = Name,
    Summary = Suma,
    Ortholog_symbol = Ortholog_symbol,
    ens_transcript_id = ens_transcript_id,
    Synonym = Synonym
  )

  #Include only tissue in which the transcripts have mean rkpm >=30 (default setting)
  #only these tissues will be shown in the recommendation section. See visualization module
  #NA is include to avoid error when the gene didn't have any recommended candidate reference gene
  data = gene_info[,6:9] %>% filter(rank <= top | is.na(rank) )

  #change cell_type in NA if rank is NA

  data$cell_type = ifelse((is.na(data$rank) | rlang::is_empty(data$rank)),
                          NA,
                          data$cell_type)

  data = unique(data[,c(1:2,4)])

  default_tissue_norm = list()

  for (i in 1:length(results[["ens_transcript_id"]])) {
    trans = results[["ens_transcript_id"]][i]
    data_s = data %>% filter(ens_transcript_id == trans)
    transL = unique(as.character(data$cell_type[which(data$ens_transcript_id==trans)]))

    default_tissue_norm[[stringr::str_glue("{trans}")]] = transL

  }
  results[["default_tissue_norm"]] = default_tissue_norm

  # Order by lenght of default_tissue_norm

  results[["default_tissue_norm"]][order(sapply(results[["default_tissue_norm"]], length), decreasing = TRUE)]


  return(results)

}

#gene_info = get_gene_info(spec="Human", gene="AES", top=10)

#gene_info = get_gene_info(spec="Human", gene="EEF2", top=10)

#gene_info = get_gene_info(spec="Human", gene="AARS", top=10)


get_tissue = function(ens_id){
  con <- dbConnect(RSQLite::SQLite(), db_path)

  tbl(con, "Tissue_Info_ref_transcripts_qpcr") %>%
    filter(ens_transcript_id == ens_id) %>%
    pull(cell_type) %>% unique() -> tissue

  dbDisconnect(con)

  return(tissue)

}


#' Function to retrieve ICC image
#'
#' @importFrom dplyr %>% filter pull tbl collect
#' @import DBI RSQLite

gene2iccImg = function(gene){

  con <- dbConnect(RSQLite::SQLite(), db_path)

  if(!is.null(gene)){
    print("Gene from fun")
    print(gene)

    tbl(con, "HPA_image") %>%
      filter(gene_name == gene) %>%
      select(gene_name, cell, imageUrl) %>% collect() %>% unique() -> imgData
  }else{
    imgData = NULL
  }



  dbDisconnect(con)

  return(imgData)
}

#gene = "EEF2"
#' Load expression data of specific transcript
#'
#' @param ens_l vector of Ensembl transcript id


loadDataTrans <- function(ens_l) {
  #transform the vector ens_l in string suitable for inclusion in the SQL statement
  transString <- toString(sprintf("'%s'", ens_l))
  # Connect to the database
  db <- dbConnect(RSQLite::SQLite(), db_path)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM HK_log_rpkm WHERE ens_transcript_id IN (%s)", transString)
  # Submit the fetch query and disconnect
  data <- RSQLite::dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

#' Accordion
#'
#'
#'

