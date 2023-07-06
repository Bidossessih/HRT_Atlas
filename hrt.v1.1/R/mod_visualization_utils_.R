#' Display table
#'


table_func = function(col1_info, col2_info) {
  tags$tr(tags$td(col1_info),
          tags$td(col2_info))


}


#' Function to display tabpanel
#'
#' @param title is a vector of string indicating the titles
#' @param named list indicating the content of each tabpanel. The list names correspond to the vector of title.
#' @param id is the id of the tab
#' @param gene.symbol selected gene symbol

tabpanel2render = function(id, title, content, gene.symbol){

  #tabpanel
  tabpanelrender = c()

  # tab content

  tabcontent = c()



  for (i in 1:length(title)) {

    if(i==1){
      tabpanelrender0 = HTML(
        str_glue(
          '
          <li class="nav-item" role="presentation">
            <button class="nav-link active" id="{title[i]}-tab" data-bs-toggle="tab"
            data-bs-target="#{title[i]}" type="button" role="tab" aria-controls={title[i]}
            aria-selected="true">{str_to_sentence(title[i])}</button></li>
          '
        )
      )

      ## tab content

      tabcontent0 = str_glue(
        '
        <div class="tab-pane fade show active" id="{title[i]}" role="tabpanel" aria-labelledby="{title[i]}-tab">{content[[title[i]]]}</div>
        '
      )


      tabpanelrender = c(tabpanelrender, tabpanelrender0)
      tabcontent = c(tabcontent, tabcontent0)

      print(content[[title[i]]])

    } else {

      # Tab title

      panel_title = str_replace_all(str_to_sentence(title[i]), "_", " ")

      # Check whether ICC is available

      icc_image_0 = gene2iccImg(gene = gene.symbol)

      print("Yes img_url Yes")
      print(gene.symbol)
      img_url = icc_image_0$imageUrl[1]
      print(img_url)


      # Tab will be disabled if ICC image is not available

      if(panel_title == "Sub cellular" & is.na(img_url)){
        disabled = "disabled"
        tooltip_icc = "tooltip"
        no.icc = "Not available"
      }else{
        disabled = tooltip_icc = no.icc = ""
      }



      tabpanelrender0 = HTML(
        str_glue(

          '
          <li class="nav-item" role="presentation">
            <span class="d-inline-block pe-auto" tabindex="0" data-bs-toggle="{tooltip_icc}" title="{no.icc}">
              <button class="nav-link {disabled}" id="{title[i]}-tab" data-bs-toggle="tab"
                data-bs-target="#{title[i]}" type="button" role="tab" aria-controls={title[i]}
                aria-selected="true">{panel_title}
              </button>
            </span>
          </li>
          '
        )
      )

      ## tab content

      tabcontent0 = str_glue(
        '
        <div class="tab-pane fade show" id="{title[i]}" role="tabpanel" aria-labelledby="{title[i]}-tab">{content[[title[i]]]}</div>
        '
      )

      tabpanelrender = c(tabpanelrender, tabpanelrender0)

      tabcontent = c(tabcontent, tabcontent0)

    }


  }

  tagList(tags$ul(class = "nav nav-tabs",
     id = id,
     role = "tablist",
     #### tabpanel
     HTML(str_c(tabpanelrender, collapse = "\t"))

     ),

     tags$div(class = "tab-content",
         id = str_glue("{id}Content"),

         HTML(str_c(tabcontent, collapse = "\t"))
         )
     )
}

#tabpanel2render(id="idtab", title=c("tab1", "tab2"), content = list(tab1 = "tab1 content", tab2 = "tab2 content"))


#' Function to display ICC and IF images from Human Protein Atlas
#'
#' @param title is a vector of string indicating the titles
#' @param named list indicating the content of each tabpanel. The list names correspond to the vector of title.
#' @param id is the id of the tab


icc_viewer = function(icc_data){

  #tabpanel
  tabpanelrender = c()

  # tab content

  tabcontent = c()

  for (i in 1:nrow(icc_data)) {


    if(i==1){
      tabpanelrender0 = HTML(
        str_glue(
          '
          <li class="nav-item" role="presentation">
            <button class="nav-link active icc-tab-button" id="icc_{i}-tab"
            data-bs-target="#icc_{i}" type="button" role="tab" aria-controls=icc_{i}
            data-bs-toggle="tab" data-bs-html="true" title="{icc_data$cell[1]}"
            aria-selected="true">
            <img src="{icc_data$imageUrl[i]}" alt="{icc_data$cell[i]}" class = "icc-tab-img">
            </button></li>
          '
        )
      )

      ## tab content

      tabcontent0 = str_glue(
        '
        <div class="tab-pane fade show active icc-main-img" id="icc_{i}" role="tabpanel" aria-labelledby="icc_{i}-tab">
          <a href = "{icc_data$imageUrl[i]}" target = "_blank">
            <img src="{icc_data$imageUrl[i]}" class="img-fluid" alt="ICC image">
          </a>
        </div>
        '
      )


      tabpanelrender = c(tabpanelrender, tabpanelrender0)
      tabcontent = c(tabcontent, tabcontent0)



    } else {

      tabpanelrender0 = HTML(
        str_glue(

          '
          <li class="nav-item" role="presentation">
            <button class="nav-link icc-tab-button" id="icc_{i}-tab"
            data-bs-target="#icc_{i}" type="button" role="tab" aria-controls=icc_{i}
            data-bs-toggle="tab" data-bs-html="true" title="<em>{icc_data$cell[i]}</em>"
            aria-selected="true">
             <img src="{icc_data$imageUrl[i]}" alt="{icc_data$cell[i]}"  class = "icc-tab-img">
            </button></li>
          '
        )
      )

      ## tab content

      tabcontent0 = str_glue(
        '
        <div class="tab-pane fade show icc-main-img" id="icc_{i}" role="tabpanel" aria-labelledby="icc_{i}-tab">
          <a href = "{icc_data$imageUrl[i]}" target = "_blank">
            <img src="{icc_data$imageUrl[i]}" class="img-fluid" alt="ICC image">
          </a>
        </div>
        '
      )

      tabpanelrender = c(tabpanelrender, tabpanelrender0)

      tabcontent = c(tabcontent, tabcontent0)

    }


  }

  tagList(tags$ul(class = "nav nav-tabs", style = "border-bottom: none; margin-top: 10px;",
                  id = "icc_tabs",
                  role = "tablist",
                  #### tabpanel
                  HTML(str_c(tabpanelrender, collapse = "\t"))

  ),
  ### ICC
  tags$div(class = "tab-content",
           id = "icc_Content",

           HTML(str_c(tabcontent, collapse = "\t"))
  )
  )
}




#' Function to define render the description of each transcript
#'
#'
#' @param specie human or mouse
#' @param gene.symbol the symbol of selected gene. Default is the default name in selectize
#' @param gene.name is the official name of the seclected gene
#' @param gene.other.names Other known names of the gene. It will appear if not Null
#' @param tissue_info tissues for which the transcript has been proposed
#'  to be a reference transcript
#' @param tissue_info_id id of the div where tissue
#'  ortholog the second specie if the same gene has been idenfity as HK in both
#'  @importFrom stringr str_glue


hk_transcript_desc = function(specie, gene.symbol,
                              gene.name,
                              gene.description,
                              tissue_info_id,
                              gene.other.names,
                              ortholog, trans_selectize){

# Define orthologous gene
  if(specie == "Human") {
    ort_specie = "Mouse"
  } else if (specie == "Mouse") {
    ort_specie = "Human"
  }

  #first column
  ortholog_0 = stringr::str_glue("{ort_specie} orthologous Housekeeping gene")
  #second column. When an ortholog HK is not identified
  if (is.na(ortholog)) {
    ortholog = stringr::str_glue("Not identified as candidate reference gene in {ort_specie}")
  }

  #print("gene.name")
  #print(gene.name)

### Clean gene.name
  gene.name = str_replace(gene.name, "<span", " <span")

  return(

    tags$div(class = "Container-fluid panelstyle",
             tags$br(),
             #Display gene symbol
             h4(str_glue("{gene.symbol}")),
             tags$section(style="border-top:1px #ccc solid; margin-bottom:50px;",
             tags$table(class = "table",

                        tags$tbody(
                          # Gene name row
                          table_func(col1_info = "Gene name", col2_info = HTML(gene.name)),
                          # Other Gene name
                          if(!is.na(gene.other.names)) {
                            table_func(col1_info = "Synonyms",
                                       col2_info = HTML(gene.other.names))
                          },
                          # Orthog Gene if it has been identified in the second specie
                          table_func(col1_info = ortholog_0, col2_info = HTML(ortholog)),
                          # Gene description from RefSeq
                          if(!is.na(gene.description)) {
                            table_func(col1_info = "Description",
                                       col2_info = HTML(gene.description))
                          },
                          # Transcript names and tissue in which it can be used for qpcr
                          #normalization
                          table_func(
                              col1_info = trans_selectize,
                              # the list of tissue with be inserted conditionally from the
                              # visualization module server using the id tissue_info_id
                              col2_info = HTML(str_glue("<div id = '{tissue_info_id}'></div>"))
                            )

                        ))
    )#End of table section
  ))
}


#' Violin Plot
#'
#' @import ggplot2
#' @import ggiraph
#'
#'


violin_bulk_tissue = function(data, width_svg, height_svg){

  data$cell_type = factor(data$cell_type)

  data %>%

  ggplot(aes(x=cell_type, y=log_rpkm, fill=cell_type)) +
    geom_violin_interactive(scale="width",adjust = 1, aes(tooltip=after_stat(density), fill=cell_type)) +
    geom_boxplot_interactive(scale="width",adjust = 1, width = 0.2, color = "white", alpha=0.7) +
    theme_linedraw() +
    theme(legend.position='none',
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) -> gg_violin

  girafe(ggobj = gg_violin,
         options = list(
           opts_sizing(rescale = FALSE)),
         width_svg=width_svg, height_svg = height_svg
         ) -> gg_violin


  girafe_options(
    gg_violin,
    #opts_selection(type = input$opt_selected_data),
    #opts_hover(reactive = input$opt_hover_data),
    opts_selection_key(
      #type = input$opt_selected_key,
      css = girafe_css("stroke:red; stroke-width:2px",
                       text = "stroke:none;fill:red;font-size:12px")
    )
  )

  gg_violin

}




violin_plotly = function(data){

  data$cell_type = factor(data$cell_type)

  data %>%

    ggplot(aes(x=cell_type, y=log_rpkm, fill=cell_type)) +
    geom_violin(scale="width",adjust = 1) +
    geom_boxplot(width = 0.1, color = "white", alpha=0.3) +
    theme_minimal() + coord_flip() +
    theme(legend.position='none',
          axis.text.x = element_text(angle = 120)) -> gg_violin

  plotly::ggplotly(gg_violin)

}




#' Function to reuse the rendering of visualization page
#'
#' @param sel_gene the selected gene or gene name retrieved from url parameter "gene"
#' @param top_ref is the number of top rank of reference transcript. Check the function get_gene_info
#' @param specie string indicating the specie
#' @param ns function. name space of the module

viz.helper = function(sel_gene, top_ref=10, specie, ns, output, input){

  gene_info = get_gene_info(spec = specie, gene = sel_gene, top = top_ref)

  gene.name = gene_info$Name
  gene.description = gene_info$Summary
  # transcript list order by lenght of tissue for which they have been recommended
  # See get_gene_info() definition
  transcript = names(gene_info$default_tissue_norm)

  gene.other.names = gene_info$Synonym
  ortholog = gene_info$Ortholog_symbol


  ### Group button to display transcript information and plot

  trans_button = radioGroupButtons(
    inputId = ns("trans_sel"),
    label = "Ensembl transcript id:",
    choices = transcript,
    direction = "vertical"
  )


  #print(paste("Gene synonym ", str(gene.other.names)))
  ### Render gene summary
  viz = renderUI(tagList(
    # Gene and transcript information
    hk_transcript_desc(
      specie = specie,
      gene.symbol = input$viz_selectize,
      gene.name = gene.name,
      gene.description = gene.description,
      tissue_info_id = ns("ref_tissue_id"),
      gene.other.names = gene.other.names,
      ortholog = ortholog,
      trans_selectize = trans_button
    )


  ))

  # Create a reactive valor of the selected transcript
  ### This value will be updated when the user click on the transcript id from the
  #visualization table(Gene information). See trans_button value above

  sel_transcript_id = NULL
  makeReactiveBinding("sel_transcript_id")

  #tissue_info = NULL
  #makeReactiveBinding("tissue_info")

  observeEvent(input$trans_sel != "", {
    #print("It is working,...")
    sel_transcript_id <<- input$trans_sel

  })



  observe({
    #print("Loading...")
    #Wait for the loading of transcript's Group button

    if(!is.null(sel_transcript_id)){

      tissue_info = gene_info$default_tissue_norm
      print(sel_transcript_id)

      removeUI(
        selector = paste0("#", ns("ref-tissue-id"))
      )


      if(all(is.na(tissue_info[[sel_transcript_id]]))){
        tissue_title = str_glue("{sel_transcript_id} is not among the top{top_ref}
                                  candicate reference transcripts of any tissue.")

        tissue_res = NULL
      }else{
        tissue_title = str_glue("{sel_transcript_id} has been ranked as a top{top_ref}
                                  candicate reference transcripts of the following tissue.")
        # Tissue recommended
        tissue_res = na.omit(tissue_info[[sel_transcript_id]])

        tis = c()

        for(i in 1:length(tissue_res)){
          tis0 = HTML(str_glue("<a href='/?page={str_to_lower(specie)}-housekeeping-gene&tissue={tissue_res[i]}'>
                                 <span class='badge bg-secondary'>{tissue_res[i]}</span></a>"))

          tis = c(tis, tis0)
        }

        tissue_res = HTML(str_c(tis, collapse = "\t"))

      }




      insertUI(
        selector = paste0("#", ns("ref_tissue_id")),
        where = "beforeEnd",
        ui = div(id = ns("ref-tissue-id"),
                 em(tissue_title),
                 br(),
                 br(),
                 div(tissue_res))
      )

    }

  })


  ### Rendering Tissue expression (GTEX)

  bulk_violinplot <- renderPlotly({
    p = plot_ly(
      loadDataTrans(ens_l=sel_transcript_id),
      y = ~ log_rpkm,
      x = ~ cell_type,
      type = "violin",
      hoverinfo = "none",
      split = ~ cell_type,
      box = list(visible = T),
      meanline = list(visible = T),
      textfont = list(color = 'red', size = 10)
    ) %>%
      layout(title = paste0("Expression level of ", sel_transcript_id)) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(
          title = "",
          showticklabels = TRUE,
          tickangle = 60
        ),
        yaxis = list(title = "Normalized Expression \n (log2 RPKM)")
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
          'zoom',
          'pan',
          'select',
          'zoomIn',
          'zoomOut',
          'autoScale',
          'hoverClosestCartesian',
          'hoverCompareCartesian'
        )
      )

    suppressWarnings(p)


  }) # End tissue expression

  output$bulk_violinplot = bulk_violinplot

  output$geneVizTab = renderUI(

    tagList(tabpanel2render(
      id = "idtab",
      gene.symbol = sel_gene,
      #title = c("summary", "tissue", "single_cell", "sub_cellular"),
      title = c("summary", "tissue", "sub_cellular"),
      content = list(
        summary =  div(id = ns("gene_summary")),
        tissue =  div(id = ns("tissue_exp")),
        #single_cell = div(id = ns("sc_exp")),
        sub_cellular = div(id = ns("subcel"))
      )
    )))

  # Insert gene summary


  insertUI(
    selector = paste0("#", ns("gene_summary")),
    where = "beforeEnd",
    ui = div(br(),
             viz
    )
  )


  # Insert tissue expression
  insertUI(
    selector = paste0("#", ns("tissue_exp")),
    where = "beforeEnd",
    ui = div(br(),
             bulk_violinplot
             )
  )


  # insert image
  insertUI(
      selector = paste0("#", ns("subcel")),
      where = "beforeEnd",
      ui = div(mod_icc_if_image_viewer_ui("icc_if_image_viewer_1")
      )
    )





}
