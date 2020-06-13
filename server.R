#Inpiration
#https://github.com/open-meta/uiStub/blob/master/app/app.R


SUSPENDWHENHIDDEN = FALSE

function(input, output, session, clientData) {
  cat("Session started.\n")                               # this prints when a session starts
  onSessionEnded(function() {cat("Session ended.\n\n")})  # this prints when a session ends
  
  # build same menu for all pages
  
  output$uihousekeeping <- renderUI(tagList(
    
    fluidPage(style="padding-left: 0px;padding-right: 0px;"#,
              #fluidRow(includeHTML("www/mainpage.html"))
              ),
    uiOutput("pageHousekeeping", style="margin-left: 10px;margin-right: 10px;")
    
    )                                          
  )
  
  # load server code for page specified in URL
  validFiles = c( list.files("External")                          # valid files must be hardcoded here
                 )                     #    for security (use all lower-case
  #    names to prevent Unix case problems)
  fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
  if(nchar(fname)==0) { fname = "?homePageGlobal" }              # blank means home page
  fname = paste0(substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"
  
  cat(paste0("Session filename: ", fname, ".\n"))      # print the URL for this session
  
  if(!fname %in% validFiles){                          # is that one of our files?
    output$pageHousekeeping <- renderUI(tagList(              # 404 if no file with that name
      fluidRow(
        column(5,
               HTML("<h2>404 Not Found Error:</h2><p>That URL doesn't exist. Use the",
                    "menu above to navigate to the page you were looking for.</p>")
        )
      )
    ))
    return()    # to prevent a "file not found" error on the next line after a 404 error
  }
  source(paste0("External/",fname), local=TRUE)                            # load and run server code for this page
  
  # Add code to head tag
  #fname2 = gsub(".R", "H.R", fname)
  #output$outx <- renderPrint(fname2)
  
  #User head tag as selector
  #if (nchar(fname2)!=0) {
   # singleton(insertUI("head", "afterBegin",
    #         source(paste0("External/headCode/", fname2))[1]))
    #}
  
}


#########################################
#######################################



