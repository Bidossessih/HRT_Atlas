#Inpiration
#https://github.com/open-meta/uiStub/blob/master/app/app.R


SUSPENDWHENHIDDEN = FALSE

function(input, output, session, clientData) {
  cat("Session started.\n")                               # this prints when a session starts
  onSessionEnded(function() {cat("Session ended.\n\n")})  # this prints when a session ends
  
  # build same menu for all pages
  
  output$uihousekeeping <- renderUI(tagList(
    
    
    
    htmlTemplate(paste0("www/HTML/", fname),
                 #Results page to html template
                 homePageHumanResult = source("External/homePageHumanResult.R", local=TRUE)[1],
                 homePageMouseResult = source("External/homePageMouseResult.R", local=TRUE)[1],
                 modifiers = source("External/modifiersUI.R", local=TRUE)[1],
                 visualizationHuman = source("External/visualizationUI.R", local=TRUE)[1],
                 visualizationMouse = source("External/visualizationMouseUI.R", local=TRUE)[1]
    )
    
    )                                          
  )
  
  # load server code for page specified in URL
  validFiles = c( list.files("www/HTML")                          # valid files must be hardcoded here
  )
  
  validFiles2 = c( list.files("External/ServerCode")                          # valid files must be hardcoded here
                 )                     #    for security (use all lower-case
  #    names to prevent Unix case problems)
  fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
  if(nchar(fname)==0) { fname = "?homePageGlobal" }              # blank means home page
  fname = paste0(substr(fname, 2, nchar(fname)), ".html") # remove leading "?", add ".R"
  
  cat(paste0("Session filename: ", fname, ".\n"))      # print the URL for this session
  
  
  if (gsub("html", "R", fname) %in% validFiles2) {
    cat("reponse: ", paste0("External/ServerCode", gsub("html", "R", fname)), "\n")
    source(paste0("External/ServerCode/", gsub("html", "R", fname)), local=TRUE)                            # load and run server code for this page
  }
  cat(paste0("Filename: ", fname, ".\n"))
  cat("line 47", "\n")
  
  
  if(!fname %in% validFiles2){                          # is that one of our files?
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
  
  
  
}


