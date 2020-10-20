#######################################################	
# Define general layout tag list	
#######################################################

headerTagList <- list(	
    tags$style(type = "text/css", ".navbar .navbar-nav {float: right; font-size: 14px} .navbar .navbar-nav li a {font-size: 14px} .nav-tabs {font-size: 12px}"),
    tags$base(target = "_blank")	
)

#######################################################
# Define the full user-interface
#######################################################

ui <- navbarPage(
    title = strong("Codex Tracking"), selected = "Home",	
    tabPanel("Summary", homepage, icon = icon("home")),	
    #tabPanel("Markers", freezerpage, icon = icon("clipboard")),	
    #tabPanel("About", aboutpage, icon = icon("info-circle")),	
    header = headerTagList,	
    collapsible = TRUE,	inverse = TRUE,
    windowTitle = "Database"
)

shinyUI(ui)