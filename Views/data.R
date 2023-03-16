datapage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar( ),
  dashboardBody(
    tabsetPanel(id="tabs",
                tabPanel("By Project", value='tab1',
                         DTOutput('AnyProjectTable'),
                         hr(),
                         uiOutput("ProjectPickerForData"),
                         downloadButton("downloadData", label = "Download Project"),
                         br(),
                         DTOutput('ProjectROICountTable')
                        
                ),
                tabPanel("By Date Range", value='tab1',
                         h4("Under construction")
                )
    )
    #DTOutput('FullDataTable')
  )
)