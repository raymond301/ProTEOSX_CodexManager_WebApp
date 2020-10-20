homepage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      id = "explorertabs",
      menuItem("Home",
               tabName = "dashboard",
               icon = icon("dashboard")
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "dashboard",
          ),
          tabItem(
            tabName = "tab_extra",
          )
        )
      )
    )
  )
)