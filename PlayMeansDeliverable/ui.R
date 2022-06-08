library(shiny)

# Basic UI
LoginPage = fluidPage(
  textInput(inputId = "client", label="Enter Credentials", placeholder="client"),
  textInput(inputId = "secret", label="", placeholder="secret"),
  actionButton(inputId="send_creds", label="Login to Spotify"),
  textOutput(outputId="login_status")
)

DefaultPage = fluidPage(
  textOutput("default_message")
)

DataPage = fluidPage(
  textOutput(outputId="song_status"),
  DT::dataTableOutput("song_data")
)

DistributionPage = fluidPage(
  uiOutput("dist_page")
)

KPage = fluidPage(
  uiOutput("k_page")
)

KSelectPage = fluidPage(
  uiOutput("k_select_page")
)

navbarPage("Playmeans",
           tabPanel("Login", LoginPage),
           tabPanel("Data", DataPage),
           tabPanel("Distributional Analysis", DistributionPage),
           tabPanel("K-Means", KPage),
           tabPanel("K Selection",KSelectPage)
           )


