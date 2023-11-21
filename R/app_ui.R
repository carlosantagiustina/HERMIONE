#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import golem
#' @import shiny
#' @import shinyWidgets
#' @import bs4Dash
#' @import echarts4r
#' @import waiter
#' @import fresh
#' @import DT
#' @import bslib
#' @import visNetwork
#' @import base64enc
#' @import tidyverse
#' @import shinyjs
#' @import textyle
#' @import ipc
#' @import future
#' @import promises
#' @import httr
#' @import tidytext
#' @import ggplot2
#' @noRd

#### UI PARAMETERS ####

#### THEME ####
my_theme = fresh::create_theme(
  fresh::bs4dash_vars(
    navbar_light_color = "#ffd100",
    navbar_light_active_color = "#ffd100",
    navbar_light_hover_color = "#ffd100"
  ),
  fresh::bs4dash_yiq(
    contrasted_threshold = 50
    ,
    text_dark = "#000"
      ,
    text_light = "darkgray"
  ),
  fresh::bs4dash_layout(main_bg = "#ffd100"),
  fresh::bs4dash_sidebar_light(
    bg = "#fff" ,
    color = "#000",
    hover_color = "#BF616A",
    submenu_bg = "#adb5bd",
    submenu_color = "#ffc107",
    submenu_hover_color = "#BF616A"
  ),
  fresh::bs4dash_status(
    primary = "#adb5bd",
    info   = "#ffc107",
    danger = "#BF616A",
    light = "#272c30"
  )
  #,fresh::bs4dash_color(# gray_900 = "#FFF")
  )

  #### TOAST OPTIONS ####
  # toast options
  toastOpts <- list(
    autohide = TRUE,
    icon = "fas fa-home",
    close = FALSE,
    position = "bottomRight"
  )

  #### APP UI ####
  app_ui <- function(request){
    tagList(
      useShinyjs(),
      # Leave this function for adding external resources
      golem_add_external_resources(),
      tags$link(rel="stylesheet",
                type="text/css",
                href="www/css/custom.css"),
    tags$head(tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")),
      # Your application UI logic
      bs4Dash::dashboardPage(
        #useShinyjs(),
        preloader = list(
          html = tagList(
            waiter::spin_half(),
            br(),
            tags$img(src = 'https://muhai.org/images/banners/muhai.svg', width = "50%",style="border: 20px solid transparent;"),
            br(),
            br(),
            HTML(
              "MUHAI welcomes you to HERMIONE<br> The Inequality Observatory is Loading.<br> Please be patient."
            )
          ),
          color = "#ffd100"
        ),
        dark = FALSE,
        help = TRUE,
        #freshTheme = my_theme,
        fullscreen = TRUE,
        scrollToTop = TRUE,
        #### UI - HEADER ####
        header = bs4Dash::dashboardHeader(
          fixed = TRUE,
          border = TRUE,
          compact = FALSE,
          controlbarIcon = tags$img(src = 'www/hermione/0_smile.png',
                                    width = "30px"),
          sidebarIcon = tags$img(src = 'https://muhai.univiu.org/images/headers/MUHAI_favicon.png',
                                 width = "30px"),
          title = tags$a(
            href = 'https://muhai.org/',
            tags$img(src = 'https://muhai.org/images/banners/muhai.svg',
                     width = "100%",style="border: 15px solid transparent;")
          ),
          bookmarkButton(label = "Bookmark HERMIONE state"),#class = "mx-2"
          # popover(
          #   title = "Bookmark",
          #   content = "This button allows you to bookmark the current state of HERMIONE and its parameters in a single URL. You can copy this URL and paste it later in your browser to reinitialize the dashboard at a specific state",
          #   placement = "bottom",
          #   # `data-trigger` = "hover",
          #   bookmarkButton(label = "Bookmark HERMIONE state")#class = "mx-2"
          # ),
          leftUi = tagList(
            # dropdownMenu(
            #   badgeStatus = "info",
            #   type = "notifications",
            #   notificationItem(
            #     inputId = "triggerAction2",
            #     text = "Error!",
            #     status = "danger"
            #   )
            # ),
            # dropdownMenu(
            #   badgeStatus = "info",
            #   type = "tasks",
            #   taskItem(
            #     inputId = "triggerAction3",
            #     text = "My progress",
            #     color = "orange",
            #     value = 30
            #   )
            # )
            )
        )
        ,
        ##### UI - SIDEBAR #####
        sidebar = bs4Dash::dashboardSidebar(
          fixed = TRUE,
          skin = "light",
          status = "primary",
          id = "sidebar",
          # customArea = shiny::fluidRow(
          #   actionButton(
          #     inputId = "AppSidebarButton",
          #     label = NULL,
          #     icon = icon("users"),
          #     width = NULL,
          #     status = "primary",
          #     style = "margin: auto",
          #     bs4Dash::dashboardBadge(shiny::textOutput("btnVal"), color = "danger")
          #   )
          # ),
          #HTML('<center style="background-color:#ffd100;">HERMIONE<br>Inequality Observatory</center>'),
          ###### Sidebar User Panel ######
          sidebarUserPanel(image = 'https://muhai.univiu.org/images/headers/MUHAI_favicon.png',
                           name = "HERMIONE"),
          ###### Sidebar Menu ######
          sidebarMenu(
            id = "current_tab",
            flat = FALSE,
            compact = FALSE,
            childIndent = TRUE,
            sidebarHeader("Toolsets"),
            ###### Sidebar Items ######
            menuItem("Introduction",
                     tabName = "intro",
                     icon = icon("key")),
            menuItem(
              "Bird's-eye Network",
              tabName = "DO",
              icon = shiny::icon("map")
            ),
            menuItem(
              "Fine Grained View",
              tabName = "NF",
              icon = shiny::icon("binoculars")
            ),
            menuItem(
              "Case studies",
              tabName = "cases",
              icon = shiny::icon("map-signs")
            ),
            menuItem(
              "Methods & Refs.",
              tabName = "methods",
              icon = shiny::icon("graduation-cap")
            ),
            menuItem(
              "Info & Contacts",
              tabName = "info",
              icon = icon("info-circle")
            )
          )
        ),
        ##### UI - BODY #####
        body = bs4Dash::dashboardBody(
          ###### Load body theme ######
          use_theme(my_theme),
          #      e_theme_register(echarts_my_theme$options, name = echarts_my_theme$name),
          ###### BODY ITEMS ######
          tabItems(
            ####### Body item 1: intro #######
            tabItem(
              tabName = "intro",
              fluidRow(
                ######## Value Boxes ########
                valueBox(
                  value = h4("62,015"),
                  subtitle = "Sample of tweets about inequality",
                  color = "primary",
                  href = "https://developer.twitter.com/en/use-cases/do-research",
                  icon = icon("twitter"),
                  #footer = div(paste0("latest update: ", Sys.Date()))
                ),
                valueBox(
                  value = h4("ML⋅NLP⋅FCG⋅KG"),
                  subtitle = "Combined MUHAI methods",
                  href = "https://www.muhai.org/output",
                  color = "indigo",
                  icon = icon("gears")
                ),
                valueBox(
                  value = h4("9,243,293"),
                  subtitle = "Semantic triples in the OKG",
                  href = "https://dml.uni-bremen.de/muhai/kg.html",
                  color = "danger",
                  icon = icon("info"),
                  #footer =  div(paste0("latest update: ", Sys.Date()))
                ),
                valueBox(
                  value = h4("3,592"),
                  subtitle = "Entities linked to the DBpedia KG",
                  color = "teal",
                  href = "https://www.dbpedia.org/",
                  icon = icon("link"),
                 # footer =  div(paste0("latest update: ", Sys.Date()))
                )
              ),
              fluidRow(column(
                width = 12,
                tabBox(
                  ribbon(text = "STEP 0",
                         color = "white"),
                  title = "Brief introduction to HERMIONE",
                  elevation = 2,
                  id = "tabcard1",
                  width = 12,
                  collapsible = TRUE,
                  closable = TRUE,
                  solidHeader = TRUE,
                  type = "tabs",
                  status = "primary",
                  selected = "Where am I?",
                  tabPanel(
                    "Where am I?",
                    #<center><img src='https://drive.google.com/uc?id=1MbPV2M-kS3LleRKZlpqr9IsvZeksV6Uo' alt='ImageInequality' style='width:400px;'></center><br>
                    {
                      div(
                      HTML(
                      "<body><center><h1>Welcome to MUHAI's HERMIONE dashboard!</h1><br><img src='www/inequality.png' alt='ImageInequality' style='width:25%;'></center><br><br></h4></body>"),
                      fluidRow(HTML(
                        "<h4> Join us as we explore the data, events, narratives, and perspectives that can enhance our understanding of intersectionality and the multifaceted nature of inequality in our contemporary world.</h4>"
                      )),
                      br(),
                      fluidRow(
                        column(width = 6,
                               div(
                              infoBox(width = 12,title = "",icon =icon(  "users"),value = HTML(
                                 "<h4> Inequalities in society can take many forms - from income and wealth disparities, to discrimination based on ethnicity, gender, age, religion and other factors.</h4>"
                               ),color = "gray"))),
                        column(width = 6,
                               HTML(
                                 "<center><img src='www/ineq_intro_chat.png' alt='InequalityIntro' style='width:40%;'></center>")),style="vertical-align: middle;"),
                      br(),
                      fluidRow(
                        column(width = 6,
                               HTML(
                                 "<center><img src='www/ineq_intro_cut.png' alt='InequalityIntroCut' style='width:45%;'></center>")),
                        column(width = 6,
                               div(
                                 infoBox(width = 12,title = "",icon =icon(  "unlock"),value = HTML(
                                   "<h4>  These inequalities can have a profound impact on individuals and communities, shaping opportunities and outcomes in ways that can be difficult to see or understand.</h4>"
                                 ),color = "warning"))),style="vertical-align: middle;"),
                      br(),
                      fluidRow(
                        column(width = 6,
                               div(
                                 infoBox(width = 12,title = "",icon =icon(  "search"),value = HTML(
                                   "<h4> Through this interactive dashboard, we aim to bring greater visibility to these issues by providing a platform for exploring and understanding perceptions of inequality in our society.<br> By understanding how people perceive and experience inequality, we can gain insight into the ways in which it shapes our society and the potential for solutions to address it.</h4>"
                                 ),color = "gray"))),
                        column(width = 6,
                               HTML(
                                 "<center><img src='www/ineq_intro_obs.png' alt='InequalityObs' style='width:50%;'></center>")),style="vertical-align: middle;")
                      ,style="vertical-align: middle;")
                      }
                  ),
                  tabPanel(
                    "What is HERMIONE?",
                    HTML(
                      "<h4>HERMIONE is a platform that uses data from social media and other online sources to analyze and understand patterns and trends in human behavior, opinion and communication. Web observatories that analyzes social media data can be used to track and measure the spread of information and opinion on online platforms, such as social media, as well as to understand how people interact with each other online.<br><br>
Web observatories, like MUHAI's Social Inequality Observatory, can be used for a variety of purposes, including monitoring public opinion, tracking the spread of misinformation and disinformation, and identifying emerging narratives and discussion patterns in social media. For example, a web observatory that analyzes social media data can be used to track the spread of misinformation about a particular topic, such as a disease outbreak, and to understand the factors that contribute to its spread.<br><br>
Additionally, web observatories can be used by researchers, citizens, businesses, and organizations to gain insights into human behavior and public opinion, as well as to identify key influencers and opinion leaders in a particular field, such as inequality.</h4>"
                    )
                  ),
                  tabPanel(
                    "Why inequality?",
                    HTML(
                      "<h4>Analysing inequality perception through social media is important for several reasons:
<br>
1. Social media provides a vast amount of data that can be used to understand how people perceive and experience inequality in their daily lives. This can provide a more complete and nuanced understanding of the issue than is possible through traditional surveys or interviews.
<br><br>
2. Social media contain insights into the ways in which inequality is discussed and represented online, and how this changes over time. This can help to identify emerging issues and trends in inequality perception and to understand how these are shaped by events, social norms, cultural values, and other factors.
<br><br>
3. Social media can be used to identify the voices and perspectives that are most influential in shaping public opinion on inequality. This can help to identify key opinion leaders and influencers and to understand the role they play in shaping public discourse on the issue.
<br><br>
4. Social media can be used to identify the ways in which different groups of people experience and perceive inequality. This can help to uncover the diversity of experiences of inequality and to challenge stereotypes and generalizations.
<br><br>
5. Social media can also be used to identify the way in which individuals and groups challenge inequalities all over the world. This can help to identify potential solutions and inform policy and practice.</h4>"
                    )
                  )
                )
              ))
            ),
            ######Body item 2: DO - Bird Eye View ######
            tabItem(tabName = "DO",
                    fluidRow(
                      box(
                        title = h1("Bird's-Eye Network"),
                        closable = F,
                        maximizable = TRUE,
                        collapsible = TRUE,
                        width = 12,
                        solidHeader = FALSE,
                        div(
                        fluidRow(
                          sortable(
                            width = 12,
                            bs4Dash::box(
                              title = "  BE network visualization & exploration",
                              icon =icon( "map"),
                              width = 12,
                              collapsed = F,
                              fluidRow(column(width = 1),
                                       column(width = 8,textInput(inputId = "entityfilter", "Entity in Tweet RegEx filter", value = "", width = NULL, placeholder = "insert the names of one or more entities separated by | (e.g.,Obama|Trump)")),
                                       column(width = 1)
                              ),
                              fluidRow(
                                column(width = 3,
                                       downloadButton('downloadNetwork_BE',
                                                      label = "Download",class = "mx-2",
                                                      icon = shiny::icon("download"),width = "100px")
                                ),
                                column(width = 3,
                                       actionButton(inputId = "runBE",
                                                    label = "Run",
                                                    icon = shiny::icon("gears"),
                                                    class = "mx-2",
                                                    width = "100px",
                                                    status="danger")
                                       ),
                                column(width = 3,
                                       actionButton(inputId = "controlbarToggleBE",
                                                    label = "Control",
                                                    icon = shiny::icon("sliders"),
                                                    class = "mx-2",
                                                    width = "100px",
                                                    status = "warning")
                              ),
                              column(width = 3,
                                     actionButton(inputId = "infobarToggleBE",
                                                  label = "Info",
                                                  icon = shiny::icon("info"),
                                                  class = "mx-2",
                                                  width = "100px",
                                                  status="info")
                            )
                            ),
                                  visNetworkOutput("BEresult",
                                                   height = "650px",
                                                   width = "100%"),
                            fluidRow(
                              column(width = 5,
                                     uiOutput(outputId = "FG_entity_1")),
                              column(width = 5,
                                     uiOutput(outputId = "FG_entity_2")),
                              column(width = 2,
                                     actionButton(inputId = "confirm_ent",
                                                  label = "  Confirm entities and go to FG view",
                                                  icon = shiny::icon("binoculars"),
                                                  #class = "mx-2",
                                                  #width = "250px",
                                                  status="danger"))
                            )
                            ),
                            bs4Dash::box(
                              title = "Sample of Tweets matching Query",
                              width = 12,collapsed = F,
                              HTML("<b>Select a node in the BE network here above to see a sample of posts referring to that entity</b><i> (Gray square: posts from accounts that don't exist any more or posts that are hidden for privacy and GDPR compliance; Not found: posts that have been deleted by their author)</i>"),
                              #div(uiOutput("render_tweets_sample")),
                              div(fluidRow(uiOutput("render_tweet1_sample"),
                                           uiOutput("render_tweet2_sample"),
                                           uiOutput("render_tweet3_sample"),
                                           uiOutput("render_tweet4_sample"),
                                           uiOutput("render_tweet5_sample"),
                                           uiOutput("render_tweet6_sample"),
                                           uiOutput("render_tweet7_sample"),
                                           uiOutput("render_tweet8_sample"),
                                           uiOutput("render_tweet9_sample"),
                                           uiOutput("render_tweet10_sample"),
                                           uiOutput("render_tweet11_sample"),
                                           uiOutput("render_tweet12_sample")
                                           ))
                              #, footer = "Advanced parameters for tweeking HERMIONE"
                            ),
                            bs4Dash::box(
                              title = "SPARQL Queries Log",
                              width = 12,collapsed = T,
                              div(uiOutput("sparqlBEqueryURL"),style="font-size: 12px;")
                              #, footer = "Advanced parameters for tweeking HERMIONE"
                            )
                            # ,bs4Dash::box(width = 12,
                            #         title = "Selected Nodes and Edges",collapsed = T,
                            #         {
                            #        div(HTML("<br>Selected node:<br>")
                            #       ,verbatimTextOutput("return_BE_node")
                            #       ,HTML("Selected edge:<br>")
                            #       ,verbatimTextOutput("return_BE_edge")
                            #         )}
                            #
                            #
                            #         )
                            ,color="primary")
                            )),
                          sidebar = boxSidebar(
                            startOpen = FALSE,
                            icon = icon( "info"),
                            id = "BE_info",
                            background = "darkgray"
                               ,fluidRow(
                                 {
                                   tags$div(HTML(
                                     "<h3>Network description and navigation information</h3><br>The bird's-eye network is constructed from the co-occurrence of both the noun-phrase entities and consolidated KG entities retrieved from the filtered Twitter OKG. <br><br>The co-occurrences of entities in tweets' sentences can be used to highlight the role the entities play in the narratives surrounding inequality.<br><br>  How to interpret the network visualization:"),
                                     tags$ul(
                                       tags$li(tags$span("Node size is proportional to the number of occurrences of the entity")),
                                       tags$li(tags$span("Edge width is proportional to the number of occurrences of the entity")),
                                       tags$li(tags$span("The shape depends on the type of entity"))),
                                     uiOutput("tweet_example")
                                     )
                                 }

                             )
                          )
                        #)#check here
                      ))),
                    ######Body item 3: NF - Fine Graned Analysis ######
            tabItem(tabName = "NF",
                    fluidRow(
                      box(
                        title = h1("Fine Grained View"),
                        closable = F,
                        maximizable = TRUE,
                        collapsible = TRUE,
                        width = 12,
                        solidHeader = FALSE,
                        div(
                          fluidRow(
                            sortable(
                              width = 12,
                              bs4Dash::box(
                                title = "  FG network visualization & exploration",
                                icon =icon( "binoculars"),
                                width = 12,
                                collapsed = F,
                                fluidRow(
                                  column(width = 6,
                                         uiOutput(outputId = "FG_entity_11")),
                                  column(width = 6,
                                         uiOutput(outputId = "FG_entity_22"))
                                ),
                                fluidRow(
                                  column(width = 3
                                         # ,downloadButton('downloadNetwork',
                                         #                label = "Download",class = "mx-2",
                                         #                icon = shiny::icon("download"),width = "100px")
                                  ),
                                  column(width = 3
                                         , actionButton(inputId = "runFG",
                                                      label = "Run",
                                                      icon = shiny::icon("gears"),
                                                      class = "mx-2",
                                                      width = "100px",
                                                      status="danger")
                                  ),
                                  column(width = 3
                                          ,actionButton(inputId = "controlbarToggleFG",
                                                       label = "Control",
                                                       icon = shiny::icon("sliders"),
                                                       class = "mx-2",
                                                       width = "100px",
                                                       status = "warning")
                                  ),
                                  column(width = 3
                                         ,actionButton(inputId = "infobarToggleFG",
                                                      label = "Info",
                                                      icon = shiny::icon("info"),
                                                      class = "mx-2",
                                                      width = "100px",
                                                      status="info")
                                  )
                                )
                                 ,visNetworkOutput("FGresult",
                                                  height = "650px",
                                                  width = "100%")
                              ),
                              bs4Dash::box(
                                title = "Sample of Tweets matching Query",
                                width = 12,collapsed = F,
                                HTML("<b>Sample of tweets referring to the selected pair of entities</b><i> (gray squares: posts from accounts that don't exist any more or are hidden for privacy compliance; Not found: posts that have been deleted by their author)</i>")
                                ,div(uiOutput("render_tweets_sample")),
                                div(fluidRow(uiOutput("render_tweet1_sampleFG"),
                                             uiOutput("render_tweet2_sampleFG"),
                                             uiOutput("render_tweet3_sampleFG"),
                                             uiOutput("render_tweet4_sampleFG"),
                                             uiOutput("render_tweet5_sampleFG"),
                                             uiOutput("render_tweet6_sampleFG")
                                ))
                              )
                              ,bs4Dash::box(
                                title = "SPARQL Queries Log",
                                width = 12,collapsed = T,
                                 div(
                                   uiOutput("sparqlFGqueryURL"),
                                   style="font-size: 12px;")
                                 #, footer = "Advanced parameters for tweeking HERMIONE"
                               )
                              ,color="primary")
                          )),
                        sidebar = boxSidebar(
                          startOpen = FALSE,
                          icon = icon( "info"),
                          id = "FG_info",
                          background = "darkgray"
                            ,fluidRow(
                              {
                                tags$div(HTML(
                                  "<h3>Network description and navigation information</h3><br>We here zoom-in on a pair of entities connected in the bird's-eye graph. This view extends the more general and impartial knowledge included in KGs with the aggregated perspectives from social media posts. Both the semantic information that can be gained from the tweets in the form of semantic embeddings, as well as the syntactic structure of the tweet, is taken into account. <br>
                                  Using this information, the subgraph of a given entity pair is extended with additional nodes containing different characterizations of each entity, and the edge between them is split apart into a number of paths that show different ways in which people understand the relationship between the two selected entities."),
                                  tags$ul(
                                    tags$li(tags$span("Node size is proportional to the number of occurrences of the entity")),
                                    tags$li(tags$span("Edge width is proportional to the number of occurrences of the entity")),
                                    tags$li(tags$span("The shape depends on the type of entity")))
                                  #,uiOutput("tweet_example")
                                )
                              }

                            )
                        )
                        #)#check here
                      ))),
                    ###### Body item 4: cases ####
                    tabItem(tabName = "cases",
                            fluidRow(column(
                              width = 12,
                              tabBox(
                                # ribbon(text = "2022",
                                #        color = "pink"),
                                title = "Case studies",
                                elevation = 2,
                                id = "tabcard1",
                                width = 12,
                                collapsible = TRUE,
                                closable = TRUE,
                                solidHeader = TRUE,
                                type = "tabs",
                                status = "primary",
                                selected = "Gender Inequality during the COVID pandemic",
                                ##### Gender Inequality #####
                                tabPanel(
                                  "Gender Inequality during the COVID pandemic",
                                  fluidRow(
                                    column(width = 3,
                                           HTML(
                                             "<h3>Gender Inequality during the COVID pandemic</h3>"),
                                           HTML(
                                             "<br>The COVID-19 pandemic has had a significant impact on gender inequality, disproportionately affecting women, both in terms of health and economic well-being.<br><br>One of the main ways in which the pandemic has affected gender inequality is through their income and work: Women have been disproportionately affected by job losses and reduced working hours, as they are more likely to work in service sectors that have been hit hard by the pandemic, such as retail and hospitality. Additionally, in many countries women are also more likely to work in insecure, and informal work, which makes them more vulnerable to the economic impacts of the crisis.<br>Another way in which the pandemic has affected gender inequality is through the increased burden of care work. The closure of schools and other care facilities has meant that many women have had to take on additional responsibilities for the care of children and other dependents. This has made it harder for women and has also led to increased stress and fatigue.<br><br>The pandemic has also highlighted existing inequalities in healthcare. Women, especially those belonging to marginalized communities, have been disproportionately affected by the pandemic, both in terms of their health and access to healthcare.<br><br>"
                                           ),

                                           ),
                                    column(width = 9
                                            ,htmlOutput("html_2")

                                  )
                                ),
                                bs4Dash::box(
                                  title = "Sample of Tweets about Gender inequality during the pandemic",
                                  width = 12,collapsed = F,
                                  HTML("<i>Gray square: posts from accounts that don't exist any more or posts that are hidden for privacy and GDPR compliance; Not found: posts that have been deleted by their author</i>"),
                                  div(
                                fluidRow(
                                                uiOutput("render_tweet1_CS1"),
                                                uiOutput("render_tweet2_CS1"),
                                                uiOutput("render_tweet3_CS1"),
                                               # uiOutput("render_tweet4_CS1"),
                                                uiOutput("render_tweet5_CS1"),
                                                uiOutput("render_tweet6_CS1")
                                )
                                )),
                                fluidRow(HTML("
                                             Report by the European Institute for Gender Equality:<br><br>
                                              <i>-Gender equality and the socio-economic impact of the COVID-19 pandemic</i>"),
                                                tags$iframe(style="height:600px; width:100%", src="www/report-ineq.pdf")

                                ),
                                htmlOutput("html_3")
                                # ,tabPanel("Climate change & inequality",
                                #          HTML("....")),
                                # tabPanel(
                                #   "Racial Inequality after the murder of George Floyd",
                                #   HTML("....")
                                # ),
                                # tabPanel("Education Inequality in the digital age",
                                #          HTML("...."))
                              )
                            )))),
                    ###### Body item 5: methods & refs. ####
                    tabItem(tabName = "methods",
                            fluidRow(sortable(width = 12,
                                ####### PDF MAPPING ######
                                box(
                                  title = "Mapping and Exploring the Dynamics of Inequality Narratives Through
Social Media",
                                  closable = F,
                                  maximizable = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  solidHeader = T,
                                #htmlOutput('pdfviewer'),
                                HTML("Narratives play a relevant role in the conceptualization, identification, and comparison of social groups and in the perception of social inequalities. For example, they are used to: (i) create, manipulate and diffuse social myths (e.g., syndicalism); (ii) justify and legitimize (or question) socio-economic differences, power structures and hierarchies; (iii) identify the origins and consequences of socio-economic differences among social groups; (iv) influence social antagonisms among groups; and make specific social categories or categorization dimensions more salient.<br><br> Despite some well known biases in terms of representativeness of certain categories of the population, given their wide coverage of societal issues and their widespread diffusion, online discourse on social media platforms, such as Twitter, can be considered a viable proxy for mapping, exploring and understanding inequalitiy narratives. This is because a wide audience made up of citizens, interest group leaders, representatives, scholars and experts, advocates and activists, as well as other stakeholders are involved in these online debate arenas. <br><br>
In addition, online discourse can serve to identify perspectives and relations among entities mentioned in the public discourse about inequality by opposing groups taking part in a debate. The resulting representations of these online narrations can be hence compared with - or integrate into - other knowledge or narrative structures, such as knowledge graphs (KG), for example Wikidata and DBpedia, and/or other types of expert opinions coming from more traditional information sources, like news broadcasters, governance bodies, or scientific articles and books by field experts and scholars. <br><br>
To map the online inequality debate, we have developed a methodology and an interactive
toolset, which allows users to explore, as networks, online narratives both from a
bird’s-eye view as well as on a fine-grained perspective.<br>
Through named entity recognition (NER) and linking (NEL), we consolidate and validate entities identified in the short texts about inequality published on Twitter during the COVID pandemic. Also, by applying a edge filtration algorithm that takes into account the relative relevance of links (i.e., edges) between entities (when having to decide which ones to keep and which ones to filter out), we can highlight interconnections between peripheral clusters of nodes whose links would otherwise be filtered out by standard filtration techniques, improving the overall informativeness and inclusiveness of our entity network representation of the online debate about inequalities. The aforementioned methodology has been first described in the following working paper:<br><br>
-<i>Laura Spillner, Carlo R. M. A. Santagiustina, Thomas Mildner, and Robert Porzel
(2022). Mapping and Exploring the Dynamics of Inequality Narratives Through
Social Media.</i><br><br>"),
                                tags$iframe(style="height:600px; width:100%", src="www/CSCW23.pdf")),
                                ####### PDF Inequality ######
                                box(
                                  title = "The Observatory Knowledge Graph and the The Observatory Integrated Ontology",
                                  closable = F,
                                  maximizable = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  solidHeader = T,
                                  #htmlOutput('pdfviewer'),
                                  HTML("The Observatory Integrated Ontology (OBIO) integrates social media metadata with various types of linguistic knowledge, like FCG-based frame extraction and Spacy-based NLP and NLU, among others. The Observatory Knowledge Graph (OKG) was built, following the OBIO specifications, from posts related to inequality retrieved from Twitter/X. The OKG is the backbone of a social media observatory, and its scope is to enable a deeper understanding of social media discourse about inequality. The OBIO and OKG have been first described in the following article:<br><br>
-<i>Inès Blin, Lise Stork, Laura Spillner, and Carlo R.M.A. Santagiustina. 2023. OKG: A Knowledge Graph for Fine-grained Understanding of Social Media Discourse on Inequality. In Knowledge Capture Conference 2023 (K-CAP ’23), December 05–07, 2023, Pensacola, FL, USA. ACM, New York, NY, USA, 9 pages.</i><br><br>"),
                                  tags$iframe(style="height:600px; width:100%", src="www/kcap23-16.pdf")),
                                ####### Entity Linking #######
                                box(
                                  title = "Entity Linking",
                                  closable = F,
                                  maximizable = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  solidHeader = T,
                                  HTML(
                                    '<!DOCTYPE html>
<html>
<head>
	<title>Entity Linking to DBpedia with EntityLinker from SpaCy</title>
</head>
<body>

	<h1>Entity Linking to DBpedia with the EntityLinker from spaCy</h1>

	<p>Entity linking is the task of identifying and linking named entities in text to their corresponding entries in a knowledge base or database. DBpedia is a knowledge base that extracts structured information from Wikipedia and makes it available as a semantic web resource.</p>

	<p>spaCy is an open-source software library for advanced natural language processing in Python. spaCy provides a simple and efficient way to perform entity linking to DBpedia with its built-in EntityLinker.</p>

   <h2>Usage in the observatory</h2>

  <p> We use Spacy and the DBpedia EntityLinker to identify named entities mentioned in tweets about inequalities and to match them with KGs on the web. By doing so we can add a further layer of knowledge to our analysis and consolidate entities that can be mentioned in different ways in online discussions.</p>

	<h2>Installation</h2>



	<p>To use the EntityLinker in spaCy for entity linking to DBpedia, you first need to install spaCy and the DBpedia Spotlight service. You can do this by running the following commands:</p>

	<pre><code>pip install spacy
	pip install spacy-lookups-data
	pip install spotlight-lookup</code></pre>

	<h2>Using the EntityLinker in spaCy for Entity Linking to DBpedia</h2>

	<p>Here is an example of how to use the EntityLinker in spaCy for entity linking to DBpedia:</p>

                                   <pre><code>import spacy
                                 from spacy.lang.en.examples import sentences
                                 from spacy_dbpedia_spotlight import EntityLinker

                                 # Load the spaCy model
                                 nlp = spacy.load("en_core_web_sm")

                                 # Load the DBpedia Spotlight service
                                 linker = EntityLinker()
                                 nlp.add_pipe(linker)

                                 # Process a sentence
                                 doc = nlp(sentences[0])

                                 # Extract entities and link them to DBpedia
                                 for ent in doc.ents:
                                   if ent._.dbpedia_url:
                                   print(ent.text, " -> ", ent._.dbpedia_url)</code></pre>

                                   <p>In this example, we are using the `en_core_web_sm` spaCy model for English language processing. We are also using the `EntityLinker` class from the `spacy_dbpedia_spotlight` package to link entities to DBpedia.</p>

                                   <p>The `EntityLinker` class automatically adds a custom spaCy pipeline component that links named entities to DBpedia. When you add the `EntityLinker` component to the spaCy pipeline, it adds a new attribute to each named entity (`ent._.dbpedia_url`) that contains the DBpedia URL for the entity, if one exists.</p>

                                   <h2>Conclusion</h2>

                                   <p>Entity linking to DBpedia with the EntityLinker from spaCy for Python is a powerful tool for enriching text data with structured knowledge from DBpedia. With the EntityLinker and the DBpedia Spotlight service, it is easy to extract named entities from text and link them to corresponding DBpedia entries.</p>
</body>
</html>'

                                  )
                                ),
#                                 ####
#                                 box(
#                                   title = "Methods 2 Explanation",
#                                   closable = TRUE,
#                                   maximizable = TRUE,
#                                   collapsible = TRUE,
#                                   width = 12,
#                                   solidHeader = FALSE,
#
#                                   HTML(
#                                     '<!DOCTYPE html>
# <html>
# <head>
# 	<title>Method 2 Title </title>
# </head>
# <body>
# </body>
# </html>')
#
#                               ),
#                                 box(
#                                   title = "Methods 4 Explanation",
#                                   closable = TRUE,
#                                   maximizable = TRUE,
#                                   collapsible = TRUE,
#                                   width = 12,
#                                   solidHeader = FALSE
#                                 )
                            ))),
                    ###### Body item 6: info ####
                    tabItem(tabName = "info",
                            # Boxes need to be put in a row (or column)
                            fluidRow(column(
                              width = 12,
                              tabBox(
                                title = "Info & Contacts",
                                elevation = 2,
                                id = "tabcard_info",
                                width = 12,
                                collapsible = TRUE,
                                closable = FALSE,
                                solidHeader = TRUE,
                                type = "tabs",
                                status = "primary",
                                selected = "Contacts & devs",
                                ###### a) frontend info ####
                                tabPanel("Frontend",
                                         HTML("<h3>The frontend: HERMIONE</h3>"),
                                         fluidRow(column(width = 5,HTML("<h4>HERMIONE is an interactive dashboard developed in R and Shiny. Its source code is available on GitHub at this link:<br><center><a href='https://github.com/carlosantagiustina/HERMIONE'>github.com/carlosantagiustina/HERMIONE/</a></center><br><br> <a href='https://shiny.posit.co/'>Shiny</a> in a R library for building UIs for data exploration and analysis performed in realtime. <br><br>  HERMIONE's interactive network visualizations have been created using Vis.js.<br><br> To standardize the dashboard and its components the <a href='https://thinkr-open.github.io/golem/'> R Golem framework</a> was used. <br><br>This framework allows to install and deploy the dashboard as if it was a R package or a <a href='https://www.docker.com/products/personal/'>Docker container</a>.</h4>")
                                         ),
                                           column(width = 7,
                                         bs4Carousel(
                                           id = "frontendcarousel",
                                           bs4CarouselItem(
                                             caption = "Hermione UI",
                                             tags$img(src = "www/home.png",style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 750px;
                              vertical-align: middle;")
                                           ),
                                           bs4CarouselItem(
                                             caption = "Bird's-eye perspective",
                                             tags$img(src = "www/birds_eye.png",style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 750px;
                              vertical-align: middle;")
                                           ),
                                           bs4CarouselItem(
                                             caption = "Fine Grained View",
                                             tags$img(src = "www/fine_grained.png",style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 750px;
                              vertical-align: middle;")
                                           )
                                         ))
                                ))
                                ###### b) backend info ####
                                ,tabPanel("Backend",
                                          HTML("<h3>The backend: OKG & OBIO</h3>"),
                                          fluidRow(column(width = 5,HTML("<h4>HERMIONE uses as database and back-end the Observatory Knowledge Graph, which has been deployed within the datalegend infrastructure, part of the Clariah project, hosted at TriplyDB.<br> For details about the OKG ontology, called OBIO, refer to:<br><br>    <a href='https://muhai-project.github.io/okg_media_discourse/ontology/obio/doc/index-en.html'>muhai-project.github.io/okg_media_discourse/ontology/obio/doc/index-en.html</a><br><br>
HERMIONE queries dynamically the OKG through templated SPARQL queries based on users’ inputs.</h4>")
                                          ),
                                          column(width = 7,carousel(
                                            id = "backendcarousel",
                                            bs4CarouselItem(
                                              caption = "The Observatory Integrated Ontology (OBIO)",
                                              tags$img(src = "www/obio_schema.svg"
                                              ,style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 750px;
                              vertical-align: middle;")
                                            ),
                                            bs4CarouselItem(
                                              caption = "Example of SPARQL query generated by HERMIONE",
                                              tags$img(src = "www/query.png"
                                                       ,style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 750px;
                              vertical-align: middle;")
                                            )
                                          ))
                                         ))
                                ###### c) ecosystem ####
                                ,tabPanel("Ecosystem",
                                         HTML("<h3>The ecosystem: Social Inequality Observatory (SIO)</h3>"),
                                         tags$img(src = "www/SIO.png"
                                                  ,style = "display: block;
                              align:center;
                              width:100%;
                              min-width : 250px;
                              max-width : 900px;
                              vertical-align: middle;"),
                                         HTML("<h4>MUHAI's SIO is a digital ecosystem of interconnected services, components and interfaces, which allow us to extract, process, explore, enrich and contextualise relations between entities and events, arguments, and other types of narrations extracted from data of different types, retrieved or extracted from a variety of sources, ranging from social media data and scientific knowledge, to general or domain-specific knowledge graphs.<br><br> More info about the SIO here:<br><center> <a href='https://dml.uni-bremen.de/muhai/'>dml.uni-bremen.de/muhai/</a></center></h4>")
                                )
                                ###### d) contacts and devs ####
                                ,tabPanel("Contacts & devs",
                                          HTML("<h3>Developers</h3>"),
                                          fluidRow(
                                            column(width = 3,offset = 0,
                                                   img(
                                                     src = "www/portraits/Carlo_Santagiustina.jpeg"
                                                     ,style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 250px;
                              margin-left:0%;
                              margin-right:0%;
                              vertical-align: middle;")
                                                   ),
                                            column(width = 3,offset = 0,
                                                   img(
                                                     src = "www/portraits/Laura_Spillner.jpeg" ,style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 250px;
                              margin-left:0%;
                              margin-right:0%;
                              vertical-align: middle;")
                                            ),
                                            column(width = 3,offset = 0,
                                                   img(
                                                     src = "www/portraits/Ines_Blin.jpeg" ,style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 250px;
                              margin-left:0%;
                              margin-right:0%;
                              vertical-align: middle;")
                                            ),
                                            column(width = 3,offset = 0,
                                                     img(
                                                       src = "www/portraits/Lise_Stork.jpeg" ,style = "display: block;
                              width:100%;
                              min-width : 150px;
                              max-width : 250px;
                              margin-left:0%;
                              margin-right:0%;
                              vertical-align: middle;")
                                            )
                                            ),
                                         HTML("<br>HERMIONE has been developed by:<br>
                                         <ul>
  <li>Carlo R. M. A. Santagiustina - Venice International University & Ca'Foscari University - <a href='mailto:carlo.santagiustina@univiu.org'>carlo.santagiustina@univiu.org</a> (lead dev.)</li>
  <li>Laura Spillner - University of Bremen - <a href='mailto:laura.spillner@uni-bremen.de'>laura.spillner@uni-bremen.de</a> </li>
  <li>Lise Stork -  Vrije Universiteit Amsterdam - <a href='mailto:l.stork@vu.nl'>l.stork@vu.nl</a>  </li>
  <li>Inès Blin -  Vrije Universiteit Amsterdam & Sony CSL-Paris - <a href='mailto:ines.blin@sony.com'>ines.blin@sony.com</a> </li>
</ul>
Please contact us by posting an issue through GitHub (<a href='https://github.com/carlosantagiustina/HERMIONE/issues'>here</a>) for any technical question or bug related to HERMIONE.
                                              ")
                                )
                                )
                              )
                              )
                            )
            #tab item finishes here
            )
          )
          ,
          ##### UI - CONTROLBAR ####
          controlbar = dashboardControlbar(
            id = "controlbar",
            skin = "light",
            width = golem::get_golem_options("controlbar_width"),
            collapsed = TRUE,
            #icon= shiny::icon("gears"),
            pinned = FALSE,
            overlay = FALSE,
            bs4Dash::box(
              title = textyle(tags$p("HERMIONE's control room", style = "font-size:2rem;font-weight:200;"),transition = "100",delay = "50",duration = "100",color = "#ffd100"),
              footer = "Tell HERMIONE what to do by setting the parameters for your exploration",
              width = 12,
               {
                 div(
                   style = "
                   margin-right:0%;
                    display: inline-block;
                    position: relative;"
                ,
                   fluidRow(
                   column(width = 4,offset = 0,
                   img(
                     src = "www/hermione/0_smile.png",
                     style = "display: block;
                              float:right;
                              width:100%;
                              min-width : 150px;
                              max-width : 250px;
                              margin-left:0%;
                              margin-right:0%;
                              vertical-align: middle;"
                   )),
                   column(width = 8,offset = 0,
                          fluidRow(column(12, align="center",div(
                            actionButton(label = "< Previous", "Hpre",width = "100"
                                         #,style="margin-left: 2em"
                                         ),
                            actionButton(label = "Next >", "Hnext",width = "100")
                            ))),
                   span(div(uiOutput("controlroom_text"),id="iphermione"),
                        style = "
                        position: relative;
                        min-width : 250px;"

                   ),
                   fluidRow(column(12, align="center",div(
                     actionButton(label = "Quit and reset tutorial", "exit_t",width = "200"
                                 # ,style="margin-left: 2em"
                                  ))))
                 )))},
              br(),
              #visNetworkOutput("result",height = "600px",width = "100%"),
              HTML("<hr>"),
              div(style='display:inline-block',
              fluidRow(column(width = 6,HTML("<b>Step 1</b><br>"),               shiny::actionButton("sparqltaskBE", "Run/Update: Bird's-eye Network",icon = shiny::icon("map")),
                              downloadButton("downloadFGData","Download data")),
                       column(width = 6,HTML("<b>Step 2</b><br>"),                 shiny::actionButton("sparqltaskFG", "Run/Update: Fine Grained View",icon = shiny::icon("binoculars")))
                ),
              #actionButton(inputId = 'run',label =  'Run',style='font-size:110%; display:center-align'),
              #actionButton(inputId = 'cancel',label =  'Cancel',style='font-size:110%; display:center-align'),
              style='display:center-align'),
              br(),
              dateRangeInput(
                'dateRange2',
                label = paste('Twitter data date range (range is limited)'),
                start = golem::get_golem_options("twitter_data_from"),
                end =   golem::get_golem_options("twitter_data_to"),
                min =   golem::get_golem_options("twitter_data_from"),
                max =   golem::get_golem_options("twitter_data_to"),
                #  start = "30-05-2020",
                #  end = "30-08-2020",
                #  min = "30-05-2020",
                # max = "30-08-2020",
                separator = " - ",
                format = "dd-mm-yy",
                startview = 'year',
                language = 'en',
                weekstart = 1
              ),
              bs4Dash::box(
                title = "Bird's-eye Network - Advanced controls",
                width = 12,
                collapsed = T,
               # footer = "Advanced parameters for tweeking HERMIONE",
                sliderInput(inputId = "slider_nentites", "Targeted Number of Entities (nodes) in Bird's-eye Network View:",value = 50, min=25, max=500,step =  25),
               sliderInput(inputId = "slider_nmaxrows", "Max Sample Size:", value = golem::get_golem_options("max_rows_sparql"),min= 100, max=golem::get_golem_options("max_rows_sparql"), step = 100)
              ),
              bs4Dash::box(
                title = "Fine Grained View  - Advanced controls",
                width = 12,
                collapsed = T
              #  footer = "Advanced parameters for tweeking HERMIONE",
              )
            )
          ),
          #### UI - FOOTER ####
          footer = bs4Dash::dashboardFooter(
            fixed = FALSE,
            left = div(a(href = "https://www.muhai.org/",
                                              target = "_blank", "Website: MUHAI.org"),
                       br(),
              a(href = "https://twitter.com/MUHAI_org",
                     target = "_blank", "Twitter: @MUHAI_org"),
              br(),
                     a(href = "https://www.linkedin.com/company/muhai/",
                       target = "_blank", "LinkedIn: MUHAI"),
              br(),
              a(href = "https://dml.uni-bremen.de/muhai/index.html",
                target = "_blank", "MUHAI Social Inequality Observatory")
                     ),
            right = HTML(
              '<center><img src="https://muhai.org/images/headers/euflag.png" alt="" style="width:100px;"></center><par style="font-size:70%;"><a href="https://cordis.europa.eu/project/id/951846">This project has received funding<br> from the European Union’s Horizon 2020<br> research and innovation programme<br> under grant agreement No 951846</a></par><br><br>'
            )
          ),
          title = "MUHAI Inequality Observatory"
        )
      ) # here removed
  }

  #' Add external Resources to the Application
  #'
  #' This function is internally used to add external
  #' resources inside the Shiny application.
  #'
  #' @import shiny
  #' @import bs4Dash
  #' @import echarts4r
  #' @import waiter
  #' @import fresh
  #' @import DT
  #' @import bslib
  #' @import visNetwork
  #' @import base64enc
  #' @import tidyverse
  #' @import shinyjs
  #' @import textyle
  #' @import ipc
  #' @import future
  #' @import promises
  #' @import httr
  #' @import tidytext
  #' @import ggplot2
  #' @importFrom golem add_resource_path activate_js favicon bundle_resources
  #' @noRd
  golem_add_external_resources <- function() {
    add_resource_path("www",
                      app_sys("app/www"))

    tags$head(
      favicon(),
      #tags$script(async = NA, src = "https://platform.twitter.com/widgets.js"),
      bundle_resources(
        path = app_sys("app/www")
        ,
        app_title = "HERMIONE - MUHAI Inequality Observatory"
        ,
        version = "0.9.1"
        #,meta = list()
        #,attachment = NULL
        #,package = NULL
        ,
        all_files = TRUE
        ,
        app_builder = NULL
        #,with_sparkles = FALSE
      )
      # Add here other external resources
      # for example, you can add shinyalert::useShinyalert()
    )
  }
