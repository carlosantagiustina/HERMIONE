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
      # Leave this function for adding external resources
      golem_add_external_resources(),
      tags$link(rel="stylesheet",
                type="text/css",
                href="www/css/custom.css"),
    tags$head(tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")),
      # Your application UI logic
      bs4Dash::dashboardPage(
        useShinyjs(),
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
                  subtitle = "Sampled tweets about inequality",
                  color = "primary",
                  icon = icon("twitter"),
                  footer = div(paste0("latest update: ", Sys.Date()))
                ),
                valueBox(
                  value = h4("ML ⋅ NLP ⋅ FCG ⋅ KG"),
                  subtitle = "combined AI methods",
                  color = "indigo",
                  icon = icon("gears")
                ),
                valueBox(
                  value = h4("8,892,244"),
                  subtitle = "Extracted semantic triples",
                  color = "danger",
                  icon = icon("info"),
                  footer =  div(paste0("latest update: ", Sys.Date()))
                ),
                valueBox(
                  value = h4("3,592"),
                  subtitle = "Links to DBpedia KG",
                  color = "teal",
                  icon = icon("link"),
                  footer =  div(paste0("latest update: ", Sys.Date()))
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
                        "<h4> Join us as we explore the data, events, narratives, and perspectives that can help us to better understand the complex and nuanced nature of inequality in our contemporary world.</h4>"
                      )),
                      br(),
                      fluidRow(
                        column(width = 6,
                               div(
                              infoBox(width = 12,title = "",icon =icon(  "users"),value = HTML(
                                 "<h4> Inequalities in society can take many forms - from income and wealth disparities, to discrimination based on etchnicity, gender, age, religion and other factors.</h4>"
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
                                                   width = "100%")
                            ),
                            bs4Dash::box(
                              title = "Sample of Tweets matching Query",
                              width = 12,collapsed = T,
                              HTML("<b>Select a node in the network obove to see a sample of posts referring to that entity</b><i> (<b>Gray squares</b>: posts from accounts that don't exist any more or hidden for privacy and GDPR compliance; <b>Not found</b>: posts that have been deleted by their author)</i>"),
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
                              div(uiOutput("sparqlqueryURL"),style="font-size: 12px;")
                              #, footer = "Advanced parameters for tweeking HERMIONE"
                            ),
                            bs4Dash::box(width = 12,
                                    title = "Selected Nodes and Edges",collapsed = T,
                                    {
                                   div(HTML("<br>Selected node:<br>")
                                  ,verbatimTextOutput("return_BE_node")
                                  ,HTML("Selected edge:<br>")
                                  ,verbatimTextOutput("return_BE_edge")
                                    )}


                                    )
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
                                         uiOutput(outputId = "FG_entity_1")),
                                  column(width = 6,
                                         uiOutput(outputId = "FG_entity_2"))
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
                                width = 12,collapsed = T,
                                HTML("<b>Select a node in the network obove to see a sample of posts referring to that entity</b><i> (gray squares : posts from accounts that don't exist any more or hidden for privacy compliance; Not found! : posts that have been delated by their author)</i>")
                                #,div(uiOutput("render_tweets_sample")),
                                # div(fluidRow(uiOutput("render_tweet1_sample"),
                                #              uiOutput("render_tweet2_sample"),
                                #              uiOutput("render_tweet3_sample"),
                                #              uiOutput("render_tweet4_sample"),
                                #              uiOutput("render_tweet5_sample"),
                                #              uiOutput("render_tweet6_sample"),
                                #              uiOutput("render_tweet7_sample"),
                                #              uiOutput("render_tweet8_sample"),
                                #              uiOutput("render_tweet9_sample"),
                                #              uiOutput("render_tweet10_sample"),
                                #              uiOutput("render_tweet11_sample"),
                                #              uiOutput("render_tweet12_sample")
                                # ))
                                #, footer = "Advanced parameters for tweeking HERMIONE"
                              )
                              # ,bs4Dash::box(
                              #   title = "SPARQL Queries Log",
                              #   width = 12,collapsed = T,
                              #    div(
                              #      uiOutput("sparqlqueryURL"),
                              #      style="font-size: 12px;")
                              #    #, footer = "Advanced parameters for tweeking HERMIONE"
                              #  )
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
                                ribbon(text = "2022",
                                       color = "pink"),
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
                                tabPanel(
                                  "Gender Inequality during the COVID pandemic",
                                  HTML(
                                    "The COVID-19 pandemic has had a significant impact on gender inequality. The pandemic has disproportionately affected women, both in terms of their health and their economic well-being.<br>One of the main ways in which the pandemic has affected gender inequality is through their income and work: Women have been disproportionately affected by job losses and reduced working hours, as they are more likely to work in service sectors that have been hit hard by the pandemic, such as retail and hospitality. Additionally, in many countries women are also more likely to work in insecure, and informal work, which makes them more vulnerable to the economic impacts of the crisis.<br>Another way in which the pandemic has affected gender inequality is through the increased burden of care work. The closure of schools and other care facilities has meant that many women have had to take on additional responsibilities for the care of children and other dependents. This has made it harder for women and has also led to increased stress and fatigue.<br>The pandemic has also highlighted existing inequalities in healthcare. Women, especially those belonging to marginalized communities, have been disproportionately affected by the pandemic, both in terms of their health and access to healthcare..."
                                  )
                                  ,
                                  htmlOutput("html_2")
                                ),
                                tabPanel("Climate change & inequality",
                                         HTML("....")),
                                tabPanel(
                                  "Racial Inequality after the murder of George Floyd",
                                  HTML("....")
                                ),
                                tabPanel("Education Inequality in the digital age",
                                         HTML("...."))
                              )
                            ))),
                    ###### Body item 5: methods & refs. ####
                    tabItem(tabName = "methods",
                            fluidRow(
                              column(
                                width = 6,
                                ######## STM #########
                                box(
                                  title = "Method 1: Structural Topic Modeling",
                                  closable = TRUE,
                                  maximizable = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  solidHeader = FALSE,
                                  HTML(
                                    '<html>
<head>
	<title>Structural Topic Models with the STM Library for R</title>
</head>
<body>

	<h1>Structural Topic Models with the STM Library for R</h1>

	<p>A structural topic model (STM) is a type of topic model that takes into account the underlying structure of a document collection. It is particularly useful when analyzing collections of text that have known attributes or metadata, such as author, time period, or source.</p>

   <h2>Usage in the observatory</h2>

  <p> We use a (seeded) structural topic model to infer how different types of inequalities are discussed in social media and to explain how these discussions change across time and among groups of users (e.g., tweets by economists and otherr inequality experts). </p>
   <p> In addition, the features of our topic model go beyond classical bag of words approaches and include dependency relations between words and noun phrases inferred through SpaCy </p>

	<h2>Installation</h2>

	<p>To use the STM library for R, you first need to install it. You can do this by running the following command in R:</p>

	<pre><code>install.packages("stm")</code></pre>

	<h2>Creating a STM</h2>

	<p>To create a STM, you need to specify the documents, the vocabulary, and the metadata. Here is an example:</p>

	<pre><code># Load the STM library
	library(stm)

	# Load the data
	data("nyt")

	# Create the STM
	stm_model <- stm(documents = nyt$documents,
	                 vocab = nyt$vocab,
	                 metadata = nyt$meta,
	                 K = 10)</code></pre>

	<p>In this example, we are using the `nyt` dataset that comes with the STM library. This dataset contains a collection of New York Times articles from 1987 to 2007, along with metadata such as the date and section of each article.</p>

	<p>The `K` parameter specifies the number of topics to extract from the documents. In this example, we have a model with 10 topics.</p>

	<h2>Viewing the Results</h2>

	<p>Once you have created a STM, you can view the results using various functions provided by the STM library. Here are a few examples:</p>

	<pre><code># View the topic proportions for each document
	stm_model$theta

	# View the top words for each topic
	labelTopics(stm_model)

	# Plot the topic proportions for each document
	plot(stm_model)</code></pre>

	<h2>Conclusion</h2>

	<p>The STM library for R provides a powerful tool for analyzing collections of text that have known metadata. By taking into account the underlying structure of the documents, STM can provide more meaningful insights into the topics and themes present in the collection.</p>

</body>
</html>'
                                  )

                                ),
                                box(
                                  title = "Methods 2 Explanation",
                                  closable = TRUE,
                                  maximizable = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  solidHeader = FALSE
                                )
                              ),
                              column(
                                width = 6,
                                ####### Entity Linking #######

                                box(
                                  title = "Methods 2: Entity Linking",
                                  closable = TRUE,
                                  maximizable = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  solidHeader = FALSE,
                                  HTML(
                                    '<!DOCTYPE html>
<html>
<head>
	<title>Entity Linking to DBpedia with EntityLinker from SpaCy for Python</title>
</head>
<body>

	<h1>Entity Linking to DBpedia with the EntityLinker from spaCy for Python</h1>

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
                                )
                                ,
                                box(
                                  title = "Methods 4 Explanation",
                                  closable = TRUE,
                                  maximizable = TRUE,
                                  collapsible = TRUE,
                                  width = 12,
                                  solidHeader = FALSE
                                )
                              )

                            )),
                    ###### Body item 6: info ####
                    tabItem(tabName = "info",
                            # Boxes need to be put in a row (or column)
                            fluidRow(
                              HTML(
                                '<h2 class="uk-h3 uk-margin-large uk-margin-remove-bottom">        The MUHAI project introduces meaning and<br>understanding in artificial intelligence    </h2><div class="uk-panel uk-margin"><div>Responsible human-centric AI needs a way to deal with&nbsp;<strong>meaning</strong>. The MUHAI project tackles this foundational question by developing computational models of narrative construction. Creating or understanding&nbsp;<strong>narratives</strong>&nbsp;requires the integration of information coming from sensory-motor embodiment, measurement data, language, &nbsp;semantic memory, mental simulation, and&nbsp;<strong>episodic memory</strong>.&nbsp;&nbsp;</div>
<p>MUHAI uses two domains as testbeds for the development of the required breakthroughs:&nbsp;<strong>common sense</strong>&nbsp;pragmatic knowledge about the world needed for the domain of cooking and&nbsp;<strong>social knowledge&nbsp;</strong>needed for coming up, understanding and checking data stories about inequality in society.</p>
<p>The project relies on many existing techniques of AI ranging from <strong>deep learning networks</strong> to <strong>knowledge graphs</strong>, but will push their boundaries and develop new techniques all operating in the service of giving AI systems a better grip on meaning and thus on explanation and other key issues for achieving human-centric AI.</p>
<p>The outcome of MUHAI is twofold. It will push the <strong>state of the art</strong> in cognitive home robotics, particularly for food production and the management of food resources, and it will provide <strong>tools</strong> for social scientists to better understand social phenomena, as for example the persistence of inequality in our society.</p>
<p>The MUHAI project has started in October 2020 and will finish 48 months later.</p></div>
</div>'
                              )
                            ))
            )
          )
          ,
          ##### UI - CONTROLBAR ####
          controlbar = dashboardControlbar(
            id = "controlbar",
            skin = "light",
            width = golem::get_golem_options("controlbar_width"),
            collapsed = TRUE,
            pinned = FALSE,
            overlay = TRUE,
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
                   column(width = 5,offset = 0,
                   img(
                     src = "www/hermione/0_smile.png",
                     style = "display: block;
                              float:right;
                              width:100%;
                              min-width : 100px;
                              max-width : 200px;
                              margin-left:0%;
                              margin-right:-12%;"
                   )),
                   column(width = 6,offset = 0,br(),
                   span(div(textyle(tags$p("Hi, here below you can set the parameters for creating your own perspective on the OKG data! Start from the Bird's-Eye network exploration tool.Click Next > to continue"),delay = "100",duration = "100",color = "black", class = "ex1"),id="iphermione"),
                        style = "
                        position: relative;
                        min-width : 150px;"

                   ),   br(),
                   fluidRow(align = 'right',div(
                   actionButton(label = "< Previous", "Hpre",width = "100",style="margin-left: 2em"),
                   actionButton(label = "Next >", "Hnext",width = "100")))
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
              sliderInput(inputId = "slider_nmaxrows", "Max Sample Size:", value = golem::get_golem_options("max_rows_sparql"),min= 100, max=golem::get_golem_options("max_rows_sparql"), step = 100),
              bs4Dash::box(
                title = "Bird's-eye Network - Advanced controls",
                width = 12,
               # footer = "Advanced parameters for tweeking HERMIONE",
                textInput(inputId = "entityfilter", "Entity in Tweet RegEx filter (use | as OR logic separator)", value = "", width = NULL, placeholder = "insert the names of one or more entities separated by | (e.g.,Obama|Trump)"),
                sliderInput(inputId = "slider_nentites", "Targeted Number of Entities (nodes) in Bird's-eye Network View:",value = 100, min=25, max=500,step =  25)
              ),
              bs4Dash::box(
                title = "Fine Grained View  - Advanced controls
",
                width = 12,
              #  footer = "Advanced parameters for tweeking HERMIONE",
              uiOutput(outputId = "FG_entity_11"),
              uiOutput(outputId = "FG_entity_22")

              )
            )
          ),
          #### UI - FOOTER ####
          footer = bs4Dash::dashboardFooter(
            fixed = TRUE,
            left = a(href = "https://twitter.com/MUHAI_org",
                     target = "_blank", "@MUHAI_org"),
            right = HTML(
              '<center><img src="https://muhai.org/images/headers/euflag.png" alt="" style="width:100px;"></center><br><par style="font-size:70%;">This project has received funding<br> from the European Union’s Horizon 2020<br> research and innovation programme<br> under grant agreement No 951846</par>'
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
