#' The application server-side
#'
#' @param input,output,session,initial Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import golem
#' @import waiter
#' @import shinyjs
#' @import shiny
#' @import shinyWidgets
#' @import bs4Dash
#' @import echarts4r
#' @import fresh
#' @import DT
#' @import bslib
#' @import tidyverse
#' @import textyle
#' @import ipc
#' @import visNetwork
#' @import httr
#' @import tidytext
#' @import ggplot2
#' @noRd

#### SERVER PARAMETERS ####
initial_server_parameters <- list(
  twitter_data_from ="01-01-2020",
  twitter_data_to  = "01-01-2022"
)

#### FUNCTIONS ####
query_and_build_net=function(target_nodes_=100,
                             filter_=NA,
                             START_DATE="2018-01-01",
                             END_DATE="2024-01-01" ,
                             N_THRESHOLD=2,
                             OFFSET=0,
                             N_LIMIT=0){

  require(httr)
  require(tidytext)
  require(tidyverse)
  library(quanteda)
  library(igraph)
  library(visNetwork)

  REQUEST= function(QUERY_=QUERY,PREFIX_=PREFIX,
                    ENDPOINT_=ENDPOINT
                    #,API_KEY_=API_KEY
                    , START_DATE_=START_DATE,
                    END_DATE_=END_DATE,
                    TIMEOUT_=30,
                    RESOURCE_=RESOURCE,
                    N_THRESHOLD_=N_THRESHOLD,
                    N_LIMIT_=N_LIMIT){
    #PASTE PREFIXES AND QUERY
    MY_QUERY=paste0(PREFIX_,QUERY_)

    #SOBSTITUTE PARAMETERS IF ANY
    if(!is.na(START_DATE_) && grepl(pattern = "[:][:]START[_]DATE[:][:]",x = QUERY_,perl = T,ignore.case = F)){
      MY_QUERY = MY_QUERY %>%
        gsub("::START_DATE::",START_DATE_,ignore.case = F,x=.)
    }
    if(!is.na(END_DATE_) && grepl(pattern = "[:][:]END[_]DATE[:][:]",x = QUERY_,perl = T,ignore.case = F)){
      MY_QUERY = MY_QUERY %>%
        gsub("::END_DATE::",END_DATE_,ignore.case = F,x=.)
    }
    if(!is.na(RESOURCE_) && grepl(pattern = "[:][:]RESOURCE[:][:]",x = QUERY_,perl = T,ignore.case = F)){
      MY_QUERY = MY_QUERY %>%
        gsub("::RESOURCE::",RESOURCE_,ignore.case = F,x=.)
    }
    if(!is.na(N_THRESHOLD_) && grepl(pattern = "[:][:]N_THRESHOLD[:][:]",x = QUERY_,perl = T,ignore.case = F)){
      MY_QUERY = MY_QUERY %>%
        gsub("::N_THRESHOLD::",N_THRESHOLD_,ignore.case = F,x=.)
    }
    if(!is.na(N_LIMIT_) && grepl(pattern = "[:][:]N_LIMIT[:][:]",x = QUERY_,perl = T,ignore.case = F)){
      MY_QUERY = MY_QUERY %>%
        gsub("::N_LIMIT::",N_LIMIT_,ignore.case = F,x=.)
    }


    #RUN REQUEST
    REQUEST=httr::GET(ENDPOINT_,timeout(TIMEOUT_),
                      query=list("query"=MY_QUERY),
                      add_headers("Accept"= "text/csv"
                                  # ,"Authorization" = paste('Bearer',
                                  #                         API_KEY_,
                                  #                         sep=" ")
                      )
    )

    if(REQUEST$status_code=="200"){
      CONTENT=content(REQUEST)
      #%>% mutate(. , n=1)
    }else{
      #return(errorCondition(message = REQUEST$status_code))
      return(REQUEST)
    }

    CONTENT

  }
  #### DEFINE  FUNCTION TO BUILD BIRD EYE NETWORK ####
  build_net=function(ANSWER_=ANSWER,target_nodes=target_nodes_,percentile=0.9,
                     power=4,filter=NA){
    DBPEDIA_DICT = unique(ANSWER_[,c('entity','entityDB')])
    DBPEDIA_DICT=DBPEDIA_DICT  %>% mutate(label=URLdecode(gsub(pattern="_",replacement=" ",x=gsub("http://example.com/ent_","",x = entity))))

    if(!is.na(filter)){
      ANSWER_=ANSWER_[ANSWER_$sentence %in% unique(ANSWER_$sentence[ANSWER_$entity %in% DBPEDIA_DICT$entity[grep(pattern = filter,DBPEDIA_DICT$label)]]),]
      DBPEDIA_DICT = unique(ANSWER_[,c('entity','entityDB')])
      DBPEDIA_DICT=DBPEDIA_DICT  %>% mutate(label=URLdecode(gsub(pattern="_",replacement=" ",x=gsub("http://example.com/ent_","",x = entity))))
    }

    N_ENTITIES_BY_ID = ANSWER_ %>% group_by(sentence) %>% summarise(n=n())
    N_ENTITES_BY_ENTITY = ANSWER_ %>% group_by(entity) %>% summarise(n=n())

    DFM= ANSWER_ %>% tidytext::cast_dfm(sentence, entity, n)
    FCM = DFM %>% quanteda::fcm()
    #)
    N=quanteda::nfeat(DFM)
    # TOP_N <- names(quanteda::topfeatures(DFM,decreasing = T,n = N))
    # #TOP_N
    # FCM_SELECT <- quanteda::fcm_select(FCM, pattern = TOP_N,
    #                                    selection = "keep",
    #                                    valuetype = "fixed")
    FCM_SELECT <- quanteda::fcm_select(FCM, pattern = quanteda::featnames(DFM),
                                       selection = "keep",
                                       valuetype = "fixed")
    if(nrow(FCM_SELECT)<5){return(message("Error: Not enought entity mentions"))}
    G = FCM_SELECT %>% quanteda.textplots::as.igraph(.,
                                                     omit_isolated = F,
                                                     weighted = T)
    row.names(DBPEDIA_DICT)=DBPEDIA_DICT$entity
    entities=DBPEDIA_DICT[names(V(G)),]
    #Node attributes
    G <- set_vertex_attr(G, "id", index = V(G), entities$entity)
    G <- set_vertex_attr(G, "label", index = V(G), entities$label)
    G <- set_vertex_attr(G, 'DBpedia', index = !is.na(entities$entityDB), "true")
    G <- set_vertex_attr(G, 'DBpedia', index = is.na(entities$entityDB), "false")
    G <- set_vertex_attr(G, 'DBpediaUrl', index = V(G), entities$entityDB)
    G <- set_vertex_attr(G, 'group', index = is.na(entities$entityDB), NA)
    G <- set_vertex_attr(G, 'occurences', index = V(G), featfreq(DFM))# add frequency of nodes
    G <- set_vertex_attr(G, 'value', index = V(G), (featfreq(DFM))^(1/2))# add frequency of nodes
    G <- set_vertex_attr(G, 'percentile', index = V(G), fmsb::percentile(featfreq(DFM))/100)
    G <- set_vertex_attr(G, 'level', index = !is.na(entities$entityDB), 1)
    G <- set_vertex_attr(G, 'shape', index = !is.na(entities$entityDB), "square")
    # G <- set_vertex_attr(G, 'scaling.label.enabled', index = !is.na(entities$entityDB), "true")
    # G <- set_vertex_attr(G, 'scaling.label.max', index = !is.na(entities$entityDB), 50)
    # G <- set_vertex_attr(G, 'scaling.label.min', index = !is.na(entities$entityDB), 25)
    # G <- set_vertex_attr(G, 'scaling.label.maxVisible', index = !is.na(entities$entityDB), 50)
    #G <- set_vertex_attr(G, 'collapse', index = is.na(entities$entityDB), T)
    #G <- set_vertex_attr(G, 'type', index = V(G), NA)
    #G <- set_vertex_attr(G, 'image', index = V(G), NA)
    #G <- set_vertex_attr(G, 'shape', index = V(G), "circularImage")
    #G <- set_vertex_attr(G, 'opacity', index = V(G), 0.5)
    #G <- set_vertex_attr(G, "title", index = V(G), NA)
    #Edge attributes
    G <- set_edge_attr(G,"width",index = E(G),E(G)$weight^(1/3))
    G <- set_edge_attr(G,"percentile",index = E(G),fmsb::percentile(E(G)$weight)/100)

    #min edge cooccurrence filter
    G <- subgraph.edges(G, E(G)[E(G)$weight > 1], delete.vertices = F) # delete all edges that occurr only once should remove about 90% for big graphs

    #target_nodes=100

    if(igraph::vcount(G)>target_nodes){
      threshold_nodes=1-(target_nodes/igraph::vcount(G))
      G <-delete.vertices(graph = G,v = V(G)[V(G)$percentile<threshold_nodes])   # delete all entities that are not in the top 0.05 percentile
    }

    #percentile filter

    if(igraph::vcount(G)/igraph::ecount(G)> 10*(target_nodes/100) | igraph::graph.density(G)>0.25){
      # percentile=0.9
      # power=4
      V(G)$edge_threshold <- sapply(names(V(G)),function(x,Graph=G, p=percentile*(V(G)$percentile^power)){quantile(incident(Graph, x, mode = "all")$weight,probs = p, na.rm = FALSE,
                                                                                                                   names = FALSE, digits = 7)})
      filter_edges=function(x,Graph=G){
        x=x
        max(V(Graph)[as.vector(ends(Graph, E(Graph)[x], names=F)) ]$edge_threshold) >= E(Graph)[x]$weight
      }

      y=1:length(E(G))
      E(G)$below_threshold =sapply(X = y, FUN = function(x){filter_edges(x)})

      G <- subgraph.edges(G, E(G)[!E(G)$below_threshold], delete.vertices = T)
    }

    #
    NETWORK=G %>%
      visNetwork::visIgraph(
        .,
        idToLabel = FALSE,
        physics = T,type = "square"
      ) %>% visNetwork::visEdges(dashes = F,arrows ="", smooth =list(enabled=T,roundness=1,type="discrete"),scaling = list(min=0.25,max=2.5),color = list(color = "lightgray", highlight = "#BF616A", hover = "goldenrod4")) %>%
      visNetwork::visNodes(color = list(background = "lightgray",border="black", highlight = list(border="firebrick",background="#BF616A"), hover = list(border="goldenrod",background='#ffc100')),scaling = list(min= 10, max= 50,label=list(enabled=T,min= 22.5, max= 45,maxVisible= 25,drawThreshold= 5))) %>%
      visPhysics(solver = "hierarchicalRepulsion",hierarchicalRepulsion
                 =list(nodeDistance=275,avoidOverlap=1,springLength=150),minVelocity=1,maxVelocity = 20,stabilization = list(enabled=F)) %>%
      visNetwork::visInteraction(multiselect = T, navigationButtons = T,hover=T,dragNodes = F,dragView = T) %>%
      visNetwork::visOptions(selectedBy = list(variable="DBpedia", multiple="true"),collapse = F,
                             manipulation = list(enabled = TRUE,deleteNode = FALSE, deleteEdge = FALSE,
                                                 editEdgeCols = c("title"),
                                                 editNodeCols = c("title","color")),
                             nodesIdSelection = list(enabled = T),
                             highlightNearest = list(enabled = T),
                             height = "600px",#"fit-content"
                             width = "100%",
                             autoResize = T
      ) %>%
      visEvents(doubleClick =
                  "function(params) {
    var nodeID = params.nodes[0];
    var DBpediaUrl = this.body.nodes[nodeID].options.DBpediaUrl;
    window.open(DBpediaUrl, '_blank');
   }")
    list(answer_final=ANSWER_,network_vis=NETWORK,igraph=G,dbpedia_dict=DBPEDIA_DICT, n_feat=N,n_ent_by_id= N_ENTITIES_BY_ID, n_ent_by_ent=N_ENTITES_BY_ENTITY)
  }

  PREFIX='
PREFIX observatory: <http://example.org/muhai/observatory#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sioc: <http://rdfs.org/sioc/ns#>
PREFIX nee: <http://www.ics.forth.gr/isl/oae/core#>
PREFIX schema: <http://schema.org/>
PREFIX dc: <http://purl.org/dc/terms/>
PREFIX earmark: <http://www.essepuntato.it/2008/12/earmark>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#>
PREFIX ex: <http://example.com>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX wsj: <https://w3id.org/framester/wsj/>
PREFIX pbdata: <https://w3id.org/framester/pb/pbdata/>
PREFIX pbschema: <https://w3id.org/framester/pb/pbschema/>
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
'

  QUERY =
    '
SELECT ?sentence ?entity ?entityDB ?date_time  (COUNT(?id) as ?n)
WHERE {
    ?id schema:mentions ?entityMention ;
       dc:created ?date_time .
    ?id  nif:sentence ?sentence .
    ?entity nif:anchorOf ?entityMention .
    OPTIONAL {?entity nee:hasMatchedURL ?entityDB .}
          FILTER (
        "::START_DATE::"^^xsd:dateTime < ?date_time &&  # start date
          ?date_time < "::END_DATE::"^^xsd:dateTime)   # end date
}
GROUP BY ?sentence ?entity ?entityDB  ?date_time
ORDER BY ?date_time
limit ::N_LIMIT::
'
ANSWER=REQUEST(QUERY_ = QUERY,
               PREFIX_ = PREFIX,
               START_DATE_ =START_DATE,
               END_DATE_=END_DATE ,
               N_THRESHOLD_=N_THRESHOLD,
               N_LIMIT_ = N_LIMIT,
               ENDPOINT_ = "https://api.druid.datalegend.net/datasets/lisestork/OKG/services/OKG/sparql"
               #,API_KEY_ = Sys.getenv("HERMIONE_ENDPOINT_KEY")
               ,RESOURCE_=NA)

results=build_net(ANSWER_  = ANSWER,target_nodes = target_nodes_,filter = filter_)
#saveRDS(results,file = paste0("results",Sys.Date(),".RDS"))
results
}



#### SERVER FUNCTION ####
app_server <- function(input, output, session) {
  ##### ACTIVATE JS FUNCTION ####
  golem::activate_js()
  ##### INCLUDE PRERENDERED HTML FILES ####
   output$html_1 <- renderUI(includeHTML(system.file("extdata","inflation_economicinequality.html",package = "Hermione")))
#   output$html_1 <- renderUI(
#     {
#       if(){
#         includeHTML(system.file("extdata","inflation_economicinequality.html",package = "Hermione"))
#       }
#
#
#     }
# )
  output$html_2 <- renderUI(includeHTML(system.file("extdata","COVID_genderinequality.html",package = "Hermione")))
  ##### HERMIONE AVATAR IMAGE FILES ####
  hermione_avatar=
    c("<img src='https://avataaars.io/?avatarStyle=Transparent&topType=Turban&accessoriesType=Round&hatColor=Blue03&facialHairType=Blank&clotheType=BlazerShirt&eyeType=Default&eyebrowType=RaisedExcitedNatural&mouthType=Twinkle&skinColor=Light'
     />",
      "<img src='https://avataaars.io/?avatarStyle=Transparent&topType=Hijab&accessoriesType=Round&hatColor=Blue03&facialHairType=Blank&clotheType=BlazerShirt&eyeType=Default&eyebrowType=RaisedExcitedNatural&mouthType=Twinkle&skinColor=Light'
        />",
      "<img src='https://avataaars.io/?avatarStyle=Transparent&topType=LongHairFro&accessoriesType=Round&hairColor=Auburn&facialHairType=Blank&clotheType=BlazerShirt&eyeType=Default&eyebrowType=RaisedExcitedNatural&mouthType=Twinkle&skinColor=Light'
        />",
      "<img src='https://avataaars.io/?avatarStyle=Transparent&topType=ShortHairShortCurly&accessoriesType=Round&hairColor=Auburn&facialHairType=Blank&clotheType=BlazerShirt&eyeType=Default&eyebrowType=RaisedExcitedNatural&mouthType=Twinkle&skinColor=Light'
        />")
  #### LISTEN INPUT CHANGES ####
  ####search for tweets#####
  vals <- reactiveValues(count = -1)
  observeEvent(input$range, vals$count <- vals$count + 1)

  # Listen_search <- reactive({
  #   input$condition==1})
  #
  # Listen_timeline <- reactive({
  #   input$condition==2})

  # Listen_import <- reactive({
  #   !is.null(input$TW_file)
  # })

  ##### REACTIVE NETWORK AND FILTERS ####


  # trigger_birdeye <- eventReactive(input$current_tab, {
  #   req(input$current_tab == 'tab')
  # })

  reactive_sparqlentresult = eventReactive(
    eventExpr = {
      input$sparqltask  | input$runBE# add other condition that triggers query
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE,
    valueExpr = {
      if (TRUE) {
        return(withProgress({
          setProgress(message = "Sending SPARQL query to OKG <br>Please wait...")
          print(input$dateRange2[1])
          print(input$dateRange2[2])
          myresult=  query_and_build_net(target_nodes_ = as.integer(input$slider_nentites),filter_ =ifelse(input$entityfilter=="",NA,input$entityfilter) ,N_THRESHOLD = 0,OFFSET = 0,N_LIMIT = input$slider_nmaxrows,START_DATE = input$dateRange2[1],END_DATE = input$dateRange2[2])
        },
          min = 0,
          max = 2,
          value = 1
        ))}
      }
    )

  reactive_bird_network = reactive({
    req(reactive_sparqlentresult())
    myresult<-reactive_sparqlentresult()
          myresult$network_vis %>%
            visEvents(hoverNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
              ;}") %>%
            visEvents(hoverEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges);
              ;}")
        }
  )

  output$birdresult <-renderVisNetwork(reactive_bird_network())

  #Provide information about selected node
  output$return_BE_node <- renderPrint({
    #data$visN_data$nodes[input$BE_current_node_id,]
    input$current_node_id
  })
  #Provide information about selected node
  output$return_BE_edge  <- renderPrint({
    # data$visN_data$edges[input$BE_current_edge_id,]
    input$current_edge_id
  })

  #Provide information about node positions
  observeEvent(input$store_position, {
    visNetworkProxy("birdresult") %>% visGetPositions()
  })

  nodes_positions <- reactive({
    positions <- input$network_positions
    if(!is.null(positions)){
      nodes_positions <- do.call("rbind", lapply(positions, function(x){ data.frame(x = x$x, y = x$y)}))
      nodes_positions$id <- names(positions)
      nodes_positions
    } else {
      NULL
    }
  })
  #Handle network download
  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste('network-', Sys.Date(),"_filter",input$entityfilter,"_from",input$dateRange2[1],"_to",input$dateRange2[2], '.html', sep='')
    },
    content = function(con) {
      nodes_positions <- nodes_positions()
      net_data <- as_data_frame(reactive_sparqlentresult()$igraph, what = "both")
      if(!is.null(nodes_positions)){
      nodes_save <- merge(net_data$vertices, nodes_positions, by = "id", all = T)
      } else  {
        nodes_save <- net_data$vertices
      }

      visNetwork(nodes = nodes_save, edges =net_data$edges,idToLabel = FALSE,
                 physics = F,type = "square") %>% visNetwork::visEdges(dashes = F,arrows ="", smooth =list(enabled=T,roundness=1,type="discrete"),scaling = list(min=0.25,max=2.5),color = list(color = "lightgray", highlight = "#BF616A", hover = "goldenrod4")) %>%
        visNetwork::visNodes(color = list(background = "lightgray",border="black", highlight = list(border="firebrick",background="#BF616A"), hover = list(border="goldenrod",background='#ffc100')),scaling = list(min= 10, max= 50,label=list(enabled=T,min= 22.5, max= 45,maxVisible= 25,drawThreshold= 5))) %>%
        visPhysics(solver = "hierarchicalRepulsion",hierarchicalRepulsion
                   =list(nodeDistance=275,avoidOverlap=1,springLength=150),minVelocity=1,maxVelocity = 20,stabilization = list(enabled=F)) %>%
        visNetwork::visInteraction(multiselect = T, navigationButtons = T,hover=T,dragNodes = F,dragView = T) %>%
        visNetwork::visOptions(selectedBy = list(variable="DBpedia", multiple="true"),collapse = F,
                               manipulation = list(enabled = TRUE,deleteNode = T, deleteEdge = T,
                                                   editEdgeCols = c("title"),
                                                   editNodeCols = c("title","color")),
                               nodesIdSelection = list(enabled = T),
                               highlightNearest = list(enabled = T),
                               height = "800px",#"fit-content"
                               width = "100%",
                               autoResize = T
        ) %>%
        visEvents(doubleClick =
                    "function(params) {
    var nodeID = params.nodes[0];
    var DBpediaUrl = this.body.nodes[nodeID].options.DBpediaUrl;
    window.open(DBpediaUrl, '_blank');
   }") %>%
        visExport() %>%
        visSave(con)
    }
  )

  #####  SIDEBAR UPDATES ####

  observeEvent(input$toggle_card_sidebar, {
    updateBoxSidebar("mycardsidebar")
  })
  observeEvent(input$BEinfo, {
    input$BEinfo=T
  })

  observeEvent(input$sidebar, {
    toastOpts$class <- if (input$sidebar) "bg-success" else "bg-danger"
    toast(
      title = if (input$sidebar) "Sidebar opened!" else "Sidebar is closed!",
      options = toastOpts
    )
  })

  #####  CONTROLBAR UPDATES ####

  shiny::observeEvent(input$controlbar, {
    toastOpts <- list(
      autohide = TRUE,
      icon = "fas fa-home",
      close = FALSE,
      position = "bottomRight"
    )
    toastOpts$class <- if (input$controlbar) "bg-success" else "bg-danger"
    toast(
      title = if (input$controlbar) "Controlbar opened!" else "Controlbar closed!",
      options = toastOpts
    )

  })

   shiny::observeEvent(input$controlbarToggleBE, {
    updateControlbar(id = "controlbar")
  })

   shiny::observeEvent(input$infobarToggleBE, {
     updateSidebar(id = "BE_info")
   })

  ####CORE ####
  ##### COMPONENT 1: BIRD EYE PERSPECTIVE #####
  # output$BEnetwork <- renderVisNetwork({
  #
  #  sparql_data= query_and_build_net(target_nodes_ = 200,filter_ = "Trump")
  #  sparql_data$network_vis %>%
  #     visEvents(hoverNode = "function(nodes) {
  #       Shiny.onInputChange('current_node_id', nodes);
  #     ;}") %>%
  #     visEvents(hoverEdge = "function(edges) {
  #       Shiny.onInputChange('current_edge_id', edges);
  #     ;}")
  # })



  # observe({
  #   visNetworkProxy("BE_network") %>%
  #     visFocus(id = input$BE_focus, scale = 4)
  # })


  # output[["DT_edges"]] <-  DT::renderDataTable(
  #   expr = {
  #   data$edges %>%
  #     head(.)
  #     }
  # , server = F
  # ,future = T,
  # options=list(scrollX=T,pageLength = 3))

  # output[["DTnodes"]] <-  DT::renderDT(
  #   expr = {
  #     data$nodes[,!(colnames(data$nodes) %in% c("occurences","timestamps","description_text"))] %>%
  #       head(.)
  #   }
  #   , server = T
  #   ,future = F
  #   #,extensions = 'Buttons'
  #   #,dom = 'tpB'
  #   ,options=list(scrollX=T, pageLength = 3,autoWidth = FALSE),selection = list(target = 'row'))

  # eventReactive(input$update_params, {
  #
  #   output$param_buttons <- renderUI({
  #
  #
  #   })
  # }
  #               )
  ##### COMPONENT 2: FINE GRAINED ANALYSIS #####

  ##### COMPONENT 3: SUMMARY STATS #####

  ##### COMPONENT 4: CASE STUDIES #####
  ###### CASE STUDY 1: #####
  ###### CASE STUDY 2: #####
  ###### CASE STUDY 3: #####
  ###### CASE STUDY 4: #####

  ##### HERMIONE SERVER ####
  ###### Hermione: at Intro ####
  observeEvent(input$current_tab, {
    if (input$current_tab == "intro") {
      showModal(modalDialog(
        title = "Welcome to the I.O. & HERMIONE!",
        list(html = tagList(HTML(paste0("<center>",hermione_avatar[sample(1:4,1)],"</center>")),HTML("<br><br>"),
        HTML("<center>Hi internaut!<br>I hope you are doing well.</center><br>My name is HERMIONE and I will be your guide while exploring MUHAI's Inequality Observatory.<br>
        If you need help during your journey just click my icon on the buttom of the screen..."))),
        size="l",
        label="",
        icon=icon("life-ring"),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    }

    ###### Hermione: Cases ####
    if (input$current_tab == "cases") {
      showModal(modalDialog(
        title = "Welcome to the case studies section",
        list(html = tagList(HTML(paste0("<center>",hermione_avatar[sample(1:4,1)],"</center>")),HTML("<br><br>"),
        HTML("Case studies about inequality perception are important to understand the inequality phenomenon because they help to uncover the complex ways in which inequality is experienced, perceived and understood by different groups of people and in different contexts.<br>
Case studies can provide a detailed understanding of the specific factors that, according to people, contribute to specific forms of inequality, such as discrimination, social norms, and structural barriers.<br>
They can also reveal how people respond to inequalities and provide insights into the ways in which inequality is perpetuated or challenged by various actors, such as individuals, institutions and organizations. They can also reveal the role of culture, history, and power relations in shaping inequality.
Finally, case studies can help to highlight the diversity of experiences of multiform inequalities. This is particularly important in addressing intersectionality."))),
        size="l",
        label="",
        icon=icon("life-ring"),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    }
    ###### Hermione: methods ####
    if (input$current_tab == "methods") {
      showModal(modalDialog(
        title = "Welcome to the methods and references section",
        list(html = tagList(HTML(paste0("<center>",hermione_avatar[sample(1:4,1)],"</center>")),HTML("<br><br>"),
                            HTML("Methods for studying inequality and its perception through social media and other online sources are relevant because the web and its deliberative platforms are powerful communication and information dissemination  tools. These methods allow for the collection and analysis of data from a large and diverse sample of people and can provide access to the spontaneous conversations about inequalities, allowing for the identification of emerging issues and trends related to inequalities and intersectionality."))),
        size="l",
        label="",
        icon=icon("life-ring"),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    }
  })
  ##### #################################################### WIP
   ###### Hermione on right panel (clickableable) ####
  # observeEvent(input$controlbar, {
  #  output$hermione_control <- renderImage({
  #    img("https://avataaars.io/?avatarStyle=Transparent&topType=Turban&accessoriesType=Round&hatColor=Blue03&facialHairType=Blank&clotheType=BlazerShirt&eyeType=Default&eyebrowType=RaisedExcitedNatural&mouthType=Twinkle&skinColor=Light")
  #   })
  # })

  ##### Selected rows DTnodes #####
 #  output$test = renderPrint({
 # cat(input[["DTnodes_rows_selected"]], sep = ', ')
 #  })
}
