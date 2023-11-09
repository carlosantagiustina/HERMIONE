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
#' @import reactlog
#' @noRd

#### SERVER PARAMETERS ####
initial_server_parameters <- list(
  twitter_data_from ="01-01-2020",
  twitter_data_to  = "01-01-2022"
)

#### FUNCTIONS ####
##### Functions for Bird's Eye View #####
REQUEST= function(QUERY_,
                  PREFIX_,
                  ENDPOINT_,
                  API_KEY_,
                  START_DATE_,
                  END_DATE_,
                  ENTITY_="",
                  TIMEOUT_=30,
                  #RESOURCE_=RESOURCE,
                  N_THRESHOLD_=2,
                  N_LIMIT_=0,
                  OFFSET_=0){
  #PASTE PREFIXES AND QUERY
  MY_QUERY=paste0(PREFIX_,QUERY_)

  require(httr)
  require(tidytext)
  require(tidyverse)

  #SOBSTITUTE PARAMETERS IF ANY
  if(!is.na(START_DATE_) && grepl(pattern = "[:][:]START[_]DATE[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::START_DATE::",START_DATE_,ignore.case = F,x=.)
  }
  if(!is.na(END_DATE_) && grepl(pattern = "[:][:]END[_]DATE[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::END_DATE::",END_DATE_,ignore.case = F,x=.)
  }
  if(!is.na(N_THRESHOLD_) && grepl(pattern = "[:][:]N_THRESHOLD[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::N_THRESHOLD::",N_THRESHOLD_,ignore.case = F,x=.)
  }

  print(MY_QUERY)
  print(paste0("N_LIMIT_: ",N_LIMIT_))
  print(paste0("OFFSET_: ",OFFSET_))


  if( is.numeric(N_LIMIT_) && N_LIMIT_>0){
    MY_QUERY = MY_QUERY %>%
      gsub("::N_LIMIT::",gsub("[.]$","",x = paste0("LIMIT ",format(N_LIMIT_, scientific = F))),ignore.case = F,x=.)
  }else{
    MY_QUERY = MY_QUERY %>% gsub("::N_LIMIT::","",ignore.case = F,x=.)
  }

  if( !(is.na(ENTITY_) | ENTITY_=="")){
    MY_QUERY = MY_QUERY %>%
      gsub("::CONTAINSENTITYFILTER::",
           paste0('&&
            regex(?entity2,"',gsub(" ","_",ENTITY_),'", "i")'),ignore.case = F,x=.)
    MY_QUERY = MY_QUERY %>%
      gsub("::CONTAINSENTITY::","
      ?id schema:mentions ?entityMention2 .
    ?entity2 nif:anchorOf ?entityMention2 .",ignore.case = F,x=.)
  }else{
    MY_QUERY = MY_QUERY %>% gsub("::CONTAINSENTITYFILTER::","",ignore.case = F,x=.)
    MY_QUERY = MY_QUERY %>% gsub("::CONTAINSENTITY::","",ignore.case = F,x=.)
  }

  if( is.numeric(OFFSET_) && OFFSET_>0){
    MY_QUERY = MY_QUERY %>%
      gsub("::OFFSET::",gsub("[.]$","",x = paste0("\nOFFSET ",format(OFFSET_, scientific = F))),ignore.case = F,x=.)
  }else{
    MY_QUERY = MY_QUERY %>% gsub("::OFFSET::","",ignore.case = F,x=.)
  }

print(MY_QUERY)
  #RUN REQUEST
  REQUEST=httr::GET(ENDPOINT_,timeout(TIMEOUT_),
                    query=list("query"=MY_QUERY),
                    add_headers(
                      #"Accept"= "text/csv"
                      "Accept"= "application/json"
                      ,"Authorization" = paste('Bearer',
                                               API_KEY_,
                                               sep=" ")
                    )
  )

  REQUEST
}

###### Function to build Bird Eye Network ######
build_net=function(ANSWER_=ANSWER,target_nodes,percentile=0.9,
                   power=4,filter=NA){
## Parameters for testing
  # ANSWER_=ANSWER
  # target_nodes=target_nodes_
  # filter=filter_
  # percentile=0.9
  # power=4
##
  require(tidytext)
  require(tidyverse)
  require(quanteda)
  require(igraph)
  require(visNetwork)

  DBPEDIA_DICT = unique(ANSWER_[,c('entity','entityDB')])
  DBPEDIA_DICT=DBPEDIA_DICT  %>% mutate(label=URLdecode(gsub(pattern="_",replacement=" ",x=gsub("http://example.com/ent_","",x = entity))))

  # if(!is.na(filter)){
  #   ANSWER_=ANSWER_[ANSWER_$id %in% unique(ANSWER_$id[ANSWER_$entity %in% DBPEDIA_DICT$entity[grep(pattern = filter,DBPEDIA_DICT$label)]]),]
  #
  #   DBPEDIA_DICT = unique(ANSWER_[,c('entity','entityDB')])
  #   DBPEDIA_DICT=DBPEDIA_DICT  %>% mutate(label=URLdecode(gsub(pattern="_",replacement=" ",x=gsub("http://example.com/ent_","",x = entity))))
  # }

  N_ENTITIES_BY_ID = ANSWER_ %>% group_by(id) %>% summarise(n=n())
  N_ENTITES_BY_ENTITY = ANSWER_ %>% group_by(entity) %>% summarise(n=n())

  DFM= ANSWER_ %>% tidytext::cast_dfm(id, entity, n)
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
  if(nrow(FCM_SELECT)<5){return({
    message("Error: Not enought entity mentions")
    NULL}
    )}
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
  G <- set_vertex_attr(G, 'shape', index = !is.na(entities$entityDB), "square")#
 # G <- set_vertex_attr(G, 'shape', index = V(G), "text")#square
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
  G <- set_edge_attr(G,"width",index = E(G),E(G)$weight^(1/2))
  G <- set_edge_attr(G,"id",index = E(G),  paste0(get.edgelist(G)[,1],"<->",get.edgelist(G)[,2]))
  #G <- set_edge_attr(G,"name",index = E(G),  paste0(get.edgelist(G)[,1],"<->",get.edgelist(G)[,2]))
  G <- set_edge_attr(G,"percentile",index = E(G),fmsb::percentile(E(G)$weight)/100)

  #min edge cooccurrence filter
  #G <- subgraph.edges(G, E(G)[E(G)$weight > 1], delete.vertices = F) # delete all edges that occurr only once should remove about 90% for big graphs

  #target_nodes=100

  if(igraph::vcount(G)>target_nodes){
    threshold_nodes=1-(target_nodes/igraph::vcount(G))
    G <-delete.vertices(graph = G,v = V(G)[V(G)$percentile<threshold_nodes])   # delete all entities that are not in the top 0.05 percentile
  }

  #percentile filter

  if( igraph::graph.density(G)>0.25 & igraph::vcount(G)>25){
    #igraph::ecount(G)/igraph::vcount(G)> 10*(target_nodes/100) |
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
    if(igraph::graph.density(G)>0.25){
    G <- subgraph.edges(G, E(G)[E(G)$weight > 1], delete.vertices = F)
    }

  }
  #remove isolated nodes
  Isolated = which(degree(G)==0)
  G = delete.vertices(G, Isolated)
  #
  NETWORK=G %>%
    visNetwork::visIgraph(
      .,
      idToLabel = FALSE,
      physics = T
      #,type = "square"
    ) %>% visNetwork::visEdges(
      dashes = F,
      arrows ="",
      smooth =list(enabled=T,roundness=1,type="discrete"),
      scaling = list(min=0.25,max=3.5),
      color = list(color = "lightgray", highlight = "#BF616A", hover = "goldenrod4")#color = "lightgray"
      ) %>%
    visNetwork::visNodes(color = list(background = "lightgray",border="black", highlight = list(border="firebrick",background="#BF616A"), hover = list(border="goldenrod",background='#ffc100')),scaling = list(min= 10, max= 50,label=list(enabled=T,min= 22.5, max= 45,maxVisible= 25,drawThreshold= 5))) %>%
    visPhysics(solver = "hierarchicalRepulsion",hierarchicalRepulsion
               =list(nodeDistance=275,avoidOverlap=1,springLength=150),minVelocity=1,maxVelocity = 20,stabilization = list(enabled=F)) %>%
    visNetwork::visInteraction(multiselect = F, navigationButtons = T,hover=T,dragNodes = F,dragView = T) %>%
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

#function to extract content from stacked httr resuts inserted in list
content_extraction=function (x, as = NULL, type = NULL, encoding = NULL, ...) {
  type <- type %||% x$headers[["Content-Type"]] %||% mime::guess_type(x$url,
                                                                      empty = "application/octet-stream")
  as <- as %||% httr:::parseability(type)
  as <- match.arg(as, c("raw", "text", "parsed"))
  if (httr:::is.path(x$content)) {
    raw <- httr::  readBin(x$content, "raw", file.info(x$content)$size)
  }
  else {
    raw <- x$content
  }
  switch(as, raw = raw, text = parse_text(raw, type, encoding),
         parsed = httr:::parse_auto(raw, type, encoding, ...))
}


query_and_build_net_pagination=function(target_nodes_=100,
                             filter_=NA,
                             ENTITY="",
                             START_DATE="2018-01-01",
                             END_DATE="2024-01-01" ,
                             N_THRESHOLD=2,
                             OFFSET=0,
                             N_LIMIT=10000){

  #parameters for testing purpose
  # target_nodes_=100
  # filter_="Netherlands"
  # START_DATE="2018-01-01"
  # END_DATE="2024-01-01"
  # N_THRESHOLD=2
  #OFFSET=0
  #N_LIMIT=0

  require(httr)
  require(tidytext)
  require(tidyverse)
  require(quanteda)
  require(igraph)
  require(visNetwork)


  PREFIX='
PREFIX observatory: <https://www.w3id.org/okg/obio-ontology/>
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
SELECT ?id ?entity ?entityDB ?date_time ?sentiment ?like ?retweet ?polarity ?subjectivity (COUNT(?id) as ?n)
WHERE {
    ?id schema:mentions ?entityMention ;
        dc:created ?date_time .
    ?entity nif:anchorOf ?entityMention . ::CONTAINSENTITY::
    ?id observatory:sentiment_label ?sentiment .
    ?id observatory:nb_like ?like .
    ?id observatory:polarity_score ?polarity .
    ?id observatory:subjectivity_score ?subjectivity .
    ?id observatory:nb_repost ?retweet .
    OPTIONAL {?entity nee:hasMatchedURL ?entityDB .}
          FILTER (
        "::START_DATE::"^^xsd:dateTime < ?date_time &&
          ?date_time < "::END_DATE::"^^xsd:dateTime ::CONTAINSENTITYFILTER::)
}
GROUP BY ?id ?entity ?entityDB  ?date_time ?sentiment ?like ?retweet ?polarity ?subjectivity
ORDER BY ?date_time ?id ?entity ?entityDB
::N_LIMIT:: ::OFFSET::'
EMPTY=F
MYREQUEST=list()
COUNTER=1
while(!EMPTY){

  MYREQUEST[[COUNTER]]=REQUEST(
    QUERY_=QUERY,
    PREFIX_=PREFIX,
    API_KEY_ =read_file(paste0(Sys.getenv("HOME"),"/HERMIONE_KEY.txt")),
    START_DATE_=START_DATE,
    END_DATE_=END_DATE,
    ENTITY_=ENTITY,
    TIMEOUT_=60,
    N_THRESHOLD_=N_THRESHOLD,
    N_LIMIT_ = N_LIMIT,
    OFFSET_ = OFFSET,
    ENDPOINT_ = "https://api.druid.datalegend.net/datasets/lisestork/OKG/services/OKG/sparql")
  print(paste0("PAGINATION_COUNTER:",COUNTER))
  print(paste0("NROWS:",length(content_extraction(MYREQUEST[[COUNTER]]))))
  #if(COUNTER>4){EMPTY=T}
  if(length(content_extraction(MYREQUEST[[COUNTER]]))<N_LIMIT){EMPTY=T}
  #if(!is.data.frame(content(MYREQUEST[[COUNTER]],flatten = T, simplifyDataFrame=T,simplifyVector=T))){EMPTY=T}
  COUNTER=COUNTER+1
  OFFSET=OFFSET+N_LIMIT
}
MYREQUEST

}

query_and_build_net=function(target_nodes_=100,
                             filter_="",
                             START_DATE="2018-01-01",
                             END_DATE="2024-01-01" ,
                             N_THRESHOLD=2,
                             OFFSET=0,
                             N_LIMIT=10000){
ENTITY=filter_
  #parameters for testing purpose
  # target_nodes_=100
  # filter_="Netherlands"
  # START_DATE="2018-01-01"
  # END_DATE="2024-01-01"
  # N_THRESHOLD=2
  #OFFSET=0
  #N_LIMIT=0

  require(httr)
  require(tidytext)
  require(tidyverse)
  require(quanteda)
  require(igraph)
  require(visNetwork)


  PREFIX='
PREFIX observatory: <https://www.w3id.org/okg/obio-ontology/>
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
SELECT ?id ?entity ?entityDB ?date_time ?sentiment ?like ?retweet ?polarity ?subjectivity (COUNT(?id) as ?n)
WHERE {
    ?id schema:mentions ?entityMention ;
        dc:created ?date_time .
    ?entity nif:anchorOf ?entityMention . ::CONTAINSENTITY::
    ?id observatory:sentiment_label ?sentiment .
    ?id observatory:nb_like ?like .
    ?id observatory:polarity_score ?polarity .
    ?id observatory:subjectivity_score ?subjectivity .
    ?id observatory:nb_repost ?retweet .
    OPTIONAL {?entity nee:hasMatchedURL ?entityDB .}
          FILTER (
        "::START_DATE::"^^xsd:dateTime < ?date_time &&
          ?date_time < "::END_DATE::"^^xsd:dateTime ::CONTAINSENTITYFILTER::)
}
GROUP BY ?id ?entity ?entityDB  ?date_time ?sentiment ?like ?retweet ?polarity ?subjectivity
ORDER BY RAND()
::N_LIMIT:: ::OFFSET::'
#ORDER BY RAND() or ORDER BY ?date_time ?id ?entity ?entityDB
MYREQUEST=REQUEST(QUERY_=QUERY,
                  PREFIX_=PREFIX,
                  ENTITY_=ENTITY,
                  API_KEY_ =read_file(paste0(Sys.getenv("HOME"),"/HERMIONE_KEY.txt")),
                  START_DATE_=START_DATE,
                  END_DATE_=END_DATE,
                  TIMEOUT_=60,
                 # RESOURCE_=RESOURCE,
                  N_THRESHOLD_=N_THRESHOLD,
                  N_LIMIT_ = N_LIMIT,
                 OFFSET_ = OFFSET,
                ENDPOINT_ = "https://api.druid.datalegend.net/datasets/lisestork/OKG/services/OKG/sparql")
ANSWER=content(MYREQUEST)
if(!is.data.frame(content(MYREQUEST,flatten = T, simplifyDataFrame=T,simplifyVector=T))){return()}
ANSWER=content(MYREQUEST,flatten = T, simplifyDataFrame=T,simplifyVector=T) %>% readr::type_convert()
results=build_net(ANSWER_  = ANSWER,
                  target_nodes = target_nodes_
                  #,filter = filter_
                  )
#saveRDS(results,file = paste0("results",Sys.Date(),".RDS"))
results[["my_request"]]=list()
results[["my_request"]][["url"]]= MYREQUEST$url %>% URLdecode()
results[["my_request"]][["status_code"]]= MYREQUEST$status_code
results[["my_request"]][["headers"]]= MYREQUEST$headers
results[["my_request"]][["request"]]= MYREQUEST$request
results[["my_request"]][["date"]]= MYREQUEST$date
results[["my_request"]][["times"]]= MYREQUEST$times
results
}

##### Functions for FineGrained analysis ####


REQUEST_FG= function(QUERY_,
                     PREFIX_,
                     ENDPOINT_=ENDPOINT,
                     API_KEY_=API_KEY,
                     START_DATE_=START_DATE,
                     END_DATE_=END_DATE,
                     ENTITY_=ENTITY,
                     ENTITY_1_=ENTITY_1,
                     ENTITY_2_=ENTITY_2,
                     TIMEOUT_=30,
                     #RESOURCE_=RESOURCE,
                     N_THRESHOLD_=2,
                     N_LIMIT_=0,
                     OFFSET_=0){
  #PASTE PREFIXES AND QUERY
  MY_QUERY=paste0(PREFIX_,QUERY_)

  require(httr)
  require(tidytext)
  require(tidyverse)

  #SOBSTITUTE PARAMETERS IF ANY
  if(!is.na(START_DATE_) && grepl(pattern = "[:][:]START[_]DATE[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::START_DATE::",START_DATE_,ignore.case = F,x=.)
  }
  if(!is.na(END_DATE_) && grepl(pattern = "[:][:]END[_]DATE[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::END_DATE::",END_DATE_,ignore.case = F,x=.)
  }
  if(!is.na(N_THRESHOLD_) && grepl(pattern = "[:][:]N_THRESHOLD[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::N_THRESHOLD::",N_THRESHOLD_,ignore.case = F,x=.)
  }
  if(!is.na(ENTITY_1_) && grepl(pattern = "[:][:]ENTITY_1[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::ENTITY_1::",ENTITY_1_,ignore.case = F,x=.)
  }
  if(!is.na(ENTITY_2_) && grepl(pattern = "[:][:]ENTITY_2[:][:]",x = QUERY_,perl = T,ignore.case = F)){
    MY_QUERY = MY_QUERY %>%
      gsub("::ENTITY_2::",ENTITY_2_,ignore.case = F,x=.)
  }

  print(MY_QUERY)
  print(paste0("N_LIMIT_: ",N_LIMIT_))
  print(paste0("OFFSET_: ",OFFSET_))
  print(paste0("N_LIMIT_: ",N_LIMIT_))
  print(paste0("OFFSET_: ",OFFSET_))

  if( is.numeric(N_LIMIT_) && N_LIMIT_>0){
    MY_QUERY = MY_QUERY %>%
      gsub("::N_LIMIT::",gsub("[.]$","",x = paste0("LIMIT ",format(N_LIMIT_, scientific = F))),ignore.case = F,x=.)
  }else{
    MY_QUERY = MY_QUERY %>% gsub("::N_LIMIT::","",ignore.case = F,x=.)
  }

  if( !(is.na(ENTITY_) | ENTITY_=="")){
    MY_QUERY = MY_QUERY %>%
      gsub("::CONTAINSENTITYFILTER::",
           paste0('&&
            regex(?entity2,"',gsub(" ","_",ENTITY_),'", "i")'),ignore.case = F,x=.)
    MY_QUERY = MY_QUERY %>%
      gsub("::CONTAINSENTITY::","
      ?id schema:mentions ?entityMention2 .
    ?entity2 nif:anchorOf ?entityMention2 .",ignore.case = F,x=.)
  }else{
    MY_QUERY = MY_QUERY %>% gsub("::CONTAINSENTITYFILTER::","",ignore.case = F,x=.)
    MY_QUERY = MY_QUERY %>% gsub("::CONTAINSENTITY::","",ignore.case = F,x=.)
  }

  if( is.numeric(OFFSET_) && OFFSET_>0){
    MY_QUERY = MY_QUERY %>%
      gsub("::OFFSET::",gsub("[.]$","",x = paste0("\nOFFSET ",format(OFFSET_, scientific = F))),ignore.case = F,x=.)
  }else{
    MY_QUERY = MY_QUERY %>% gsub("::OFFSET::","",ignore.case = F,x=.)
  }

  print(MY_QUERY)
  #RUN REQUEST_FG
  REQUEST=httr::GET(ENDPOINT_,timeout(TIMEOUT_),
                    query=list("query"=MY_QUERY),
                    add_headers(
                      #"Accept"= "text/csv"
                      "Accept"= "application/json"
                      ,"Authorization" = paste('Bearer',
                                               API_KEY_,
                                               sep=" ")
                    )
  )

  REQUEST
}

selected_entities=c("<http://dbpedia.org/resource/Racism>","<http://dbpedia.org/resource/Economic_inequality>")
#<http://dbpedia.org/resource/Racism>  <http://dbpedia.org/resource/Economic_inequality>
QUERY_FG =
  '
SELECT DISTINCT ?ent1 ?entityDB1 ?entityMention1 ?ent2 ?entityDB2 ?entityMention2 ?date  ?id ?token ?word ?pos ?index ?dep_prop ?token_dep ?index_dep ?word_dep  ?entity ?entityDB ?refers_to ?tokenrole ?arg
WHERE {
  ?id schema:mentions ?entityMention1 .
  ?ent1 nif:anchorOf ?entityMention1 ;
        nee:hasMatchedURL ?entityDB1 .
  ?ent1 nee:hasMatchedURL ::ENTITY_1:: .
  ?id schema:mentions ?entityMention2 .
  ?ent2 nif:anchorOf ?entityMention2 ;
        nee:hasMatchedURL ?entityDB2 .
  ?ent2 nee:hasMatchedURL ::ENTITY_2:: .
  ?id dc:created ?date .
  ?id  nif:sentence ?sentence .
  ?sentence nif:word ?token .
  ?token nif:lemma ?word .
  ?token nif:posTag ?pos .
  ?token observatory:hasTokenIndex ?index .
  OPTIONAL{?token nif:superString ?refers_to .
           ?entity nif:anchorOf ?refers_to ;
                   nee:hasMatchedURL ?entityDB .}
    OPTIONAL{?dep_prop rdfs:subPropertyOf observatory:dependency_relation .
  ?token_dep ?dep_prop ?token .
  ?token_dep observatory:hasTokenIndex ?index_dep .
  ?token_dep rdf:value ?word_dep .}
    OPTIONAL{ ?role_inst wsj:onRoleSet ?tokenrole ;
                       observatory:onToken ?token ;
                       wsj:withmappedrole ?arg . }
}
ORDER BY DESC(?id)
limit 1000
'
PREFIX_FG='
PREFIX observatory: <https://www.w3id.org/okg/obio-ontology/>
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

fg_analysis=function(QUERY=QUERY_FG,
                     ENTITY_1,
                     ENTITY_2,
                     PREFIX=PREFIX_FG,
                     API_KEY =read_file(paste0(Sys.getenv("HOME"),"/HERMIONE_KEY.txt")),
                     START_DATE,
                     END_DATE,...){

  library(visNetwork)
  library(igraph)
  library(httr)
  library(tidytext)
  library(tidyverse)
  #PARAMS
  # ENTITY_1 = "<http://dbpedia.org/resource/Donald_Trump>"
  # ENTITY_2 = "<http://dbpedia.org/resource/Economic_inequality>"
  # START_DATE = "2018-01-01"
  # END_DATE = "2024-01-01"
  # API_KEY =read_file(paste0(Sys.getenv("HOME"),"/HERMIONE_KEY.txt"))
  # QUERY=QUERY_FG
  # PREFIX=PREFIX_FG
  # API_KEY =read_file(paste0(Sys.getenv("HOME"),"/HERMIONE_KEY.txt"))

  #using lemma as word:   ?token rdf:value ?word .

  if(ENTITY_1==ENTITY_2){
    return(showModal(modalDialog(paste("Error:","Selected entities are identical. Please select two different entities."),,easyClose = TRUE)))
  }

  selected_entities=c(ENTITY_1,ENTITY_2)

  MYREQUEST=REQUEST_FG(TIMEOUT_ = 120,QUERY_ = QUERY,ENTITY_1_ = ENTITY_1,ENTITY_2_ = ENTITY_2,PREFIX_ = PREFIX,API_KEY_ = API_KEY,START_DATE_ = START_DATE,END_DATE_ = END_DATE,ENDPOINT_  = "https://api.druid.datalegend.net/datasets/lisestork/OKG/services/OKG/sparql",N_THRESHOLD_ = NA,ENTITY_ = NA)


  ANSWER = tryCatch({
    content(MYREQUEST,flatten = T, simplifyDataFrame=T,simplifyVector=T) %>% readr::type_convert()
    },
                    error=function(e){showModal(modalDialog(paste("Error:","The number of posts mentioning the two selected entities is insufficient or null.  Network cannot be built, please select another pair of entities."),,easyClose = TRUE))},
                  warning=function(w) {}
    )
  if(!is.data.frame(ANSWER )){
    return()
  }
  ANSWER_BKP=ANSWER

  ANSWER = ANSWER %>% mutate(token_clean=gsub(pattern="^[^%]{1,}[%]{1}[2][3]",replacement="",x=token,perl = T))
  ANSWER =ANSWER %>% arrange(.,desc(id),index)
  ANSWER=ANSWER[!is.na(ANSWER[,c("word")]),]

  ENTITY_TOKEN_DICT_A= ANSWER[,c(1:3)]
  ENTITY_TOKEN_DICT_A=ENTITY_TOKEN_DICT_A[!duplicated(ENTITY_TOKEN_DICT_A),]
  colnames(ENTITY_TOKEN_DICT_A)=c("entity","entityDB" ,"entityMention")

  ENTITY_TOKEN_DICT_B= ANSWER[,c(4:6)]
  ENTITY_TOKEN_DICT_B=ENTITY_TOKEN_DICT_B[!duplicated(ENTITY_TOKEN_DICT_B),]
  colnames(ENTITY_TOKEN_DICT_B)=c("entity","entityDB" ,"entityMention")

  ENTITY_TOKEN_DICT_C= ANSWER[,c(17:19)]
  ENTITY_TOKEN_DICT_C=ENTITY_TOKEN_DICT_C[!duplicated(ENTITY_TOKEN_DICT_C),]
  colnames(ENTITY_TOKEN_DICT_C)=c("entity","entityDB" ,"entityMention")

  ENTITY_TOKEN_DICT=rbind(ENTITY_TOKEN_DICT_A,ENTITY_TOKEN_DICT_B,ENTITY_TOKEN_DICT_C)

  colnames(ENTITY_TOKEN_DICT)=c("label","entity","id")
  ENTITY_TOKEN_DICT=ENTITY_TOKEN_DICT[!is.na(ENTITY_TOKEN_DICT$label),]
  ENTITY_TOKEN_DICT=ENTITY_TOKEN_DICT[!duplicated(ENTITY_TOKEN_DICT),]

  EDGE_DB_tokens=ANSWER[!(is.na(ANSWER$word_dep) | ANSWER$pos=="PUNCT"),c("token","token_dep","id","dep_prop")]

  colnames(EDGE_DB_tokens)=c("from","to","tweet_id","title")
  EDGE_DB_tokens$title=gsub(pattern = "https://www.w3id.org/okg/obio-ontology/dep_rel_",replacement = "",EDGE_DB_tokens$title)
  EDGE_DB_tokens$type="dep_rel"
  EDGE_DB_tokens$id=paste0(EDGE_DB_tokens$from,"-",EDGE_DB_tokens$type,":",EDGE_DB_tokens$title,"-",EDGE_DB_tokens$to)
  EDGE_DB_tokens$color="#E5E4E2"
    EDGE_DB_tokens$hidden=F

    EDGE_DB_entities=ANSWER[!(ANSWER$pos=="PUNCT" |is.na(ANSWER$refers_to)),c("refers_to","token","id","word")]
    colnames(EDGE_DB_entities)=c("from","to","tweet_id","title")
    EDGE_DB_entities$type="refers_to"
    EDGE_DB_entities$id=paste0(EDGE_DB_entities$from,"-",EDGE_DB_entities$type,":",EDGE_DB_entities$title,"-",EDGE_DB_entities$to)
    EDGE_DB_entities$color="#BF616A"
      EDGE_DB_entities$hidden=F

      EDGE_DB_frames=ANSWER[!is.na(ANSWER$tokenrole) ,c("token","arg","id","tokenrole")]
      colnames(EDGE_DB_frames)=c("from","to","tweet_id","type")
      EDGE_DB_frames$title=gsub("^https://w3id.org/framester/pb/data/|^http://example.org/muhai/observatory#roleset/|[.][0-9]{1,}$",replacement = "",EDGE_DB_frames$type)
      EDGE_DB_frames$type="frame"
      EDGE_DB_frames$id=paste0(EDGE_DB_frames$from,"-",EDGE_DB_frames$type,":",EDGE_DB_frames$title,"-",EDGE_DB_frames$to)
      EDGE_DB_frames$color="#C1E3ED"
        EDGE_DB_frames$hidden=F

        VERTICES_DB_frames_roots=tibble(id=unique(EDGE_DB_frames$title) ,
                                        label=unique(EDGE_DB_frames$title),
                                        tweet_id=NA,
                                        group=NA,
                                        shape="circle",
                                        color=c("#C1E3ED"),
                                        hidden=F,
                                        level=NA
                                        #,url=gsub("^[<]?|[>]?$","",selected_entities)
        )
        EDGE_DB_frames_roots=eval(parse(text=paste0("tibble(",paste0(colnames(EDGE_DB_frames),"=NA",collapse = ", "),", .rows =",  length( which( EDGE_DB_frames$title %in% VERTICES_DB_frames_roots$label))*2,")")))
        EDGE_DB_frames_roots=as.data.frame(EDGE_DB_frames_roots)
        #####
        EDGE_DB_frames_roots$from=rep(EDGE_DB_frames$title,2)
        EDGE_DB_frames_roots$to=c(EDGE_DB_frames$from,EDGE_DB_frames$to)
        EDGE_DB_frames_roots$tweet_id=rep(EDGE_DB_frames$tweet_id,2)
        EDGE_DB_frames_roots$title=EDGE_DB_frames_roots$from
        EDGE_DB_frames_roots$type="SelectedFrame"
        EDGE_DB_frames_roots$color="rgba(255,255,255, 0)"
        EDGE_DB_frames_roots$id= paste0(EDGE_DB_frames_roots$from,"-",EDGE_DB_frames_roots$type,":",EDGE_DB_frames_roots$title,"-",EDGE_DB_frames_roots$to)
        EDGE_DB_frames_roots$hidden=F

        #####
        EDGE_DB=rbind(EDGE_DB_tokens,EDGE_DB_entities, EDGE_DB_frames,EDGE_DB_frames_roots)

        EDGE_DB=EDGE_DB[!(is.na(EDGE_DB$from)|is.na(EDGE_DB$to)),]
        EDGE_DB=EDGE_DB[!duplicated(EDGE_DB),]

        ANSWER_ND=ANSWER[!duplicated(ANSWER$token),]



        rownames(ANSWER_ND)=ANSWER_ND$token
        VERTICES_DB_tokens= ANSWER_ND[unique(c(EDGE_DB$from,EDGE_DB$to)),c("token","word","id")]
        colnames(VERTICES_DB_tokens)=c("id","label","tweet_id")
        VERTICES_DB_tokens=VERTICES_DB_tokens[!is.na(VERTICES_DB_tokens$id),]
        VERTICES_DB_tokens$group=paste0("word_", VERTICES_DB_tokens$tweet_id)
        VERTICES_DB_tokens$shape="text"
        VERTICES_DB_tokens$color="black"
          VERTICES_DB_tokens$hidden=FALSE
          VERTICES_DB_tokens$level=({gsub(pattern =  "http://example[.]com/tweet_",replacement ="",x = VERTICES_DB_tokens$tweet_id) %>% as.factor() %>% as.numeric() +0.75 })*1.5 + (gsub(pattern =  "^.*%23",replacement ="",x = VERTICES_DB_tokens$id) %>% as.factor() %>% as.numeric() / (max({gsub(pattern =  "^.*%23",replacement ="",x = VERTICES_DB_tokens$id) %>% as.factor() %>% as.numeric()})+1))*1.5

          VERTICES_DB_entities= data.frame(ANSWER_ND[!is.na(ANSWER_ND$refers_to) & !duplicated(ANSWER_ND$refers_to),c("refers_to", "id")])
          colnames(VERTICES_DB_entities)=c("id","tweet_id")
          VERTICES_DB_entities=left_join(VERTICES_DB_entities,ENTITY_TOKEN_DICT[,-c(1)],by="id")
          colnames(VERTICES_DB_entities)[3]=c("label")
          VERTICES_DB_entities=VERTICES_DB_entities[c("id","label","tweet_id")]
          VERTICES_DB_entities$group=paste0("entity_", VERTICES_DB_entities$tweet_id)
          VERTICES_DB_entities$label=gsub("^http://dbpedia.org/resource/","",VERTICES_DB_entities$label)
          VERTICES_DB_entities$label=gsub("_"," ",VERTICES_DB_entities$label)
          VERTICES_DB_entities$shape="box"
          VERTICES_DB_entities$color="#BF616A"
            VERTICES_DB_entities$hidden=F
            VERTICES_DB_entities$level=2
            #VERTICES_DB_entities$selectible=F

            VERTICES_DB_entities_roots=tibble(id=selected_entities,
                                              label=gsub("_"," ",gsub("^[<]?http://dbpedia.org/resource/|[>]?$","",selected_entities)),
                                              tweet_id=NA,
                                              group=NA,
                                              shape="circle",
                                              color=c("#bdd9bf","#ffc857"),
                                              hidden=F
                                              #,url=gsub("^[<]?|[>]?$","",selected_entities)
            )

            #EDGES_DB_entities_roots=eval(parse(text=paste0("tibble(",paste0(colnames(EDGE_DB),paste0("=",sapply(EDGE_DB, class),"()"),collapse = ", "),", .rows =",  length( which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label)),")")))


            EDGES_DB_entities_roots=eval(parse(text=paste0("tibble(",paste0(colnames(EDGE_DB),"=NA",collapse = ", "),", .rows =",  length( which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label)),")")))
            EDGES_DB_entities_roots=as.data.frame(EDGES_DB_entities_roots)

            EDGES_DB_entities_roots$from[1:length( which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label[1]))]=VERTICES_DB_entities_roots$id[1]

            EDGES_DB_entities_roots[1:length( which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label[1])),c("to","tweet_id","title")]=VERTICES_DB_entities[which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label[1]),c("id","tweet_id","label")]

            EDGES_DB_entities_roots$from[(length(which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label[1]))+1):nrow(EDGES_DB_entities_roots)]=VERTICES_DB_entities_roots$id[2]
            EDGES_DB_entities_roots[(length( which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label[1]))+1):nrow(EDGES_DB_entities_roots),c("to","tweet_id","title")]=VERTICES_DB_entities[which(VERTICES_DB_entities$label %in% VERTICES_DB_entities_roots$label[2]),c("id","tweet_id","label")]
            EDGES_DB_entities_roots$type="SelectedEntity"
            EDGES_DB_entities_roots$color="rgba(255,255,255, 0)"
            EDGES_DB_entities_roots$id= paste0(EDGES_DB_entities_roots$from,"-",EDGES_DB_entities_roots$type,":",EDGES_DB_entities_roots$title,"-",EDGES_DB_entities_roots$to)
            EDGES_DB_entities_roots$hidden=F

            #VERTICES_DB_entities_roots$label=VERTICES_DB_entities_roots$id
            VERTICES_DB_entities_roots$level=0.5
            VERTICES_DB=rbind(VERTICES_DB_tokens,VERTICES_DB_entities, VERTICES_DB_entities_roots,VERTICES_DB_frames_roots)

            rownames(VERTICES_DB)=VERTICES_DB$id

            EDGE_DB=rbind(EDGE_DB,EDGES_DB_entities_roots)

            #EDGE_DB[EDGE_DB$from %in% EDGES_DB_entities_roots$to[EDGES_DB_entities_roots$from==selected_entities[1]],"from"]=selected_entities[1]
            #EDGE_DB[EDGE_DB$from %in% EDGES_DB_entities_roots$to[EDGES_DB_entities_roots$from==selected_entities[2]],"from"]=selected_entities[2]
            #VERTICES_DB=VERTICES_DB[!(VERTICES_DB$id %in% EDGES_DB_entities_roots$to),]

            EDGE_DB=EDGE_DB[EDGE_DB$from %in% VERTICES_DB$id & EDGE_DB$to %in% VERTICES_DB$id, ]

            GRAPH= graph_from_data_frame(d = EDGE_DB, directed = TRUE, vertices =VERTICES_DB)

            paths1=all_simple_paths(
              GRAPH,
              from = c(selected_entities[1]),
              to = c(selected_entities[2]),#,VERTICES_DB_entities$id
              mode = c("all")
            )

            paths2=all_simple_paths(
              GRAPH,
              from = c(selected_entities[2]),
              to = c(selected_entities[1]),#,VERTICES_DB_entities$id
              mode = c("all")
            )

            neighbors1= sapply(VERTICES_DB_entities$id, function(x) neighbors(GRAPH,x , mode = c("all"))) %>% unname() %>% unlist() %>% names()

            neighbors2=sapply(neighbors1, function(x) neighbors(GRAPH,x , mode = c("all"))) %>% unname() %>% unlist() %>% names()

            neighbors3=sapply(neighbors2, function(x) neighbors(GRAPH,x , mode = c("all"))) %>% unname() %>% unlist() %>% names()

            SUBGRAPH=subgraph(GRAPH, unique(c(EDGE_DB_frames$from[EDGE_DB_frames$from %in% VERTICES_DB$id],EDGE_DB_frames$to[EDGE_DB_frames$to %in% VERTICES_DB$id], neighbors1,neighbors2, neighbors3,names(unlist(paths1)),names(unlist(paths2))) ))
            #SUBGRAPH=subgraph(GRAPH, unique(c(neighbors1,neighbors2, neighbors3,names(unlist(paths1)),names(unlist(paths2))) ))

            components <- igraph::clusters(SUBGRAPH, mode="weak")
            biggest_cluster_id <- which.max(components$csize)

            # ids
            vert_ids <- V(SUBGRAPH)[components$membership == biggest_cluster_id]

            # subgraph
            SUBGRAPH=igraph::induced_subgraph(SUBGRAPH, vert_ids)
            #vertex_attr(SUBGRAPH,"level")
            SUBGRAPH=set_vertex_attr(SUBGRAPH,"level",index = vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_frames_roots$id],value = max(vertex_attr(SUBGRAPH,"level"),na.rm = T)+1)



            ancestor <- function(g, vs) {
              da <- subset(colSums(distances(g, mode = "out")[vs, ]), !names(V(g)) %in% vs)
              if (all(is.infinite(da))) {
                return("None")
              }
              names(which(da == min(da)))
            }

            paths_roots=tryCatch({
              ancestor(SUBGRAPH,c(selected_entities[1],selected_entities[2]))
            },
            error=function(e){showModal(modalDialog(paste("Error:","The number of posts mentioning the two selected entities is insufficient or null.  Network cannot be built, please select another pair of entities."),,easyClose = TRUE))},
            warning=function(w) {}
            )


            #Subset graph keeping Tweets with direct paths between entities

            n_tweets=length(unique(gsub("http://example.com/ent_|http://example.com/token_|[%]{1}23[0-9%A-Z]{1,}$","",vertex_attr(SUBGRAPH,"name",index =   grepl("http://example.com/ent_|http://example.com/token_",vertex_attr(SUBGRAPH,"name"))))))

            #vertex_attr(SUBGRAPH,"name")[grepl(pattern = "http://example[.]com/ent_",x = vertex_attr(SUBGRAPH,"name"))]
            if(n_tweets>7 && length(paths_roots)>=6){
              SUBGRAPH=subgraph(SUBGRAPH, c(selected_entities,vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_frames_roots$id],vertex_attr(SUBGRAPH,"name")[gsub("http://example.com/ent_|http://example.com/token_|[%]{1}23[0-9%A-Z]{1,}$","",vertex_attr(SUBGRAPH,"name")) %in%  gsub("http://example.com/ent_|http://example.com/token_|[%]{1}23[0-9%A-Z]{1,}$","",paths_roots)]))

              #vertex_attr(SUBGRAPH,"level",index = grepl("http://example.com/token_",x = vertex_attr(SUBGRAPH,"name")))

              SUBGRAPH=set_vertex_attr(SUBGRAPH,"level",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")),value = ({gsub(pattern =  "http://example[.]com/tweet_",replacement ="",x = vertex_attr(SUBGRAPH,"tweet_id",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() +0.75 })*1.5 + (gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() / (max({gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric()})+1))*1.5)
              SUBGRAPH=set_vertex_attr(SUBGRAPH,"level",index = vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_frames_roots$id],value = max(({gsub(pattern =  "http://example[.]com/tweet_",replacement ="",x = vertex_attr(SUBGRAPH,"tweet_id",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() +0.75 })*1.5 + (gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() / (max({gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric()})+1))*1.5)+1)
            }

            n_tweets=length(unique(gsub("http://example.com/ent_|http://example.com/token_|[%]{1}23[0-9%A-Z]{1,}$","",vertex_attr(SUBGRAPH,"name",index =   grepl("http://example.com/ent_|http://example.com/token_",vertex_attr(SUBGRAPH,"name"))))))
            if(n_tweets>7){
              sample=gsub("http://example.com/ent_|http://example.com/token_|[%]{1}23[0-9%A-Z]{1,}$","",paths_roots)[1:6]
              #which(is.na(sample))
              sample_add= unique(setdiff(x = gsub("http://example.com/ent_|http://example.com/token_|[%]{1}23[0-9%A-Z]{1,}$","",vertex_attr(SUBGRAPH,"name",index =   grepl("http://example.com/ent_|http://example.com/token_",vertex_attr(SUBGRAPH,"name")))),y=sample[which(!is.na(sample))]))

              if(length(sample_add)<length(which(is.na(sample)))){
                sample[which(is.na(sample))]= c(sample_add, rep(NA,length(which(is.na(sample)))-length(sample_add)))
              }else{
                sample[which(is.na(sample))]= sample(sample_add,length(which(is.na(sample))))
              }

              sample=sample[!is.na(sample)]

              SUBGRAPH=subgraph(SUBGRAPH, c(selected_entities,vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_frames_roots$id],vertex_attr(SUBGRAPH,"name")[gsub("http://example.com/ent_|http://example.com/token_|[%]{1}23[0-9%A-Z]{1,}$","",vertex_attr(SUBGRAPH,"name")) %in%  sample]))

              SUBGRAPH=set_vertex_attr(SUBGRAPH,"level",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")),value = ({gsub(pattern =  "http://example[.]com/tweet_",replacement ="",x = vertex_attr(SUBGRAPH,"tweet_id",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() +0.75 })*1.5 + (gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() / (max({gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric()})+1))*1.5)
              SUBGRAPH=set_vertex_attr(SUBGRAPH,"level",index = vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_frames_roots$id],value = max(({gsub(pattern =  "http://example[.]com/tweet_",replacement ="",x = vertex_attr(SUBGRAPH,"tweet_id",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() +0.75 })*1.5 + (gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric() / (max({gsub(pattern =  "^.*%23",replacement ="",x = vertex_attr(SUBGRAPH,"name",index = grepl("http://example[.]com/token_",x = vertex_attr(SUBGRAPH,"name")))) %>% as.factor() %>% as.numeric()})+1))*1.5)+1)
            }

            components <- igraph::clusters(SUBGRAPH, mode="weak")
            biggest_cluster_id <- which.max(components$csize)

            # ids
            vert_ids <- V(SUBGRAPH)[components$membership == biggest_cluster_id]

            # subgraph
            SUBGRAPH=igraph::induced_subgraph(SUBGRAPH, vert_ids)

            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index = vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_frames_roots$id],25)

            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.color", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="NOUN"],"white")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.background", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="NOUN"],"#006ddb")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="NOUN"],15)

            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.color", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="ADJ"|ANSWER$pos=="ADV"],"white")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.background", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="ADJ"|ANSWER$pos=="ADV"],"#b66dff")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="ADJ"|ANSWER$pos=="ADV"],15)


            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.color", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="VERB"],"white")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.background", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="VERB"],"black")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index = vertex_attr(SUBGRAPH,"name") %in% ANSWER$token[ANSWER$pos=="VERB"],15)



            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index = selected_entities, 25)#change size of selected entities

            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.color", index =vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_entities$id,"white")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index =vertex_attr(SUBGRAPH,"name") %in% VERTICES_DB_entities$id,15)

            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.background", index = vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% paths_roots], "black")

            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.color", index = EDGE_DB_entities$to[EDGE_DB_entities$to %in% vertex_attr(SUBGRAPH,"name")],"white")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index = EDGE_DB_entities$to[EDGE_DB_entities$to %in% vertex_attr(SUBGRAPH,"name")],15)
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.background", index = EDGE_DB_entities$to[EDGE_DB_entities$to %in% vertex_attr(SUBGRAPH,"name")],"#BF616A")

            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.color", index = vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% paths_roots],"yellow")
            SUBGRAPH=set_vertex_attr(SUBGRAPH, "font.size", index = vertex_attr(SUBGRAPH,"name")[vertex_attr(SUBGRAPH,"name") %in% paths_roots], 20)

            #plot(GRAPH)
            plot=SUBGRAPH %>%
              visNetwork::visIgraph(igraph = .,
                                    idToLabel = FALSE,
                                    physics = T) %>%
              visNetwork::visEdges(
                dashes = T,
                arrows ="middle",
                smooth =list(enabled=F,roundness=0,type="discrete"),
                scaling = list(min=0.15,max=4),
                color = list(highlight = "#BF616A",
                             hover = "goldenrod4",
                             opacity=0.5)
              )%>%
              visNetwork::visNodes(color = list(highlight = list(border="#ffc100"),
                                                hover = list(border="goldenrod",background='#ffc100')
              )
              ,scaling = list(min= 40,
                              max= 100,
                              label=list(enabled=T,min= 60, max= 90,maxVisible= 150,drawThreshold= 1)
              )
              ) %>%
              visNetwork::visInteraction(multiselect = T,
                                         navigationButtons = T,
                                         hover=F,
                                         dragView = T,
                                         dragNodes = F,
                                         selectable =  T)    %>%
              visNetwork::visOptions(manipulation = FALSE,selectedBy = list(variable="tweet_id", multiple="true"),
                                     nodesIdSelection = list(enabled = F),
                                     height = "600px",#"fit-content"
                                     width = "100%",
                                     autoResize = T,
                                     highlightNearest = list(enabled = T,
                                                             degree = 20,
                                                             hover = F,
                                                             algorithm="hierarchical",
                                                             labelOnly = F),
                                     collapse = list(enabled = TRUE,
                                                     clusterOptions = list(shape = "text"))) %>%
              visHierarchicalLayout(direction="UD",parentCentralization = F,nodeSpacing = 150,levelSeparation = 110,blockShifting = F,sortMethod = "directed") %>%
              visNetwork::visPhysics(solver = "hierarchicalRepulsion",
                                     hierarchicalRepulsion =list(
                                       nodeDistance=130,
                                       avoidOverlap=1,
                                       springLength=50,
                                       centralGravity=0.15,
                                       springConstant=0.05,
                                       onlyDynamicEdges=T),
                                     minVelocity=5,
                                     maxVelocity = 5,
                                     stabilization = list(enabled=T),
                                     enabled = T,
                                     adaptiveTimestep = T
                                     ,wind=list(y=5)
              )

            results=list(visualization=plot,graph=SUBGRAPH, data=ANSWER_BKP)
            results[["url"]]= MYREQUEST$url %>% URLdecode()
            results[["status_code"]]= MYREQUEST$status_code
            results[["headers"]]= MYREQUEST$headers
            results[["request"]]= MYREQUEST$request
            results[["date"]]= MYREQUEST$date
            results[["times"]]= MYREQUEST$times
            return(results)
}
hermione_controlroom_text=c("",
                            ""
                            )

#reactive_sparqlentresult=query_and_build_net()

#### SERVER FUNCTION ####
app_server <- function(input, output, session) {
  ##### ACTIVATE JS FUNCTION ####
  golem::activate_js()
  #### SET REACTIVE VALUES ####
  #vals <- reactiveValues(count = -1)
  sparql_queries_BE_n <- reactiveValues(value_BE_n=0)
  sparql_queries_FG_n <- reactiveValues(value_FG_n=0)
  hermione_flags <- reactiveValues(intro=0,methods=0,cases=0, DO=0,NF=0,info=0)
  hermione_controlroom_tutorial_n <- reactiveValues(value_tut_n=0)

  random_tweet_ids<- reactiveVal(c(one="1266799402263478273",two="1266799254980440070",three="1266799220184383489"))
  sparqlLog <-reactiveVal("")
  observeEvent(input$range, vals$count <- vals$count + 1)

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

  # Listen_search <- reactive({
  #   input$condition==1})
  #
  # Listen_timeline <- reactive({
  #   input$condition==2})

  # Listen_import <- reactive({
  #   !is.null(input$TW_file)
  # })


   output$tweet_example <-   renderUI({
     tagList(
       tags$blockquote(class = "twitter-tweet",
                       tags$a(href = "https://twitter.com/twitter/status/1277996661520859136")),
       tags$script('twttr.widgets.load(document.getElementById("tweet"));')
     )})
   ##### HERMIONE SERVER ####
   ###### Hermione: at Intro ####
   observeEvent(input$current_tab, {
     if (input$current_tab == "intro" && hermione_flags$intro==0) {
       hermione_flags$intro=1
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
     if (input$current_tab == "cases" && hermione_flags$cases==0) {
       hermione_flags$cases=1
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
     if (input$current_tab == "methods" && hermione_flags$methods==0) {
       hermione_flags$methods=1
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
     if (input$current_tab == "info" && hermione_flags$info==0) {
       hermione_flags$info=1
       showModal(modalDialog(
         title = "Welcome to the info section",
         list(html = tagList(HTML(paste0("<center>",hermione_avatar[sample(1:4,1)],"</center>")),HTML("<br><br>"),
                             HTML("Welcome to the info  Section. We sincerely appreciate your visit to the HERMIONE dashboard. In this space, you will find more info about the creators of the dashboard and opportunities to connect us. If you are an organization, expert, journalist, artist or advocate working on issues related to inequality please don't hesitate to get in touch. Together, we can make a difference. Thank you for your time."))),
         size="l",
         label="",
         icon=icon("life-ring"),
         easyClose = TRUE,
         footer = modalButton("Dismiss")
       ))
     }
   })
  ####CORE ####
  ##### COMPONENT 1: BIRD EYE PERSPECTIVE #####
   ###### REACTIVE NETWORK AND FILTERS ####


   # trigger_birdeye <- eventReactive(input$current_tab, {
   #   req(input$current_tab == 'tab')
   # })

   reactive_sparqlentresult = eventReactive(
     eventExpr = {
       input$sparqltaskBE  | input$runBE# add other condition that triggers query
     },
     ignoreNULL = TRUE,
     ignoreInit = FALSE,
     valueExpr = {
       if (TRUE) {
         return(withProgress({
           setProgress(message = "BE component sending SPARQL query to OKG. Please wait...")
           print(paste0("START_DATE: ",input$dateRange2[1]))
           print(paste0("END_DATE: ",input$dateRange2[2]))
           print(paste0("filter_: ",input$entityfilter))
           print(paste0("target_nodes_: ",input$slider_nentites))
           print(paste0("N_LIMIT: ",input$slider_nmaxrows))

           myresult=  query_and_build_net(target_nodes_ = as.integer(input$slider_nentites),filter_ =ifelse(input$entityfilter=="",NA,input$entityfilter) ,N_THRESHOLD = 0,OFFSET = 0,N_LIMIT = as.integer(input$slider_nmaxrows),START_DATE = input$dateRange2[1],END_DATE = input$dateRange2[2])

        #if(myresult=="Error: Not enought entity mentions") return(NULL)
           #n_unique=length(unique(myresult$n_ent_by_id$id))
           #random_tweet_ids(gsub(pattern = "http://example.com/tweet_",replacement = "",sample(x = unique(myresult$n_ent_by_id$id), size = n_unique, replace=FALSE)))
           if(is.null(myresult)){return(NULL)}
           if(length(unique(myresult$n_ent_by_id$id))>=1){
             random_tweet_ids(sample(gsub(pattern = "http://example.com/tweet_",replacement = "",unique(myresult$n_ent_by_id$id))))
             }
           myresult
         },
         min = 0,
         max = 2,
         value = 1
         ))}
     }
   )

   observeEvent(eventExpr = {
     (input$sparqltaskBE | input$runBE)
     #&& reactive_sparqlentresult()!=# add other condition that triggers query
   },{
     req(reactive_sparqlentresult())
     sparqlLog({paste0("<br><br><b>Query date: ",Sys.Date()," Query time: ",Sys.time(),"</b><br>",gsub(pattern = "\n |\\n ", replacement = "<br>", reactive_sparqlentresult()$my_request$url,"<br>",perl = T),sparqlLog() )})
   }
   )

   #Sample tweets for selected entity
    observeEvent(eventExpr = is.character(input$current_BEnode_id$node) && input$current_BEnode_id$node!="" ,{
     # require(input$current_BEnode_id$node)
      myresult= reactive_sparqlentresult()
random_tweet_ids(sample(gsub(pattern = "http://example.com/tweet_",replacement = "",unique(myresult$answer_final$id[myresult$answer_final$entity %in% unlist(input$current_BEnode_id$nodes,use.names = F)]))))
    }
)

   output$sparqlqueryURL= renderUI({
     HTML(text = sparqlLog())
   })

   reactive_BE_network = reactive({
     req(reactive_sparqlentresult())
     myresult<-reactive_sparqlentresult()
     myresult$network_vis %>%
       visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_BEnode_id', nodes);
              ;}") %>%
       visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_BEedge_id', edges);
              ;}")
   }
   )

   output$BEresult <-renderVisNetwork(reactive_BE_network())
   #Provide information about selected node
   output$return_BE_node <- renderPrint({
     #data$visN_data$nodes[input$BE_current_BEnode_id,]
     input$current_BEnode_id
   })
   #Provide information about selected node
   output$return_BE_edge  <- renderPrint({
     # data$visN_data$edges[input$BE_current_BEedge_id,]
     input$current_BEedge_id
   })

   #Provide information about node positions
   observeEvent(input$store_position, {
     visNetworkProxy("birdresult") %>% visGetPositions()
   })

   nodes_positions_BE <- reactive({
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
   output$downloadNetwork_BE <- downloadHandler(
     filename = function() {
       paste('network-', Sys.Date(),"_filter",input$entityfilter,"_from",input$dateRange2[1],"_to",input$dateRange2[2], '.html', sep='')
     },
     content = function(con) {
       nodes_positions <- nodes_positions_BE()
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

   ######  SIDEBAR UPDATES ####

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

   ######  CONTROLBAR UPDATES ####

   # shiny::observeEvent(input$controlbar, {
   #   toastOpts <- list(
   #     autohide = TRUE,
   #     icon = "fas fa-home",
   #     close = FALSE,
   #     position = "bottomRight"
   #   )
   #   toastOpts$class <- if (input$controlbar) "bg-success" else "bg-danger"
   #   toast(
   #     title = if (input$controlbar) "Controlbar opened!" else "Controlbar closed!",
   #     options = toastOpts
   #   )
   #
   # })

   shiny::observeEvent(input$controlbarToggleBE, {
     updateControlbar(id = "controlbar")
   })
   shiny::observeEvent(input$controlbarToggleFG, {
     updateControlbar(id = "controlbar")
   })
   shiny::observeEvent(input$infobarToggleBE, {
     updateSidebar(id = "BE_info")
   })
   shiny::observeEvent(input$infobarToggleFG, {
     updateSidebar(id = "FG_info")
   })

  # output$BEnetwork <- renderVisNetwork({
  #
  #  sparql_data= query_and_build_net(target_nodes_ = 200,filter_ = "Trump")
  #  sparql_data$network_vis %>%
  #     visEvents(hoverNode = "function(nodes) {
  #       Shiny.onInputChange('current_BEnode_id', nodes);
  #     ;}") %>%
  #     visEvents(hoverEdge = "function(edges) {
  #       Shiny.onInputChange('current_BEedge_id', edges);
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

   #extract_tweets_sample <- observeEvent({})
   # output$render_tweets_sample <-   renderUI(
   #   {
   #  fluidRow(column(width = 4,
   #   tagList(
   #     tags$blockquote(class = "twitter-tweet",
   #                     tags$a(href =  paste0("https://twitter.com/equalitytrust/status/",random_tweet_ids()[1])))
   #     ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
   #   )),
   #   column(width = 4,tagList(
   #     tags$blockquote(class = "twitter-tweet",
   #                     tags$a(href = paste0("https://twitter.com/twitter/status/",random_tweet_ids()[2])))
   #     ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
   #   )),
   #   column(width = 4, tagList(
   #     tags$blockquote(class = "twitter-tweet",
   #                     tags$a(href = paste0("https://twitter.com/twitter/status/",random_tweet_ids()[3])))
   #     ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
   #   )),
   #   )
   #     }
   #   )

   #browser()
   #observe({
   #print(random_tweet_ids()[1:12])
   #})

   output$render_tweet1_sample <-   renderUI(
    {column(width = 8,
                       tagList(
                         tags$blockquote(class = "twitter-tweet",
                                         tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[1])))
                         ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
                       )
       )
     }
   )
   output$render_tweet2_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[2])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet3_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[3])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet4_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[4])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet5_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[5])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet6_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[6])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet7_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[7])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet8_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[8])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet9_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[9])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet10_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[10])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet11_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[11])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   output$render_tweet12_sample <-   renderUI(
     {column(width = 8,
             tagList(
               tags$blockquote(class = "twitter-tweet",
                               tags$a(href =  paste0("https://twitter.com/twitter/status/",random_tweet_ids()[12])))
               ,tags$script('twttr.widgets.load(document.getElementById("tweet"));')
             )
     )
     }
   )
   #browser()

   output$downloadFGData <- downloadHandler(
     filename =  paste0(Sys.time(),'_from', input$dateRange2[1],'_to',input$dateRange2[2],ifelse(input$entityfilter!="",yes = input$entityfilter,no = ""),'_finegrained_data_HERMIONE.csv')
     ,
     content = function(file) {
       readr::write_csv(reactive_sparqlentresult()$answer_final, file)
     }
   )



  ##### COMPONENT 2: FINE GRAINED ANALYSIS #####
#    FG_entity_1 <- reactiveVal()
#    FG_entity_2 <- reactiveVal()
   #Sample tweets for selected entity


   output$FG_entity_1 <- renderUI({
     selectInput("myFG_entity_1",
                 "Selected entity A:",
                 choices = unique(reactive_sparqlentresult()$dbpedia_dict$label[!is.na(reactive_sparqlentresult()$dbpedia_dict$entityDB)]),selectize = TRUE)
   })
   output$FG_entity_2 <- renderUI({
     selectInput("myFG_entity_2",
                 "Selected entity B:",
                 choices = unique(reactive_sparqlentresult()$dbpedia_dict$label[!is.na(reactive_sparqlentresult()$dbpedia_dict$entityDB)]), selectize = TRUE)
   })
#browser()



   FG_entity_1 <- reactive({input$myFG_entity_1 })
   FG_entity_2 <- reactive({input$myFG_entity_2 })

   output$FG_entity_11 <- renderUI({
     req(FG_entity_1())
     myFG_entity_1=FG_entity_1()
     selectInput("myFG_entity_11",
                 "Selected entity A:",
                 choices = unique(reactive_sparqlentresult()$dbpedia_dict$label[!is.na(reactive_sparqlentresult()$dbpedia_dict$entityDB)]),selected = myFG_entity_1 ,selectize = TRUE)
   })
   output$FG_entity_22 <- renderUI({
     req(FG_entity_2())
     myFG_entity_2=FG_entity_2()
     selectInput("myFG_entity_22",
                 "Selected entity B:",
                 choices = unique(reactive_sparqlentresult()$dbpedia_dict$label[!is.na(reactive_sparqlentresult()$dbpedia_dict$entityDB)]),selected = myFG_entity_2, selectize = TRUE)
   })
   #FG ENTETIES UPDATE BASED ON SELECTED NODE
    observeEvent(is.character(input$current_BEnode_id$node) && input$current_BEnode_id$node!=""  , {
req(input$current_BEnode_id$node)
      req(input$myFG_entity_1)
      #browser()
      updateSelectInput(session, "myFG_entity_1", selected =gsub("_"," ",gsub("http://example.com/ent_","",input$current_BEnode_id$node)))

    })

    observeEvent(is.character(input$current_BEnode_id$node) && input$current_BEnode_id$node!=""  , {
      req(input$current_BEnode_id$node)
      req(input$myFG_entity_11)
      #browser()
      updateSelectInput(session, "myFG_entity_11", selected =gsub("_"," ",gsub("http://example.com/ent_","",input$current_BEnode_id$node)))

    })

    #FG ENTETIES UPDATE BASED ON SELECTED EDGES
    observeEvent(is.character(input$current_BEedge_id$edge) && input$current_BEedge_id$edge!=""  , {
      req(input$current_BEedge_id$edge)
      req(input$myFG_entity_1)
      req(input$myFG_entity_2)
      #browser()
      updateSelectInput(session, "myFG_entity_1", selected =gsub("_"," ",gsub("^http://example.com/ent_|<->.*$","",input$current_BEedge_id$edge)))

      updateSelectInput(session, "myFG_entity_2", selected =gsub("_"," ",gsub("^.*<->http://example.com/ent_","",input$current_BEedge_id$edge)))

    })

    observeEvent(is.character(input$current_BEedge_id$edge) && input$current_BEedge_id$edge!=""  , {
      req(input$current_BEedge_id$edge)
      req(input$myFG_entity_11)
      req(input$myFG_entity_22)
      print(input$current_BEedge_id$edge)
      #browser()
      updateSelectInput(session, "myFG_entity_11", selected =gsub("_"," ",gsub("^http://example.com/ent_|<->.*$","",input$current_BEedge_id$edge)))

      updateSelectInput(session, "myFG_entity_22", selected =gsub("_"," ",gsub("^.*<->http://example.com/ent_","",input$current_BEedge_id$edge)))

    })

#SYNCRO THE TWO UI INPUTS
   observeEvent(input$myFG_entity_11, {
     req(input$myFG_entity_11)
     updateSelectInput(session, "myFG_entity_1", selected = input$myFG_entity_11)
   })

   observeEvent(input$myFG_entity_22, {
     req(input$myFG_entity_22)
     updateSelectInput(session,  "myFG_entity_2", selected = input$myFG_entity_22)
   })

#browser()

   reactive_sparqlentresult_FG = eventReactive(
     eventExpr = {
       (input$sparqltaskFG  | input$runFG)  # add other condition that triggers query
     },
     ignoreNULL = TRUE,
     ignoreInit = TRUE,
     valueExpr = {
       req(FG_entity_1())
       req(FG_entity_2())
       if (TRUE) {
         return(withProgress({
           setProgress(message = "FG component sending SPARQL query to OKG. Please wait...")
           print(paste0("START_DATE: ",input$dateRange2[1]))
           print(paste0("END_DATE: ",input$dateRange2[2]))
           entity1=FG_entity_1()
           entity2=FG_entity_2()
           #browser()
           print(paste0("<http://dbpedia.org/resource/", gsub(" ","_",entity1),">"))
           print(paste0("<http://dbpedia.org/resource/", gsub(" ","_",entity2),">"))


           myresultFG= fg_analysis(ENTITY_1 = paste0("<http://dbpedia.org/resource/", gsub(" ","_",FG_entity_1()),">"),
                                 ENTITY_2 = paste0("<http://dbpedia.org/resource/", gsub(" ","_",FG_entity_2()),">"),
                                 START_DATE = input$dateRange2[1],
                                 END_DATE = input$dateRange2[2])
           if(is.null(myresultFG)){return(NULL)}
           # if(length(myresultFG)>=3){
           #   random_tweet_ids(sample(gsub(pattern = "http://example.com/tweet_",replacement = "",unique(myresultFG$n_ent_by_id$id))))
           # }
           myresultFG
         },
         min = 0,
         max = 2,
         value = 1
         ))}
     }
   )

   reactive_FG_network = reactive({
     req(reactive_sparqlentresult_FG())
     myresult<-reactive_sparqlentresult_FG()
     myresult$visualization
     # %>%
     #   visEvents(selectNode = "function(nodes) {
     #            Shiny.onInputChange('current_FGnode_id', nodes);
     #          ;}") %>%
     #   visEvents(selectEdge = "function(edges) {
     #            Shiny.onInputChange('current_FGedge_id', edges);
     #          ;}")
   }
   )

   output$FGresult <-renderVisNetwork(reactive_FG_network())
   #reactive_sparqlentresult_FG()$visualization

   # observeEvent(eventExpr = {
   #   (input$sparqltaskFG | input$runFG)
   #   #&& reactive_sparqlentresult()!=# add other condition that triggers query
   # },{
   #   req(reactive_sparqlentresult())
   #   sparqlLog({paste0("<br><br><b>Query date: ",Sys.Date()," Query time: ",Sys.time(),"</b><br>",gsub(pattern = "\n |\\n ", replacement = "<br>", reactive_sparqlentresult()$my_request$url,"<br>",perl = T),sparqlLog() )})
   # }
   # )

   #Sample tweets for selected entity
   # observeEvent(eventExpr = is.character(input$current_FGnode_id$node) && input$current_FGnode_id$node!="" ,{
   #   # require(input$current_FGnode_id$node)
   #   myresult= reactive_sparqlentresult()
   #   random_tweet_ids(sample(gsub(pattern = "http://example.com/tweet_",replacement = "",unique(myresult$answer_final$id[myresult$answer_final$entity==input$current_FGnode_id$node]))))
   # }
   # )

  ##### COMPONENT 3: SUMMARY STATS #####

  ##### COMPONENT 4: CASE STUDIES #####
  ###### CASE STUDY 1: #####
  ###### CASE STUDY 2: #####
  ###### CASE STUDY 3: #####
  ###### CASE STUDY 4: #####

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
