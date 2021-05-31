####################################################
#get structure of article

getInfo <- function(id){
  datCitingID <- getCiting(id)
  datCiting <- getSummary(datCitingID)
  Sys.sleep(.5)
  datCitedID <- getCited(id)
  datCited <- getSummary(datCitedID)
  Sys.sleep(.5)
  datCitedCited <- lapply(c(datCitedID, datCitingID), function(i){
    Sys.sleep(.1)
    getCited(i)
  })
  datMeta <- sapply(datCitedCited, length)
  names(datMeta) <- c(datCited, datCiting)
  
  id <- getSummary(id)
  
  list(citing = datCiting, cited = datCited, meta = datMeta, query=id)
}

####################################################
#get cited
getCited <- function(id){
  citedQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id="
  cited <-
    xml2::read_xml(paste0(citedQuery, id)) |>
    xml2::as_list()
  cited <- cited$eLinkResult$LinkSet$LinkSetDb |> unlist()
  names(cited) <- NULL
  cited <- cited[-1:-2]
  cited
}

####################################################
#get cited
getCiting <- function(id){
  citingQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id="
  citing <-
    xml2::read_xml(paste0(citingQuery, id)) |>
    xml2::as_list()
  citing <- citing$eLinkResult$LinkSet$LinkSetDb |> unlist()
  names(citing) <- NULL
  citing <- citing[-1:-2]
  citing
}

####################################################
#get the summary of articles
getSummary <- function(ids){
  summaryQuery <- paste0("https://api.ncbi.nlm.nih.gov/lit/ctxp/v1/pubmed/?format=citation&contenttype=json&id=")
  if(length(ids)>1){
    summmary <- jsonlite::read_json(paste0(summaryQuery, paste0(ids, collapse = ",")))
    summmary <- lapply(summmary, \(x) x$ama[[1]]) |> unlist()
  }else{
    summmary <- jsonlite::read_json(paste0(summaryQuery, ids, collapse = ","))
    summmary <- summmary$ama[[1]] |> unlist()
  }
  summmary
}

####################################################
#get metadata about the articles

getMeta <- function(id){
  getCiting(id)
}
