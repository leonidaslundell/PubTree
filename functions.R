####################################################
#get structure of article

getInfo <- function(id){
  
  results <- getSummary(id)
  # results <- keep
  
  bind <- function(id2){
    if(is.null(results[[id2]]$citing))
      results[[id2]]$citing <- NA
    if(is.null(results[[id2]]$citations))
      results[[id2]]$citations <- NA
    
    x <- rbind(
      data.frame(from = id2,
                 to = results[[id2]]$citing),
      data.frame(from = id2,
                 to = id2),
      data.frame(from = results[[id2]]$citations,
                 to = id2)
    )
    
    x[!(is.na(x$from)|is.na(x$to)),]
  }
  
  resultsDF <- lapply(names(results), \(id){
    bind(id)
  })
  resultsDF <- Reduce(rbind, resultsDF)
  resultsDF$type <- "cited"
  resultsDF$type[resultsDF$from == id] <- "citing"
  resultsDF$type[resultsDF$from == id & resultsDF$to == id] <- "root"
  resultsDF$type <- as.factor(resultsDF$type)
  
  #if an article is cited extensively, take sample of 200... (eutils limit)
  #also this wierd detour to get same vector length as the df to make net
  if(nrow(resultsDF)>200){
    x <- resultsDF[c(sample(which(resultsDF$type == "cited"),
                            200 - sum(resultsDF$type != "cited")),
                     which(resultsDF$type != "cited")),]  
  }else{
    x <- resultsDF
  }
  
  xx <- as.numeric(x$type)
  xx[xx>2] <- 2
  xx <- c(x[xx == 2, 2], x[xx == 1, 1])
  
  resultsRoot <- results[[1]]
  
  #second round
  
  results <- getSummary(id = xx)
  resultsCit <- sapply(results, \(x) c(length(x$citations), length(x$citing)))
  rownames(resultsCit) <- c("citations", "citing")
  
  #meta data
  resultsDFmeta <- data.frame(PMID=colnames(resultsCit))
  
  resultsDFmeta$citations <- resultsCit["citations",]
  resultsDFmeta$citing <- resultsCit["citing",]
  resultsDFmeta$title <- sapply(results, \(x) x$title)
  resultsDFmeta$abstract <- sapply(results, \(x){
    if(length(x$abstract)==0)
      return("NA")
    return(x$abstract)
  })
  resultsDFmeta$language <- sapply(results, \(x) x$language)
  resultsDFmeta$journal <- sapply(results, \(x) x$journal)
  resultsDFmeta$type <- sapply(results, \(x) x$type)
  resultsDFmeta$author <- sapply(results, \(x) paste0(x$author, collapse = ", "))
  resultsDFmeta$year <- sapply(results, \(x){
    if(length(x$year)==0)
      return("NA")
    return(x$year)
  })
  
  ####################################################
  #author distance
  resultsDFmeta$group <- authorGroup(results)
  
  return(list(df = resultsDF, meta = resultsDFmeta))
}

####################################################
#author distance

authorGroup <- function(results){
  ji <- function(x, y){
    length(intersect(x, y))/length(unique(c(x, y)))
  }
  
  xx <- matrix(NA, length(results), length(results))
  colnames(xx) <- rownames(xx) <- names(results)
  
  for(x in colnames(xx)){
    for(y in colnames(xx)){
      xx[x, y] <- 1- ji(x = results[[x]]$author,
                        y = results[[y]]$author) 
    }
  }
  xx[is.na(xx)] <- 0
  
  xxDist <- hclust(as.dist(xx), method = "complete")
  # rev(unique(xxDist$height))[2]
  
  xxGroup <- cutree(xxDist, h = rev(unique(xxDist$height))[2])
  
  xxGroupName <- names(table(xxGroup))[table(xxGroup)>1]
  xxGroupTable <- table(xxGroup)
  xxGroup[xxGroup %in% names(xxGroupTable)[!names(xxGroupTable) %in% xxGroupName]] <- 
    "0"
  
  for(i in xxGroupName){
   ii <- which(xxGroupName %in% i)
     
   xxGroup[xxGroup %in% xxGroupName[[ii]]] <- paste0(Reduce(intersect, lapply(results[xxGroup == i], \(x) x$author)),
                                                     collapse = ", ")
  }
  
  xxGroup[xxGroup == "0"] <- "none"
  
  return(xxGroup)
}

# ####################################################
# #get cited
# getCited <- function(id){
#   citedQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id="
#   cited <-
#     xml2::read_xml(paste0(citedQuery, id)) |>
#     xml2::as_list()
#   cited <- cited$eLinkResult$LinkSet$LinkSetDb |> unlist()
#   names(cited) <- NULL
#   cited <- cited[-1:-2]
#   cited
# }

####################################################
#get cited
# getCiting <- function(id){
#   citingQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id="
#   citing <-
#     xml2::read_xml(paste0(citingQuery, id)) |>
#     xml2::as_list()
#   citing <- citing$eLinkResult$LinkSet$LinkSetDb |> unlist()
#   names(citing) <- NULL
#   citing <- citing[-1:-2]
#   citing
# }

# ####################################################
# #get citing and citation and reviews
# 
# getCitation <- function(id){
#   
#   citingQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&retmode=json"
#   citingQuery <- paste(citingQuery, paste0("&id=",id, collapse = ""), collapse = "", sep = "")
#   citingQuery <- paste0(citingQuery, "&tool=pubnet&email=leonidas.lundell@gmail.com")
#   citing <- jsonlite::read_json(citingQuery)
#   
#   #fix the names for clarity
#   for(i in 1:length(citing$linksets)){
#     names(citing$linksets[[i]]$linksetdbs) <- sapply(citing$linksets[[i]]$linksetdbs, \(x) {
#       x$linkname
#     })
#     citing$linksets[[i]]$linksetdbs <- sapply(citing$linksets[[i]]$linksetdbs, \(x){
#       unlist(x$links)
#     })
#     names(citing$linksets)[i] <- citing$linksets[[i]]$ids[[1]]
#     citing$linksets[[i]] <- citing$linksets[[i]]$linksetdbs
#   }
#   citing <- citing$linksets
#   
#   citing <- lapply(citing, \(x){
#     x <- x[c("pubmed_pubmed_refs", "pubmed_pubmed_citedin")]
#     names(x) <- c("out", "in")
#     x
#   })
#   
#   #get citations
#   "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&retmode=xml&linkname=pubmed_pmc_refs&id=25605792"
#   
#   citing
# }

####################################################
#get the summary of articles

getSummary <- function(id){
  summaryQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&rettype=abstract&version=2.0&id="
  summaryQuery <- paste0(summaryQuery, paste0(id, collapse = ","))
  summaryQuery <- paste0(summaryQuery, "&tool=pubnet&email=leonidas.lundell@gmail.com")
  
  summaryRes <- read_xml(summaryQuery)
  
  #loop over length of id extract the xml attrs
  language <- lapply(1:length(id), \(i){
    xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//Language") |> xml_text()
  })

  year <- lapply(1:length(id), \(i){
    xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//DateCompleted/Year") |> xml_text()
  })
 
  journal <- lapply(1:length(id), \(i){
    xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//Title") |> xml_text()
  })
  
  type <- lapply(1:length(id), \(i){
    i <- xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//PublicationType") |> xml_text()
    if(any(grepl("Review", i))){
      return(i[grepl("Review", i)])
    }else{
      return("Article")
    }
  })
  
  title <- lapply(1:length(id), \(i){
    xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//ArticleTitle") |> xml_text()
  })
  
  abstract <- lapply(1:length(id), \(i){
    xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//Abstract") |> xml_text()
  })
  
  citing <- lapply(1:length(id), \(i){
    ii <- xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//ReferenceList/Reference") |> as_list()
    if(length(ii)>0){
      return({
        sapply(ii, \(x){
          x$ArticleIdList$ArticleId[[1]]
        })
      })
    }else{
      return(NULL)
    }
  })
  
  author <- lapply(1:length(id), \(i){
    ii <- xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//AuthorList/Author") |> as_list()
    if(length(ii)>0){
      return({
        sapply(ii, \(x){
          paste0(x$LastName[[1]]," ", x$Initials[[1]])
        })
      })
    }else{
      return(NULL)
    }
  })
  
  #get citations from elink 
  
  summaryQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&retmode=xml&linkname=pubmed_pmc_refs&version=2.0&"
  summaryQuery <- paste0(summaryQuery, paste0("id=",id, collapse = "&"))
  summaryQuery <- paste0(summaryQuery, "&tool=pubnet&email=leonidas.lundell@gmail.com")
  
  Sys.sleep(.34)
  summaryRes <- read_xml(summaryQuery)
  
  citations <- lapply(1:length(id), \(i){
    # print(i)
    ii <- xml_child(summaryRes, search = i) |> xml_find_all(xpath = ".//Link") |> as_list()
    if(length(ii)>0){
      return({
        sapply(ii, \(x){
          x$Id[[1]]
        })
      })
    }else{
      return(NULL)
    }
  })
  
  out <- list(title = title,
              language = language,
              type = type,
              journal = journal,
              year = year,
              abstract = abstract,
              author = author,
              citing = citing,
              citations = citations)
  
  out <- lapply(out, \(x){
    names(x) <- id
    x
  })
  
  out <- lapply(id, \(x){
    lapply(out, \(xx){
      xx[[x]]
    })
  })
  names(out) <- id
  
  return(out)
}
