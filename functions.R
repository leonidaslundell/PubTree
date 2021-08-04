####################################################
#get structure of article

getInfo <- function(id){
  results <- getSummary(id)
  Sys.sleep(.34) #to avoid spamming the API (limit = 3/sec)
  results[[1]]$citing <- unlist(results[[1]]$citing)#unlist due to rare issue with the API and R interaction 
  results[[1]]$citations <- unlist(results[[1]]$citations)
  
  if(is.null(results[[1]]$citing))
    results[[1]]$citing <- "none"
  if(is.null(results[[1]]$citations))
    results[[1]]$citations <- "none"
  
  if (length(results[[1]]$citations) < 200) {
    citations <- results[[1]]$citations[!is.na(as.numeric(results[[1]]$citations))]
    citations <- getSummary(citations)
  } else {
    results[[1]]$citations <- sample(results[[1]]$citations, size = 200, replace = F)
    citations <- getSummary(results[[1]]$citations)
  }
  Sys.sleep(.34) #to avoid spamming the API
  if (length(results[[1]]$citing) < 200) {
    citing <- results[[1]]$citing[!is.na(as.numeric(results[[1]]$citing))]
    citing <- getSummary(citing)
  } else {
    results[[1]]$citing <- sample(results[[1]]$citing, size = 200, replace = F)
    citing <- getSummary(results[[1]]$citing)
  }
  
  resultsDF <- rbind(
    data.frame(from = results[[1]]$citations,
               to = id), #unlist since some API returns are list??
    data.frame(from = id,
               to = id),
    data.frame(from = id,
               to = results[[1]]$citing)
  )
  resultsDF <- resultsDF[!(resultsDF$from == "none" | resultsDF$to == "none"),]
  
  resultsDF$type <- "cited" #cited = query is being cited
  resultsDF$type[resultsDF$to %in% unlist(results[[1]]$citing)] <- "citing"#citing = query is citing
  resultsDF$type[resultsDF$from == id & resultsDF$to == id] <- "root"
  resultsDF$type <- as.factor(resultsDF$type)
  
  if(citations == "none")
    citations <- NULL
  if(citing == "none")
    citing <- NULL
  
  resultsMeta <- c(citations, results, citing)
  
  resultsCit <- sapply(resultsMeta, \(x){
    c(length(x$citations), length(x$citing))
  })
  rownames(resultsCit) <- c("citations", "citing")
  
  #meta data
  resultsDFmeta <- data.frame(PMID=names(resultsMeta))
  
  resultsDFmeta$citations <- resultsCit["citations",]
  resultsDFmeta$citing <- resultsCit["citing",]
  resultsDFmeta$title <- sapply(resultsMeta, \(x) x$title)
  resultsDFmeta$abstract <- sapply(resultsMeta, \(x){
    if(length(x$abstract)==0)
      return("NA")
    return(x$abstract)
  })
  resultsDFmeta$language <- sapply(resultsMeta, \(x) x$language)
  resultsDFmeta$journal <- sapply(resultsMeta, \(x) x$journal)
  resultsDFmeta$type <- sapply(resultsMeta, \(x) paste0(x$type, collapse = "/"))
  resultsDFmeta$authorNames <- sapply(resultsMeta, \(x) paste0(x$author, collapse = ", "))
  resultsDFmeta$author <- sapply(resultsMeta, \(x) x$author)
  resultsDFmeta$meshTerms <- sapply(resultsMeta, \(x) x$meshTerms)
  resultsDFmeta$year <- sapply(resultsMeta, \(x){
    if(length(x$year)==0)
      return("NA")
    return(x$year)
  })
  
  resultsDFmeta$order <- "cited"#cited = article is being cited
  resultsDFmeta$order[resultsDFmeta$PMID %in% unlist(results[[1]]$citing)] <- "citing"#citing = article is citing other
  resultsDFmeta$order[resultsDFmeta$PMID == id] <- "root"
  resultsDFmeta$order <- as.factor(resultsDFmeta$order)
  
  return(list(df = resultsDF, meta = resultsDFmeta))
}

####################################################
#get the summary of articles

getSummary <- function(id){
  if(is.null(id)|length(id)==0){
    return("none")
  }
  summaryQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&rettype=abstract&version=2.0&id="
  summaryQuery <- paste0(summaryQuery, paste0(id, collapse = ","))
  summaryQuery <- paste0(summaryQuery, "&tool=pubnet&email=leonidas.lundell@gmail.com")

  summaryRes <- xml2::read_xml(summaryQuery)

  #loop over length of id extract the xml attrs
  language <- lapply(1:length(id), \(i){
    xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//Language") |> xml2::xml_text()
  })

  year <- lapply(1:length(id), \(i){
    xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//PubDate/Year") |> xml2::xml_text()
  })

  journal <- lapply(1:length(id), \(i){
    xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//Title") |> xml2::xml_text()
  })

  type <- lapply(1:length(id), \(i){
    i <- xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//PublicationType") |> xml2::xml_text()
    if(any(grepl("Review", i))){
      return(i[grepl("Review", i)])
    }else{
      return("Article")
    }
  })

  title <- lapply(1:length(id), \(i){
    xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//ArticleTitle") |> xml2::xml_text()
  })

  abstract <- lapply(1:length(id), \(i){
    xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//Abstract") |> xml2::xml_text()
  })

  citing <- lapply(1:length(id), \(i){
    ii <- xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//ReferenceList/Reference/ArticleIdList/ArticleId") |> xml2::xml_text()
    if(length(ii)>0){
      return({
        sapply(ii, \(x){
          x[[1]]
        })
      })
    }else{
      return(NULL)
    }
  })

  author <- lapply(1:length(id), \(i){
    ii <- xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//AuthorList/Author") |> xml2::as_list()
    if(length(ii)>0){
      return({
        sapply(ii, \(x){
          paste0(x$LastName[[1]]," ", x$Initials[[1]])
        })
      })
    }else{
      return("none")
    }
  })

  meshTerms <- lapply(1:length(id), \(i){
    ii <- xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//MeshHeadingList/MeshHeading") |> xml2::as_list()
    ii
    if(length(ii)>0){
      return({
        iii <- sapply(ii, \(x){
          x$DescriptorName
        })
        names(iii) <- sapply(ii, \(x){
          attributes(x$DescriptorName)$UI
        })
        return(unlist(iii))
      })
    }else{
      return(NULL)
    }
  })
  
  features <- lapply(1:length(id), \(i){
    ii <- xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//ChemicalList/Chemical") |> xml2::as_list()
    ii
    if(length(ii)>0){
      return({
        iii <- sapply(ii, \(x){
          x$NameOfSubstance
        })
        names(iii) <- sapply(ii, \(x){
          attributes(x$NameOfSubstance)$UI
        })
        return(unlist(iii))
      })
    }else{
      return(NULL)
    }
  })

  #get citations from elink

  summaryQuery <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&retmode=xml&linkname=pubmed_pubmed_citedin&version=2.0&"
  summaryQuery <- paste0(summaryQuery, paste0("id=",id, collapse = "&"))
  summaryQuery <- paste0(summaryQuery, "&tool=pubnet&email=leonidas.lundell@gmail.com")

  Sys.sleep(.34)
  summaryRes <- xml2::read_xml(summaryQuery)

  citations <- lapply(1:length(id), \(i){
    # print(i)
    ii <- xml2::xml_child(summaryRes, search = i) |> xml2::xml_find_all(xpath = ".//Link") |> xml2::as_list()
    if(length(ii)>0){
      return({
        sapply(ii, \(x){
          x$Id[[1]]
        })
      })
    }else{
      return("none")
    }
  })

  out <- list(title = title,
              language = language,
              type = type,
              journal = journal,
              year = year,
              abstract = abstract,
              author = author,
              meshTerms = meshTerms,
              features = features,
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

####################################################
#author distance

authorGroup <- function(results){
  ji <- function(x, y){
    length(intersect(x, y))/length(unique(c(x, y)))
  }
  
  xx <- matrix(NA, nrow(results), nrow(results))
  colnames(xx) <- rownames(xx) <- results$PMID
  
  for(x in colnames(xx)){
    for(y in colnames(xx)){
      xx[x, y] <- 1 - ji(x = results[results$PMID == x,]$author[[1]],
                         y = results[results$PMID == y,]$author[[1]])
    }
  }
  xx[is.na(xx)] <- 0
  
  if(any(xx != 0)){
    xxDist <- hclust(as.dist(xx), method = "complete")
    group <- cutree(xxDist, h = rev(unique(xxDist$height))[2])
    
    groupName <- table(group) |> sort() |> tail(n = 9) |> names()
    groupTable <- table(group)
    group[group %in% names(groupTable)[!names(groupTable) %in% groupName]] <-
      "none"
    
    for(i in groupName){
      authors <- lapply(results[group == i, "author"], 
                        \(x) x)
      # authors <- unlist(authors)
      
      if(length(Reduce(intersect, authors))>0){
        authorsReduced <- Reduce(intersect, authors)
      }else{
        authorsReduced <- Reduce(union, authors)
      }
      
      #if there more than 2 authors, take the most common ones and the rest et al.
      if(length(authorsReduced)>=2){
        authors <- authors |> unlist() |> table() |> sort() |> tail(n=2) |> names()
        authorsReduced <- intersect(authorsReduced, authors)
        authorsReduced <- paste0(paste0(authorsReduced, collapse = ", "), ", et al.")
      }else{
        authorsReduced <- paste0(authorsReduced, collapse = ", ")
      }
      
      group[group %in% i] <- authorsReduced
    }
  }else{
    group <- rep("none", length(results))
  }
  
  return(group)
}

####################################################
#mesh clustering

meshClustering <- function(results){
  nullTerms <- results[sapply(results$meshTerms, \(x) length(x))==0, "PMID"]
  
  ji <- function(x, y){
    length(intersect(x, y))/length(unique(c(x, y)))
  }
  
  xx <- matrix(NA, nrow(results), nrow(results))
  colnames(xx) <- rownames(xx) <- results$PMID
  
  for(x in colnames(xx)){
    for(y in colnames(xx)){
      xx[x, y] <- 1 - ji(x = names(results[results$PMID == x,]$meshTerms[[1]]),
                         y = names(results[results$PMID == y,]$meshTerms[[1]]))
    }
  }
  xx[is.na(xx)] <- 0
  
  if(any(xx != 0)){
    
    group <- Mclust(as.dist(xx))$classification
    
    meshNames <- Reduce(c, results$meshTerms)
    meshNames <- meshNames[unique(names(meshNames))]
    
    for(i in 1:max(group)){
      groupTerm <- lapply(results[group == i,"meshTerms"], 
                          \(x) names(x)) |> 
        unlist() |> table() |> sort() |> tail()
      
      group[group %in% i] <- paste0(meshNames[names(groupTerm)] |> unique() |> paste0(collapse = "\n"),
                                    "\n")
    }
    group[nullTerms] <- "no MESH terms"
  }else{
    group <- rep("no MESH terms", length(results))
  }
  
  return(group)
}

####################################################
#create igraph

net <- function(results, layout = "tree"){
  
  if(layout != "tree"){
    results <- list(df = Reduce(rbind,
                                  lapply(results, \(i) {
                                    i$df
                                  })),
                      meta = Reduce(rbind,
                                    lapply(results, \(i) {
                                      i$meta
                                    }))) 
  }
  
  results$meta$authorGroups <- authorGroup(results = results$meta)
  results$meta$meshTerms <- meshClustering(results = results$meta)
  
  #reorder here to have authorGroups next to eachother on plot.
  results$df <- results$df[order(results$meta$meshTerms),]
  g <- graph_from_data_frame(results$df)
  results$meta <- results$meta[match(V(g)$name,
                                         results$meta$PMID),]

  V(g)$citations <- results$meta$citations+.2#0 sized legend otherwise
  V(g)$title <- results$meta$title
  V(g)$journal <- results$meta$journal
  V(g)$abstract <- results$meta$abstract
  V(g)$language <- results$meta$language
  V(g)$author <- results$meta$authorNames
  V(g)$authorGroups <- results$meta$authorGroups
  V(g)$meshTerms <- results$meta$meshTerms
  V(g)$year <- results$meta$year
  V(g)$type <- results$meta$type

  #look into layout here graphlayouts
  
  if(layout == "tree"){
    if(length(results$df[results$df$type == "cited", "from"])>0){
      gg <- create_layout(g,
                          layout = layout_as_tree(g, 
                                                  root = results$df[results$df$type == "cited","from"])
      )
    }else{
      gg <- create_layout(g,
                          layout = layout_as_tree(g, 
                                                  root = results$df[results$df$type == "root","from"])
      )
    } 
  }else{
    gg <- create_layout(g,
                        layout = layout_as_tree(g, 
                                                mode = "all")
    )
  }
  
  #cant put these in the authorgroup function since it works on the ggraph object
  gg$authorGroups <- as.factor(gg$authorGroups)
  gg$authorGroups <- relevel(gg$authorGroups, ref = "none")

  gg$meshTerms <- as.factor(gg$meshTerms)
  if(any(levels(gg$meshTerms) == "no MESH terms"))
    gg$meshTerms <- relevel(gg$meshTerms, ref = "no MESH terms")
  
  gg
}

plotNet <- function(ggDat, 
                    maxSize,
                    nodeSize,
                    x, 
                    y){
  if (length(levels(ggDat$authorGroups)) > 10) {
    x <- table(ggDat$authorGroups)
    x <- x[names(x) != "none"]
    x <- names(sort(x, decreasing = T)[1:10])
    ggDat$authorGroups[!ggDat$authorGroups %in% x] <- "none"
    ggDat$authorGroups <- ggDat$authorGroups |> as.character() |> as.factor()
    ggDat$authorGroups <- relevel(ggDat$authorGroups, "none")
  }
  # ggDat$Relation <- ggDat$type
  
  gg <- ggraph(ggDat) + 
    geom_edge_diagonal(aes(colour = type),
                       show.legend = T, 
                       arrow = arrow(length = unit(2, 'mm'),
                                     type = "closed")) + 
    geom_node_point(aes(size = citations, 
                        color = meshTerms,
                        shape = authorGroups)) +
    scale_size(breaks = seq(from = 1,
                            to = maxSize,
                            length.out = 3),
               range = c(1,5)*nodeSize,
               name = "Number of citations") +
    ggsci::scale_color_igv(name = "Common MESH terms") + 
    scale_shape_manual(values = c(15:18,0:2, 5:7), name = "Common Authors") +
    theme_void() +
    theme(legend.position = "right") +
    guides(color = guide_legend(override.aes = list(edge_linetype = 0,
                                                    size = 5)),
           shape = guide_legend(override.aes = list(edge_linetype = 0,
                                                    size = 5)),
           size = guide_legend(override.aes = list(edge_linetype = 0))) 
  
  if(any(!c(is.null(x),is.null(y))))
    gg <- gg + coord_cartesian(xlim = y, ylim = x, expand = FALSE)#ggraph inverts the brush
  
  gg
}
