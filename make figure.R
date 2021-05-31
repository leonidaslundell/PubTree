leo <- getInfo("32938935")

library(igraph)

citing <- data.frame(from = rep(leo$query, length(leo$citing)), 
                     to = leo$citing)
cited <- data.frame(from = leo$cited, 
                    to = rep(leo$query, length(leo$cited)))

all <- rbind(cited, citing)
all <- graph_from_data_frame(all)
E(all)$color <- c(rep("darkred", nrow(cited)),
                  rep("darkblue", nrow(citing)))

V(all)$size <- leo$meta[V(all)$name]
V(all)$size[is.na(V(all)$size)] <- 1

leo$meta <- (leo$meta-min(leo$meta))/(max(leo$meta) - min(leo$meta))
leo$meta <- leo$meta * 10


plot(all, 
     layout = layout_on_grid(all),
     vertex.color = "lightgray",
     vertex.label = NA,
     # vertex.size = 4,
     edge.arrow.size = .5)
