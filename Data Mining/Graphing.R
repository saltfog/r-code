#Network graphing

#- should generate a directed graph based on the attached pairs which denote connection between nodes
#- color code edges according to column C
#- compute the number of paths leading from each to another on the graph and provide average path length. 
#- code with some documentation is needed as well


#- plotted graph
#- a table for all the possible connections and average path length
#- documented R code

#need packages
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(igraph)
library(statnet)

pairs <- read.table('pairs.txt')
g <- graph.data.frame(pairs, directed = TRUE)

#vertices count
vcount(g)

#edge count
ecount(g)
E(g)

#get edge list
get.edgelist(g)

#sample vertices
neighbors(g, 1)

#Longest geodesic distance
diameter(g, directed=F)

diam <- get_diameter(g, directed=T)
diam
class(diam)
as.vector(diam)

distances(g, v = V(g), to = V(g), mode = c("all", "out",
                                           "in"), weights = NULL, algorithm = c("automatic"))
#Average path distance
mean_distance(g, directed = TRUE, unconnected = TRUE)
closeness(g, mode="all") 

#Edges going into or out of a vertex
inc.edges <- incident(g,  V(g), mode="all")
class(inc.edges)
inc.edges

neighbors(g, V(g), mode="in")
neighbors(g, V(g), mode="out")
neighbors(g, V(g), mode="all")

#Number of paths
#Number of geodesics that pass through the node or the edge.
routes <- edge_betweenness(g, directed=T, weights=NA)
print(routes)
summary(routes)
sum(routes)

#Cocitation counts
cocitation(g, v = V(g))

#Edge sequences
E(g)[ V(g)[pairs$V1] %->% V(g)[pairs$V2] ] #to
E(g)[ V(g)[pairs$V1] %<-% V(g)[pairs$V2] ] #from

E(g)$weight <- runif(ecount(g))
get.adjacency(g, attr="weight")

E(g)$width <- runif(ecount(g))
get.adjacency(g, attr="width")

E(g)$name <- runif(ecount(g))
get.adjacency(g, attr="name")

#Graph attributes
edge_attr(g, name = "weight", index = E(g))
edge_attr(g, name = "width", index = E(g))
edge_attr(g, name = "name", index = E(g))


ceb <- cluster_edge_betweenness(g) 

#dendPlot(ceb, mode="hclust")
#class(diam)


#Final Plot
E(g)$routes <- runif(ecount(g))
E(g)$weight <- runif(length(E(g)),.1,20)
E(g)$edge <- runif(length(E(g)),.1,20)
E(g)$lty <- 1:47

#grid style
plot(g, layout=layout.grid, edge.width=E(g)$width,
     edge.arrow.size=.3, vertex.color="gold", vertex.size=20, vertex.label.color="black", 
     vertex.label.cex=.7, vertex.label.dist=0, edge.width=E(g)$weigth, edge.color="blue", edge.width=.01)

#reingold style
plot(g, layout=layout.fruchterman.reingold, edge.width=E(g)$width,
     edge.arrow.size=.3, vertex.color="gold", vertex.size=20, vertex.label.color="black", 
     vertex.label.cex=.7, vertex.label.dist=0, edge.width=E(g)$weigth, edge.color="blue", edge.width=.01)

#automatic style
plot(g, layout=layout_nicely, edge.width=E(g)$width,
     edge.arrow.size=.3, vertex.color="gold", vertex.size=20, vertex.label.color="black", 
     vertex.label.cex=.7, vertex.label.dist=0, edge.width=E(g)$weigth, edge.color="blue", edge.width=.01)


