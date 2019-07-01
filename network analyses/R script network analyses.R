#igraph notes
#info here:http://dyerlab.github.io/popgraph/ and http://kateto.net/networks-r-igraph
# see also R script in networks large-scale data

require(popgraph)
library(gstudio)
library(igraph)
require(ggplot2)
require(ggmap)

#open data sets
all_real_MLGs = read_population("GENOTYPED_INDIVIDUALS_all_real_MLGs.csv", type="column", 
                                locus.columns = 3:42, sep=";")

df_all_real_MLGs = read.csv("GENOTYPED_INDIVIDUALS_all_real_MLGs.csv", sep=";")

df_coords = read.csv("coordinates_allsites_Renamed.csv", header=F)
names(df_coords) = c("Population", "Longitude", "Latitude")

######################
# Subset only K sampling sites and coordinates:
######################

K_real_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-BK",  "K-K",   "K-KR",  "K-LD",
                                                            "K-NG",  "K-NI", "K-OK",  "K-ON",  
                                                            "K-RT",  "K-SK",  "K-SO"),]


K_coords = df_coords[df_coords$Population %in% c("K-BK",  "K-K",   "K-KR",  "K-LD",
                                                 "K-NG",  "K-NI", "K-OK",  "K-ON",  
                                                 "K-RT",  "K-SK",  "K-SO"),]


#The classificative dataframe is the same as the coordinate one, because we don't need to add the labels
K_classif = K_coords

######################
# Simple plot:
######################

# Convert the dataset to a multivatiate one, and get the factor that will be used to identify the nodes (populations/meadows)
K_mv = to_mv(K_real_MLGs)
K_populations = K_real_MLGs$population

# Make the graph using the multivariate data set and the factor to identify the nodes
K_graph = popgraph(x=K_mv, groups = K_populations, alpha=0.05, tol=1e-04)

#Adding data to a graph:
summary(K_classif)
K_graph_dec = decorate_graph(K_graph, K_classif, stratum="Population")

#Plotting the graph in a simple way:
plot(K_graph_dec, vertex.label.cex=1, vertex.label.dist=3,
     vertex.color="grey", vertex.label.color="black")

######################
# Subset only G sampling sites and coordinates:
######################

G_real_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-AF",  "G-AS", "G-BB",  "G-BV",  "G-GB", 
                                                            "G-NB",  "G-RX",  "G-SG",  "G-SK",  "G-SN",
                                                            "G-T"),]

G_coords = df_coords[df_coords$Population %in% c("G-AF",  "G-AS", "G-BB",  "G-BV",  "G-GB", 
                                                 "G-NB",  "G-RX",  "G-SG",  "G-SK",  "G-SN",
                                                 "G-T"),]

#The classificative dataframe is the same as the coordinate one, because we don't need to add the labels
G_classif = G_coords


######################
# Simple plot:
######################

# Convert the dataset to a multivatiate one, and get the factor that will be used to identify the nodes (populations/meadows)
G_mv = to_mv(G_real_MLGs)
G_populations = G_real_MLGs$population

# Make the graph using the multivariate data set and the factor to identify the nodes
G_graph = popgraph(x=G_mv, groups = G_populations, alpha=0.05, tol=1e-04)

#Adding data to a graph:
summary(G_classif)
G_graph_dec = decorate_graph(G_graph, G_classif, stratum="Population")


#Plotting the graph in a simple way:
plot(G_graph_dec, vertex.label.cex=1, vertex.label.dist=3,
     vertex.color="coral2", vertex.label.color="black")

######################
# Subset K and G sampling sites and coordinates:
######################

KG_real_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-AF",  "G-AS", "G-BB",  "G-BV",  "G-GB", 
                                                            "G-NB",  "G-RX",  "G-SG",  "G-SK",  "G-SN",
                                                            "G-T", "K-BK",  "K-K", "K-KR",  "K-LD",
                                                            "K-NG",  "K-NI", "K-OK",  "K-ON",  
                                                            "K-RT",  "K-SK",  "K-SO"),]

KG_coords = df_coords[df_coords$Population %in% c("G-AF",  "G-AS", "G-BB",  "G-BV",  "G-GB", 
                                                 "G-NB",  "G-RX",  "G-SG",  "G-SK",  "G-SN",
                                                 "G-T", "K-BK",  "K-K",   "K-KR",  "K-LD",
                                                 "K-NG",  "K-NI", "K-OK",  "K-ON",  
                                                 "K-RT",  "K-SK",  "K-SO"),]

#The classificative dataframe is the same as the coordinate one, because we don't need to add the labels
KG_classif = KG_coords

#####################################################################################################
## Population graphs (5): import genetic distance matrices from EDENetworks to plot them onto a real map:
#####################################################################################################

remove(list=ls())

###################
# Open the squared genetic distance matrices, the vector of population names, the initial genotype files,
# the coordinate ones and the genotypic richness ones: 
###################

# All meadows:
df_matrix_squared_allsites_Goldstein = read.table("~/2.Allsites_Goldstein_matrix_squared.txt")
df_popnames_allsites_Goldstein = read.table("~/2.Allsites_Goldstein_vector_popnames.txt")
vector_pop_names_allsites_Goldstein = df_popnames_allsites_Goldstein$V1

df_matrix_squared_allsites_Fst = read.table("~/3.Allsites_Fst_matrix_squared.txt")
df_popnames_allsites_Fst = read.table("~/3.Allsites_Fst_popvector_matrixsquared.txt")
vector_pop_names_allsites_Fst = df_popnames_allsites_Fst$V1

df_matrix_squared_allsites_cGD = read.table("~/Matrix_allsites_cGD_shortestpath.txt", 
                                            header=F)
df_popnames_allsites_cGD = read.table("~/vector_Matrix_allsites.txt")
vector_pop_names_allsites_cGD = df_popnames_allsites_cGD$V1

df_matrix_squared_allsites_Dps = read.table("~/Dps_allsites_input.txt")
df_pop_names_allsites_Dps = read.table("~/dps_vector_allsites.txt")
vector_pop_names_allsites_Dps = df_pop_names_allsites_Dps$V1

df_all_real_MLGs = read.csv("all_real_MLGs_noD_noZEN_Snack_Torg_together_noG4_Renamed&Reordered.csv", sep=";")
df_coords_allsites = read.csv("coordinates_allsites_Renamed.csv", header=F)

vector_pop_names_allsites_Goldstein == vector_pop_names_allsites_Fst #Equal
vector_pop_names_allsites_Goldstein == vector_pop_names_allsites_cGD #Equal
vector_pop_names_allsites_Goldstein == vector_pop_names_allsites_Dps # NOT EQUAL!

clonal_richness_allsites = read.csv("Clonal_diversity.csv")
names(clonal_richness_allsites) = c("Population", "Clonal_diversity", "Region")


######################
# Create a dataset that contains the coordinates of the sampling sites, their names 
# and the "Gullmarsfjord", "Kung??lv", "Large Scale" labels:
######################
df_classif_allsites = df_coords_allsites
names(df_classif_allsites) = c("Population", "Longitude", "Latitude")
df_classif_allsites$Region = c(rep("Gullmarsfjord", 9), rep("Kung??lv", 11), rep("Large Scale", 19),
                               rep("Gullmarsfjord", 2))


df_classif_allsites = merge(df_classif_allsites, clonal_richness_allsites, by="Population")
df_classif_allsites = df_classif_allsites[,c(1,4,2,3,5)]
names(df_classif_allsites) = c("Population", "Region", "Longitude", "Latitude", "Clonal_diversity")

df_classif_KG = df_classif_allsites[which(df_classif_allsites$Region != "Large Scale"),]

df_classif_K = df_classif_allsites[which(df_classif_allsites$Region == "Kung??lv"),]

df_classif_G = df_classif_allsites[which(df_classif_allsites$Region == "Gullmarsfjord"),]

#for percolation threshold, THIS IS NOT WORKING
library(sidier)
#G
#df_matrix_squared_G_Goldstein[is.na(df_matrix_squared_G_Goldstein)] <- 0  # to get a matrix with zeros instead NA
perc.thr(dis=as.data.frame(df_matrix_squared_G_Goldstein),ptPDF = TRUE, range = seq(0, 10, 0.001), ptPDFname = "PercolatedNetwork.pdf",
         estimPDF = TRUE, estimPDFname = "PercThr Estimation.pdf", estimOutfile = TRUE,
         estimOutName = "PercThresholdEstimation.txt", cex.label = 1, cex.vertex = 1,
         appendOutfile = TRUE, plotALL = FALSE, bgcol = "white", label.col = "black",
         label = colnames(V(g)$name), modules = FALSE, moduleCol = NA,
         modFileName = "Modules_summary.txt", ncs = 6, na.rm.row.col = FALSE, merge = FALSE,
         save.distance = FALSE, save.distance.name = "DistanceMatrix_Perc.thr.txt") #0.094


###################
# Change to 0 all values above the threshold in the dataframe, convert it into a matrix
# and change the row names and column names for the poopulation names: 
###################

# All sites:

df_matrix_squared_allsites_Goldstein[df_matrix_squared_allsites_Goldstein>0.60067527] = 0  #This is percolation threshold EdeN
matrix_squared_allsites_Goldstein = as.matrix(df_matrix_squared_allsites_Goldstein)
row.names(matrix_squared_allsites_Goldstein) = vector_pop_names_allsites_Goldstein
colnames(matrix_squared_allsites_Goldstein) = vector_pop_names_allsites_Goldstein

df_matrix_squared_allsites_Fst[df_matrix_squared_allsites_Fst>0.030285985] = 0
matrix_squared_allsites_Fst = as.matrix(df_matrix_squared_allsites_Fst)
row.names(matrix_squared_allsites_Fst) = vector_pop_names_allsites_Fst
colnames(matrix_squared_allsites_Fst) = vector_pop_names_allsites_Fst

df_matrix_squared_allsites_cGD[df_matrix_squared_allsites_cGD>5.19] = 0
matrix_squared_allsites_cGD = as.matrix(df_matrix_squared_allsites_cGD)
row.names(matrix_squared_allsites_cGD) = vector_pop_names_allsites_cGD
colnames(matrix_squared_allsites_cGD) = vector_pop_names_allsites_cGD

df_matrix_squared_allsites_Dps[df_matrix_squared_allsites_Dps>0.17454] = 0
matrix_squared_allsites_Dps = as.matrix(df_matrix_squared_allsites_Dps)
row.names(matrix_squared_allsites_Dps) = vector_pop_names_allsites_Dps
colnames(matrix_squared_allsites_Dps) = vector_pop_names_allsites_Dps


###################
# Convert the matrices to igraph objects:
###################

# All sites:

igraph_obj_allsites_Goldstein = graph_from_adjacency_matrix(matrix_squared_allsites_Goldstein, 
                                                            mode="undirected",
                                                            weighted=TRUE,
                                                            add.rownames=T)
igraph_obj_allsites_Fst = graph_from_adjacency_matrix(matrix_squared_allsites_Fst, 
                                                      mode="undirected",
                                                      weighted=TRUE,
                                                      add.rownames=T)
igraph_obj_allsites_cGD = graph_from_adjacency_matrix(matrix_squared_allsites_cGD, 
                                                      mode="undirected",
                                                      weighted=TRUE,
                                                      add.rownames=T)
igraph_obj_allsites_Dps = graph_from_adjacency_matrix(matrix_squared_allsites_Dps, 
                                                      mode="undirected",
                                                      weighted=TRUE,
                                                      add.rownames=T)

###################
# Add the properties of df_classif to the graphs (region, latitude, longitude, clonal richness, etc):
###################

igraph_dec_allsites_Goldstein = decorate_graph(igraph_obj_allsites_Goldstein, df_classif_allsites, stratum="Population")
igraph_dec_allsites_Fst = decorate_graph(igraph_obj_allsites_Fst, df_classif_allsites, stratum="Population")
igraph_dec_allsites_cGD = decorate_graph(igraph_obj_allsites_cGD, df_classif_allsites, stratum="Population")
igraph_dec_allsites_Dps = decorate_graph(igraph_obj_allsites_Dps, df_classif_allsites, stratum="Population")
#plot(igraph_dec_allsites_Goldstein)


##################################################################################################################

### now analyse network properties

##################################################################################################################

#check data-frame
class(igraph_dec_allsites_Goldstein)
plot(igraph_dec_allsites_Goldstein)

#save as popgraph
g <- as.popgraph(igraph_dec_G_Goldstein)
k <- as.popgraph(igraph_dec_K_Goldstein)
kg <- as.popgraph(igraph_dec_KG_Goldstein)
# this is for symmetric matrices,this can be changed with mode to directed: graph_from_adjacency_matrix(adjmatrix, mode = c("directed", "undirected",
#  "max", "min", "upper", "lower", "plus"), weighted = NULL, diag = TRUE,
# add.colnames = NULL, add.rownames = NA)
plot(g)

#see edge attributes and plot different strata
E(g)
plot(g, edge.color="black", vertex.label.color="black", vertex.color="#cccccc", vertex.label.dist=3)
plot(k, edge.color="black", vertex.label.color="black", vertex.color="#cccccc", vertex.label.dist=3)
plot(kg, edge.color="black", vertex.label.color="black", vertex.color="#cccccc", vertex.label.dist=3)

#for including allelic diversity in plot
require(ggplot2)
V(k)$A=c(3.63,4,4.14,3.6,3.57,3.73,3.71,3.77,3.6,3.86,3.27)
V(g)$A=c(3.96,3.51,3.23,4.16,3.35,3.1,3.85,3.65,3.45,3.72,3.55)

#for nice initial graph:
#pdf("nice Fst network plot.pdf")
plot(g,edge.width=2, edge.arrow.size=0.9,edge.color="black",vertex.size=V(g)$A*4, , vertex.color="#cccccc", vertex.label.color="black", vertex.label.dist=3)
#legend(x=-1.2, y=1.3, c("Cluster 1","Cluster 2", "Cluster 3"), pch=21,col="#777777", pt.bg=c("red","green","blue"), pt.cex=2, cex=.8, bty="n", ncol=1)
#dev.off()

##############################
###### Analysis of properties of networks
##############################

##### 1) get number of nodes
df.nodesG <- data.frame(Pop=V(g)$name, Latitude=V(g)$Latitude, Longitude=V(g)$Longitude)
df.nodesK <- data.frame(Pop=V(k)$name, Latitude=V(k)$Latitude, Longitude=V(k)$Longitude)

##### 2) get number of edges
df.edgeG <- data.frame(Weight=E(g)$weight )
df.edgeK <- data.frame(Weight=E(k)$weight )

##### 3) get average node degree
x=degree(g, mode="all")
sum(x)/11
y=degree(k, mode="all")
sum(y)/11

##### 4) get connectance
edge_density(g, loops = FALSE)
edge_density(k, loops = FALSE)

##### 5) get average clustering coefficient
transitivity(g, type="global")
transitivity(k, type="global")

##### 5) network diameter
diameter(g, directed=F, weights=NA) #think it should NOT be weight?, which is default?
diameter(k, directed=F, weights=NA) #think it should NOT be weight?, which is default?

diam <- get_diameter(g, directed=T) #gived nodes
diam

##### 6) average path length
average.path.length(g)
average.path.length(k)

##### 7) modularity
# clustering based on Louvain
cg=cluster_louvain(g, weights = NULL)
cg
plot(cg,as.undirected(g))
ck=cluster_louvain(k, weights = NULL)
ck
plot(ck,as.undirected(k))

##### 8) node degree distribution
degG <- degree(g, mode="all") 
hist(degG, breaks=1:vcount(g)-1, main="Histogram of node degree")
degK <- degree(k, mode="all") 
hist(degK, breaks=1:vcount(k)-1, main="Histogram of node degree")
#or
degree_dist=degree_distribution(g, cumulative = FALSE, v=V(g))
hist(degree_dist, breaks=10)
#or
deg.dist <- degree_distribution(g, cumulative=T, mode="all") 
plot( x=0:max(deg.dist), y=1-deg.dist, pch=19, cex=1.2, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")
plot(k, vertex.size=deg*3)

##### 9) betweenness centrality
df.nodesG$betweenness <- betweenness(g)
betweenness(g)
df.nodesK$betweenness <- betweenness(k)
betweenness(k)
#or
betweenness(k, v = V(k), directed = F, weights = NULL,
            nobigint = TRUE, normalized = T)

##### 10) page rank algorithm
page_rank(g, algo = c("prpack"), vids = V(g),
          directed = F, damping = 0.85, personalized = NULL, weights = NULL,
          options = NULL)   #For the explanation of the PageRank algorithm, see the following webpage: http://infolab.stanford.edu/~backrub/google.html, 

page_rank(k, algo = c("prpack"), vids = V(k),
          directed = F, damping = 0.85, personalized = NULL, weights = NULL,
          options = NULL) 

##### 10) which type?
source("is.smallworld.R")
is.smallworld(g)
is.smallworld(k)
is.smallworld(kg)
source("is.scalefree.R")
is.scalefree(k)
source("is.hierarchy.R")
is.hierarchy(k)


##############################
##### to compare network properties to random graph
##############################

#make random graph
#Erdos-Renyi random graph model(???n??? is number of nodes, ???m??? is the number of edges).
edge_attr(g) #to find out number of edges
er <- sample_gnm(n=11, m=23, directed=F)
plot(er, vertex.size=6, vertex.label=NA)

##to make many random garphs and get mean of measures
er2<-replicate(1000, sample_gnm(n=7, m=7, directed=F), simplify=FALSE); 
sapply(er2, transitivity)
mean(sapply(er2, transitivity))

sapply(er2, degree)
mean(sapply(er2, degree))

sapply(er2, edge_density)
mean(sapply(er2, edge_density))

sapply(er2, diameter)
mean(sapply(er2, diameter))

sapply(er2, average.path.length)
mean(sapply(er2, average.path.length))

degree_dist=sapply(er2, betweenness)
mean(sapply(er2,betweenness))


# wilcoxon test to see if real data is sig different from random networks
wilcox.test(0.38, 0.39, paired=T) # probably use node specific and edge specific data only



##collection of some more useful tests
#node specific
df.nodes$closeness <- closeness(g)
closeness(g) # The closeness centrality of a vertex is defined by the inverse of the average length of the shortest paths to/from all the other vertices in the graph:

#network density
edge_density(g, loops = FALSE)

# to look at degree distribution to decide where to set limit
degree(g, v = V(g), mode = c("all"), loops = TRUE, normalized = FALSE)
z=degree_distribution(g,v=V(g), cumulative=T, mode=c("all"))
plot(z)

# to find most connected node
hist(degree(g2))
deg=degree(g2)
key=which(deg==max(deg))
key
newweb= delete_vertices(g,v=key) #or newweb= delete_edges(g,e=key)


