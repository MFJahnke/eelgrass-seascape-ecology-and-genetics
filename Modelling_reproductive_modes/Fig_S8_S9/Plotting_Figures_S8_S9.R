# Author: Núria Serra Serra
# Date: April 2017



############################################################################################################


# GRAPHIC REPRESENTATION OF GDS with the simulated datasets created with the script "functions_selfing_outcr_randmat.R""

# - selfing
# - outcrossing
# - random mating (can be either selfing or outcrossing)



############################################################################################################




#################################
###### Function to split the dataframes from gstudio into the right format for RClone:
#################################

### Run script "functions_selfing_outcr_randmat.R" before

split_dfs_from_gstudio = function(df){
  
  new_df = data.frame(A2_1=character(100), A2_2=character(100),    #Create an empty df with the columns we want (2 per locus):
                      A3_1=character(100), A3_2=character(100),
                      B1_1=character(100), B1_2=character(100),
                      B2_1=character(100), B2_2=character(100),
                      B3_1=character(100), B3_2=character(100),
                      C1_1=character(100), C1_2=character(100),
                      C2_1=character(100), C2_2=character(100),
                      C3_1=character(100), C3_2=character(100),
                      C4_1=character(100), C4_2=character(100),
                      D1_1=character(100), D1_2=character(100),
                      GA12_1=character(100), GA12_2=character(100),
                      GA16_1=character(100),GA16_2=character(100),
                      GA17D_1=character(100), GA17D_2=character(100), 
                      GA19_1=character(100), GA19_2=character(100),
                      GA2_1=character(100), GA2_2 = character(100),
                      GA20_1=character(100), GA20_2=character(100),
                      GA23_1=character(100),GA23_2=character(100),
                      GA35_1=character(100), GA35_2=character(100),
                      H1_1=character(100), H1_2=character(100),
                      K1_1=character(100), K1_2=character(100))
  
  
  ncols = dim(df)[2]
  
  # Separate each column into two:
  for (i in 1:ncols) {
    sep_ls = strsplit(df[,i], ":")    #Separates the column by the ":" and returns a list
    sep_df = data.frame(matrix(unlist(sep_ls), ncol=2, nrow=100, byrow=T))  #Makes a dataframe of 2 columns from the list
    new_df[,i*2-1] = sep_df[,1]
    new_df[,i*2] = sep_df[,2]
  }
  
  #ncols of the new dataframe:
  ncols_double = ncols*2
  
  #Convert the factors into integers, to be able to run sort_all() in the dataset
  for (i in 1:ncols_double) {
    new_df[,i] = as.integer(levels(new_df[,i]))[new_df[,i]]
  }
  
  # Sort the dataset before performing functions of RClone
  new_df = sort_all(new_df) 
  
  
}



#################################
###### Applying the previous function to have the correct dataframes to input in RClone,
###### and apply the genet_dist with Rozenfeld and allele dist to each of them:
#################################


#######
#G-AF (1)
#######

#Genetic distances
input_G_AF_clonality = split_dfs_from_gstudio(G_AF_clonality)
input_G_AF_selfing1 = split_dfs_from_gstudio(G_AF_selfing1)
input_G_AF_selfing10 = split_dfs_from_gstudio(G_AF_selfing10)
input_G_AF_outcrossing = split_dfs_from_gstudio(G_AF_outcrossing)
input_G_AF_randmat = split_dfs_from_gstudio(G_AF_randmat)

G_AF_clonality_dist_numalleles = function_genetic_distance_numalleles_new(input_G_AF_clonality)
G_AF_clonality_dist_Rozenfeld = function_genetic_distance_Rozenfeld(input_G_AF_clonality)

G_AF_selfing1_dist_numalleles = function_genetic_distance_numalleles_new(input_G_AF_selfing1)
G_AF_selfing1_dist_Rozenfeld = function_genetic_distance_Rozenfeld(input_G_AF_selfing1)

G_AF_selfing10_dist_numalleles = function_genetic_distance_numalleles_new(input_G_AF_selfing10)
G_AF_selfing10_dist_Rozenfeld = function_genetic_distance_Rozenfeld(input_G_AF_selfing10)

G_AF_outcrossing_dist_numalleles = function_genetic_distance_numalleles_new(input_G_AF_outcrossing)
G_AF_outcrossing_dist_Rozenfeld = function_genetic_distance_Rozenfeld(input_G_AF_outcrossing)

G_AF_randmat_dist_numalleles = function_genetic_distance_numalleles_new(input_G_AF_randmat)
G_AF_randmat_dist_Rozenfeld = function_genetic_distance_Rozenfeld(input_G_AF_randmat)


#Plots:
p1_G_AF_real_numalleles = hist(G_dist_numalleles[[1]], freq=FALSE,
                               col="red", breaks=seq(0,30,1),
                               main="G-AF num alleles")

write.table(as.numeric(p1_G_AF_real_numalleles$density),"p1_G_AF_real_numalleles_density.txt")

p2_G_AF_real_Rozenfeld = hist(G_dist_Rozenfeld[[1]], freq=FALSE,
                              col="red", breaks=seq(0,10,0.1),
                              main="G-AF Rozenfeld")

write.table(as.numeric(p2_G_AF_real_Rozenfeld$density), "p2_G_AF_real_Rozenfeld_density.txt")

p3_G_AF_self1_numalleles = hist(G_AF_selfing1_dist_numalleles, freq=FALSE, col=rgb(0.7,0.9,1,1),
                                breaks=seq(0,30,1),
                                main = "G-AF selfing1 numalleles")

write.table(as.numeric(p3_G_AF_self1_numalleles$density), "p3_G_AF_self1_numalleles_density.txt")

#etc


#Superposition numalleles (the simulated from the files written, because this way we can draw them as points
# or lines instead of histogram) - and they do not overlap with the real data:

p1_G_AF_real_numalleles = read.table("p1_G_AF_real_numalleles_density.txt")
p3_G_AF_self1_numalleles = read.table("p3_G_AF_self1_numalleles_density.txt")
p3_G_AF_self10_numalleles = read.table("p3_G_AF_self10_numalleles_density.txt")
#etc


p1_G_AF_real_numalleles$classif = "Real"
p3_G_AF_self1_numalleles$classif = "Selfing_1"
p3_G_AF_self10_numalleles$classif = "Selfing_10"
p5_G_AF_outcr_numalleles$classif = "Outcrossing"
#p7_G_AF_randmat_numalleles$classif = "Random_mating"
p9_G_AF_clonality_numalleles$classif = "Clonality"


p1_G_AF_real_numalleles$genetic_distance = seq(0,nrow(p1_G_AF_real_numalleles)-1,1)
p3_G_AF_self1_numalleles$genetic_distance = seq(0,nrow(p3_G_AF_self1_numalleles)-1,1)
p3_G_AF_self10_numalleles$genetic_distance = seq(0,nrow(p3_G_AF_self10_numalleles)-1,1)
#etc

df = rbind(p1_G_AF_real_numalleles, p3_G_AF_self1_numalleles, p3_G_AF_self10_numalleles, p5_G_AF_outcr_numalleles,
           p9_G_AF_clonality_numalleles)


names(df)[1] = "density"

jpeg(filename = "G_AF_numalleles.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot(df) + 
  geom_bar(data=subset(df, classif %in% "Real"), stat="identity", aes(x=genetic_distance, y=density, group=classif, col=classif, fill=classif)) +
  geom_bar(data=subset(df, classif %in% c("Selfing_10", "Clonality")), alpha=0.4, stat = "identity", aes(x=genetic_distance, y=density, group=classif, col=classif, fill=classif)) + 
  geom_line(data=subset(df, classif %in% c("Outcrossing", "Selfing_1")), size=1.2,aes(x=genetic_distance, y=density, group=classif, col=classif, fill=classif)) +
  ggtitle("G-AF  Shared Alleles' Distance") + theme(panel.background = element_rect(fill = 'white', colour='black'), 
                                                    axis.text=element_text(size=12),
                                                    axis.title=element_text(size=13),
                                                    legend.title=element_blank(), 
                                                    legend.position="none") +
  labs(y="Frequency", x="Shared Alleles' Distance")



dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


#Superposition Rozenfeld:

p2_G_AF_real_Rozenfeld = read.table("p2_G_AF_real_Rozenfeld_density.txt")
p4_G_AF_self1_Rozenfeld = read.table("p4_G_AF_self1_Rozenfeld_density.txt")
#etc

p2_G_AF_real_Rozenfeld$classif = "Real"
p4_G_AF_self1_Rozenfeld$classif = "Selfing_1"
p4_G_AF_self10_Rozenfeld$classif = "Selfing_10"
p6_G_AF_outcr_Rozenfeld$classif = "Outcrossing"
#p8_G_AF_randmat_Rozenfeld$classif = "Random_mating"
p10_G_AF_clonality_Rozenfeld$classif = "Clonality"

p2_G_AF_real_Rozenfeld$genetic_distance = seq(0,(nrow(p2_G_AF_real_Rozenfeld)-1)/10,0.1)
p4_G_AF_self1_Rozenfeld$genetic_distance = seq(0,(nrow(p4_G_AF_self1_Rozenfeld)-1)/10,0.1)
p4_G_AF_self10_Rozenfeld$genetic_distance = seq(0,(nrow(p4_G_AF_self10_Rozenfeld)-1)/10,0.1)
p6_G_AF_outcr_Rozenfeld$genetic_distance = seq(0,(nrow(p6_G_AF_outcr_Rozenfeld)-1)/10,0.1)
#p8_G_AF_randmat_Rozenfeld$genetic_distance = seq(0,(nrow(p8_G_AF_randmat_Rozenfeld)-1)/10,0.1)
p10_G_AF_clonality_Rozenfeld$genetic_distance = seq(0,(nrow(p10_G_AF_clonality_Rozenfeld)-1)/10,0.1)

df1 = rbind(p2_G_AF_real_Rozenfeld, p4_G_AF_self1_Rozenfeld, p4_G_AF_self10_Rozenfeld, p6_G_AF_outcr_Rozenfeld,
           p10_G_AF_clonality_Rozenfeld)

names(df1)[1] = "density"

jpeg(filename = "G_AF_Rozenfeld.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot(df1) + 
  geom_bar(data=subset(df1, classif %in% "Real"), stat="identity", aes(x=genetic_distance, y=density, group=classif, col=classif, fill=classif)) +
  geom_bar(data=subset(df1, classif %in% c("Selfing_10", "Clonality")), alpha=0.3, stat = "identity", aes(x=genetic_distance, y=density, group=classif, col=classif, fill=classif)) + 
  geom_line(data=subset(df1, classif %in% c("Outcrossing", "Selfing_1")), size=1.2, aes(x=genetic_distance, y=density, group=classif, col=classif, fill=classif)) +
  ggtitle("G-AF  Rozenfeld's Distance") + theme(panel.background = element_rect(fill = 'white', colour='black'), 
                                                axis.text=element_text(size=12),
                                                axis.title=element_text(size=13),
                                                legend.title=element_blank(), 
                                                legend.position="none")+
  labs(y="Frequency", x="Rozenfeld's Distance") + xlim(-0.1,15)


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


##########################################
# and do the same for each population
##########################################



