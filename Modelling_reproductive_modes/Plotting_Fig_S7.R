# Author: Núria Serra Serra
# Date: June 2017

########################################################################################################################################################################

# PLOTTING Fig. S7: AVERAGE SIMULATED GDS FOR G AND K REGIONS WITH THE 3 DIFFERENT pself, AND PLOTTING THE AVERAGE REAL GDS AND THE AVERAGE SIMULATED ONES

########################################################################################################################################################################



#########################################################################################################
#########################################################################################################

# CREATING THE AVERAGE GDS FOR KUNGÄLV AND GULLMARSFJORD WITH pself = 0.01

#########################################################################################################
#########################################################################################################



setwd("Histogram_densities/6.Repr_modes_together_survival&nosurvival_pself0.01")


###########################################
# Kungälv - pself 0.01
###########################################



#####
# SAD
#####
h1_K_withoutsurvival_SAD = read.table("p5_K_BK_allreprmodes_withoutsurvival_numalleles_density.txt")
h1_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h1_K_withoutsurvival_SAD$x)

h2_K_withoutsurvival_SAD = read.table("p5_K_K_allreprmodes_withoutsurvival_numalleles_density.txt")
h2_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h2_K_withoutsurvival_SAD$x)

h3_K_withoutsurvival_SAD = read.table("p5_K_KR_allreprmodes_withoutsurvival_numalleles_density.txt")
h3_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h3_K_withoutsurvival_SAD$x)

h4_K_withoutsurvival_SAD = read.table("p5_K_LD_allreprmodes_withoutsurvival_numalleles_density.txt")
h4_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h4_K_withoutsurvival_SAD$x)

h5_K_withoutsurvival_SAD = read.table("p5_K_NG_allreprmodes_withoutsurvival_numalleles_density.txt")
h5_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h5_K_withoutsurvival_SAD$x)

h6_K_withoutsurvival_SAD = read.table("p5_K_NI_allreprmodes_withoutsurvival_numalleles_density.txt")
h6_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h6_K_withoutsurvival_SAD$x)

h7_K_withoutsurvival_SAD = read.table("p5_K_OK_allreprmodes_withoutsurvival_numalleles_density.txt")
h7_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h7_K_withoutsurvival_SAD$x)

h8_K_withoutsurvival_SAD = read.table("p5_K_ON_allreprmodes_withoutsurvival_numalleles_density.txt")
h8_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h8_K_withoutsurvival_SAD$x)

h9_K_withoutsurvival_SAD = read.table("p5_K_RT_allreprmodes_withoutsurvival_numalleles_density.txt")
h9_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h9_K_withoutsurvival_SAD$x)

h10_K_withoutsurvival_SAD = read.table("p5_K_SK_allreprmodes_withoutsurvival_numalleles_density.txt")
h10_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_K_withoutsurvival_SAD$x)-1, 1),
                                       density = h10_K_withoutsurvival_SAD$x)

h11_K_withoutsurvival_SAD = read.table("p5_K_SO_allreprmodes_withoutsurvival_numalleles_density.txt")
h11_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_K_withoutsurvival_SAD$x)-1, 1),
                                       density = h11_K_withoutsurvival_SAD$x)

average_K_withoutsurvival_SAD_pself0.01 = apply(cbind(h1_K_withoutsurvival_SAD$density, h2_K_withoutsurvival_SAD$density,
                                                      h3_K_withoutsurvival_SAD$density, h4_K_withoutsurvival_SAD$density,
                                                      h5_K_withoutsurvival_SAD$density,h6_K_withoutsurvival_SAD$density,
                                                      h7_K_withoutsurvival_SAD$density,h8_K_withoutsurvival_SAD$density,
                                                      h9_K_withoutsurvival_SAD$density,h10_K_withoutsurvival_SAD$density,
                                                      h11_K_withoutsurvival_SAD$density),1,sum)

df_average_K_withoutsurvival_SAD_pself0.01 = data.frame(Genetic_distance = seq(0,length(average_K_withoutsurvival_SAD_pself0.01)-1, 1), 
                                                        density = average_K_withoutsurvival_SAD_pself0.01)

#write.csv(df_average_K_withoutsurvival_SAD_pself0.01, "avg_K_withoutsurvival_SAD_pself0.01.csv")

ggplot(df_average_K_withoutsurvival_SAD_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))


#####
# Rozenfeld
#####

h1_K_withoutsurvival_Roz = read.table("p6_K_BK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h1_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h1_K_withoutsurvival_Roz$x)

h2_K_withoutsurvival_Roz = read.table("p6_K_K_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h2_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h2_K_withoutsurvival_Roz$x)

h3_K_withoutsurvival_Roz = read.table("p6_K_KR_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h3_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h3_K_withoutsurvival_Roz$x)

h4_K_withoutsurvival_Roz = read.table("p6_K_LD_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h4_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h4_K_withoutsurvival_Roz$x)

h5_K_withoutsurvival_Roz = read.table("p6_K_NG_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h5_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h5_K_withoutsurvival_Roz$x)

h6_K_withoutsurvival_Roz = read.table("p6_K_NI_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h6_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h6_K_withoutsurvival_Roz$x)

h7_K_withoutsurvival_Roz = read.table("p6_K_OK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h7_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h7_K_withoutsurvival_Roz$x)

h8_K_withoutsurvival_Roz = read.table("p6_K_ON_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h8_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h8_K_withoutsurvival_Roz$x)

h9_K_withoutsurvival_Roz = read.table("p6_K_RT_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h9_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h9_K_withoutsurvival_Roz$x)

h10_K_withoutsurvival_Roz = read.table("p6_K_SK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h10_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_K_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h10_K_withoutsurvival_Roz$x)

h11_K_withoutsurvival_Roz = read.table("p6_K_SO_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h11_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_K_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h11_K_withoutsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_K_withoutsurvival_Roz, h2_K_withoutsurvival_Roz, #List of data frames
                h3_K_withoutsurvival_Roz, h4_K_withoutsurvival_Roz,
                h5_K_withoutsurvival_Roz,h6_K_withoutsurvival_Roz,
                h7_K_withoutsurvival_Roz,h8_K_withoutsurvival_Roz,
                h9_K_withoutsurvival_Roz,h10_K_withoutsurvival_Roz,
                h11_K_withoutsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

names(df)
average_K_withoutsurvival_Roz_pself0.01 = apply(cbind(df[,2],df[,4],df[,6],
                                                      df[,8],df[,10],df[,12],df[,14],
                                                      df[,16],df[,18],df[,20],
                                                      df[,22]),1,sum, na.rm=T)

df_average_K_withoutsurvival_Roz_pself0.01 = data.frame(Genetic_distance = seq(0,(length(average_K_withoutsurvival_Roz_pself0.01)-1)/10, 0.1), 
                                                        density = average_K_withoutsurvival_Roz_pself0.01)

#write.csv(df_average_K_withoutsurvival_Roz_pself0.01, "avg_K_withoutsurvival_Roz_pself0.01.csv")


ggplot(df_average_K_withoutsurvival_Roz_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nRozenfeld's Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



#################### 
# With survival (Kungälv - pself 0.01)
#################### 

#####
# SAD
#####
h1_K_withsurvival_SAD = read.table("p7_K_BK_allreprmodes_withsurvival_numalleles_density.txt")
h1_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_K_withsurvival_SAD$x)-1, 1),
                                   density = h1_K_withsurvival_SAD$x)

h2_K_withsurvival_SAD = read.table("p7_K_K_allreprmodes_withsurvival_numalleles_density.txt")
h2_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_K_withsurvival_SAD$x)-1, 1),
                                   density = h2_K_withsurvival_SAD$x)

h3_K_withsurvival_SAD = read.table("p7_K_KR_allreprmodes_withsurvival_numalleles_density.txt")
h3_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_K_withsurvival_SAD$x)-1, 1),
                                   density = h3_K_withsurvival_SAD$x)

h4_K_withsurvival_SAD = read.table("p7_K_LD_allreprmodes_withsurvival_numalleles_density.txt")
h4_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_K_withsurvival_SAD$x)-1, 1),
                                   density = h4_K_withsurvival_SAD$x)

h5_K_withsurvival_SAD = read.table("p7_K_NG_allreprmodes_withsurvival_numalleles_density.txt")
h5_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_K_withsurvival_SAD$x)-1, 1),
                                   density = h5_K_withsurvival_SAD$x)

h6_K_withsurvival_SAD = read.table("p7_K_NI_allreprmodes_withsurvival_numalleles_density.txt")
h6_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_K_withsurvival_SAD$x)-1, 1),
                                   density = h6_K_withsurvival_SAD$x)

h7_K_withsurvival_SAD = read.table("p7_K_OK_allreprmodes_withsurvival_numalleles_density.txt")
h7_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_K_withsurvival_SAD$x)-1, 1),
                                   density = h7_K_withsurvival_SAD$x)

h8_K_withsurvival_SAD = read.table("p7_K_ON_allreprmodes_withsurvival_numalleles_density.txt")
h8_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_K_withsurvival_SAD$x)-1, 1),
                                   density = h8_K_withsurvival_SAD$x)

h9_K_withsurvival_SAD = read.table("p7_K_RT_allreprmodes_withsurvival_numalleles_density.txt")
h9_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_K_withsurvival_SAD$x)-1, 1),
                                   density = h9_K_withsurvival_SAD$x)

h10_K_withsurvival_SAD = read.table("p7_K_SK_allreprmodes_withsurvival_numalleles_density.txt")
h10_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_K_withsurvival_SAD$x)-1, 1),
                                    density = h10_K_withsurvival_SAD$x)

h11_K_withsurvival_SAD = read.table("p7_K_SO_allreprmodes_withsurvival_numalleles_density.txt")
h11_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_K_withsurvival_SAD$x)-1, 1),
                                    density = h11_K_withsurvival_SAD$x)

average_K_withsurvival_SAD_pself0.01 = apply(cbind(h1_K_withsurvival_SAD$density, h2_K_withsurvival_SAD$density,
                                                   h3_K_withsurvival_SAD$density, h4_K_withsurvival_SAD$density,
                                                   h5_K_withsurvival_SAD$density,h6_K_withsurvival_SAD$density,
                                                   h7_K_withsurvival_SAD$density,h8_K_withsurvival_SAD$density,
                                                   h9_K_withsurvival_SAD$density,h10_K_withsurvival_SAD$density,
                                                   h11_K_withsurvival_SAD$density),1,sum)

df_average_K_withsurvival_SAD_pself0.01 = data.frame(Genetic_distance = seq(0,length(average_K_withsurvival_SAD_pself0.01)-1, 1), 
                                                     density = average_K_withsurvival_SAD_pself0.01)

#write.csv(df_average_K_withsurvival_SAD_pself0.01, "avg_K_withsurvival_SAD_pself0.01.csv")

ggplot(df_average_K_withsurvival_SAD_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))

#####
# Rozenfeld
#####

h1_K_withsurvival_Roz = read.table("p8_K_BK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h1_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h1_K_withsurvival_Roz$x)

h2_K_withsurvival_Roz = read.table("p8_K_K_allreprmodes_withsurvival_Rozenfeld_density.txt")
h2_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h2_K_withsurvival_Roz$x)

h3_K_withsurvival_Roz = read.table("p8_K_KR_allreprmodes_withsurvival_Rozenfeld_density.txt")
h3_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h3_K_withsurvival_Roz$x)

h4_K_withsurvival_Roz = read.table("p8_K_LD_allreprmodes_withsurvival_Rozenfeld_density.txt")
h4_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h4_K_withsurvival_Roz$x)

h5_K_withsurvival_Roz = read.table("p8_K_NG_allreprmodes_withsurvival_Rozenfeld_density.txt")
h5_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h5_K_withsurvival_Roz$x)

h6_K_withsurvival_Roz = read.table("p8_K_NI_allreprmodes_withsurvival_Rozenfeld_density.txt")
h6_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h6_K_withsurvival_Roz$x)

h7_K_withsurvival_Roz = read.table("p8_K_OK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h7_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h7_K_withsurvival_Roz$x)

h8_K_withsurvival_Roz = read.table("p8_K_ON_allreprmodes_withsurvival_Rozenfeld_density.txt")
h8_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h8_K_withsurvival_Roz$x)

h9_K_withsurvival_Roz = read.table("p8_K_RT_allreprmodes_withsurvival_Rozenfeld_density.txt")
h9_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h9_K_withsurvival_Roz$x)

h10_K_withsurvival_Roz = read.table("p8_K_SK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h10_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_K_withsurvival_Roz$x)/10)-1, 1),
                                    density = h10_K_withsurvival_Roz$x)

h11_K_withsurvival_Roz = read.table("p8_K_SO_allreprmodes_withsurvival_Rozenfeld_density.txt")
h11_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_K_withsurvival_Roz$x)/10)-1, 1),
                                    density = h11_K_withsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_K_withsurvival_Roz, h2_K_withsurvival_Roz, #List of data frames
                h3_K_withsurvival_Roz, h4_K_withsurvival_Roz,
                h5_K_withsurvival_Roz,h6_K_withsurvival_Roz,
                h7_K_withsurvival_Roz,h8_K_withsurvival_Roz,
                h9_K_withsurvival_Roz,h10_K_withsurvival_Roz,
                h11_K_withsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

average_K_withsurvival_Roz_pself0.01 = apply(cbind(df[,2],df[,4],df[,6],
                                                   df[,8],df[,10],df[,12],df[,14],
                                                   df[,16],df[,18],df[,20],
                                                   df[,22]),1,sum, na.rm=T)

df_average_K_withsurvival_Roz_pself0.01 = data.frame(Genetic_distance = seq(0,(length(average_K_withsurvival_Roz_pself0.01)-1)/10, 0.1), 
                                                     density = average_K_withsurvival_Roz_pself0.01)

#write.csv(df_average_K_withsurvival_Roz_pself0.01, "avg_K_withsurvival_Roz_pself0.01.csv")


ggplot(df_average_K_withsurvival_Roz_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))







###########################################
# Gullmarsfjord - pself 0.01
###########################################




#################### 
# Without survival (Gullmarsfjord - pself 0.01)
#################### 


#####
# SAD
#####
h1_G_withoutsurvival_SAD = read.table("p5_G_AF_allreprmodes_withoutsurvival_numalleles_density.txt")
h1_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h1_G_withoutsurvival_SAD$x)

h2_G_withoutsurvival_SAD = read.table("p5_G_AS_allreprmodes_withoutsurvival_numalleles_density.txt")
h2_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h2_G_withoutsurvival_SAD$x)

h3_G_withoutsurvival_SAD = read.table("p5_G_BB_allreprmodes_withoutsurvival_numalleles_density.txt")
h3_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h3_G_withoutsurvival_SAD$x)

h4_G_withoutsurvival_SAD = read.table("p5_G_BV_allreprmodes_withoutsurvival_numalleles_density.txt")
h4_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h4_G_withoutsurvival_SAD$x)

h5_G_withoutsurvival_SAD = read.table("p5_G_GB_allreprmodes_withoutsurvival_numalleles_density.txt")
h5_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h5_G_withoutsurvival_SAD$x)

h6_G_withoutsurvival_SAD = read.table("p5_G_NB_allreprmodes_withoutsurvival_numalleles_density.txt")
h6_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h6_G_withoutsurvival_SAD$x)

h7_G_withoutsurvival_SAD = read.table("p5_G_RX_allreprmodes_withoutsurvival_numalleles_density.txt")
h7_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h7_G_withoutsurvival_SAD$x)

h8_G_withoutsurvival_SAD = read.table("p5_G_SG_allreprmodes_withoutsurvival_numalleles_density.txt")
h8_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h8_G_withoutsurvival_SAD$x)

h9_G_withoutsurvival_SAD = read.table("p5_G_SK_allreprmodes_withoutsurvival_numalleles_density.txt")
h9_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h9_G_withoutsurvival_SAD$x)

h10_G_withoutsurvival_SAD = read.table("p5_Snack_allreprmodes_withoutsurvival_numalleles_density.txt")
h10_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h10_G_withoutsurvival_SAD$x)

h11_G_withoutsurvival_SAD = read.table("p5_Torg_allreprmodes_withoutsurvival_numalleles_density.txt")
h11_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h11_G_withoutsurvival_SAD$x)

average_G_withoutsurvival_SAD_pself0.01 = apply(cbind(h1_G_withoutsurvival_SAD$density, h2_G_withoutsurvival_SAD$density,
                                            h3_G_withoutsurvival_SAD$density, h4_G_withoutsurvival_SAD$density,
                                            h5_G_withoutsurvival_SAD$density,h6_G_withoutsurvival_SAD$density,
                                            h7_G_withoutsurvival_SAD$density,h8_G_withoutsurvival_SAD$density,
                                            h9_G_withoutsurvival_SAD$density,h10_G_withoutsurvival_SAD$density,
                                            h11_G_withoutsurvival_SAD$density),1,sum)

df_average_G_withoutsurvival_SAD_pself0.01 = data.frame(Genetic_distance = seq(0,length(average_G_withoutsurvival_SAD_pself0.01)-1, 1), 
                                  density = average_G_withoutsurvival_SAD_pself0.01)

#write.csv(df_average_G_withoutsurvival_SAD_pself0.01, "avg_G_withoutsurvival_SAD_pself0.01.csv")

ggplot(df_average_G_withoutsurvival_SAD_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))


#####
# Rozenfeld
#####

h1_G_withoutsurvival_Roz = read.table("p6_G_AF_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h1_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h1_G_withoutsurvival_Roz$x)

h2_G_withoutsurvival_Roz = read.table("p6_G_AS_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h2_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h2_G_withoutsurvival_Roz$x)

h3_G_withoutsurvival_Roz = read.table("p6_G_BB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h3_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h3_G_withoutsurvival_Roz$x)

h4_G_withoutsurvival_Roz = read.table("p6_G_BV_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h4_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h4_G_withoutsurvival_Roz$x)

h5_G_withoutsurvival_Roz = read.table("p6_G_GB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h5_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h5_G_withoutsurvival_Roz$x)

h6_G_withoutsurvival_Roz = read.table("p6_G_NB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h6_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h6_G_withoutsurvival_Roz$x)

h7_G_withoutsurvival_Roz = read.table("p6_G_RX_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h7_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h7_G_withoutsurvival_Roz$x)

h8_G_withoutsurvival_Roz = read.table("p6_G_SG_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h8_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h8_G_withoutsurvival_Roz$x)

h9_G_withoutsurvival_Roz = read.table("p6_G_SK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h9_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_G_withoutsurvival_Roz$x)/10)-1, 1),
                                   density = h9_G_withoutsurvival_Roz$x)

h10_G_withoutsurvival_Roz = read.table("p6_Snack_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h10_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_G_withoutsurvival_Roz$x)/10)-1, 1),
                                    density = h10_G_withoutsurvival_Roz$x)

h11_G_withoutsurvival_Roz = read.table("p6_Torg_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h11_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_G_withoutsurvival_Roz$x)/10)-1, 1),
                                    density = h11_G_withoutsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_G_withoutsurvival_Roz, h2_G_withoutsurvival_Roz, #List of data frames
                h3_G_withoutsurvival_Roz, h4_G_withoutsurvival_Roz,
                h5_G_withoutsurvival_Roz,h6_G_withoutsurvival_Roz,
                h7_G_withoutsurvival_Roz,h8_G_withoutsurvival_Roz,
                h9_G_withoutsurvival_Roz,h10_G_withoutsurvival_Roz,
                h11_G_withoutsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

names(df)
average_G_withoutsurvival_Roz_pself0.01 = apply(cbind(df[,2],df[,4],df[,6],
                                                   df[,8],df[,10],df[,12],df[,14],
                                                   df[,16],df[,18],df[,20],
                                                   df[,22]),1,sum, na.rm=T)

df_average_G_withoutsurvival_Roz_pself0.01 = data.frame(Genetic_distance = seq(0,(length(average_G_withoutsurvival_Roz_pself0.01)-1)/10, 0.1), 
                                                     density = average_G_withoutsurvival_Roz_pself0.01)


ggplot(df_average_G_withoutsurvival_Roz_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nRozenfeld's Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



#################### 
# With survival (Gullmarsfjord - pself 0.01)
#################### 

#####
# SAD
#####
h1_G_withsurvival_SAD = read.table("p7_G_AF_allreprmodes_withsurvival_numalleles_density.txt")
h1_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_G_withsurvival_SAD$x)-1, 1),
                                      density = h1_G_withsurvival_SAD$x)

h2_G_withsurvival_SAD = read.table("p7_G_AS_allreprmodes_withsurvival_numalleles_density.txt")
h2_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_G_withsurvival_SAD$x)-1, 1),
                                      density = h2_G_withsurvival_SAD$x)

h3_G_withsurvival_SAD = read.table("p7_G_BB_allreprmodes_withsurvival_numalleles_density.txt")
h3_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_G_withsurvival_SAD$x)-1, 1),
                                      density = h3_G_withsurvival_SAD$x)

h4_G_withsurvival_SAD = read.table("p7_G_BV_allreprmodes_withsurvival_numalleles_density.txt")
h4_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_G_withsurvival_SAD$x)-1, 1),
                                      density = h4_G_withsurvival_SAD$x)

h5_G_withsurvival_SAD = read.table("p7_G_GB_allreprmodes_withsurvival_numalleles_density.txt")
h5_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_G_withsurvival_SAD$x)-1, 1),
                                      density = h5_G_withsurvival_SAD$x)

h6_G_withsurvival_SAD = read.table("p7_G_NB_allreprmodes_withsurvival_numalleles_density.txt")
h6_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_G_withsurvival_SAD$x)-1, 1),
                                      density = h6_G_withsurvival_SAD$x)

h7_G_withsurvival_SAD = read.table("p7_G_RX_allreprmodes_withsurvival_numalleles_density.txt")
h7_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_G_withsurvival_SAD$x)-1, 1),
                                      density = h7_G_withsurvival_SAD$x)

h8_G_withsurvival_SAD = read.table("p7_G_SG_allreprmodes_withsurvival_numalleles_density.txt")
h8_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_G_withsurvival_SAD$x)-1, 1),
                                      density = h8_G_withsurvival_SAD$x)

h9_G_withsurvival_SAD = read.table("p7_G_SK_allreprmodes_withsurvival_numalleles_density.txt")
h9_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_G_withsurvival_SAD$x)-1, 1),
                                      density = h9_G_withsurvival_SAD$x)

h10_G_withsurvival_SAD = read.table("p7_Snack_allreprmodes_withsurvival_numalleles_density.txt")
h10_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_G_withsurvival_SAD$x)-1, 1),
                                       density = h10_G_withsurvival_SAD$x)

h11_G_withsurvival_SAD = read.table("p7_Torg_allreprmodes_withsurvival_numalleles_density.txt")
h11_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_G_withsurvival_SAD$x)-1, 1),
                                       density = h11_G_withsurvival_SAD$x)

average_G_withsurvival_SAD_pself0.01 = apply(cbind(h1_G_withsurvival_SAD$density, h2_G_withsurvival_SAD$density,
                                                      h3_G_withsurvival_SAD$density, h4_G_withsurvival_SAD$density,
                                                      h5_G_withsurvival_SAD$density,h6_G_withsurvival_SAD$density,
                                                      h7_G_withsurvival_SAD$density,h8_G_withsurvival_SAD$density,
                                                      h9_G_withsurvival_SAD$density,h10_G_withsurvival_SAD$density,
                                                      h11_G_withsurvival_SAD$density),1,sum)

df_average_G_withsurvival_SAD_pself0.01 = data.frame(Genetic_distance = seq(0,length(average_G_withsurvival_SAD_pself0.01)-1, 1), 
                                                        density = average_G_withsurvival_SAD_pself0.01)

ggplot(df_average_G_withsurvival_SAD_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))

#####
# Rozenfeld
#####

h1_G_withsurvival_Roz = read.table("p8_G_AF_allreprmodes_withsurvival_Rozenfeld_density.txt")
h1_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h1_G_withsurvival_Roz$x)

h2_G_withsurvival_Roz = read.table("p8_G_AS_allreprmodes_withsurvival_Rozenfeld_density.txt")
h2_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h2_G_withsurvival_Roz$x)

h3_G_withsurvival_Roz = read.table("p8_G_BB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h3_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h3_G_withsurvival_Roz$x)

h4_G_withsurvival_Roz = read.table("p8_G_BV_allreprmodes_withsurvival_Rozenfeld_density.txt")
h4_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h4_G_withsurvival_Roz$x)

h5_G_withsurvival_Roz = read.table("p8_G_GB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h5_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h5_G_withsurvival_Roz$x)

h6_G_withsurvival_Roz = read.table("p8_G_NB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h6_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h6_G_withsurvival_Roz$x)

h7_G_withsurvival_Roz = read.table("p8_G_RX_allreprmodes_withsurvival_Rozenfeld_density.txt")
h7_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h7_G_withsurvival_Roz$x)

h8_G_withsurvival_Roz = read.table("p8_G_SG_allreprmodes_withsurvival_Rozenfeld_density.txt")
h8_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h8_G_withsurvival_Roz$x)

h9_G_withsurvival_Roz = read.table("p8_G_SK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h9_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h9_G_withsurvival_Roz$x)

h10_G_withsurvival_Roz = read.table("p8_Snack_allreprmodes_withsurvival_Rozenfeld_density.txt")
h10_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_G_withsurvival_Roz$x)/10)-1, 1),
                                    density = h10_G_withsurvival_Roz$x)

h11_G_withsurvival_Roz = read.table("p8_Torg_allreprmodes_withsurvival_Rozenfeld_density.txt")
h11_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_G_withsurvival_Roz$x)/10)-1, 1),
                                    density = h11_G_withsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_G_withsurvival_Roz, h2_G_withsurvival_Roz, #List of data frames
                h3_G_withsurvival_Roz, h4_G_withsurvival_Roz,
                h5_G_withsurvival_Roz,h6_G_withsurvival_Roz,
                h7_G_withsurvival_Roz,h8_G_withsurvival_Roz,
                h9_G_withsurvival_Roz,h10_G_withsurvival_Roz,
                h11_G_withsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

average_G_withsurvival_Roz_pself0.01 = apply(cbind(df[,2],df[,4],df[,6],
                                                   df[,8],df[,10],df[,12],df[,14],
                                                   df[,16],df[,18],df[,20],
                                                   df[,22]),1,sum, na.rm=T)

df_average_G_withsurvival_Roz_pself0.01 = data.frame(Genetic_distance = seq(0,(length(average_G_withsurvival_Roz_pself0.01)-1)/10, 0.1), 
                                                     density = average_G_withsurvival_Roz_pself0.01)


ggplot(df_average_G_withsurvival_Roz_pself0.01, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))





#########################################################################################################
#########################################################################################################

# CREATING THE AVERAGE GDS FOR KUNGALV AND GULLMARSFJORD WITH pself = 0.1

#########################################################################################################
#########################################################################################################

setwd("Histogram_densities/5.Repr_modes_together_survival&nosurvival_pself0.1")


#####
# SAD
#####
h1_K_withoutsurvival_SAD = read.table("p5_K_BK_allreprmodes_withoutsurvival_numalleles_density.txt")
h1_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h1_K_withoutsurvival_SAD$x)

h2_K_withoutsurvival_SAD = read.table("p5_K_K_allreprmodes_withoutsurvival_numalleles_density.txt")
h2_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h2_K_withoutsurvival_SAD$x)

h3_K_withoutsurvival_SAD = read.table("p5_K_KR_allreprmodes_withoutsurvival_numalleles_density.txt")
h3_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h3_K_withoutsurvival_SAD$x)

h4_K_withoutsurvival_SAD = read.table("p5_K_LD_allreprmodes_withoutsurvival_numalleles_density.txt")
h4_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h4_K_withoutsurvival_SAD$x)

h5_K_withoutsurvival_SAD = read.table("p5_K_NG_allreprmodes_withoutsurvival_numalleles_density.txt")
h5_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h5_K_withoutsurvival_SAD$x)

h6_K_withoutsurvival_SAD = read.table("p5_K_NI_allreprmodes_withoutsurvival_numalleles_density.txt")
h6_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h6_K_withoutsurvival_SAD$x)

h7_K_withoutsurvival_SAD = read.table("p5_K_OK_allreprmodes_withoutsurvival_numalleles_density.txt")
h7_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h7_K_withoutsurvival_SAD$x)

h8_K_withoutsurvival_SAD = read.table("p5_K_ON_allreprmodes_withoutsurvival_numalleles_density.txt")
h8_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h8_K_withoutsurvival_SAD$x)

h9_K_withoutsurvival_SAD = read.table("p5_K_RT_allreprmodes_withoutsurvival_numalleles_density.txt")
h9_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h9_K_withoutsurvival_SAD$x)

h10_K_withoutsurvival_SAD = read.table("p5_K_SK_allreprmodes_withoutsurvival_numalleles_density.txt")
h10_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_K_withoutsurvival_SAD$x)-1, 1),
                                       density = h10_K_withoutsurvival_SAD$x)

h11_K_withoutsurvival_SAD = read.table("p5_K_SO_allreprmodes_withoutsurvival_numalleles_density.txt")
h11_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_K_withoutsurvival_SAD$x)-1, 1),
                                       density = h11_K_withoutsurvival_SAD$x)

average_K_withoutsurvival_SAD_pself0.1 = apply(cbind(h1_K_withoutsurvival_SAD$density, h2_K_withoutsurvival_SAD$density,
                                                      h3_K_withoutsurvival_SAD$density, h4_K_withoutsurvival_SAD$density,
                                                      h5_K_withoutsurvival_SAD$density,h6_K_withoutsurvival_SAD$density,
                                                      h7_K_withoutsurvival_SAD$density,h8_K_withoutsurvival_SAD$density,
                                                      h9_K_withoutsurvival_SAD$density,h10_K_withoutsurvival_SAD$density,
                                                      h11_K_withoutsurvival_SAD$density),1,sum)

df_average_K_withoutsurvival_SAD_pself0.1 = data.frame(Genetic_distance = seq(0,length(average_K_withoutsurvival_SAD_pself0.1)-1, 1), 
                                                        density = average_K_withoutsurvival_SAD_pself0.1)

ggplot(df_average_K_withoutsurvival_SAD_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))


#####
# Rozenfeld
#####

h1_K_withoutsurvival_Roz = read.table("p6_K_BK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h1_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h1_K_withoutsurvival_Roz$x)

h2_K_withoutsurvival_Roz = read.table("p6_K_K_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h2_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h2_K_withoutsurvival_Roz$x)

h3_K_withoutsurvival_Roz = read.table("p6_K_KR_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h3_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h3_K_withoutsurvival_Roz$x)

h4_K_withoutsurvival_Roz = read.table("p6_K_LD_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h4_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h4_K_withoutsurvival_Roz$x)

h5_K_withoutsurvival_Roz = read.table("p6_K_NG_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h5_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h5_K_withoutsurvival_Roz$x)

h6_K_withoutsurvival_Roz = read.table("p6_K_NI_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h6_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h6_K_withoutsurvival_Roz$x)

h7_K_withoutsurvival_Roz = read.table("p6_K_OK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h7_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h7_K_withoutsurvival_Roz$x)

h8_K_withoutsurvival_Roz = read.table("p6_K_ON_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h8_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h8_K_withoutsurvival_Roz$x)

h9_K_withoutsurvival_Roz = read.table("p6_K_RT_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h9_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h9_K_withoutsurvival_Roz$x)

h10_K_withoutsurvival_Roz = read.table("p6_K_SK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h10_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_K_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h10_K_withoutsurvival_Roz$x)

h11_K_withoutsurvival_Roz = read.table("p6_K_SO_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h11_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_K_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h11_K_withoutsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_K_withoutsurvival_Roz, h2_K_withoutsurvival_Roz, #List of data frames
                h3_K_withoutsurvival_Roz, h4_K_withoutsurvival_Roz,
                h5_K_withoutsurvival_Roz,h6_K_withoutsurvival_Roz,
                h7_K_withoutsurvival_Roz,h8_K_withoutsurvival_Roz,
                h9_K_withoutsurvival_Roz,h10_K_withoutsurvival_Roz,
                h11_K_withoutsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

names(df)
average_K_withoutsurvival_Roz_pself0.1 = apply(cbind(df[,2],df[,4],df[,6],
                                                      df[,8],df[,10],df[,12],df[,14],
                                                      df[,16],df[,18],df[,20],
                                                      df[,22]),1,sum, na.rm=T)

df_average_K_withoutsurvival_Roz_pself0.1 = data.frame(Genetic_distance = seq(0,(length(average_K_withoutsurvival_Roz_pself0.1)-1)/10, 0.1), 
                                                        density = average_K_withoutsurvival_Roz_pself0.1)


ggplot(df_average_K_withoutsurvival_Roz_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nRozenfeld's Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



#################### 
# With survival (Kungälv - pself 0.1)
#################### 

#####
# SAD
#####
h1_K_withsurvival_SAD = read.table("p7_K_BK_allreprmodes_withsurvival_numalleles_density.txt")
h1_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_K_withsurvival_SAD$x)-1, 1),
                                   density = h1_K_withsurvival_SAD$x)

h2_K_withsurvival_SAD = read.table("p7_K_K_allreprmodes_withsurvival_numalleles_density.txt")
h2_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_K_withsurvival_SAD$x)-1, 1),
                                   density = h2_K_withsurvival_SAD$x)

h3_K_withsurvival_SAD = read.table("p7_K_KR_allreprmodes_withsurvival_numalleles_density.txt")
h3_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_K_withsurvival_SAD$x)-1, 1),
                                   density = h3_K_withsurvival_SAD$x)

h4_K_withsurvival_SAD = read.table("p7_K_LD_allreprmodes_withsurvival_numalleles_density.txt")
h4_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_K_withsurvival_SAD$x)-1, 1),
                                   density = h4_K_withsurvival_SAD$x)

h5_K_withsurvival_SAD = read.table("p7_K_NG_allreprmodes_withsurvival_numalleles_density.txt")
h5_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_K_withsurvival_SAD$x)-1, 1),
                                   density = h5_K_withsurvival_SAD$x)

h6_K_withsurvival_SAD = read.table("p7_K_NI_allreprmodes_withsurvival_numalleles_density.txt")
h6_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_K_withsurvival_SAD$x)-1, 1),
                                   density = h6_K_withsurvival_SAD$x)

h7_K_withsurvival_SAD = read.table("p7_K_OK_allreprmodes_withsurvival_numalleles_density.txt")
h7_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_K_withsurvival_SAD$x)-1, 1),
                                   density = h7_K_withsurvival_SAD$x)

h8_K_withsurvival_SAD = read.table("p7_K_ON_allreprmodes_withsurvival_numalleles_density.txt")
h8_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_K_withsurvival_SAD$x)-1, 1),
                                   density = h8_K_withsurvival_SAD$x)

h9_K_withsurvival_SAD = read.table("p7_K_RT_allreprmodes_withsurvival_numalleles_density.txt")
h9_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_K_withsurvival_SAD$x)-1, 1),
                                   density = h9_K_withsurvival_SAD$x)

h10_K_withsurvival_SAD = read.table("p7_K_SK_allreprmodes_withsurvival_numalleles_density.txt")
h10_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_K_withsurvival_SAD$x)-1, 1),
                                    density = h10_K_withsurvival_SAD$x)

h11_K_withsurvival_SAD = read.table("p7_K_SO_allreprmodes_withsurvival_numalleles_density.txt")
h11_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_K_withsurvival_SAD$x)-1, 1),
                                    density = h11_K_withsurvival_SAD$x)

average_K_withsurvival_SAD_pself0.1 = apply(cbind(h1_K_withsurvival_SAD$density, h2_K_withsurvival_SAD$density,
                                                   h3_K_withsurvival_SAD$density, h4_K_withsurvival_SAD$density,
                                                   h5_K_withsurvival_SAD$density,h6_K_withsurvival_SAD$density,
                                                   h7_K_withsurvival_SAD$density,h8_K_withsurvival_SAD$density,
                                                   h9_K_withsurvival_SAD$density,h10_K_withsurvival_SAD$density,
                                                   h11_K_withsurvival_SAD$density),1,sum)

df_average_K_withsurvival_SAD_pself0.1 = data.frame(Genetic_distance = seq(0,length(average_K_withsurvival_SAD_pself0.1)-1, 1), 
                                                     density = average_K_withsurvival_SAD_pself0.1)

ggplot(df_average_K_withsurvival_SAD_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))

#####
# Rozenfeld
#####

h1_K_withsurvival_Roz = read.table("p8_K_BK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h1_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h1_K_withsurvival_Roz$x)

h2_K_withsurvival_Roz = read.table("p8_K_K_allreprmodes_withsurvival_Rozenfeld_density.txt")
h2_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h2_K_withsurvival_Roz$x)

h3_K_withsurvival_Roz = read.table("p8_K_KR_allreprmodes_withsurvival_Rozenfeld_density.txt")
h3_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h3_K_withsurvival_Roz$x)

h4_K_withsurvival_Roz = read.table("p8_K_LD_allreprmodes_withsurvival_Rozenfeld_density.txt")
h4_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h4_K_withsurvival_Roz$x)

h5_K_withsurvival_Roz = read.table("p8_K_NG_allreprmodes_withsurvival_Rozenfeld_density.txt")
h5_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h5_K_withsurvival_Roz$x)

h6_K_withsurvival_Roz = read.table("p8_K_NI_allreprmodes_withsurvival_Rozenfeld_density.txt")
h6_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h6_K_withsurvival_Roz$x)

h7_K_withsurvival_Roz = read.table("p8_K_OK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h7_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h7_K_withsurvival_Roz$x)

h8_K_withsurvival_Roz = read.table("p8_K_ON_allreprmodes_withsurvival_Rozenfeld_density.txt")
h8_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h8_K_withsurvival_Roz$x)

h9_K_withsurvival_Roz = read.table("p8_K_RT_allreprmodes_withsurvival_Rozenfeld_density.txt")
h9_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h9_K_withsurvival_Roz$x)

h10_K_withsurvival_Roz = read.table("p8_K_SK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h10_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_K_withsurvival_Roz$x)/10)-1, 1),
                                    density = h10_K_withsurvival_Roz$x)

h11_K_withsurvival_Roz = read.table("p8_K_SO_allreprmodes_withsurvival_Rozenfeld_density.txt")
h11_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_K_withsurvival_Roz$x)/10)-1, 1),
                                    density = h11_K_withsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_K_withsurvival_Roz, h2_K_withsurvival_Roz, #List of data frames
                h3_K_withsurvival_Roz, h4_K_withsurvival_Roz,
                h5_K_withsurvival_Roz,h6_K_withsurvival_Roz,
                h7_K_withsurvival_Roz,h8_K_withsurvival_Roz,
                h9_K_withsurvival_Roz,h10_K_withsurvival_Roz,
                h11_K_withsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

average_K_withsurvival_Roz_pself0.1 = apply(cbind(df[,2],df[,4],df[,6],
                                                   df[,8],df[,10],df[,12],df[,14],
                                                   df[,16],df[,18],df[,20],
                                                   df[,22]),1,sum, na.rm=T)

df_average_K_withsurvival_Roz_pself0.1 = data.frame(Genetic_distance = seq(0,(length(average_K_withsurvival_Roz_pself0.1)-1)/10, 0.1), 
                                                     density = average_K_withsurvival_Roz_pself0.1)


ggplot(df_average_K_withsurvival_Roz_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



###########################################
# Gullmarsfjord - pself 0.1
###########################################




#################### 
# Without survival (Gullmarsfjord - pself 0.1)
#################### 


#####
# SAD
#####
h1_G_withoutsurvival_SAD = read.table("p5_G_AF_allreprmodes_withoutsurvival_numalleles_density.txt")
h1_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h1_G_withoutsurvival_SAD$x)

h2_G_withoutsurvival_SAD = read.table("p5_G_AS_allreprmodes_withoutsurvival_numalleles_density.txt")
h2_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h2_G_withoutsurvival_SAD$x)

h3_G_withoutsurvival_SAD = read.table("p5_G_BB_allreprmodes_withoutsurvival_numalleles_density.txt")
h3_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h3_G_withoutsurvival_SAD$x)

h4_G_withoutsurvival_SAD = read.table("p5_G_BV_allreprmodes_withoutsurvival_numalleles_density.txt")
h4_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h4_G_withoutsurvival_SAD$x)

h5_G_withoutsurvival_SAD = read.table("p5_G_GB_allreprmodes_withoutsurvival_numalleles_density.txt")
h5_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h5_G_withoutsurvival_SAD$x)

h6_G_withoutsurvival_SAD = read.table("p5_G_NB_allreprmodes_withoutsurvival_numalleles_density.txt")
h6_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h6_G_withoutsurvival_SAD$x)

h7_G_withoutsurvival_SAD = read.table("p5_G_RX_allreprmodes_withoutsurvival_numalleles_density.txt")
h7_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h7_G_withoutsurvival_SAD$x)

h8_G_withoutsurvival_SAD = read.table("p5_G_SG_allreprmodes_withoutsurvival_numalleles_density.txt")
h8_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h8_G_withoutsurvival_SAD$x)

h9_G_withoutsurvival_SAD = read.table("p5_G_SK_allreprmodes_withoutsurvival_numalleles_density.txt")
h9_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h9_G_withoutsurvival_SAD$x)

h10_G_withoutsurvival_SAD = read.table("p5_Snack_allreprmodes_withoutsurvival_numalleles_density.txt")
h10_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_G_withoutsurvival_SAD$x)-1, 1),
                                       density = h10_G_withoutsurvival_SAD$x)

h11_G_withoutsurvival_SAD = read.table("p5_Torg_allreprmodes_withoutsurvival_numalleles_density.txt")
h11_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_G_withoutsurvival_SAD$x)-1, 1),
                                       density = h11_G_withoutsurvival_SAD$x)

average_G_withoutsurvival_SAD_pself0.1 = apply(cbind(h1_G_withoutsurvival_SAD$density, h2_G_withoutsurvival_SAD$density,
                                                      h3_G_withoutsurvival_SAD$density, h4_G_withoutsurvival_SAD$density,
                                                      h5_G_withoutsurvival_SAD$density,h6_G_withoutsurvival_SAD$density,
                                                      h7_G_withoutsurvival_SAD$density,h8_G_withoutsurvival_SAD$density,
                                                      h9_G_withoutsurvival_SAD$density,h10_G_withoutsurvival_SAD$density,
                                                      h11_G_withoutsurvival_SAD$density),1,sum)

df_average_G_withoutsurvival_SAD_pself0.1 = data.frame(Genetic_distance = seq(0,length(average_G_withoutsurvival_SAD_pself0.1)-1, 1), 
                                                        density = average_G_withoutsurvival_SAD_pself0.1)

ggplot(df_average_G_withoutsurvival_SAD_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))


#####
# Rozenfeld
#####

h1_G_withoutsurvival_Roz = read.table("p6_G_AF_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h1_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h1_G_withoutsurvival_Roz$x)

h2_G_withoutsurvival_Roz = read.table("p6_G_AS_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h2_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h2_G_withoutsurvival_Roz$x)

h3_G_withoutsurvival_Roz = read.table("p6_G_BB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h3_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h3_G_withoutsurvival_Roz$x)

h4_G_withoutsurvival_Roz = read.table("p6_G_BV_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h4_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h4_G_withoutsurvival_Roz$x)

h5_G_withoutsurvival_Roz = read.table("p6_G_GB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h5_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h5_G_withoutsurvival_Roz$x)

h6_G_withoutsurvival_Roz = read.table("p6_G_NB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h6_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h6_G_withoutsurvival_Roz$x)

h7_G_withoutsurvival_Roz = read.table("p6_G_RX_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h7_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h7_G_withoutsurvival_Roz$x)

h8_G_withoutsurvival_Roz = read.table("p6_G_SG_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h8_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h8_G_withoutsurvival_Roz$x)

h9_G_withoutsurvival_Roz = read.table("p6_G_SK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h9_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h9_G_withoutsurvival_Roz$x)

h10_G_withoutsurvival_Roz = read.table("p6_Snack_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h10_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_G_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h10_G_withoutsurvival_Roz$x)

h11_G_withoutsurvival_Roz = read.table("p6_Torg_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h11_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_G_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h11_G_withoutsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_G_withoutsurvival_Roz, h2_G_withoutsurvival_Roz, #List of data frames
                h3_G_withoutsurvival_Roz, h4_G_withoutsurvival_Roz,
                h5_G_withoutsurvival_Roz,h6_G_withoutsurvival_Roz,
                h7_G_withoutsurvival_Roz,h8_G_withoutsurvival_Roz,
                h9_G_withoutsurvival_Roz,h10_G_withoutsurvival_Roz,
                h11_G_withoutsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

names(df)
average_G_withoutsurvival_Roz_pself0.1 = apply(cbind(df[,2],df[,4],df[,6],
                                                      df[,8],df[,10],df[,12],df[,14],
                                                      df[,16],df[,18],df[,20],
                                                      df[,22]),1,sum, na.rm=T)

df_average_G_withoutsurvival_Roz_pself0.1 = data.frame(Genetic_distance = seq(0,(length(average_G_withoutsurvival_Roz_pself0.1)-1)/10, 0.1), 
                                                        density = average_G_withoutsurvival_Roz_pself0.1)


ggplot(df_average_G_withoutsurvival_Roz_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nRozenfeld's Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



#################### 
# With survival (Gullmarsfjord - pself 0.1)
#################### 

#####
# SAD
#####
h1_G_withsurvival_SAD = read.table("p7_G_AF_allreprmodes_withsurvival_numalleles_density.txt")
h1_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_G_withsurvival_SAD$x)-1, 1),
                                   density = h1_G_withsurvival_SAD$x)

h2_G_withsurvival_SAD = read.table("p7_G_AS_allreprmodes_withsurvival_numalleles_density.txt")
h2_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_G_withsurvival_SAD$x)-1, 1),
                                   density = h2_G_withsurvival_SAD$x)

h3_G_withsurvival_SAD = read.table("p7_G_BB_allreprmodes_withsurvival_numalleles_density.txt")
h3_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_G_withsurvival_SAD$x)-1, 1),
                                   density = h3_G_withsurvival_SAD$x)

h4_G_withsurvival_SAD = read.table("p7_G_BV_allreprmodes_withsurvival_numalleles_density.txt")
h4_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_G_withsurvival_SAD$x)-1, 1),
                                   density = h4_G_withsurvival_SAD$x)

h5_G_withsurvival_SAD = read.table("p7_G_GB_allreprmodes_withsurvival_numalleles_density.txt")
h5_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_G_withsurvival_SAD$x)-1, 1),
                                   density = h5_G_withsurvival_SAD$x)

h6_G_withsurvival_SAD = read.table("p7_G_NB_allreprmodes_withsurvival_numalleles_density.txt")
h6_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_G_withsurvival_SAD$x)-1, 1),
                                   density = h6_G_withsurvival_SAD$x)

h7_G_withsurvival_SAD = read.table("p7_G_RX_allreprmodes_withsurvival_numalleles_density.txt")
h7_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_G_withsurvival_SAD$x)-1, 1),
                                   density = h7_G_withsurvival_SAD$x)

h8_G_withsurvival_SAD = read.table("p7_G_SG_allreprmodes_withsurvival_numalleles_density.txt")
h8_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_G_withsurvival_SAD$x)-1, 1),
                                   density = h8_G_withsurvival_SAD$x)

h9_G_withsurvival_SAD = read.table("p7_G_SK_allreprmodes_withsurvival_numalleles_density.txt")
h9_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_G_withsurvival_SAD$x)-1, 1),
                                   density = h9_G_withsurvival_SAD$x)

h10_G_withsurvival_SAD = read.table("p7_Snack_allreprmodes_withsurvival_numalleles_density.txt")
h10_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_G_withsurvival_SAD$x)-1, 1),
                                    density = h10_G_withsurvival_SAD$x)

h11_G_withsurvival_SAD = read.table("p7_Torg_allreprmodes_withsurvival_numalleles_density.txt")
h11_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_G_withsurvival_SAD$x)-1, 1),
                                    density = h11_G_withsurvival_SAD$x)

average_G_withsurvival_SAD_pself0.1 = apply(cbind(h1_G_withsurvival_SAD$density, h2_G_withsurvival_SAD$density,
                                                   h3_G_withsurvival_SAD$density, h4_G_withsurvival_SAD$density,
                                                   h5_G_withsurvival_SAD$density,h6_G_withsurvival_SAD$density,
                                                   h7_G_withsurvival_SAD$density,h8_G_withsurvival_SAD$density,
                                                   h9_G_withsurvival_SAD$density,h10_G_withsurvival_SAD$density,
                                                   h11_G_withsurvival_SAD$density),1,sum)

df_average_G_withsurvival_SAD_pself0.1 = data.frame(Genetic_distance = seq(0,length(average_G_withsurvival_SAD_pself0.1)-1, 1), 
                                                     density = average_G_withsurvival_SAD_pself0.1)

ggplot(df_average_G_withsurvival_SAD_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))

#####
# Rozenfeld
#####

h1_G_withsurvival_Roz = read.table("p8_G_AF_allreprmodes_withsurvival_Rozenfeld_density.txt")
h1_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h1_G_withsurvival_Roz$x)

h2_G_withsurvival_Roz = read.table("p8_G_AS_allreprmodes_withsurvival_Rozenfeld_density.txt")
h2_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h2_G_withsurvival_Roz$x)

h3_G_withsurvival_Roz = read.table("p8_G_BB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h3_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h3_G_withsurvival_Roz$x)

h4_G_withsurvival_Roz = read.table("p8_G_BV_allreprmodes_withsurvival_Rozenfeld_density.txt")
h4_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h4_G_withsurvival_Roz$x)

h5_G_withsurvival_Roz = read.table("p8_G_GB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h5_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h5_G_withsurvival_Roz$x)

h6_G_withsurvival_Roz = read.table("p8_G_NB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h6_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h6_G_withsurvival_Roz$x)

h7_G_withsurvival_Roz = read.table("p8_G_RX_allreprmodes_withsurvival_Rozenfeld_density.txt")
h7_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h7_G_withsurvival_Roz$x)

h8_G_withsurvival_Roz = read.table("p8_G_SG_allreprmodes_withsurvival_Rozenfeld_density.txt")
h8_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h8_G_withsurvival_Roz$x)

h9_G_withsurvival_Roz = read.table("p8_G_SK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h9_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h9_G_withsurvival_Roz$x)

h10_G_withsurvival_Roz = read.table("p8_Snack_allreprmodes_withsurvival_Rozenfeld_density.txt")
h10_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_G_withsurvival_Roz$x)/10)-1, 1),
                                    density = h10_G_withsurvival_Roz$x)

h11_G_withsurvival_Roz = read.table("p8_Torg_allreprmodes_withsurvival_Rozenfeld_density.txt")
h11_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_G_withsurvival_Roz$x)/10)-1, 1),
                                    density = h11_G_withsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_G_withsurvival_Roz, h2_G_withsurvival_Roz, #List of data frames
                h3_G_withsurvival_Roz, h4_G_withsurvival_Roz,
                h5_G_withsurvival_Roz,h6_G_withsurvival_Roz,
                h7_G_withsurvival_Roz,h8_G_withsurvival_Roz,
                h9_G_withsurvival_Roz,h10_G_withsurvival_Roz,
                h11_G_withsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

average_G_withsurvival_Roz_pself0.1 = apply(cbind(df[,2],df[,4],df[,6],
                                                   df[,8],df[,10],df[,12],df[,14],
                                                   df[,16],df[,18],df[,20],
                                                   df[,22]),1,sum, na.rm=T)

df_average_G_withsurvival_Roz_pself0.1 = data.frame(Genetic_distance = seq(0,(length(average_G_withsurvival_Roz_pself0.1)-1)/10, 0.1), 
                                                     density = average_G_withsurvival_Roz_pself0.1)


ggplot(df_average_G_withsurvival_Roz_pself0.1, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))












#########################################################################################################
#########################################################################################################

# CREATING THE AVERAGE GDS FOR KUNGALV AND GULLMARSFJORD WITH pself = 0.3

#########################################################################################################
#########################################################################################################

setwd("Histogram_densities/7.Repr_modes_together_survival&nosurvival_pself0.3")



#####
# SAD
#####
h1_K_withoutsurvival_SAD = read.table("p5_K_BK_allreprmodes_withoutsurvival_numalleles_density.txt")
h1_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h1_K_withoutsurvival_SAD$x)

h2_K_withoutsurvival_SAD = read.table("p5_K_K_allreprmodes_withoutsurvival_numalleles_density.txt")
h2_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h2_K_withoutsurvival_SAD$x)

h3_K_withoutsurvival_SAD = read.table("p5_K_KR_allreprmodes_withoutsurvival_numalleles_density.txt")
h3_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h3_K_withoutsurvival_SAD$x)

h4_K_withoutsurvival_SAD = read.table("p5_K_LD_allreprmodes_withoutsurvival_numalleles_density.txt")
h4_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h4_K_withoutsurvival_SAD$x)

h5_K_withoutsurvival_SAD = read.table("p5_K_NG_allreprmodes_withoutsurvival_numalleles_density.txt")
h5_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h5_K_withoutsurvival_SAD$x)

h6_K_withoutsurvival_SAD = read.table("p5_K_NI_allreprmodes_withoutsurvival_numalleles_density.txt")
h6_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h6_K_withoutsurvival_SAD$x)

h7_K_withoutsurvival_SAD = read.table("p5_K_OK_allreprmodes_withoutsurvival_numalleles_density.txt")
h7_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h7_K_withoutsurvival_SAD$x)

h8_K_withoutsurvival_SAD = read.table("p5_K_ON_allreprmodes_withoutsurvival_numalleles_density.txt")
h8_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h8_K_withoutsurvival_SAD$x)

h9_K_withoutsurvival_SAD = read.table("p5_K_RT_allreprmodes_withoutsurvival_numalleles_density.txt")
h9_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_K_withoutsurvival_SAD$x)-1, 1),
                                      density = h9_K_withoutsurvival_SAD$x)

h10_K_withoutsurvival_SAD = read.table("p5_K_SK_allreprmodes_withoutsurvival_numalleles_density.txt")
h10_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_K_withoutsurvival_SAD$x)-1, 1),
                                       density = h10_K_withoutsurvival_SAD$x)

h11_K_withoutsurvival_SAD = read.table("p5_K_SO_allreprmodes_withoutsurvival_numalleles_density.txt")
h11_K_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_K_withoutsurvival_SAD$x)-1, 1),
                                       density = h11_K_withoutsurvival_SAD$x)

average_K_withoutsurvival_SAD_pself0.3 = apply(cbind(h1_K_withoutsurvival_SAD$density, h2_K_withoutsurvival_SAD$density,
                                                     h3_K_withoutsurvival_SAD$density, h4_K_withoutsurvival_SAD$density,
                                                     h5_K_withoutsurvival_SAD$density,h6_K_withoutsurvival_SAD$density,
                                                     h7_K_withoutsurvival_SAD$density,h8_K_withoutsurvival_SAD$density,
                                                     h9_K_withoutsurvival_SAD$density,h10_K_withoutsurvival_SAD$density,
                                                     h11_K_withoutsurvival_SAD$density),1,sum)

df_average_K_withoutsurvival_SAD_pself0.3 = data.frame(Genetic_distance = seq(0,length(average_K_withoutsurvival_SAD_pself0.3)-1, 1), 
                                                       density = average_K_withoutsurvival_SAD_pself0.3)

ggplot(df_average_K_withoutsurvival_SAD_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))


#####
# Rozenfeld
#####

h1_K_withoutsurvival_Roz = read.table("p6_K_BK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h1_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h1_K_withoutsurvival_Roz$x)

h2_K_withoutsurvival_Roz = read.table("p6_K_K_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h2_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h2_K_withoutsurvival_Roz$x)

h3_K_withoutsurvival_Roz = read.table("p6_K_KR_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h3_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h3_K_withoutsurvival_Roz$x)

h4_K_withoutsurvival_Roz = read.table("p6_K_LD_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h4_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h4_K_withoutsurvival_Roz$x)

h5_K_withoutsurvival_Roz = read.table("p6_K_NG_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h5_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h5_K_withoutsurvival_Roz$x)

h6_K_withoutsurvival_Roz = read.table("p6_K_NI_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h6_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h6_K_withoutsurvival_Roz$x)

h7_K_withoutsurvival_Roz = read.table("p6_K_OK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h7_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h7_K_withoutsurvival_Roz$x)

h8_K_withoutsurvival_Roz = read.table("p6_K_ON_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h8_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h8_K_withoutsurvival_Roz$x)

h9_K_withoutsurvival_Roz = read.table("p6_K_RT_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h9_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_K_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h9_K_withoutsurvival_Roz$x)

h10_K_withoutsurvival_Roz = read.table("p6_K_SK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h10_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_K_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h10_K_withoutsurvival_Roz$x)

h11_K_withoutsurvival_Roz = read.table("p6_K_SO_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h11_K_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_K_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h11_K_withoutsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_K_withoutsurvival_Roz, h2_K_withoutsurvival_Roz, #List of data frames
                h3_K_withoutsurvival_Roz, h4_K_withoutsurvival_Roz,
                h5_K_withoutsurvival_Roz,h6_K_withoutsurvival_Roz,
                h7_K_withoutsurvival_Roz,h8_K_withoutsurvival_Roz,
                h9_K_withoutsurvival_Roz,h10_K_withoutsurvival_Roz,
                h11_K_withoutsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

names(df)
average_K_withoutsurvival_Roz_pself0.3 = apply(cbind(df[,2],df[,4],df[,6],
                                                     df[,8],df[,10],df[,12],df[,14],
                                                     df[,16],df[,18],df[,20],
                                                     df[,22]),1,sum, na.rm=T)

df_average_K_withoutsurvival_Roz_pself0.3 = data.frame(Genetic_distance = seq(0,(length(average_K_withoutsurvival_Roz_pself0.3)-1)/10, 0.1), 
                                                       density = average_K_withoutsurvival_Roz_pself0.3)


ggplot(df_average_K_withoutsurvival_Roz_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nRozenfeld's Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



#################### 
# With survival (Kungälv - pself 0.3)
#################### 

#####
# SAD
#####
h1_K_withsurvival_SAD = read.table("p7_K_BK_allreprmodes_withsurvival_numalleles_density.txt")
h1_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_K_withsurvival_SAD$x)-1, 1),
                                   density = h1_K_withsurvival_SAD$x)

h2_K_withsurvival_SAD = read.table("p7_K_K_allreprmodes_withsurvival_numalleles_density.txt")
h2_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_K_withsurvival_SAD$x)-1, 1),
                                   density = h2_K_withsurvival_SAD$x)

h3_K_withsurvival_SAD = read.table("p7_K_KR_allreprmodes_withsurvival_numalleles_density.txt")
h3_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_K_withsurvival_SAD$x)-1, 1),
                                   density = h3_K_withsurvival_SAD$x)

h4_K_withsurvival_SAD = read.table("p7_K_LD_allreprmodes_withsurvival_numalleles_density.txt")
h4_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_K_withsurvival_SAD$x)-1, 1),
                                   density = h4_K_withsurvival_SAD$x)

h5_K_withsurvival_SAD = read.table("p7_K_NG_allreprmodes_withsurvival_numalleles_density.txt")
h5_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_K_withsurvival_SAD$x)-1, 1),
                                   density = h5_K_withsurvival_SAD$x)

h6_K_withsurvival_SAD = read.table("p7_K_NI_allreprmodes_withsurvival_numalleles_density.txt")
h6_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_K_withsurvival_SAD$x)-1, 1),
                                   density = h6_K_withsurvival_SAD$x)

h7_K_withsurvival_SAD = read.table("p7_K_OK_allreprmodes_withsurvival_numalleles_density.txt")
h7_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_K_withsurvival_SAD$x)-1, 1),
                                   density = h7_K_withsurvival_SAD$x)

h8_K_withsurvival_SAD = read.table("p7_K_ON_allreprmodes_withsurvival_numalleles_density.txt")
h8_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_K_withsurvival_SAD$x)-1, 1),
                                   density = h8_K_withsurvival_SAD$x)

h9_K_withsurvival_SAD = read.table("p7_K_RT_allreprmodes_withsurvival_numalleles_density.txt")
h9_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_K_withsurvival_SAD$x)-1, 1),
                                   density = h9_K_withsurvival_SAD$x)

h10_K_withsurvival_SAD = read.table("p7_K_SK_allreprmodes_withsurvival_numalleles_density.txt")
h10_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_K_withsurvival_SAD$x)-1, 1),
                                    density = h10_K_withsurvival_SAD$x)

h11_K_withsurvival_SAD = read.table("p7_K_SO_allreprmodes_withsurvival_numalleles_density.txt")
h11_K_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_K_withsurvival_SAD$x)-1, 1),
                                    density = h11_K_withsurvival_SAD$x)

average_K_withsurvival_SAD_pself0.3 = apply(cbind(h1_K_withsurvival_SAD$density, h2_K_withsurvival_SAD$density,
                                                  h3_K_withsurvival_SAD$density, h4_K_withsurvival_SAD$density,
                                                  h5_K_withsurvival_SAD$density,h6_K_withsurvival_SAD$density,
                                                  h7_K_withsurvival_SAD$density,h8_K_withsurvival_SAD$density,
                                                  h9_K_withsurvival_SAD$density,h10_K_withsurvival_SAD$density,
                                                  h11_K_withsurvival_SAD$density),1,sum)

df_average_K_withsurvival_SAD_pself0.3 = data.frame(Genetic_distance = seq(0,length(average_K_withsurvival_SAD_pself0.3)-1, 1), 
                                                    density = average_K_withsurvival_SAD_pself0.3)

ggplot(df_average_K_withsurvival_SAD_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))

#####
# Rozenfeld
#####

h1_K_withsurvival_Roz = read.table("p8_K_BK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h1_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h1_K_withsurvival_Roz$x)

h2_K_withsurvival_Roz = read.table("p8_K_K_allreprmodes_withsurvival_Rozenfeld_density.txt")
h2_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h2_K_withsurvival_Roz$x)

h3_K_withsurvival_Roz = read.table("p8_K_KR_allreprmodes_withsurvival_Rozenfeld_density.txt")
h3_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h3_K_withsurvival_Roz$x)

h4_K_withsurvival_Roz = read.table("p8_K_LD_allreprmodes_withsurvival_Rozenfeld_density.txt")
h4_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h4_K_withsurvival_Roz$x)

h5_K_withsurvival_Roz = read.table("p8_K_NG_allreprmodes_withsurvival_Rozenfeld_density.txt")
h5_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h5_K_withsurvival_Roz$x)

h6_K_withsurvival_Roz = read.table("p8_K_NI_allreprmodes_withsurvival_Rozenfeld_density.txt")
h6_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h6_K_withsurvival_Roz$x)

h7_K_withsurvival_Roz = read.table("p8_K_OK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h7_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h7_K_withsurvival_Roz$x)

h8_K_withsurvival_Roz = read.table("p8_K_ON_allreprmodes_withsurvival_Rozenfeld_density.txt")
h8_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h8_K_withsurvival_Roz$x)

h9_K_withsurvival_Roz = read.table("p8_K_RT_allreprmodes_withsurvival_Rozenfeld_density.txt")
h9_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_K_withsurvival_Roz$x)/10)-1, 1),
                                   density = h9_K_withsurvival_Roz$x)

h10_K_withsurvival_Roz = read.table("p8_K_SK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h10_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_K_withsurvival_Roz$x)/10)-1, 1),
                                    density = h10_K_withsurvival_Roz$x)

h11_K_withsurvival_Roz = read.table("p8_K_SO_allreprmodes_withsurvival_Rozenfeld_density.txt")
h11_K_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_K_withsurvival_Roz$x)/10)-1, 1),
                                    density = h11_K_withsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_K_withsurvival_Roz, h2_K_withsurvival_Roz, #List of data frames
                h3_K_withsurvival_Roz, h4_K_withsurvival_Roz,
                h5_K_withsurvival_Roz,h6_K_withsurvival_Roz,
                h7_K_withsurvival_Roz,h8_K_withsurvival_Roz,
                h9_K_withsurvival_Roz,h10_K_withsurvival_Roz,
                h11_K_withsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

average_K_withsurvival_Roz_pself0.3 = apply(cbind(df[,2],df[,4],df[,6],
                                                  df[,8],df[,10],df[,12],df[,14],
                                                  df[,16],df[,18],df[,20],
                                                  df[,22]),1,sum, na.rm=T)

df_average_K_withsurvival_Roz_pself0.3 = data.frame(Genetic_distance = seq(0,(length(average_K_withsurvival_Roz_pself0.3)-1)/10, 0.1), 
                                                    density = average_K_withsurvival_Roz_pself0.3)


ggplot(df_average_K_withsurvival_Roz_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



###########################################
# Gullmarsfjord - pself 0.3
###########################################




#################### 
# Without survival (Gullmarsfjord - pself 0.3)
#################### 


#####
# SAD
#####
h1_G_withoutsurvival_SAD = read.table("p5_G_AF_allreprmodes_withoutsurvival_numalleles_density.txt")
h1_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h1_G_withoutsurvival_SAD$x)

h2_G_withoutsurvival_SAD = read.table("p5_G_AS_allreprmodes_withoutsurvival_numalleles_density.txt")
h2_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h2_G_withoutsurvival_SAD$x)

h3_G_withoutsurvival_SAD = read.table("p5_G_BB_allreprmodes_withoutsurvival_numalleles_density.txt")
h3_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h3_G_withoutsurvival_SAD$x)

h4_G_withoutsurvival_SAD = read.table("p5_G_BV_allreprmodes_withoutsurvival_numalleles_density.txt")
h4_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h4_G_withoutsurvival_SAD$x)

h5_G_withoutsurvival_SAD = read.table("p5_G_GB_allreprmodes_withoutsurvival_numalleles_density.txt")
h5_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h5_G_withoutsurvival_SAD$x)

h6_G_withoutsurvival_SAD = read.table("p5_G_NB_allreprmodes_withoutsurvival_numalleles_density.txt")
h6_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h6_G_withoutsurvival_SAD$x)

h7_G_withoutsurvival_SAD = read.table("p5_G_RX_allreprmodes_withoutsurvival_numalleles_density.txt")
h7_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h7_G_withoutsurvival_SAD$x)

h8_G_withoutsurvival_SAD = read.table("p5_G_SG_allreprmodes_withoutsurvival_numalleles_density.txt")
h8_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h8_G_withoutsurvival_SAD$x)

h9_G_withoutsurvival_SAD = read.table("p5_G_SK_allreprmodes_withoutsurvival_numalleles_density.txt")
h9_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_G_withoutsurvival_SAD$x)-1, 1),
                                      density = h9_G_withoutsurvival_SAD$x)

h10_G_withoutsurvival_SAD = read.table("p5_Snack_allreprmodes_withoutsurvival_numalleles_density.txt")
h10_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_G_withoutsurvival_SAD$x)-1, 1),
                                       density = h10_G_withoutsurvival_SAD$x)

h11_G_withoutsurvival_SAD = read.table("p5_Torg_allreprmodes_withoutsurvival_numalleles_density.txt")
h11_G_withoutsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_G_withoutsurvival_SAD$x)-1, 1),
                                       density = h11_G_withoutsurvival_SAD$x)

average_G_withoutsurvival_SAD_pself0.3 = apply(cbind(h1_G_withoutsurvival_SAD$density, h2_G_withoutsurvival_SAD$density,
                                                     h3_G_withoutsurvival_SAD$density, h4_G_withoutsurvival_SAD$density,
                                                     h5_G_withoutsurvival_SAD$density,h6_G_withoutsurvival_SAD$density,
                                                     h7_G_withoutsurvival_SAD$density,h8_G_withoutsurvival_SAD$density,
                                                     h9_G_withoutsurvival_SAD$density,h10_G_withoutsurvival_SAD$density,
                                                     h11_G_withoutsurvival_SAD$density),1,sum)

df_average_G_withoutsurvival_SAD_pself0.3 = data.frame(Genetic_distance = seq(0,length(average_G_withoutsurvival_SAD_pself0.3)-1, 1), 
                                                       density = average_G_withoutsurvival_SAD_pself0.3)

ggplot(df_average_G_withoutsurvival_SAD_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))


#####
# Rozenfeld
#####

h1_G_withoutsurvival_Roz = read.table("p6_G_AF_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h1_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h1_G_withoutsurvival_Roz$x)

h2_G_withoutsurvival_Roz = read.table("p6_G_AS_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h2_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h2_G_withoutsurvival_Roz$x)

h3_G_withoutsurvival_Roz = read.table("p6_G_BB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h3_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h3_G_withoutsurvival_Roz$x)

h4_G_withoutsurvival_Roz = read.table("p6_G_BV_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h4_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h4_G_withoutsurvival_Roz$x)

h5_G_withoutsurvival_Roz = read.table("p6_G_GB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h5_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h5_G_withoutsurvival_Roz$x)

h6_G_withoutsurvival_Roz = read.table("p6_G_NB_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h6_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h6_G_withoutsurvival_Roz$x)

h7_G_withoutsurvival_Roz = read.table("p6_G_RX_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h7_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h7_G_withoutsurvival_Roz$x)

h8_G_withoutsurvival_Roz = read.table("p6_G_SG_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h8_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h8_G_withoutsurvival_Roz$x)

h9_G_withoutsurvival_Roz = read.table("p6_G_SK_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h9_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_G_withoutsurvival_Roz$x)/10)-1, 1),
                                      density = h9_G_withoutsurvival_Roz$x)

h10_G_withoutsurvival_Roz = read.table("p6_Snack_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h10_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_G_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h10_G_withoutsurvival_Roz$x)

h11_G_withoutsurvival_Roz = read.table("p6_Torg_allreprmodes_withoutsurvival_Rozenfeld_density.txt")
h11_G_withoutsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_G_withoutsurvival_Roz$x)/10)-1, 1),
                                       density = h11_G_withoutsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_G_withoutsurvival_Roz, h2_G_withoutsurvival_Roz, #List of data frames
                h3_G_withoutsurvival_Roz, h4_G_withoutsurvival_Roz,
                h5_G_withoutsurvival_Roz,h6_G_withoutsurvival_Roz,
                h7_G_withoutsurvival_Roz,h8_G_withoutsurvival_Roz,
                h9_G_withoutsurvival_Roz,h10_G_withoutsurvival_Roz,
                h11_G_withoutsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

names(df)
average_G_withoutsurvival_Roz_pself0.3 = apply(cbind(df[,2],df[,4],df[,6],
                                                     df[,8],df[,10],df[,12],df[,14],
                                                     df[,16],df[,18],df[,20],
                                                     df[,22]),1,sum, na.rm=T)

df_average_G_withoutsurvival_Roz_pself0.3 = data.frame(Genetic_distance = seq(0,(length(average_G_withoutsurvival_Roz_pself0.3)-1)/10, 0.1), 
                                                       density = average_G_withoutsurvival_Roz_pself0.3)


ggplot(df_average_G_withoutsurvival_Roz_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nRozenfeld's Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))



#################### 
# With survival (Gullmarsfjord - pself 0.3)
#################### 

#####
# SAD
#####
h1_G_withsurvival_SAD = read.table("p7_G_AF_allreprmodes_withsurvival_numalleles_density.txt")
h1_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h1_G_withsurvival_SAD$x)-1, 1),
                                   density = h1_G_withsurvival_SAD$x)

h2_G_withsurvival_SAD = read.table("p7_G_AS_allreprmodes_withsurvival_numalleles_density.txt")
h2_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h2_G_withsurvival_SAD$x)-1, 1),
                                   density = h2_G_withsurvival_SAD$x)

h3_G_withsurvival_SAD = read.table("p7_G_BB_allreprmodes_withsurvival_numalleles_density.txt")
h3_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h3_G_withsurvival_SAD$x)-1, 1),
                                   density = h3_G_withsurvival_SAD$x)

h4_G_withsurvival_SAD = read.table("p7_G_BV_allreprmodes_withsurvival_numalleles_density.txt")
h4_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h4_G_withsurvival_SAD$x)-1, 1),
                                   density = h4_G_withsurvival_SAD$x)

h5_G_withsurvival_SAD = read.table("p7_G_GB_allreprmodes_withsurvival_numalleles_density.txt")
h5_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h5_G_withsurvival_SAD$x)-1, 1),
                                   density = h5_G_withsurvival_SAD$x)

h6_G_withsurvival_SAD = read.table("p7_G_NB_allreprmodes_withsurvival_numalleles_density.txt")
h6_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h6_G_withsurvival_SAD$x)-1, 1),
                                   density = h6_G_withsurvival_SAD$x)

h7_G_withsurvival_SAD = read.table("p7_G_RX_allreprmodes_withsurvival_numalleles_density.txt")
h7_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h7_G_withsurvival_SAD$x)-1, 1),
                                   density = h7_G_withsurvival_SAD$x)

h8_G_withsurvival_SAD = read.table("p7_G_SG_allreprmodes_withsurvival_numalleles_density.txt")
h8_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h8_G_withsurvival_SAD$x)-1, 1),
                                   density = h8_G_withsurvival_SAD$x)

h9_G_withsurvival_SAD = read.table("p7_G_SK_allreprmodes_withsurvival_numalleles_density.txt")
h9_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h9_G_withsurvival_SAD$x)-1, 1),
                                   density = h9_G_withsurvival_SAD$x)

h10_G_withsurvival_SAD = read.table("p7_Snack_allreprmodes_withsurvival_numalleles_density.txt")
h10_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h10_G_withsurvival_SAD$x)-1, 1),
                                    density = h10_G_withsurvival_SAD$x)

h11_G_withsurvival_SAD = read.table("p7_Torg_allreprmodes_withsurvival_numalleles_density.txt")
h11_G_withsurvival_SAD = data.frame(Genetic_Distance = seq(0,length(h11_G_withsurvival_SAD$x)-1, 1),
                                    density = h11_G_withsurvival_SAD$x)

average_G_withsurvival_SAD_pself0.3 = apply(cbind(h1_G_withsurvival_SAD$density, h2_G_withsurvival_SAD$density,
                                                  h3_G_withsurvival_SAD$density, h4_G_withsurvival_SAD$density,
                                                  h5_G_withsurvival_SAD$density,h6_G_withsurvival_SAD$density,
                                                  h7_G_withsurvival_SAD$density,h8_G_withsurvival_SAD$density,
                                                  h9_G_withsurvival_SAD$density,h10_G_withsurvival_SAD$density,
                                                  h11_G_withsurvival_SAD$density),1,sum)

df_average_G_withsurvival_SAD_pself0.3 = data.frame(Genetic_distance = seq(0,length(average_G_withsurvival_SAD_pself0.3)-1, 1), 
                                                    density = average_G_withsurvival_SAD_pself0.3)

ggplot(df_average_G_withsurvival_SAD_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))

#####
# Rozenfeld
#####

h1_G_withsurvival_Roz = read.table("p8_G_AF_allreprmodes_withsurvival_Rozenfeld_density.txt")
h1_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h1_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h1_G_withsurvival_Roz$x)

h2_G_withsurvival_Roz = read.table("p8_G_AS_allreprmodes_withsurvival_Rozenfeld_density.txt")
h2_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h2_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h2_G_withsurvival_Roz$x)

h3_G_withsurvival_Roz = read.table("p8_G_BB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h3_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h3_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h3_G_withsurvival_Roz$x)

h4_G_withsurvival_Roz = read.table("p8_G_BV_allreprmodes_withsurvival_Rozenfeld_density.txt")
h4_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h4_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h4_G_withsurvival_Roz$x)

h5_G_withsurvival_Roz = read.table("p8_G_GB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h5_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h5_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h5_G_withsurvival_Roz$x)

h6_G_withsurvival_Roz = read.table("p8_G_NB_allreprmodes_withsurvival_Rozenfeld_density.txt")
h6_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h6_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h6_G_withsurvival_Roz$x)

h7_G_withsurvival_Roz = read.table("p8_G_RX_allreprmodes_withsurvival_Rozenfeld_density.txt")
h7_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h7_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h7_G_withsurvival_Roz$x)

h8_G_withsurvival_Roz = read.table("p8_G_SG_allreprmodes_withsurvival_Rozenfeld_density.txt")
h8_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h8_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h8_G_withsurvival_Roz$x)

h9_G_withsurvival_Roz = read.table("p8_G_SK_allreprmodes_withsurvival_Rozenfeld_density.txt")
h9_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h9_G_withsurvival_Roz$x)/10)-1, 1),
                                   density = h9_G_withsurvival_Roz$x)

h10_G_withsurvival_Roz = read.table("p8_Snack_allreprmodes_withsurvival_Rozenfeld_density.txt")
h10_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h10_G_withsurvival_Roz$x)/10)-1, 1),
                                    density = h10_G_withsurvival_Roz$x)

h11_G_withsurvival_Roz = read.table("p8_Torg_allreprmodes_withsurvival_Rozenfeld_density.txt")
h11_G_withsurvival_Roz = data.frame(Genetic_Distance = seq(0,(length(h11_G_withsurvival_Roz$x)/10)-1, 1),
                                    density = h11_G_withsurvival_Roz$x)


# Since not all dfs have the same number of rows, I need to add NAs to the ones with less number of
# rows in order to be able to cbind them:

list.df <- list(h1_G_withsurvival_Roz, h2_G_withsurvival_Roz, #List of data frames
                h3_G_withsurvival_Roz, h4_G_withsurvival_Roz,
                h5_G_withsurvival_Roz,h6_G_withsurvival_Roz,
                h7_G_withsurvival_Roz,h8_G_withsurvival_Roz,
                h9_G_withsurvival_Roz,h10_G_withsurvival_Roz,
                h11_G_withsurvival_Roz)

max.rows <- max(unlist(lapply(list.df, nrow), use.names = F))

list.df <- lapply(list.df, function(x) {
  na.count <- max.rows - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

df = do.call(cbind, list.df)

average_G_withsurvival_Roz_pself0.3 = apply(cbind(df[,2],df[,4],df[,6],
                                                  df[,8],df[,10],df[,12],df[,14],
                                                  df[,16],df[,18],df[,20],
                                                  df[,22]),1,sum, na.rm=T)

df_average_G_withsurvival_Roz_pself0.3 = data.frame(Genetic_distance = seq(0,(length(average_G_withsurvival_Roz_pself0.3)-1)/10, 0.1), 
                                                    density = average_G_withsurvival_Roz_pself0.3)


ggplot(df_average_G_withsurvival_Roz_pself0.3, aes(x = Genetic_distance, y = density)) + 
  geom_bar(stat="identity", fill="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Density\n") +
  ggtitle("") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13))













#########################################################################################################
#########################################################################################################

# Plotting the real with the simulated

#########################################################################################################
#########################################################################################################

setwd("Histogram_densities/8.Average_GDS_simulated")



################################################
# Kungälv - with survival - SAD - all pselfs
################################################

avg_GDS_real_K_SAD = read.csv("avg_GDS_Mars_SAD.csv")
avg_GDS_K_withsurvival_SAD_pself0.01 = read.csv("avg_K_withsurvival_SAD_pself0.01.csv")
avg_GDS_K_withsurvival_SAD_pself0.1 = read.csv("avg_K_withsurvival_SAD_pself0.1.csv")
avg_GDS_K_withsurvival_SAD_pself0.3 = read.csv("avg_K_withsurvival_SAD_pself0.3.csv")

jpeg(filename = "K_SAD_withsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_K_SAD, stat='identity', aes(x=genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_K_withsurvival_SAD_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.01")) +
  geom_line(data=avg_GDS_K_withsurvival_SAD_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.1")) + 
  geom_line(data=avg_GDS_K_withsurvival_SAD_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.3")) +
  scale_fill_manual(values="cyan3") +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Shared Allele Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Kungälv Shared Alleles' Distance avg real & avg simulated with survival") +
    theme(panel.background = element_rect(fill = 'white', colour='black'), 
          axis.text=element_text(size=12),
          axis.title=element_text(size=13),
          plot.title=element_text(size=15),
          legend.title=element_blank(), 
          legend.position="none")


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()



################################################
# Kungälv - without survival - SAD - all pselfs
################################################

avg_GDS_real_K_SAD = read.csv("avg_GDS_Mars_SAD.csv")
avg_GDS_K_withoutsurvival_SAD_pself0.01 = read.csv("avg_K_withoutsurvival_SAD_pself0.01.csv")
avg_GDS_K_withoutsurvival_SAD_pself0.1 = read.csv("avg_K_withoutsurvival_SAD_pself0.1.csv")
avg_GDS_K_withoutsurvival_SAD_pself0.3 = read.csv("avg_K_withoutsurvival_SAD_pself0.3.csv")

jpeg(filename = "K_SAD_withoutsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_K_SAD, stat='identity', aes(x=genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_K_withoutsurvival_SAD_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.01")) +
  geom_line(data=avg_GDS_K_withoutsurvival_SAD_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.1")) + 
  geom_line(data=avg_GDS_K_withoutsurvival_SAD_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.3")) +
  scale_fill_manual(values="cyan3") +
  #scale_color_manual(values=c("blue", "darkgrey", "orange")) +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Shared Allele Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Kungälv Shared Alleles' Distance avg real & avg simulated without survival") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15),
        legend.title=element_blank(), legend.position="none")

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()



################################################
# Kungälv - with survival - Rozenfeld - all pselfs
################################################

avg_GDS_real_K_Roz = read.csv("avg_GDS_Mars_Rozenfeld.csv")
avg_GDS_K_withsurvival_Roz_pself0.01 = read.csv("avg_K_withsurvival_Roz_pself0.01.csv")
avg_GDS_K_withsurvival_Roz_pself0.1 = read.csv("avg_K_withsurvival_Roz_pself0.1.csv")
avg_GDS_K_withsurvival_Roz_pself0.3 = read.csv("avg_K_withsurvival_Roz_pself0.3.csv")


jpeg(filename = "K_Roz_withsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_K_Roz, stat='identity', aes(x=genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_K_withsurvival_Roz_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.01")) +
  geom_line(data=avg_GDS_K_withsurvival_Roz_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.1")) + 
  geom_line(data=avg_GDS_K_withsurvival_Roz_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.3")) +
  scale_fill_manual(values="cyan3") +
  #scale_color_manual(values=c("blue", "darkgrey", "orange")) +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Rozenfeld's Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Kungälv Rozenfeld's Distance avg real & avg simulated with survival") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15),
        legend.title=element_blank(), legend.position="none")


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


################################################
# Kungälv - without survival - Rozenfeld - all pselfs
################################################

avg_GDS_real_K_Roz = read.csv("avg_GDS_Mars_Rozenfeld.csv")
avg_GDS_K_withoutsurvival_Roz_pself0.01 = read.csv("avg_K_withoutsurvival_Roz_pself0.01.csv")
avg_GDS_K_withoutsurvival_Roz_pself0.1 = read.csv("avg_K_withoutsurvival_Roz_pself0.1.csv")
avg_GDS_K_withoutsurvival_Roz_pself0.3 = read.csv("avg_K_withoutsurvival_Roz_pself0.3.csv")

jpeg(filename = "K_Roz_withoutsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_K_Roz, stat='identity', aes(x=genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_K_withoutsurvival_Roz_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.01")) +
  geom_line(data=avg_GDS_K_withoutsurvival_Roz_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.1")) + 
  geom_line(data=avg_GDS_K_withoutsurvival_Roz_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.3")) +
  scale_fill_manual(values="cyan3") +
  #scale_color_manual(values=c("blue", "darkgrey", "orange")) +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Rozenfeld's Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Kungälv Rozenfeld's Distance avg real & avg simulated without survival") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15),
        legend.title=element_blank(), legend.position="none")


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


################################################
# Gullmarsfjord - with survival - SAD - all pselfs
################################################

avg_GDS_real_G_SAD = read.csv("avg_GDS_Gullm_SAD.csv")
avg_GDS_G_withsurvival_SAD_pself0.01 = read.csv("avg_G_withsurvival_SAD_pself0.01.csv")
avg_GDS_G_withsurvival_SAD_pself0.1 = read.csv("avg_G_withsurvival_SAD_pself0.1.csv")
avg_GDS_G_withsurvival_SAD_pself0.3 = read.csv("avg_G_withsurvival_SAD_pself0.3.csv")

jpeg(filename = "G_SAD_withsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_G_SAD, stat='identity', aes(x=Genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_G_withsurvival_SAD_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.01")) +
  geom_line(data=avg_GDS_G_withsurvival_SAD_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.1")) + 
  geom_line(data=avg_GDS_G_withsurvival_SAD_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.3")) +
  scale_fill_manual(values="coral2") +
  #scale_color_manual(values=c("blue", "darkgrey", "green")) +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Shared Allele Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Gullmarsfjord Shared Alleles' Distance avg real & avg simulated with survival") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15),
        legend.title=element_blank(), legend.position="none")


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


################################################
# Gullmarsfjord - without survival - SAD - all pselfs
################################################

avg_GDS_real_G_SAD = read.csv("avg_GDS_Gullm_SADs.csv")
avg_GDS_G_withoutsurvival_SAD_pself0.01 = read.csv("avg_G_withoutsurvival_SAD_pself0.01.csv")
avg_GDS_G_withoutsurvival_SAD_pself0.1 = read.csv("avg_G_withoutsurvival_SAD_pself0.1.csv")
avg_GDS_G_withoutsurvival_SAD_pself0.3 = read.csv("avg_G_withoutsurvival_SAD_pself0.3.csv")

jpeg(filename = "G_SAD_withoutsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_G_SAD, stat='identity', aes(x=Genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_G_withoutsurvival_SAD_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.01")) +
  geom_line(data=avg_GDS_G_withoutsurvival_SAD_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.1")) + 
  geom_line(data=avg_GDS_G_withoutsurvival_SAD_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.3")) +
  scale_fill_manual(values="coral2") +
  #scale_color_manual(values=c("blue", "darkgrey", "green")) +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Shared Allele Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Gullmarsfjord Shared Alleles' Distance avg real & avg simulated without survival") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15),
        legend.title=element_blank(), legend.position="none")


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()



################################################
# Gullmarsfjord - with survival - Rozenfeld - all pselfs
################################################

avg_GDS_real_G_Roz = read.csv("avg_GDS_Gullm_Rozenfeld.csv")
avg_GDS_G_withsurvival_Roz_pself0.01 = read.csv("avg_G_withsurvival_Roz_pself0.01.csv")
avg_GDS_G_withsurvival_Roz_pself0.1 = read.csv("avg_G_withsurvival_Roz_pself0.1.csv")
avg_GDS_G_withsurvival_Roz_pself0.3 = read.csv("avg_G_withsurvival_Roz_pself0.3.csv")


jpeg(filename = "G_Roz_withsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_G_Roz, stat='identity', aes(x=Genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_G_withsurvival_Roz_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.01")) +
  geom_line(data=avg_GDS_G_withsurvival_Roz_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.1")) + 
  geom_line(data=avg_GDS_G_withsurvival_Roz_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated with survival pself 0.3")) +
  scale_fill_manual(values="coral2") +
  #scale_color_manual(values=c("blue", "darkgrey", "green")) +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Rozenfeld's Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Gullmarsfjord Rozenfeld's Distance avg real & avg simulated with survival") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15),
        legend.title=element_blank(), legend.position="none")


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


################################################
# Gullmarsfjord - without survival - Rozenfeld - all pselfs
################################################

avg_GDS_real_G_Roz = read.csv("avg_GDS_Gullm_Rozenfeld.csv")
avg_GDS_G_withoutsurvival_Roz_pself0.01 = read.csv("avg_G_withoutsurvival_Roz_pself0.01.csv")
avg_GDS_G_withoutsurvival_Roz_pself0.1 = read.csv("avg_G_withoutsurvival_Roz_pself0.1.csv")
avg_GDS_G_withoutsurvival_Roz_pself0.3 = read.csv("avg_G_withoutsurvival_Roz_pself0.3.csv")

jpeg(filename = "G_Roz_withoutsurvival_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", quality=75, res=600)

ggplot() + 
  geom_bar(data=avg_GDS_real_G_Roz, stat='identity', aes(x=Genetic_distance, y=Frequency, fill="Real data")) +
  geom_line(data=avg_GDS_G_withoutsurvival_Roz_pself0.01,  size = 1,aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.01")) +
  geom_line(data=avg_GDS_G_withoutsurvival_Roz_pself0.1, size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.1")) + 
  geom_line(data=avg_GDS_G_withoutsurvival_Roz_pself0.3 , size=1, aes(x=Genetic_distance, y=density, color="Simulated without survival pself 0.3")) +
  scale_fill_manual(values="coral2") +
  #scale_color_manual(values=c("blue", "darkgrey", "green")) +
  scale_color_manual(values=c("gray48", "blue", "gray8")) +
  labs(x="Rozenfeld's Distance", ylab="Density", fill="", colour="") +
  #ggtitle("Gullmarsfjord Rozenfeld's Distance avg real & avg simulated without survival") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15),
        legend.title=element_blank(), legend.position="none")


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()

