# Author: Núria Serra Serra
# Date: July 2017

########################################################################################################

### PLOTTING FIG S.11

########################################################################################################


# In the script "calculation_pairwise_distances.R" I have used marmap and fossil to calculate pairwise geographical distances
# between sampling sites of the G and K fjords.

# Now, to do the GDS splitting them by geographical distance we need to calculate the genetic distance 
# matrix for all pairwise comparisons between sites and add them, to make the histogram.

##### NOTE:  we only want the genetic distances from individuals of one population to the other, but not
##### between individuals of the same population ---> SOLUTION: do an asymetric matrix with individuals
##### from one population as columns and individuals from the other population as rows, and do it full
##### (not half/triangle, but the whole matrix as genetic distances)

##### NOTE 2: we do it for MLGs and without clones.
##### It's good to have clones when doing it for the smallest distance (<5km), because peak at 0.
##### However, the clones change the shape of the GDS, not being normal.
##### Use the same script, and after the splitting of the dataframes change "MLGs" to "withclones" or the other way around



remove(list=ls())

library(RClone)
library(ggplot2)
#library(gstudio)

all_real_MLGs = read.csv("all_real_MLGs_noD_noZEN_Snack&Torg_together_noG4.csv", sep=";")
summary(all_real_MLGs$population)

all_real_withclones = read.csv("all_real_withclones_noD_noZEN_Snack&Torg_together_noG4.csv", sep=";")
summary(all_real_withclones$population)


###################################
### Creating G/K dataframes
###################################

G_real_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-AF", "G-AS", 
                                                            "G-BB", "G-BV", "G-GB", 
                                                            "G-NB", "G-RX", "G-SG", "G-SK", 
                                                            "Snackebackebukten", 
                                                            "Torgestad"),]

K_real_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-BK", "K-K", "K-KR", "K-LD", 
                                                            "K-NG", "K-NI", "K-OK", "K-ON", 
                                                            "K-RT", "K-SK", "K-SO"),]

G_real_withclones = all_real_withclones[all_real_withclones$population %in% c("G-AF", "G-AS", 
                                                                              "G-BB", "G-BV", "G-GB", 
                                                                              "G-NB", "G-RX", "G-SG", "G-SK", 
                                                                              "Snackebackebukten", 
                                                                              "Torgestad"),]

K_real_withclones = all_real_withclones[all_real_withclones$population %in% c("K-BK", "K-K", "K-KR", "K-LD", 
                                                                              "K-NG", "K-NI", "K-OK", "K-ON", 
                                                                              "K-RT", "K-SK", "K-SO"),]



###################################
### Creating dataframes per population
###################################

G_AF_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-AF"),]
G_AS_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-AS"),]
G_BB_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-BB"),]
G_BV_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-BV"),]
G_GB_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-GB"),]
G_NB_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-NB"),]
G_RX_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-RX"),]
G_SG_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-SG"),]
G_SK_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("G-SK"),]
Snack_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("Snackebackebukten"),]
Torg_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("Torgestad"),]

K_BK_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-BK"),]
K_K_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-K"),]
K_KR_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-KR"),]
K_LD_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-LD"),]
K_NG_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-NG"),]
K_NI_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-NI"),]
K_OK_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-OK"),]
K_ON_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-ON"),]
K_RT_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-RT"),]
K_SK_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-SK"),]
K_SO_MLGs = all_real_MLGs[all_real_MLGs$population %in% c("K-SO"),]

G_AF_withclones = all_real_withclones[all_real_withclones$population %in% c("G-AF"),]
G_AS_withclones = all_real_withclones[all_real_withclones$population %in% c("G-AS"),]
G_BB_withclones = all_real_withclones[all_real_withclones$population %in% c("G-BB"),]
G_BV_withclones = all_real_withclones[all_real_withclones$population %in% c("G-BV"),]
G_GB_withclones = all_real_withclones[all_real_withclones$population %in% c("G-GB"),]
G_NB_withclones = all_real_withclones[all_real_withclones$population %in% c("G-NB"),]
G_RX_withclones = all_real_withclones[all_real_withclones$population %in% c("G-RX"),]
G_SG_withclones = all_real_withclones[all_real_withclones$population %in% c("G-SG"),]
G_SK_withclones = all_real_withclones[all_real_withclones$population %in% c("G-SK"),]
Snack_withclones = all_real_withclones[all_real_withclones$population %in% c("Snackebackebukten"),]
Torg_withclones = all_real_withclones[all_real_withclones$population %in% c("Torgestad"),]

K_BK_withclones = all_real_withclones[all_real_withclones$population %in% c("K-BK"),]
K_K_withclones = all_real_withclones[all_real_withclones$population %in% c("K-K"),]
K_KR_withclones = all_real_withclones[all_real_withclones$population %in% c("K-KR"),]
K_LD_withclones = all_real_withclones[all_real_withclones$population %in% c("K-LD"),]
K_NG_withclones = all_real_withclones[all_real_withclones$population %in% c("K-NG"),]
K_NI_withclones = all_real_withclones[all_real_withclones$population %in% c("K-NI"),]
K_OK_withclones = all_real_withclones[all_real_withclones$population %in% c("K-OK"),]
K_ON_withclones = all_real_withclones[all_real_withclones$population %in% c("K-ON"),]
K_RT_withclones = all_real_withclones[all_real_withclones$population %in% c("K-RT"),]
K_SK_withclones = all_real_withclones[all_real_withclones$population %in% c("K-SK"),]
K_SO_withclones = all_real_withclones[all_real_withclones$population %in% c("K-SO"),]

# Drop levels:
G_real_MLGs$population = droplevels(G_real_MLGs$population)
G_real_MLGs$SampleNames = droplevels(G_real_MLGs$SampleNames)
K_real_MLGs$population = droplevels(K_real_MLGs$population)
K_real_MLGs$SampleNames = droplevels(K_real_MLGs$SampleNames)
G_real_withclones$population = droplevels(G_real_withclones$population)
G_real_withclones$SampleNames = droplevels(G_real_withclones$SampleNames)
K_real_withclones$population = droplevels(K_real_withclones$population)
K_real_withclones$SampleNames = droplevels(K_real_withclones$SampleNames)

G_AF_MLGs$population = droplevels(G_AF_MLGs$population)
G_AS_MLGs$population = droplevels(G_AS_MLGs$population)
G_BB_MLGs$population = droplevels(G_BB_MLGs$population)
G_BV_MLGs$population = droplevels(G_BV_MLGs$population)
G_GB_MLGs$population = droplevels(G_GB_MLGs$population)
G_NB_MLGs$population = droplevels(G_NB_MLGs$population)
G_RX_MLGs$population = droplevels(G_RX_MLGs$population)
G_SG_MLGs$population = droplevels(G_SG_MLGs$population)
G_SK_MLGs$population = droplevels(G_SK_MLGs$population)
Snack_MLGs$population = droplevels(Snack_MLGs$population)
Torg_MLGs$population = droplevels(Torg_MLGs$population)

K_BK_MLGs$population = droplevels(K_BK_MLGs$population)
K_K_MLGs$population = droplevels(K_K_MLGs$population)
K_KR_MLGs$population = droplevels(K_KR_MLGs$population)
K_LD_MLGs$population = droplevels(K_LD_MLGs$population)
K_NG_MLGs$population = droplevels(K_NG_MLGs$population)
K_NI_MLGs$population = droplevels(K_NI_MLGs$population)
K_OK_MLGs$population = droplevels(K_OK_MLGs$population)
K_ON_MLGs$population = droplevels(K_ON_MLGs$population)
K_RT_MLGs$population = droplevels(K_RT_MLGs$population)
K_SK_MLGs$population = droplevels(K_SK_MLGs$population)
K_SO_MLGs$population = droplevels(K_SO_MLGs$population)

G_AF_withclones$population = droplevels(G_AF_withclones$population)
G_AS_withclones$population = droplevels(G_AS_withclones$population)
G_BB_withclones$population = droplevels(G_BB_withclones$population)
G_BV_withclones$population = droplevels(G_BV_withclones$population)
G_GB_withclones$population = droplevels(G_GB_withclones$population)
G_NB_withclones$population = droplevels(G_NB_withclones$population)
G_RX_withclones$population = droplevels(G_RX_withclones$population)
G_SG_withclones$population = droplevels(G_SG_withclones$population)
G_SK_withclones$population = droplevels(G_SK_withclones$population)
Snack_withclones$population = droplevels(Snack_withclones$population)
Torg_withclones$population = droplevels(Torg_withclones$population)

K_BK_withclones$population = droplevels(K_BK_withclones$population)
K_K_withclones$population = droplevels(K_K_withclones$population)
K_KR_withclones$population = droplevels(K_KR_withclones$population)
K_LD_withclones$population = droplevels(K_LD_withclones$population)
K_NG_withclones$population = droplevels(K_NG_withclones$population)
K_NI_withclones$population = droplevels(K_NI_withclones$population)
K_OK_withclones$population = droplevels(K_OK_withclones$population)
K_ON_withclones$population = droplevels(K_ON_withclones$population)
K_RT_withclones$population = droplevels(K_RT_withclones$population)
K_SK_withclones$population = droplevels(K_SK_withclones$population)
K_SO_withclones$population = droplevels(K_SO_withclones$population)


# Function to convert into the right format for RClone:
convert_to_RClone = function(df){
  df_popvect = df$population
  df_SampleNamesvect = df$SampleNames
  df = df[,-1]
  df = df[,-1]
  df = sort_all(df)
  
  df
  
}

input_G_real_MLGs = convert_to_RClone(G_real_MLGs)
input_K_real_MLGs = convert_to_RClone(K_real_MLGs)

input_G_real_withclones = convert_to_RClone(G_real_withclones)
input_K_real_withclones = convert_to_RClone(K_real_withclones)

input_G_AF_MLGs = convert_to_RClone(G_AF_MLGs)
input_G_AS_MLGs = convert_to_RClone(G_AS_MLGs)
input_G_BB_MLGs = convert_to_RClone(G_BB_MLGs)
input_G_BV_MLGs = convert_to_RClone(G_BV_MLGs)
input_G_GB_MLGs = convert_to_RClone(G_GB_MLGs)
input_G_NB_MLGs = convert_to_RClone(G_NB_MLGs)
input_G_RX_MLGs = convert_to_RClone(G_RX_MLGs)
input_G_SG_MLGs = convert_to_RClone(G_SG_MLGs)
input_G_SK_MLGs = convert_to_RClone(G_SK_MLGs)
input_Snack_MLGs = convert_to_RClone(Snack_MLGs)
input_Torg_MLGs = convert_to_RClone(Torg_MLGs)

input_K_BK_MLGs = convert_to_RClone(K_BK_MLGs)
input_K_K_MLGs = convert_to_RClone(K_K_MLGs)
input_K_KR_MLGs = convert_to_RClone(K_KR_MLGs)
input_K_LD_MLGs = convert_to_RClone(K_LD_MLGs)
input_K_NG_MLGs = convert_to_RClone(K_NG_MLGs)
input_K_NI_MLGs = convert_to_RClone(K_NI_MLGs)
input_K_OK_MLGs = convert_to_RClone(K_OK_MLGs)
input_K_ON_MLGs = convert_to_RClone(K_ON_MLGs)
input_K_RT_MLGs = convert_to_RClone(K_RT_MLGs)
input_K_SK_MLGs = convert_to_RClone(K_SK_MLGs)
input_K_SO_MLGs = convert_to_RClone(K_SO_MLGs)

input_G_AF_withclones = convert_to_RClone(G_AF_withclones)
input_G_AS_withclones = convert_to_RClone(G_AS_withclones)
input_G_BB_withclones = convert_to_RClone(G_BB_withclones)
input_G_BV_withclones = convert_to_RClone(G_BV_withclones)
input_G_GB_withclones = convert_to_RClone(G_GB_withclones)
input_G_NB_withclones = convert_to_RClone(G_NB_withclones)
input_G_RX_withclones = convert_to_RClone(G_RX_withclones)
input_G_SG_withclones = convert_to_RClone(G_SG_withclones)
input_G_SK_withclones = convert_to_RClone(G_SK_withclones)
input_Snack_withclones = convert_to_RClone(Snack_withclones)
input_Torg_withclones = convert_to_RClone(Torg_withclones)

input_K_BK_withclones = convert_to_RClone(K_BK_withclones)
input_K_K_withclones = convert_to_RClone(K_K_withclones)
input_K_KR_withclones = convert_to_RClone(K_KR_withclones)
input_K_LD_withclones = convert_to_RClone(K_LD_withclones)
input_K_NG_withclones = convert_to_RClone(K_NG_withclones)
input_K_NI_withclones = convert_to_RClone(K_NI_withclones)
input_K_OK_withclones = convert_to_RClone(K_OK_withclones)
input_K_ON_withclones = convert_to_RClone(K_ON_withclones)
input_K_RT_withclones = convert_to_RClone(K_RT_withclones)
input_K_SK_withclones = convert_to_RClone(K_SK_withclones)
input_K_SO_withclones = convert_to_RClone(K_SO_withclones)




#################################################################################################################################################################

##### K populations (relying on the separation by lc.dist):

#################################################################################################################################################################


###########################
# <5km apart (from 0 to 4)
###########################

# Only each population with itself:

K_less_than_5km_1_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_NG_MLGs)
K_less_than_5km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_NG_MLGs)

K_less_than_5km_2_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_K_MLGs)
K_less_than_5km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_K_MLGs)

K_less_than_5km_3_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_SK_MLGs)
K_less_than_5km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_SK_MLGs)

K_less_than_5km_4_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_BK_MLGs)
K_less_than_5km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_BK_MLGs)

K_less_than_5km_5_numalleles = asym_genet_dist_main_numalleles_new(input_K_LD_MLGs, input_K_LD_MLGs)
K_less_than_5km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_LD_MLGs, input_K_LD_MLGs)

K_less_than_5km_6_numalleles = asym_genet_dist_main_numalleles_new(input_K_NI_MLGs, input_K_NI_MLGs)
K_less_than_5km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NI_MLGs, input_K_NI_MLGs)

K_less_than_5km_7_numalleles = asym_genet_dist_main_numalleles_new(input_K_ON_MLGs, input_K_ON_MLGs)
K_less_than_5km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_ON_MLGs, input_K_ON_MLGs)

K_less_than_5km_8_numalleles = asym_genet_dist_main_numalleles_new(input_K_KR_MLGs, input_K_KR_MLGs)
K_less_than_5km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_KR_MLGs, input_K_KR_MLGs)

K_less_than_5km_9_numalleles = asym_genet_dist_main_numalleles_new(input_K_SO_MLGs, input_K_SO_MLGs)
K_less_than_5km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SO_MLGs, input_K_SO_MLGs)

K_less_than_5km_10_numalleles = asym_genet_dist_main_numalleles_new(input_K_OK_MLGs, input_K_OK_MLGs)
K_less_than_5km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_OK_MLGs, input_K_OK_MLGs)

K_less_than_5km_11_numalleles = asym_genet_dist_main_numalleles_new(input_K_RT_MLGs, input_K_RT_MLGs)
K_less_than_5km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_RT_MLGs, input_K_RT_MLGs)

p1_K_less_than_5km_1_numalleles = hist(K_less_than_5km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_K_less_than_5km_1_numalleles")
p2_K_less_than_5km_2_numalleles = hist(K_less_than_5km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_K_less_than_5km_2_numalleles")
p3_K_less_than_5km_3_numalleles = hist(K_less_than_5km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_K_less_than_5km_3_numalleles")
p4_K_less_than_5km_4_numalleles = hist(K_less_than_5km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_K_less_than_5km_4_numalleles")
p5_K_less_than_5km_5_numalleles = hist(K_less_than_5km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_K_less_than_5km_5_numalleles")
p6_K_less_than_5km_6_numalleles = hist(K_less_than_5km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_K_less_than_5km_6_numalleles")
p7_K_less_than_5km_7_numalleles = hist(K_less_than_5km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_K_less_than_5km_7_numalleles")
p8_K_less_than_5km_8_numalleles = hist(K_less_than_5km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_K_less_than_5km_8_numalleles")
p9_K_less_than_5km_9_numalleles = hist(K_less_than_5km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_less_than_5km_9_numalleles")
p10_K_less_than_5km_10_numalleles = hist(K_less_than_5km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p10_K_less_than_5km_10_numalleles")
p11_K_less_than_5km_11_numalleles = hist(K_less_than_5km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_less_than_5km_11_numalleles")

p1_K_less_than_5km_1_Rozenfeld = hist(K_less_than_5km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_K_less_than_5km_1_Rozenfeld")
p2_K_less_than_5km_2_Rozenfeld = hist(K_less_than_5km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_K_less_than_5km_2_Rozenfeld")
p3_K_less_than_5km_3_Rozenfeld = hist(K_less_than_5km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_K_less_than_5km_3_Rozenfeld")
p4_K_less_than_5km_4_Rozenfeld = hist(K_less_than_5km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_K_less_than_5km_4_Rozenfeld")
p5_K_less_than_5km_5_Rozenfeld = hist(K_less_than_5km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_K_less_than_5km_5_Rozenfeld")
p6_K_less_than_5km_6_Rozenfeld = hist(K_less_than_5km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_K_less_than_5km_6_Rozenfeld")
p7_K_less_than_5km_7_Rozenfeld = hist(K_less_than_5km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_K_less_than_5km_7_Rozenfeld")
p8_K_less_than_5km_8_Rozenfeld = hist(K_less_than_5km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_K_less_than_5km_8_Rozenfeld")
p9_K_less_than_5km_9_Rozenfeld = hist(K_less_than_5km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_less_than_5km_9_Rozenfeld")
p10_K_less_than_5km_10_Rozenfeld = hist(K_less_than_5km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p10_K_less_than_5km_10_Rozenfeld")
p11_K_less_than_5km_11_Rozenfeld = hist(K_less_than_5km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p11_K_less_than_5km_11_Rozenfeld")

# Plot numalleles

a = p1_K_less_than_5km_1_numalleles$counts + p2_K_less_than_5km_2_numalleles$counts + 
  p3_K_less_than_5km_3_numalleles$counts + p4_K_less_than_5km_4_numalleles$counts + 
  p5_K_less_than_5km_5_numalleles$counts + p6_K_less_than_5km_6_numalleles$counts + 
  p7_K_less_than_5km_7_numalleles$counts + p8_K_less_than_5km_8_numalleles$counts + 
  p9_K_less_than_5km_9_numalleles$counts + p10_K_less_than_5km_10_numalleles$counts +
  p11_K_less_than_5km_11_numalleles$counts

b = seq(0,34,1)

K_less_than_5km_numalleles = data.frame(counts=a,genet_dist=b)

#ggplot(K_less_than_5km_numalleles) + 
  geom_bar(data=K_less_than_5km_numalleles, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K <5km numalleles")

# Plot Rozenfeld

a = p1_K_less_than_5km_1_Rozenfeld$counts + p2_K_less_than_5km_2_Rozenfeld$counts + 
  p3_K_less_than_5km_3_Rozenfeld$counts + p4_K_less_than_5km_4_Rozenfeld$counts + 
  p5_K_less_than_5km_5_Rozenfeld$counts + p6_K_less_than_5km_6_Rozenfeld$counts + 
  p7_K_less_than_5km_7_Rozenfeld$counts + p8_K_less_than_5km_8_Rozenfeld$counts + 
  p9_K_less_than_5km_9_Rozenfeld$counts + p10_K_less_than_5km_10_Rozenfeld$counts +
  p11_K_less_than_5km_11_Rozenfeld$counts

b = seq(0,9.9,0.1)

K_less_than_5km_Rozenfeld = data.frame(counts=a,genet_dist=b)

#ggplot(K_less_than_5km_Rozenfeld) + 
  geom_bar(data=K_less_than_5km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K <5km Rozenfeld")


###########################
# 5-19km apart
###########################

K_5to9km_1_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_K_MLGs)
K_5to9km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_K_MLGs)

K_5to9km_2_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_SK_MLGs)
K_5to9km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_SK_MLGs)

K_5to9km_3_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_BK_MLGs)
K_5to9km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_BK_MLGs)

K_5to9km_4_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_OK_MLGs)
K_5to9km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_OK_MLGs)

K_5to9km_5_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_BK_MLGs)
K_5to9km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_BK_MLGs)

K_5to9km_6_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_OK_MLGs)
K_5to9km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_OK_MLGs)

K_5to9km_7_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_LD_MLGs)
K_5to9km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_LD_MLGs)

K_5to9km_8_numalleles = asym_genet_dist_main_numalleles_new(input_K_LD_MLGs, input_K_NI_MLGs)
K_5to9km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_LD_MLGs, input_K_NI_MLGs)

K_5to9km_9_numalleles = asym_genet_dist_main_numalleles_new(input_K_LD_MLGs, input_K_ON_MLGs)
K_5to9km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_LD_MLGs, input_K_ON_MLGs)

K_5to9km_10_numalleles = asym_genet_dist_main_numalleles_new(input_K_NI_MLGs, input_K_ON_MLGs)
K_5to9km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NI_MLGs, input_K_ON_MLGs)

K_5to9km_11_numalleles = asym_genet_dist_main_numalleles_new(input_K_ON_MLGs, input_K_KR_MLGs)
K_5to9km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_ON_MLGs, input_K_KR_MLGs)

p1_K_5to9km_1_numalleles = hist(K_5to9km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_K_5to9km_1_numalleles")
p2_K_5to9km_2_numalleles = hist(K_5to9km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_K_5to9km_2_numalleles")
p3_K_5to9km_3_numalleles = hist(K_5to9km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_K_5to9km_3_numalleles")
p4_K_5to9km_4_numalleles = hist(K_5to9km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_K_5to9km_4_numalleles")
p5_K_5to9km_5_numalleles = hist(K_5to9km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_K_5to9km_5_numalleles")
p6_K_5to9km_6_numalleles = hist(K_5to9km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_K_5to9km_6_numalleles")
p7_K_5to9km_7_numalleles = hist(K_5to9km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_K_5to9km_7_numalleles")
p8_K_5to9km_8_numalleles = hist(K_5to9km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_K_5to9km_8_numalleles")
p9_K_5to9km_9_numalleles = hist(K_5to9km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_5to9km_9_numalleles")
p10_K_5to9km_10_numalleles = hist(K_5to9km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p10_K_5to9km_10_numalleles")
p11_K_5to9km_11_numalleles = hist(K_5to9km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_5to9km_11_numalleles")

p1_K_5to9km_1_Rozenfeld = hist(K_5to9km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_K_5to9km_1_Rozenfeld")
p2_K_5to9km_2_Rozenfeld = hist(K_5to9km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_K_5to9km_2_Rozenfeld")
p3_K_5to9km_3_Rozenfeld = hist(K_5to9km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_K_5to9km_3_Rozenfeld")
p4_K_5to9km_4_Rozenfeld = hist(K_5to9km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_K_5to9km_4_Rozenfeld")
p5_K_5to9km_5_Rozenfeld = hist(K_5to9km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_K_5to9km_5_Rozenfeld")
p6_K_5to9km_6_Rozenfeld = hist(K_5to9km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_K_5to9km_6_Rozenfeld")
p7_K_5to9km_7_Rozenfeld = hist(K_5to9km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_K_5to9km_7_Rozenfeld")
p8_K_5to9km_8_Rozenfeld = hist(K_5to9km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_K_5to9km_8_Rozenfeld")
p9_K_5to9km_9_Rozenfeld = hist(K_5to9km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_5to9km_9_Rozenfeld")
p10_K_5to9km_10_Rozenfeld = hist(K_5to9km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p10_K_5to9km_10_Rozenfeld")
p11_K_5to9km_11_Rozenfeld = hist(K_5to9km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p11_K_5to9km_11_Rozenfeld")

# Plot numalleles

a = p1_K_5to9km_1_numalleles$counts + p2_K_5to9km_2_numalleles$counts + 
  p3_K_5to9km_3_numalleles$counts + p4_K_5to9km_4_numalleles$counts + 
  p5_K_5to9km_5_numalleles$counts + p6_K_5to9km_6_numalleles$counts + 
  p7_K_5to9km_7_numalleles$counts + p8_K_5to9km_8_numalleles$counts + 
  p9_K_5to9km_9_numalleles$counts + p10_K_5to9km_10_numalleles$counts +
  p11_K_5to9km_11_numalleles$counts

b = seq(0,34,1)

K_from5to9km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(K_from5to9km_numalleles ) + 
  geom_bar(data=K_from5to9km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 5-9km numalleles")

# Plot Rozenfeld

a = p1_K_5to9km_1_Rozenfeld$counts + p2_K_5to9km_2_Rozenfeld$counts + 
  p3_K_5to9km_3_Rozenfeld$counts + p4_K_5to9km_4_Rozenfeld$counts + 
  p5_K_5to9km_5_Rozenfeld$counts + p6_K_5to9km_6_Rozenfeld$counts + 
  p7_K_5to9km_7_Rozenfeld$counts + p8_K_5to9km_8_Rozenfeld$counts + 
  p9_K_5to9km_9_Rozenfeld$counts + p10_K_5to9km_10_Rozenfeld$counts +
  p11_K_5to9km_11_Rozenfeld$counts

b = seq(0,9.9,0.1)

K_from5to9km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(K_from5to9km_Rozenfeld) + 
  geom_bar(data=K_from5to9km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 5-9km Rozenfeld")


###########################
# 10-14km apart
###########################
K_10to14km_1_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_LD_MLGs)
K_10to14km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_LD_MLGs)

K_10to14km_2_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_SK_MLGs)
K_10to14km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_SK_MLGs)

K_10to14km_3_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_BK_MLGs)
K_10to14km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_BK_MLGs)

K_10to14km_4_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_OK_MLGs)
K_10to14km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_OK_MLGs)

K_10to14km_5_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_LD_MLGs)
K_10to14km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_LD_MLGs)

K_10to14km_6_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_NI_MLGs)
K_10to14km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_NI_MLGs)

K_10to14km_7_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_ON_MLGs)
K_10to14km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_ON_MLGs)

K_10to14km_8_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_OK_MLGs)
K_10to14km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_OK_MLGs)

K_10to14km_9_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_RT_MLGs)
K_10to14km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_RT_MLGs)

K_10to14km_10_numalleles = asym_genet_dist_main_numalleles_new(input_K_LD_MLGs, input_K_KR_MLGs)
K_10to14km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_LD_MLGs, input_K_KR_MLGs)

K_10to14km_11_numalleles = asym_genet_dist_main_numalleles_new(input_K_LD_MLGs, input_K_OK_MLGs)
K_10to14km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_LD_MLGs, input_K_OK_MLGs)

K_10to14km_12_numalleles = asym_genet_dist_main_numalleles_new(input_K_NI_MLGs, input_K_KR_MLGs)
K_10to14km_12_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NI_MLGs, input_K_KR_MLGs)

K_10to14km_13_numalleles = asym_genet_dist_main_numalleles_new(input_K_KR_MLGs, input_K_SO_MLGs)
K_10to14km_13_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_KR_MLGs, input_K_SO_MLGs)

p1_K_10to14km_1_numalleles = hist(K_10to14km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_K_10to14km_1_numalleles")
p2_K_10to14km_2_numalleles = hist(K_10to14km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_K_10to14km_2_numalleles")
p3_K_10to14km_3_numalleles = hist(K_10to14km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_K_10to14km_3_numalleles")
p4_K_10to14km_4_numalleles = hist(K_10to14km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_K_10to14km_4_numalleles")
p5_K_10to14km_5_numalleles = hist(K_10to14km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_K_10to14km_5_numalleles")
p6_K_10to14km_6_numalleles = hist(K_10to14km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_K_10to14km_6_numalleles")
p7_K_10to14km_7_numalleles = hist(K_10to14km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_K_10to14km_7_numalleles")
p8_K_10to14km_8_numalleles = hist(K_10to14km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_K_10to14km_8_numalleles")
p9_K_10to14km_9_numalleles = hist(K_10to14km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_10to14km_9_numalleles")
p10_K_10to14km_10_numalleles = hist(K_10to14km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p10_K_10to14km_10_numalleles")
p11_K_10to14km_11_numalleles = hist(K_10to14km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_10to14km_11_numalleles")
p12_K_10to14km_12_numalleles = hist(K_10to14km_12_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_10to14km_12_numalleles")
p13_K_10to14km_13_numalleles = hist(K_10to14km_13_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_10to14km_13_numalleles")

p1_K_10to14km_1_Rozenfeld = hist(K_10to14km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_K_10to14km_1_Rozenfeld")
p2_K_10to14km_2_Rozenfeld = hist(K_10to14km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_K_10to14km_2_Rozenfeld")
p3_K_10to14km_3_Rozenfeld = hist(K_10to14km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_K_10to14km_3_Rozenfeld")
p4_K_10to14km_4_Rozenfeld = hist(K_10to14km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_K_10to14km_4_Rozenfeld")
p5_K_10to14km_5_Rozenfeld = hist(K_10to14km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_K_10to14km_5_Rozenfeld")
p6_K_10to14km_6_Rozenfeld = hist(K_10to14km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_K_10to14km_6_Rozenfeld")
p7_K_10to14km_7_Rozenfeld = hist(K_10to14km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_K_10to14km_7_Rozenfeld")
p8_K_10to14km_8_Rozenfeld = hist(K_10to14km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_K_10to14km_8_Rozenfeld")
p9_K_10to14km_9_Rozenfeld = hist(K_10to14km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_10to14km_9_Rozenfeld")
p10_K_10to14km_10_Rozenfeld = hist(K_10to14km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p10_K_10to14km_10_Rozenfeld")
p11_K_10to14km_11_Rozenfeld = hist(K_10to14km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p11_K_10to14km_11_Rozenfeld")
p12_K_10to14km_12_Rozenfeld = hist(K_10to14km_12_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p12_K_10to14km_12_Rozenfeld")
p13_K_10to14km_13_Rozenfeld = hist(K_10to14km_13_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p13_K_10to14km_13_Rozenfeld")

# Plot numalleles

a = p1_K_10to14km_1_numalleles$counts + p2_K_10to14km_2_numalleles$counts + 
  p3_K_10to14km_3_numalleles$counts + p4_K_10to14km_4_numalleles$counts + 
  p5_K_10to14km_5_numalleles$counts + p6_K_10to14km_6_numalleles$counts + 
  p7_K_10to14km_7_numalleles$counts + p8_K_10to14km_8_numalleles$counts + 
  p9_K_10to14km_9_numalleles$counts + p10_K_10to14km_10_numalleles$counts +
  p11_K_10to14km_11_numalleles$counts + p12_K_10to14km_12_numalleles$counts +
  p13_K_10to14km_13_numalleles$counts

b = seq(0,34,1)

K_from10to14km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(K_from10to14km_numalleles ) + 
  geom_bar(data=K_from10to14km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 10-14km numalleles")

# Plot Rozenfeld

a = p1_K_10to14km_1_Rozenfeld$counts + p2_K_10to14km_2_Rozenfeld$counts + 
  p3_K_10to14km_3_Rozenfeld$counts + p4_K_10to14km_4_Rozenfeld$counts + 
  p5_K_10to14km_5_Rozenfeld$counts + p6_K_10to14km_6_Rozenfeld$counts + 
  p7_K_10to14km_7_Rozenfeld$counts + p8_K_10to14km_8_Rozenfeld$counts + 
  p9_K_10to14km_9_Rozenfeld$counts + p10_K_10to14km_10_Rozenfeld$counts +
  p11_K_10to14km_11_Rozenfeld$counts+ p12_K_10to14km_12_Rozenfeld$counts +
  p13_K_10to14km_13_Rozenfeld$counts

b = seq(0,9.9,0.1)

K_from10to14km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(K_from10to14km_Rozenfeld) + 
  geom_bar(data=K_from10to14km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 10-14km Rozenfeld")




###########################
# 15-19km apart
###########################
K_15to19km_1_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_NI_MLGs)
K_15to19km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_NI_MLGs)

K_15to19km_2_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_RT_MLGs)
K_15to19km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_RT_MLGs)

K_15to19km_3_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_LD_MLGs)
K_15to19km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_LD_MLGs)

K_15to19km_4_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_NI_MLGs)
K_15to19km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_NI_MLGs)

K_15to19km_5_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_ON_MLGs)
K_15to19km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_ON_MLGs)

K_15to19km_6_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_RT_MLGs)
K_15to19km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_RT_MLGs)

K_15to19km_7_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_KR_MLGs)
K_15to19km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_KR_MLGs)

K_15to19km_8_numalleles = asym_genet_dist_main_numalleles_new(input_K_LD_MLGs, input_K_SO_MLGs)
K_15to19km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_LD_MLGs, input_K_SO_MLGs)

K_15to19km_9_numalleles = asym_genet_dist_main_numalleles_new(input_K_LD_MLGs, input_K_RT_MLGs)
K_15to19km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_LD_MLGs, input_K_RT_MLGs)

K_15to19km_10_numalleles = asym_genet_dist_main_numalleles_new(input_K_NI_MLGs, input_K_SO_MLGs)
K_15to19km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NI_MLGs, input_K_SO_MLGs)

K_15to19km_11_numalleles = asym_genet_dist_main_numalleles_new(input_K_NI_MLGs, input_K_OK_MLGs)
K_15to19km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NI_MLGs, input_K_OK_MLGs)

K_15to19km_12_numalleles = asym_genet_dist_main_numalleles_new(input_K_ON_MLGs, input_K_SO_MLGs)
K_15to19km_12_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_ON_MLGs, input_K_SO_MLGs)

K_15to19km_13_numalleles = asym_genet_dist_main_numalleles_new(input_K_OK_MLGs, input_K_RT_MLGs)
K_15to19km_13_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_OK_MLGs, input_K_RT_MLGs)

p1_K_15to19km_1_numalleles = hist(K_15to19km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_K_15to19km_1_numalleles")
p2_K_15to19km_2_numalleles = hist(K_15to19km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_K_15to19km_2_numalleles")
p3_K_15to19km_3_numalleles = hist(K_15to19km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_K_15to19km_3_numalleles")
p4_K_15to19km_4_numalleles = hist(K_15to19km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_K_15to19km_4_numalleles")
p5_K_15to19km_5_numalleles = hist(K_15to19km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_K_15to19km_5_numalleles")
p6_K_15to19km_6_numalleles = hist(K_15to19km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_K_15to19km_6_numalleles")
p7_K_15to19km_7_numalleles = hist(K_15to19km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_K_15to19km_7_numalleles")
p8_K_15to19km_8_numalleles = hist(K_15to19km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_K_15to19km_8_numalleles")
p9_K_15to19km_9_numalleles = hist(K_15to19km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_15to19km_9_numalleles")
p10_K_15to19km_10_numalleles = hist(K_15to19km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p10_K_15to19km_10_numalleles")
p11_K_15to19km_11_numalleles = hist(K_15to19km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_15to19km_11_numalleles")
p12_K_15to19km_12_numalleles = hist(K_15to19km_12_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_15to19km_12_numalleles")
p13_K_15to19km_13_numalleles = hist(K_15to19km_13_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_K_15to19km_13_numalleles")

p1_K_15to19km_1_Rozenfeld = hist(K_15to19km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_K_15to19km_1_Rozenfeld")
p2_K_15to19km_2_Rozenfeld = hist(K_15to19km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_K_15to19km_2_Rozenfeld")
p3_K_15to19km_3_Rozenfeld = hist(K_15to19km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_K_15to19km_3_Rozenfeld")
p4_K_15to19km_4_Rozenfeld = hist(K_15to19km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_K_15to19km_4_Rozenfeld")
p5_K_15to19km_5_Rozenfeld = hist(K_15to19km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_K_15to19km_5_Rozenfeld")
p6_K_15to19km_6_Rozenfeld = hist(K_15to19km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_K_15to19km_6_Rozenfeld")
p7_K_15to19km_7_Rozenfeld = hist(K_15to19km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_K_15to19km_7_Rozenfeld")
p8_K_15to19km_8_Rozenfeld = hist(K_15to19km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_K_15to19km_8_Rozenfeld")
p9_K_15to19km_9_Rozenfeld = hist(K_15to19km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_15to19km_9_Rozenfeld")
p10_K_15to19km_10_Rozenfeld = hist(K_15to19km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p10_K_15to19km_10_Rozenfeld")
p11_K_15to19km_11_Rozenfeld = hist(K_15to19km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p11_K_15to19km_11_Rozenfeld")
p12_K_15to19km_12_Rozenfeld = hist(K_15to19km_12_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p12_K_15to19km_12_Rozenfeld")
p13_K_15to19km_13_Rozenfeld = hist(K_15to19km_13_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p13_K_15to19km_13_Rozenfeld")

# Plot numalleles

a = p1_K_15to19km_1_numalleles$counts + p2_K_15to19km_2_numalleles$counts + 
  p3_K_15to19km_3_numalleles$counts + p4_K_15to19km_4_numalleles$counts + 
  p5_K_15to19km_5_numalleles$counts + p6_K_15to19km_6_numalleles$counts + 
  p7_K_15to19km_7_numalleles$counts + p8_K_15to19km_8_numalleles$counts + 
  p9_K_15to19km_9_numalleles$counts + p10_K_15to19km_10_numalleles$counts +
  p11_K_15to19km_11_numalleles$counts + p12_K_15to19km_12_numalleles$counts +
  p13_K_15to19km_13_numalleles$counts

b = seq(0,34,1)

K_from15to19km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(K_from15to19km_numalleles ) + 
  geom_bar(data=K_from15to19km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 15-19km numalleles")

# Plot Rozenfeld

a = p1_K_15to19km_1_Rozenfeld$counts + p2_K_15to19km_2_Rozenfeld$counts + 
  p3_K_15to19km_3_Rozenfeld$counts + p4_K_15to19km_4_Rozenfeld$counts + 
  p5_K_15to19km_5_Rozenfeld$counts + p6_K_15to19km_6_Rozenfeld$counts + 
  p7_K_15to19km_7_Rozenfeld$counts + p8_K_15to19km_8_Rozenfeld$counts + 
  p9_K_15to19km_9_Rozenfeld$counts + p10_K_15to19km_10_Rozenfeld$counts +
  p11_K_15to19km_11_Rozenfeld$counts+ p12_K_15to19km_12_Rozenfeld$counts +
  p13_K_15to19km_13_Rozenfeld$counts

b = seq(0,9.9,0.1)

K_from15to19km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(K_from15to19km_Rozenfeld) + 
  geom_bar(data=K_from15to19km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 15-19km Rozenfeld")




###########################
# 20-35km apart
###########################
K_20to35km_1_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_ON_MLGs)
K_20to35km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_ON_MLGs)

K_20to35km_2_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_NI_MLGs)
K_20to35km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_NI_MLGs)

K_20to35km_3_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_ON_MLGs)
K_20to35km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_ON_MLGs)

K_20to35km_4_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_RT_MLGs)
K_20to35km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_RT_MLGs)

K_20to35km_5_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_KR_MLGs)
K_20to35km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_KR_MLGs)

K_20to35km_6_numalleles = asym_genet_dist_main_numalleles_new(input_K_BK_MLGs, input_K_SO_MLGs)
K_20to35km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_BK_MLGs, input_K_SO_MLGs)

K_20to35km_7_numalleles = asym_genet_dist_main_numalleles_new(input_K_NI_MLGs, input_K_RT_MLGs)
K_20to35km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NI_MLGs, input_K_RT_MLGs)

K_20to35km_8_numalleles = asym_genet_dist_main_numalleles_new(input_K_ON_MLGs, input_K_OK_MLGs)
K_20to35km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_ON_MLGs, input_K_OK_MLGs)

K_20to35km_9_numalleles = asym_genet_dist_main_numalleles_new(input_K_ON_MLGs, input_K_RT_MLGs)
K_20to35km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_ON_MLGs, input_K_RT_MLGs)

K_20to35km_10_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_KR_MLGs)
K_20to35km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_KR_MLGs)

K_20to35km_11_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_KR_MLGs)
K_20to35km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_KR_MLGs)

K_20to35km_12_numalleles = asym_genet_dist_main_numalleles_new(input_K_SK_MLGs, input_K_SO_MLGs)
K_20to35km_12_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SK_MLGs, input_K_SO_MLGs)

K_20to35km_13_numalleles = asym_genet_dist_main_numalleles_new(input_K_KR_MLGs, input_K_OK_MLGs)
K_20to35km_13_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_KR_MLGs, input_K_OK_MLGs)

K_20to35km_14_numalleles = asym_genet_dist_main_numalleles_new(input_K_KR_MLGs, input_K_RT_MLGs)
K_20to35km_14_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_KR_MLGs, input_K_RT_MLGs)

K_20to35km_15_numalleles = asym_genet_dist_main_numalleles_new(input_K_NG_MLGs, input_K_SO_MLGs)
K_20to35km_15_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_NG_MLGs, input_K_SO_MLGs)

K_20to35km_16_numalleles = asym_genet_dist_main_numalleles_new(input_K_K_MLGs, input_K_SO_MLGs)
K_20to35km_16_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_K_MLGs, input_K_SO_MLGs)

K_20to35km_17_numalleles = asym_genet_dist_main_numalleles_new(input_K_SO_MLGs, input_K_OK_MLGs)
K_20to35km_17_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SO_MLGs, input_K_OK_MLGs)

K_20to35km_18_numalleles = asym_genet_dist_main_numalleles_new(input_K_SO_MLGs, input_K_RT_MLGs)
K_20to35km_18_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_K_SO_MLGs, input_K_RT_MLGs)



p1_K_20to35km_1_numalleles = hist(K_20to35km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_K_20to35km_1_numalleles")
p2_K_20to35km_2_numalleles = hist(K_20to35km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_K_20to35km_2_numalleles")
p3_K_20to35km_3_numalleles = hist(K_20to35km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_K_20to35km_3_numalleles")
p4_K_20to35km_4_numalleles = hist(K_20to35km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_K_20to35km_4_numalleles")
p5_K_20to35km_5_numalleles = hist(K_20to35km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_K_20to35km_5_numalleles")
p6_K_20to35km_6_numalleles = hist(K_20to35km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_K_20to35km_6_numalleles")
p7_K_20to35km_7_numalleles = hist(K_20to35km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_K_20to35km_7_numalleles")
p8_K_20to35km_8_numalleles = hist(K_20to35km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_K_20to35km_8_numalleles")
p9_K_20to35km_9_numalleles = hist(K_20to35km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_9_numalleles")
p10_K_20to35km_10_numalleles = hist(K_20to35km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_10_numalleles")
p11_K_20to35km_11_numalleles = hist(K_20to35km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_11_numalleles")
p12_K_20to35km_12_numalleles = hist(K_20to35km_12_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_12_numalleles")
p13_K_20to35km_13_numalleles = hist(K_20to35km_13_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_13_numalleles")
p14_K_20to35km_14_numalleles = hist(K_20to35km_14_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_14_numalleles")
p15_K_20to35km_15_numalleles = hist(K_20to35km_15_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_15_numalleles")
p16_K_20to35km_16_numalleles = hist(K_20to35km_16_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_16_numalleles")
p17_K_20to35km_17_numalleles = hist(K_20to35km_17_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_17_numalleles")
p18_K_20to35km_18_numalleles = hist(K_20to35km_18_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_K_20to35km_18_numalleles")

p1_K_20to35km_1_Rozenfeld = hist(K_20to35km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_K_20to35km_1_Rozenfeld")
p2_K_20to35km_2_Rozenfeld = hist(K_20to35km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_K_20to35km_2_Rozenfeld")
p3_K_20to35km_3_Rozenfeld = hist(K_20to35km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_K_20to35km_3_Rozenfeld")
p4_K_20to35km_4_Rozenfeld = hist(K_20to35km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_K_20to35km_4_Rozenfeld")
p5_K_20to35km_5_Rozenfeld = hist(K_20to35km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_K_20to35km_5_Rozenfeld")
p6_K_20to35km_6_Rozenfeld = hist(K_20to35km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_K_20to35km_6_Rozenfeld")
p7_K_20to35km_7_Rozenfeld = hist(K_20to35km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_K_20to35km_7_Rozenfeld")
p8_K_20to35km_8_Rozenfeld = hist(K_20to35km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_K_20to35km_8_Rozenfeld")
p9_K_20to35km_9_Rozenfeld = hist(K_20to35km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_9_Rozenfeld")
p10_K_20to35km_10_Rozenfeld = hist(K_20to35km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_10_Rozenfeld")
p11_K_20to35km_11_Rozenfeld = hist(K_20to35km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_11_Rozenfeld")
p12_K_20to35km_12_Rozenfeld = hist(K_20to35km_12_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_12_Rozenfeld")
p13_K_20to35km_13_Rozenfeld = hist(K_20to35km_13_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_13_Rozenfeld")
p14_K_20to35km_14_Rozenfeld = hist(K_20to35km_14_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_14_Rozenfeld")
p15_K_20to35km_15_Rozenfeld = hist(K_20to35km_15_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_15_Rozenfeld")
p16_K_20to35km_16_Rozenfeld = hist(K_20to35km_16_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_16_Rozenfeld")
p17_K_20to35km_17_Rozenfeld = hist(K_20to35km_17_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_17_Rozenfeld")
p18_K_20to35km_18_Rozenfeld = hist(K_20to35km_18_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_K_20to35km_18_Rozenfeld")

# Plot numalleles

a = p1_K_20to35km_1_numalleles$counts + p2_K_20to35km_2_numalleles$counts + 
  p3_K_20to35km_3_numalleles$counts + p4_K_20to35km_4_numalleles$counts + 
  p5_K_20to35km_5_numalleles$counts + p6_K_20to35km_6_numalleles$counts + 
  p7_K_20to35km_7_numalleles$counts + p8_K_20to35km_8_numalleles$counts + 
  p9_K_20to35km_9_numalleles$counts + p10_K_20to35km_10_numalleles$counts + 
  p11_K_20to35km_11_numalleles$counts + p12_K_20to35km_12_numalleles$counts + 
  p13_K_20to35km_13_numalleles$counts + p14_K_20to35km_14_numalleles$counts + 
  p15_K_20to35km_15_numalleles$counts + p16_K_20to35km_16_numalleles$counts + 
  p17_K_20to35km_17_numalleles$counts + p18_K_20to35km_18_numalleles$counts

b = seq(0,34,1)

K_from20to35km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(K_from20to35km_numalleles ) + 
  geom_bar(data=K_from20to35km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 20-35km numalleles")

# Plot Rozenfeld

a = p1_K_20to35km_1_Rozenfeld$counts + p2_K_20to35km_2_Rozenfeld$counts + 
  p3_K_20to35km_3_Rozenfeld$counts + p4_K_20to35km_4_Rozenfeld$counts + 
  p5_K_20to35km_5_Rozenfeld$counts + p6_K_20to35km_6_Rozenfeld$counts + 
  p7_K_20to35km_7_Rozenfeld$counts + p8_K_20to35km_8_Rozenfeld$counts + 
  p9_K_20to35km_9_Rozenfeld$counts + p10_K_20to35km_10_Rozenfeld$counts + 
  p11_K_20to35km_11_Rozenfeld$counts + p12_K_20to35km_12_Rozenfeld$counts + 
  p13_K_20to35km_13_Rozenfeld$counts + p14_K_20to35km_14_Rozenfeld$counts + 
  p15_K_20to35km_15_Rozenfeld$counts + p16_K_20to35km_16_Rozenfeld$counts + 
  p17_K_20to35km_17_Rozenfeld$counts + p18_K_20to35km_18_Rozenfeld$counts


b = seq(0,9.9,0.1)

K_from20to35km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(K_from20to35km_Rozenfeld) + 
  geom_bar(data=K_from20to35km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS K 20-35km Rozenfeld")



######################################################
# Create from 5 to 19km distance class:
######################################################

a1 = p1_K_5to9km_1_numalleles$counts + p2_K_5to9km_2_numalleles$counts + 
  p3_K_5to9km_3_numalleles$counts + p4_K_5to9km_4_numalleles$counts + 
  p5_K_5to9km_5_numalleles$counts + p6_K_5to9km_6_numalleles$counts + 
  p7_K_5to9km_7_numalleles$counts + p8_K_5to9km_8_numalleles$counts + 
  p9_K_5to9km_9_numalleles$counts + p10_K_5to9km_10_numalleles$counts +
  p11_K_5to9km_11_numalleles$counts +p1_K_10to14km_1_numalleles$counts + 
  p2_K_10to14km_2_numalleles$counts + 
  p3_K_10to14km_3_numalleles$counts + p4_K_10to14km_4_numalleles$counts + 
  p5_K_10to14km_5_numalleles$counts + p6_K_10to14km_6_numalleles$counts + 
  p7_K_10to14km_7_numalleles$counts + p8_K_10to14km_8_numalleles$counts + 
  p9_K_10to14km_9_numalleles$counts + p10_K_10to14km_10_numalleles$counts +
  p11_K_10to14km_11_numalleles$counts + p12_K_10to14km_12_numalleles$counts +
  p13_K_10to14km_13_numalleles$counts +
  p1_K_15to19km_1_numalleles$counts + p2_K_15to19km_2_numalleles$counts + 
  p3_K_15to19km_3_numalleles$counts + p4_K_15to19km_4_numalleles$counts + 
  p5_K_15to19km_5_numalleles$counts + p6_K_15to19km_6_numalleles$counts + 
  p7_K_15to19km_7_numalleles$counts + p8_K_15to19km_8_numalleles$counts + 
  p9_K_15to19km_9_numalleles$counts + p10_K_15to19km_10_numalleles$counts +
  p11_K_15to19km_11_numalleles$counts + p12_K_15to19km_12_numalleles$counts +
  p13_K_15to19km_13_numalleles$counts

b = seq(0,34,1)

K_from5to19km_numalleles = data.frame(counts=a1, genet_dist=b)


a2 = p1_K_5to9km_1_Rozenfeld$counts + p2_K_5to9km_2_Rozenfeld$counts + 
  p3_K_5to9km_3_Rozenfeld$counts + p4_K_5to9km_4_Rozenfeld$counts + 
  p5_K_5to9km_5_Rozenfeld$counts + p6_K_5to9km_6_Rozenfeld$counts + 
  p7_K_5to9km_7_Rozenfeld$counts + p8_K_5to9km_8_Rozenfeld$counts + 
  p9_K_5to9km_9_Rozenfeld$counts + p10_K_5to9km_10_Rozenfeld$counts +
  p11_K_5to9km_11_Rozenfeld$counts +p1_K_10to14km_1_Rozenfeld$counts + 
  p2_K_10to14km_2_Rozenfeld$counts + 
  p3_K_10to14km_3_Rozenfeld$counts + p4_K_10to14km_4_Rozenfeld$counts + 
  p5_K_10to14km_5_Rozenfeld$counts + p6_K_10to14km_6_Rozenfeld$counts + 
  p7_K_10to14km_7_Rozenfeld$counts + p8_K_10to14km_8_Rozenfeld$counts + 
  p9_K_10to14km_9_Rozenfeld$counts + p10_K_10to14km_10_Rozenfeld$counts +
  p11_K_10to14km_11_Rozenfeld$counts + p12_K_10to14km_12_Rozenfeld$counts +
  p13_K_10to14km_13_Rozenfeld$counts +
  p1_K_15to19km_1_Rozenfeld$counts + p2_K_15to19km_2_Rozenfeld$counts + 
  p3_K_15to19km_3_Rozenfeld$counts + p4_K_15to19km_4_Rozenfeld$counts + 
  p5_K_15to19km_5_Rozenfeld$counts + p6_K_15to19km_6_Rozenfeld$counts + 
  p7_K_15to19km_7_Rozenfeld$counts + p8_K_15to19km_8_Rozenfeld$counts + 
  p9_K_15to19km_9_Rozenfeld$counts + p10_K_15to19km_10_Rozenfeld$counts +
  p11_K_15to19km_11_Rozenfeld$counts + p12_K_15to19km_12_Rozenfeld$counts +
  p13_K_15to19km_13_Rozenfeld$counts

b = seq(0,9.9,0.1)

K_from5to19km_Rozenfeld = data.frame(counts=a2, genet_dist=b)



######################################################
# Plot all K together
######################################################


K_less_than_5km_numalleles$classif = "<5km"
K_less_than_5km_Rozenfeld$classif = "<5km"
K_from5to19km_numalleles$classif = "5-20km"
K_from5to19km_Rozenfeld$classif = "5-20km"
K_from20to35km_numalleles$classif = ">20km"
K_from20to35km_Rozenfeld$classif = ">20km"

sum(K_less_than_5km_numalleles$counts)

K_less_than_5km_numalleles$density = (K_less_than_5km_numalleles$counts)/(sum(K_less_than_5km_numalleles$counts))
K_less_than_5km_Rozenfeld$density = (K_less_than_5km_Rozenfeld$counts)/(sum(K_less_than_5km_Rozenfeld$counts))

K_from5to19km_numalleles$density = (K_from5to19km_numalleles$counts)/(sum(K_from5to19km_numalleles$counts))
K_from5to19km_Rozenfeld$density = (K_from5to19km_Rozenfeld$counts)/(sum(K_from5to19km_Rozenfeld$counts))

K_from20to35km_numalleles$density = (K_from20to35km_numalleles$counts)/(sum(K_from20to35km_numalleles$counts))
K_from20to35km_Rozenfeld$density = (K_from20to35km_Rozenfeld$counts)/(sum(K_from20to35km_Rozenfeld$counts))


alltogether_K_numalleles = rbind(K_less_than_5km_numalleles, K_from5to19km_numalleles, 
                                 K_from20to35km_numalleles)

alltogether_K_Rozenfeld = rbind(K_less_than_5km_Rozenfeld, K_from5to19km_Rozenfeld, 
                                K_from20to35km_Rozenfeld)

alltogether_K_numalleles$classif = factor(alltogether_K_numalleles$classif, 
                                          levels = c("<5km", "5-20km", ">20km"))

alltogether_K_Rozenfeld$classif = factor(alltogether_K_Rozenfeld$classif, 
                                          levels = c("<5km", "5-20km", ">20km"))

#Plot numalleles
jpeg(filename = "~/Documents/GRONINGEN/TOP PROGRAMME EVOLUTIONARY BIOLOGY/Master project ZOSTERA/Data Analysis/GDS (RClone)/GDS_among_meadows/GDS_bygeographic_dist/Graphs/K_numalleles_MLGs_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", res=600)

ggplot(alltogether_K_numalleles, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nShared Alleles' Distance", y="Frequency\n", col="Distance classes") +
  #ggtitle("Kungälv GDS by geographical distance - Shared Alleles' Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15), 
        legend.key=element_blank(), 
        legend.position="none") +
  scale_colour_manual(values=c("darkslategray1", "cyan3", "cyan4"))

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


#Plot Rozenfeld
jpeg(filename = "~/Documents/GRONINGEN/TOP PROGRAMME EVOLUTIONARY BIOLOGY/Master project ZOSTERA/Data Analysis/GDS (RClone)/GDS_among_meadows/GDS_bygeographic_dist/Graphs/K_Rozenfeld_MLGs_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", res=600)

ggplot(alltogether_K_Rozenfeld, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nRozenfeld's Distance", y="Frequency\n", col="Distance classes") +
  #ggtitle("Kungälv GDS by geographical distance - Rozenfeld's Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15), 
        legend.key=element_blank(), 
        legend.position="none") +
  scale_colour_manual(values=c("darkslategray1", "cyan3", "cyan4"))

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()

# Grey and pdf
pdf(filename = "~/Documents/GRONINGEN/TOP PROGRAMME EVOLUTIONARY BIOLOGY/Master project ZOSTERA/Data Analysis/GDS (RClone)/GDS_among_meadows/GDS_bygeographic_dist/Graphs/K_MLGs_paper.pdf", 
     width=20, height=12, onefile=T, family="Times New Roman", 
    pointsize=12, bg="white")

ggplot(alltogether_K_numalleles, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nShared Alleles' Distance", y="Frequency\n", col="Distance classes") +
  ggtitle("Kungälv GDS by geographical distance - Shared Alleles' Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15), 
        legend.key=element_blank()) +
  scale_colour_manual(values=c("gray85", "gray48", "gray8"))

ggplot(alltogether_K_Rozenfeld, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nRozenfeld's Distance", y="Density\n", col="Distance classes") +
  ggtitle("Kungälv GDS by geographical distance - Rozenfeld's Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15)) +
  scale_colour_manual(values=c("gray85", "gray48", "gray8"))

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()







######################################################################################################

##### G populations (relying on the separation by lc.dist but allowing transition from deepest point until 7m above sea level):

######################################################################################################


###########################
# <5km apart (from 0 to 4)
###########################

G_less_than_5km_1_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_SK_MLGs)
G_less_than_5km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_SK_MLGs)

G_less_than_5km_2_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_NB_MLGs)
G_less_than_5km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_NB_MLGs)

G_less_than_5km_3_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_G_SG_MLGs)
G_less_than_5km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_G_SG_MLGs)

G_less_than_5km_4_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_G_GB_MLGs)
G_less_than_5km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_G_GB_MLGs)

G_less_than_5km_5_numalleles = asym_genet_dist_main_numalleles_new(input_G_AF_MLGs, input_G_AF_MLGs)
G_less_than_5km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AF_MLGs, input_G_AF_MLGs)

G_less_than_5km_6_numalleles = asym_genet_dist_main_numalleles_new(input_G_AS_MLGs, input_G_AS_MLGs)
G_less_than_5km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AS_MLGs, input_G_AS_MLGs)

G_less_than_5km_7_numalleles = asym_genet_dist_main_numalleles_new(input_G_BB_MLGs, input_G_BB_MLGs)
G_less_than_5km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BB_MLGs, input_G_BB_MLGs)

G_less_than_5km_8_numalleles = asym_genet_dist_main_numalleles_new(input_G_BV_MLGs, input_G_BV_MLGs)
G_less_than_5km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BV_MLGs, input_G_BV_MLGs)

G_less_than_5km_9_numalleles = asym_genet_dist_main_numalleles_new(input_G_RX_MLGs, input_G_RX_MLGs)
G_less_than_5km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_RX_MLGs, input_G_RX_MLGs)

G_less_than_5km_10_numalleles = asym_genet_dist_main_numalleles_new(input_Snack_MLGs, input_Snack_MLGs)
G_less_than_5km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_Snack_MLGs, input_Snack_MLGs)

G_less_than_5km_11_numalleles = asym_genet_dist_main_numalleles_new(input_Torg_MLGs, input_Torg_MLGs)
G_less_than_5km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_Torg_MLGs, input_Torg_MLGs)

G_less_than_5km_12_numalleles = asym_genet_dist_main_numalleles_new(input_Snack_MLGs, input_Torg_MLGs)
G_less_than_5km_12_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_Snack_MLGs, input_Torg_MLGs)


p1_G_less_than_5km_1_numalleles = hist(G_less_than_5km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_G_less_than_5km_1_numalleles")
p2_G_less_than_5km_2_numalleles = hist(G_less_than_5km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_G_less_than_5km_2_numalleles")
p3_G_less_than_5km_3_numalleles = hist(G_less_than_5km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_G_less_than_5km_3_numalleles")
p4_G_less_than_5km_4_numalleles = hist(G_less_than_5km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_G_less_than_5km_4_numalleles")
p5_G_less_than_5km_5_numalleles = hist(G_less_than_5km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_G_less_than_5km_5_numalleles")
p6_G_less_than_5km_6_numalleles = hist(G_less_than_5km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_G_less_than_5km_6_numalleles")
p7_G_less_than_5km_7_numalleles = hist(G_less_than_5km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_G_less_than_5km_7_numalleles")
p8_G_less_than_5km_8_numalleles = hist(G_less_than_5km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_G_less_than_5km_8_numalleles")
p9_G_less_than_5km_9_numalleles = hist(G_less_than_5km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_G_less_than_5km_9_numalleles")
p10_G_less_than_5km_10_numalleles = hist(G_less_than_5km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p10_G_less_than_5km_10_numalleles")
p11_G_less_than_5km_11_numalleles = hist(G_less_than_5km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_G_less_than_5km_11_numalleles")
p12_G_less_than_5km_12_numalleles = hist(G_less_than_5km_12_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p12_G_less_than_5km_12_numalleles")

p1_G_less_than_5km_1_Rozenfeld = hist(G_less_than_5km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_G_less_than_5km_1_Rozenfeld")
p2_G_less_than_5km_2_Rozenfeld = hist(G_less_than_5km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_G_less_than_5km_2_Rozenfeld")
p3_G_less_than_5km_3_Rozenfeld = hist(G_less_than_5km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_G_less_than_5km_3_Rozenfeld")
p4_G_less_than_5km_4_Rozenfeld = hist(G_less_than_5km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_G_less_than_5km_4_Rozenfeld")
p5_G_less_than_5km_5_Rozenfeld = hist(G_less_than_5km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_G_less_than_5km_5_Rozenfeld")
p6_G_less_than_5km_6_Rozenfeld = hist(G_less_than_5km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_G_less_than_5km_6_Rozenfeld")
p7_G_less_than_5km_7_Rozenfeld = hist(G_less_than_5km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_G_less_than_5km_7_Rozenfeld")
p8_G_less_than_5km_8_Rozenfeld = hist(G_less_than_5km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_G_less_than_5km_8_Rozenfeld")
p9_G_less_than_5km_9_Rozenfeld = hist(G_less_than_5km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_G_less_than_5km_9_Rozenfeld")
p10_G_less_than_5km_10_Rozenfeld = hist(G_less_than_5km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p10_G_less_than_5km_10_Rozenfeld")
p11_G_less_than_5km_11_Rozenfeld = hist(G_less_than_5km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p11_G_less_than_5km_11_Rozenfeld")
p12_G_less_than_5km_12_Rozenfeld = hist(G_less_than_5km_12_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p12_G_less_than_5km_12_Rozenfeld")

# Plot numalleles

a = p1_G_less_than_5km_1_numalleles$counts + p2_G_less_than_5km_2_numalleles$counts + 
  p3_G_less_than_5km_3_numalleles$counts + p4_G_less_than_5km_4_numalleles$counts + 
  p5_G_less_than_5km_5_numalleles$counts + p6_G_less_than_5km_6_numalleles$counts + 
  p7_G_less_than_5km_7_numalleles$counts + p8_G_less_than_5km_8_numalleles$counts + 
  p9_G_less_than_5km_9_numalleles$counts + p10_G_less_than_5km_10_numalleles$counts +
  p11_G_less_than_5km_11_numalleles$counts + p12_G_less_than_5km_12_numalleles$counts


b = seq(0,34,1)

G_less_than_5km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(G_less_than_5km_numalleles ) + 
  geom_bar(data=G_less_than_5km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G <5km numalleles")

# Plot Rozenfeld

a = p1_G_less_than_5km_1_Rozenfeld$counts + p2_G_less_than_5km_2_Rozenfeld$counts + 
  p3_G_less_than_5km_3_Rozenfeld$counts + p4_G_less_than_5km_4_Rozenfeld$counts + 
  p5_G_less_than_5km_5_Rozenfeld$counts + p6_G_less_than_5km_6_Rozenfeld$counts + 
  p7_G_less_than_5km_7_Rozenfeld$counts + p8_G_less_than_5km_8_Rozenfeld$counts + 
  p9_G_less_than_5km_9_Rozenfeld$counts + p10_G_less_than_5km_10_Rozenfeld$counts +
  p11_G_less_than_5km_11_Rozenfeld$counts+ p12_G_less_than_5km_12_Rozenfeld$counts 

b = seq(0,9.9,0.1)

G_less_than_5km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(G_less_than_5km_Rozenfeld) + 
  geom_bar(data=G_less_than_5km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G <5km Rozenfeld")


###########################
# 5-9km apart
###########################
G_5to9km_1_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_Snack_MLGs)
G_5to9km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_Snack_MLGs)

G_5to9km_2_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_G_GB_MLGs)
G_5to9km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_G_GB_MLGs)

G_5to9km_3_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_Torg_MLGs)
G_5to9km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_Torg_MLGs)

G_5to9km_4_numalleles = asym_genet_dist_main_numalleles_new(input_G_BB_MLGs, input_G_RX_MLGs)
G_5to9km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BB_MLGs, input_G_RX_MLGs)

G_5to9km_5_numalleles = asym_genet_dist_main_numalleles_new(input_G_BV_MLGs, input_G_RX_MLGs)
G_5to9km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BV_MLGs, input_G_RX_MLGs)

G_5to9km_6_numalleles = asym_genet_dist_main_numalleles_new(input_G_AS_MLGs, input_G_BB_MLGs)
G_5to9km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AS_MLGs, input_G_BB_MLGs)


p1_G_5to9km_1_numalleles = hist(G_5to9km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_G_5to9km_1_numalleles")
p2_G_5to9km_2_numalleles = hist(G_5to9km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_G_5to9km_2_numalleles")
p3_G_5to9km_3_numalleles = hist(G_5to9km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_G_5to9km_3_numalleles")
p4_G_5to9km_4_numalleles = hist(G_5to9km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_G_5to9km_4_numalleles")
p5_G_5to9km_5_numalleles = hist(G_5to9km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_G_5to9km_5_numalleles")
p6_G_5to9km_6_numalleles = hist(G_5to9km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_G_5to9km_6_numalleles")

p1_G_5to9km_1_Rozenfeld = hist(G_5to9km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_G_5to9km_1_Rozenfeld")
p2_G_5to9km_2_Rozenfeld = hist(G_5to9km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_G_5to9km_2_Rozenfeld")
p3_G_5to9km_3_Rozenfeld = hist(G_5to9km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_G_5to9km_3_Rozenfeld")
p4_G_5to9km_4_Rozenfeld = hist(G_5to9km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_G_5to9km_4_Rozenfeld")
p5_G_5to9km_5_Rozenfeld = hist(G_5to9km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_G_5to9km_5_Rozenfeld")
p6_G_5to9km_6_Rozenfeld = hist(G_5to9km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_G_5to9km_6_Rozenfeld")

# Plot numalleles

a = p1_G_5to9km_1_numalleles$counts + p2_G_5to9km_2_numalleles$counts + 
  p3_G_5to9km_3_numalleles$counts + p4_G_5to9km_4_numalleles$counts + 
  p5_G_5to9km_5_numalleles$counts + p6_G_5to9km_6_numalleles$counts 

b = seq(0,34,1)

G_from5to9km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(G_from5to9km_numalleles ) + 
  geom_bar(data=G_from5to9km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 5-9km numalleles")

# Plot Rozenfeld

a = p1_G_5to9km_1_Rozenfeld$counts + p2_G_5to9km_2_Rozenfeld$counts + 
  p3_G_5to9km_3_Rozenfeld$counts + p4_G_5to9km_4_Rozenfeld$counts + 
  p5_G_5to9km_5_Rozenfeld$counts + p6_G_5to9km_6_Rozenfeld$counts 


b = seq(0,9.9,0.1)

G_from5to9km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(G_from5to9km_Rozenfeld) + 
  geom_bar(data=G_from5to9km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 5-9km Rozenfeld")




###########################
# 10-14km apart
###########################
G_10to14km_1_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_NB_MLGs)
G_10to14km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_NB_MLGs)

G_10to14km_2_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_Torg_MLGs)
G_10to14km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_Torg_MLGs)

G_10to14km_3_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_G_BB_MLGs)
G_10to14km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_G_BB_MLGs)

G_10to14km_4_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_G_BB_MLGs)
G_10to14km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_G_BB_MLGs)

G_10to14km_5_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_Snack_MLGs)
G_10to14km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_Snack_MLGs)

G_10to14km_6_numalleles = asym_genet_dist_main_numalleles_new(input_G_AF_MLGs, input_G_AS_MLGs)
G_10to14km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AF_MLGs, input_G_AS_MLGs)

G_10to14km_7_numalleles = asym_genet_dist_main_numalleles_new(input_G_AS_MLGs, input_G_RX_MLGs)
G_10to14km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AS_MLGs, input_G_RX_MLGs)

G_10to14km_8_numalleles = asym_genet_dist_main_numalleles_new(input_G_BB_MLGs, input_G_BV_MLGs)
G_10to14km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BB_MLGs, input_G_BV_MLGs)


p1_G_10to14km_1_numalleles = hist(G_10to14km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_G_10to14km_1_numalleles")
p2_G_10to14km_2_numalleles = hist(G_10to14km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_G_10to14km_2_numalleles")
p3_G_10to14km_3_numalleles = hist(G_10to14km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_G_10to14km_3_numalleles")
p4_G_10to14km_4_numalleles = hist(G_10to14km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_G_10to14km_4_numalleles")
p5_G_10to14km_5_numalleles = hist(G_10to14km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_G_10to14km_5_numalleles")
p6_G_10to14km_6_numalleles = hist(G_10to14km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_G_10to14km_6_numalleles")
p7_G_10to14km_7_numalleles = hist(G_10to14km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_G_10to14km_7_numalleles")
p8_G_10to14km_8_numalleles = hist(G_10to14km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_G_10to14km_8_numalleles")

p1_G_10to14km_1_Rozenfeld = hist(G_10to14km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_G_10to14km_1_Rozenfeld")
p2_G_10to14km_2_Rozenfeld = hist(G_10to14km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_G_10to14km_2_Rozenfeld")
p3_G_10to14km_3_Rozenfeld = hist(G_10to14km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_G_10to14km_3_Rozenfeld")
p4_G_10to14km_4_Rozenfeld = hist(G_10to14km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_G_10to14km_4_Rozenfeld")
p5_G_10to14km_5_Rozenfeld = hist(G_10to14km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_G_10to14km_5_Rozenfeld")
p6_G_10to14km_6_Rozenfeld = hist(G_10to14km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_G_10to14km_6_Rozenfeld")
p7_G_10to14km_7_Rozenfeld = hist(G_10to14km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_G_10to14km_7_Rozenfeld")
p8_G_10to14km_8_Rozenfeld = hist(G_10to14km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_G_10to14km_8_Rozenfeld")


# Plot numalleles

a = p1_G_10to14km_1_numalleles$counts + p2_G_10to14km_2_numalleles$counts + 
  p3_G_10to14km_3_numalleles$counts + p4_G_10to14km_4_numalleles$counts + 
  p5_G_10to14km_5_numalleles$counts + p6_G_10to14km_6_numalleles$counts + 
  p7_G_10to14km_7_numalleles$counts + p8_G_10to14km_8_numalleles$counts  


b = seq(0,34,1)

G_from10to14km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(G_from10to14km_numalleles ) + 
  geom_bar(data=G_from10to14km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 10-14km numalleles")

# Plot Rozenfeld

a = p1_G_10to14km_1_Rozenfeld$counts + p2_G_10to14km_2_Rozenfeld$counts + 
  p3_G_10to14km_3_Rozenfeld$counts + p4_G_10to14km_4_Rozenfeld$counts + 
  p5_G_10to14km_5_Rozenfeld$counts + p6_G_10to14km_6_Rozenfeld$counts + 
  p7_G_10to14km_7_Rozenfeld$counts + p8_G_10to14km_8_Rozenfeld$counts  

b = seq(0,9.9,0.1)

G_from10to14km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(G_from10to14km_Rozenfeld) + 
  geom_bar(data=G_from10to14km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 10-14km Rozenfeld")



###########################
# 15-19km apart
###########################

G_15to19km_1_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_Torg_MLGs)
G_15to19km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_Torg_MLGs)

G_15to19km_2_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_G_AS_MLGs)
G_15to19km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_G_AS_MLGs)

G_15to19km_3_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_G_RX_MLGs)
G_15to19km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_G_RX_MLGs)

G_15to19km_4_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_G_RX_MLGs)
G_15to19km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_G_RX_MLGs)

G_15to19km_5_numalleles = asym_genet_dist_main_numalleles_new(input_G_AF_MLGs, input_G_BB_MLGs)
G_15to19km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AF_MLGs, input_G_BB_MLGs)

G_15to19km_6_numalleles = asym_genet_dist_main_numalleles_new(input_G_AS_MLGs, input_G_BV_MLGs)
G_15to19km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AS_MLGs, input_G_BV_MLGs)

G_15to19km_7_numalleles = asym_genet_dist_main_numalleles_new(input_G_BB_MLGs, input_Torg_MLGs)
G_15to19km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BB_MLGs, input_Torg_MLGs)

G_15to19km_8_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_Torg_MLGs)
G_15to19km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_Torg_MLGs)

G_15to19km_9_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_G_AS_MLGs)
G_15to19km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_G_AS_MLGs)

G_15to19km_10_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_Snack_MLGs)
G_15to19km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_Snack_MLGs)

G_15to19km_11_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_GB_MLGs)
G_15to19km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_GB_MLGs)

G_15to19km_12_numalleles = asym_genet_dist_main_numalleles_new(input_G_AF_MLGs, input_G_RX_MLGs)
G_15to19km_12_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AF_MLGs, input_G_RX_MLGs)




p1_G_15to19km_1_numalleles = hist(G_15to19km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_G_15to19km_1_numalleles")
p2_G_15to19km_2_numalleles = hist(G_15to19km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_G_15to19km_2_numalleles")
p3_G_15to19km_3_numalleles = hist(G_15to19km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_G_15to19km_3_numalleles")
p4_G_15to19km_4_numalleles = hist(G_15to19km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_G_15to19km_4_numalleles")
p5_G_15to19km_5_numalleles = hist(G_15to19km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_G_15to19km_5_numalleles")
p6_G_15to19km_6_numalleles = hist(G_15to19km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_G_15to19km_6_numalleles")
p7_G_15to19km_7_numalleles = hist(G_15to19km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_G_15to19km_7_numalleles")
p8_G_15to19km_8_numalleles = hist(G_15to19km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_G_15to19km_8_numalleles")
p9_G_15to19km_9_numalleles = hist(G_15to19km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_G_15to19km_9_numalleles")
p10_G_15to19km_10_numalleles = hist(G_15to19km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p10_G_15to19km_10_numalleles")
p11_G_15to19km_11_numalleles = hist(G_15to19km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_G_15to19km_11_numalleles")
p12_G_15to19km_12_numalleles = hist(G_15to19km_12_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p12_G_15to19km_12_numalleles")

p1_G_15to19km_1_Rozenfeld = hist(G_15to19km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_G_15to19km_1_Rozenfeld")
p2_G_15to19km_2_Rozenfeld = hist(G_15to19km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_G_15to19km_2_Rozenfeld")
p3_G_15to19km_3_Rozenfeld = hist(G_15to19km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_G_15to19km_3_Rozenfeld")
p4_G_15to19km_4_Rozenfeld = hist(G_15to19km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_G_15to19km_4_Rozenfeld")
p5_G_15to19km_5_Rozenfeld = hist(G_15to19km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_G_15to19km_5_Rozenfeld")
p6_G_15to19km_6_Rozenfeld = hist(G_15to19km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_G_15to19km_6_Rozenfeld")
p7_G_15to19km_7_Rozenfeld = hist(G_15to19km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_G_15to19km_7_Rozenfeld")
p8_G_15to19km_8_Rozenfeld = hist(G_15to19km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_G_15to19km_8_Rozenfeld")
p9_G_15to19km_9_Rozenfeld = hist(G_15to19km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_G_15to19km_9_Rozenfeld")
p10_G_15to19km_10_Rozenfeld = hist(G_15to19km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p10_G_15to19km_10_Rozenfeld")
p11_G_15to19km_11_Rozenfeld = hist(G_15to19km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p11_G_15to19km_11_Rozenfeld")
p12_G_15to19km_12_Rozenfeld = hist(G_15to19km_12_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p12_G_15to19km_12_Rozenfeld")


# Plot numalleles

a = p1_G_15to19km_1_numalleles$counts + p2_G_15to19km_2_numalleles$counts + 
  p3_G_15to19km_3_numalleles$counts + p4_G_15to19km_4_numalleles$counts + 
  p5_G_15to19km_5_numalleles$counts + p6_G_15to19km_6_numalleles$counts + 
  p7_G_15to19km_7_numalleles$counts + p8_G_15to19km_8_numalleles$counts + 
  p9_G_15to19km_9_numalleles$counts + p10_G_15to19km_10_numalleles$counts +
  p11_G_15to19km_11_numalleles$counts + p12_G_15to19km_12_numalleles$counts


b = seq(0,34,1)

G_from15to19km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(G_from15to19km_numalleles ) + 
  geom_bar(data=G_from15to19km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 15-19km numalleles")

# Plot Rozenfeld

a = p1_G_15to19km_1_Rozenfeld$counts + p2_G_15to19km_2_Rozenfeld$counts + 
  p3_G_15to19km_3_Rozenfeld$counts + p4_G_15to19km_4_Rozenfeld$counts + 
  p5_G_15to19km_5_Rozenfeld$counts + p6_G_15to19km_6_Rozenfeld$counts + 
  p7_G_15to19km_7_Rozenfeld$counts + p8_G_15to19km_8_Rozenfeld$counts + 
  p9_G_15to19km_9_Rozenfeld$counts + p10_G_15to19km_10_Rozenfeld$counts +
  p11_G_15to19km_11_Rozenfeld$counts + p12_G_15to19km_12_Rozenfeld$counts

b = seq(0,9.9,0.1)

G_from15to19km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(G_from15to19km_Rozenfeld) + 
  geom_bar(data=G_from15to19km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 15-19km Rozenfeld")


###########################
# 20-34km apart
###########################

G_20to34km_1_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_G_AF_MLGs)
G_20to34km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_G_AF_MLGs)

G_20to34km_2_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_G_BV_MLGs)
G_20to34km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_G_BV_MLGs)

G_20to34km_3_numalleles = asym_genet_dist_main_numalleles_new(input_G_SG_MLGs, input_Snack_MLGs)
G_20to34km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SG_MLGs, input_Snack_MLGs)

G_20to34km_4_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_G_AF_MLGs)
G_20to34km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_G_AF_MLGs)

G_20to34km_5_numalleles = asym_genet_dist_main_numalleles_new(input_G_GB_MLGs, input_G_BV_MLGs)
G_20to34km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_GB_MLGs, input_G_BV_MLGs)

G_20to34km_6_numalleles = asym_genet_dist_main_numalleles_new(input_G_AF_MLGs, input_G_BV_MLGs)
G_20to34km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AF_MLGs, input_G_BV_MLGs)

G_20to34km_7_numalleles = asym_genet_dist_main_numalleles_new(input_G_AF_MLGs, input_Torg_MLGs)
G_20to34km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AF_MLGs, input_Torg_MLGs)

G_20to34km_8_numalleles = asym_genet_dist_main_numalleles_new(input_G_AS_MLGs, input_Snack_MLGs)
G_20to34km_8_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AS_MLGs, input_Snack_MLGs)

G_20to34km_9_numalleles = asym_genet_dist_main_numalleles_new(input_G_AS_MLGs, input_Torg_MLGs)
G_20to34km_9_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AS_MLGs, input_Torg_MLGs)

G_20to34km_10_numalleles = asym_genet_dist_main_numalleles_new(input_G_BB_MLGs, input_Snack_MLGs)
G_20to34km_10_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BB_MLGs, input_Snack_MLGs)

G_20to34km_11_numalleles = asym_genet_dist_main_numalleles_new(input_G_BV_MLGs, input_Snack_MLGs)
G_20to34km_11_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BV_MLGs, input_Snack_MLGs)

G_20to34km_12_numalleles = asym_genet_dist_main_numalleles_new(input_G_BV_MLGs, input_Torg_MLGs)
G_20to34km_12_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_BV_MLGs, input_Torg_MLGs)

G_20to34km_13_numalleles = asym_genet_dist_main_numalleles_new(input_G_RX_MLGs, input_Snack_MLGs)
G_20to34km_13_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_RX_MLGs, input_Snack_MLGs)

G_20to34km_14_numalleles = asym_genet_dist_main_numalleles_new(input_G_RX_MLGs, input_Torg_MLGs)
G_20to34km_14_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_RX_MLGs, input_Torg_MLGs)

p1_G_20to34km_1_numalleles = hist(G_20to34km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_G_20to34km_1_numalleles")
p2_G_20to34km_2_numalleles = hist(G_20to34km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_G_20to34km_2_numalleles")
p3_G_20to34km_3_numalleles = hist(G_20to34km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_G_20to34km_3_numalleles")
p4_G_20to34km_4_numalleles = hist(G_20to34km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_G_20to34km_4_numalleles")
p5_G_20to34km_5_numalleles = hist(G_20to34km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_G_20to34km_5_numalleles")
p6_G_20to34km_6_numalleles = hist(G_20to34km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_G_20to34km_6_numalleles")
p7_G_20to34km_7_numalleles = hist(G_20to34km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_G_20to34km_7_numalleles")
p8_G_20to34km_8_numalleles = hist(G_20to34km_8_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p8_G_20to34km_8_numalleles")
p9_G_20to34km_9_numalleles = hist(G_20to34km_9_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p9_G_20to34km_9_numalleles")
p10_G_20to34km_10_numalleles = hist(G_20to34km_10_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p10_G_20to34km_10_numalleles")
p11_G_20to34km_11_numalleles = hist(G_20to34km_11_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p11_G_20to34km_11_numalleles")
p12_G_20to34km_12_numalleles = hist(G_20to34km_12_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p12_G_20to34km_12_numalleles")
p13_G_20to34km_13_numalleles = hist(G_20to34km_13_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p13_G_20to34km_13_numalleles")
p14_G_20to34km_14_numalleles = hist(G_20to34km_14_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p14_G_20to34km_14_numalleles")

p1_G_20to34km_1_Rozenfeld = hist(G_20to34km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_G_20to34km_1_Rozenfeld")
p2_G_20to34km_2_Rozenfeld = hist(G_20to34km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_G_20to34km_2_Rozenfeld")
p3_G_20to34km_3_Rozenfeld = hist(G_20to34km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_G_20to34km_3_Rozenfeld")
p4_G_20to34km_4_Rozenfeld = hist(G_20to34km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_G_20to34km_4_Rozenfeld")
p5_G_20to34km_5_Rozenfeld = hist(G_20to34km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_G_20to34km_5_Rozenfeld")
p6_G_20to34km_6_Rozenfeld = hist(G_20to34km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_G_20to34km_6_Rozenfeld")
p7_G_20to34km_7_Rozenfeld = hist(G_20to34km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_G_20to34km_7_Rozenfeld")
p8_G_20to34km_8_Rozenfeld = hist(G_20to34km_8_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p8_G_20to34km_8_Rozenfeld")
p9_G_20to34km_9_Rozenfeld = hist(G_20to34km_9_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p9_G_20to34km_9_Rozenfeld")
p10_G_20to34km_10_Rozenfeld = hist(G_20to34km_10_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p10_G_20to34km_10_Rozenfeld")
p11_G_20to34km_11_Rozenfeld = hist(G_20to34km_11_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p11_G_20to34km_11_Rozenfeld")
p12_G_20to34km_12_Rozenfeld = hist(G_20to34km_12_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p12_G_20to34km_12_Rozenfeld")
p13_G_20to34km_13_Rozenfeld = hist(G_20to34km_13_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p13_G_20to34km_13_Rozenfeld")
p14_G_20to34km_14_Rozenfeld = hist(G_20to34km_14_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p14_G_20to34km_14_Rozenfeld")

# Plot numalleles

a = p1_G_20to34km_1_numalleles$counts + p2_G_20to34km_2_numalleles$counts + 
  p3_G_20to34km_3_numalleles$counts + p4_G_20to34km_4_numalleles$counts + 
  p5_G_20to34km_5_numalleles$counts + p6_G_20to34km_6_numalleles$counts + 
  p7_G_20to34km_7_numalleles$counts + p8_G_20to34km_8_numalleles$counts +
  p9_G_20to34km_9_numalleles$counts +  p10_G_20to34km_10_numalleles$counts +
  p11_G_20to34km_11_numalleles$counts +  p12_G_20to34km_12_numalleles$counts +
  p13_G_20to34km_13_numalleles$counts +  p14_G_20to34km_14_numalleles$counts


b = seq(0,34,1)

G_from20to34km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(G_from20to34km_numalleles ) + 
  geom_bar(data=G_from20to34km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 20-34km numalleles")

# Plot Rozenfeld

a = p1_G_20to34km_1_Rozenfeld$counts + p2_G_20to34km_2_Rozenfeld$counts + 
  p3_G_20to34km_3_Rozenfeld$counts + p4_G_20to34km_4_Rozenfeld$counts + 
  p5_G_20to34km_5_Rozenfeld$counts + p6_G_20to34km_6_Rozenfeld$counts + 
  p7_G_20to34km_7_Rozenfeld$counts + p8_G_20to34km_8_Rozenfeld$counts +
  p9_G_20to34km_9_Rozenfeld$counts +  p10_G_20to34km_10_Rozenfeld$counts +
  p11_G_20to34km_11_Rozenfeld$counts +  p12_G_20to34km_12_Rozenfeld$counts +
  p13_G_20to34km_13_Rozenfeld$counts +  p14_G_20to34km_14_Rozenfeld$counts

b = seq(0,9.9,0.1)

G_from20to34km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(G_from20to34km_Rozenfeld) + 
  geom_bar(data=G_from20to34km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 20-34km Rozenfeld")


###########################
# 35-49km apart
###########################
G_35to50km_1_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_SG_MLGs)
G_35to50km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_SG_MLGs)

G_35to50km_2_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_GB_MLGs)
G_35to50km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_GB_MLGs)

G_35to50km_3_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_SG_MLGs)
G_35to50km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_SG_MLGs)

G_35to50km_4_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_AS_MLGs)
G_35to50km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_AS_MLGs)

G_35to50km_5_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_BB_MLGs)
G_35to50km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_BB_MLGs)

G_35to50km_6_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_RX_MLGs)
G_35to50km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_RX_MLGs)

G_35to50km_7_numalleles = asym_genet_dist_main_numalleles_new(input_G_AF_MLGs, input_Snack_MLGs)
G_35to50km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_AF_MLGs, input_Snack_MLGs)

p1_G_35to50km_1_numalleles = hist(G_35to50km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_G_35to50km_1_numalleles")
p2_G_35to50km_2_numalleles = hist(G_35to50km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_G_35to50km_2_numalleles")
p3_G_35to50km_3_numalleles = hist(G_35to50km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_G_35to50km_3_numalleles")
p4_G_35to50km_4_numalleles = hist(G_35to50km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_G_35to50km_4_numalleles")
p5_G_35to50km_5_numalleles = hist(G_35to50km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_G_35to50km_5_numalleles")
p6_G_35to50km_6_numalleles = hist(G_35to50km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_G_35to50km_6_numalleles")
p7_G_35to50km_7_numalleles = hist(G_35to50km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_G_35to50km_7_numalleles")

p1_G_35to50km_1_Rozenfeld = hist(G_35to50km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_G_35to50km_1_Rozenfeld")
p2_G_35to50km_2_Rozenfeld = hist(G_35to50km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_G_35to50km_2_Rozenfeld")
p3_G_35to50km_3_Rozenfeld = hist(G_35to50km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_G_35to50km_3_Rozenfeld")
p4_G_35to50km_4_Rozenfeld = hist(G_35to50km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_G_35to50km_4_Rozenfeld")
p5_G_35to50km_5_Rozenfeld = hist(G_35to50km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_G_35to50km_5_Rozenfeld")
p6_G_35to50km_6_Rozenfeld = hist(G_35to50km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_G_35to50km_6_Rozenfeld")
p7_G_35to50km_7_Rozenfeld = hist(G_35to50km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_G_35to50km_7_Rozenfeld")

# Plot numalleles

a = p1_G_35to50km_1_numalleles$counts + p2_G_35to50km_2_numalleles$counts + 
  p3_G_35to50km_3_numalleles$counts + p4_G_35to50km_4_numalleles$counts + 
  p5_G_35to50km_5_numalleles$counts + p6_G_35to50km_6_numalleles$counts + 
  p7_G_35to50km_7_numalleles$counts


b = seq(0,34,1)

G_from35to50km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(G_from35to50km_numalleles ) + 
  geom_bar(data=G_from35to50km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 35-50km numalleles")

# Plot Rozenfeld

a = p1_G_35to50km_1_Rozenfeld$counts + p2_G_35to50km_2_Rozenfeld$counts + 
  p3_G_35to50km_3_Rozenfeld$counts + p4_G_35to50km_4_Rozenfeld$counts + 
  p5_G_35to50km_5_Rozenfeld$counts + p6_G_35to50km_6_Rozenfeld$counts + 
  p7_G_35to50km_7_Rozenfeld$counts 

b = seq(0,9.9,0.1)

G_from35to50km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(G_from35to50km_Rozenfeld) + 
  geom_bar(data=G_from35to50km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS G 35-50km Rozenfeld")


###########################
# >50km apart
###########################

G_more_than_50km_1_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_AF_MLGs)
G_more_than_50km_1_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_AF_MLGs)

G_more_than_50km_2_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_AS_MLGs)
G_more_than_50km_2_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_AS_MLGs)

G_more_than_50km_3_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_BB_MLGs)
G_more_than_50km_3_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_BB_MLGs)

G_more_than_50km_4_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_BV_MLGs)
G_more_than_50km_4_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_BV_MLGs)

G_more_than_50km_5_numalleles = asym_genet_dist_main_numalleles_new(input_G_SK_MLGs, input_G_RX_MLGs)
G_more_than_50km_5_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_SK_MLGs, input_G_RX_MLGs)

G_more_than_50km_6_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_AF_MLGs)
G_more_than_50km_6_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_AF_MLGs)

G_more_than_50km_7_numalleles = asym_genet_dist_main_numalleles_new(input_G_NB_MLGs, input_G_BV_MLGs)
G_more_than_50km_7_Rozenfeld = asym_genet_dist_main_Rozenfeld(input_G_NB_MLGs, input_G_BV_MLGs)

p1_G_more_than_50km_1_numalleles = hist(G_more_than_50km_1_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1), main="p1_G_more_than_50km_1_numalleles")
p2_G_more_than_50km_2_numalleles = hist(G_more_than_50km_2_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p2_G_more_than_50km_2_numalleles")
p3_G_more_than_50km_3_numalleles = hist(G_more_than_50km_3_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p3_G_more_than_50km_3_numalleles")
p4_G_more_than_50km_4_numalleles = hist(G_more_than_50km_4_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p4_G_more_than_50km_4_numalleles")
p5_G_more_than_50km_5_numalleles = hist(G_more_than_50km_5_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p5_G_more_than_50km_5_numalleles")
p6_G_more_than_50km_6_numalleles = hist(G_more_than_50km_6_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p6_G_more_than_50km_6_numalleles")
p7_G_more_than_50km_7_numalleles = hist(G_more_than_50km_7_numalleles, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,35,1),main="p7_G_more_than_50km_7_numalleles")

p1_G_more_than_50km_1_Rozenfeld = hist(G_more_than_50km_1_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p1_G_more_than_50km_1_Rozenfeld")
p2_G_more_than_50km_2_Rozenfeld = hist(G_more_than_50km_2_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p2_G_more_than_50km_2_Rozenfeld")
p3_G_more_than_50km_3_Rozenfeld = hist(G_more_than_50km_3_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1), main="p3_G_more_than_50km_3_Rozenfeld")
p4_G_more_than_50km_4_Rozenfeld = hist(G_more_than_50km_4_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p4_G_more_than_50km_4_Rozenfeld")
p5_G_more_than_50km_5_Rozenfeld = hist(G_more_than_50km_5_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p5_G_more_than_50km_5_Rozenfeld")
p6_G_more_than_50km_6_Rozenfeld = hist(G_more_than_50km_6_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p6_G_more_than_50km_6_Rozenfeld")
p7_G_more_than_50km_7_Rozenfeld = hist(G_more_than_50km_7_Rozenfeld, freq=TRUE, col=rgb(0,0.4,1,1), breaks=seq(0,10,0.1),main="p7_G_more_than_50km_7_Rozenfeld")


# Plot numalleles

a = p1_G_more_than_50km_1_numalleles$counts + p2_G_more_than_50km_2_numalleles$counts + 
  p3_G_more_than_50km_3_numalleles$counts + p4_G_more_than_50km_4_numalleles$counts + 
  p5_G_more_than_50km_5_numalleles$counts + p6_G_more_than_50km_6_numalleles$counts + 
  p7_G_more_than_50km_7_numalleles$counts

b = seq(0,34,1)

G_more_than_50km_numalleles = data.frame(counts=a,genet_dist=b)

ggplot(G_more_than_50km_numalleles ) + 
  geom_bar(data=G_more_than_50km_numalleles , stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS >50km numalleles")

# Plot Rozenfeld

a = p1_G_more_than_50km_1_Rozenfeld$counts + p2_G_more_than_50km_2_Rozenfeld$counts + 
  p3_G_more_than_50km_3_Rozenfeld$counts + p4_G_more_than_50km_4_Rozenfeld$counts + 
  p5_G_more_than_50km_5_Rozenfeld$counts + p6_G_more_than_50km_6_Rozenfeld$counts + 
  p7_G_more_than_50km_7_Rozenfeld$counts

b = seq(0,9.9,0.1)

G_more_than_50km_Rozenfeld = data.frame(counts=a,genet_dist=b)

ggplot(G_more_than_50km_Rozenfeld) + 
  geom_bar(data=G_more_than_50km_Rozenfeld, stat="identity", aes(x=genet_dist, y=counts)) +
  ggtitle("GDS >50km Rozenfeld")




######################################################
# Make new categories:
######################################################

a1 = p1_G_5to9km_1_numalleles$counts + p2_G_5to9km_2_numalleles$counts + 
  p3_G_5to9km_3_numalleles$counts + p4_G_5to9km_4_numalleles$counts + 
  p5_G_5to9km_5_numalleles$counts + p6_G_5to9km_6_numalleles$counts +
  p1_G_10to14km_1_numalleles$counts + p2_G_10to14km_2_numalleles$counts + 
  p3_G_10to14km_3_numalleles$counts + p4_G_10to14km_4_numalleles$counts + 
  p5_G_10to14km_5_numalleles$counts + p6_G_10to14km_6_numalleles$counts + 
  p7_G_10to14km_7_numalleles$counts + p8_G_10to14km_8_numalleles$counts +
  p1_G_15to19km_1_numalleles$counts + p2_G_15to19km_2_numalleles$counts + 
  p3_G_15to19km_3_numalleles$counts + p4_G_15to19km_4_numalleles$counts + 
  p5_G_15to19km_5_numalleles$counts + p6_G_15to19km_6_numalleles$counts + 
  p7_G_15to19km_7_numalleles$counts + p8_G_15to19km_8_numalleles$counts + 
  p9_G_15to19km_9_numalleles$counts + p10_G_15to19km_10_numalleles$counts +
  p11_G_15to19km_11_numalleles$counts + p12_G_15to19km_12_numalleles$counts

b = seq(0,34,1)

G_from5to19km_numalleles = data.frame(counts=a1, genet_dist=b)


a2 = p1_G_5to9km_1_Rozenfeld$counts + p2_G_5to9km_2_Rozenfeld$counts + 
  p3_G_5to9km_3_Rozenfeld$counts + p4_G_5to9km_4_Rozenfeld$counts + 
  p5_G_5to9km_5_Rozenfeld$counts + p6_G_5to9km_6_Rozenfeld$counts +
  p1_G_10to14km_1_Rozenfeld$counts + p2_G_10to14km_2_Rozenfeld$counts + 
  p3_G_10to14km_3_Rozenfeld$counts + p4_G_10to14km_4_Rozenfeld$counts + 
  p5_G_10to14km_5_Rozenfeld$counts + p6_G_10to14km_6_Rozenfeld$counts + 
  p7_G_10to14km_7_Rozenfeld$counts + p8_G_10to14km_8_Rozenfeld$counts +
  p1_G_15to19km_1_Rozenfeld$counts + p2_G_15to19km_2_Rozenfeld$counts + 
  p3_G_15to19km_3_Rozenfeld$counts + p4_G_15to19km_4_Rozenfeld$counts + 
  p5_G_15to19km_5_Rozenfeld$counts + p6_G_15to19km_6_Rozenfeld$counts + 
  p7_G_15to19km_7_Rozenfeld$counts + p8_G_15to19km_8_Rozenfeld$counts + 
  p9_G_15to19km_9_Rozenfeld$counts + p10_G_15to19km_10_Rozenfeld$counts +
  p11_G_15to19km_11_Rozenfeld$counts + p12_G_15to19km_12_Rozenfeld$counts

b = seq(0,9.9,0.1)

G_from5to19km_Rozenfeld = data.frame(counts=a2, genet_dist=b)



a1 = p1_G_20to34km_1_numalleles$counts + p2_G_20to34km_2_numalleles$counts + 
  p3_G_20to34km_3_numalleles$counts + p4_G_20to34km_4_numalleles$counts + 
  p5_G_20to34km_5_numalleles$counts + p6_G_20to34km_6_numalleles$counts + 
  p7_G_20to34km_7_numalleles$counts + p8_G_20to34km_8_numalleles$counts +
  p9_G_20to34km_9_numalleles$counts +  p10_G_20to34km_10_numalleles$counts +
  p11_G_20to34km_11_numalleles$counts +  p12_G_20to34km_12_numalleles$counts +
  p13_G_20to34km_13_numalleles$counts +  p14_G_20to34km_14_numalleles$counts +
  p1_G_35to50km_1_numalleles$counts + p2_G_35to50km_2_numalleles$counts + 
  p3_G_35to50km_3_numalleles$counts + p4_G_35to50km_4_numalleles$counts + 
  p5_G_35to50km_5_numalleles$counts + p6_G_35to50km_6_numalleles$counts + 
  p7_G_35to50km_7_numalleles$counts

b = seq(0,34,1)

G_from20to49km_numalleles = data.frame(counts=a1, genet_dist=b)

a2 = p1_G_20to34km_1_Rozenfeld$counts + p2_G_20to34km_2_Rozenfeld$counts + 
  p3_G_20to34km_3_Rozenfeld$counts + p4_G_20to34km_4_Rozenfeld$counts + 
  p5_G_20to34km_5_Rozenfeld$counts + p6_G_20to34km_6_Rozenfeld$counts + 
  p7_G_20to34km_7_Rozenfeld$counts + p8_G_20to34km_8_Rozenfeld$counts +
  p9_G_20to34km_9_Rozenfeld$counts +  p10_G_20to34km_10_Rozenfeld$counts +
  p11_G_20to34km_11_Rozenfeld$counts +  p12_G_20to34km_12_Rozenfeld$counts +
  p13_G_20to34km_13_Rozenfeld$counts +  p14_G_20to34km_14_Rozenfeld$counts +
  p1_G_35to50km_1_Rozenfeld$counts + p2_G_35to50km_2_Rozenfeld$counts + 
  p3_G_35to50km_3_Rozenfeld$counts + p4_G_35to50km_4_Rozenfeld$counts + 
  p5_G_35to50km_5_Rozenfeld$counts + p6_G_35to50km_6_Rozenfeld$counts + 
  p7_G_35to50km_7_Rozenfeld$counts

b = seq(0,9.9,0.1)

G_from20to49km_Rozenfeld = data.frame(counts=a2, genet_dist=b)







######################################################
# Plot all G together (with new categories)
######################################################

G_less_than_5km_numalleles$classif = "<5km"
G_less_than_5km_Rozenfeld$classif = "<5km"
G_from5to19km_numalleles$classif = "5-19km"
G_from5to19km_Rozenfeld$classif = "5-19km"
G_from20to49km_numalleles$classif = "20-50km"
G_from20to49km_Rozenfeld$classif = "20-50km"
G_more_than_50km_numalleles$classif = ">50km"
G_more_than_50km_Rozenfeld$classif = ">50km"

G_less_than_5km_numalleles$density = (G_less_than_5km_numalleles$counts)/(sum(G_less_than_5km_numalleles$counts))
G_less_than_5km_Rozenfeld$density = (G_less_than_5km_Rozenfeld$counts)/(sum(G_less_than_5km_Rozenfeld$counts))
G_from5to19km_numalleles$density = (G_from5to19km_numalleles$counts)/(sum(G_from5to19km_numalleles$counts))
G_from5to19km_Rozenfeld$density = (G_from5to19km_Rozenfeld$counts)/(sum(G_from5to19km_Rozenfeld$counts))
G_from20to49km_numalleles$density = (G_from20to49km_numalleles$counts)/(sum(G_from20to49km_numalleles$counts))
G_from20to49km_Rozenfeld$density = (G_from20to49km_Rozenfeld$counts)/(sum(G_from20to49km_Rozenfeld$counts))
G_more_than_50km_numalleles$density = (G_more_than_50km_numalleles$counts)/(sum(G_more_than_50km_numalleles$counts))
G_more_than_50km_Rozenfeld$density = (G_more_than_50km_Rozenfeld$counts)/(sum(G_more_than_50km_Rozenfeld$counts))



alltogether_G_numalleles = rbind(G_less_than_5km_numalleles, G_from5to19km_numalleles, 
                                 G_from20to49km_numalleles, 
                                 G_more_than_50km_numalleles)

alltogether_G_Rozenfeld = rbind(G_less_than_5km_Rozenfeld, G_from5to19km_Rozenfeld, 
                                G_from20to49km_Rozenfeld, 
                                G_more_than_50km_Rozenfeld)


alltogether_G_numalleles$classif = factor(alltogether_G_numalleles$classif, 
                                          levels=c("<5km", "5-19km", "20-50km", ">50km"))

alltogether_G_Rozenfeld$classif = factor(alltogether_G_Rozenfeld$classif, 
                                          levels=c("<5km", "5-19km", "20-50km", ">50km"))


#Plot numalleles
jpeg(filename = "~/Documents/GRONINGEN/TOP PROGRAMME EVOLUTIONARY BIOLOGY/Master project ZOSTERA/Data Analysis/GDS (RClone)/GDS_among_meadows/GDS_bygeographic_dist/Graphs/G_numalleles_MLGs_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", res=600)

ggplot(alltogether_G_numalleles, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nShared Alleles' Distance", y="Frequency\n", col="Distance classes") +
  #ggtitle("Gullmarsfjord GDS by geographical distance - Shared Alleles' Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15), 
        legend.key=element_blank(), 
        legend.position="none") + 
  scale_colour_manual(values=c("lightgoldenrod", "coral", "coral3", "coral4"))
  

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()

#Plot Rozenfeld
jpeg(filename = "~/Documents/GRONINGEN/TOP PROGRAMME EVOLUTIONARY BIOLOGY/Master project ZOSTERA/Data Analysis/GDS (RClone)/GDS_among_meadows/GDS_bygeographic_dist/Graphs/G_Rozenfeld_MLGs_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", res=600)

ggplot(alltogether_G_Rozenfeld, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nRozenfeld's Distance", y="Frequency\n", col="Distance classes") +
  #ggtitle("Gullmarsfjord GDS by geographical distance - Rozenfeld's Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15), 
        legend.key=element_blank(), 
        legend.position="none") + 
  scale_colour_manual(values=c("lightgoldenrod", "coral", "coral3", "coral4"))

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()






# B-W and pdf for article:
pdf(filename = "~/Documents/GRONINGEN/TOP PROGRAMME EVOLUTIONARY BIOLOGY/Master project ZOSTERA/Data Analysis/GDS (RClone)/GDS_among_meadows/GDS_bygeographic_dist/Graphs/G_numalleles_MLGs_paper.jpeg", 
     width=20, height=12, units="cm", pointsize=12, bg="white", res=600)

ggplot(alltogether_G_numalleles, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nShared Alleles' Distance", y="Density\n", col="Distance classes") +
  ggtitle("Gullmarsfjord GDS by geographical distance - Shared Alleles' Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15)) + 
  scale_colour_manual(values=c("gray85", "gray66", "gray47","gray8"))


dev.off()
dev.list()
dev.set(which = 2)
dev.cur()

pdf(filename = "~/Documents/GRONINGEN/TOP PROGRAMME EVOLUTIONARY BIOLOGY/Master project ZOSTERA/Data Analysis/GDS (RClone)/GDS_among_meadows/GDS_bygeographic_dist/Graphs/G_Rozenfeld_MLGs_paper.jpeg", 
     width=20, height=12, units="cm", pointsize=12, bg="white", res=600)

ggplot(alltogether_G_Rozenfeld, aes(x=genet_dist, y=density,  col=classif)) + 
  geom_line(size=1) + 
  labs(x="\nRozenfeld's Distance", y="Frequency\n", col="Distance classes") +
  ggtitle("Gullmarsfjord GDS by geographical distance - Rozenfeld's Distance") +
  theme(panel.background = element_rect(fill = 'white', colour='black'), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.title=element_text(size=15)) + 
  scale_colour_manual(values=c("gray85", "gray66", "gray47","gray8"))

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()
