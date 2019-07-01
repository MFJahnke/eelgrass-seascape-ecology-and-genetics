# Author: Núria Serra Serra
# Date: June 2017

############################################################################################################

# PLOTTING "Averaged_G&K_Roz_report.jpeg" and "Averaged_G&K_SAD_report.jpeg", FIG. S6, AVERAGED WITHIN-MEADOW GDS OVER ALL SITES 
# OF GULLMARSFJORD AND KUNGÄLV

############################################################################################################


##################################################
#####   Open dfs:
##################################################

#### Gullmarsfjord - SAD:
df_average_GDS_G_SAD = read_table("avg_GDS_Gullmars_SAD.csv", 
          row.names=F)

#### Gullmarsfjord - Rozenfeld:
df_average_GDS_G_Roz = read_table("avg_GDS_Gullmars_Rozenfeld.csv", 
          row.names=F)

#### Marstrand - SAD:
df_average_GDS_K_SAD = read_table("avg_GDS_Mars_SAD.csv", 
          row.names=F)


#### Marstrand - Rozenfeld:
df_average_GDS_K_Roz = read_table("avg_GDS_Mars_Rozenfeld.csv", 
          row.names=F)
		  
######################################################################################################################################################

# Plot Average GDS for K and G together:

names(df_average_GDS_G_SAD)[1] = "genetic_distance"
df_average_GDS_G_SAD[,2] = df_average_GDS_G_SAD[,2]/10
df_average_GDS_K_SAD[,2] = df_average_GDS_K_SAD[,2]/10

names(df_average_GDS_G_Roz)[1] = "genetic_distance"
df_average_GDS_G_Roz[,2] = df_average_GDS_G_Roz[,2]/10
df_average_GDS_K_Roz[,2] = df_average_GDS_K_Roz[,2]/10

# SAD:
jpeg(filename = "Averaged_G&M_SAD_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", res=600)


ggplot() + 
  geom_line(data=df_average_GDS_G_SAD, size=1.2, aes(x=genetic_distance, y=Frequency), col="coral2") +
  geom_line(data=df_average_GDS_K_SAD, size=1.2, aes(x=genetic_distance, y=Frequency), col="cyan3") +
  labs(x="\nShared Alleles' Distance", y ="Frequency") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13),
                     plot.title = element_text(size=14))

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()


# SAD:
jpeg(filename = "Averaged_G&M_Roz_report.jpeg", 
     width=15, height=12, units="cm", pointsize=12, bg="white", res=600)


ggplot() + 
  geom_line(data=df_average_GDS_G_Roz, size=1.2, aes(x=genetic_distance, y=Frequency), col="coral2") +
  geom_line(data=df_average_GDS_K_Roz, size=1.2, aes(x=genetic_distance, y=Frequency), col="cyan3") +
  labs(x="\nRozenfeld's Distance", y ="Frequency") +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13),
                     plot.title = element_text(size=14))

dev.off()
dev.list()
dev.set(which = 2)
dev.cur()
