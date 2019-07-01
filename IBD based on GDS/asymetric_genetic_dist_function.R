# Author: Núria Serra Serra
# Date: April 2017


########################################################################################################

##### Function ASYMMETRIC genetic distance computation

# It needs two dfs as input, because it creates an asymmetric genetic distance matrix with pop X as rows
# and population Y as columns, and the matrix is full (not half, because asymmetric)

########################################################################################################




#####################
## Based on Rozenfeld:
#####################

asym_genet_dist_main_Rozenfeld = function(pop1, pop2){
  num_indiv_pop1  = nrow(pop1)
  num_indiv_pop2 = nrow(pop2)
  mat_dist = matrix(NA, ncol=num_indiv_pop1, nrow=num_indiv_pop2) 
  colnames(mat_dist) = 1:num_indiv_pop1
  rownames(mat_dist) = 1:num_indiv_pop2
  
  index_loci <- 1:c(ncol(pop1)/2)*2-1
  num_loci <- ncol(pop1)/2
  
  for (j in 1:num_indiv_pop1){
    for (i in 1:num_indiv_pop2){ #Satarting from 1 instead of j, because we want the full matrix, not half
      ind1 = as.numeric(pop1[j,])
      ind2 = as.numeric(pop2[i,])
      recup = NULL
      
      for (k in index_loci){
        recup = c(recup, min(abs(ind1[k]-ind2[k])+abs(ind1[k+1]-ind2[k+1]),
                             abs(ind1[k]-ind2[k+1])  + abs(ind1[k+1] - ind2[k])))
      }
      
      mat_dist[i,j] = sum(recup)/num_loci
    }
  }
  
  mat_dist_all = mat_dist
  mat_dist_all
  
}

function_asymetric_genetic_distance_Rozenfeld = function(pop1, pop2){
    result = list(NULL)
    result <- asym_genet_dist_main_Rozenfeld(pop1, pop2)
  
  result 
  
}

#####################
## Based on num alleles:
#####################

asym_genet_dist_main_numalleles = function(pop1, pop2){
  num_indiv_pop1  = nrow(pop1)
  num_indiv_pop2 = nrow(pop2)
  mat_dist = matrix(NA, ncol=num_indiv_pop1, nrow=num_indiv_pop2) 
  colnames(mat_dist) = 1:num_indiv_pop1
  rownames(mat_dist) = 1:num_indiv_pop2
  
  index_loci <- 1:c(ncol(pop1)/2)*2-1
  num_loci <- ncol(pop1)/2

  for (j in 1:num_indiv_pop1){
    for (i in 1:num_indiv_pop2){
      mat_dist[i,j] = length(which(pop2[i,] != pop1[j,]))#The distance between indiv is the number of different alleles
    }
  }
  
  mat_dist_all = mat_dist
  mat_dist_all
  
}

