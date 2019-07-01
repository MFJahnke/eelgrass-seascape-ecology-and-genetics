# Author: Núria Serra Serra
# Date: May 2017


############################################################################################################

## GENETIC DISTANCE FUNCTIONS (2, one for the Rozenfeld distance and the other one with the SAD)

## For each of them, there can be a vecpop (-> genetic distance is calculated within meadow) or without vecpop 
## (pairwise genetic distances among all individuals)

############################################################################################################


#############
##### Function genetic distance computation based on ROZENFELD:
#############

genet_dist_main_Rozenfeld = function(df){
  num_rows  = nrow(df)
  mat_dist = matrix(NA, ncol=num_rows, nrow=num_rows) 
  colnames(mat_dist) = 1:num_rows
  rownames(mat_dist) = 1:num_rows
  
  index_loci <- 1:c(ncol(df)/2)*2-1
  num_loci <- ncol(df)/2
  
  for (j in 1:num_rows){
    for (i in j:num_rows){
      ind1 = as.numeric(df[i,])
      ind2 = as.numeric(df[j,])
      recup = NULL
      
      for (k in index_loci){
        recup = c(recup, min(abs(ind1[k]-ind2[k])+abs(ind1[k+1]-ind2[k+1]),
                             abs(ind1[k]-ind2[k+1])  + abs(ind1[k+1] - ind2[k])))
      }
      
      mat_dist[i,j] = sum(recup)/num_loci
    }
  }
  
  diag(mat_dist) = NA       #Remove 0s in the diagonal
  mat_dist_all = mat_dist
  mat_dist_all
  
}

function_genetic_distance_Rozenfeld = function(df, popvect=NULL){
  if (length(popvect) !=0 ){
    if (length(popvect) != nrow(df)) {stop("Error: popvect length is not equal to the number of rows in your dataset.")}
    
    data_split = split(df, popvect)
    
    result = list(NULL)
    
    for (pop in 1:length(unique(popvect))){
      rownames(data_split[[pop]]) <- 1:nrow(data_split[[pop]])
      result[[pop]] <- genet_dist_main_Rozenfeld(data_split[[pop]])
      
    }
    names(result) = names(data_split)
    
  } else {
    result = list(NULL)
    result <- genet_dist_main_Rozenfeld(df)
  }
  
  result 
  
}








#############
##### Function genetic distance computation based on NUMBER OF ALLELES:
#############

genetic_dist_between2ind_SAD = function(ind1, ind2){
  
  total_number_alleles = length(ind1) # = as number of columns
  index_loci = seq(1,total_number_alleles, by=2) #Index for position of each locus
  
  num_shared_alleles = 0
  num_different_alleles = 0
  
  for (i in index_loci){
    
    vector_locus_indiv_1 = c(ind1[i], ind1[i+1])
    vector_locus_indiv_2 = c(ind2[i], ind2[i+1])
    
    if(setequal(vector_locus_indiv_1,vector_locus_indiv_2)){ 
      num_shared_alleles = num_shared_alleles + 2   #If the 2 vectors are equal they share the 2 alleles
    } else {  #If they are not the same calculate if they have one in common or none, and add it to the vector
      num_shared_alleles = num_shared_alleles + length(intersect(vector_locus_indiv_1,vector_locus_indiv_2))
    }
    
    num_shared_alleles # Number of shared alleles between the 2 individuals over ALL LOCI
    
  }
  
  num_shared_alleles
  num_different_alleles = total_number_alleles - num_shared_alleles
  num_different_alleles
  
}


genet_dist_main_numalleles = function(df){
  num_rows  = nrow(df)
  mat_dist = matrix(NA, ncol=num_rows, nrow=num_rows) 
  colnames(mat_dist) = 1:num_rows
  rownames(mat_dist) = 1:num_rows
  
  for (j in 1:num_rows){
    for (i in j:num_rows){
      ind1 = df[i,]
      ind2 = df[j,]
      mat_dist[i,j] = genetic_dist_between2ind_SAD(ind1,ind2)
    }
  }
  
  diag(mat_dist) = NA     #Remove 0s in the diagonal
  mat_dist_all = mat_dist
  mat_dist_all
  
}

function_genetic_distance_numalleles_new = function(df, popvect=NULL){
  if (length(popvect) !=0 ){
    if (length(popvect) != nrow(df)) {stop("Error: popvect length is not equal to the number of rows in your dataset.")}
    
    data_split = split(df, popvect)
    
    result = list(NULL)
    
    for (pop in 1:length(unique(popvect))){
      rownames(data_split[[pop]]) <- 1:nrow(data_split[[pop]])
      par(ask = TRUE)
      result[[pop]] <- genet_dist_main_numalleles(data_split[[pop]])
    }
    names(result) = names(data_split)
    
  } else {
    result = list(NULL)
    result <- genet_dist_main_numalleles(df)
  }
  
  result 
  
}

