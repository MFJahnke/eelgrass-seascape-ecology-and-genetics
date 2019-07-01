# Author: Núria Serra Serra
# Date: April 2017


############################################################################################################

# FUNCTIONS TO SIMULATE SELFING, OUTCROSSING AND RANDOM MATING (and applying them separately per meadow):

# Before running the script "GDS_with_simulated_self_outcr_randmat.R"

############################################################################################################


# Import data:

G_withclones = read_table("Gullm_withclones.csv")

K_withclones = read_table("Mars_withclones.csv")


###########
### Create files for each meadow separately WITH CLONES:(because to simulate selfing and outcrossing 
#we use the allelic frequencies of each meadow, WITH CLONES - To be more accurate to the reality):
###########
G_AF_withclones = G_withclones[G_withclones$population == "G-AF",]
G_AS_withclones = G_withclones[G_withclones$population == "G-AS",]
G_BB_withclones = G_withclones[G_withclones$population == "G-BB",]
G_BV_withclones = G_withclones[G_withclones$population == "G-BV",]
G_GB_withclones = G_withclones[G_withclones$population == "G-GB",]
G_NB_withclones = G_withclones[G_withclones$population == "G-NB",]
G_RX_withclones = G_withclones[G_withclones$population == "G-RX",]
G_SG_withclones = G_withclones[G_withclones$population == "G-SG",]
G_SK_withclones = G_withclones[G_withclones$population == "G-SK",]
Snack_withclones = G_withclones[G_withclones$population == "Snackebackebukten",]
Torg_withclones = G_withclones[G_withclones$population == "Torgestad",]

K_BK_withclones = K_withclones[K_withclones$population == "K-BK",]
K_K_withclones = K_withclones[K_withclones$population == "K-K",]
K_KR_withclones = K_withclones[K_withclones$population == "K-KR",]
K_LD_withclones = K_withclones[K_withclones$population == "K-LD",]
K_NG_withclones = K_withclones[K_withclones$population == "K-NG",]
K_NI_withclones = K_withclones[K_withclones$population == "K-NI",]
K_OK_withclones = K_withclones[K_withclones$population == "K-OK",]
K_ON_withclones = K_withclones[K_withclones$population == "K-ON",]
K_RT_withclones = K_withclones[K_withclones$population == "K-RT",]
K_SK_withclones = K_withclones[K_withclones$population == "K-SK",]
K_SO_withclones = K_withclones[K_withclones$population == "K-SO",]

##########
# Calculate allelic frequencies (per meadow):
##########

G_AF_withclones_freq = frequencies(G_AF_withclones)
G_AS_withclones_freq = frequencies(G_AS_withclones)
G_BB_withclones_freq = frequencies(G_BB_withclones)
G_BV_withclones_freq = frequencies(G_BV_withclones)
G_GB_withclones_freq = frequencies(G_GB_withclones)
G_NB_withclones_freq = frequencies(G_NB_withclones)
G_RX_withclones_freq = frequencies(G_RX_withclones)
G_SG_withclones_freq = frequencies(G_SG_withclones)
G_SK_withclones_freq = frequencies(G_SK_withclones)
Snack_withclones_freq = frequencies(Snack_withclones)
Torg_withclones_freq = frequencies(Torg_withclones)

K_BK_withclones_freq = frequencies(K_BK_withclones)
K_K_withclones_freq = frequencies(K_K_withclones)
K_KR_withclones_freq = frequencies(K_KR_withclones)
K_LD_withclones_freq = frequencies(K_LD_withclones)
K_NG_withclones_freq = frequencies(K_NG_withclones)
K_NI_withclones_freq = frequencies(K_NI_withclones)
K_OK_withclones_freq = frequencies(K_OK_withclones)
K_ON_withclones_freq = frequencies(K_ON_withclones)
K_RT_withclones_freq = frequencies(K_RT_withclones)
K_SK_withclones_freq = frequencies(K_SK_withclones)
K_SO_withclones_freq = frequencies(K_SO_withclones)


########################
## 1) Create a population of 100 individuals per meadow from the real allelic frequencies of the meadow:
########################

newpop_G_AF = make_population(G_AF_withclones_freq, N=100) ###########
newpop_G_AS = make_population(G_AS_withclones_freq, N=100)
newpop_G_BB = make_population(G_BB_withclones_freq, N=100)
newpop_G_BV = make_population(G_BV_withclones_freq, N=100) ############
newpop_G_GB = make_population(G_GB_withclones_freq, N=100) ############
newpop_G_NB = make_population(G_NB_withclones_freq, N=100)
newpop_G_RX = make_population(G_RX_withclones_freq, N=100)
newpop_G_SG = make_population(G_SG_withclones_freq, N=100)
newpop_G_SK = make_population(G_SK_withclones_freq, N=100)
newpop_Snack = make_population(Snack_withclones_freq, N=100)
newpop_Torg = make_population(Torg_withclones_freq, N=100)

newpop_K_BK = make_population(K_BK_withclones_freq, N=100)
newpop_K_K = make_population(K_K_withclones_freq, N=100)
newpop_K_KR = make_population(K_KR_withclones_freq, N=100)
newpop_K_LD = make_population(K_LD_withclones_freq, N=100)
newpop_K_NG = make_population(K_NG_withclones_freq, N=100)
newpop_K_NI = make_population(K_NI_withclones_freq, N=100)
newpop_K_OK = make_population(K_OK_withclones_freq, N=100)
newpop_K_ON = make_population(K_ON_withclones_freq, N=100)
newpop_K_RT = make_population(K_RT_withclones_freq, N=100) #######
newpop_K_SK = make_population(K_SK_withclones_freq, N=100) #######
newpop_K_SO = make_population(K_SO_withclones_freq, N=100) #######

#Remove ID column from the newpops:
newpop_G_AF = newpop_G_AF[,-1]
newpop_G_AS = newpop_G_AS[,-1]
newpop_G_BB = newpop_G_BB[,-1]
newpop_G_BV = newpop_G_BV[,-1]
newpop_G_GB = newpop_G_GB[,-1]
newpop_G_NB = newpop_G_NB[,-1]
newpop_G_RX = newpop_G_RX[,-1]
newpop_G_SG = newpop_G_SG[,-1]
newpop_G_SK = newpop_G_SK[,-1]
newpop_Snack = newpop_Snack[,-1]
newpop_Torg = newpop_Torg[,-1]

newpop_K_BK = newpop_K_BK[,-1]
newpop_K_K = newpop_K_K[,-1]
newpop_K_KR = newpop_K_KR[,-1]
newpop_K_LD = newpop_K_LD[,-1]
newpop_K_NG = newpop_K_NG[,-1]
newpop_K_NI = newpop_K_NI[,-1]
newpop_K_OK = newpop_K_OK[,-1]
newpop_K_ON = newpop_K_ON[,-1]
newpop_K_RT = newpop_K_RT[,-1]
newpop_K_SK = newpop_K_SK[,-1]
newpop_K_SO = newpop_K_SO[,-1]


########################
## 2) Create functions to simulate selfing and outcrossing (within population/meadow):
########################


# Function to simulate SELFING within meadow:

selfing_within <- function(pop) {
  nrows <- dim(pop)[1]  # nrows
  ret <- pop  # Make a new dataframe with the same dimensions as pop
  for (i in 1:nrows) {
    # choose 1 row index at random to be the parent (selfing)
    parent <- sample(1:nrows, size = 1) 
    # use the same row index (individual) from the initial population as both mom and dad to make 1 selfed offspring
    off <- mate(pop[parent, ], pop[parent, ], N = 1)
    # add the genotype back to the population.
    ret[i,] <- off
  }
  return(ret)
}

#Function for simulating mating between two different ramets within the meadow, which can be either
# OUTCROSSING (if they have different genotypes) or SELFING (if they are actually the same genotype):

random_mating_within <- function(pop) {
  nrows <- dim(pop)[1]  # nrows
  ret <- pop  # Make a new dataframe with the same dimensions as pop
  for (i in 1:nrows) {
    # grab 2 different random parent indices (we take them without replacement, but they can still be
    #the same genotype, which would lead to selfing - we don't know it, it's at random)
    parents <- sample(1:nrows, size = 2, replace = FALSE)
    # mate them to make 1 offspring 
    off <- mate(pop[parents[1], ], pop[parents[2], ], N = 1)
    # add the genotype back to the population.
    ret[i,] <- off
  }
  return(ret)
}

# Function to simulate only OUTCROSSING
outcrossing_within <- function(pop) {
  nrows <- dim(pop)[1]  # nrows
  ret <- pop  # Make a new dataframe with the same dimensions as pop
  for (i in 1:nrows) {
    # grab 2 different random parent indices (without replacement)
    parents <- sample(1:nrows, size = 2, replace = FALSE)
    
    while(parents[1] == parents[2]) {
      parents <- sample(1:nrows, size = 2, replace = FALSE)
    }
    
    # mate them to make 1 offspring 
    off <- mate(pop[parents[1], ], pop[parents[2], ], N = 1)
    # add the genotype back to the population.
    ret[i,] <- off
  }
  return(ret)
}

#Function to simulate CLONALITY:

clonality_within = function(pop){
  nrows <- dim(pop)[1]  # nrows
  ret <- pop  # Make a new dataframe with the same dimensions as pop
  for (i in 1:nrows) {
    # choose 1 row index at random to be the parent (selfing)
    parent <- sample(1:nrows, size = 1) 
    # Transfer the genotype of the parent direcly to the next generation
    ret[i,] <- pop[parent,]
  }
  return(ret)
}





########################
## 3) Apply functions to simulate selfing and outcrossing to each meadow, for 10 generations:
########################


Ngenerations1 = 1   #1 generation for selfing 
Ngenerations2 = 10   #10 generations for selfing, outcrossing and random mating



#######
#G-AF
#######


# Clonality 10
pop = newpop_G_AF  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_AF_clonality = pop


# Selfing 1 
pop = newpop_G_AF  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_AF_selfing1 = pop
#G_AF_selfing_freq = frequencies(G_AF_selfing)
#names(G_AF_selfing_freq)[3] = "Frequency_after_selfing"
#trial = merge(G_AF_withclones_freq, G_AF_selfing_freq, by=c("Locus", "Allele"))

# Selfing 10
pop = newpop_G_AF  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_AF_selfing10 = pop

#Random mating
pop = newpop_G_AF   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_AF_randmat = pop

#Outcrossing
pop = newpop_G_AF   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_AF_outcrossing = pop


#######
#G-AS
#######

# Clonality 10
pop = newpop_G_AS  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_AS_clonality = pop

# Selfing 1
pop = newpop_G_AS  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_AS_selfing1 = pop

# Selfing 10
pop = newpop_G_AS  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_AS_selfing10 = pop

#Random mating
pop = newpop_G_AS   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_AS_randmat = pop

#Outcrossing
pop = newpop_G_AS   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_AS_outcrossing = pop


#######
#G-BB
#######

# Clonality 10
pop = newpop_G_BB  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_BB_clonality = pop

# Selfing 1
pop = newpop_G_BB  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_BB_selfing1 = pop

# Selfing 10
pop = newpop_G_BB  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_BB_selfing10 = pop

#Random mating
pop = newpop_G_BB   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_BB_randmat = pop

#Outcrossing
pop = newpop_G_BB   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_BB_outcrossing = pop


#######
#G-BV
#######

# Clonality 10
pop = newpop_G_BV  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_BV_clonality = pop

# Selfing 1
pop = newpop_G_BV  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_BV_selfing1 = pop

# Selfing 10
pop = newpop_G_BV  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_BV_selfing10 = pop

#Random mating
pop = newpop_G_BV   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_BV_randmat = pop

#Outcrossing
pop = newpop_G_BV   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_BV_outcrossing = pop



#######
#G-GB
#######

# Clonality 10
pop = newpop_G_GB  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_GB_clonality = pop

# Selfing 1
pop = newpop_G_GB  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_GB_selfing1 = pop

# Selfing 10
pop = newpop_G_GB  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_GB_selfing10 = pop

#Random mating
pop = newpop_G_GB   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_GB_randmat = pop

#Outcrossing
pop = newpop_G_GB   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_GB_outcrossing = pop


#######
#G-NB
#######

# Clonality 10
pop = newpop_G_NB  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_NB_clonality = pop

# Selfing 1
pop = newpop_G_NB  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_NB_selfing1 = pop

# Selfing 10
pop = newpop_G_NB  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_NB_selfing10 = pop

#Random mating
pop = newpop_G_NB   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_NB_randmat = pop

#Outcrossing
pop = newpop_G_NB   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_NB_outcrossing = pop


#######
#G-RX
#######

# Clonality 10
pop = newpop_G_RX  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_RX_clonality = pop

# Selfing 1
pop = newpop_G_RX  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_RX_selfing1 = pop

# Selfing 10
pop = newpop_G_RX  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_RX_selfing10 = pop

#Random mating
pop = newpop_G_RX   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_RX_randmat = pop

#Outcrossing
pop = newpop_G_RX   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_RX_outcrossing = pop


#######
#G-SG
#######

# Clonality 10
pop = newpop_G_SG  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_SG_clonality = pop

# Selfing 1
pop = newpop_G_SG  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_SG_selfing1 = pop

# Selfing 10
pop = newpop_G_SG  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_SG_selfing10 = pop

#Random mating
pop = newpop_G_SG   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_SG_randmat = pop

#Outcrossing
pop = newpop_G_SG   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_SG_outcrossing = pop


#######
#G-SK
#######

# Clonality 10
pop = newpop_G_SK  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

G_SK_clonality = pop

# Selfing 1
pop = newpop_G_SK  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_SK_selfing1 = pop

# Selfing 10
pop = newpop_G_SK  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

G_SK_selfing10 = pop

#Random mating
pop = newpop_G_SK   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

G_SK_randmat = pop

#Outcrossing
pop = newpop_G_SK   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

G_SK_outcrossing = pop


#######
#Snackebackebukten
#######

# Clonality 10
pop = newpop_Snack

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

Snack_clonality = pop

# Selfing 1
pop = newpop_Snack  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

Snack_selfing1 = pop

# Selfing 10
pop = newpop_Snack  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

Snack_selfing10 = pop

#Random mating
pop = newpop_Snack   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

Snack_randmat = pop

#Outcrossing
pop = newpop_Snack   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

Snack_outcrossing = pop


#######
#Torgestad
#######

# Clonality 10
pop = newpop_Torg

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

Torg_clonality = pop

# Selfing 1
pop = newpop_Torg  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

Torg_selfing1 = pop

# Selfing 10
pop = newpop_Torg  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

Torg_selfing10 = pop

#Random mating
pop = newpop_Torg   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

Torg_randmat = pop

#Outcrossing
pop = newpop_Torg   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

Torg_outcrossing = pop


#################################################
########## K populations:
#################################################


#######
#K-BK
#######

# Clonality 10
pop = newpop_K-BK  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_BK_clonality = pop

# Selfing 1
pop = newpop_K_BK  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_BK_selfing1 = pop

# Selfing 10
pop = newpop_K_BK  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_BK_selfing10 = pop

#Random mating
pop = newpop_K_BK   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_BK_randmat = pop

#Outcrossing
pop = newpop_K_BK   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_BK_outcrossing = pop


#######
#K-K
#######

# Clonality 10
pop = newpop_K_K  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_K_clonality = pop

# Selfing 1
pop = newpop_K_K  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_K_selfing1 = pop

# Selfing 10
pop = newpop_K_K  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_K_selfing10 = pop

#Random mating
pop = newpop_K_K   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_K_randmat = pop

#Outcrossing
pop = newpop_K_K   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_K_outcrossing = pop


#######
#K-KR
#######

# Clonality 10
pop = newpop_K_KR 

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_KR_clonality = pop

# Selfing 1
pop = newpop_K_KR  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_KR_selfing1 = pop

# Selfing 10
pop = newpop_K_KR  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_KR_selfing10 = pop

#Random mating
pop = newpop_K_KR   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_KR_randmat = pop

#Outcrossing
pop = newpop_K_KR   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_KR_outcrossing = pop


#######
#K-LD
#######

# Clonality 10
pop = newpop_K_LD  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_LD_clonality = pop

# Selfing 1
pop = newpop_K_LD  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_LD_selfing1 = pop

# Selfing 10
pop = newpop_K_LD  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_LD_selfing10 = pop

#Random mating
pop = newpop_K_LD   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_LD_randmat = pop

#Outcrossing
pop = newpop_K_LD   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_LD_outcrossing = pop


#######
#K-NG
#######

# Clonality 10
pop = newpop_K_NG  

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_NG_clonality = pop

# Selfing 1
pop = newpop_K_NG  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_NG_selfing1 = pop

# Selfing 10
pop = newpop_K_NG  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_NG_selfing10 = pop

#Random mating
pop = newpop_K_NG   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_NG_randmat = pop

#Outcrossing
pop = newpop_K_NG   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_NG_outcrossing = pop


#######
#K-NI
#######

# Clonality 10
pop = newpop_K_NI

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_NI_clonality = pop

# Selfing 1
pop = newpop_K_NI  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_NI_selfing1 = pop

# Selfing 10
pop = newpop_K_NI  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_NI_selfing10 = pop

#Random mating
pop = newpop_K_NI   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_NI_randmat = pop

#Outcrossing
pop = newpop_K_NI   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_NI_outcrossing = pop


#######
#K-OK
#######

# Clonality 10
pop = newpop_K_OK

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_OK_clonality = pop

# Selfing 1
pop = newpop_K_OK  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_OK_selfing1 = pop

# Selfing 10
pop = newpop_K_OK  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_OK_selfing10 = pop

#Random mating
pop = newpop_K_OK   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_OK_randmat = pop

#Outcrossing
pop = newpop_K_OK   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_OK_outcrossing = pop


#######
#K-ON
#######

# Clonality 10
pop = newpop_K_ON

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_ON_clonality = pop

# Selfing 1
pop = newpop_K_ON  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_ON_selfing1 = pop

# Selfing 10
pop = newpop_K_ON  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_ON_selfing10 = pop

#Random mating
pop = newpop_K_ON   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_ON_randmat = pop

#Outcrossing
pop = newpop_K_ON   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_ON_outcrossing = pop


#######
#K-RT
#######

# Clonality 10
pop = newpop_K_RT

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_RT_clonality = pop

# Selfing 1
pop = newpop_K_RT  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_RT_selfing1 = pop

# Selfing 10
pop = newpop_K_RT  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_RT_selfing10 = pop

#Random mating
pop = newpop_K_RT   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_RT_randmat = pop

#Outcrossing
pop = newpop_K_RT   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_RT_outcrossing = pop


#######
#K-SK
#######

# Clonality 10
pop = newpop_K_SK

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_SK_clonality = pop

# Selfing 1
pop = newpop_K_SK  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_SK_selfing1 = pop

# Selfing 10
pop = newpop_K_SK  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_SK_selfing10 = pop

#Random mating
pop = newpop_K_SK   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_SK_randmat = pop

#Outcrossing
pop = newpop_K_SK   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_SK_outcrossing = pop


#######
#K-SO
#######

# Clonality 10
pop = newpop_K_SO

for (i in 1:Ngenerations2) {
  new_pop_clonal = clonality_within(pop)
  pop = new_pop_clonal
}

K_SO_clonality = pop

# Selfing 1
pop = newpop_K_SO  

for (i in 1:Ngenerations1) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_SO_selfing1 = pop

# Selfing 10
pop = newpop_K_SO  

for (i in 1:Ngenerations2) {
  new_pop_selfing = selfing_within(pop)
  pop = new_pop_selfing
}

K_SO_selfing10 = pop

#Random mating
pop = newpop_K_SO   

for (i in 1:Ngenerations2) {
  new_pop_randmat = random_mating_within(pop)
  pop = new_pop_randmat
}

K_SO_randmat = pop

#Outcrossing
pop = newpop_K_SO   

for (i in 1:Ngenerations2) {
  new_pop_outcrossing = outcrossing_within(pop)
  pop = new_pop_outcrossing
}

K_SO_outcrossing = pop

