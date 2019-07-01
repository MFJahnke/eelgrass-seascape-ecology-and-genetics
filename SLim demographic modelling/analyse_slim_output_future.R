#####################################################################################################
########analyse output of future slim models
#####################################################################################################

remove(list=ls())

libraries <- c("poppr", "adegenet", "spaa", "igraph", "metap", "plyr", "reshape2", "dplyr", "sp", "rgdal", "pegas", "lattice", "dismo", "ggplot2", "diveRsity", "assigner", "tess3r", "gstudio", "progress", "ggmap", "plotrix", "gridExtra", "hierfstat", "RColorBrewer", "grid", "knitr")
sapply(libraries, function(x) { suppressMessages(require(x, character.only=T)) } )

#library(RClone)
#library(Demerelate)
library(poppr)
#library(diveRsity)
library(gstudio)
library(stringr)
library(genepopedit)

#for testing of single files
#file <- read.delim("~/Desktop/test2.txt", header=F, sep=" ", skip = 1)
#write.table(file,"test.stru")

# delete some unessecary files
file.remove("output.txt")
junk <- dir(path=path, pattern="*.stru") 
file.remove(junk)

file<-0
file.names <- dir(path, pattern ="Zostera_microsat_2bottlenecks_nonWF_mig_outputSample19_1_Pop12_gen.*.txt")  #careful when .stru files are generated, it takes them to!!
options(max.print = 999999)

for(i in 1:length(file.names)){
  file <- read.delim(file.names[i], header=F, sep=" ", skip = 1)
  file <- head(file, -1) 
  file = file[, -c(1:2)]
  file = as.data.frame(lapply(file, as.numeric))
  sink(mypath <- file.path(paste(file.names[i], ".stru", sep = "")), split=TRUE)
  print(file)  
  sink()
  #fileNew=cbind(file,individuals)  
  #print(fileNew)  
}
##for testing of single files
#obj=import2genind("test.stru", onerowperind=FALSE, n.ind=50, n.loc=20, col.lab=0, col.pop=0, ask=F, quiet = T)

file.names2<- dir(path, pattern ="*.stru")
for(j in 1:length(file.names2)){
  obj=import2genind(file.names2[j], onerowperind=FALSE, n.ind=50, n.loc=20, col.lab=0, col.pop=0, ask=FALSE, quiet = T)
  #remove clones
  MLGs_only=clonecorrect(obj, strata=NA)

  #genotypic richness
  titi=summary(obj)
  toto=summary(MLGs_only)
  R=(sum(toto$n.by.pop)-1)/(sum(titi$n.by.pop)-1)

  #get heterozygosity and no alleles
  Hobs=sum(toto$Hobs)/20
  Hexp=sum(toto$Hexp)/20
  
  #get total number of allels
  A=sum(toto$pop.n.all) #I'm not sure that the number of alleles is read correctly
  sink(file="output.txt", append=TRUE)
  #print(file)  
  print(R)
  print(Hobs)
  print(Hexp)
  print(A)
  sink(append=T)

  m= apply(tab(MLGs_only, freq=TRUE),2,mean, na.rm=TRUE)  #allele frequencies
  plot(rev(sort(m)), ylim=c(0,1), main=file.names2[j], cex.main=0.8)
} 

file.names2

#For G no decline
output10=as.vector(read.table("output.txt",colClasses = c("NULL",NA)))
output10 <- output10[, "V2"]
# manually copy output and save
#now copy output manually, save and put in vector with c() called output
results10=matrix(output10, ncol=4, byrow=TRUE)
#adjust generation depending on extinctions!
generation=c(28010,28100,28020,28030,28040,28050,28060,28070,28080,28090)
results10=cbind(results10,newColumn=generation)
colnames(results10) <- c("R","Hobs","Hexp","A","generation")
results10=as.data.frame(results10)
View(results10)
write.table(results10,"results_Pop12_13.txt")

output11=as.vector(read.table("output.txt",colClasses = c("NULL",NA)))
output11 <- output11[, "V2"]
results11=matrix(output11, ncol=4, byrow=TRUE)
results11=cbind(results11,newColumn=generation)
colnames(results11) <- c("R","Hobs","Hexp","A","generation")
results11=as.data.frame(results11)
View(results11)
write.table(results11,"results_Pop12_14.txt")

#etc


#population sizes for 10decline, as example
set_Ne=c(63,19,11,39,7,3,395,13,7,19,12)
gen28010_11=c(58,0,9,40,8,0,329,9,0,22,13)
gen28010_13=c(62,20,0,41,8,3,327,11,6,15,11)
gen28010_17=c(0,0,0,0,6,2,225,14,7,21,11)
gen28010_18=c(59,19,8,44,4,2,309,11,7,18,11)
gen28010_19=c(55,17,11,35,5,2,0,10,4,20,8)
gen28010_110=c(61,20,12,0,7,3,292,11,5,19,13)
gen28010_117=c(0,0,0,35,0,2,0,13,7,18,12)
gen28010_120=c(59,19,8,34,6,2,325,0,5,20,13)
#64,19,13,39,8,2,320,14,6,24,15

#population sizes for K declining, this is for 28020...28100
pop1_13=c(103,32,6,0,0,0,0,0,1)
pop3_13=c(0,0,0,0,0,1,5,2,3)
pop4_13=c(4,0,0,0,0,0,0,0,1)
pop5_13=c(20,9,1,3,2,3,3,1,3)
pop6_13=c(132,39,5,6,3,0,2,0,0)
pop7_13=c(8,6,1,0,2,0,0,0,0)
pop8_13=c(7,5,1,1,0,2,4,6,0)
pop9_13=c(9,6,0,0,0,2,0,0,0)
pop10_13=c(0,0,0,0,0,1,0,0,1)
pop11_13=c(1,0,0,1,3,2,0,5,6)
pop12_13=c(6,0,0,0,1,2,2,1,0)
#etc

Ne_generations=c(28020,28030,28040,28050,28060,28070,28080,28090,28100)
#plot
pdf(file="Pop_size_K_declining_zoomedNe20.pdf") 
plot(Ne_generations,pop1_13, pch=20,xlab="Generations", ylab="Pop size", ylim=c(0,20))
points(Ne_generations,pop1_14,pch=20)
points(Ne_generations,pop1_17,pch=20)
points(Ne_generations,pop1_18,pch=20)
points(Ne_generations,pop1_19,pch=20)
points(Ne_generations,pop3_13,pch=20, col="#CC79A7")
points(Ne_generations,pop3_14,pch=20, col="#CC79A7")
points(Ne_generations,pop3_17,pch=20, col="#CC79A7")
points(Ne_generations,pop3_18,pch=20, col="#CC79A7")
points(Ne_generations,pop3_19,pch=20, col="#CC79A7")
points(Ne_generations,pop4_13,pch=20, col="#E69F00")
points(Ne_generations,pop4_14,pch=20, col="#E69F00")
points(Ne_generations,pop4_17,pch=20, col="#E69F00")
points(Ne_generations,pop4_18,pch=20, col="#E69F00")
points(Ne_generations,pop4_19,pch=20, col="#E69F00")
points(Ne_generations,pop5_13,pch=20, col="#56B4E9")
points(Ne_generations,pop5_14,pch=20, col="#56B4E9")
points(Ne_generations,pop5_17,pch=20, col="#56B4E9")
points(Ne_generations,pop5_18,pch=20, col="#56B4E9")
points(Ne_generations,pop5_19,pch=20, col="#56B4E9")
points(Ne_generations,pop6_13,pch=20, col="#268bd2")
points(Ne_generations,pop6_14,pch=20, col="#268bd2")
points(Ne_generations,pop6_17,pch=20, col="#268bd2")
points(Ne_generations,pop6_18,pch=20, col="#268bd2")
points(Ne_generations,pop6_19,pch=20, col="#268bd2")
points(Ne_generations,pop7_13,pch=20, col="#009E73")
points(Ne_generations,pop7_14,pch=20, col="#009E73")
points(Ne_generations,pop7_17,pch=20, col="#009E73")
points(Ne_generations,pop7_18,pch=20, col="#009E73")
points(Ne_generations,pop7_19,pch=20, col="#009E73")
points(Ne_generations,pop8_13,pch=20, col="#d33682")
points(Ne_generations,pop8_14,pch=20, col="#d33682")
points(Ne_generations,pop8_17,pch=20, col="#d33682")
points(Ne_generations,pop8_18,pch=20, col="#d33682")
points(Ne_generations,pop8_19,pch=20, col="#d33682")
points(Ne_generations,pop9_13,pch=20, col="#2aa198")
points(Ne_generations,pop9_14,pch=20, col="#2aa198")
points(Ne_generations,pop9_17,pch=20, col="#2aa198")
points(Ne_generations,pop9_18,pch=20, col="#2aa198")
points(Ne_generations,pop9_19,pch=20, col="#2aa198")
points(Ne_generations,pop10_13,pch=20, col="#0072B2")
points(Ne_generations,pop10_14,pch=20, col="#0072B2")
points(Ne_generations,pop10_17,pch=20, col="#0072B2")
points(Ne_generations,pop10_18,pch=20, col="#0072B2")
points(Ne_generations,pop10_19,pch=20, col="#0072B2")
points(Ne_generations,pop11_13,pch=20, col="#D55E00")
points(Ne_generations,pop11_14,pch=20, col="#D55E00")
points(Ne_generations,pop11_17,pch=20, col="#D55E00")
points(Ne_generations,pop11_18,pch=20, col="#D55E00")
points(Ne_generations,pop11_19,pch=20, col="#D55E00")
points(Ne_generations,pop12_13,pch=20, col="grey")
points(Ne_generations,pop12_14,pch=20, col="grey")
points(Ne_generations,pop12_17,pch=20, col="grey")
points(Ne_generations,pop12_18,pch=20, col="grey")
points(Ne_generations,pop12_19,pch=20, col="grey")
legend("topright", legend=c("K-K", "K-BK","K-KR","K-LD","K-NG","K-NI","K-OK","K-ON", "K-RT","K-SK","K-SO"),
       col=c("black", "#CC79A7","#E69F00","#56B4E9","#268bd2","#009E73","#d33682","#2aa198","#0072B2","#D55E00","grey"),pch=20, cex=1,
       text.font=1, box.lty=0, ncol=2)
dev.off()

#get data frames
#For G stable
results10_Pop1=read.table("results_Pop1_11.txt")
results11_Pop1=read.table("results_Pop1_14.txt")
#etc

# to rank site according to highest gen div at 28090
library(abind)
results10_Pop1_28090=subset(results10_Pop1,generation==28090) 
results11_Pop1_28090=subset(results11_Pop1,generation==28090) 
results12_Pop1_28090=subset(results12_Pop1,generation==28090) 
results13_Pop1_28090=subset(results13_Pop1,generation==28090) 
results14_Pop1_28090=subset(results14_Pop1,generation==28090) 
arr = abind(results10_Pop1_28090,results11_Pop1_28090,results12_Pop1_28090,results13_Pop1_28090,results14_Pop1_28090, along=1)
Pop1=round(colMeans(arr),3)
Pop12=round(colMeans(arr),3)
Pop1
pops=abind(Pop1,Pop3,Pop4,Pop5,Pop6,Pop7,Pop8,Pop9,Pop10,Pop11,Pop12, along = 2)
pops <- as.data.frame(t(pops))
pops[order(pops$A),]

#Plotting G, just change R, A, Ho and Hexp, and change geom_line to geom_point, use 0.3 for dots and 0.1 for line
  pdf(file="A_stableG.pdf") 
  par(mfrow = c(2,1))
  ggplot(results10_Pop1,aes(x = generation, y = A, , size=I(0.3), ymax=1, ymin=0), colour="#000000")+
    geom_point() + geom_point(data=results11_Pop1,colour="#000000", size=0.3) + 
    geom_point() +geom_point(data=results12_Pop1,colour="#000000", size=0.3) + 
    geom_point(data=results13_Pop1,colour="#000000", size=0.3) +
    geom_point(data=results14_Pop1,colour="#000000", size=0.3) +
    geom_point(data=results10_Pop3,colour="#CC79A7", size=0.3) +
    geom_point(data=results11_Pop3,colour="#CC79A7", size=0.3) +
    geom_point(data=results12_Pop3,colour="#CC79A7", size=0.3) +
    geom_point(data=results13_Pop3,colour="#CC79A7", size=0.3) +
    geom_point(data=results14_Pop3,colour="#CC79A7", size=0.3) +
    geom_point(data=results10_Pop4,colour="#E69F00", size=0.3) +
    geom_point(data=results11_Pop4,colour="#E69F00", size=0.3) +
    geom_point(data=results12_Pop4,colour="#E69F00", size=0.3) +
    geom_point(data=results13_Pop4,colour="#E69F00", size=0.3) +
    geom_point(data=results14_Pop4,colour="#E69F00", size=0.3) +
    geom_point(data=results10_Pop5,colour="#56B4E9", size=0.3) +
    geom_point(data=results11_Pop5,colour="#56B4E9", size=0.3) +
    geom_point(data=results12_Pop5,colour="#56B4E9", size=0.3) +
    geom_point(data=results13_Pop5,colour="#56B4E9", size=0.3) +
    geom_point(data=results14_Pop5,colour="#56B4E9", size=0.3) +
    geom_point(data=results10_Pop6,colour="#268bd2", size=0.3) +
    geom_point(data=results11_Pop6,colour="#268bd2", size=0.3) +
    geom_point(data=results12_Pop6,colour="#268bd2", size=0.3) +
    geom_point(data=results13_Pop6,colour="#268bd2", size=0.3) +
    geom_point(data=results14_Pop6,colour="#268bd2", size=0.3) +
    geom_point(data=results10_Pop7,colour="#009E73", size=0.3) +
    geom_point(data=results11_Pop7,colour="#009E73", size=0.3) +
    geom_point(data=results12_Pop7,colour="#009E73", size=0.3) +
    geom_point(data=results13_Pop7,colour="#009E73", size=0.3) +
    geom_point(data=results14_Pop7,colour="#009E73", size=0.3) +
    geom_point(data=results10_Pop8,colour="#d33682", size=0.3) +
    geom_point(data=results11_Pop8,colour="#d33682", size=0.3) +
    geom_point(data=results12_Pop8,colour="#d33682", size=0.3) +
    geom_point(data=results13_Pop8,colour="#d33682", size=0.3) +
    geom_point(data=results14_Pop8,colour="#d33682", size=0.3) +
    geom_point(data=results10_Pop9,colour="#2aa198", size=0.3) +
    geom_point(data=results11_Pop9,colour="#2aa198", size=0.3) +
    geom_point(data=results12_Pop9,colour="#2aa198", size=0.3) +
    geom_point(data=results13_Pop9,colour="#2aa198", size=0.3) +
    geom_point(data=results14_Pop9,colour="#2aa198", size=0.3) +
    geom_point(data=results10_Pop10,colour="#0072B2", size=0.3) +
    geom_point(data=results11_Pop10,colour="#0072B2", size=0.3) +
    geom_point(data=results12_Pop10,colour="#0072B2", size=0.3) +
    geom_point(data=results13_Pop10,colour="#0072B2", size=0.3) +
    geom_point(data=results14_Pop10,colour="#0072B2", size=0.3) +
    geom_point(data=results10_Pop11,colour="#D55E00", size=0.3) +
    geom_point(data=results11_Pop11,colour="#D55E00", size=0.3) +
    geom_point(data=results12_Pop11,colour="#D55E00", size=0.3) +
    geom_point(data=results13_Pop11,colour="#D55E00", size=0.3) +
    geom_point(data=results14_Pop11,colour="#D55E00", size=0.3) +
    geom_point(data=results10_Pop12,colour="grey", size=0.3) +
    geom_point(data=results11_Pop12,colour="grey", size=0.3) +
    geom_point(data=results12_Pop12,colour="grey", size=0.3) +
    geom_point(data=results13_Pop12,colour="grey", size=0.3) +
    geom_point(data=results14_Pop12,colour="grey", size=0.3) +
    scale_x_continuous(breaks=seq(28010, 28100, 10), 
                       limits = c(28020, 28090))
  dev.off()
 
# Now safe under different name for each pop
output10_Pop3=output10
output11_Pop3=output11
#etc

#AFS for different generations in same run

obj=import2genind("Zostera_microsat_2bottlenecks_nonWF_mig_outputSample1_1_Pop3_gen10.txt.stru", onerowperind=FALSE, n.ind=50, n.loc=20, col.lab=0, col.pop=0, ask=F, quiet = T)
MLGs_only=clonecorrect(obj, strata=NA)
m1000= apply(tab(MLGs_only, freq=TRUE),2,mean, na.rm=TRUE)  #allele frequencies
obj=import2genind("Zostera_microsat_2bottlenecks_nonWF_mig_outputSample1_1_Pop3_gen20.txt.stru", onerowperind=FALSE, n.ind=50, n.loc=20, col.lab=0, col.pop=0, ask=F, quiet = T)
MLGs_only=clonecorrect(obj, strata=NA)
m2000= apply(tab(MLGs_only, freq=TRUE),2,mean, na.rm=TRUE) 
#etc

plot(rev(sort(m1000)), col='gray87', ylim=c(0,1), xlim=c(0,80), cex=0.5)
points(rev(sort(m2000)), col='gray87', cex=0.5)
#etc

#for pop size use awk and then read table
pop_size=read.delim("G2_2bottlenecks1_popsizes.txt", sep =" ", header=F)
generations_slim=read.delim("G2_2bottlenecks1_generations.txt", sep =" ", header=F)
slim_results=cbind(pop_size, generations_slim)
colnames(slim_results)=c("V1" , "V2" , "V3" , "Pop size" , "V5" , "V6" , "Generation" , "8" , "9" , "Pi" , "11" , "12" , "D")

#plot Pop size and other outputs of slim
pdf(file="POP_size_and_slimout_withburnin.pdf") 
par(mfrow = c(2,1))
plot(slim_results$Generation,slim_results$`Pop size`)
plot(slim_results$Generation,slim_results$Pi)
plot(slim_results$Generation,slim_results$D)
dev.off()


#now remove the burnin of 20000
threshhold <- 20000  
results2=subset(results, results[ , 5] > threshhold)  
slim_results2=subset(slim_results, slim_results[ , 7] > threshhold)  

#and plot again
pdf(file="gen_div_with2repeatsadded.pdf") 
par(mfrow = c(2,1))
plot(results2$generation,results2$R)
plot(results2$generation, results2$Hobs)
plot(results2$generation,results2$Hexp)
plot(results2$generation,results2$A)
dev.off()

pdf(file="POP_size_and_slimout.pdf") 
par(mfrow = c(2,1))
plot(slim_results2$Generation,slim_results2$`Pop size`)
plot(slim_results2$Generation,slim_results2$Pi)
plot(slim_results2$Generation,slim_results2$D)
dev.off()


#my real data-set
#all sampling sites
real_all=import2genind("project_data_recoded.stru", onerowperind=T, n.ind=1195, n.loc=20, col.lab=1, col.pop=2, ask=FALSE, quiet = T)

y=seppop(real_all,pop=NULL,truenames=TRUE,res.type=c("genind","matrix"),
         drop=FALSE, treatOther=TRUE, quiet=TRUE)
#par(mfrow = c(2, 2))
k= apply(tab(y$'1', freq=TRUE),2,mean, na.rm=T) 
threshhold = 0
k2=subset(k, k > threshhold)  
l= apply(tab(y$'2', freq=TRUE),2,mean, na.rm=T) 
l2=subset(l, l > threshhold) 
#etc

plot(rev(sort(k2)), cex=0.5,col='#C0C0C044', xlim=c(0,120))
points(rev(sort(l2)),col='#C0C0C044', pch=0, cex=0.5)
#etc


