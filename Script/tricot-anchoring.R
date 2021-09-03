library("climatrends")
library("tidyverse")
library("PlackettLuce")
library("gosset")# Running gosset immediately after plackettluce will work, but doing it alone will not run
library("patchwork")
library("qvcalc")
library("ggparty")
library("igraph")
source("https://raw.githubusercontent.com/agrobioinfoservices/ClimMob-analysis/master/R/functions.R")


# Rank_1map<-rank_tricot(data=tricot1[,13:14], items=c("Variety.A", "Variety.B", "Variety.C"), 
                           input=c("X1month.Overall.impression.Best","X1month.Overall.impression.Worst.off")

tricot_data<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/tricot data 2020/tricot-1map-single trait.csv")
orig_name<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/tricot data 2020/Final_Tricot_List_30varieties.csv")# that contains 30 tricot list
#tricot_complete<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/tricot data 2020/Tricot-anchoring1-9map.csv")
dim(orig_name)
dim(tricot_data)
str(orig_name)
str(tricot_data)
#dim(tricot_complete)
un_R<-unclass(R_1map)

all_vars<-union(tricot_data$Variety.A, tricot_data$Variety.B )
all_vars2<-union(all_vars, tricot_data$VARIETY.C )
all_vars2<-gsub("\n", "",all_vars2)

all_vars2[!which(all_vars2%in%orig_name$Genotype)]# looking for the genot
all_vars[-c(which(all_vars%in%orig_name$Genotype))]
View(tricot_data)
View(orig_name)



present2<-all_vars2[(all_vars2 %in% (orig_name[,2]))]
absent<-all_vars2[(!all_vars2 %in% (orig_name[,2]))]

#tricot_data[tricot_data$Variety.A=="IITA-TMS-IBA30572"]<-TMS_IBA30572

library("tidyverse")
#tricot_data.modified <- tricot_data %>%
 # mutate(Variety.A = ifelse(VARIETY.C == "TMS_IBA30572", "IITA-TMS-IBA30572", VARIETY.C))
#View(tricot_data.modified)
#rm(tricot_data.modified )

unique(tricot_data$VARIETY.C)
R_1map<- rank_tricot(data = tricot_data,
                     items = c("Variety.A" ,"Variety.B",  "VARIETY.C"),
                     input = c("X1month.Overall.impression.Best", "X1month.Overall.impression.Worst.off"))
dim(R_1map)
#RRR2<-rrr2[1:length(rrr2),as.rankings=FALSE]

class(R_1map)

un_R<-unclass(R_1map)#

adj_R<-adjacency(R_1map)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_R<-network(adj_R)
favor<-summarise_favourite(R_1map) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor)
plot(favor)+
  theme_bw()+
  theme(panel.grid = element_blank())

dom<-summarise_dominance(R_1map)

plot(dom)


#Using the complete data
R_1map2<- rank_tricot(data = tricot_data,
                     items = c("Variety.A" ,"Variety.B",  "VARIETY.C"),
                     input = c("X1month.Overall.impression.Best", "X1month.Overall.impression.Worst.off"))
dim(R_1map2)


Tricot_dat<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/tricot-1map-3map.csv")# also contains 6 to 9 months data
str(Tricot_dat)
dim(Tricot_dat)

#tricot_harvest<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Tricot-anchoring1-9map.csv")

Tricot_dat$Variety.A<-gsub("\n", "",Tricot_dat$Variety.A) # removing the extra name (\n at the end of the genotype names)
Tricot_dat$Variety.B<-gsub("\n", "",Tricot_dat$Variety.B)
Tricot_dat$VARIETY.C<-gsub("\n", "",Tricot_dat$VARIETY.C)


Rank_1map<- rank_tricot(data = Tricot_dat,
                        items = c("Variety.A" ,"Variety.B",  "VARIETY.C"),
                        input = c("X1month.Overall.impression.Best", "X1month.Overall.impression.Worst.off"))
dim(Rank_1map)

View(Rank_1map)


un_Rank<-unclass(Rank_1map)

All_vars<-union(Tricot_dat$Variety.A, Tricot_dat$Variety.B )
All_vars2<-union(All_vars,Tricot_dat$VARIETY.C )

adj_R1map<-adjacency(Rank_1map)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_R1map<-network(adj_R1map)
plot(adj_net_R1map)
favor_1map<-summarise_favourite(Rank_1map) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_1map)
plot(favor_1map)+xlab("genotype")+
  theme_bw()+
  theme(panel.grid = element_blank())

dominace<-summarise_dominance(Rank_1map)

plot(dominace)

write_csv(favor_1map,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_1MMAP.csv")

#for 3 MAP
Rank_3map<- rank_tricot(data = Tricot_dat,
                        items = c("Variety.A" ,"Variety.B",  "VARIETY.C"),
                        input = c("X3Months.overall.impression.Best", "X3Months.overall.impression.Worst.off"))
dim(Rank_3map)

un_Rank3<-unclass(Rank_3map)

adj_R3map<-adjacency(Rank_3map)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_R3map<-network(adj_R3map)
plot(adj_net_R3map)
favor_3map<-summarise_favourite(Rank_3map) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_3map)
plot(favor_3map)+ xlab("genotype")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_3map,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_3MMAP.csv")


dominace_3map<-summarise_dominance(Rank_3map)

plot(dominace)


#for 6 MAP

Rank_6map<- rank_tricot(data = Tricot_dat,
                        items = c("Variety.A" ,"Variety.B",  "VARIETY.C"),
                        input = c("X6MAP.Overall.impression.Best", "X6MAP.Overall.impression.Worst.off"))
dim(Rank_6map)

un_Rank6<-unclass(Rank_6map)

adj_R6map<-adjacency(Rank_6map)# to know how the varieties are connected to the other Ie how many time varieties was preffered
adj_net_R6map<-network(adj_R6map)
plot(adj_net_R6map)
favor_6map<-summarise_favourite(Rank_6map) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_6map)
plot(favor_6map)+ xlab("genotype")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_6map,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_6MAP.csv")


ibsetA<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/iita_uyt36/uyt36-ibadan-setA.csv")

present-UYT<-all_vars2[(all_vars2 %in% (ibsetA[,19]))]
absent<-all_vars2[(!all_vars2 %in% (orig_name[,2]))]

present-UYT2<-orig_name[(orig_name[,2]%in% (ibsetA[,19]))]

absent2<-all_vars2[(!all_vars2 %in% (ibsetA[,19]))] #Absent in ibadan uyt36 A
present2<-all_vars2[(all_vars2 %in% (ibsetA[,19]))] #present in ibadan uyt36 A

ibsetb<-("/Users/chinedoziamaefula/OneDrive - Cornell University/iita_uyt36/uyt36-ibadan-setb.csv")
abs-uyt-ibsetb<-all_vars2[(!all_vars2 %in% (ibsetb[,19]))] #Absent in ibadan uyt36b
pre-uyt-ibsetb<-all_vars2[(all_vars2 %in% (ibsetb[,19]))] #present in ibadan uyt36b

## standard error and z-score for the rank
model_1map <- PlackettLuce(Rank_1map, npseudo = 0.5)
summary(model_1map)
summary(model_1map, ref = NULL)

qv_1map <- qvcalc(model_1map)
summary(qv_1map)
#plot(qv_1map, xlab = "genotypes", ylab = "Worth (log)", main = NULL)
qv_1map<- qvcalc(model_1map)
qv_1map$qvframe <-qv_1map$qvframe[order(coef(model_1map)),]
plot(qv_1map, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_1map$qvframe), las = 2, cex.axis = 0.6)


model_3map <- PlackettLuce(Rank_3map, npseudo = 0.5)
summary(model_3map)
summary(model_3map , ref = NULL)

qv_3map <- qvcalc(model_3map)
summary(qv_3map)
#plot(qv_3map, xlab = "genotypes", ylab = "Worth (log)", main = NULL)

qv_3<- qvcalc(model_3map)
qv_3$qvframe <- qv_3$qvframe[order(coef(model_3map)),]
plot(qv_3, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_3$qvframe), las = 2, cex.axis = 0.6)




mod1_6map <- PlackettLuce(Rank_6map, npseudo = 0.5)
summary(mod1_6map)
summary(mod1_6map, ref = NULL)

qv_6map <- qvcalc(mod1_6map)
summary(qv_6map)
#plot(qv_6map, xlab = "genotypes", ylab = "Worth (log)", main = NULL)

coef(summary(mod1_6map))

qv_6<- qvcalc(mod1_6map)
qv_6$qvframe <- qv_6$qvframe[order(coef(mod1_6map)),]
plot(qv_6, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_6$qvframe), las = 2, cex.axis = 0.6)



#for 9 MAP

Rank_9map<- rank_tricot(data = Tricot_dat,
                        items = c("Variety.A" ,"Variety.B",  "VARIETY.C"),
                        input = c("X9map.Best.Overall.impression", "X9map.Overall.impression.Worst.off"))
dim(Rank_9map)

un_Rank9<-unclass(Rank_9map)

adj_R9map<-adjacency(Rank_9map)# to know how the varieties are connected to the other Ie how many time varieties was preffered
adj_net_R9map<-network(adj_R9map)
plot(adj_net_R9map)
favor_9map<-summarise_favourite(Rank_9map) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_9map)
plot(favor_9map)+ xlab("genotype")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_9map,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_9MAP.csv")


mod_9map <- PlackettLuce(Rank_9map, npseudo = 0.5)
summary(mod_9map)
summary(mod_9map, ref = NULL)

qv_9map <- qvcalc(mod_9map)
summary(qv_9map)
#plot(qv_9map, xlab = "genotypes", ylab = "estimate", main = NULL)

coef(summary(mod_9map))

qv_9<- qvcalc(mod_9map)
qv_9$qvframe <- qv_9$qvframe[order(coef(mod_9map)),]
plot(qv_9, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_9$qvframe), las = 2, cex.axis = 0.6)




#looking for tricot genotypes in uyt36 and uyt34

uyt36_uyt34<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/UYT_inTRICOT/BLUEs-uyt36-34.csv")
geno_tricot<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/UYT_inTRICOT/Final_Tricot_List_30varieties.csv")


present<-(uyt36_uyt34[uyt36_uyt34$gen %in% geno_tricot$Genotype,])
present2 <- count(uyt36_uyt34[uyt36_uyt34$gen %in% geno_tricot$Genotype,])# picking genotypes  present data using geno_tricot
abs <- (geno_tricot[!geno_tricot$Genotype %in% uyt36_uyt34$gen,])
abs2<- count(geno_tricot[!geno_tricot$Genotype %in% uyt36_uyt34$gen,])

#absent<-(uyt36_uyt34[!uyt36_uyt34$gen %in% geno_tricot$Genotype,])
#absent<-count(uyt36_uyt34[!uyt36_uyt34$gen %in% geno_tricot$Genotype,])

library(Hmisc)
best_all<-data.frame(favor_1map$best, favor_3map$best,favor_6map$best, favor_9map$best)
colnames(best_all)

#Rename corename
library(tidyverse)
library(dplyr)
best_all_rename<-rename(best_all,Best_1MAP=favor_1map.best, Best_3MAP=favor_3map.best, Best_6MAP=favor_6map.best, Best_9MAP=favor_9map.best )



#rm(colnames(best_all)) <-c("favor_1map$best", "favor_3map$best","favor_6map$best", "favor_9map$best")
#colnames(best_all)
library(corrplot)

best_Tricot<-round(cor(best_all_rename,use="complete.obs"),3)

best_Tricot1<-corrplot(best_Tricot, type = "upper", method="number", order = "hclust",tl.col = "black" , tl.srt = 90,  cl.pos = "n")

#write.csv(corplot.umu, file=here("correlation.umu_new.csv"))

#correlation using the estimates 

all_timepoint<-data.frame(qv_1map$qvframe$estimate, qv_3$qvframe$estimate, qv_6$qvframe$estimate, qv_9$qvframe$estimate)
colnames(all_timepoint)
#newname_timepoint<-rename(all_timepoint,AMAP=qv_1map$qvframe$estimate, BMAP=qv_3$qvframe$estimate, CMAP=qv_6$qvframe$estimate, DMAP=qv_9$qvframe$estimate)

cor_placketluce_estimate<-round(cor(all_timepoint,use="complete.obs"),3)
cor_placketluce_estimate

write.csv(cor_placketluce_estimate,file("cor_placketluce_estimate.csv"))


# Using the complete tricot anchoring harvest data (containg the harvest sensory and agronomic data)
tricot_harvest<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2020 harvest tricot.csv")
dim(tricot_harvest)


# for yield data
Rank_yield_parameters<- rank_tricot(complete(data =tricot_harvest,
                        items = c("Variety.A" ,"Variety.B","Variety.C"),
                        input = c("Root.trait.Overall.impression.Best", "Root.trait.overall.impression.Worst")))

dim(Rank_yield_parameters)

un_Rank3<-unclass(Rank_3map)

adj_R3map<-adjacency(Rank_3map)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_R3map<-network(adj_R3map)
plot(adj_net_R3map)
favor_3map<-summarise_favourite(Rank_3map) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_3map)
plot(favor_3map)+ xlab("genotype")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_3map,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_3MMAP.csv")


dominace_3map<-summarise_dominance(Rank_3map)

plot(dominace)


Rank_processing<- rank_tricot(data =tricot_harvest,
                                    items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                                    input = c("Processing.overall.impression.Best", "Processing.Overall.impression.Worst"))
dim(Rank_yield_parameters)

un_Rank3<-unclass(Rank_3map)
