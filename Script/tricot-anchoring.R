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


Tricot_dat<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/data/tricot-1map-3map.csv")# also contains 6 to 9 months data
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
plot(favor_3map)+ xlab("genotype")+ylab("score")+
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
plot(favor_6map)+ xlab("genotype")+ylab("score")+
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
plot(favor_9map)+ xlab("genotype")+ ylab("score")+
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

cor_estimate2020_agro<-round(cor(all_timepoint,use="complete.obs"),3)
cor2020_table<-xtable(cor_estimate2020_agro)

write_csv(cor2020_table,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/ccor_estimate2020_agro.csv")

write.csv(cor_placketluce_estimate,file("cor_placketluce_estimate.csv"))
??worth

# Using the complete tricot anchoring harvest data (containg the harvest sensory and agronomic data)
tricot_harvest_2020<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2020 harvest tricot.csv")
dim(tricot_harvest)

tricot2019<-read.csv('/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2019 tricot Yield.csv')

dim(tricot2019)

# for 2019 yield data

Rank_yield2019<- rank_tricot(data =tricot2019,
                        items = c("Variety.A" ,"Variety.B","Variety.C"),
                        input = c("Fresh.root.overall.impression.Best", "Fresh.root.overall.impression.Worst"))

dim(Rank_yield2019)

un_Rank_yield2019<-unclass(Rank_yield2019)

adj_yield2019<-adjacency(Rank_yield2019)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_yield19<-network(adj_yield2019)
plot(adj_net_yield19)

favor_yield2019<-summarise_favourite(Rank_yield2019) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_yield2019)
plot(favor_yield2019)+ xlab("genotype")+ ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_yield2019,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_2019 yield.csv")

mod_yield2019 <- PlackettLuce(Rank_yield2019, npseudo = 0.5)
summary(mod_yield2019)
summary(mod_yield2019, ref = NULL)

qv_yield2019 <- qvcalc(mod_yield2019)
summary(qv_yield2019)
#plot(qv_9map, xlab = "genotypes", ylab = "estimate", main = NULL)

coef(summary(mod_yield2019))

qv_yield2019<- qvcalc(mod_yield2019)
qv_yield2019$qvframe <- qv_yield2019$qvframe[order(coef(mod_yield2019)),]
plot(qv_yield2019, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_yield2019$qvframe), las = 2, cex.axis = 0.6)

#Favourability score of 2019 tricot agronomic data

tricot_harvest_2019<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2019 21 tricot harvest data.csv")
dim(tricot_harvest_2019)
tricot_agro<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2019 tricot agronomic 21 farmers.csv")# 2019 agronomic data
dim(tricot_agro)
colnames(tricot_agro)

Rank_3map_2019<- rank_tricot(data =tricot_agro,
                             items = c("Variety_A" ,"Variety_B","Variety_C"),
                             input = c("best.overimpres3", "worst.overimpres3"))

dim(Rank_3map_2019)

un_Rank_3map_2019<-unclass(Rank_3map_2019)

adj_3map_2019<-adjacency(Rank_3map_2019)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_3map_2019<-network(adj_3map_2019)
plot(adj_net_3map_2019)

favor_3map_2019<-summarise_favourite(Rank_3map_2019) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_3map_2019)
plot(favor_3map_2019)+ xlab("genotype")+ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_3map_2019,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_3map_2019.csv")

mod_3map_2019 <- PlackettLuce(Rank_3map_2019, npseudo = 0.5)
summary(mod_3map_2019)
summary(mod_3map_2019, ref = NULL)

qv_3map_2019<- qvcalc(mod_3map_2019)
summary(qv_3map_2019)
#plot(qv_9map, xlab = "genotypes", ylab = "estimate", main = NULL)

coef(summary(mod_3map_2019))

qv_3map_2019<- qvcalc(mod_3map_2019)
qv_3map_2019$qvframe <- qv_3map_2019$qvframe[order(coef(mod_3map_2019)),]
plot(qv_3map_2019, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_3map_2019$qvframe), las = 2, cex.axis = 0.6)

# favourability for 2019 6MAP
Rank_6map_2019<- rank_tricot(data =tricot_agro,
                             items = c("Variety_A" ,"Variety_B","Variety_C"),
                             input = c("best.overimpres6", "worst.overimpres6"))

dim(Rank_6map_2019)

un_Rank_6map_2019<-unclass(Rank_6map_2019)

adj_6map_2019<-adjacency(Rank_6map_2019)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_6map_2019<-network(adj_6map_2019)
plot(adj_net_6map_2019)

favor_6map_2019<-summarise_favourite(Rank_6map_2019) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_6map_2019)
plot(favor_6map_2019)+ xlab("genotype")+ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_6map_2019,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_6map_2019.csv")

mod_6map_2019 <- PlackettLuce(Rank_6map_2019, npseudo = 0.5)
summary(mod_6map_2019)
summary(mod_6map_2019, ref = NULL)

qv_6map_2019<- qvcalc(mod_6map_2019)
summary(qv_6map_2019)


coef(summary(mod_6map_2019))

qv_6map_2019<- qvcalc(mod_6map_2019)
qv_6map_2019$qvframe <- qv_6map_2019$qvframe[order(coef(mod_6map_2019)),]
plot(qv_6map_2019, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_6map_2019$qvframe), las = 2, cex.axis = 0.6)

#2019 favourability 9MAP study
Rank_9map_2019<- rank_tricot(data =tricot_agro,
                             items = c("Variety_A" ,"Variety_B","Variety_C"),
                             input = c("best.overimpres9", "worst.overimpres9"))

dim(Rank_9map_2019)

un_Rank_9map_2019<-unclass(Rank_9map_2019)

adj_9map_2019<-adjacency(Rank_9map_2019)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_9map_2019<-network(adj_9map_2019)
plot(adj_net_9map_2019)

favor_9map_2019<-summarise_favourite(Rank_9map_2019) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_9map_2019)
plot(favor_9map_2019)+ xlab("genotype")+ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_9map_2019,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_9map_2019.csv")

mod_9map_2019 <- PlackettLuce(Rank_9map_2019, npseudo = 0.5)
summary(mod_9map_2019)
summary(mod_9map_2019, ref = NULL)

qv_9map_2019<- qvcalc(mod_9map_2019)
summary(qv_9map_2019)


coef(summary(mod_9map_2019))

qv_9map_2019<- qvcalc(mod_9map_2019)
qv_9map_2019$qvframe <- qv_9map_2019$qvframe[order(coef(mod_9map_2019)),]
plot(qv_9map_2019, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_9map_2019$qvframe), las = 2, cex.axis = 0.6)


#harvest data for 2019
Rank_garri_2019<- rank_tricot(data =tricot_harvest_2019,
                             items = c("Variety_A" ,"Variety_B","Variety_C"),
                             input = c("best_overimpres_gariqual", "worst_overimpres_gariqual"))

dim(Rank_garri_2019)

un_Rank_garri_2019<-unclass(Rank_garri_2019)

adj_garri_2019<-adjacency(Rank_garri_2019)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_garri_2019<-network(adj_garri_2019)
plot(adj_net_garri_2019)

favor_garri_2019<-summarise_favourite(Rank_garri_2019) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_garri_2019)
plot(favor_garri_2019)+ xlab("genotype")+ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_garrimap_2019,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_garriquality_2019.csv")

mod_garri_2019 <- PlackettLuce(Rank_garri_2019, npseudo = 0.5)
summary(mod_garri_2019)
summary(mod_garri_2019, ref = NULL)

qv_garri_2019<- qvcalc(mod_garri_2019)
summary(qv_garri_2019)
#plot(qv_9map, xlab = "genotypes", ylab = "estimate", main = NULL)

coef(summary(mod_garri_2019))

qv_garri_2019<- qvcalc(mod_garri_2019)
qv_garri_2019$qvframe <- qv_garri_2019$qvframe[order(coef(mod_garri_2019)),]
plot(qv_garri_2019, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_garri_2019$qvframe), las = 2, cex.axis = 0.6)

#2019 eba quality
Rank_eba_2019<- rank_tricot(data =tricot_harvest_2019,
                              items = c("Variety_A" ,"Variety_B","Variety_C"),
                              input = c("best_overimpres_ebaprep", "worst_overimpres_ebaprep"))

dim(Rank_eba_2019)

un_Rank_eba_2019<-unclass(Rank_eba_2019)

adj_eba_2019<-adjacency(Rank_eba_2019)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_eba_2019<-network(adj_eba_2019)
plot(adj_net_eba_2019)

favor_eba_2019<-summarise_favourite(Rank_eba_2019) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_eba_2019)
plot(favor_eba_2019)+ xlab("genotype")+ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_eba_2019,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_eba_quality_2019.csv")

mod_eba_2019 <- PlackettLuce(Rank_eba_2019, npseudo = 0.5)
summary(mod_eba_2019)
summary(mod_eba_2019, ref = NULL)

qv_eba_2019<- qvcalc(mod_eba_2019)
summary(qv_eba_2019)
#plot(qv_9map, xlab = "genotypes", ylab = "estimate", main = NULL)

coef(summary(mod_eba_2019))

qv_eba_2019<- qvcalc(mod_eba_2019)
qv_eba_2019$qvframe <- qv_eba_2019$qvframe[order(coef(mod_eba_2019)),]
plot(qv_eba_2019, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_eba_2019$qvframe), las = 2, cex.axis = 0.6)


# make correlation with the estimates of 2019
all_timepoint2019<-data.frame( qv_3map_2019$qvframe$estimate, qv_6map_2019$qvframe$estimate, qv_9map_2019$qvframe$estimate, qv_garri_2019$qvframe$estimate, qv_eba_2019$qvframe$estimate)
colnames(all_timepoint)
qv_9map_2019$
#newname_timepoint<-rename(all_timepoint,AMAP=qv_1map$qvframe$estimate, BMAP=qv_3$qvframe$estimate, CMAP=qv_6$qvframe$estimate, DMAP=qv_9$qvframe$estimate)

cor_placketluce_estimate2019<-round(cor(all_timepoint2019,use="complete.obs"),3)
cor_placketluce_estimate2019

library(xtable)
corr_table_2019<-xtable(cor_placketluce_estimate2019)

#write.csv(corr_table_2019,file("2019 estimate correlation.csv"))
write_csv(corr_table_2019,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2019 estimate correlation.csv")





#2020 harvest data
tricot_harvest_2020<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2020 harvest tricot.csv")
dim(tricot_harvest_2020)
Rank_Yield_2020<- rank_tricot(data =tricot_harvest_2020,
                             items = c("Variety.A" ,"Variety.B","Variety.C"),
                             input = c("Root.trait.Overall.impression.Best", "Root.trait.overall.impression.Worst"))

dim(Rank_Yield_2020)

# for missing data 
library("PlackettLuce")
library("gosset")
library("janitor")


fillNAs <- function(x){
  
  for (i in seq_along(x)) {
    
    if (is.na(x[i])) {
      x[i] <- x[i-1]
    }
    
  }
  
  x
}


tricot_harvest_2020<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/2020 harvest tricot.csv")
dim(tricot_harvest_2020)



keep <- apply(tricot_harvest_2020[c("overall.impression.garri.trait.Best", "overall.impression.garri.trait..Worst")], 1, is.na)
keep <- as.vector(colSums(keep) == 0)

# no ties
keep2 <- apply(tricot_harvest_2020[c("overall.impression.garri.trait.Best", "overall.impression.garri.trait..Worst")], 1, function(x) {
  x[1] != x[2]
})

keep2 <- keep2 == 1

keep <- keep & keep2

# use this vector to filter the data, is likely that this will not work with other traits
# so it need to be updated for each trait
# we supply the vector direct into the function in gosset
?rank_tricot

# an object of class rankings 
Rank_harvest_2020 <- rank_tricot(tricot_harvest_2020[keep, ], # HERE!
                 items = c("Variety.A", "Variety.B", "Variety.C"),
                 input = c("overall.impression.garri.trait.Best", "overall.impression.garri.trait..Worst"))

# or grouped rankings
G <- rank_tricot(tricot_harvest_2020[keep, ], # HERE!
                 items = c("Variety.A", "Variety.B", "Variety.C"),
                 input = c("overall.impression.garri.trait.Best", "overall.impression.garri.trait..Worst"),
                 group = TRUE)

#G


#PlackettLuce(R)

dim(Rank_harvest_2020)

un_Rank_harvest_2020<-unclass(Rank_harvest_2020)

adj_harvest_2020<-adjacency(Rank_harvest_2020)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_harvest_2020<-network(adj_harvest_2020)
plot(adj_net_harvest_2020)

favor_harvest_2020<-summarise_favourite(Rank_harvest_2020) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_harvest_2020)
plot(favor_harvest_2020)+ xlab("genotype")+ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_harvest_2020,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_yield_2020.csv")

mod_harvest_2020 <- PlackettLuce(Rank_harvest_2020, npseudo = 0.5)
summary(mod_harvest_2020)
summary(mod_harvest_2020, ref = NULL)

qv_harvest<- qvcalc(mod_harvest_2020)
summary(qv_harvest_2020)


coef(summary(mod_harvest_2020))

qv_harvest_2020<- qvcalc(mod_harvest_2020)
qv_harvest_2020$qvframe <- qv_harvest_2020$qvframe[order(coef(mod_harvest_2020)),]
plot(qv_harvest_2020, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_harvest_2020$qvframe), las = 2, cex.axis = 0.6)




newnames_harvest <- make_clean_names(names(tricot_harvest_2020))
newnames_harvest

newnames_harvest <- gsub("NA|[...]|[/]| |[0-9]+", "", newnames_harvest)
newnames_harvest <- gsub("x_", NA, newnames_harvest)
newnames_harvest<- fillNAs(newnames_harvest)

index <- which(duplicated(newnames_harvest))
newnames_harvest[index] <- paste0(newnames_harvest[index], "_worst")

newnames_harvest[index-1] <- paste0(newnames_harvest[index-1], "_best")


### Kaeu script
# the empty cells should be red as NAs
#dat <- read.csv("data/part of yield data.csv", na.strings = c(".", ""))

#dat
# as I don't have any variety name I will supply it here
#genotypes <- read.csv("data/varieties.csv")

# we need to fix the col names before proceeding
newnames <- make_clean_names(names(dat))
newnames
# remove special chars 
newnames <- gsub("NA|[...]|[/]| |[0-9]+", "", newnames)
newnames <- gsub("x_", NA, newnames)
newnames <- fillNAs(newnames)

# add the worst side to each trait
index <- which(duplicated(newnames))

newnames[index] <- paste0(newnames[index], "_worst")

newnames[index-1] <- paste0(newnames[index-1], "_best")

# remove line 1 
dat <- dat[-1, ]

names(dat) <- newnames

# combine with varieties names
dat <- cbind(genotypes, dat)

# now check the rankings I will use only the first yield, 
# but you can generalize it for the other traits using a loop 
# and providing the traits as strings

# check for data completeness in this trait
# should return a vector with TRUE, FALSE, 
# where TRUE = complete, FALSE = missing
keep <- apply(dat[c("yield_best", "yield_worst")], 1, is.na)
keep <- as.vector(colSums(keep) == 0)

# no ties
keep2 <- apply(dat[c("yield_best", "yield_worst")], 1, function(x) {
  x[1] != x[2]
})

keep2 <- keep2 == 1

keep <- keep & keep2

# use this vector to filter the data, is likely that this will not work with other traits
# so it need to be updated for each trait
# we supply the vector direct into the function in gosset
?rank_tricot

# an object of class rankings 
R <- rank_tricot(dat[keep, ], # HERE!
                 items = c("varietya", "varietyb", "varietyc"),
                 input = c("yield_best", "yield_worst"))

# or grouped rankings
G <- rank_tricot(dat[keep, ], # HERE!
                 items = c("varietya", "varietyb", "varietyc"),
                 input = c("yield_best", "yield_worst"),
                 group = TRUE)

G


PlackettLuce(R)











all_timepoint<-data.frame(qv_1map$qvframe$estimate, qv_3$qvframe$estimate, qv_6$qvframe$estimate, qv_9$qvframe$estimate,qv_garri$qvframe$estimate)
colnames(all_timepoint)
# Analysis for garri 2020
Rank_garri_2020<- rank_tricot(data =tricot_harvest_2020,
                             items = c("Variety.A" ,"Variety.B","Variety.C"),
                             input = c("overall.impression.garri.trait.Best", "overall.impression.garri.trait..Worst"))

dim(Rank_garri)

un_Rank_garri<-unclass(Rank_garri)

adj_garri<-adjacency(Rank_garri)# to know how the varieties are connected to the other Ie how many time varieties was prefered
adj_net_garri<-network(adj_garri)
plot(adj_net_garri)

favor_garri<-summarise_favourite(Rank_garri) # looking for favorite varieties describes the data shows how many time one variety wins against the other
View(favor_garri)
plot(favor_garri)+ xlab("genotype")+ylab("score")+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_garri,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_garri.csv")

mod_garri <- PlackettLuce(Rank_garri, npseudo = 0.5)
summary(mod_garri)
summary(mod_garri, ref = NULL)

qv_garri<- qvcalc(mod_garri)
summary(qv_garri)
#plot(qv_9map, xlab = "genotypes", ylab = "estimate", main = NULL)

coef(summary(mod_garri))

qv_garri<- qvcalc(mod_garri)
qv_garri$qvframe <- qv_garri$qvframe[order(coef(mod_garri)),]
plot(qv_garri, xlab = NULL, ylab = "Estimate", main = NULL,
     xaxt="n", xlim = c(1, 30))
axis(1, at = seq_len(30), labels = rownames(qv_garri$qvframe), las = 2, cex.axis = 0.6)


#best_all_rename<-rename(best_all,Best_1MAP=favor_1map.best, Best_3MAP=favor_3map.best, Best_6MAP=favor_6map.best, Best_9MAP=favor_9map.best )



