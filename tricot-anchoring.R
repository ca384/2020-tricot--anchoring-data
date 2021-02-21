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
dim(orig_name)
dim(tricot_data)
str(orig_name)
str(tricot_data)

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

un_R<-unclass(R_1map)

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


Tricot_dat<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/tricot-1map-3map.csv")
str(Tricot_dat)

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
plot(favor_1map)+
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
plot(favor_3map)+
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
plot(favor_6map)+
  theme_bw()+
  theme(panel.grid = element_blank())

write_csv(favor_6map,"/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020-tricot-anchoring-github/Favorability_6MAP.csv")

