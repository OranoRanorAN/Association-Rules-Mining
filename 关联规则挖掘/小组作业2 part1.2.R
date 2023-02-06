library(tidyverse) # data manipulation
library(igraph)
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics
library(RColorBrewer)
library(ggplot2)
#library("rJava")
#library("rCBA")
#install.packages('arulesViz')
#remove.packages('Matrix')


trans = read.transactions("order_products_train.csv", format="single", cols=c(1,2), sep=",")
trans2 = read.transactions("order_products_name.csv", format="single", cols=c(1,2), sep=",",quote='')
# Transaction object
trans
summary(trans)
glimpse(trans)
str(trans)
# Absolute Item Frequency Plot
itemFrequencyPlot(trans2, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")
#relative Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="relative", col="#d1c6d4",xlab="Item name", 
                  ylab="Frequency (relative)", main="Top15 Item Frequency Plot")


#computing time
support = c(0.05, 0.01, 0.005)
time = function(n){
  t=integer(length=5)
  for(i in 1:5){
    old=Sys.time()
    apriori(trans, parameter=list(sup=support[n],target="rules"))
    new=Sys.time()
    t[i]=as.numeric(new-old)
  }
  return(mean(t))
}
rapr=integer(length=length(support))
for (i in 1:length(support)) {
  rapr[i]=time(i)
}
#time comparision
support=c('0.05','0.01','0.005')
apr=c(1.140655,1.244092,1.288347)
apro=c(5.3518373489379885, 26.304054975509644, 145.94544458389282)
aprs=c(0.5611345767974854, 4.21549654006958, 26.286905431747435)
fpgo=c(7.159727382659912, 7.4995135307312015, 8.627013158798217)
fpgs=c(0.9260592937469483, 1.8784759998321534, 2.913156032562256)
df=data.frame(apr,apro,aprs,fpgo,fpgs)
ggplot(data=df, aes(x=support,group=1)) +
  geom_line(aes(y=apr, colour="apriori in R")) +
  geom_point(aes(y=apr, colour="apriori in R")) +
  geom_line(aes(y=apro, colour="apriori in python")) +
  geom_point(aes(y=apro, colour="apriori in python")) +
  geom_line(aes(y=fpgo, colour="fpgrowth in python")) +
  geom_point(aes(y=fpgo, colour="fpgrowth in python")) +
  geom_line(aes(y=fpgs, colour="fpgrowth in python(with sparse)")) +
  geom_point(aes(y=fpgs, colour="fpgrowth in python(with sparse)")) +
  geom_line(aes(y=aprs, colour="apriori in python(with sparse)")) +
  geom_point(aes(y=aprs, colour="apriori in python(with sparse)")) +
  scale_x_discrete(labels=c('0.005','0.01','0.05'))+
  labs(x="min_support", y="time used(second)", 
       title="time comparision") +
  theme_bw() +
  scale_color_manual(values=c('#e6c9ce','#d1c6d4',"#b8b1c9",'#9397ad','#414d64'))+
  theme(legend.title=element_blank())

 
# Support and confidence values
supportLevels <- c(0.05, 0.01, 0.005, 0.0025)
confidenceLevels <- c(0.5,0.45, 0.4, 0.35,0.3, 0.25,0.2,0.15, 0.10)
# Empty integers 
rules_sup0.25 <- integer(length=length(confidenceLevels))
rules_sup5 <- integer(length=length(confidenceLevels))
rules_sup1 <- integer(length=length(confidenceLevels))
rules_sup0.5 <- integer(length=length(confidenceLevels))

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  rules_sup5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  rules_sup1[i] <- length(apriori(trans, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)){
  rules_sup0.5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)){
  rules_sup0.25[i] <- length(apriori(trans, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
}


# Number of rules found with a support level of 5%
plot1 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", color=I('darkblue'),
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot2 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", color=I('darkblue'),
               main="Apriori with a support level of 1%") + 
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot3 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", color=I('darkblue'),
               main="Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks=seq(0, 220, 50)) +
  theme_bw()

# Number of rules found with a support level of 0.25%
plot4 <- qplot(confidenceLevels, rules_sup0.25, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", color=I('darkblue'),
               main="Apriori with a support level of 0.25%") + 
 # scale_y_continuous(breaks=seq(0, 800, 50)) +
  theme_bw()

# Subplot
apr=grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
ggsave("apr with different support levels.jpg",
       plot = apr,
       device = "jpg",
       width = 30,
       height = 20,
       units = c("cm"),
       limitsize = T)

# Data frame
num_rules <- data.frame(rules_sup5, rules_sup1, rules_sup0.5, rules_sup0.25, confidenceLevels)

# Number of rules found with a support level of 5%, 1% ， 0.5%，0.25%
com=ggplot(data=num_rules, aes(x=confidenceLevels)) +
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  # Plot line and points (support level of 0.25%)
  geom_line(aes(y=rules_sup0.25, colour="Support level of 0.25%")) +
  geom_point(aes(y=rules_sup0.25, colour="Support level of 0.25%")) +
  scale_y_continuous(breaks=seq(0, 700, 50)) +
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  scale_color_manual(values=c('#d1c6d4',"#b8b1c9",'#9397ad','#414d64'))+
  theme(legend.title=element_blank())

ggsave("apr with different support levels (combination).jpg",
       plot = com,
       device = "jpg",
       width = 20,
       height = 15,
       units = c("cm"),
       limitsize = T)

#set support=0.5% confidence=0.2
rules_sup0.5_conf25 <- apriori(trans2, parameter=list(sup=0.005, 
                                                   conf=0.25, target="rules"))
ordered_rules_sup0.5_conf25 <- sort(rules_sup0.5_conf25, by = "lift")
#提取最强的的20个规则
inspect(ordered_rules_sup0.5_conf25)
# 计算Φ-coefficient （关联规则的评价指标）
a=interestMeasure(ordered_rules_sup0.5_conf25[1:20], measure = 'phi', transactions = trans2)
a=data.frame(a)


# Visualization

# Scatter plot
plot(rules_sup0.5_conf25, measure=c("support", "confidence"), shading="lift",
     control=list(col = c('#153f77','lightgrey')))
# Graph (default layout)
plot(rules_sup0.5_conf25, method="graph",control=list(col = rev(brewer.pal(3, 'Blues'))))
# Graph (circular layout)
#plot(rules_sup0.5_conf20, method="graph", layout=igraph::in_circle())
plot(rules_sup0.5_conf25, method="grouped"
     ,control=list(col = rev(brewer.pal(3, 'Blues')))
     ,engine = "htmlwidget")

# Parallel coordinates plot
plot(rules_sup0.5_conf25, method="paracoord", control=list(reorder=TRUE,col = rev(brewer.pal(9, 'Blues'))))

plot(rules_sup0.5_conf25, method="graph",
     layout = 'linear', circular = TRUE,
     control=list(col = c('#153f77','lightgrey')))

plot(rules_sup0.5_conf25, method = "graph", asEdges = TRUE,shading="confidence",
     control=list(col = c('#1769ac','#f3f0f1')))

plot(rules_sup0.5_conf25, method = "graph", engine = "igraph",
     #control=list(col = rev(brewer.pal(9, 'Blues'))))
    nodeCol = rev(brewer.pal(9, 'Blues')), edgeCol = grey(.7), alpha = 1,)

plot(rules_sup0.5_conf25, method = "graph", engine = "igraph",
     plot_options = list(
       edge.lty = 2, 
       vertex.label.cex = 0.8, 
       margin = c(.1,.1,.1,.1)),
     nodeCol = rev(brewer.pal(9, 'Blues')), edgeCol = grey(.7),alpha = 0.6)
       #asp = .5))

#  
plot(ordered_rules_sup0.5_conf25[1:10], method = "graph", 
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .2,
         #col = rev(brewer.pal(9, 'Blues'))
       ),
       nodes = ggraph::geom_node_point(aes_string(size = "support", color = "lift")),
       nodetext = ggraph::geom_node_label(aes_string(label = "label"), alpha = .8, repel = TRUE)
     ),
     limit = 10
) + 
  scale_color_gradient(high='#6e9bc4',low='#e1e8ee') + 
  scale_size(range = c(2, 10)) 
