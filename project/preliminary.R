setwd('~/Documents/umsi/SI608/project/')
len = length
library(igraph)
library(data.table)
library(dplyr)
library(ggplot2)

#######################################
## Goal: basic investor & startup plot ##
#######################################
# load in data
data = read.csv('data/investments.csv', as.is = T)
data = data.table(data)

# remove redundant columns
i = which(names(data) %in% c('company_name','company_country_code','company_region',
                             'company_city','investor_name','investor_country_code',
                             'investor_region','investor_city'))
if(len(i)) data = data[, i, with = F]


## see the investor and startup distribution
# investor: how many investment investor makes?
d = data
d = d %>% group_by(investor_name) %>% summarize(num_investment = n ())
p = data.table(table(d$num_investment))
names(p) = c('num_investment', 'num_investor')
p$num_investment = as.integer(p$num_investment)

# startup: how many investment startup receives?
d = data
d = d %>% group_by(company_name) %>% summarize(num_investment = n ())
tmp = data.table(table(d$num_investment))
names(tmp) = c('num_investment', 'num_company')
tmp$num_investment = as.integer(tmp$num_investment)
p$num_company = as.integer(NA)
p[match(tmp$num_investment, p$num_investment), ]$num_company = tmp$num_company

# plot
ggplot(data = p) + 
    geom_point(aes(x = num_investment, y = num_investor, col = 'Investor'), size = 0.5) +
    geom_point(aes(x = num_investment, y = num_company, col = 'Start-up'), size = 0.5) +
    scale_x_log10('number of investment') +
    scale_y_log10('number of investor') +
    ggtitle('Investor vs Startup - Investment Distribution')

####################################
## Goal: to plot tripartite graph ##
####################################
d = data
# see the region distribution
tmp = sort(table(d$investor_region))
region = data.table(region = names(tmp), count = as.numeric(tmp))
p = data.table(table(region$count))
names(p) = c('num_trade', 'num_org')
p$num_trade = as.numeric(p$num_trade)
# basic 
ggplot(p, aes(num_trade, num_org)) + 
    geom_point(col = 'red') +
    scale_x_continuous('Number of Investment') + 
    scale_y_continuous('Number of Organization') +
    ggtitle('Investor by Region')
# log scale
ggplot(p, aes(num_trade, num_org)) + 
    geom_point(col = 'red') +
    scale_x_log10('Number of Investment') + 
    scale_y_log10('Number of Organization') +
    ggtitle('Investor by Region')

# randomly pick region and plot their relations
selected_region = c('Barcelona')
i = which(data$investor_region %in% selected_region)
d = data[i,]

## create tripartite graph
# create relations (edges)
from = c(d$investor_region, d$investor_name)#, d$company_name)
to = c(d$investor_name, d$company_name)#, d$company_region)
all_edges = unique(data.frame(from = from, to = to))
g = graph.data.frame(all_edges, directed = F)
# create layers
layers = c(rep(1, len(unique(d$investor_region))), 
           rep(2, len(unique(d$investor_name))),
           rep(3, len(unique(d$company_name))))
if(len(V(g)) != len(layers)) 'Sth wrong here!'
V(g)$layer <- layers
# source other's function
layout.k_partite <- function(g) {
    l <- layout.sugiyama(g)$layout[,2:1]
    l[,1] <- V(g)$layer
    l[,2] <- - l[,2] + 1 + max(l[,2])
    l
}
# plot
plot(g, layout = layout.k_partite(g), vertex.label='', vertex.size = 3)


###################################
## Goal: to calculate tripartite ##
###################################
d = data[0:5000,]
# n = sample(nrow(data), 7000)
# d = data[n,]
## create country region dataframe
tmp = data.table(city = c(data$company_city, data$investor_city),
                 country = c(data$company_country_code, data$investor_country_code))
tmp = unique(tmp)
i = which(tmp$city == '')
if(len(i)) tmp = tmp[-i, ]
i = which(tmp$country == '')
if(len(i)) tmp = tmp[-i, ]
city_country = tmp

## create tripartite graph
# create relations (edges)
from = d$investor_city
to = d$company_city
all_edges = data.frame(from = from, to = to)
g = graph.data.frame(all_edges, directed = F)
V(g)$country = city_country[match(names(V(g)), city_country$city),]$country

## create country color
country_color = data.frame(country = unique(V(g)$country), color = 1:len(unique(V(g)$country)))

V(g)$color = country_color[match(V(g)$country, country_color$country),]$color
plot(g, vertex.size = 3, vertex.label = '', vertex.color = V(g)$color)



