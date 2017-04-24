setwd('~/Documents/umsi/SI608/project/')
# options(scipen = 999)
len = length
library(igraph)
library(data.table)
library(dplyr)

#########################################
## Goal: to predict potential investor ##
#########################################
# load in data
data = read.csv('data/investments.csv', as.is = T)
data = data.table(data)

# remove redundant columns
i = which(names(data) %in% c('company_name', 'investor_name', 'funded_at'))
if(len(i)) data = data[, i, with = F]

# remove self-investment
i = which(data$company_name == data$investor_name)
if(len(i)) data = data[-i,]

# get only 2014 data
data$year = substr(data$funded_at, 1, 4)
i = which(data$year == '2014')
d = data[i, ]

# check if a start-up is also an investor
i = which(d$company_name %in% d$investor_name)
if(len(i)) d = d[-i, ]

# # make a graph
# # create relations (edges)
# from = d$company_name
# to = d$investor_name
# all_edges = data.frame(from = from, to = to)
# g = graph.data.frame(all_edges, directed = F)
# V(g)
# 
# # create bipartite 
# ## http://stackoverflow.com/questions/15367779/how-to-create-a-bipartite-network-in-r-with-igraph-or-tnet
# V(g)$type = V(g)$name %in% d$company_name


# predicted result
library('Matrix') # for sparse matrix
m = Matrix(data = 0, nrow = len(unique(d$company_name)),
       ncol = len(unique(d$investor_name)), sparse = T)
rownames(m) = unique(d$company_name)
colnames(m) = unique(d$investor_name)
# special case
d[which(d$investor_name == ''),]$investor_name = 'Anonymous'
colnames(m)[which(colnames(m) == '')] = 'Anonymous'


if(file.exists('finalMatrixResult.Rdata')){
    load('finalMatrixResult.Rdata')
} else {
    flag = 0
    for(s in unique(d$company_name)){
        flag = flag + 1
        # find startups' investors
        cat(sprintf('working on %s out of %s\n', flag, len(unique(d$company_name))))
        investor_1 = unique(d[d$company_name == s]$investor_name)
        tmp = list()
        # find investors' startups
        for(i1 in investor_1){
            tmp[[i1]] = 1/len(investor_1)
            startup_1 = unique(d[d$investor_name %in% i1]$company_name)
            startup_1 = startup_1[-which(startup_1 %in% s)]
            # m[startup_1, i1] = m[s,i1] / len(startup_1)
            if(len(startup_1) == 0) {
                m[s, i1] = tmp[[i1]]
            } else {
                for(s1 in startup_1){
                    tmp[[s1]] = tmp[[i1]] / len(startup_1)
                    investor_2 = unique(d[d$company_name %in% s1]$investor_name)
                    investor_2 = investor_2[-which(investor_2 == i1)]
                    if(len(investor_2) == 0) {
                        if(m[s, i1] == 0){
                            m[s, i1] = tmp[[s1]]
                        } else {
                            m[s, i1] = m[s, i1] + tmp[[s1]]
                        }
                    } else {
                        for(i2 in investor_2){
                            if(m[s, i2] == 0){
                                m[s, i2] = tmp[[s1]] / len(investor_2)
                            } else {
                                m[s, i2] = m[s, i2] + tmp[[s1]] / len(investor_2)
                            }
                        }
                    }
                }
            }
        }
    }
    save(m, file =  'matrix_result.Rdata')
}



# validation
d2014 = d
i = which(data$year == '2015')
d2015 = data[i, ]
# check if a start-up is also an investor
i = which(d2015$company_name %in% d2015$investor_name)
if(len(i)) d2015 = d2015[-i, ]
i = which(d2015$investor_name == '')
d2015[i, ]$investor_name = 'Anonymous'


# check how much the intersection between 2014 and 2015 company name
d4 = d2014
d5 = d2015

# get the companies which got funding at both 2014 and 2015
i = which(unique(d4$investor_name) %in% unique(d5$investor_name))
len(i)
i = which(unique(d4$company_name) %in% unique(d5$company_name))
company_name = unique(d4$company_name)[i]
len(i)
len(i)/len(unique(d4$company_name))


## get the predicted result for these companies
m = m[company_name, ]
# get the top three predicted potential investors

N = 3
topN = lapply(1:nrow(m), function(x) {
    # ### don't get 2014 investor
    i = d2014[d2014$company_name == rownames(m)[x], ]$investor_name
    tmp = m[x, !colnames(m) %in% i]
    tmp = tmp[ which(tmp != 0)]
    if(len(tmp) >= 3){
        i = order(tmp,decreasing = T)[1:N]    
    } else {
        i = order(tmp, decreasing = T)[1:len(tmp)]
    }
    # i = order(m[x,],decreasing = T)[1:N]
    col = names(tmp[i])
    val = tmp[i]
    data.table(rownames(m)[x], col, val)
})

topN = do.call(rbind, topN)
names(topN) = c('company_name', 'potential_investor', 'probability')

i = which(d5$company_name %in% company_name)
d5 = d5[i, ]
i = which(d4$company_name %in% company_name)
d4 = d4[i, ]

predicted_result = list()
for(i in unique(d5$company_name)) {
    predicted_result[[i]] = 
        data.frame(
            got = len(which(d5[d5$company_name == i, ]$investor_name %in% 
                                topN[topN$company_name == i, ]$potential_investor)),
            dontgot = len(which(!d5[d5$company_name ==i, ]$investor_name %in% 
                                    d4[d4$company_name ==i, ]$investor_name))
        )
}

predicted_result = do.call(rbind, predicted_result)
predicted_result
tmp = table(predicted_result)
tmp

## use MAP to evaluate 


map = list()
flag = 1
for(i in 2:ncol(tmp)){
    for(j in 1:nrow(tmp)){
        map[[flag]] = c(tmp[j, i]*(j-1)/(i+j-2), tmp[j, i])
        flag = flag + 1
    }
}
map = do.call(rbind, map)


print ((sum(map[,1]))/sum(tmp))




#################
## Random Walk ##
#################
from = d4$company_name
to = d4$investor_name
all_edges = data.frame(from = from, to = to)
g = graph.data.frame(all_edges, directed = F)
load('predicted_rw.Rdata')
# predicted_rw = list()
for(i in len(predicted_rw):len(V(g))){
    tmp = list()
    start = names(V(g)[i])
    for(j in 1:100){
        end = random_walk(g, start = start, steps = 4)[4]
        tmp[[j]] = c(start, names(end))
    }
    predicted_rw[[i]] = do.call(rbind, tmp)    
    print(sprintf('running %s out of %s', i, len(V(g))))
}

# save(predicted_rw, file = 'predicted_rw.Rdata')
predicted_rw = do.call(rbind, predicted_rw)
predicted_rw = as.data.table(predicted_rw)
save(predicted_rw, file = 'datatable_rw.Rdata')
load('datatable_rw.Rdata')

tmp = predicted_rw %>% group_by(V1, V2) %>% summarize(n = n())
tmp$p = tmp$n / 100
names(tmp) = c('startup', 'potential_investor', 'num_invest', 'percentage')
tmp$startup[1]

N = 3
topN = tmp %>%
    group_by(startup) %>%
    arrange(desc(percentage)) %>%
    slice(1:N)

### validate
i = which(d5$company_name %in% company_name)
d5 = d5[i, ]
i = which(d4$company_name %in% company_name)
d4 = d4[i, ]
topN$potential_investor = as.character(topN$potential_investor)
topN$startup = as.character(topN$startup)

d4$funded_at = NULL
d5$funded_at = NULL
d4 = unique(d4)
d5 = unique(d5)
predicted_result = list()
for(i in unique(d5$company_name)) {
    predicted_result[[i]] = 
        data.frame(
            got = len(which(d5[d5$company_name == i, ]$investor_name %in% 
                                topN[topN$startup == i, ]$potential_investor)),
            dontgot = len(which(d5[d5$company_name ==i, ]$investor_name %in% 
                                    d4[d4$company_name ==i, ]$investor_name))
        )
}

predicted_result = do.call(rbind, predicted_result)
predicted_result
tmp = table(predicted_result)
tmp


map = list()
flag = 1
for(i in 2:ncol(tmp)){
    for(j in 1:nrow(tmp)){
        map[[flag]] = c(tmp[j, i]*(j-1)/(i+j-2), tmp[j, i])
        flag = flag + 1
    }
}
map = do.call(rbind, map)


print ((sum(map[,1]) + sum(tmp[,1]))/sum(tmp))
