####################################################
## Goal: to plot graph of top N investor/startup ##
####################################################
### select top n investor by its number of investment 
d = data
tmp = d %>% group_by(investor_name) %>% summarize(num_investment = n())
top_investor = tmp[with(tmp, order(-num_investment)), ][1:N, ]


## plot top n investor graph
i = which(d$investor_name %in% top_investor$investor_name)
if(len(i)) d = d[i, ]
# remove the cases where investors are also startup
i = which(d$company_name %in% d$investor_name)
if(len(i)) d = d[-i, ]
dim(d)

## create name region dataframe
tmp = data.table(region = c(d$company_region, d$investor_region),
                 name = c(d$company_name, d$investor_name))
tmp = unique(tmp)
i = which(tmp$region == '')
if(len(i)) tmp = tmp[-i, ]
i = which(tmp$name == '')
if(len(i)) tmp = tmp[-i, ]
name_region = tmp

# to plot
from = d$investor_name
to = d$company_name
all_edges = data.frame(from = from, to = to)
g = graph.data.frame(all_edges, directed = T)

# create vertex attributes
V(g)$region = name_region[match(names(V(g)), name_region$name),]$region

# top 10 region
top_region = names((sort(table(name_region$region), decreasing = T))[1:N])

## create region color
region_color = data.frame(region = unique(V(g)$region), color = 1:len(unique(V(g)$region)))
i = match(top_region[1:3], region_color$region)
region_color[-i, ]$color = 'gray50'
V(g)$color = region_color[match(V(g)$region, region_color$region),]$color
# specify layout
layout = layout.fruchterman.reingold(g)

# use in-degree as their importance
# pr <- page.rank(g)$vector
g.degree = degree(g, v = V(g), mode = 'in',
                  loops = TRUE, normalized = FALSE)
# create investor name
V(g)$investor_name = c(names(V(g))[1:10], rep(NA_character_, len(names(V(g)))-10))

png('plot/top_investor_wo.png', width = 1024, height = 1024)
plot(g, layout = layout, vertex.size = 0.01, #map(g.degree, c(1,100))/20 ,#map(pr, c(1,100))/20,
     vertex.label = V(g)$investor_name,  vertex.label.cex = 3,
     vertex.label.color = '#f46524',
     edge.arrow.size = .1, edge.color="#55555533")
title(sprintf('Top %s Investor Graph', N), cex = 5)
dev.off()

png('plot/top_investor.png', width = 1024, height = 1024)
plot(g, layout = layout, vertex.size = map(g.degree, c(1,100))/20 ,#map(pr, c(1,100))/20,
     vertex.label = NA,
     edge.arrow.size = .1, edge.color="#55555533")
title(sprintf('Top %s Investor Graph scaled by indegree', N), cex = 5)
dev.off()