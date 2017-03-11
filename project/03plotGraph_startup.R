####################################################
## Goal: to plot graph of top N investor/startup ##
####################################################
### select top n startup by its number of investment 
## plot top n startup graph
d = data
tmp = d %>% group_by(company_name) %>% summarize(num_investment = n())
top_startup = tmp[with(tmp, order(-num_investment)), ][1:N, ]

i = which(d$company_name %in% top_startup$company_name)
if(len(i)) d = d[i, ]
# remove the cases where investors are also startup
i = which(d$company_name %in% d$investor_name)
if(len(i)) d = d[-i, ]

# to create graph
from = d$investor_name
to = d$company_name
all_edges = data.frame(from = from, to = to)
g = graph.data.frame(all_edges, directed = F)

# top n directed graph
# create color 
V(g)$color = "orange"
V(g)[which(names(V(g)) %in% top_startup$company_name)]$color = 'blue'
# assign size
V(g)$size = 2
V(g)[which(names(V(g)) %in% top_startup$company_name)]$size = 4
g.degree = degree(g, v = V(g), mode = 'in',
                  loops = TRUE, normalized = FALSE)

layout = layout.fruchterman.reingold(g)
png('top_startup_wo.png', width = 640, height = 640)
plot(g, layout = layout, vertex.size = map(g.degree, c(1,5)),
     vertex.label = NA, vertex.color = V(g)$color,
     edge.arrow.size = .1)
title(sprintf('Top %s Investor Graph', N))
dev.off()


####################################################################
### work on bipartite, which means there should only be one edge ###
####################################################################

d = d %>% group_by(investor_name, company_name) %>% summarize(num_investment = n())
dt = d
d = dt
d = d[1:100,]

# to create graph
from = d$investor_name
to = d$company_name
all_edges = data.frame(from = from, to = to)
g = graph.data.frame(all_edges, directed = F)

# top n directed graph
# create color 
V(g)$type = bipartite_mapping(g)$type
# assign size
g.degree = degree(g, v = V(g), mode = 'in',
                  loops = TRUE, normalized = FALSE)

layout = layout.fruchterman.reingold(g)
png('top_startup_single.png', width = 640, height = 640)
plot(g, layout = layout, vertex.size = map(g.degree,c(1,5)),
     vertex.label = NA, vertex.color = ifelse(V(g)$type,'blue','orange'),
     edge.arrow.size = .1)
title(sprintf('Top %s Investor Graph', N))
dev.off()

