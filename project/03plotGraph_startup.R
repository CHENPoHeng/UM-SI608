####################################################
## Goal: to plot graph of top N investor/startup ##
####################################################
### select top n startup by its number of investment 
N = 50
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
png('')
plot(g, layout = layout, vertex.size = map(g.degree, c(1,5)),
     vertex.label = NA, vertex.color = V(g)$color,
     edge.arrow.size = .1)

############################
# work on bipartite, which means there should only be one edge
############################
# use in-degree as their importance
# pr <- page.rank(g)$vector




png('top_startup_wo.png', width = 640, height = 640)
plot(g, layout = layout, vertex.size = 0.01, vertex.color = V(g)$color,#map(g.degree, c(1,100))/20 ,#map(pr, c(1,100))/20,
     vertex.label = V(g)$investor_name,  vertex.label.font = 2, #vertex.label.dist=0.5,
     # vertex.label.color = 'Black',
     edge.arrow.size = .1, edge.color="#55555533")
title('Top10 Investor Graph')
dev.off()

png('top_startup.png', width = 640, height = 640)
plot(g, layout = layout, vertex.size = map(g.degree, c(1,100))/20 ,#map(pr, c(1,100))/20,
     vertex.label = NA,
     edge.arrow.size = .1, edge.color="#55555533")
title('Top10 Investor Graph scaled by indegree')
dev.off()

