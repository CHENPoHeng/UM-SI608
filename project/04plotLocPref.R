#####################################################
## Goal: to plot Region graph of investor/startup  ##
#####################################################
d = data
# plot graph by region and then use indegree/outdegree to color
## we don't use city because the distribution is inbalanced. (SF has too many cities)
# remove empty region
d = d %>% group_by(investor_region, company_region) %>% summarize(num_investment = n())
i = which(d$investor_region == '')
if(len(i)) d = d[-i, ]
i = which(d$company_region == '')
if(len(i)) d = d[-i, ]

## calculate investing to all other region
all = d %>% group_by(investor_region) %>% summarize(total_investment = sum(num_investment))
res = list()
for(region in unique(d$company_region)) {
    i = which(d$company_region == region)
    tmp = d[i, ]
    tmp = merge(all, tmp[,c('investor_region', 'num_investment')], by.x = 'investor_region', by.y = 'investor_region', all.x = T)    
    tmp[is.na(tmp$num_investment),]$num_investment = 0
    tmp$p_investment = tmp$num_investment / tmp$total_investment
    res[[region]] = c(region, summary(tmp$p_investment))
}

## calculate the percentage of each region invests itself and invests not-itself
i = which(d$investor_region == d$company_region)
tmp = d[i, ]
d = d %>% group_by(investor_region) %>% summarize(total_investment = sum(num_investment))

# remove which number of investment is too small
d = d %>% filter(total_investment > median(total_investment))

tmp = merge(d, tmp[,c('investor_region', 'num_investment')], by.x = 'investor_region', by.y = 'investor_region', all.x = T)
tmp[is.na(tmp$num_investment),]$num_investment = 0
tmp$p_self_investment = tmp$num_investment / tmp$total_investment
tmp = c('Self', summary(tmp$p_self_investment))
res[['Self']] = tmp

res = data.table(do.call(rbind, res))
rownames(res) = NULL
res$Mean = as.numeric(res$Mean)
res = res[with(res, order(-Mean)), ]


ggplot(res[1:5,], aes(x = reorder(V1, -Mean), y = Mean, fill = 'red', col = 'red')) +
    geom_bar(stat = 'identity', alpha = 0.6) +
    guides(fill = F, col = F) + 
    theme_light()+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    labs(x = 'region', y = 'percentage of investment') 
ggsave('plot/investment_region.png')
