#######################################
## Goal: basic investor/startup plot ##
#######################################
## see the investor and startup distribution
## investor: how many investment investor makes?
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
    scale_y_log10('number of organization') +
    ggtitle('Investor vs Startup - Investment Distribution') +
    theme_light()
ggsave('plot/investor_startup.png')
