from scipy import stats
import scipy as sp
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt

###### part a
def NetworkGenerator(model = None, nodes = 100, probability = 0.02, parameter = 10, times = 100):
    # iterate n times
    res = {}
    res['number_component'] = []
    res['percentage_of_largest'] = []
    res['diameter_of_largest'] = []
    res['ave_shortest_path_of_largest'] = []
    res['ave_degree'] = []
    res['ave_clustering_coefficient'] = []
    if model == nx.erdos_renyi_graph:
        model = 1
    elif model == nx.watts_strogatz_graph:
        model = 2
    elif model == nx.barabasi_albert_graph:
        model = 3
    else:
        stop()
    # start building graphs
    for i in xrange(times):
        # create the erdos_renyi graph
        if model % 3 == 1:
            g = nx.erdos_renyi_graph(n = nodes, p = probability)
        elif model % 3 == 2:
            g = nx.watts_strogatz_graph(n = nodes, p = probability, k = parameter)
        elif model % 3 == 0:
            g = nx.barabasi_albert_graph(n = nodes, m = parameter)
        else:
            stop()
        # get number of components (n_c)
        n_c =nx.number_connected_components(g)
        # get percentage of largest component  (p_lc)
        ## find the largest component (lc)
        lc = max(nx.connected_component_subgraphs(g), key = len)
        p_lc = len(lc)/ (nodes * 1.0)
        # get diameter of largest component
        d_lc = nx.diameter(lc)
        # get average shortest path (asp) of lc
        asp_lc = nx.average_shortest_path_length(lc)
        # get average degree (ad) of graph
        ad_g = np.mean(g.degree().values())
        # get average clustering coefficient (acc) of graph
        acc_g = nx.average_clustering(g)
        # store all results in a dictionary
        res['number_component'].append(n_c)
        res['percentage_of_largest'].append(p_lc)
        res['diameter_of_largest'].append(d_lc)
        res['ave_shortest_path_of_largest'].append(asp_lc)
        res['ave_degree'].append(ad_g)
        res['ave_clustering_coefficient'].append(acc_g)
    return res

def CIfinder(values):
    n, min_max, mean, var, skew, kurt = stats.describe(values)
    std=np.sqrt(var)
    CI = stats.norm.interval(0.05, loc=mean, scale=std)
    if np.isnan(CI[0]) or np.isnan(CI[1]):
        return (1,1) 
    return CI

# calculate the confidence interval of each key
d = NetworkGenerator(model = nx.erdos_renyi_graph, nodes = 100, probability =  0.02, times = 1000)
for key, values in d.items():    
    print 'CI of "%s" is \n\t %s' % (key, CIfinder(values))



###### part b
g = nx.Graph(nx.read_pajek("LadaFacebookAnon.net"))
# get number of components
n_c = nx.number_connected_components(g)
# get percentage of largest component  (p_lc)
## find the largest component (lc)
lc = max(nx.connected_component_subgraphs(g), key = len)
# get number of nodes
n_n = len(g.nodes())
p_lc = len(lc)/ (n_n * 1.0)
# get number of edges
n_e = len(g.edges())
print 'The summary of Lada Facebook network'
print '\t # of components is %s' % n_c
print '\t percentage of largest components is %s' % p_lc
print '\t # of nodes is %s' % n_n
print '\t # of edges is %s \n' % n_e
# largest component statistics
n_lc =nx.number_connected_components(lc)
d_lc = nx.diameter(lc)
asp_lc = nx.average_shortest_path_length(lc)
# get average degree (ad) of graph
ad_lc = np.mean(lc.degree().values())
# get average clustering coefficient (acc) of graph
acc_lc = nx.average_clustering(lc)
llc = max(nx.connected_component_subgraphs(lc), key = len)
p_llc = len(llc)/ (len(lc.nodes()) * 1.0)
n_nlc = len(lc.nodes())
print 'The summary of the largest component in Lada Facebook network'
print '\t # of components is %s' % n_lc
print '\t percentage of largest components is %s' % p_llc
print '\t diameter is %s' % d_lc
print '\t average degree is %s' % ad_lc
print '\t average shortest path is %s' % asp_lc
print '\t average clustering coefficient is %s' % acc_lc


###### part c
# create erdos_renyi_graph
g = nx.erdos_renyi_graph(n = 350, p = 0.057)
n_c = nx.number_connected_components(g)
# get percentage of largest component  (p_lc)
## find the largest component (lc)
lc = max(nx.connected_component_subgraphs(g), key = len)
# get number of nodes
n_n = len(g.nodes())
p_lc = len(lc)/ (n_n * 1.0)
# get number of edges
n_e = len(g.edges())
print 'the i is %s' % i
print ' %s \n %s \n %s \n %s\n\n' % (n_c, p_lc, n_n, n_e)

# create watts_strogatz_graph
g = nx.watts_strogatz_graph(n = 350, k = 20, p = 0.2)
n_c = nx.number_connected_components(g)
# get percentage of largest component  (p_lc)
## find the largest component (lc)
lc = max(nx.connected_component_subgraphs(g), key = len)
# get number of nodes
n_n = len(g.nodes())
p_lc = len(lc)/ (n_n * 1.0)
# get number of edges
n_e = len(g.edges())
print ' %s \n %s \n %s \n %s' % (n_c, p_lc, n_n, n_e)

# create a barbasi_albert_graph 
g = nx.barabasi_albert_graph(n = 350, m = 10)
n_c = nx.number_connected_components(g)
# get percentage of largest component  (p_lc)
## find the largest component (lc)
lc = max(nx.connected_component_subgraphs(g), key = len)
# get number of nodes
n_n = len(g.nodes())
p_lc = len(lc)/ (n_n * 1.0)
# get number of edges
n_e = len(g.edges())
print ' %s \n %s \n %s \n %s' % (n_c, p_lc, n_n, n_e)



###### part d
# erdos_renyi_graph
d = NetworkGenerator(model = nx.erdos_renyi_graph, nodes = 350, probability = 0.057, times = 1000)
print 'Erdos Renyi graph summary:'
# calculate the confidence interval of each key
for key, values in d.items():    
    print 'CI of "%s" is \n\t %s' % (key, CIfinder(values))

# watts_strogatz_graph summary
d = NetworkGenerator(model = nx.watts_strogatz_graph, nodes = 350, parameter = 20, probability = 0.2, times = 1000)
print 'Watts Strogatz graph summary:'
# calculate the confidence interval of each key
for key, values in d.items():    
    print 'CI of "%s" is \n\t %s' % (key, CIfinder(values))

# barabasi_albert_graph
d = NetworkGenerator(model = nx.barabasi_albert_graph, nodes = 350, parameter = 10, times = 1000)
print 'Barabasi Albert graph summary:'
# calculate the confidence interval of each key
for key, values in d.items():    
    print 'CI of "%s" is \n\t %s' % (key, CIfinder(values))


###### part e
g = nx.Graph(nx.read_pajek("LadaFacebookAnon.net"))
lc = max(nx.connected_component_subgraphs(g), key = len)
g = lc

n_c = nx.number_connected_components(g)
asp_lc = nx.average_shortest_path_length(g)
acc_g = nx.average_clustering(g)
d_lc = nx.diameter(g)

lc = max(nx.connected_component_subgraphs(g), key = len)
n_n = len(g.nodes())
p_lc = len(lc)/ (n_n * 1.0)
ad_g = np.mean(g.degree().values())
print ' %s \n %s \n %s \n %s \n %s \n %s' % (n_c, asp_lc, acc_g, d_lc, p_lc, ad_g)

