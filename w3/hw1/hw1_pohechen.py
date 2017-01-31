#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 25 11:25:58 2017

@author: CHENPoHeng
"""

import networkx as nx




g = nx.read_gml('sj.gml')

g.degree = nx.degree_centrality(g)
g.close = nx.closeness_centrality(g)
g.between = nx.betweenness_centrality(g)

g.id = []
g.status = []
g.betweenness = []
g.degree_centrality = []
g.closeness = []

for i in g.nodes(True):
    g.id.append(i[0])
    g.status.append(i[1]['status'])
    g.betweenness.append(g.between[i[0]]) 
    g.degree_centrality.append(g.degree[i[0]])
    g.closeness.append(g.close[i[0]])

# calculate correlations 
import scipy.stats
scipy.stats.pearsonr(g.status, g.betweenness)
scipy.stats.pearsonr(g.status, g.degree_centrality)
scipy.stats.pearsonr(g.status, g.closeness)


# calculate p prestige 
g.sum_shortest_paths = {}
g.all_pairs = nx.all_pairs_dijkstra_path_length(g)
for i in g.all_pairs:
    for j in g.all_pairs[i]:
        if j in g.sum_shortest_paths:
            g.sum_shortest_paths[j][0] += g.all_pairs[i][j]
            g.sum_shortest_paths[j][1] += 1
        else:
            g.sum_shortest_paths[j] = [g.all_pairs[i][j], 0]

g.prestige = []
for i in g.id:
    if g.sum_shortest_paths[i][1] == 0:
        g.prestige.append(0)
    else:
        denominator = g.sum_shortest_paths[i][0]*(len(g.sum_shortest_paths)-1)
        numerator = g.sum_shortest_paths[i][1]**2
        g.prestige.append(numerator*1.0/denominator)

scipy.stats.pearsonr(g.status, g.prestige)
        
        