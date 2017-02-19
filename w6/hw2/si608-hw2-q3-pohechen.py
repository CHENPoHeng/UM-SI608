from scipy import stats
import scipy as sp
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt


g = nx.read_gml("BowTie.gml")


def bowtie(g):
    OUT = len([n for n in g.out_degree().items() if n[1] == 0])
    IN = len([n for n in g.in_degree().items() if n[1] == 0])
    SCC = len(g) - IN - OUT
    return [IN, OUT, SCC]

print bowtie(g)