import sys, subprocess, glob, os, shutil, re, importlib
from subprocess import call
import baltic as bt
import matplotlib as mpl
from matplotlib import pyplot as plt
import matplotlib.patheffects as path_effects
import matplotlib.lines as mlines
from matplotlib.font_manager import FontProperties
import matplotlib.colors as clr
import pandas as pd
import textwrap as textwrap
from textwrap import wrap
import numpy as np
from scipy.special import binom


def get_taxa_lines(tree_path):    
    
    # write out a temp tree file
    temp_tree = tree_path.replace(".trees",".temp.tree")
    with open(temp_tree, "w") as outfile: 
        outfile.write("")

    lines_to_write = ""
    with open(tree_path) as infile:
        for line in infile: ## iterate through each line
            if 'state' not in line.lower(): #going to grab all the interesting stuff in the .trees file prior to the newick tree strings
                lines_to_write = lines_to_write + line

    return(lines_to_write)

def get_burnin_value(tree_path, burnin_percent):
    with open(tree_path) as infile:
        numtrees = 0
        for line in infile: ## iterate through each line
            if 'state' in line.lower(): #going to grab all the interesting stuff in the .trees file prior to the newick tree strings
                numtrees += 1
    
    burnin = numtrees * burnin_percent
    return(burnin)

def retrieve_subtrees(tree):
    
    traitName='domwildbyb'

    tree.root.traits[traitName]='ancestor' ## give root node some trait value that's different from what the actual tree root has, so it registers as a switch

    tree_strings={division:[] for division in division_order}
    subtype_trees={division:[] for division in division_order}

    for k in sorted(tree.Objects,key=lambda x:x.height):
        kp=k.parent     # kp is the parent node of k

        ## get current node's (k) and its parent's (kp) trait states
        kloc=k.traits[traitName]      # kloc = trait of k; kc = trait of k; they are the same thing
        if traitName in k.parent.traits:       # if parent has a trait block, use that trait, else assign to ancestor
            kploc=kp.traits[traitName]              # kploc = trait of parental node
        else:
            kploc='ancestor'

        ## if states do not match
        if kloc!=kploc:      # if node and parental node do not have the same trait
            #N_children=len(k.leaves)
            traverse_condition=lambda w:w.traits[traitName]==kloc     # traverse tree for all nodes whose traitname = kc
            #print('subtree resulting from %s>%s switch, traversing within %s')%(kploc,kloc,kloc)

            subtree=tree.subtree(k,traverse_condition=traverse_condition) #traitName = traitName ## this function returns a new baltic object that contains a trait-traversed subtree, starting from node k, for as long as the traversal stays within the starting trait value state

            if subtree != None:
                subtree.traverse_tree()
                subtree.sortBranches()
                subtype_trees[kloc].append((kploc,subtree))
    
    return(subtype_trees)

def count_transitions(tree_path,temp_tree):
    
    ## count transitions in the posterior set of trees 
    trees_per_file={}
    transitions_dict = {}
    treeCounter = 0

    # open .trees file and read in line by line
    with open(tree_path) as infile:
        for line in infile: ## iterate through each line

            # write an empty tree to read file; this will get filled in as a new nexus file for each tree in the posterior
            with open(temp_tree, "w") as outfile:
                outfile.write("")

            # if tree line...
            if 'tree STATE_' in line:
                treeCounter += 1
                identifier = " ".join(line.split(" ")[0:2])

                if treeCounter > burnin:

                    with open(temp_tree, "a") as outfile:
                        outfile.write(lines_to_write)
                        outfile.write(line)
                    tree = bt.loadNexus(temp_tree)

                    # run this function to retrieve all of the transitions into domestic (i.e., all the domestic subtrees)
                    subtype_trees = retrieve_subtrees(tree)

                    # output the number of domestic subtrees to dictionary
                    transitions_dict[identifier] = len(subtype_trees['Domestic'])

                    with open(dict_outfile, "a") as outfile: 
                        outfile.write(identifier + "\t" + str(len(subtype_trees['Domestic'])) + "\n")

division_order = ["Domestic","Backyard_bird","Wild"]
                 
# specify tree path
tree_path = "../RF-wild-1_1_75.trees"
#tree_path = "/Users/lmoncla/Documents/Mumps/beast/DTA/2020-10-23-skygrid-25-exp-bssvs-prior/offset-26/mumps-north-american-seqs-DTA-2020-01-16.aligned.no-metadata.trees"

# specify burnin percentage
dict_outfile = tree_path.replace(".trees",".transitionscomm.txt")
temp_tree = tree_path.replace(".trees",".tempcomm.tree")
burnin_percent = 0.10

with open(dict_outfile, "w") as outfile:
    outfile.write("state\tnumber_transitions\n")

# get taxa line and burnin number
lines_to_write = get_taxa_lines(tree_path)
burnin = get_burnin_value(tree_path, burnin_percent)
print(burnin)

count_transitions(tree_path,temp_tree)


