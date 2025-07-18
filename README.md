# Intensive transmission in wild, migratory birds drove rapid geographic dissemination and repeated spillovers of H5N1 into agriculture in North America 

#### Lambodhar Damodaran*<sup>1</sup>, Anna Jaeger<sup>1</sup>, Louise H. Moncla<sup>3</sup>

<sup>1</sup>Department of Pathobiology, School of Veterinary Medicine, University of Pennsylvania, Philadelphia, Pennsylvania, United States


## Abstract 
Since late 2021, a panzootic of highly pathogenic H5N1 avian influenza virus has driven significant morbidity and mortality in wild birds, domestic poultry, and mammals. In North America, infections in novel avian and mammalian species suggest the potential for changing ecology and establishment of new animal reservoirs. Outbreaks among domestic birds have persisted despite aggressive culling, necessitating a re-examination of how these outbreaks were sparked and maintained. To recover how these viruses were introduced and disseminated in North America, we analyzed 1,818 Hemagglutinin gene sequences sampled from North American wild birds, domestic birds and mammals from November 2021-September 2023 using Bayesian phylodynamic approaches. The North American panzootic was driven by ~8 independent introductions into North America via the Atlantic and Pacific Flyways, followed by rapid dissemination westward via wild, migratory birds. Transmission was primarily driven by Anseriformes, shorebirds, and Galliformes, while species such as songbirds, raptors, and owls mostly acted as dead-end hosts. Unlike the epizootic of 2015, outbreaks in domestic birds were driven by ~46-113 independent introductions from wild birds, with some onward transmission. Backyard birds were infected ~10 days earlier on average than birds in commercial poultry production settings, suggesting that they could act as “early warning signals” for transmission upticks in a given area. Our findings support wild birds as an emerging reservoir for HPAI transmission in North America and suggest continuous surveillance of wild Anseriformes and shorebirds as crucial for outbreak inference. Future prevention of agricultural outbreaks may require investment in strategies that reduce transmission at the wild bird/agriculture interface, and investigation of backyard birds as putative early warning signs.

## Overview
This repo contains all of the code used to perform the analyses of HPAI in North America from 2021-2023. All sequence data used in this analysis were sourced from the GISAID repository and the corresponding acknowledgements table for all sequences can be found in the `metadata` folder. 

Alignments and xml files used for beast analyses are available in the `phylodynamics` directory along with plotting scripts to visualize the resulting files. This folder also contains the MCC tree files for each analysis. Scripts used to process metadata for sequences as well as beast outputs can be found in the `scripts_metadata_files` folder. This repository contains subdirectories which include scripts for data processing, metadata, and plotting scripts. The `HPAI_USDA_casedata` subdirectory contains detection data sourced from the USDA as well as Skygrid effective population size reconstructions which can be visualized using the `HPAI_usda_vis.R script` in the `data-processing` sub-directory.

For any questions please feel free to email *Lambodhar1@gmail.com*


## System requirements and installation

All bayesian phylodynamic analyses were performed using BEAST v1.10.4 (https://beast.community) please see link for installation instructions and install time (typically < 1 hour ). We performed all analyses using the PMACS HPC cluster at UPenn which utilizes a CentOS v7.8 operating system.  

XML files for BEAST analysis can be run using standard command line interface or BEAST GUI with input files, runtimes vary depending on analysis. Analyses using empirical tree distribtuions should complete in > 1 day. All other anlaysis run between 2-5 days. 




## Citation

