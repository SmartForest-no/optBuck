---
title: optBuck – An R package for handling single-grip forest harvester data and bucking optimization
tags:
- R
- Forestry
- Harvester data
- Optimal bucking
authors: 
- name: Lennart Noordermeer 
affiliations: 
- name: Faculty of Environmental Sciences and Natural Resource Management, Norwegian University of Life Sciences

date: 12 May 2022

---

# Summary

The R package optBuck provides functions for reading and handling production files from forest harvesters and computing the optimum bucking outcome for harvested stems. The package is based on harvested production report (hpr) files in the StanFord 2010 data format, retrieved from single-grip harvesters. Functionality includes the extraction of production data on harvested stems, produced logs, and the bucking outcome for each harvested stem. Additionally, information on assortments, such as the species, dimension and quality requirements, and price matrices can be extracted from the hpr files. These data can then be used as input parameters in a bucking algorithm, which maximizes the total stem value using dynamic programming. Thus, apart from providing tools to extract and process data from single-grip harvesters, optBuck can be used to evaluate bucking outcomes and perform sensitivity analyses and economic simulations based on harvester data.

# Statement of need

Single-grip harvesters record large amounts of standardized production data during operation. Sensors mounted on the harvester head record log dimensions and volumes, and each cut is allocated a time stamp and stored on an on-board computer. The operator selects and records tree species and timber grades, providing information on the characteristics and quality of the harvested logs. Additionally, most operating systems can be coupled with Global Navigation Satellite Systems, providing spatial data on the machine’s operation path and harvested trees. Harvester data are a key source of information in the fields of harvester productivity assessment, forest inventory, operation management and bucking optimization.   

Bucking, i.e., cutting felled trees into logs, is a primary task in timber harvesting and has great impact on the value of the produced logs. A given stem can be bucked into different numbers of logs with different dimensions, assortments, and values. Many such potential outcomes typically exist, whereby suboptimal bucking results in reduced timber quality and volume utilization, and can reduce the economic value of a stem substantially. Besides being used in harvester on-board computers to aid machine operators in the bucking, bucking algorithms have found widespread application in research. For example, studies have used bucking algorithms for timber market assessments and economic simulations.   

Although machine manufacturers provide software solutions for handling production data obtained from forest harvesters, few other tools currently provide the functionality to read and process hpr files, or to perform bucking optimization. Apart from reading and managing information obtained from hpr files, optBuck can be used to evaluate bucking outcomes and provides a range of potential research applications.

# Funding details

The optBuck package was developed as part of the project SmartForest, funded by the Research council of Norway (project no. 309671). 
