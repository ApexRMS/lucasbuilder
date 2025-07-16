---
layout: default
title: Home
description: "SyncroSim package for forest carbon modeling"
permalink: /
---

# **LUCAS Builder** SyncroSim Package
<img align="right" style="padding: 13px" width="180" src="assets/images/logo/lucasbuilder-sticker.png">
[![GitHub release](https://img.shields.io/github/v/release/ApexRMS/lucasbuilder.svg?style=for-the-badge&color=d68a06)](https://GitHub.com/ApexRMS/lucasbuilder/releases/)    <a href="https://github.com/ApexRMS/lucasbuilder"><img align="middle" style="padding: 1px" width="30" src="assets/images/logo/github-trans2.png">
<br>

## Landscape scale forest carbon simulations

### *LUCAS Builder* is an open-source <a href="https://syncrosim.com/download/" target="_blank">SyncroSim</a> package for integrating dynamics from the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the <a href="http://docs.stsim.net" target="_blank">ST-Sim</a> landscape simulation model.

**LUCAS Builder** integrates inputs and outputs from the Carbon Budget Model of the Canadian Forest Sector (<a href="https://natural-resources.canada.ca/climate-change/climate-change-impacts-forests/carbon-budget-model" target="_blank">CBM-CFS3</a>, <a href="https://doi.org/10.1016/j.ecolmodel.2008.10.018" target="_blank">Kurz *et al*. 2009</a>) into landscape scale simulations using the <a href="http://docs.stsim.net" target="_blank">ST-Sim</a> <a href="https://syncrosim.com/" target="_blank">SyncroSim</a> package. The package allows users to load outputs from the CBM-CFS3, calculate flow rates by carbon pool based on CBM-CFS3 parameters and user defined temperatures, run spin-up simulations to create initial carbon maps based on forest type and recent disturbance, and generate spatially explicit forecasts of forest carbon under alternative scenarios.

**LUCAS Builder** is a package that plugs into the <a href="https://syncrosim.com/" target="_blank">SyncroSim</a> modeling framework. It can also be run from the R programming language using the <a href="https://syncrosim.com/r-package/" target="_blank">rsyncrosim</a> R package and from the Python programming language using the <a href="https://pysyncrosim.readthedocs.io/en/latest/" target="_blank">pysyncrosim</a> Python package.

## Requirements

This package requires SyncroSim <a href="https://syncrosim.com/download/" target="_blank">3.1.9 or higher</a> and the <a href="https://docs.stsim.net/" target="_blank">*stsim*</a> SyncroSim package. <br>
If you choose to run without installing the conda environment, R <a href="https://www.r-project.org/" target="_blank">version 4.1.3</a> or higher is required. <br>

## How to Install

For installation instructions, see the **Install LUCAS Builder** section on the [Getting Started](https://apexrms.github.io/lucasbuilder/getting_started.html) page.

## Getting Started

For more information on **LUCAS Builder**, including a Quickstart Tutorial, see the [Getting Started](https://apexrms.github.io/lucasbuilder/getting_started.html) page.

## Templates

- LUCAS Builder - CONUS: **LUCAS Builder** Library containing the 29 forest types required to run forest carbon simulations for CONUS.

## Links

Browse source code at <a href="https://github.com/ApexRMS/lucasbuilder/" target="_blank">https://github.com/ApexRMS/lucasbuilder/</a>
<br>
Report a bug at <a href="https://github.com/ApexRMS/lucasbuilder/issues" target="_blank">https://github.com/ApexRMS/lucasbuilder/issues</a>

## Developers

Amanda Schwantes (Author, maintainer) <a href="https://orcid.org/0000-0002-7791-1078"><img align="middle" style="padding: 0.5px" width="17" src="assets/images/ORCID.png"></a>
<br>
Leonardo Frid (Author) <a href="https://orcid.org/0000-0002-5489-2337"><img align="middle" style="padding: 0.5px" width="17" src="assets/images/ORCID.png"></a>
<br>
Benjamin Sleeter (Author) <a href="https://orcid.org/0000-0003-2371-9571"><img align="middle" style="padding: 0.5px" width="17" src="assets/images/ORCID.png"></a>
<br>
Schuyler Pearman-Gillman (Author) <a href="https://orcid.org/0000-0002-3911-1985"><img align="middle" style="padding: 0.5px" width="17" src="assets/images/ORCID.png"></a>
<br>
Diego Bilski (Author)
<br>
Colin Daniel (Author)

