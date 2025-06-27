---
layout: default
title: Getting started
---

# Getting started with **LUCAS Builder**

## Quickstart Tutorial

This quickstart tutorial will introduce you to the basics of working with **LUCAS Builder**. The steps include:
<br>
* Installing LUCAS Builder
* Creating a LUCAS Builder Library from a template
* Viewing model inputs and outputs
* Running the model


## **Step 1: Install LUCAS Builder**
**LUCAS Builder** is a *package* within the Syncrosim simulation modeling framework, and requires **SyncroSim Studio** to be installed on your computer. Download and install <a href="https://syncrosim.com/download/" target="_blank">**SyncroSim**</a>  3.1.9 or later.

To install the **LUCAS Builder** *package*, open **SyncroSim** and select **File > Local Packages > Install from Server...**, select the **lucasbuilder** *package* and click OK. Repeat the process to intall the **stsim** *package*. Visit the <a href="https://docs.stsim.net/" target="_blank">ST-Sim documentation</a> page for additional information on ST-Sim.

If you do not have **Miniforge** installed on your computer, a dialog box will open asking if you would like to install Miniforge. Click **Yes**. Once Miniforge is installed, a dialog box will open asking if you would like to create a new conda environment. Click **Yes**. Note that the process of installing Miniforge and the **lucasbuilder** conda environment can take several minutes. If you choose not to install the conda environment, you will need to install <a href="https://www.r-project.org/" target="_blank">**R**</a> version 4.1.3 or later.

> **Miniforge** is an installer for conda, a package environment manager that installs any required packages and their dependencies. By default, **LUCAS Builder** runs conda to install, create, save, and load the required environment for running **LUCAS Builder**. The **LUCAS Builder** environment includes the R software and necessary packages.

> **Note:** The **LUCAS Builder** *package* includes a template *Library*, **CBM-CFS3 CONUS**, that contain example inputs and outputs. Installation of conda or R is not required to view the Template Library inputs and outputs.

## **Step 2: Create a new LUCAS Builder Library**
Having installed the **LUCAS Builder** package, you are now ready to create your first SyncroSim Library. A *Library* is a file (with extension *.ssim*) that contains all of your model inputs and outputs. The format of each Library is specific to the Package for which it was initially created. You can opt to create an empty library or download the **lucasbuilder** template library. In this tutorial, we will be using the the **CBM-CFS3 CONUS** template library.
<br>
<img align="middle" style="padding: 3px" width="680" src="assets/images/screencap-1.png">
<br>
In this window:
<br>
* Go to **File > New > From Online Template...** Select the row for **lucasbuilder**. Note that as you select a row, the list of **Templates** available and suggested **File name** for that base package are updated.
* Select the **CBM-CFS3 CONUS** Template as shown above.
* Optionally, type in a new **File name** for the Library (or accept the default); you can also change the target **Folder** using the **Browse...** button.

> **Note:** If you intend on using Multiprocessing (recommended), ensure your SyncroSim Library is saved to a drive that is not being syncronized to the cloud. Saving your library to OneDrive, Dropbox or some other similar location can result in an error when completing a model run.

When you are ready to create the Library file, click **OK**. A new Library will be created and loaded into the Library Explorer.

## **Step 3: Review the model inputs and outputs**
The contents of the Template Library are now displayed in the Library Explorer. Model inputs in SyncroSim are organized into *Scenarios*, where each Scenario consists of a suite of values, one for each of the Model's required inputs.

Because you chose the **CBM-CFS3 CONUS** template when you created your Library, your Library already contains four folders:
* 1 - Predefined Inputs
* 2 - User Defined Inputs
* 3 - Run Setup
* 4 - Run Forecast

The **Predefined Inputs** folder contains pre-configured Scenarios that act as inputs for the **Run Setup** and **Run Forecast** Scenarios. The **User Defined Inputs** folder contains two sub-folders (**Run Setup Inputs** and **Run Forecast Inputs**) that house user input Scenarios that need to be populated before running the **Run Setup** and **Run Forecast** Scenarios.
>**Note:** The **User Defined Inputs** have been populated to provide an executable example to help you get started quickly.
<br>
<img align="middle" style="padding: 3px" width="550" src="assets/images/screencap-2.png">
<br>
In the  **User Defined Inputs**  folder, select and review the inputs for the Scenarios in the **Run Setup Inputs** sub-folder.

* Select the Scenario named  **CBM Crosswalk – Spatial Unit and Species Type**  in the Library Explorer.
* Double-click to open the Scenario and view its details.

This opens the Scenario Properties window. The first tab in this window, called **General**, contains three datasheets. The first, **Summary**, displays some general information for the Scenario. The second, **Pipeline**, allows the user to select the run order of the inputs in the model. Finally, the **Datafeeds** datasheet (shown below) displays a list of all data sources.
<br>
<img align="middle" style="padding: 3px" width="975" src="assets/images/screencap-3.png">
<br>
Select the **CBM Crosswalk to ST-Sim – Spatial Unit and Species Type** datafeed to view the example inputs.
<br>
>**Note:** Populated datasheets will appear at the top of the Datafeeds list with a green check mark in the Data field.

The crosswalk datasheet allows a user to associate each forest type to a CBM equivalent combination of Ecological Boundary, Admin Boundary, and Species Type.  Here a user can specify temperature values that should be used when modeling dead organic matter transfer and decay rates.  Note that if temperature values are not specified, the default values for the selected Ecological Boundary will be used. Optionally, a user can load CBM output files, which can be compared against simulations run in ST-Sim for validation purposes (see below).
<br>
<img align="middle" style="padding: 3px" width="975" src="assets/images/screencap-4.png">
<br>
>**Note:** In the example, the **ST-Sim Stratum**, **ST-Sim Secondary Stratum**, and **Average Temperature** columns have been populated and hidden for image clarity. In a custom Library, users will need to manually set values for these columns.

Looking at the **Spin-up** Scenario, we see that each ST-Sim State Class defined in the species crosswalk has been linked with each transition (disturbance) type.   
<br>
<img align="middle" style="padding: 3px" width="975" src="assets/images/screencap-5.png">
<br>
Merchantable volume curves need to be added to the **Merchantable Volume Curve** scenario for each ST-Sim State Class defined in the species crosswalk.
<br>
<img align="middle" style="padding: 3px" width="975" src="">
<br>
When all Scenarios in the **User Defined Inputs** folder are populated, the **Run Setup** Scenarios can be run.
<br>
<img align="middle" style="padding: 3px" width="550" src="assets/images/screencap-6.png">
<br>
Click on the Scenarios in the **Run Setup** and **Run Forecast** folders to view the Scenario dependencies and familiarize yourself with each Scenario's inputs. 
<!-- The Scenarios in **Run Setup** and **Run Forecast** have already been run so you will also see a Results folder when you click on each Scenario.  -->

Open the **CBM Output** Scenario and select **Pipeline** from the **General** tab. Notice that, only the first stage *1 - Load CBM Output* has been selected for this scenario. Now select **Datafeeds** from the **General** tab to see the Scenario's active datasheets. 
<br>
<img align="middle" style="padding: 3px" width="975" src="assets/images/screencap-7.png">
<br>
Open the Result Scenario for **Load CBM Output**. 
<br>
<img align="middle" style="padding: 3px" width="550" src="assets/images/screencap-8_2.png">
<br>
Looking at **Datafeeds**, notice that the **State Attribute Values** datasheet was populated as a result of running the **Load CBM Output** Scenario. 
<br>
<img align="middle" style="padding: 3px" width="975" src="assets/images/screencap-9.png">
<br>
Following the steps above, view the Run Stage and input and output **Datafeeds** for the **Calculate Flow Rates** and **Run Spin-up** Scenarios.

Select the **Calculate Flow Rates** scenario and press **Run** on the top toolbar. 
<br>
<img align="middle" style="padding: 3px" width="750" src="assets/images/screencap-9_2.png">
<br>
Repeat the process using the **Run Spin-up** scenario. Note that the outputs from the **Calculate Flow Rates** scenario serve as inputs for the spin-up runs. In turn, the results from the **Run Spin-up** scenario are used as inputs for the **Single Cell - No Disturbance** scenario.

> **Warning Note:** Running these scenarios will take approximatelly 17.4 GB of disk space.

Scenario results are automatically loaded after a successfull scenario run. Right-click over the **Calculate Flow Rates** and **Run Spin-up** scenarios and select **Remove from Results** at the bottom of the menu.

Scenario Results can also be viewed through **Charts** and **Maps**. To view results for the **Single Cell – No Disturbance** Scenario, select the Results Scenario in the Library Explorer and then choose **Add to Results**. 
<br>
<img align="middle" style="padding: 3px" width="550" src="assets/images/screencap-10.png">
<br>
>**Note:** You can add and remove Results Scenarios from the list of Scenarios being analyzed by selecting a Scenario in the Library Explorer and then choosing either **Add to Results** or **Remove from Results** from the Scenario menu. **Scenarios** currently selected for analysis are highlighted in **bold** in the Library explorer.

Next, move to the **Charts** tab at the bottom left of the **Scenario Manager** screen and double-click on the **Single Cell – Biomass** chart to open it.
<br>
<img align="middle" style="padding: 3px" width="550" src="assets/images/screencap-11.png">
<br>
The following charts will be plotted.
<br>
<img align="middle" style="padding: 3px" width="975" src="assets/images/screencap-13.png">
<br>
You can compare the results of your **Single Cell – No Disturbance** run with validation outputs generated by the **Load CBM Output** Scenario by adding both Scenarios to the Results Viewer. 

Double-click on the **Single Cell – Aboveground DOM** and **Single Cell – Belowground DOM** charts to view the results. You can view results for the different forest types by changing the **State Class** selection and then clicking **Apply**.  When done viewing the **Single Cell** Results, remove the **Load CBM Output** and **Single Cell – No Disturbance** Scenarios from the Results Viewer.

<!-- To view spatial results, add the **Landscape** Scenario to the Results and then select the **Maps** tab from the bottom of the **Scenario Manager** window (i.e. beside the **Charts** tab). Double click on the **Biomass** Map. By default, the mapping window should display Wildfire events and changes in Total Biomass over time.
<br>
<img align="middle" style="padding: 3px" width="975" src="assets/images/screencap-13.png">
<br>
Double click on **Flows** and **Age** Maps to view changes in carbon flows and forest age for the **Landscape** Scenario. -->

## **Step 5: Run the model**
If not using conda, SyncroSim needs the location of your R executable, which will be found automatically if it is installed in the default location. To check, double-click on **CBM-CFS3 CONUS** and navigate to the **System** tab. In the **Tools > R** datasheet, you should see the file path to your R executable. If not, click **Browse...** and navigate to the correct file location. 

Once your **CBM-CFS3 Example** Library is configured, you can run the model by right-clicking on the **Load CBM Output** Scenario in the **Scenario Manager** window and selecting **Run** from the context menu. If prompted to save your project, click **Yes**. If the run is successful, you will see a Status of **Done** in the **Run Monitor** window, at which point you can close the **Run Monitor** window; otherwise, click on the **Run Log** link to see a report of any problems. Make any necessary changes to your Scenario, then re-run the Scenario.
<br>
<img align="middle" style="padding: 3px" width="550" src="assets/images/screencap-16.png">
<br>

Next, repeat the steps above to run the **Generate Flow Multiplier** and **Run Spin-up** Scenarios.  
>**Note:** The **Run Setup** and **Run Forecast** Scenarios rely on dependencies that are defined in **Predefined Inputs** and **User Defined Inputs**, as well as results from previous **Run Setup** Scenarios. For this reason, it is important the Scenarios are run in sequence.  

Once the **Run Setup** Scenarios have completed successfully, the **Run Forecast** Scenarios can be run. 

Repeat the steps above to run the **Single Cell – No Disturbance**

When a Scenario is run, a new Results Scenario will appear in the Results folder. To view the results, select the Results Scenario in the Library Explorer and then choose **Add to Results** from the Scenario menu. The selected Scenario will appear in **bold** in the Library Explorer and the Scenario results will display in Charts and Maps.
