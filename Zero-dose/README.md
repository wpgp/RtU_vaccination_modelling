# Zero-Dose Estimation Using QGIS

This document depicts how to calculate the estimates of numbers of un and under-vaccinated children in Cameroon at the Commune Arrondissement administrative level for Cameroon.

## Required Files

1. **Population raster**
2. **Vaccination coverage rasters** (DTP1, DTP2, DTP3)
3. **Shapefile** defining the zones for estimation

## Required Tools

In QGIS software, the following tools are required:

- Raster Calculator
- Zonal Statistics

For this exercise, QGIS v3.40.0 was used.

## Adding Files to the Map Window

### Methods to Add Files
1. Navigate to the folders where the files are located, select them, and drag and drop them into the map window.
2. Add raster layers using the following steps:
   - Click the **Layer** tab on the **Menu Toolbar**.
   - Select **Add Layer** > **Add Raster Layer**.


<div align="center">
<img src="Figures/Fig_z1.png" alt="Raster Dataset Selection">
</div>


Upon clicking, the window below opens. Click the three dots beside **Raster dataset(s)** and navigate to where these files are found. This way allows adding a single file or multiple files at once.

<div align="center">
<img src="Figures/Fig_z2.png" alt="Datasource Manager">
</div>


Add one or multiple files and click **Add** > **Close** to add the selected files which will be displayed on the map window and checked on the Layers panel as shown below. 

<div align="center">
<img src="Figures/Fig_z3.png" alt="Layers Panel" width="500" height="120">
</div>


Follow the same process to add the shapefile for Commune Arrondissement by selecting **Add Vector Layer** instead.

## Zero-Dose Calculation

Zero-dose calculations use a population-weighted aggregation with the formula:

```
(1 - Vaccination Coverage Raster) * Population Raster
```
This requires the Raster calculator. Navigate to the Processing Toolbox where all tools are found by clicking on the Attributes toolbar above the map window, and click on the tool shown as a gear sign. This opens the Processing toolbox on any side of the map window. Navigate to the search box and type in the **Raster calculator** tool. 

<div align="center">
<img src="Figures/Fig_z4.png" alt="Processing Toolbox" width="400" height="250">
</div>

Double-click on the tool to open its panel and follow the following instructions:

1. Click on the three dots beside the **Input layers** box, to select the vaccination raster and the population raster.
2. Open the Expression box (∑) and type:
     ```
     (1 - vaccination raster) * Population raster
     ```
     Click OK to go back to the Calculator.
3. Set an output extent by clicking the down arrow beside **Output extent [optional]** and select **Calculate from Layer**. This shows the available layers, then select the Population layer. This sets the extent to that of the population layer.
4. Under the **Calculated** section, click the three dots to save the final output to folder, using a preferred meaningful name.
5. Leave other settings as default and click **Run**.

<div align="center">
<img src="Figures/Fig_z5.png" alt="Raster Calculator Setup">
</div>


<div align="center">
<img src="Figures/Fig_z6.png" alt="Raster Calculator" >
</div>

Upon running the tool, the number of unvaccinated or zero-dose children for dtp1 gets added to the map window. Repeat the same process using the same formula for other vaccination rasters. Upon completion, they all get added to the map window as shown below.

<div align="center">
<img src="Figures/Fig_z7.png" alt="Map Window" width="600" height="500">
</div>


However, this is still at the grid level and the final estimates are required at the administrative level. We will use the Zonal Statistics tool to produce the estimates of the number of zero-dose children.


Again, open the Processing Toolbox, and search for **Zonal Statistics** in the search box. 

<div align="center">
<img src="Figures/Fig_z8.png" alt="Processing Toolbox" width="350" height="200">
</div>

Select the **Input layer** as the shapefile to be used, **Raster layer** as any of the zero-dose raster layers produced earlier, in this case, dtp1 zero-dose raster. In the output column prefix, give it a meaningful name, such as `dtp1_` to indicate which vaccine is considered. In the **Statistics to calculate**, click on the three dots, which opens a new pane to select a statistic. Click on **Sum** and select Ok. Go back to the Zonal statistics window using the arrow. 

<div align="center">
<img src="Figures/Fig_z9.png" alt="Zonal Statistics Control">
</div>

Click *Run* but do not close the tool window. This adds a new layer to the layers panel called *Zonal Statistics*. Right-click to open the attribute table and check to see that a new field/column `dtp1_sum` has been added. This is the sum or estimate of dtp1 zero-dose children at each Commune Arrondissement level. 

<div align="center">
<img src="Figures/Fig_z10.png" alt="Zonal Statistics">
</div>


Go back to the Zonal Statistics window and change the **Input layer** to the newly added Zonal Statistics layer, also change the **Raster layer** to the dtp2 zero-dose raster layer, and change the **Output column prefix** to `dtp2`. Click **Run** and do not close. 
   
   
<div align="center">
<img src="Figures/Fig_z11.png" alt="Zonal Statistics Execute">
</div>


<div align="center">
<img src="Figures/Fig_z12.png" alt="Choose Zonal Statistics Option" width="500" height="120">
</div>
   
This adds a new **Zonal Statistics** layer on the previous layer. Remove the old layer by right-clicking and choose **Remove layer**. Repeat the same step for the dtp3 zero-dose and Population raster layers, and close the Zonal Statistics tool window.

<div align="center">
<img src="Figures/Fig_z13.png" alt="DTP3 Zonal Statistics">
</div>

Open the attribute table for the last zonal statistics layer run, and this includes four fields namely `dtp1_sum`, `dtp2_sum`, `dtp3_sum`, and `Pop_2022_sum`, as shown below.


<div align="center">
<img src="Figures/Fig_z14.png" alt="Sum Zonal Statistics">
</div>

Close the attribute table and double-click on the Zonal Statistic layer, then navigate to **Fields**. Currently, the field names produced from the zonal statistics do not show the years. To ensure that the years are added, at the top of the Fields window, click on **Toggle editing mode**, and click on the field names to begin editing as shown below.

<div align="center">
<img src="Figures/Fig_z15.png" alt="Zero-dose layer1">
</div>


<div align="center">
<img src="Figures/Fig_z16.png" alt="Zero-dose layer2">
</div>

The field names now include the years. To save, click on **Toggle editing mode** again. Select **Save** to stop editing. You can open the attribute table again to see the edits, as shown below. 

<div align="center">
<img src="Figures/Fig_z17.png" alt="Toggle Editing Mode">
</div>

Close the attribute table. You can now export your layer either to a shapefile or CSV. To do this, right-click on the layer and select **Export** and **Save Features As** to navigate to a preferred folder where the files will be saved. 


<div align="center">
<img src="Figures/Fig_z18.png" alt="Shapefile Layer" width="600" height="500">
</div>


Once clicked, this opens a new window. Under the **Format** section, select `ESRI Shapefile` to save as a shapefile or **Comma Separated Value [CSV]** to save as CSV. 

<div align="center">
<img src="Figures/Fig_z19.png" alt="Save Vector Layer">
</div>


<div align="center">
<img src="Figures/Fig_z20.png" alt="Save Vector Layer CSV">
</div>

## Notes
The above steps can be repeated for any administrative areas.


## Acknowledgements
The Reach the Unreached – Digital technologies to map zero-dose and unreached children in West and Central Africa project is funded by UNICEF — The United Nations Children’s Fund (contract No. 43387656). The project is led by UNICEF West Africa Regional Office and the partners include the UNICEF Country Offices, WorldPop at the University of Southampton, MapAction, and CartONG.  
This method report was written by Somnath Chaudhuri and Edson Utazi. The contributions of the entire WorldPop team to the developed method and to the current applications are also acknowledged.


## License
This code and Readme document may be redistributed following the terms of a Creative Commons Attribution 4.0 International (CC BY 4.0) License.
