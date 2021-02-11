
<p>
    <img src="/www/Logo.png" width="250" />
</p>

# Bikair_Logger_V1
This repository contains the first version of the BikAIr Logger. It allows visualizing the data collected from the BikAIr project. <br>
**Author:** Bikair team  <br>
**Email Address:** eoairquality@gmail.com

## The project
Bikair is a project developed by students from the 4th Generation EAGLE graduate program. It focuses on testing low-cost Arduino-based sensors in an urban environment such as the city of Würzburg. Eventually, the project aims to correlate in-situ data with relevant Remote Sensing-based parameters. It takes advantage of the recent advancements in the Internet of Things (IoT), which allows the collection of data on specific locations and its further transportation using wireless network protocols. Bikair team designed static and mobile devices using sensors that measure environmental parameters like temperature, humidity, UV radiation, particulate matter (PM1, PM 2.5, PM 10), and soil moisture. These measurements could strengthen a wide range of applications such as monitoring the relationship between urban green and temperature, identifying air pollution hotspots, or supporting the downscaling of satellite remote sensing products.

## Usage

### First Steps 
To visualize the logger, you could follow the next steps:
1. Download the repository 
2. Unzip the file  
3. Open the project `Bikair_Logger_V1.Rproj`
4. Run the code `Main_App.R`

After running the code, a new window with the logger will open.  At first, it will look like the following figure. 

<p>
    <img src="/www/app.PNG" width="850" />
</p>
<br>

### Data Import 
#### Load File
The following step is uploading the data. This is possible from the `Data Import` tab. In the left corner, there is a `Browse` button. By clicking it, you can select the file with the data. It is located in the folder of the repository `\Bikair_Logger_V1-main\Data` <br>
<p>
    <img src="/www/Load data.PNG" width="850" />
</p>

#### Table Display
After loading the file, a table with the data will be displayed. In the left panel you can select variables and observe some statistics such as the median, quartiles, and the minimum and maximum value. <br>

The variable selected would work as a starting point for the next steps. It could be changed afterward. <br>

<p>
    <img src="/www/data_import.PNG" width="850" />
</p>

### Interactive Map
In the `Interactive Map` tab, you can visualize the spatial distribution of the data collected. Initially, it would be clear, but you can change the type of visualization to points, lines or raster.

#### Points View
In the **Points View**, you could visualize each measurement as a point. In the right panel, the display could be adjusted to your preferences by the following options:
* Choose the range of points
* Select a variable
* Pick a color palette
* Change the size of the points
* Reduce the opacity
* Modify the legend

By clicking `Export`, the map could be saved as a png file.

<p>
    <img src="/www/points.PNG" width="850" />
</p>

#### Line View
When switching to **Line View**, the direction of movement can be observed through an animation. In the right panel, you can adjust the following options:
* Choose the range of points
* Select a base color
* Pick a pulse color 

<p>
    <img src="/www/lines.PNG" width="850" />
</p>

#### Raster View
By clicking the **Raster View** option, tha app will show an interpolation of the measurements. The preferences that could be adjusted are the following:
* Choose the range of points
* Select a variable
* Pick a color palette
* Change the resolution (depends on the density of the data collected)
* Reduce the opacity
* Modify the legend

By clicking `Export`, the map could be saved as a png file.


<p>
    <img src="/www/raster.PNG" width="850" />
</p>

### Plots
The `Plots` tab contains a variety of graphs that allow a better understanding of the variables.

<p>
    <img src="/www/plots.PNG" width="850" />
</p>
