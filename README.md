# Deglaciation

## Step 1. Extract data from the LipD:
Use: 
lipd.R to get temperature data;
https://github.com/yeshancqcq/deglaciation/blob/master/lipd.R
Get datafiles in the lipd2csv folder;
lipd_meta.R to get metadata from LipD;
https://github.com/yeshancqcq/deglaciation/blob/master/lipd_meta.R
Get the metadata.csv file;
lipd_depth_et.R to get geochronological data from LipD;
https://github.com/yeshancqcq/deglaciation/blob/master/lipd_depth_et.R
Get datafiles in the lipd_chron folder;

https://github.com/yeshancqcq/GEO420_data
Previous data (Shakun and Marcott papers) are in the GEO_420 repository.


## Step 2. Interpolation / Anomaly construction
Use interpolation_anomaly.R to generate the interpolated datasets, and calculate the anomaly (base: 8000 - 12000 bp).
Data files in the anomaly_interpolated folder
https://github.com/yeshancqcq/deglaciation/blob/master/interpolation_anomaly.R

## Step 3. Join interpolated temperature and anomaly to metadata -- prepare for the spatiotemporal reconstruction
Use join.R

Better to have a helper.csv file prepared from excel.

This file should only contains fieldnames from t0 to t22000

Results: 2 metadata files (anomaly_metadata.csv and temperature_metadata.csv)
https://github.com/yeshancqcq/deglaciation/blob/master/join.R

## Step 4. Spatiotemporal reconstruction.
If using ArcGIS: 
Import the csv metadata and convert it into a point shapefile

Preparing girds with Grid_Index_Feature, Then

Point To Raster --> Zonal Statistics as table--> Join back to the shapefile

If using ArcPy:
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/arcgis.py

Without ArcPy:
https://github.com/yeshancqcq/deglaciation/blob/master/spatiotemporal_reconstruction.R

and then: hemispheric reconstruction and plotting:
https://github.com/yeshancqcq/deglaciation/blob/master/hemisphere_construction.R

## Step 5. Regional average
With or without model
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/regional_construction.R

## Step 6. Data model comparison
Generating coordinates for model outputs (.nc files)
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/coor_gen.R

# Visualization
Regional plots:
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/plot_regional.R

Plot LipD metadata:
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/meta_plot.R

Plot maps in R:
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/map.R

Automatically plot spatiotemporal reconstructions in ArcPy/ArcGIS (need to set up the mxd and symbol layer in the desktop)
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/symbol.lyr
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/Untitled.mxd
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/paleo_process1.mxd
https://github.com/yeshancqcq/paleo_data_spatial/blob/master/mapping_series.py

# Latest files to use
Gridded data: model_sealand.csv and proxy_sealand.csv

Package data: model_lat_band3.csv and proxy_lat_band3.csv

Regional packages: model_region.csv and proxy_region.csv

