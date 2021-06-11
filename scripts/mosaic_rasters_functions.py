import os, rasterio
import pandas as pd
from geopandas import GeoDataFrame
import rasterio.features
from shapely.geometry import shape
from osgeo import gdal, ogr
import numpy as np

def raster_mask_to_shapefile(raster, outname="vectorized.shp", outdir=None):
    """
    Generate a shapefile with a single feature outlining the extent of the input raster.
    There is probably a better way to do this, but this works...
    :param raster: (str) path to raster to vectorize
    :param outname: (str) name of the generated shapefile
    :param outdir: (str) if given, save the output to this folder
    :return:
    """
    if outdir:
        # if outdir is specified, save the clipped raster there
        outpath = os.path.join(outdir, outname)
    else:
        # otherwise, save to the same folder as the input raster
        outpath = os.path.join(os.path.split(raster)[0], outname)
    d = dict()
    d['val'] = []
    geometry = []
    with rasterio.open(raster, 'r') as src:
        empty = src.read(1)
        for shp, val in rasterio.features.shapes(source=empty, transform=src.transform):
            d['val'].append(val)
            geometry.append(shape(shp))
        raster_crs = src.crs
    df = pd.DataFrame(data=d)
    geo_df = GeoDataFrame(df, crs={'init': raster_crs['init']}, geometry=geometry)
    geo_df['area'] = geo_df.area
    geo_df = geo_df[geo_df["val"] == 1]
    geo_df.to_file(outpath, driver="ESRI Shapefile")

    return outpath

def clip_by_utm_zone(raster, mask):
    warp_options_clip = gdal.WarpOptions(format = "GTiff",
                                    dstSRS = "EPSG:3005",
                                    xRes = 30,
                                    yRes = 30,
                                    resampleAlg = "near",
                                    cutlineDSName = mask,
                                    cropToCutline = True)

    zone = mask.split("_")[3]

    save_location = os.path.join("..", "scratch", zone + ".tif") 
    print(save_location)

    gdal.Warp(save_location, raster, options = warp_options_clip)
    
    return(save_location)

def summarize_structure(file):
    print(file)

    loaded = pd.read_csv(file)
    csv = loaded[["protected", "subzone", "vlce", "elev_cv", "loreys_height", "percentage_first_returns_above_2m", "total_biomass"]]
    
    # removal of harvested and non-forest pixels is now done in the sampling step (R)
    #csv = csv[csv["vlce"].isin([230, 220, 210, 81])] # use only forested pixels
    #csv = csv[csv["Change_Attribution"] != 2] # remove harvested pixels
    csv = csv[csv["elev_cv"] < 1]
    
    output = csv.groupby(["protected", "subzone"]).agg(
        mean_elev_cv = ("elev_cv", "mean"),
        mean_loreys_height = ("loreys_height", "mean"),
        mean_percentage_first_returns_above_2m = ("percentage_first_returns_above_2m", "mean"),
        mean_total_biomass = ("total_biomass", "mean"),
        sd_elev_cv = ("elev_cv", np.std),
        sd_loreys_height = ("loreys_height", np.std),
        sd_percentage_first_returns_above_2m = ("percentage_first_returns_above_2m", np.std),
        sd_total_biomass = ("total_biomass", np.std),
        count = ("protected", "count"))
    
    return output