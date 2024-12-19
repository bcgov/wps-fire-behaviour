import geopandas as gpd
import rasterio
import rasterio.features
import numpy as np
from rasterio.transform import from_origin
from shapely.geometry import mapping
import matplotlib.pyplot as plt
import matplotlib.colors

def shapefile_to_raster(shape_files, to_tif=False, pixel_size=1):
    """
    Convert a shapefile(s) to a raster file.

    Parameters:
    - shape_files (list or str) of paths to the input shapefiles.
    - to_tif (bool) writes raster to a tif file if True
    - pixel_size (int): Size of each pixel in the output raster (in the same units as the shapefile's CRS).

    """
    #changes single file to list if called on a single file
    if type(shape_files) != list:
        shape_files = [shape_files]

    #defining max and min cord arrays
    maxxs = []
    maxys = []
    minxs = []
    minys = []
    gdfs = []
    
    #searching for largest square in shape_files
    for file in shape_files:
        gdf = gpd.read_file(file).to_crs(epsg=3005)
        bounds = gdf.total_bounds
        minx, miny, maxx, maxy = bounds
        minxs.append(minx)
        minys.append(miny)
        maxxs.append(maxx)
        maxys.append(maxy)
        gdfs.append(gdf)
    
    #defining max x, y and min x, y
    maxx = max(maxxs)
    maxy = max(maxys)
    minx = min(minxs)
    miny = min(minys)

    #defining width/height
    width = int((maxx - minx) / pixel_size)
    height = int((maxy - miny) / pixel_size)

    #defining transform
    transform = from_origin(minx, maxy, pixel_size, pixel_size)

    #defining metadata for tif
    metadata = {
        'driver': 'GTiff',
        'count': 1,
        'dtype': 'float32',
        'width': width,
        'height': height,
        'crs': 'EPSG:3005',
        'transform': transform
    }

    #creatiing list of arrays, one for each shape file
    array_list = []
    i = 0
    for gdf in gdfs:
        raster_array = np.zeros((height, width), dtype=np.float32)
        raster_array += rasterio.features.rasterize(
            [(mapping(geom), 1) for geom in gdf.geometry],
            out_shape=raster_array.shape,
            transform=transform,
            fill=0,
            dtype='float32'
        )
        array_list.append(raster_array)
        #saving to a tif if called
        if to_tif:
            with rasterio.open(f'raster_shape{i}', 'w', **metadata) as dst:
                dst.write(raster_array, 1)
        i += 1

    return array_list

def confusion(shape_files):
    '''
    Plots the confusion matrix for the shape files provided

    Parameters:
    - shape files (list), list of two shape files
    '''
    #converting shapefiles to arrays
    data = shapefile_to_raster(shape_files)

    #calculating lodgic arrays
    TT = np.logical_and(data[0]==1,data[1]==1)
    TF = np.logical_and(data[0]==1,data[1]==0)
    FT= np.logical_and(data[0]==0,data[1]==1)
    FF = np.logical_and(data[0]==0,data[1]==0)

    #calculating plotting array
    plotter = 3*TT + 2*TF + FT

    #calculating percents of each catagory
    size = TT.shape[0]*TT.shape[1]
    tt_per = round(100*np.sum(TT)/size, 1)
    tf_per = round(100*np.sum(TF)/size, 1)
    ft_per = round(100*np.sum(FT)/size, 1)
    ff_per = round(100*np.sum(FF)/size, 1)

    #plotting
    plt.figure(figsize=(15,15))
    cmap = matplotlib.colors.ListedColormap(['green','yellow','orange','red'])
    plt.imshow(plotter, cmap=cmap,vmin=0,vmax=3)
    plt.scatter(np.nan,np.nan,marker='s',s=100,label=f'Sim correct unburned area: {ff_per}%',color='green')
    plt.scatter(np.nan,np.nan,marker='s',s=100,label=f'Sim missed burned area: {ft_per}%' ,color='yellow')
    plt.scatter(np.nan,np.nan,marker='s',s=100,label=f'Sim false positives: {tf_per}%',color='orange')
    plt.scatter(np.nan,np.nan,marker='s',s=100,label=f'Sim correct burned area: {tt_per}%',color='red')
    plt.legend(fontsize="12")
    plt.show()
    print(f'Model correct: {tt_per+ff_per}%')
    print(f'Model incorect: {tf_per+ft_per}%')