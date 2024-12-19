import os
import geopandas as gpd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import zipfile

from shapely.geometry import Polygon, MultiPolygon, Point
from shapely.ops import unary_union
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
from scipy.spatial import Delaunay, ConvexHull
from shapely.geometry import Polygon, MultiPolygon
from shapely.ops import unary_union
import numpy as np
from datetime import datetime, timedelta

import matplotlib.pyplot as plt
from matplotlib.widgets import Cursor
from matplotlib.patches import Polygon as pol

def extract_kmz(kmz_path, extract_to_folder):
    with zipfile.ZipFile(kmz_path, 'r') as kmz:
        kmz.extractall(extract_to_folder)
        # List all extracted files to help find shapefiles
        return [os.path.join(extract_to_folder, file_name) for file_name in kmz.namelist()]
    


def check_fire_date():
    simlist = os.listdir("/mnt/d/MODELS/simulations")
    fires_gdf = gpd.read_file('../shape_files/prot_current_fire_polys_202310241608.zip')
    fires_gdf = fires_gdf.to_crs(epsg=4326)
    firms_gdf = gpd.read_file('../shape_files/fire_archive_SV-C2_493123.shp')
    firms_gdf = firms_gdf.to_crs(epsg=4326)
    firelist = []
    kept_fires = []
    for fire in simlist:
        if len(fire.split('_')[-1]) == 6:
         firelist.append(fire)
    for fire in firelist: #firelist:
        models = os.listdir(f'/mnt/d/MODELS/simulations/{fire}')
        dates = []
        for model in models:
            if len(model.split('_')) == 6:
                dates.append((int(model.split('_')[5][:8]),model))
        dates.sort()
        if len(dates) != 0 and 20230801 <= dates[0][0] <= 20230830:
            sims = os.listdir(f'/mnt/d/MODELS/simulations/{fire}/{dates[0][1]}')
            for file in sims:
                if '.shp' in file:
                    kept_fires.append(fire.split('_')[-1])
    good_fires = []
    for fire in kept_fires:
        fire_gdf = fires_gdf[fires_gdf['FIRE_NUM'] == fire]
        if fire_gdf.shape[0] != 0:
            good_fires.append(fire)
    return good_fires
    
def get_buffered_perimeter(fire_num):
    fires_gdf = gpd.read_file('../shape_files/prot_current_fire_polys_202310241608.zip')
    sim_file = f'/mnt/d/MODELS/simulations/BCWS_2023_{fire_num}'
    first_det = os.listdir(sim_file)[0]
    if len(first_det.split('_')) == 6:
        first_date = datetime.strptime(first_det.split("_")[5][:-3], '%Y%m%d%H%M%S')
    else:
        print('Invalid fire')
        return
    date_range = first_date + timedelta(days=1)
    if fire_num in fires_gdf['FIRE_NUM'].tolist():
        firms_gdf = gpd.read_file('../shape_files/fire_archive_SV-C2_493123.shp')
        firms_gdf = firms_gdf.to_crs(epsg=4326)
        fire_gdf = fires_gdf[fires_gdf['FIRE_NUM'] == fire_num]
        fire_gdf = fire_gdf.to_crs(epsg=4326)
        firms_trim = gpd.sjoin(firms_gdf, fire_gdf,how='inner',predicate='intersects')
        firms_list = [(point.x,point.y) for point in firms_trim.geometry]
        firms_dates = [date for date in firms_trim.ACQ_DATE]
        firms_times = [time for time in firms_trim.ACQ_TIME]
        firms_dt = [datetime.strptime(f'{str(firms_dates[i])[:10]}-{firms_times[i]}','%Y-%m-%d-%H%M') for i in range(len(firms_dates))]
        firms_perim = []
        firms_point = []
        for point in range(len(firms_list)):
            if first_date <= firms_dt[point] <= date_range:
                firms_perim.append(firms_list[point])
                firms_point.append(Point(firms_list[point]))
        perim_point_gdf = gpd.GeoDataFrame({'geometry':firms_point}, crs='EPSG:4326')

        files = os.listdir(f'{sim_file}/{first_det}')
        for file in files:
            if '.shp' in file:
                shape_file = file
                break
        perim_sim = gpd.read_file(f'{sim_file}/{first_det}/{shape_file}')
        perim_sim = perim_sim.to_crs(epsg=4326)

        '''
        #Method for manually cutting a fire perimeter from VIIRS data:
        # x = []
        # y = []
        # for point in firms_perim:
        #     x.append(point[1])
        #     y.append(point[0])
        # clicked_points = []

        # def on_click(event):
        #     if event.inaxes is not None:
        #         # Append the clicked point to the list
        #         clicked_points.append([event.xdata, event.ydata])
                
        #         # Clear the previous plot
        #         ax.clear()
                
        #         # Plot the points
        #         ax.scatter(x,y, color='blue', label='Points')
                
        #         # Plot the traced perimeter
        #         if len(clicked_points) > 1:
        #             perimeter = pol(clicked_points, closed=True, fill=None, edgecolor='red')
        #             ax.add_patch(perimeter)
                
        #         # Plot the clicked points
        #         clicked_array = np.array(clicked_points)
        #         if len(clicked_points) > 1:
        #             ax.plot(clicked_array[:, 0], clicked_array[:, 1], 'ro-', label='Perimeter Trace')
        #         else:
        #             ax.plot(clicked_array[:, 0], clicked_array[:, 1], 'ro', label='Perimeter Trace')
                
        #         # Update the plot with labels and legend
        #         ax.set_title("Click to Trace Perimeter")
        #         ax.legend()
        #         plt.draw()
        

        # # Create the plot
        # fig, ax = plt.subplots()
        # ax.scatter(x,y, color='blue', label='Points')

        # # Add a cursor for better interaction
        # cursor = Cursor(ax, useblit=True, color='red', linewidth=1)

        # # Connect the click event to the handler
        # fig.canvas.mpl_connect('button_press_event', on_click)

        # # Show the plot
        # plt.title("Click to Trace Perimeter")
        # plt.legend()
        # plt.show()

        # points = []
        # for point in clicked_points:
        #     points.append((float(point[1]),float(point[0])))
        # points.append((float(clicked_points[0][1]),float(clicked_points[0][0])))
        # poly = Polygon(points)
        # perimeter_gdf = gpd.GeoDataFrame(geometry=[poly], crs='EPSG:4326')
        if perim_sim.shape[0] != 0:
            fig, ax = plt.subplots()
            perim_sim.plot(ax=ax,color='blue')
            fire_gdf.plot(ax=ax, facecolor='none')
            if perim_point_gdf.shape[0] != 0:
                perim_point_gdf.plot(ax=ax,color='orange')
            plt.savefig(f'../plots/{fire_num}.png')
        #perimeter_gdf.plot(ax=ax, edgecolor='red', facecolor='none', linewidth=2, label='Perimeter')
        #perimeter_gdf.to_file(f'../shape_files/{fire_num}.shp', driver='ESRI Shapefile')
        '''
        
    else:
        print('Fire not in perimeters')

fires = check_fire_date()
for fire in fires:
    get_buffered_perimeter(fire)