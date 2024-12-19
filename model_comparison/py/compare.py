
'''
Author: Adithi Balaji

Script that compares fire growth models from two vendors (Heartland Software FireCast and Technosylva FireSim) to observed fire perimeters for model validation and evaluation.
Currently associated with 2023 data. May be used for future results with slight modifications.

Dependencies: Python, Matplotlib, Geopandas, Pandas, Shapely
Use by calling file name with the alphanumeric fire number you are interested in seing evaluated
>>python3 compare.py K52125
>>python3 compare V82990 

Notes:
-Not all fires have data. The script is currenlty written to compare cases where all three inputs have data.
-Firecast simulations all run for exactly 12 hours from ignition time. Technosylva simulations have more variation, but the script ties to get data for an hour as close to 12 as possible.
-The observed perimeter is fetched from he most recently available file after the two simulations end. This may mean that the observed data is often represnting a time much later than the simulations.
    Please check thye terminal outputs to see the dates.

'''



import matplotlib
matplotlib.use('Qt5Agg')
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import os
import sys
from shapely.geometry import box, MultiPolygon
from datetime import datetime, timedelta
import re


lookup_table_path = "../data/2023_fire_season_lookup_table.xlsx"
perimeters_path = "../data/2023_fire_perimeters/interrim_perims/polys" 
technosylva_folder_path = "/mnt/e/MODELS/TECHNOSYLVA/simulations/"
firecast_folder_path = "/mnt/e/MODELS/FIRECAST/simulations/"
firms_path = "../data/2023_firms/fire_archive_SV-C2_493123.shp"


def lookup_technosylva(fire_name, simulation_duration=12):
    """
    Function to lookup corresponding Technosylva data folder from an input fire name.
    If multiple files are associated with the fire name, forces the user to choose, but only files with a DURATION_HOURS of simulation_duration will be shown for selection.

    Inputs:
    - fire_name: The fire name (e.g., "Inks Lake Fire") or number (e.g., 'K52125').
    - simulation_duration: Integer representing time in hours for desired simulations. Set to 12 for this study, can be changed for future use.

    Outputs:
    - Path to the chosen file.
    """
    
    # Read the CSV/Excel assuming it has headers
    df = pd.read_excel(lookup_table_path, engine='openpyxl')  # Adjust path if necessary
    
    # Find all rows where the 'NUMBER' column matches the fire name
    matching_rows = df[df['NUMBER'] == fire_name]
    
    if matching_rows.empty:
        raise ValueError(f"No file found for fire name: {fire_name}")

    # Filter matching rows to only include those with DURATION_HOURS >= 12
    matching_rows_12h = matching_rows[matching_rows['DURATION_HOURS'] >= simulation_duration]
    
    if matching_rows_12h.empty:
        raise ValueError(f"No files found for fire name '{fire_name}' with DURATION_HOURS >= {simulation_duration}.")

    # Extract the 'Sim_unique' values (which are the file names or unique identifiers)
    sim_unique_files = matching_rows_12h['Sim_unique'].tolist()

    if len(sim_unique_files) == 1:
        # If there's only one matching file, return it directly
        sim_unique = sim_unique_files[0]
        timestamp = matching_rows_12h[matching_rows_12h['Sim_unique'] >= sim_unique]['FORECAST_TIMESTAMP'].iloc[0]
        print(f"Found file: {sim_unique}")
        print(f"Corresponding FORECAST_TIMESTAMP: {timestamp}")
        return os.path.join(technosylva_folder_path, sim_unique, "Perimeters.geojson"), timestamp
    
    else:
        # If there are multiple files, user chooses which one
        print(f"Multiple files found for fire name '{fire_name}' with DURATION_HOURS == {simulation_duration}:")
        for i, file in enumerate(sim_unique_files):
            timestamp = matching_rows_12h[matching_rows_12h['Sim_unique'] == file]['FORECAST_TIMESTAMP'].iloc[0]
            print(f"{i + 1}: {file} (forecast time: {timestamp})")

        # User input
        while True:
            try:
                choice = int(input(f"Choose a file (1-{len(sim_unique_files)}): ")) - 1
                if 0 <= choice < len(sim_unique_files):
                    chosen_file = sim_unique_files[choice]
                    chosen_timestamp = matching_rows_12h[matching_rows_12h['Sim_unique'] == chosen_file]['FORECAST_TIMESTAMP'].iloc[0]
                    print(f"Chosen file: {chosen_file}")
                    print(f"Forecast time {chosen_timestamp}")
                    return os.path.join(technosylva_folder_path, chosen_file, "Perimeters.geojson"), timestamp
                else:
                    print(f"Invalid choice. Please enter a number between 1 and {len(sim_unique_files)}.")
            except ValueError:
                print("Invalid input. Please enter a valid number.")


def load_technosylva(fire_number, simulation_duration=12):
    """
    Function which loads Technosylva fire growth model geojson data
    Inputs:
    - fire_number: Alphanumeric fire number (as per BCWS categorization system)

    Outputs:
    - Tuple containing:
      - GeoDataFrame containing polygon of the most recent time in simulation
      - Timestamp of the selected data
    """
    # Get path and timestamp
    geojson_path, timestamp = lookup_technosylva(fire_number)
    
    # Load the GeoJSON 
    gdf = gpd.read_file(geojson_path)
    # Reproject the GeoDataFrame to EPSG:3005 
    gdf = gdf.to_crs(epsg=3005)
    #Encountered some issues with weird newlines, coerce into readable format
    gdf['hour'] = pd.to_numeric(gdf['hour'], errors='coerce')
    
    # Check for rows where 'hour' matches the simulation_duration
    matching_rows = gdf[gdf['hour'] == simulation_duration]

    if not matching_rows.empty:
        # If there are matching rows, select the first one
        gdf = matching_rows.iloc[[0]]
        latest_hour = 12
       
    else:
        # If no rows match, select the row with the latest hour
        latest_hour = gdf['hour'].max()
        gdf = gdf['hour']
        print(f"Warning: No data for hour {simulation_duration}. Using the latest available hour: {latest_hour}.")
    print('Technosylva data loaded')
    return gdf, timestamp, latest_hour

def lookup_firecast(fire_number, target_date):
    """
    Given a fire number, find the FireCast folder containing data for the simulation of that fire
    and get the shapefile from the folder whose date is closest to the given target_date.
    
    Inputs:
    - fire_number: Alphanumeric fire number (as per BCWS categorization system)
    - target_date: A datetime object representing the target date
    
    Outputs:
    - Path to chosen file
    - The datetime of the selected shapefile
    """
    # Append the fire_number to the root folder path
    subfolder = 'BCWS_2023_' + str(fire_number)
    full_path = os.path.join(firecast_folder_path, subfolder)
    
    # Ensure the full path exists
    if not os.path.isdir(full_path):
        raise ValueError(f"The path {full_path} does not exist or is not a directory.")
    
    closest_folder = None
    closest_date_diff = None
    
    # Traverse the subfolders
    for folder_name in sorted(os.listdir(full_path)):
        folder_path = os.path.join(full_path, folder_name)
        
        # Check if the current item is a directory
        if os.path.isdir(folder_path):
            # Extract the date from the folder name (e.g., 20230815101011)
            match = re.search(r'_(\d{8})(\d{6})', folder_name)
            if match:
                folder_date_str = match.group(1) + match.group(2)  # Combine the date and time
                folder_date = datetime.strptime(folder_date_str, "%Y%m%d%H%M%S")
                
                # Calculate the difference between the folder date and the target date
                date_diff = abs((folder_date - target_date).total_seconds())
                
                # Update closest folder if this is the closest one so far
                if closest_date_diff is None or date_diff < closest_date_diff:
                    closest_date_diff = date_diff
                    closest_folder = folder_path
    
    # If no folder was found, raise an error
    if closest_folder is None:
        raise FileNotFoundError(f"No suitable folder found under {full_path}.")

    # Now look for the .shp file inside the closest folder
    for file_name in os.listdir(closest_folder):
        if file_name.endswith(".shp"):
            # Extract the timestamp from the shapefile
            timestamp = datetime.strptime(re.search(r'_(\d{8}\d{6})_', file_name).group(1), "%Y%m%d%H%M%S")
            print(f'Found file: {file_name}')
            return os.path.join(closest_folder, file_name), timestamp
    
    # If no shapefile found, raise an error
    raise FileNotFoundError(f"No .shp file found in folder {closest_folder}.")


def load_firecast(fire_number, target_date):
    """
    Function which loads firecast fire growth model shapefile data
    Inputs:
    - fire_number: Alphanumeric fire number (as per BCWS categorization system)

    Outputs:
    - geodataframe containing polygon of most recent time in simulation
    """
    shp_path, timestamp = lookup_firecast(fire_number, target_date)
    shp_gdf= gpd.read_file(shp_path)
    shp_gdf = shp_gdf.to_crs(epsg=3005)
    print('Firecast data loaded')
    return shp_gdf.iloc[[-1]], timestamp


def load_perimeter(fire_number, event_date_1, event_date_2):
    """
    Function to load fire perimeter data based on two datetime objects,
    selecting the subfolder which has the earliest date that comes after both input dates.
    
    Inputs:
    - fire_number: Alphanumeric fire number (as per BCWS categorization system)
    - event_date_1: datetime object, first event date for comparison.
    - event_date_2: datetime object, second event date for comparison.
    
    Outputs:
    - geodataframe containing polygon of fire as seen in the selected subfolder
    - datetime object of the selected subfolder's date
    """
    
    # Convert input datetime objects to timestamp format
    event_date_1_timestamp = event_date_1.timestamp()
    event_date_2_timestamp = event_date_2.timestamp()

    # List of subfolders in the perimeters_path directory
    subfolders = [f for f in os.listdir(perimeters_path) if os.path.isdir(os.path.join(perimeters_path, f))]
    
    # Sort the subfolders by the date extracted from the folder name
    subfolders.sort(key=lambda folder: datetime.strptime(folder.split('_')[-1], '%Y%m%d%H%M'))
    
    # Initialize variables to keep track of the closest subfolder and its date
    selected_subfolder = None
    selected_date = None

    # Loop through each subfolder to find the date in the folder name
    for folder in subfolders:
        try:
            # Extract date from the folder name (assuming folder naming convention is like 'prot_current_fire_polys_YYYYMMDDHHMM')
            folder_date_str = folder.split('_')[-1]  # Extract last part (date)
            folder_date = datetime.strptime(folder_date_str, '%Y%m%d%H%M')
            
            # Ensure the folder date is after both event dates
            if folder_date.timestamp() > event_date_1_timestamp and folder_date.timestamp() > event_date_2_timestamp:
                # If we haven't selected a subfolder yet or this folder is later than the current selected one
                selected_subfolder = folder
                selected_date = folder_date
                
                # Now load the shapefile from this subfolder
                selected_folder_path = os.path.join(perimeters_path, selected_subfolder)
            
                perimeters = gpd.read_file(selected_folder_path)
                
                # Reproject the GeoDataFrame to EPSG:3005 (or another CRS if needed)
                perimeters = perimeters.to_crs(epsg=3005)
                
                # Check if either 'FIRE_NUMBE' or 'FIRE_NUM' exists in the GeoDataFrame
                fire_column = 'FIRE_NUMBE' if 'FIRE_NUMBE' in perimeters.columns else 'FIRE_NUM'
                
                # Select the polygon in the shapefile with the specified 'fire_column' value
                perimeter_gdf = perimeters[perimeters[fire_column] == fire_number]
                
                # If the polygon is found, return it
                if not perimeter_gdf.empty:
                    print('Perimeter loaded')
                    perimeter_gdf.loc[perimeter_gdf['geometry'].geom_type == 'Polygon', 'geometry'] = perimeter_gdf.loc[perimeter_gdf['geometry'].geom_type == 'Polygon', 'geometry'].apply(lambda geom: MultiPolygon([geom]))
                    return perimeter_gdf, selected_date
                
        except Exception as e:
            print(f"Error processing folder '{folder}': {e}")
    
    # If no valid polygon was found in any of the selected subfolders, raise an error
    raise ValueError(f"No polygon found for fire number {fire_number} in any valid folder after both event dates.")

            
def compute_areas(observed, predicted):
    """
    Compute areas of overlap and uniqueness for predicted and obsered data
    Inputs:
    - observed: geodataframe of observed data (from periemters shapefile, sentinel-2 cutting, etc.)
    - predicted: geodataframe of predicted data (from TechnoSylva simulations or FireCast simulations)

    Outputs:
    - area_only_observed: Area (in m^2) in the observed polygon but not the predicted polygon
    - area_only_predictes: Area (in m^2) in the predicted polygon but not the observed polygon
    - area_intersection: Area (in m^2) where the polygons overlap
    """
    # Compute the differences and intersection areas
    # Area in the selected shapefile polygon but not in the max_hour_polygon from the GeoJSON
    only_observed = observed.overlay(predicted, how='difference', keep_geom_type = False)
    area_only_observed = only_observed.geometry.area.sum()

    # 2. Area in the max_hour_polygon but not in the selected shapefile polygon
    only_predicted = predicted.overlay(observed, how='difference', keep_geom_type = False)
    area_only_predicted= only_predicted.geometry.area.sum()

    # 3. Area in the intersection of selected shapefile polygon and max_hour_polygon
    intersection = predicted.overlay(observed, how='intersection', keep_geom_type = False)
    area_intersection = intersection.geometry.area.sum()

    return area_intersection, area_only_predicted, area_only_observed

def get_skill_scores(observed, forecast):
    """
    Compute prediction skill score based on Brett Moore's thesis pg 39
    Inpputs are expected to have been put through compare_rasters first.
    Parameters:
    - observed: geodataframe of observed data (from periemters shapefile, sentinel-2 cutting, etc.)
    - predicted: geodataframe of predicted data (from TechnoSylva simulations or FireCast simulations)
  
    Returns:
    - bias (float32) : Magnitude of under/over prediction
    - hit_rate (float2): Frequency of correctly pedicted events
    - false_alarm_ratio (float32): Fraction of events incorrectly predicted
    - critical_success_index (float32): Measure of forecast overall skill
    """
    #Get areas:
    A, B, C = compute_areas(observed, forecast)
    #Calculate 
    bias = (A+B)/(A+C)
    hit_rate = A/(A+C)
    false_alarm_ratio = 1 - A/(A+B)
    critical_success_index = A/(A+B+C)

    print(f'bias : {bias}')
    print(f'hit rate: {hit_rate}')
    print(f'false alarm ratio: {false_alarm_ratio}')
    print(f'critical success index: {critical_success_index}')
    return bias, hit_rate, false_alarm_ratio, critical_success_index


def plot_data(observed, start_date, technosylva, firecast, plot_hotspots=True, show_values=True, simulation_time=12):
    """
    Compute prediction skill score based on Brett Moore's thesis pg 39
    Inpputs are expected to have been put through compare_rasters first.
    Parameters:
    - observed: geodataframe of observed data (from periemters shapefile, sentinel-2 cutting, etc.)
    - technosylva: geodataframe of predicted data (from TechnoSylva simulations)
    - firecast: geodataframe of predicted data (from FireCast simulations)
    - plot_hotpots: Bool value of whether FIRMS hotspot data (limited to 24 hours before ignition) is plotted
    - simulation_time: Time of simulation to consider
  
    Returns:
    - None, shows a plot and prints skill scores
    """
    # Plot the selected shapefile polygon and the GeoJSON polygon
    fig, ax = plt.subplots(figsize=(10, 10))

    # Use transparent colors for better overlap visibility
    observed.plot(ax=ax, color='blue', alpha=0.3, edgecolor='blue', linewidth=1, label='Observed')
    technosylva.plot(ax=ax, color='red', alpha=0.3, edgecolor='red', linewidth=1, label='Technosylva Forecast')
    firecast.plot(ax=ax, color='green', alpha=0.3, edgecolor='green', linewidth=1, label='Firecast Forecast')

    if plot_hotspots:
        # Read in FIRMS hotspot data
        firms_gdf = gpd.read_file(firms_path)
        firms_gdf = firms_gdf.to_crs(epsg=3005)
        
        # Get limits to get hotspots in plotting range
        x_limits = plt.gca().get_xlim()  
        y_limits = plt.gca().get_ylim()  

        # Spatial filter: Use GeoPandas' within method to filter the points that fall within the bounding box
        bounding_box = box(x_limits[0], y_limits[0], x_limits[1], y_limits[1])
        firms_gdf = firms_gdf[firms_gdf.geometry.within(bounding_box)]

        # Temporal filter: Get hotspots within a specific time range
        start_time = start_date - timedelta(hours=24)
        end_time = start_date + timedelta(hours=simulation_time)
        firms_gdf = firms_gdf[(firms_gdf['ACQ_DATE'] >= start_time) & (firms_gdf['ACQ_DATE'] <= end_time)]
        
        # Plot hotspots as points (scatter)
        firms_gdf.plot(ax=ax, color='black', markersize=10, label='FIRMS hotspots', marker='o')

    if show_values:
        bias_fc, hit_rate_fc, false_alarms_fc, csi_fc = get_skill_scores(observed, firecast)
        bias_ts, hit_rate_ts, false_alarms_ts, csi_ts = get_skill_scores(observed, technosylva)

    # Add labels and legend
    ax.set_title(f'Fire perimeters for fire number {fire_number}', fontsize=14)
    ax.set_xlabel('Longitude')
    ax.set_ylabel('Latitude')

    # Create proxies for the color and set the label with the name of the model
    red_proxy = mlines.Line2D([0], [0], color='red', lw=4, label='Technosylva')  # Only the name in the label
    green_proxy = mlines.Line2D([0], [0], color='green', lw=4, label='Firecast')  # Only the name in the label
    blue_proxy = mlines.Line2D([0], [0], color='blue', lw=4, label='Observed')  # Only the name in the label
    firms_proxy = mlines.Line2D([0], [0], color='black', markersize=10, label='FIRMS hotspots')

    # Add the legend with the model names and additional scores as text annotations
    fig.legend(handles=[red_proxy, green_proxy, blue_proxy, firms_proxy], loc='upper right')

    # Create the skill scores text
    skill_scores_text = (
        f"Technosylva\nCSI = {csi_ts:.3f}\nBias = {bias_ts:.3f}\nHit Rate = {hit_rate_ts:.3f}\nFalse Alarms = {false_alarms_ts:.3f}\n\n"
        f"Firecast\nCSI = {csi_fc:.3f}\nBias = {bias_fc:.3f}\nHit Rate = {hit_rate_fc:.3f}\nFalse Alarms = {false_alarms_fc:.3f}"
    )

    # Add the skill scores in a box
    plt.text(0.98, 0.5, skill_scores_text, transform=fig.transFigure, ha='right', va='center', fontsize=12,
            bbox=dict(facecolor='white', edgecolor='black', boxstyle='round,pad=0.5'))

    # Show the plot
    plt.show()


if __name__ == "__main__":

    args = [arg for arg in sys.argv if arg != '..']
    fire_number = args[1] 
    
    #Load data for selected fire
    technosylva, timestamp_ts, latest_hour = load_technosylva(fire_number)

    #Many firecast simualations, so pick one with start time closest to that of technosylva sim
    firecast, timestamp_fc = load_firecast(fire_number, timestamp_ts)
    #Get earliest possible perimeter occuring after both simulations start +
    perimeter, timestamp_perim = load_perimeter(fire_number, timestamp_ts, timestamp_fc)


    
    print(f'perimeter time: {timestamp_perim}')
    print(f'technosylva time: {timestamp_ts}')
    print(f'firecast time: {timestamp_fc}')
    plot_data(perimeter, timestamp_perim, technosylva, firecast, plot_hotspots=True, show_values = True)
    
    
    

