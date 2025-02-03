ssiddall Jan 2025

#Script that will acess open meteo API to pull weather data from available models. When run, it will ask for lats and longs for area of interest, and then which wx models.
#Program will take weatherr models listed, in order, and append them so that when the first model runs out, the ssecond model will pick up were the first left off and finish 
#to the end of that forecast model. Can use as many models as needed as long as the longest model is used last.

#next steps: TODO 1: identify correct models to use. TODO 2: clean up inputs. TODO 3: select which dates each model should print? TODO 4: select which variables to use from each model?

import requests
import pandas as pd

# STEP 1: Get User Input for Location and Models
latitude = input("Enter latitude (e.g., 48.4284): ").strip()
longitude = input("Enter longitude (e.g., -123.3656): ").strip()

# TODO available models with forecast length
available_models = {
    "best_match": "Auto-selected model (Varies)",
    "ecmwf": "10 days",  # Renamed from ecmwf_ifs025
    "gfs": "16 days",  # Renamed from gfs_seamless
    "gem": "10 days",  # Renamed from gem_seamless
    "gem_hrdps": "2 days",  # Renamed from gem_hrdps_continental
    "knmi": "10 days",  # Renamed from knmi_seamless
    "dmi": "10 days"  # Renamed from dmi_seamless
}

# TODO map simple names to API names, for easier input
model_name_mapping = {
    "ecmwf": "ecmwf_ifs025",
    "gfs": "gfs_seamless",
    "gem": "gem_seamless",
    "gem_hrdps": "gem_hrdps_continental",
    "knmi": "knmi_seamless",
    "dmi": "dmi_seamless"
}

# TODO display the models for user to select. with length
print("\nAvailable weather models:")
for model, days in available_models.items():
    print(f"  - {model}: {days}")

# TODO prompt user for model(s)
print("\nAvailable weather models:")
for model, days in available_models.items():
    print(f"  - {model}: {days}")

selected_models = input("\nEnter the models you want to use (comma-separated, in order): ").split(",")
selected_models = [model.strip().lower() for model in selected_models]

# TODO change simple names back to API names after user input.
api_models = [model_name_mapping[model] for model in selected_models if model in model_name_mapping]

# STEP 2: Pull down wx data from open meteo. using sample code from open meteo website
weather_data_frames = []

for model in selected_models:
    if model not in available_models:
        print(f"Model '{model}' is not available. Skipping...")
        continue

    api_url = "https://api.open-meteo.com/v1/forecast"
    forecast_days = int(available_models[model].split()[0]) if available_models[model][0].isdigit() else 10

    params = {
        "latitude": latitude,
        "longitude": longitude,
        "hourly": [
            "temperature_2m", "relative_humidity_2m", "dew_point_2m", "precipitation",
            "cloud_cover", "cloud_cover_low", "cloud_cover_mid", "cloud_cover_high",
            "visibility", "vapour_pressure_deficit", "wind_speed_10m", "wind_gusts_10m",
            "soil_temperature_0cm", "soil_temperature_6cm", "soil_temperature_18cm",
            "soil_temperature_54cm", "soil_moisture_0_to_1cm", "soil_moisture_1_to_3cm",
            "soil_moisture_3_to_9cm", "soil_moisture_9_to_27cm", "soil_moisture_27_to_81cm"
        ],
        "models": model,
        "forecast_days": forecast_days,
        "timezone": "America/Los_Angeles"
    }

    try:
        response = requests.get(api_url, params=params)
        response.raise_for_status()
        weather_data = response.json()

        # Convert JSON response into a pandas DataFrame
        hourly_data = weather_data["hourly"]
        df = pd.DataFrame(hourly_data)

        # Convert timestamps to pandas datetime
        df["date"] = pd.to_datetime(df["time"])
        df = df.drop(columns=["time"])  # Drop original time column

        # Add metadata
        df["model"] = model
        df["forecast_days"] = forecast_days
        df["latitude"] = latitude
        df["longitude"] = longitude

        # Store the DataFrame
        weather_data_frames.append(df)
        print(f"Successfully fetched data for {model} ({forecast_days} days).")

    except requests.exceptions.RequestException as e:
        print(f"Error fetching data for {model}: {e}")

# SSTEP 3: process and format data to transition from model to model
if weather_data_frames:
    df_combined = pd.concat(weather_data_frames, ignore_index=True)

    # Convert 'date' column to datetime
    df_combined['date'] = pd.to_datetime(df_combined['date'])

    # Identify first model's forecast end time
    first_model = df_combined['model'].iloc[0]
    first_model_duration = df_combined[df_combined['model'] == first_model]['forecast_days'].max()
    first_model_end_time = df_combined['date'].min() + pd.Timedelta(days=first_model_duration)

    # Keep first model data until forecast ends
    df_filtered = df_combined[(df_combined['model'] == first_model) & (df_combined['date'] <= first_model_end_time)]
    df_second_model = df_combined[df_combined['model'] != first_model].copy()

    # Ensure second model starts at the next hour
    last_timestamp_first_model = df_filtered["date"].max()
    df_second_model = df_second_model[df_second_model["date"] >= last_timestamp_first_model + pd.Timedelta(hours=1)]

    # Merge the data
    df_final = pd.concat([df_filtered, df_second_model], ignore_index=True)

    # STEP 4: make the outputs look familiar, or be usable. upload spotwx csv files for formating
    standard_format_path = "SpotWx_Forecast_standard_version.csv"
    prometheus_format_path = "SpotWx_Forecast_prometheus_version.csv"

    df_standard_format = pd.read_csv(standard_format_path)
    df_prometheus_format = pd.read_csv(prometheus_format_path)

    standard_columns = list(df_standard_format.columns)
    prometheus_columns = list(df_prometheus_format.columns)

    # reorder columns and append extra columns on end
    def reorder_columns(df, reference_columns):
        common_columns = [col for col in reference_columns if col in df.columns]
        extra_columns = [col for col in df.columns if col not in reference_columns]
        return df[common_columns + extra_columns]

    # make the dataframes
    df_standard_output = reorder_columns(df_final, standard_columns)
    df_prometheus_output = reorder_columns(df_final, prometheus_columns)

    # STEP 5: kick it out and save it
    standard_output_path = "open_meteo_weather_data_standard.csv"
    prometheus_output_path = "open_meteo_weather_data_prometheus.csv"

    df_standard_output.to_csv(standard_output_path, index=False)
    df_prometheus_output.to_csv(prometheus_output_path, index=False)

    print(f"\n✅ Weather data saved successfully!")
    print(f" - Standard format: {standard_output_path}")
    print(f" - Prometheus format: {prometheus_output_path}")

# print statement if you messed up
else:
    print("No data fetched. Please check your inputs and try again.")
