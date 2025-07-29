import requests
import pandas as pd
from datetime import datetime
import numpy as np

class NOAANCEIClient:
    def __init__(self, token=None):
        self.base_url = "https://www.ncei.noaa.gov/cdo-web/api/v2/"
        if not token or token == "YOUR_TOKEN_HERE":
            raise ValueError("Please provide a valid NOAA API token")
        self.token = token
        self.headers = {
            "token": self.token
        }
        print(f"NOAA Client initialized with token: {self.token[:5]}...")  # Log token prefix for debugging

    def _make_request(self, endpoint, params=None):
        """Helper method to make API requests with error handling"""
        try:
            url = f"{self.base_url}{endpoint}"
            print(f"Making request to: {url}")
            print(f"Params: {params}")
            
            response = requests.get(url, headers=self.headers, params=params)
            response.raise_for_status()
            
            print(f"Response status: {response.status_code}")
            print(f"Response content: {response.text[:500]}")  # Print first 500 chars of response
            
            return response.json()
            
        except requests.exceptions.HTTPError as http_err:
            print(f"HTTP error occurred: {http_err}")
            print(f"Response content: {http_err.response.text if hasattr(http_err, 'response') else 'No response'}")
            raise
        except Exception as err:
            print(f"An error occurred: {err}")
            raise

    def get_stations(self, location=None, dataset_id="GHCND"):
        """
        Get available stations for a given location
        """
        params = {
            "datasetid": dataset_id,
            "limit": 1000
        }
        if location:
            params["locationid"] = location
        
        response = requests.get(
            f"{self.base_url}stations",
            headers=self.headers,
            params=params
        )
        response.raise_for_status()
        return response.json()

    def get_weather_data(self, station_id, start_date, end_date):
        """
        Get daily weather data for a specific station
        """
        params = {
            "stationid": station_id,
            "startdate": start_date,
            "enddate": end_date,
            "limit": 1000,
            "units": "standard"
        }
        
        response = requests.get(
            f"{self.base_url}data",
            headers=self.headers,
            params=params
        )
        response.raise_for_status()
        return response.json()

    def process_weather_data(self, weather_data):
        """
        Process raw weather data into a pandas DataFrame
        """
        records = []
        for record in weather_data["results"]:
            date = datetime.strptime(record["date"], "%Y-%m-%dT%H:%M:%S")
            records.append({
                "date": date.date(),
                "tmax": record.get("TMAX", np.nan),
                "tmin": record.get("TMIN", np.nan),
                "prcp": record.get("PRCP", np.nan),
                "snow": record.get("SNOW", np.nan),
                "snwd": record.get("SNWD", np.nan)
            })
        
        return pd.DataFrame(records)

    def get_location_by_coordinates(self, latitude, longitude):
        """
        Find the nearest weather station to the given coordinates
        """
        try:
            print(f"Searching for stations near coordinates: {latitude}, {longitude}")
            
            # First try with 1 degree bounding box
            params = {
                "datasetid": "GHCND",
                "extent": f"{max(-90, float(latitude)-1)},{max(-180, float(longitude)-1)},{min(90, float(latitude)+1)},{min(180, float(longitude)+1)}",
                "sortfield": "datacoverage",
                "sortorder": "desc",
                "limit": 1
            }
            
            print("Attempting first search with params:", params)
            results = self._make_request("stations", params)
            
            if results.get('results') and len(results['results']) > 0:
                station = results['results'][0]
                print(f"Found station: {station.get('name')} (ID: {station.get('id')})")
                return station['id']
                
            # If no station found, try a wider search (5 degree bounding box)
            print("No stations found in initial search, trying wider area...")
            params = {
                "datasetid": "GHCND",
                "extent": f"{max(-90, float(latitude)-5)},{max(-180, float(longitude)-5)},{min(90, float(latitude)+5)},{min(180, float(longitude)+5)}",
                "sortfield": "datacoverage",
                "sortorder": "desc",
                "limit": 5
            }
            
            print("Attempting wider search with params:", params)
            results = self._make_request("stations", params)
            
            if results.get('results') and len(results['results']) > 0:
                station = results['results'][0]
                print(f"Found station in wider search: {station.get('name')} (ID: {station.get('id')})")
                return station['id']
                
            print("No stations found in wider search either")
            return None
            
        except Exception as e:
            print(f"Error in get_location_by_coordinates: {str(e)}")
            if hasattr(e, 'response') and e.response is not None:
                print(f"Response status: {e.response.status_code}")
                print(f"Response content: {e.response.text}")
            return None
