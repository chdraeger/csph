import cdsapi

def download_era_sl(variable, year):

    dataset = "reanalysis-era5-single-levels"
    request = {
        "product_type": ["reanalysis"],
        "variable": variable,
        "year": [year],
        "month": [
            "01", "02", "03",
            "04", "05", "06",
            "07", "08", "09",
            "10", "11", "12"
        ],
        "day": [
            "01", "02", "03",
            "04", "05", "06",
            "07", "08", "09",
            "10", "11", "12",
            "13", "14", "15",
            "16", "17", "18",
            "19", "20", "21",
            "22", "23", "24",
            "25", "26", "27",
            "28", "29", "30",
            "31"
        ],
        "time": [
            "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00",
            "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00",
            "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00",
            "21:00", "22:00", "23:00"
        ],
        "data_format": "grib",
        "download_format": "unarchived",
        # "area": [-11.9, 43, -26, 51]  # Madagascar
        "area": [31, 80, 26, 89]  # Nepal
    }

    client = cdsapi.Client()

    filename = f'nepal/era5_sl_{variable}_{year}.grib'
    client.retrieve(dataset, request, filename)


variable = "volumetric_soil_water_layer_1"
# volumetric_soil_water_layer_1, total_precipitation, surface_solar_radiation_downwards, 2m_temperature, 2m_dewpoint_temperature

years = list(range(2015, 2024, 1)) # last year doesn't count
for year in years:
   download_era_sl(variable, year)
