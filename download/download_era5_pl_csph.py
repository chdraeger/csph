import cdsapi

def download_era_pl(variable, year):

    dataset = "reanalysis-era5-pressure-levels"
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
            "00:00", "06:00", "12:00",
            "18:00"
        ],
        "pressure_level": [
            "650", "700", "750",
            "800", "850", "900",
            "950", "1000"
        ],
        "data_format": "grib",
        "download_format": "unarchived",
        # "area": [-11.9, 43, -26, 51]  # Madagascar
        "area": [31, 80, 26, 89]  # Nepal
    }

    client = cdsapi.Client()

    filename = f'nepal/era5_pl_{variable}_{year}.grib'
    client.retrieve(dataset, request, filename)

variable = "geopotential"  # temperature

years = list(range(2020, 2024, 1)) # last year doesn't count
for year in years:
   download_era_pl(variable, year)
