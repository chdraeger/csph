import pandas as pd
import numpy as np
import xarray as xr
import gcsfs
import os

# Download CMIP6 input data for the ML downscaling from Google GCSFS
#=======================================================================================================================
def get_cmip6(var, table_id, source_id, experiment_id, member_id, start_day, end_day, cell_data, cmip6_overview):

    # CMIP storage
    gcs = gcsfs.GCSFileSystem(token='anon')

    # Query zarr storage
    query_str = ("activity_id=='ScenarioMIP' & table_id == '" + table_id + "' & variable_id == '" + var
                 + "' & experiment_id == '" + experiment_id + "' & member_id == '" + member_id
                 + "' & source_id == '" + source_id + "'")
    df_var = cmip6_overview.query(query_str)

    if len(df_var.zstore) == 0:
        return None

    zstore = df_var.zstore.values[-1]  # take the most recent store (table is sorted by version)
    mapper = gcs.get_mapper(zstore)
    ds = xr.open_zarr(mapper, consolidated=True, use_cftime='True')

    ds_loc_per_shed = ds.sel(lat=cell_data["lat"].to_xarray(),
                      lon=cell_data["lon"].to_xarray(),
                      method='nearest')  # location
    cmip_to_shed_loc = pd.DataFrame({"lon": ds_loc_per_shed["lon"].values,  # cmip_lon
                                    "lat": ds_loc_per_shed["lat"].values})  # cmip_lat

    # Save only unique CMIP
    cmip_loc_unique = cmip_to_shed_loc.drop_duplicates()

    # Create cmip id
    cmip_loc_unique.insert(0,'cmip_id',[f'cmip_cell{i + 1}' for i in range(len(cmip_loc_unique))])
    ds_loc = ds.sel(lat=cmip_loc_unique["lat"].to_xarray(),
                      lon=cmip_loc_unique["lon"].to_xarray(),
                      method='nearest')  # location

    # Clean mapping tables
    cmip_to_shed_loc.insert(0,'fs_uid',cell_data["fs_uid"])
    mapping_df = pd.merge(cmip_to_shed_loc, cmip_loc_unique, on=['lon', 'lat'], how='left')
    mapping_df = mapping_df.drop(columns=['lon', 'lat'])

    if not table_id == 'fx':  # if data is not time-invariant
        # select time slice
        ds_loc = ds_loc.sel(time=slice(start_day, end_day))

        ds_loc_var = pd.DataFrame(ds_loc[var].values,
                      index=ds_loc['time'].values.astype('datetime64[D]').astype('str'),
                      columns=cmip_loc_unique['cmip_id'] )
    else:
        ds_loc_var = pd.DataFrame([ds_loc[var].values],
                          index = ['fx'],
                          columns=cmip_loc_unique['cmip_id'])

    return [mapping_df, ds_loc_var]


if __name__ == "__main__":

    country = "Madagascar"
    basedir = "/home/christina/Data/csph"
    filename = f'{basedir}/shape_files/centroids_healthsheds_{country}.csv'

    cmip6_overview_file = f'{basedir}/cmip6-zarr-consolidated-stores.csv'
    # from 'https://storage.googleapis.com/cmip6/cmip6-zarr-consolidated-stores.csv'

    # Download period
    start_day = '2015-01-01'
    end_day = '2100-01-01'

    # CMIP
    experiments = ['ssp126', 'ssp245', 'ssp370', 'ssp585']

    cmip6_models = ['MPI-ESM1-2-HR',  # (0.9 x 0.9) Germany
                    'NorESM2-MM',  # (1.25 x 0.94) Norway
                    'ACCESS-CM2',  # (1.88 x 1.25) Australia
                    'ACCESS-ESM1-5',  # (1.88 x 1.25) Australia
                    'BCC-CSM2-MR',  # (1.13 x 1.12) China
                    'CESM2',
                    'CESM2-WACCM',  # (1.25 x 0.94) USA
                    'CMCC-CM2-SR5',  # Italy
                    'CMCC-ESM2',  # (1.0 × 1.0) Italy
                    'CNRM-ESM2-1',  # (1.41 x 1.40) France
                    'CNRM-CM6-1-HR',  # (0.50 x 0.50) France
                    'EC-Earth3',  # (0.70 x 0.70) Europe
                    'GFDL-CM4',  # US
                    # 'GFDL-ESM4',  # (1.3 x 1.0) USA - gr1 instead of gn
                    'HadGEM3-GC31-MM',  # (0.83 x 0.56) UK
                    'IITM-ESM',  # India, only until 2099
                    # 'INM-CM5-0',  # (2.0 x 1.5) Russia
                    'KACE-1-0-G',  # (1.88 x 1.25) South Korea
                    'MIROC6',  # (1.41 x 1.40) Japan
                    'MPI-ESM1-2-HR',  # (0.9 x 0.9) Germany
                    'MRI-ESM2-0',  # (1.1 x 1.1) Japan
                    'TaiESM1',  # Thailand
                    'UKESM1-0-LL'  # (1.9 x 1.3) UK need to use 12-30
                    ]

    my_variables = [
        ['tas', 'day'],  # mean near-surface temperature
        ['tasmin', 'day'],  # min near-surface temperature
        ['tasmax', 'day'],  # max near-surface temperature
        ['sfcWind', 'day'],  # mean near-surface wind speed
        ['sfcWindmax', 'day'],  # max near-surface wind speed
        ['rsds', 'day'],  # surface downwelling shortwave flux in air
        ['hurs', 'day'],  # near-surface relative humidity
        ['hursmin', 'day'],  # min near-surface relative humidity
        ['hursmax', 'day'],  # max near-surface relative humidity
        ['huss', 'day'],  # near-surface specific humidity
        ['pr', 'day'],  # precipitation_flux
        ['mrsos', 'day'],  # moisture in upper portion (10cm) of the soil column
        ['clt', 'day'],  # total cloud cover percentage
        ['hfss', 'day'],  # surface upward sensible heat flux
        ['hfls', 'day'],  # surface upward latent heat flux
        ['loadbc', 'Eday'],  # load of black carbon aerosol
        ['orog', 'fx']  # surface altitude
    ]

    # Get CMIP data
    sheds = pd.read_csv(filename, sep = ",")

    # Get GCSFS overview file
    cmip6_overview = pd.read_csv(cmip6_overview_file)

    for source_id in cmip6_models:
        print(f"Starting: {source_id}")

        if not os.path.exists(f"{basedir}/CMIP6_{country}"):
            os.makedirs(f"{basedir}/CMIP6_{country}/{source_id}")

        for experiment_id in experiments:
            print(f"\tScenario: {experiment_id}")

            for my_var, my_table_id in my_variables:
                print(f"\t\tVariable: {my_var}")

                member_ids = cmip6_overview.query(
                    "activity_id == 'ScenarioMIP' and source_id == @source_id and experiment_id == @experiment_id and "
                    "variable_id == @my_var and table_id == @my_table_id"
                )["member_id"].unique()

                for member_id in member_ids:
                    print(f"\t\t\tMember: {member_id}")

                    result_file = (f"{basedir}/CMIP6_{country}/{source_id}/"
                                   f"{experiment_id}_{source_id}_{member_id}_{my_var}.csv")

                    if os.path.isfile(result_file):
                        print(f'\t\t\t\tFile exists. Continuing')
                        continue

                    lat = sheds["lat"]
                    lon = sheds["lon"]

                    cell_data = pd.DataFrame({
                        'lat': lat.to_xarray(),
                        'lon': lon.to_xarray(),
                        'fs_uid': sheds["fs_uid"]
                    })

                    result = get_cmip6(my_var, my_table_id, source_id, experiment_id, member_id,
                                                      start_day, end_day, cell_data, cmip6_overview)

                    if result is None:
                        print(f'\t\t\t\tNo gcsfs data for {my_var}.')
                        continue
                    else:
                        print(f'\t\t\t\tData obtained for {my_var}.')
                        [mapping_df, df] = result

                    # Save unique CMIP data
                    # ======================================================================================================
                    df = df.round(2)  # round to 2 digits
                    df.index.name = "date"
                    df.to_csv(result_file, index=True)

                    # Save shed-to-CMIP mapping
                    # ==============================================================================================================
                    file_path_mapping = f"{basedir}/CMIP6_{country}/{source_id}/CMIP_mapping_{source_id}.csv"

                    if not os.path.exists(file_path_mapping):
                        mapping_df.to_csv(file_path_mapping, index=False)
