import glob
import pandas


def meaner(tool: str):
    csv_files = glob.glob(f'{tool}_*.csv')

    df_collection = pandas.read_csv(csv_files[0], sep=',', header=0, names=['n', 'real', 'user', 'sys', 'mRSS'])

    for csv_file in csv_files[1:]:
        df = pandas.read_csv(csv_file, sep=',', header=0, names=['n', 'real', 'user', 'sys', 'mRSS'])
        df_collection += df

    df_collection = df_collection / 11
    df_collection.to_csv(f"mean_{tool}.csv", sep=',', index=False)


meaner('rslts')
meaner('rsltc')
