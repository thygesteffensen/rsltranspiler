import glob

import pandas
import numpy


def meaner(tool: str):
    csv_files = glob.glob(f'{tool}_*.csv')

    n = []
    time = [[]]
    mrss = [[]]

    for csv_file in csv_files:
        print(csv_file)
        df = pandas.read_csv(csv_file, sep=',', header=0, names=['n', 'real', 'user', 'sys', 'mRSS'])

        for i, row in df.iterrows():
            if (i + 1) > len(n):
                n.append(0)
                time.append([])
                mrss.append([])
            n[i] = int(row['n'])
            time[i].append(row['real'])
            mrss[i].append(row['mRSS'])

    with open(f'mean_{tool}.csv', "w") as f:
        print('n,time_mean,time_std_err,mrss_mean,mrss_std_err', file=f)
        for (n, t, m) in zip(n, time, mrss):
            print(f'{n},{numpy.mean(t)},{numpy.std(t)},{numpy.mean(m)},{numpy.std(m)}', file=f)

    print("Done")


meaner('rslts_t')
meaner('rslts_s')
meaner('rsltc_t')
meaner('rsltc_s')
