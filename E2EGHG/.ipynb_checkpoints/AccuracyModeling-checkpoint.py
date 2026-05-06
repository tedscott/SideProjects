# includes
import numpy as np
import pandas as pd
import pickle
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor

# warnings around column names in RF and GBR models can be ignored
# if you catch these warnings instead you could remove this ignore
import warnings 
warnings.filterwarnings("ignore")

# function to create accuracy model
def accuracy_model(spans, metric = 'r2'):
    '''
    spans: a data frame of firefly gathered data with columns: span_id, measurements_count, energy_joules, cpu_ms, concurrency
    metric: whether to use rmse or r2 to choose best model. Default: r2 (aka R^2)

    Function determines the median energy for each cpu_ms (should be span_id eventually?), 
    then calculates the absolute % deviation from the median energy for each span collected.

    Using that deviation value it build models for (deviation) accuracy vs span length (cpu_ms), 
    # of measurements per span (measurements_count), and concurrency

    Currenty it builds models via multi-linear regression, a random forest, and a gradient boosted forest.
    More model types can easily be added.

    If metric is "rmse" it will choose which model has the lowest RMSE and save it out via pickle as "model.pkl"
    If metric is "r2" it will choose which model has the highest R^2 and save it out via pickle as "model.pkl"

    The best model can then be loaded via pickle and used for prediction.
    
    returns: the name of model type that was the best ("MLR", "RF", "GBR"), and the filename of the serialized (pickled) model

    usage:
    best = accuracy_model(spans, 'rmse')
    
    '''

    # reduce to expected columns
    # ***
    # *** must be edited if column naming convention changes, for example span_id -> service, cpu_ms -> duration
    # ***
    df_spans = spans[['span_id', 'measurements_count', 'energy_joules', 'cpu_ms', 'concurrency']]

    # filter energy_joules == 0 values before modeling
    df_spans = df_spans.query("(energy_joules > 0)")

    # calculate median energy per cpu_ms
    # ***
    # *** this should change to be median energy 'per service' since cpu_ms won't be fixed/constant
    # ***
    df_median_e_cpu = df_spans[['energy_joules','cpu_ms']].copy()
    df_median_e_cpu = df_median_e_cpu.groupby(['cpu_ms'], as_index=False).median()

    # copy median energy to the main data frame
    # ***
    # *** this should change to be median energy 'per service' since cpu_ms won't be fixed/constant
    # ***
    cpu_ms_vals = np.unique(df_spans['cpu_ms'])
    for i in cpu_ms_vals:
        df_spans.loc[df['cpu_ms'] == i, 'median_energy_cpu_ms'] = df_median_e_cpu[df_median_e_cpu['cpu_ms'] == i].energy_joules.values[0]

    # *** TODO: delete the temporary data frame df_median_e_cpu

    # calculate accuracy value column
    # value is absolute % deviation from median for that cpu_ms (TODO: should be per service)
    # accuracy of 1 means the value of energy_joules is within 1% of the median energy
    df_spans['accuracy'] = np.abs(100*(df_spans.median_energy_cpu_ms - df_spans.energy_joules)/df_spans.median_energy_cpu_ms)

    # build model input and label arrays
    # ***
    # *** here span_id will be replaed with the name of the service once data includes it
    # ***
    X = df_spans.drop(['span_id', 'energy_joules','median_energy_cpu_ms','accuracy'], axis=1)
    y = df_spans['accuracy']

    # create train/test split of data, 80/20 split
    x_train, x_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 47)

    # multi-linear regression model
    mlr = LinearRegression()  
    mlr.fit(x_train, y_train)
    # make predictions based on test data
    y_pred = mlr.predict(x_test)
    # calculate rmse and r2 metrics
    rmse_lm = np.sqrt(mean_squared_error(y_test, y_pred))
    r2_lm = r2_score(y_test, y_pred)

    # random forest regression model
    # ***
    # *** can be updated to do a grid search of optimized parameters
    # ***
    rfr = RandomForestRegressor(n_estimators=100, random_state=47)
    rfr.fit(x_train, y_train)
    y_pred = rfr.predict(x_test)
    rmse_rf = np.sqrt(mean_squared_error(y_test, y_pred))
    r2_rf = r2_score(y_test, y_pred)

    # gradient boosted regression model
    # ***
    # *** can be updated to do a grid search of optimized parameters
    # ***
    gbr = GradientBoostingRegressor(n_estimators=100, learning_rate=0.1, max_depth=3)
    gbr.fit(x_train, y_train)
    y_pred = gbr.predict(x_test)
    rmse_gb = np.sqrt(mean_squared_error(y_test, y_pred))
    r2_gb = r2_score(y_test, y_pred)

    # build data frame of model results
    # ***
    # *** if additional models are added this must be updated
    # ***
    df_models = pd.DataFrame({'Model':['MLR','RF','GBR'],
                         'rmse':[rmse_lm, rmse_rf, rmse_gb],
                          'r2':[r2_lm, r2_rf, r2_gb]})
    
    # find best model
    # use value for argument metric for determining best model
    if (metric == "r2"):
        df_models.sort_values(by=["r2"], ascending=False, inplace=True)
    else:
        df_models.sort_values(by=["rmse"], inplace=True)
        
    best_model = df_models['Model'].iloc[0]

    # pickle and save the best model
    # ***
    # *** edit this filename for path and name if desired
    # ***
    filename = "./model.pkl"
    if (best_model == 'RF'):
        with open(filename,'wb') as f:
            pickle.dump(rfr,f)
    elif (best_model == 'GBR'):
        with open(filename,'wb') as f:
            pickle.dump(gbr,f)
    else:
        with open(filename,'wb') as f:
            pickle.dump(mlr,f)

    return best_model, filename


def determine_measurements(span_duration = 500, concurrency = 1, desired_accuracy_pct = 50, filename = "model.pkl"):
    '''
    span_duration: the span execution length in milliseconds (default 500 ms)
    concurrency: count of overlapping spans for this instance (default 1)
    desired_accuracy_pct: the desired accuracy in percent [deviation from median energy] (default 50%)
    filename: path to pickled model

    The best pre-determined accuracy model is loaded via pickle and used for prediction.
    
    returns: 
     - the recommended number of measurements for the desired accuracy 
     - the estimated accuracy for that number
     - the minimum accuracy value
     - the number of measurements for the minimum (best) accuracy

    usage:
    num, acc, num_best, best_acc  = determine_measurements(250, 2, 100, 'model.pkl')
    
    '''

    # load the best model so far
    # ***
    # *** this should be part of a try-catch or something else in case model does not exist?
    # ***
    with open(filename, 'rb') as f:
        acc_model = pickle.load(f)

    # range of possible number of measurements to test
    # ***
    # *** this range can be adjusted based on overhead model
    # ***
    x = np.arange(1,10,1)
    y=[]
    for i in x:
        y.append(acc_model.predict([[i,span_duration,concurrency]]))

    mod_results = pd.DataFrame({'num' : x, 'accuracy' : y})
    
    #find row with closest value to desired_accuracy
    df_closest = mod_results.iloc[(mod_results['accuracy'] - desired_accuracy_pct).abs().argsort()[:1]]

    # sort to find min accuracy and num of measurements
    mod_results.sort_values(by=["accuracy"], inplace=True)
    # return results
 
    return df_closest.num.values[0], df_closest.accuracy.values[0][0], mod_results['num'].iloc[0], mod_results['accuracy'].iloc[0][0]


# examples of usage - change filename for newer data and directory
df_spans = pd.read_csv("./span_concurrent_new.csv")

# find best model for this data
# in this case, "RF" random forest
best_model, model_file = accuracy_model(df_spans)

# Outputs: 5 33.344447354750116 1 23.61492833134136
num, acc, min_num, best_acc = determine_measurements(250, 2, 100, 'model.pkl')
print(num, acc, min_num, best_acc)

# Outputs: 1 23.61492833134136 1 23.61492833134136
num, acc, min_num, best_acc = determine_measurements(375, 2, 10, 'model.pkl')
print(num, acc, min_num, best_acc)