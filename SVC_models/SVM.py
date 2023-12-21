## Code adapted from: https://github.com/fungenomics/scCoAnnotate/tree/main/Scripts/SVC
#--------------- Libraries -------------------
import numpy as np
import pandas as pd
from sklearn.svm import LinearSVC
from sklearn.calibration import CalibratedClassifierCV
import anndata as ad
import sys
import scanpy as sc
import pickle
import os
import random

# Set seed
random.seed(123456) 

#--------------- Parameters -------------------
ref_path = str(sys.argv[1])
lab_path = str(sys.argv[2])
sample_path = str(sys.argv[3])
out_path = str(sys.argv[4])
out_other_path = os.path.dirname(str(sys.argv[4]))
threads = int(sys.argv[5])

# Function to get the column name and maximum value for each row
def get_max_column_and_value(row):
    pred_label = row.idxmax()
    proba_label = row.max()
    return pred_label, proba_label

#--------------- Data -------------------------
# read the data
ref = pd.read_csv(ref_path,
                  index_col=0,
                  sep=',',
                  engine='c') ## could be pyarrow to make it faster but it needs to be install on the module and has many problems with dependencies

labels = pd.read_csv(lab_path,
                     index_col = 0,
                     sep=',')

# check if cell names are in the same order in labels and ref
order = all(labels.index == ref.index)

# throw error if order is not the same 
if not order:
  sys.exit("@ Order of cells in reference and labels do not match")

adata = ad.AnnData(X = ref,
                   obs = dict(obs_names=ref.index.astype(str)),
                   var = dict(var_names=ref.columns.astype(str))
)

# Now I normalize the matrix with scanpy:
# Normalize each cell by total counts over all genes,
# so that every cell has the same total count after normalization.
# If choosing `target_sum=1e6`, this is CPM normalization
# 1e4 similar as Seurat
sc.pp.normalize_total(adata, target_sum=1e4)

# Logarithmize the data:
sc.pp.log1p(adata)


print('@ READ QUERY')
query = pd.read_csv(sample_path,
                    index_col=0,
                    sep=',',
                    engine='c') ## could be pyarrow to make it faster but it needs to be install on the module and has many problems with dependencies

print('@ DONE')

query = ad.AnnData(X = query,
                   obs = dict(obs_names=query.index.astype(str)),
                   var = dict(var_names=query.columns.astype(str))
)

# Now I normalize the matrix with scanpy:
# Normalize each cell by total counts over all genes,
# so that every cell has the same total count after normalization.
# If choosing `target_sum=1e6`, this is CPM normalization
# 1e4 similar as Seurat
sc.pp.normalize_total(query, target_sum=1e4)

#Logarithmize the data:
sc.pp.log1p(query)

## Transform labels to array

label = np.array(labels['label'])

#------------- Train SVM Lineal -------------
# kernel could be ‘linear’, ‘poly’, ‘rbf’, ‘sigmoid’, ‘precomputed’
# When the constructor option probability is set to True, class membershipsq
# probability estimates (from the methods predict_proba and predict_log_proba)
# are enabled. 
svm_model = LinearSVC()

# Calibrate the model using 5-fold cross validation
SVM_model = CalibratedClassifierCV(svm_model,
                                  n_jobs = threads) #Default
SVM_model.fit(adata.X, label)

pred_proba = SVM_model.predict(query.X)

print('@ WRITTING PREDICTIONS')
pred_df = pd.DataFrame({'cell': query.obs_names, "SVMLinear": pred_proba})
pred_df.to_csv(out_path, index = False)
print('@ DONE')
