#!/usr/bin/bash
#SBATCH --job-name="SCENIC_fold4"
#SBATCH --time=24:00:00
#SBATCH --account=kleinman-lab
#SBATCH --cpus-per-task=1
#SBATCH --mem=100G
#SBATCH --output=/project/kleinman/tomas.vegawaichman/from_hydra/LaunchJobs/SLURM/output/%x.out
#SBATCH --error=/project/kleinman/tomas.vegawaichman/from_hydra/LaunchJobs/SLURM/output/%x.err

module load scCoAnnotate/2.0

ref_path='/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/fold4/train_SCENIC.csv'
lab_path='/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/fold4/train_labels.csv'
sample_path='/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/fold4/test_SCENIC.csv'
out_path='/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/fold4/SVM/pred_SCENIC.csv'
threads='1'

python /project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/code/SVM_SCENIC.py $ref_path $lab_path $sample_path $out_path $threads