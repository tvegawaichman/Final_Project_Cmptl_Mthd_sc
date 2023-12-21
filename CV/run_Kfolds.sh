#!/usr/bin/bash
#SBATCH --job-name="Generate_folds"
#SBATCH --time=24:00:00
#SBATCH --account=kleinman-lab
#SBATCH --cpus-per-task=1
#SBATCH --mem=40G
#SBATCH --output=/project/kleinman/tomas.vegawaichman/from_hydra/LaunchJobs/SLURM/output/%x.out
#SBATCH --error=/project/kleinman/tomas.vegawaichman/from_hydra/LaunchJobs/SLURM/output/%x.err

module load scCoAnnotate/2.0

ref_path='/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/data/ref/expression.csv'
lab_path='/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/data/ref/labels.csv'
out_path='/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output'
threads='1'
n_folds='5'
Rscript --vanilla /project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/code/Script/benchmark/subset_folds.R $ref_path $lab_path $out_path $threads $n_folds