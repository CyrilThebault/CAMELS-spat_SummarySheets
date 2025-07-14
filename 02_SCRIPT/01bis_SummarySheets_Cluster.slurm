#!/bin/bash
#SBATCH --job-name=SummarySheets
#SBATCH --partition=cpu2022,cpu2021,cpu2019
#SBATCH --mem=5G
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=00-01:00:00
#SBATCH --array=2

# set --array=1-1426 if you want to create the Summary Sheets for all catchments

# ------------------------------------------------------------------------------
# --- 0. Environment setup
# ------------------------------------------------------------------------------

. /work/comphyd_lab/local/modules/spack/2024v5/lmod-init-bash
module unuse $MODULEPATH
module use /work/comphyd_lab/local/modules/spack/2024v5/modules/linux-rocky8-x86_64/Core/
module load r/4.4.1 gdal/3.9.2 proj/9.4.1

# ------------------------------------------------------------------------------
# --- 1. Run the code
# ------------------------------------------------------------------------------

# Get SLURM ID
input=${SLURM_ARRAY_TASK_ID}

Rscript 01bis_SummarySheets_Cluster.R ${input}