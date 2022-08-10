#!/bin/sh
#SBATCH -J BeeSim
#SBATCH -p batch #run on batch queue
#SBATCH --time=1-00:00:00 #day-hour:minute:second 
#SBATCH -n 12
#SBATCH --mem = 32000 #request MG memory 
#SBATCH --output=PName.%N.%j.out #your output file 
#SBATCH --error=PName.%j.err #your error file

# Set up job environment:
module purge   # clear any inherited modules
module load R/4.1.1

# Run Rscript in a clean R instance
Rscript --vanilla --verbose /cluster/home/btidsw01/Bee-Simulation/Cluster Simulation Script.R

