#!/bin/sh
#SBATCH -J BeeSim
#SBATCH --time=07-00:00:00  #requested time (DD-HH:MM:SS)
#SBATCH -p batch    #running on "mpi" partition/queue
#SBATCH -N 1    #1 nodes
#SBATCH -n 40   #2 tasks total
#SBATCH -c 1    #1 cpu cores per task
#SBATCH --mem=32g  #requesting 2GB of RAM total
#SBATCH --output=MyJob.%j.%N.out  #saving standard output to file, %j=JOBID, %N=NodeName
#SBATCH --error=MyJob.%j.%N.err   #saving standard error to file, %j=JOBID, %N=NodeName
#SBATCH --mail-type=ALL    #email optitions
#SBATCH --mail-user=ben.tidswell@tufts.edu

# Set up job environment:
module purge   # clear any inherited modules
module load R/4.1.1

# Run Rscript in a clean R instance
Rscript --vanilla --verbose /cluster/home/btidsw01/Bee-Simulation/Cluster_Simulation_Script_Heat.R 6000

