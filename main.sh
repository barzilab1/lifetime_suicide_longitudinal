
#!/bin/sh
### General options
### -- set the job Name --
###BSUB -J My_Serial_R
### -- ask for 20 core --
#BSUB -n 20
### -- specify that we need 2GB of memory per core/slot --
###BSUB -R "rusage[mem=2GB]"
### -- specify that we want the job to get killed if it exceeds 3 GB per core/slot --
###BSUB -M 3GB
### -- set the email address --
# please uncomment the following line and put in your e-mail address,
# if you want to receive e-mail notifications on a non-default address
#BSUB -u Elina.Visoki@Pennmedicine.upenn.edu
### -- send notification at start --
#BSUB -B
### -- send notification at completion --
#BSUB -N
### -- Specify the output and error file. %J is the job-id --
### -- -o and -e mean append, -oo and -eo mean overwrite --
#BSUB -o Output_%J.out
#BSUB -e Error_%J.err
module load R/3.6.1
R --no-save CMD BATCH main.R

### scripts for copy
### scp -r  visokie@transfer.pmacs.upenn.edu:~/longitudinal_suicide/life_time_suicide_longitudinal/Rplots.pdf /Users/visokie/Desktop/tes/
### scp -r /Users/visokie/life_time_suicide_longitudinal/ visokie@transfer.pmacs.upenn.edu:~/longitudinal_suicide