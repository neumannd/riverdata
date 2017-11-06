#!/bin/bash

cd /media/neumannd/work_dell/68_Tools_Software/25_R/10_packages/riverdata

for iFile in `ls man/*.Rd | sed 's#man/##g' | sed 's/\.Rd//g'`; do
 echo "Processing file ${iFile}.Rd"
 R CMD Rdconv --type=html man/${iFile}.Rd > html/${iFile}.html
done
