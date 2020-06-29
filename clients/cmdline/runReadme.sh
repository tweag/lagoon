#!/bin/bash

DATALAKE=bin/datalake

set -x
set -e

# We assume freshly initialized server

${DATALAKE} ingest -d 'Cancer tumor data' -n proteinatlas/cancer sources/proteinatlas/cancer.csv
${DATALAKE} ingest -n embl-ebi/genes.fpkm http://ftp.ebi.ac.uk/pub/databases/arrayexpress/data/atlas/rnaseq/studies/ena/SRP033494/arabidopsis_thaliana/genes.fpkm.tsv
${DATALAKE} list-sources
${DATALAKE} show-source proteinatlas/cancer
${DATALAKE} set-type proteinatlas/cancer -c c5 TEXT
${DATALAKE} show-source proteinatlas/cancer
${DATALAKE} make-typed proteinatlas/cancer
${DATALAKE} set-type proteinatlas/cancer -c c5 INTEGER
${DATALAKE} make-typed proteinatlas/cancer
${DATALAKE} ingest --tag proteinatlas \
                   --tag subcellular  \
                   -n proteinatlas/subcellular \
                   sources/proteinatlas/subcellular_location.csv
${DATALAKE} tag proteinatlas/subcellular localization
${DATALAKE} untag proteinatlas/subcellular proteinatlas
${DATALAKE} show-source proteinatlas/subcellular
${DATALAKE} ingest -n targetvalidation/assoc sources/targetvalidation/assoc-10000.json
${DATALAKE} ingest -n PharmKGB/amitriptyline/relatedGenes --json-path '{"relatedGenes":[_]}' sources/PharmGKB/dosingGuidelines/CPIC_Guideline_for_amitriptyline_and_CYP2C19_CYP2D6.json
