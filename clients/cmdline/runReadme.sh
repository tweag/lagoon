#!/bin/bash

LAGOON=bin/lagoon

set -x
set -e

# We assume freshly initialized server

${LAGOON} ingest -d 'Cancer tumor data' -n proteinatlas/cancer sources/proteinatlas/cancer.csv
${LAGOON} ingest -n embl-ebi/genes.fpkm http://ftp.ebi.ac.uk/pub/databases/arrayexpress/data/atlas/rnaseq/studies/ena/SRP033494/arabidopsis_thaliana/genes.fpkm.tsv
${LAGOON} list-sources
${LAGOON} show-source proteinatlas/cancer
${LAGOON} set-type proteinatlas/cancer -c c5 TEXT
${LAGOON} show-source proteinatlas/cancer
${LAGOON} make-typed proteinatlas/cancer
${LAGOON} set-type proteinatlas/cancer -c c5 INTEGER
${LAGOON} make-typed proteinatlas/cancer
${LAGOON} ingest --tag proteinatlas \
                   --tag subcellular  \
                   -n proteinatlas/subcellular \
                   sources/proteinatlas/subcellular_location.csv
${LAGOON} tag proteinatlas/subcellular localization
${LAGOON} untag proteinatlas/subcellular proteinatlas
${LAGOON} show-source proteinatlas/subcellular
${LAGOON} ingest -n targetvalidation/assoc sources/targetvalidation/assoc-10000.json
${LAGOON} ingest -n PharmKGB/amitriptyline/relatedGenes --json-path '{"relatedGenes":[_]}' sources/PharmGKB/dosingGuidelines/CPIC_Guideline_for_amitriptyline_and_CYP2C19_CYP2D6.json
