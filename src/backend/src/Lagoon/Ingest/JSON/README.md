# Type inference

## JSON type inference

### Scalars

``` javascript
1
```

Inferred type: `number`

``` javascript
"hi"
```

Inferred type: `string`

``` javascript
true
```

Inferred type: `bool`

``` javascript
null
```

Inferred type: `nullable unknown`

### Arrays

``` javascript
[]
```

Inferred type: `[unknown]`

``` javascript
[1]
```

Inferred type: `[number]`

``` javascript
[1,2,3]
```

Inferred type: `[number]`

``` javascript
["hi"]
```

Inferred type: `[string]`

``` javascript
[true]
```

Inferred type: `[bool]`

``` javascript
[null]
```

Inferred type: `[nullable unknown]`

``` javascript
[1,null]
```

Inferred type: `[nullable number]`

``` javascript
[1,"true"]
```

Inferred type: `[mixed]`

``` javascript
[[1],[2,3]]
```

Inferred type: `[[number]]`

``` javascript
[[1],[null]]
```

Inferred type: `[[nullable number]]`

``` javascript
[[1],null]
```

Inferred type: `[nullable [number]]`

``` javascript
[[1],[null],null]
```

Inferred type: `[nullable [nullable number]]`

``` javascript
[[1],[true]]
```

Inferred type: `[[mixed]]`

``` javascript
[[1],true]
```

Inferred type: `[mixed]`

``` javascript
[true,[1]]
```

Inferred type: `[mixed]`

``` javascript
[1,["John","Doe"]]
```

Inferred type: `[mixed]`

``` javascript
[[1],[true],["hi"]]
```

Inferred type: `[[mixed]]`

``` javascript
[[1],[true],["hi"],true]
```

Inferred type: `[mixed]`

### Objects

``` javascript
{}
```

Inferred type: `{}`

``` javascript
{"a": true}
```

Inferred type: `{a:bool}`

``` javascript
{"a": {"b": 1}}
```

Inferred type: `{a:{b:number}}`

``` javascript
{"a": true, "b": 1}
```

Inferred type: `{a:bool, b:number}`

``` javascript
{"a": 1
,"b": 2
,"c": 3
}
```

Inferred type: `{a:number, b:number, c:number}`

``` javascript
{"a": 1
,"b": true
,"c": "hi"
,"d": null
}
```

Inferred type: `{a:number, b:bool, c:string, d:nullable unknown}`

``` javascript
{"id": 1, "name": {"firstName": "John", "lastName": "Doe"}}
```

Inferred type: `{id:number, name:{firstName:string, lastName:string}}`

``` javascript
{"a": 1, "b": [2,3], "c": 4}
```

Inferred type: `{a:number, b:[number], c:number}`

``` javascript
[1,{"a":2},3]
```

Inferred type: `[mixed]`

``` javascript
{"a": "hi"
,"b": [1,2,3]
,"c":
 {"ca": 4
 ,"cb": [5,6,7]
 ,"cc": [8, {"cca": 9}]
 }
}
```

Inferred type: `{a:string, b:[number], c:{ca:number, cb:[number], cc:[mixed]}}`

``` javascript
[{"a":1},{"a":2}]
```

Inferred type: `[{a:number}]`

``` javascript
[{"a":1},{"a":true}]
```

Inferred type: `[{a:mixed}]`

``` javascript
[{"a":1},{"b":true}]
```

Inferred type: `[{a:optional number, b:optional bool}]`

``` javascript
[{"b":true},{"a":1}]
```

Inferred type: `[{a:optional number, b:optional bool}]`

``` javascript
[{"a":1},{"a":2,"b":true}]
```

Inferred type: `[{a:number, b:optional bool}]`

### Examples from real data sources

The dosing guidelines at https://www.pharmgkb.org/downloads/ contains a number of files in JSON format; the smallest is `Professional_Society_Guideline_for_allopurinol_and_HLA_B.json`, for which we infer the following type:

```
{@context:string,
 @id:string,
 groups:[unknown],
 id:string,
 name:string,
 objCls:string,
 relatedChemicals:[{@context:string,
                    @id:string,
                    id:string,
                    name:string,
                    objCls:string}],
 relatedGenes:[{@context:string,
                @id:string,
                id:string,
                name:string,
                objCls:string,
                symbol:string}],
 source:string,
 summaryHtml:string,
 textHtml:string,
 variants:[unknown],
 xrefs:[unknown]}
```

The largest (at 4.5 MB) is `CPIC_Guideline_for_amitriptyline_and_CYP2C19_CYP2D6.json`, for which we infer

```
{@context:string,
 @id:string,
 groups:[{@context:string,
          @id:string,
          annotations:[{id:number,
                        text:string,
                        textHtml:string,
                        type:{@context:string,
                              @id:string,
                              id:number,
                              resource:string,
                              term:string,
                              termId:string}}],
          genotypes:[string],
          id:string,
          name:string,
          objCls:string,
          strength:{@context:string,
                    @id:string,
                    id:number,
                    resource:string,
                    term:string,
                    termId:string}}],
 id:string,
 name:string,
 objCls:string,
 relatedChemicals:[{@context:string,
                    @id:string,
                    id:string,
                    name:string,
                    objCls:string}],
 relatedGenes:[{@context:string,
                @id:string,
                id:string,
                name:string,
                objCls:string,
                symbol:string}],
 source:string,
 summaryHtml:string,
 summaryMarkdown:{html:string,
                  id:number,
                  internalLinks:[unknown],
                  markdown:string,
                  pmids:[unknown],
                  relatedObjects:[unknown],
                  rsids:[unknown]},
 textHtml:string,
 textMarkdown:{html:string,
               id:number,
               internalLinks:[string],
               markdown:string,
               pmids:[unknown],
               relatedObjects:[unknown],
               rsids:[unknown]},
 variants:[unknown],
 xrefs:[{@context:string,
         @id:string,
         id:number,
         resource:string,
         xrefId:string}]}
```

The "Association objects" file from
https://www.targetvalidation.org/downloads/data (3.4 GB unzipped) is not strictly a valid
JSON file, as it contains multiple top-level objects. However, type inference
handles this case without problems; we infer

```
{association_score:{datasources:{cancer_gene_census:number,
                                 chembl:number,
                                 disgenet:number,
                                 europepmc:number,
                                 eva:number,
                                 eva_somatic:number,
                                 expression_atlas:number,
                                 gene2phenotype:number,
                                 gwas_catalog:number,
                                 intogen:number,
                                 phenodigm:number,
                                 reactome:number,
                                 uniprot:number,
                                 uniprot_literature:number},
                    datatypes:{affected_pathway:number,
                               animal_model:number,
                               genetic_association:number,
                               known_drug:number,
                               literature:number,
                               rna_expression:number,
                               somatic_mutation:number},
                    overall:number},
 disease:{efo_info:{label:string,
                    path:[[string]],
                    therapeutic_area:{codes:[string], labels:[string]}},
          id:string},
 evidence_count:{datasources:{cancer_gene_census:number,
                              chembl:number,
                              disgenet:number,
                              europepmc:number,
                              eva:number,
                              eva_somatic:number,
                              expression_atlas:number,
                              gene2phenotype:number,
                              gwas_catalog:number,
                              intogen:number,
                              phenodigm:number,
                              reactome:number,
                              uniprot:number,
                              uniprot_literature:number},
                 datatypes:{affected_pathway:number,
                            animal_model:number,
                            genetic_association:number,
                            known_drug:number,
                            literature:number,
                            rna_expression:number,
                            somatic_mutation:number},
                 total:number},
 id:string,
 is_direct:bool,
 target:{gene_info:{name:string, symbol:string}, id:string}}
```

For the evidence data from the same website we infer

```
{access_level:string,
 data_release:string,
 disease:{biosample:optional {id:optional string, name:string},
          efo_info:{efo_id:string,
                    label:string,
                    path:[[string]],
                    therapeutic_area:{codes:[string], labels:[string]}},
          id:string,
          name:optional [string]},
 drug:optional {id:[string],
                max_phase_for_all_diseases:{label:string, numeric_index:number},
                molecule_name:string,
                molecule_type:string},
 evidence:{biological_model:optional {allele_ids:string,
                                      allelic_composition:string,
                                      date_asserted:string,
                                      evidence_codes:[string],
                                      genetic_background:string,
                                      is_associated:bool,
                                      model_gene_id:string,
                                      model_id:string,
                                      phenotypes:[{id:string, label:string, term_id:string}],
                                      provenance_type:{database:{id:string, version:string}},
                                      resource_score:{method:{description:string},
                                                      type:string,
                                                      value:number},
                                      species:string,
                                      zygosity:string},
           comparison_name:optional string,
           confidence_level:optional string,
           date_asserted:optional string,
           disease_model_association:optional {date_asserted:string,
                                               disease_id:string,
                                               evidence_codes:[string],
                                               human_phenotypes:[{id:string,
                                                                  label:string,
                                                                  term_id:string}],
                                               is_associated:bool,
                                               model_id:string,
                                               model_phenotypes:[{id:string,
                                                                  label:string,
                                                                  term_id:string}],
                                               provenance_type:{database:{id:string,
                                                                          version:string}},
                                               resource_score:{method:{description:string},
                                                               type:string,
                                                               value:number}},
           drug2clinic:optional {date_asserted:string,
                                 evidence_codes:[string],
                                 is_associated:bool,
                                 max_phase_for_disease:{label:string, numeric_index:number},
                                 provenance_type:{expert:{status:bool}},
                                 resource_score:{method:{url:string}, type:string, value:number},
                                 status:optional string,
                                 urls:[{nice_name:string, url:string}]},
           evidence_codes:[string],
           evidence_codes_info:[[{eco_id:string, label:string}]],
           experiment_overview:optional string,
           gene2variant:optional {date_asserted:string,
                                  evidence_codes:[string],
                                  functional_consequence:string,
                                  is_associated:bool,
                                  provenance_type:{database:{dbxref:optional {id:string,
                                                                              url:optional string,
                                                                              version:string},
                                                             id:string,
                                                             version:string},
                                                   expert:optional {statement:string, status:bool},
                                                   literature:optional {references:[{lit_id:string}]}},
                                  resource_score:{type:string, value:number},
                                  urls:optional [{nice_name:string, url:string}]},
           is_associated:optional bool,
           known_mutations:optional mixed,
           literature_ref:optional {lit_id:string,
                                    mined_sentences:[{d_end:number,
                                                      d_start:number,
                                                      section:string,
                                                      t_end:number,
                                                      t_start:number,
                                                      text:string}]},
           log2_fold_change:optional {percentile_rank:number, value:number},
           orthologs:optional {date_asserted:string,
                               evidence_codes:[string],
                               human_gene_id:string,
                               is_associated:bool,
                               model_gene_id:string,
                               provenance_type:{database:{id:string, version:string}},
                               resource_score:{method:{description:string},
                                               type:string,
                                               value:number},
                               species:string},
           provenance_type:optional {database:{dbxref:optional {id:string,
                                                                url:string,
                                                                version:string},
                                               id:string,
                                               version:string},
                                     expert:optional {statement:string, status:bool},
                                     literature:optional {references:[{lit_id:string}]}},
           reference_replicates_n:optional number,
           reference_sample:optional string,
           resource_score:optional {method:optional {description:string,
                                                     reference:optional string,
                                                     url:optional string},
                                    type:string,
                                    value:number},
           target2drug:optional {action_type:string,
                                 date_asserted:string,
                                 evidence_codes:[string],
                                 is_associated:bool,
                                 mechanism_of_action:string,
                                 provenance_type:{database:{id:string, version:string},
                                                  expert:{status:bool},
                                                  literature:optional {references:[{lit_id:string}]}},
                                 resource_score:{method:{url:string}, type:string, value:number},
                                 urls:[{nice_name:string, url:string}]},
           test_replicates_n:optional number,
           test_sample:optional string,
           unique_experiment_reference:optional string,
           urls:optional [{nice_name:string, url:string}],
           variant2disease:optional {date_asserted:string,
                                     evidence_codes:[string],
                                     gwas_panel_resolution:optional number,
                                     gwas_sample_size:optional number,
                                     is_associated:bool,
                                     provenance_type:{database:{dbxref:optional {id:string,
                                                                                 url:optional string,
                                                                                 version:string},
                                                                id:string,
                                                                version:string},
                                                      expert:optional {statement:string,
                                                                       status:bool},
                                                      literature:optional {references:[{lit_id:string}]}},
                                     resource_score:{method:{description:string,
                                                             url:optional string},
                                                     type:string,
                                                     value:number},
                                     unique_experiment_reference:string,
                                     urls:optional [{nice_name:string, url:string}]}},
 id:string,
 literature:optional {references:[{lit_id:string}]},
 scores:{association_score:number},
 sourceID:string,
 target:{activity:string,
         complex_members:optional [string],
         complex_type:optional string,
         gene_info:{geneid:string, name:string, symbol:string},
         id:string,
         target_class:optional [string],
         target_name:optional string,
         target_type:string},
 type:string,
 unique_association_fields:{alleleOrigin:optional string,
                            biological_objects:optional string,
                            biological_subjects:optional string,
                            chembl_molecules:optional string,
                            chembl_targets:optional string,
                            clinvarAccession:optional string,
                            comparison_name:optional string,
                            dbSnps:optional string,
                            disease:optional string,
                            disease_acronym:optional string,
                            disease_uri:optional string,
                            efo_classification:optional string,
                            evidence_level:optional string,
                            gene:optional string,
                            geneID:optional string,
                            gwas_panel_resolution:optional string,
                            method:optional string,
                            method_description:optional string,
                            mutation_type:optional string,
                            mutations:optional string,
                            object:optional string,
                            phenotype:optional string,
                            predictionModel:optional string,
                            projectName:optional string,
                            publicationIDs:optional string,
                            pubmed_refs:optional string,
                            pvalue:optional string,
                            reaction_id:optional string,
                            role:optional string,
                            role_description:optional string,
                            sample_size:optional string,
                            score:optional number,
                            source_id:optional string,
                            study_id:optional string,
                            study_name:optional string,
                            symbol:optional string,
                            target:optional string,
                            targetId:optional string,
                            tumor_type:optional string,
                            tumor_type_acronym:optional string,
                            uniprot_release:optional string,
                            unique_id:optional string,
                            url:optional string,
                            variant:optional string,
                            variant_id:optional string},
 validated_against_schema_version:string,
 variant:optional {id:[string], type:string}}
 ```
