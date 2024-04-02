# Description about the files in the extdata directory

All of the files in the extdata directory, with the exeception of
the `validation_summary.tsv` file, were manually created/edited.

These files are necessary for 1) the bugphyzz package to work properly and
2) a convenient way for curators to update the files.

What follows is the description of each file.

## [attribute_sources.tsv](./attribute_sources.tsv)

Contains the original sources of bugphyzz annotations.

Columns:

| Column | Description |
| --- | --- |
| Attribute_source | The abbreviated name of the source. This is the one used in the output of `importBugphyzz`.|
| Confidence_in_curation | One of three: high, medium, low.|
| Evidence \* | The type of evidence supporting the annotations in the original source. |
| full_source | The full name of the source. It could be a citation, a link to webpage or project, etc. |

**\* Evidence**
Options:
- exp. Inferred from experiment.
- igc. Inferred from genomic context.
- nas. Non-traceable author statement.
- tas. Traceable author statement. 
- tax. IBD = inferred from biological aspect of descendant.
- asr. anscestral state reconstruction.

## [attributes.tsv](./attributes.tsv)

It contains the description of the attribute values included in bugphyzz.

Columns:

| Column name | Description |
| --- | --- |
| attribute | The name of the attribute. |
| validity | The type of the attribute, either 'logical' for categorical values or 'numeric' for ranges. |
| ontology | Mapping to an ontology term (if possible). |
| attribute_group | The group(s) where an attribute name could be found. |
| description | The meaning of the attribute described in the 'attribute' column. |

## [spreadsheet_custmolinks.tsv](./spreadsheet_customlinks.tsv)

Links for datasets in google spreadsheets that are not in tidy format and
need to be converted to tidy format.

Columns:

| Column name | Description |
| --- | --- |
| physiology | Name of the physiology or attribute group. |
| ontology | If applicably, mapping to an ontology term. |
| link | Link to the csv export. |
| functionname | The name of the function (unexported) in bugphyzz. |
| source_link |  Link to the source spreadsheet on Google Docs. |

## [spreadsheet_links.tsv](./spreadsheet_links.tsv)

Links for datasets in spreadsheets that are already in tidy format.

Columns:

| Column name | Description |
| --- | --- |
| physiology | Name of the dataset. Equivalent to attribute group. |
| ontolgy | Mapping to an ontology term if applicable. |
| sig_type | The type of the signature. Either range for numeric or logical for categorical and binary values. |
| link | Link to the csv export. |
| source_link | Link to the source spreadsheet. |

## [curation_template.tsv](./curation_template.tsv)

Contains the formal description of the data model in the spreadsheets imported
by the physiologies funtion.

Columns:

| Column name | Description |
| --- | --- |
| column_name | Name of the column in the spreadsheet. |
| requiredness | Whether the column needs to be present in all attributes (required) or not (optional). |
| required_column_order | Order of the column if required. |
| attribute_types | Types of the attributes. Not R classes. Import function behavior will change based on this values. |
| valid_values | A regular expression with the values that are accepted. Except for the column 'Attriubte' which is a function name ('.attributes'). |
| value_test | If 'string' the test of validity is based on the regular expresion. If '.attributes', it's based on the function .attriubtes (no exported). |
| column_class | One of the R classes for atomic vectors. |

## [thresholds.tsv](./thresholds.tsv)

Tresholds for converting numeric/range data to categorical.

Columns:

| Column name | Description |
| ----------- | ----------- |
| Attribute_group | Physiology. Name of the spreadsheet. |
| Attribute | The actual attribute used. |
| lower | lower threshold. |
| uppwer | upper threshold. |
| unit | units used for numeric data. |


## [validation_summary.tsv](./validation_summary.tsv)

This file was obtained from the waldronlab/taxPProValidtion repository hosted
on GitHub. It contains the results of a 10-fold cross-validation analysis
of the ancestral state reconstruction (ASR) methods used to get
annotations for the package.

Detailed information (code and explanation) can be found on the repository.

The file was downloaded on April 1st, 2024 from this URL (pay attention to 
the commit hash):

https://raw.githubusercontent.com/waldronlab/taxPProValidation/e736097/validation_summary.tsv

The code used, which was executed directly in the extdata directory, was:

```bash
wget https://raw.githubusercontent.com/waldronlab/taxPProValidation/e736097/validation_summary.tsv
```

| Column name | Description |
| ----------- | ----------- |
| method | ASR method (phytools-ltp or castor-ltp) |
| rank | Taxonomic rank (all, genus, species, strain). "all" was used for the output of `importBugphyzz`.|
| physiology | The name of the attribute group. |
| attribute | The name of the attribute or attribute value.|
| mcc_mean | The mean of Mathew's correlation coefficient for discrete only. |
| mcc_sd | The standard deviation of Matthew's correlation coefficient for discrete only.|
| r2_mean | The mean of the R-squared for numeric only.|
| r2_sd | The standard deviation of the R-squared for numeric only.|
| ltp_bp | Interesction between taxa in bugphyzz (per attribute/attribute value) and the ltp tree.|
| bp | Taxa in bugphyzz (per attribute/attribute value). |
| ltp_bp_phys | Intersection between taxa in bugphyz (per attribute group) and the ltp tree.|
| bp_phys | Total taxa in the bugphyzz (per attribute group).|
| ltp | Number of taxa in the LTP tree. |
| nsti_mean | The mean of the NSTI values. This was calculated for all physiologies, but it's only relevant for numeric ones.|
| nsti_sd | The standard deviation of the NSTI values. This was calculated for all physiologies, but it's only relevant for numeric ones.|
| ltp_bp_per | Intersection of LTP tree and bugphyzz (per attribute/attribute value) in numbers.|
| ltp_bp_phys_per | Intersection of LTP tree and bugphyzz (per physiology/attribute group) in percentage.|

