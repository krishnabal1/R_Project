# Plant health text exploration and cluster-based evaluation

Unlabeled gardening Stack Exchange questions are clustered for topic exploration.
K-means is fit only on unlabeled Stack Exchange questions. A separate,
source-linked calibration subset maps clusters to conditions, and disjoint
source pages are held out for evaluation. Test labels never name clusters.

## Run

Open this folder in RStudio and source `main.R`, or run `Rscript main.R`.
The original CSV is read from the project root. Set `PLANT_DATA_FILE` to
override its location. Install required R packages once with:

```r
install.packages(c("readr", "dplyr", "stringr", "ggplot2", "xml2",
                   "text2vec", "stopwords", "Matrix", "irlba",
                   "cluster", "dbscan"))
```

## Workflow

1. Run `main.R` to clean the local Stack Exchange CSV and compare k-means, DBSCAN, and hierarchical clusters.
2. Review `outputs/tables/04_cluster_summary_top_terms.csv` and representative examples.
3. Enter human-written topic summaries in `data/review/cluster_topic_labels.csv`; these are not diagnoses.
4. Add short paraphrased symptom cases with a supported condition and source URL to `data/verified/verified_plant_cases.csv`.
5. Set `verified` to `yes` only after checking a reliable diagnostic source; enter verifier and date.
6. Rerun `main.R`; calibration cases map k-means clusters to conditions, while disjoint source pages are held out for evaluation.
7. Use `predict_new_question.R` to assign a new question to a mapped cluster. Unmapped clusters return an explicit message.

Unsupervised scores (silhouette, Davies-Bouldin, Calinski-Harabasz) describe
cluster structure, not disease accuracy. Cluster-mapping metrics are computed
only from a held-out split grouped by source page. Never use held-out
labels to assign a condition to a cluster. Stack Exchange questions and
the old manual annotation sheet are not used as diagnosis labels.
Prefer university extension, plant diagnostic clinic, or government sources.
Store paraphrased symptom text and a source link, not copied articles.
Model outputs are possible causes for triage, not a confirmed plant diagnosis.
Existing Stack Exchange annotation files are preserved but are not used to train the model.

See `WORKFLOW.md` for data quality and evaluation requirements.
