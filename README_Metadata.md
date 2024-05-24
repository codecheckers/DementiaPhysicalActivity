# This README file was generated on 20-03-2024 by Daniela Gawehns
---

# GENERAL INFORMATION

Title of Dataset: Using commercial wearables to estimate activity levels of nursing home residents with dementia 

## Authors

### Principal Investigator Information
Name: Dr. Adriana Petronella Anna van Beek
ORCID: https://orcid.org/0000-0001-08077-4855
Institution: NIVEL, Netherlands
Email: svanbeek@verenso.nl

### Author/Co-Investigator
Name: Dr. Matthijs van Leeuwen
ORCID: https://orcid.org/0000-0002-0510-3549
Institution: LIACS, Leiden
Email: m.van.leeuwen@liacs.leidenuniv.nl

### Co-Investigator
Name: Daniela Gawehns
ORCID: https://orcid.org/0009-0002-9678-9012
Institution: LIACS, Leiden
Email: gawehnsd@liacs.leidenuniv.nl

### Co-Investigator
Name: Suzanne Portegijs
ORCID: https://orcid.org/0009-0002-4367-3235
Institution: NIVEL, Utrecht
Email: s.portegijs@nivel.nl

## Date of data collection: 
May 2021 (not the exact dates to preserve privacy of participants)

## Geographic location of data collection: 
Kaatsheuvel, the Netherlands

## Information about funding sources that supported the collection of the data: 
ZonMw funded research. Projectnumber: 733050846


# DATA & FILE OVERVIEW

## Data File Description: 
The data of this project comes with one datafile called "linkedData21.csv" this file includes all data needed to recreate the correlation matrix in the manuscript. It containes only complete sets (i.e., only people with both, wearables and observational data). 

## Data File: linkedData.csv

### data

- linkedData.csv : Contains behavioral observations, normalized 24 hrs data and MAD data information. 

The MAD data is calculated over 5sec intervals and this forms the basis of the following derivatives:
SD
Median
Qant 
Frag
Rel

The 24hrs data was cleaned of time intervals that the watch was not in use (not on patient wrist but recording while being charged or being with the research team).

The 24 hrs data and the behavioral observations are normalized to 30 min. See more details in attributes list. 



#### attributes of linkedData.csv

| **Attribute name** | **Description** 
|---|---|
| experiment_id |  identifier of the experiment | 
| dataset_label |  dataset label of the experiment | 
| plate_number | the identifier of the plate | 
| plate_content | the description of the plate content | 


# Relationship between files: 


# METHODOLOGICAL INFORMATION

Hardware used: Samsung Wearable, Gear Fit 2 Pro
Software used: WEARDA https://research-software-directory.org/software/wearda
Note on wearable data collection: we have different timelines per patient, some participated 4 days for 8 hrs each day, others missed hours due to changing sleep cycles or leaving the building to visit family or a doctor's appointment. The data got was normalized to 30 min intervals to make up for those difference.
The MEDLO observation schema was used for the behavioral observations.
