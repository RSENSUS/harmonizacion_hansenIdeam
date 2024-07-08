# Homologation of Global and National Forest Cover Products

**A goal of large-area land cover mapping is to produce globally
consistent characterizations that have local relevance and utility; that
is, reliable information across scales.** (Hansen et al 2013)

This is the base for a package to obtain forest cover maps from the
Global Forest Cover dataset by Hansen et al (2013) and run a pixelwise
comparison with Colombian Forest Cover Datasets produced by the
**Forests and Carbon Monitoring System - SMBYC** at the National
Institute for Meteorology and Environmental Studies (IDEAM).

# Introduction

## Objectives

### Main

1.  Assess the level of agreement between forest cover products by
    iteratively comparing multiple canopy cover for GLAD forest products
    with the national forest cover assessment
2.  Identify the threshold that produces the highest level of overall
    agreement between both datasets
3.  Perform the process for ecologically homogeneous spatial units.

### Specific:

Fill spatial and temporal gaps in national forest coverage reporting,
but homogenized to the national standards (policy) Reduce costs and
increase speed in obtaining new data.

Use local experience and datasets as validation datasets.

Identify assess over/sub register of forest coverage/forest loss in the
national dataset.

## Study Area

Continental extent of Colombia, divided in 379 spatial units (biomes).
(Londoño et al, 2017) )  
\## Biomes Map

## GLAD Forest Cover Data

National data on forest cover and change for Colombia (IDEAM) The Forest
and Carbon Monitoring System (Sistema de Monitoreo de Bosques y Carbono)
ascribed to IDEAM in Colombia (Table 1) produces the official datasets
on forest cover and change for the country and is the reference for
national and regional policy (DNP, 2020) and to fulfill Colombia’s
international reporting obligations (IDEAM, 2019). The forest cover
product represents forest presence in areas where vegetation meets the
national definition of forests (Table 2). This dataset explicitly
excludes commercial forestry, oil palm, and trees intended for
agricultural use (Galindo et al., 2014a,b).

Forest change is detected through a Principal Component Analysis of
merged data of end and start years. Forest change maps are produced
first by overlaying the maps representing forest cover as described
above. These maps then are subjected to a quality control process that
consists of manual reclassification of mistaken pixels based on visual
interpretation of the last available images for the year (IDEAM, 2019).
Any pixels with low quality observations due mostly to the presence of
clouds or cloud shadows in at least one of the mapped years are removed
from the analysis of forest loss in the entire time series. Maps were
produced at irregular intervals for the years 1990, 2000, 2005 and 2010
and annually from 2012 onwards. The IDEAM national forest change data is
the reference source to produce the official national report of annual
rates of forest loss and therefore it was used for comparing estimates
of forest cover loss with the GFC product. For change between 2012-2013
we used V5, for 2013-2014 we used V6, for 2014-2015, 2015-2016 , and
2016-2017 we used V7, and for 2017-2018, and 2018-19 we used V8. We used
Magna-Sirgas as the coordinate system for map comparison, given that
this is the official reference coordinate system for Colombia.

## Global Forest Cover Change (GFC)

The High-Resolution Global Maps of 21st-Century Forest Cover Change
(GFC- Table 1) is produced by the Group on Land Analysis and Discovery
(GLAD) at the University of Maryland (Hansen et al., 2013). GFC
harmonizes all available spectral data from the Landsat mission to
derive descriptive annual and semi-annual statistics (median, maximum,
minimum, and quantiles) for each spectral band that constitutes data
inputs to produce annual forest loss estimates through the application
of a supervised decision tree classifier. The final GFC product is
constituted by two main data sets. Pixel values for the first one
represent the percentage canopy cover in the first year of analysis
(2000). The second layer, assigns a value representing the year of
forest cover loss to all pixels that were mapped with a tree cover
higher than 30% in the reference year (2000). This layer enables users
to define forest extent in terms of a minimum percent tree cover
threshold. This threshold can be used to produce a mask of forest cover
for the initial year with a tree cover above the user-specified
threshold. This mask can then be applied to the layer on forest loss
year to remove pixels labeled as loss that are under the defined tree
cover threshold (Hansen et al 2013).

# Comparison of GLAD and SMBYC Datasets

This table provides a comparison between the GLAD and SMBYC datasets
used for land cover analysis.

<table>
<colgroup>
<col style="width: 23%" />
<col style="width: 39%" />
<col style="width: 36%" />
</colgroup>
<thead>
<tr class="header">
<th>Criteria</th>
<th>SMBYC</th>
<th>GLAD</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Spatial Resolution</td>
<td>30m</td>
<td>30m</td>
</tr>
<tr class="even">
<td>Base Information</td>
<td>Landsat</td>
<td>Landsat</td>
</tr>
<tr class="odd">
<td>Coverage</td>
<td>National</td>
<td>Global</td>
</tr>
<tr class="even">
<td>Frequency</td>
<td>Irregular</td>
<td>Annual</td>
</tr>
<tr class="odd">
<td>Years</td>
<td>1990, 2000, 2005, 2010, 2012, 2013, 2014, 2015, 2016, 2017, 2018,
2019</td>
<td>2000-2020</td>
</tr>
<tr class="even">
<td>Strengths</td>
<td>- Distinguishes between planted and natural forests<br>- Official
information used to measure and report forest cover in Colombia</td>
<td>- Higher temporal resolution<br>- Less data loss<br>- Annual
updates<br>- Multiple canopy cover thresholds available</td>
</tr>
<tr class="odd">
<td>Weaknesses</td>
<td>- Maps produced at irregular intervals<br>- Multiple sources of data
loss (SLC-off and cloudiness)</td>
<td>- Does not distinguish between planted and natural forests<br>- Does
not report forest gain</td>
</tr>
</tbody>
</table>

# Methods

<figure>
<img src="images/workflow.png" alt="Armonization Workflow" />
<figcaption aria-hidden="true">Armonization Workflow</figcaption>
</figure>

Harmonization consisted of the production of forest cover maps from GFC
that best match the representation of forest cover and change by IDEAM
(Fig. 1) and that we refer here as HGFC. The first step was to exclude
from the class forest in GFC, pixels representing canopy forming
cultivations such as tree plantations, oil palm cultivations and
agroforestry systems that are explicitly excluded from the definition of
forest used for IDEAM. We eliminated those pixels based on areas
representing canopy forming cultivation in the national land cover maps
for 2012 and 2018 (IDEAM, 2010 - Table 1), which are the closest dates
to the period of analysis.

The second step was to produce a map representing three categories:
forest persistence, non forest persistence and forest loss between 2010
and 2017 using the forest non-forest maps produced by IDEAM for those
years. We selected 2010 as the first year because it is when the IDEAM
maps started to be produced at a higher temporal resolution. We selected
2017 as the last year because that was the latest year available for
IDEAM maps at the start of this research. We produced analogous maps
from GFC assuming different minimum thresholds in percent tree cover
that define forest and non-forest in GFC and used them to identify the
threshold that maximized agreement with the map produced from IDEAM.
Thresholds ranged between 20 and 100%. Thresholds increased by 10% for
values between 20% and 50%, 5% between 50% and 90%, and 1% between 90%
and 100%. Finer increments at higher percent tree cover values ensure
precision in these ranges, where most of the variation in percent tree
cover occurs in the GFC dataset. Forest gain was not considered in the
analysis because this data is not available for most years in the GFC
product.

To account for possible spatial variations in optimal percent tree cover
thresholds within the country, we performed the above described analysis
independently for each of the 396 geographic subunits representing areas
with homogeneous ecological conditions (Table 1). Data downloading and
thresholding was produced using the R package ‘ecochange’ (Lara et. al,
2022). For each subunit, we calculated overall agreement between the
change map calculated from IDEAM and each thresholded forest change map
calculated from GFC and selected the one that produced the maximum
overall agreement for further analysis. We used the optimum percent tree
cover thresholds identified for each subunit to derive harmonized maps
representing forest cover for the years 2012 to 2021 for the entire
country. These maps were used to evaluate class agreement between the
maps and to compare estimates of annual forest loss derived from HGFC
and IDEAM. Overall class agreement for the entire country was calculated
as the area weighted average of the agreements obtained in all
subnational units.

![Main Biome types and selected biomes](images/doble_paperV_diss.jpg)
![Agreement Metrics; overall and class
level](images/harmonization1.jpeg)

<figure>
<img src="images/fig5_paper.jpg"
alt="Optimal Canopy Threshold, Agreement level and Harmonized change map" />
<figcaption aria-hidden="true">Optimal Canopy Threshold, Agreement level
and Harmonized change map</figcaption>
</figure>

<figure>
<img src="images/image.png"
alt="Area weighted agreement assessment between change maps derived from IDEAM and Harmonized GFC maps for the categories non-forest, forest, and forest loss." />
<figcaption aria-hidden="true">Area weighted agreement assessment
between change maps derived from IDEAM and Harmonized GFC maps for the
categories non-forest, forest, and forest loss.</figcaption>
</figure>
