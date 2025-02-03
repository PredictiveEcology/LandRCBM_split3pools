---
title: "LandRCBM_split3pools Manual"
subtitle: "v.0.0.0.9000"
date: "Last updated: 2025-02-03"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  bibliography: citations/references_LandRCBM_split3pools.bib
link-citations: true
always_allow_html: true
---

# LandRCBM_split3pools Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:LandRCBM-split3pools) *LandRCBM_split3pools*



[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Celine Boisvenue <cboivenue@gmail.com> [aut, cre], Alex M Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Provide a brief summary of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

### Module inputs and parameters

Describe input data required by the module and how to obtain it (e.g., directly from online sources or supplied by other modules)
If `sourceURL` is specified, `downloadData("LandRCBM_split3pools", "C:/Users/docaron/Documents/Repos")` may be sufficient.
Table \@ref(tab:moduleInputs-LandRCBM-split3pools) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-LandRCBM-split3pools)(\#tab:moduleInputs-LandRCBM-split3pools)List of (ref:LandRCBM-split3pools) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> canfi_species </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> File containing the possible species in the Boudewyn table </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cbmAdmin </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Provides equivalent between provincial boundaries, CBM-id for provincial boundaries and CBM-spatial unit ids </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CBM_AGB </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1ANziym1UWZyDHPoVdRR5WHwrNw6b9Ms7/view?usp=sharing </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CBM_speciesCodes </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1GunHO8hN54WeMVgCh-MgWvxRaguPYuMJ/view?usp=drive_link </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Above ground biomass of cohorts in pixel groups </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/17VSBMgnvJtcDYgeaLXZWUA36DbsnLDyF/view?usp=drive_link </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> PixelGroup map from LandR </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/18FuRnQHPgY9-K3jkhKKQFTpGGbs0PmOT/view?usp=drive_link </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> template raster to use for simulations; defaults to RIA study area </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/18FuRnQHPgY9-K3jkhKKQFTpGGbs0PmOT/view?usp=drive_link </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spuRaster </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Raster has spatial units for each pixel </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> table6 </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Proportion model parameters similar to Boudewyn et al 2007, but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc </td>
  </tr>
  <tr>
   <td style="text-align:left;"> table7 </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Caps on proportion models similar to Boudewyn et al. 2007 but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt </td>
  </tr>
</tbody>
</table>

Provide a summary of user-visible parameters (Table \@ref(tab:moduleParams-LandRCBM-split3pools))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-LandRCBM-split3pools)(\#tab:moduleParams-LandRCBM-split3pools)List of (ref:LandRCBM-split3pools) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> numCohortPlots </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> When plotting the yield curves, this is how many unique cohorts per pixelGroup plotted. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> numPixGroupPlots </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> When plotting the yield curves, this is how many unique pixel groups will be randomly selected and plotted. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> screen </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Used by Plots function, which can be optionally used here </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first plot event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time interval between plot events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first save event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .seed </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Named list of seeds to use for each event (names). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should caching of events or module be used? </td>
  </tr>
</tbody>
</table>

### Events

Describe what happens for each event type.

### Plotting

Write what is plotted.

### Saving

Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-LandRCBM-split3pools)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-LandRCBM-split3pools)(\#tab:moduleOutputs-LandRCBM-split3pools)List of (ref:LandRCBM-split3pools) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> allInfoCohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Above ground biomass (in tonnes of g/m2) of each cohort per pixelGroup provided by LandR with additionnal information to match with Boudewyn et al. equations. Gets updated each timestep </td>
  </tr>
  <tr>
   <td style="text-align:left;"> allInfoYieldTables </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Yield table provided by the biomass_yieldTables module with additionnal information to match with Boudewyn et al. equations. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortPools </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Cumulative biomass in each aboveground pool for each cohort per pixelGroup. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cumPools </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Cumulative biomass in each aboveground biomass pool for each yield curve (in tonnes of carbon/ha). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cumPoolsRaw </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Same as cumPools with additionnal lines for age 0 of each cohort. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> increments </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Increments for each yield curve (in tonnes of carbon/ha). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rawIncPlots </td>
   <td style="text-align:left;"> ggplot </td>
   <td style="text-align:left;"> Plot of the increments for yield curves of randomly selected pixelGroup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> yieldCurvePlots </td>
   <td style="text-align:left;"> ggplot </td>
   <td style="text-align:left;"> Plot of the yield curves of randomly selected pixelGroup provided by the biomass_yieldTables module </td>
  </tr>
  <tr>
   <td style="text-align:left;"> yieldCurvePlotsStacked </td>
   <td style="text-align:left;"> ggplot </td>
   <td style="text-align:left;"> Plot of the AGB values per cohort of randomly selected pixelGroup provided by the biomass_yieldTables module </td>
  </tr>
  <tr>
   <td style="text-align:left;"> yieldCurvePoolPlots </td>
   <td style="text-align:left;"> ggplot </td>
   <td style="text-align:left;"> Plot of the cumulative biomass of the three AGB pools for randomly selected pixelGroup. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

-   provide a way for people to obtain help (e.g., module repository issues page)
