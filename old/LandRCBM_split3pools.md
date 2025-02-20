---
title: "LandRCBM_split3pools Manual"
subtitle: "v.0.0.0.9000"
date: "Last updated: 2022-11-17"
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
citation-style: citations/ecology-letters.csl
link-citations: true
always_allow_html: true
---

# LandRCBM_split3pools Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:LandRCBM-split3pools) *LandRCBM_split3pools*



[![made-with-Markdown](figures/markdownBadge.png)](http://commonmark.org)

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

Table \@ref(tab:moduleInputs-LandRCBM-split3pools) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-LandRCBM-split3pools)List of (ref:LandRCBM-split3pools) input objects and their description.</caption>
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
   <td style="text-align:left;"> table6 </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Proportion model parameters similar to Boudewyn et al 2007,but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc </td>
  </tr>
  <tr>
   <td style="text-align:left;"> table7 </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Caps on proportion models similar to Boudewyn et al. 2007 but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cbmAdmin </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Provides equivalent between provincial boundaries, CBM-id for provincial boundaries and CBM-spatial unit ids </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ecozone </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Ecozones of Canada </td>
   <td style="text-align:left;"> http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip </td>
  </tr>
  <tr>
   <td style="text-align:left;"> canfi_species </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> File containing the possible species in the Boudewyn table </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CBM_AGB </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1xxfG-8ZJKPlO5HguHpVtqio5TXdcRGy7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CBM_speciesCodes </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1sVsDoT1E-CDgo2hnCU2pgqV6PpVic2Fe </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> PixelGroup map from LandR </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1Pso52N9DFVJ46OFxtvtqrVX9VzOVhcf3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> template raster to use for simulations; defaults to RIA study area </td>
   <td style="text-align:left;"> https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci </td>
  </tr>
</tbody>
</table>

Provide a summary of user-visible parameters (Table \@ref(tab:moduleParams-LandRCBM-split3pools))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-LandRCBM-split3pools)List of (ref:LandRCBM-split3pools) parameters and their description.</caption>
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
   <td style="text-align:left;"> start(sim) </td>
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
<caption>(\#tab:moduleOutputs-LandRCBM-split3pools)List of (ref:LandRCBM-split3pools) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> CBM_yieldOut </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> AGB values by pixel/pixelGroup, cohort (species and age) will be provided by the Yield module </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CBM_AGBplots </td>
   <td style="text-align:left;"> plot </td>
   <td style="text-align:left;"> Plot of the AGB values per cohort provided by the Yield module </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cumPools </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> Cumulative carbon in three pools, totMerch, fol, and other per cohort </td>
  </tr>
  <tr>
   <td style="text-align:left;"> growth_incForSpinup </td>
   <td style="text-align:left;"> matrix </td>
   <td style="text-align:left;"> Matrix of the 1/2 increment for every age provided per cohort </td>
  </tr>
</tbody>
</table>

### Links to other modules

Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

-   provide a way for people to obtain help (e.g., module repository issues page)
