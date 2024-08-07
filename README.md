
# STRIDER (STate, effoRt, Identification/DEtection, Reporting)

[![R-CMD-check](https://github.com/BiologicalRecordsCentre/STRIDER/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BiologicalRecordsCentre/STRIDER/actions/workflows/R-CMD-check.yaml)

## What is STRIDER?

STRIDER is an R package for facilitating the simulation of virtual
species and the subsequent sampling and reporting. Its development is
motivated by the need to simulate citizen science data, alongside other
biodiversity data sampling, to validate method development. The language
is intentionally generic so uses terms such as ‘target’ to mean species,
organisms or similar. The simulation is split into a series processes:

- State: what is the state of the simulated reality?
  - Environment (e.g abiotic and biotic)
  - Target (e.g. species) suitability as a function of the environment
  - Target realised state as a function of the target suitability
- Effort: how is sampling effort allocated? (where/what/when are they
  sampling? Who/what is doing the sampling?)
- Identification/Detection: what happens when a sampler encounters the
  target (is the species detected? is the species correctly identified?)
- Reporting: how is the interaction reported? (is the species recorded?
  Are absences recorded? At what spatial resolution is it reported at?)

![](man/figures/overview.drawio.svg)

## Overview

STRIDER provides a reproducible basis for all your simulation needs.
There are 3 key components.

Firstly, it provides a `SimulationObject` class with slots for each
object describing each stage of the simulation workflow. This keeps
everything in one R object that can be

Secondly, STRIDER provides a series of functions to build up your
simulation object. This includes both straightforward functions for
common simulation approaches and the ability to bring your own
data/functions.

Thirdly, STRIDER will provide (in due course) example workflows that can
be adapted and extended your to kick start your simulation work.

## Installation

Install from GitHub

    remotes::install_github("BiologicalRecordsCentre/STRIDER")

## Requirements

`terra`, `sf`

`targets` is recommended for the pipeline/workflow management

## How to use the R package

For each of the 5 processes, there are choices of functions to use
depending on your needs. For each process, there is the most basic
version for demonstration purposes.

The functions all follow this basic schema whereby all the objects from
the previous stage, along with the `background` object, are combined
into a single `simulation_object`. This object is then used as an
argument in the subsequent functions, whether or not they are actually
used in the calculations within the function.

The `simulation_object` includes the following components:

- `@background`: Background extent and resolution of the simulated
  reality
- `@state_env`: Simulated state of the environment
- `@state_target_suitability`: Simulated state of the target (as
  environmental suitability)
- `@state_target_realised`: Simulated state of the target (as a realised
  absolute/binary value)
- `@effort`: Simulated sampling effort
- `@detect`: Simulated detection information

You can access and manipulate the `simulation_object` at each step to
generate the outputs of the corresponding processes. The outputs at each
step are `terra` SpatRasters or `sf` feature collections (POINT), as
shown in the figure above. You can use custom R scripts to generate the
outputs of any of the steps, ensuring flexibility and interoperability.

The functions used at each stage are as follows:

- `sim_state_env(simulation_object, ...)`
- `sim_state_target_suitability(simulation_object, ...)`
- `sim_state_target_realise(simulation_object, ...)`
- `sim_effort(simulation_object, ...)`
- `sim_detect(simulation_object, ...)`

You could use the `targets` R package to create reproducible workflows
for simulating your data.

## Simulating state

The term “state” refers to the underlying true conditions of the
environment and the target.

Environmental State: This refers to the simulated state of the
environment, which can include various abiotic and biotic factors, such
as temperature, humidity, terrain features, vegetation, or any other
relevant ecological parameters. Simulating the environmental state
facilitates understanding of how different environmental conditions may
influence the presence or behaviour of the target species.

Target State: This represents the simulated distribution of the target
within the environment. The target is often influenced by the
environmental state. In STRIDER the target is represented in two forms:
a continuous variable representing a probability of occurrence (slot
`@state_target_suitability`), and a realised absolute value (slot
`@state_target_realised`) which could contain a binary (0 or 1)
representing species occupancy or a positive integer representing
abundance.

In current form STRIDER represents states as being grid-based and relies
on a consistent grid resolution/extent across all state variables.
States are represented as rasters (`SpatRaster`) with any resolution,
extent or CRS (or no CRS). We simulate the state of the environment and
target separately. The simulation object uses a SpatRaster in slot
`@background`. The resolution, extent and CRS is inherited from the
background when simulating states.

In future developments we will consider using point process models
describe the target state with slot `@state_target` capturing the
intensity surface and `@state_target_realised` capturing the realised
points.

### Defining the background

The background is simply a SpatRaster from which the
CRS/extent/resolution will be used for subsequent simulation steps. Here
we create a background using a 30x30 matrix then create a simulation
object from it.

``` r
# Create the background
background <- terra::rast(matrix(0,30,30))

# Create the simulation object
sim_obj <- SimulationObject(background = background)
sim_obj <- sim_state_env(sim_obj,fun = state_env_gradient,from = 0,to = 1)
sim_obj
```

    ## An object of class "SimulationObject"
    ## Slot "background":
    ## class       : SpatRaster 
    ## dimensions  : 30, 30, 1  (nrow, ncol, nlyr)
    ## resolution  : 1, 1  (x, y)
    ## extent      : 0, 30, 0, 30  (xmin, xmax, ymin, ymax)
    ## coord. ref. :  
    ## source(s)   : memory
    ## name        : lyr.1 
    ## min value   :     0 
    ## max value   :     0 
    ## 
    ## Slot "state_env":
    ## class       : SpatRaster 
    ## dimensions  : 30, 30, 1  (nrow, ncol, nlyr)
    ## resolution  : 1, 1  (x, y)
    ## extent      : 0, 30, 0, 30  (xmin, xmax, ymin, ymax)
    ## coord. ref. :  
    ## source(s)   : memory
    ## name        : env 
    ## min value   :   0 
    ## max value   :   1 
    ## 
    ## Slot "state_target_suitability":
    ## NULL
    ## 
    ## Slot "state_target_realised":
    ## NULL
    ## 
    ## Slot "effort":
    ## NULL
    ## 
    ## Slot "detect":
    ## NULL
    ## 
    ## Slot "report":
    ## NULL
    ## 
    ## Slot "metadata":
    ## $state_env
    ## $state_env$fun
    ## state_env_gradient
    ## 
    ## $state_env$from
    ## [1] 0
    ## 
    ## $state_env$to
    ## [1] 1
    ## 
    ## 
    ## 
    ## Slot "hash":
    ## [1] "841156773684e37801aefddb92881877"

### Simulating the environmental state

Here we want to represent the state of the environment. Essentially we
need to capture variables (real or abstract) which influence where the
target might exist, and where the effort might be allocated. This might
include:

- Physical features like altitude or slope
- Climatic variable such as rainfall or temperature
- Effort-impacting variables such as human population, focal points
  (e.g. nature reserves) or access features (footpaths).

The output of this stage is a SpatRaster with layers for each
environmental variable. There is currently no way to capture non-gridded
spatial features such as lines or polygons but you could derive
grid-based approximations of it. This works well for time invariant
environmental variables. If you need time variant environmental
variables then the best current implementation is to create a simulation
object for each time step, sample from each time step, sample from each
time step, then aggregate later.

The function for simulating environmental state is `sim_state_env`

The minimal version of this function is
`sim_state_env(fun=state_env_uniform)` which produces a simulation
object with a single layer which is uniform in value in space.

The BYOD (Bring Your Own Data) function is
`sim_state_env(spatraster = [your_raster])` where you can provide a
SpatRaster with custom environmental state that meets you needs and it
will be added to the correct slot.

### Simulating the target state

Here we define the state of the target or targets. We define two
versions of this: a continuous variable representing a probability of
occurrence (slot `@state_target`), and a realised absolute value (slot
`@state_target_realised`) which could contain a binary (0 or 1)
representing species occupancy or a positive integer representing
abundance. Here’s an example:

![](man/figures/realisation_diagram-1.png)<!-- -->![](man/figures/realisation_diagram-2.png)<!-- -->![](man/figures/realisation_diagram-3.png)<!-- -->

Both of these representations of state are represented in the simulation
object as SpatRaster with layers for each target. Again, if you want the
target to change over time then create a list of rasters where each list
item represents the target state at each time step, but this will need
some wrangling.

The function for simulating target state is
`sim_state_target_suitability`

The minimal version of this function is
`sim_state_target_suitability(fun=state_target_suitability_uniform)`
which produces a uniform abundance across space.

The BYOD function is `sim_state_target(fun=[your custom function])`
meaning you could also use other packages to generate a target state
(eg.rangeshiftR, virtualspecies) then convert the output to a
`SpatRaster`.

## Simulating effort

In the context of STRIDER, “effort” refers to the representation of
sampling activities, encompassing the allocation and execution of
various sampling procedures across a defined landscape or study area.

Several factors could potentially predict the allocation and execution
of sampling efforts, including but not limited to:

- Accessibility: Proximity to human infrastructure, trails, or road
  networks might influence where sampling efforts are concentrated.
- Environmental conditions: Factors such as topography, habitat type,
  and climate could affect the selection of sampling sites.
- Species-specific considerations: The presence of specific target
  species or their preferred habitats could influence where sampling
  efforts are directed.

Functionally, effort in STRIDER currently describes:

- Who (or what) is did the sampling?
- Where did the samplers visit to do sampling?
- When did the sampling happen? (this is captured implicitly)

There are many ways to simulate effort from sampling points based on
cell weights, to agent based models, to manually setting specific
locations for sampling therefore STRIDER only facilitates representing
effort as the realisation of sampling procedures. If you want to capture
information about probabilities of locations being sampled then either
have a non-slotted SpatRaster, or capture this information in the
environmental state.

The slot for effort is `@effort` and contains a sf spatial data frame
with the following columns:

- `sampler` - who (or what)
- `visit` - the identifier for each visit by a sampler
- `unit` - the identifier for each sampling unit within a visit
- `cell_id` - where
- `geometry` - where

Here’s a very basic example where we generate effort comprising of two
samplers, each making one visit, and within each visit they use two
sampling units (meaning they had two opportunities to observe each
target present).

``` r
# Simulate the sampling effort
sim_obj <- sim_effort(sim_obj,fun = effort_basic, n_samplers=2, n_visits = 1, n_sample_units = 2)
sim_obj@effort
```

    ## Simple feature collection with 4 features and 4 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 7.5 ymin: 6.5 xmax: 15.5 ymax: 15.5
    ## CRS:           NA
    ##   sampler visit unit cell_id         geometry
    ## 1       1     1    1     706 POINT (15.5 6.5)
    ## 2       1     1    2     706 POINT (15.5 6.5)
    ## 3       2     1    1     428 POINT (7.5 15.5)
    ## 4       2     1    2     428 POINT (7.5 15.5)

The function for simulating effort start is `sim_effort`

The minimal function for this process is `sim_effort(fun="uniform")` in
which effort is uniformly distributed across the landscape.

## Simulating identification/detection

In STRIDER, “detection” refers to the process of whether the applied
effort identifies and records the presence of a target within the
specified visit. It describes the interaction between realised target
state and effort, but may also be influenced by environmental state.
Detection can be influenced by various factors such as the sampling
methodology, the proficiency of the observer, the environmental
conditions, and the characteristics of the target species.

Given detection, “identification” refers to the accurate recognition and
categorization of a species during the data collection process. This
step involves correctly identifying the observed organism to the
appropriate taxonomic group or species. Samplers may not always identify
a target correctly and these functions may take confusion matrices.

The minimal function for this process is `sim_detect(fun=detect_equal)`
in which all targets are detected at equal probability.

## Simulating the reporting

The reporting phase in STRIDER is designed to simulate various reporting
scenarios, considering factors such as reporting probabilities, data
resolution, and recording platforms, among others.

Data may not be reported exactly as the sampler experienced it, for
example:

- Aggregating the detections across multiple sampling units into the
  species observed across in one visit.
- Aggregating to a coarser spatial resolution
- Only interesting or novel species are reported (eg. as a result of
  life listing)

The minimal function for this process is `sim_report(fun=report_equal)`
in which all data is reported at equal probability.

## Custom functions

For each simulation stage you can provide your own function to simulate
that process. If there are different parameters that you want to provide
to these functions you can pass them as other named arguments to the
function. For example if I want to use a custom function to produce a
environmental suitability layer for a target, based on the environment I
could do define the following function `suit_fun()`:

``` r
suit_fun <- function(sim_obj){
  target_suitability <- sim_obj@background # use the background to 
  terra::values(target_suitability) <- 0.2
  names(target_suitability) <- "frog" #give my layer a name

  # set suitability under certain critera eg. 0.7 when rainfall>500 and altitude < 50
  target_suitability[[sim_obj@state_env$rainfall>500 & sim_obj@state_env$altitude<50]] <- 0.4
  target_suitability[sim_obj@state_env$rainfall>800 & sim_obj@state_env$altitude<40] <- 0.9

  target_suitability #return just the suitability layer
}

sim_obj <- sim_state_target_suitability(sim_obj, fun = suit_fun)
```

This function must take the SimulationObject as its first argument. This
means you’ve got access to all other simulation components. Therefore,
if for example you wanted to your detection process to depend on the
environment then you’d simply need to access it via the correct slot.
