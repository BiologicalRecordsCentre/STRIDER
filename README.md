
# STRIDER (STate, effoRt, Identification/DEtection, Reporting)

## Overview

STRIDER is an R package for simulating virtual species and the
subsequent sampling and reporting. It’s primary use is for simulating
citizen science data to validate method development. The simulation is
split into 4 processes:

- State: what is the true state of the simulated reality? (where are the
  species? what is the environment like?)
- Effort: how is sampling effort allocated? (where/what/when are they
  sampling? who is doing the sampling?)
- Identification/Detection: what happens when the sampler meets the
  species (is the species detected? is the species correctly
  identified?)
- Reporting: how is the interaction reported? (is the species recorded?
  Are absences recorded? At what spatial resolution is it reported at?)

## Requirements

`terra`, `sf`

## How to use the R package

For each of the 4 processes there are choices of functions to use
depending on your need. For each the processes there is the most basic
version for demonstration purposes.

## Simulating state

All functions for simulating state start with `sim_state_`

    sim_state_uniform() # a uniform true state

We also provide wrappers around other packages: \* rangeshiftr \*
virtualspecies

## Simulating effort

All functions for simulating effort start with `sim_effort_`

    sim_effort_uniform() #effort is uniformly distributed across the landscape

## Simulating identification/detection

    sim_detect_all() # all species are detected

## Simulating the reporting

    sim_report_all() # all data is reported

## Bring it all together

Use the pipe to chain it all together. This is using the most basic
version of each process:

    simulated_data <- sim_state_unform() |>
      sim_effort_uniform() |>
      sim_detect_all() |>
      sim_report_all()

    print(simulated_data)

## Demonstration of using simulated data to validate a model approach

Here we fit a very simple model using the simulated data

    mod1 <- lm(simulated_data, ...)
    plot(mod1)
