# DER_disturbance_analysis
Tool to analyse PV and DER response to System Disturbances


## Version Control

This project uses Git for version control.

For guides on how to start using Git see those by [Github](https://guides.github.com/activities/hello-world/) or [Atlassian](https://www.atlassian.com/git/tutorials/what-is-version-control).

### Branching

This project uses feature branches. New features should increment minor version number once version labels are implemented.
Major version numbers will be incremented when non-backwards compatible API changes are made.

An example requiring a major version increment would be changing input or output formats drastically to allow new features that would no longer work with older data/analyses.


### Pull requests

Branches being merged to master require Pull Requests with at least one approval.
When reviewing a pull request the following are things you can consider:
- Does the new code make sense?
- Is there enough documentation for you to understand what's going on?
- Does it match the style guide?
- Do tests have good coverage?
- Do you have any questions about how things are working?
- Do existing tests still pass?

Work is continuing on automating tests for PRs.


## Style Guide

We're planning to start using the [Tidyverse style guide](https://style.tidyverse.org/index.html) with the following exceptions:

1. Functions should continue to use explicit returns rather than the implicit ones in the guide.
e.g.
```
divide_by_two <- function(x) {
  return(x/2)
}
```
2. We'll use a 120 character maximum line length.
