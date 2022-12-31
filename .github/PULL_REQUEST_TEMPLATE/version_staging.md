<!-- Throughout this template, replace `x` and `y` with the relevant info -->
# Parsley `x.y.0` Staging PR
This is the PR encompassing all changes for version `x.y.0`. As this is substantial new functionality, it will be previewed incrementally starting with `x.y.0-M1`. When the API is completed and seems reasonable, `x.y.0-RC1` will be released to allow for testing of the release, full documentation, and ironing out of bugs. When the release is stable enough, it will be merged into `master` and `x.y.0` will be officially released.

## High-Level Changes
<!-- What is the _aim_ of this release? -->

## Low-Level Changes
<!-- Delete this if this is a minor change -->
### Major Changes
None.

<!-- Delete this if not relevant: includes future deprecation and enforced deprecation -->
### Deprecations

### Minor Changes
None.

### Patch Changes
None.

<!-- Optionally -->
<!--
### Internal Changes
None.
-->

## Release Checklist
#### `x.y.0-Mn`
* [ ] add step by step changes here

#### `x.y.0-RC1`
* [ ] all new functionality has partial documentation

#### `x.y.0`
* [ ] all methods have completed documentation
* [ ] new functionality is well tested
* [ ] `README.md` updated to reflect new version
* [ ] `build.sbt` updated to disable snapshots on this branch
* [ ] documentation checked to ensure no leakage of `private [parsley]` members or `parsley.internal`.

## Milestone Migration Guide
As each milestone release may choose to make binary incompatible changes, any necessary migration required to get from one milestone to the next will be tracked here.

<!-- Remove this when the first milestone is ready: if this is a MAJOR release then provide migration from latest-release, and "No changes required" for MINOR releases. -->
_Nothing to see here!_
<!--
| From | To | Necessary Changes |
|:----:|:--:|-------------------|
| `<previous-version>` | `x.y.0-M1` | <see above> |
-->
