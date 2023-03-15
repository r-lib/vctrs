This is a minor release with no expected breakage of any reverse dependencies.

We fixed the CRAN check result failures, in particular:
- We removed `SystemRequirements: C++11`.
- We fixed the S3 method inconsistencies for `cnd_header()` and `cnd_body()`.

The following reverse dependencies showed up in our checks, but we believe they are false alarms:
- GenomeAdmixR: Can't reproduce locally.
- portalr: A common failing package that we see. Likely related to a failed download.
- openalexR: A common failing package that we see. Likely related to a failed download.
- rapbase: Likely related to a failed download from GitHub.
