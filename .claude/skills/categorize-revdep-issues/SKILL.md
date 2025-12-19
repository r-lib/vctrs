---
name: categorize-revdep-issues
description: Write revdep/issue.md to group packages by common error categories
---

# Categorize Reverse Dependency Issues

Write `revdep/issue.md` to group packages by common error categories based on the error output in `revdep/problems.md` and `revdep/failures.md`.

## What This Skill Does

This skill analyzes reverse dependency check results and writes out an issue tracking file to group packages by their error types, making it easier to:
- Identify common problems across packages
- Understand which issues are dplyr-related vs external dependencies
- Prioritize fixes based on error patterns
- Provide clear guidance to package maintainers

## Files Involved

**Input files:**
- `revdep/README.md` - The `New problems` section lists the packages to group
- `revdep/problems.md` - Detailed error output for packages with test/check issues
- `revdep/failures.md` - Installation failures and error messages

**Output file:**
- `revdep/issue.md` - File to write packages grouped by error category to, may need to be created

## Process

### Step 1: Read All Input Files

Read all input files in parallel to understand the complete error landscape.

### Step 2: Analyze and Categorize Errors

Group packages by their primary error cause. Common categories include:

**Issues related to changes in our package:**
- Missing exports
- Defunct functions
- Defunct function arguments
- Missing Rd cross-references to removed functions

**Issues unrelated to changes we have made:**
- Missing exports from other packages (e.g., `purrr::at_depth`)
- Dependency issues
- Compilation failures (C++, Rust)
- Other packages that passed or have unclear status

### Step 3: Create Category Structure

For each category:
1. Create an H2 header with a descriptive category name
2. Write a brief description explaining:
   - What the issue is
   - Why it's happening
   - Suggested solution or migration path
3. List all affected packages in checkbox format

### Step 4: Write the New File

Format requirements:

```markdown
## Category Name

Brief description of the issue and solution approach.

* [ ] [package1](url1)
* [ ] [package2](url2)
...
```

If you must include package specific comments, do so like:

```markdown
## Category Name

Brief description of the issue and solution approach.

* [ ] [package1](url1)
  * Comments about package1.
* [ ] [package2](url2)
...
```

Do not include package specific comments in the `Brief description` section.

**Critical constraints:**
- MUST preserve ALL packages from the `README.md`'s `New problems` section
- Each package appears in exactly ONE category
- Maintain checkbox format: `* [ ] [package](url)`
- Order categories by relevance (common issues first)
- Use "Other issues requiring investigation" for unclear cases

## Quality Checks

After completing the rewrite, verify:
- [ ] Total package count in new file matches the `README.md`'s `New problems` section
- [ ] Each package from original appears exactly once in new file
- [ ] Each category has a clear H2 header and description
- [ ] Packages are in checkbox format with correct URLs
- [ ] Categories are logically ordered
- [ ] Common patterns are grouped together

## Example Category

```markdown
## Missing export: `dplyr::id`

Packages using `dplyr::id` which is not exported. These packages get "no visible binding for global variable 'id'" errors.

* [ ] [packageA](https://github.com/userA/packageA)
* [ ] [packageB](https://github.com/userB/packageB)
```
