# Contributing to Social Growth Index (SGI)

Thank you for your interest in contributing to the Social Growth Index project! This document provides guidelines for contributing to the codebase.

## Ways to Contribute

- **Report bugs**: Open an issue describing the problem
- **Suggest enhancements**: Propose new features or improvements
- **Improve documentation**: Fix typos, clarify instructions, add examples
- **Add tests**: Create unit tests or validation scripts
- **Submit code**: Fix bugs or implement new features

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/The-Social-Growth-Index-...`
3. Create a branch: `git checkout -b feature/your-feature-name`
4. Make your changes
5. Test your changes (see Testing section below)
6. Commit with clear messages: `git commit -m "Add feature X"`
7. Push to your fork: `git push origin feature/your-feature-name`
8. Open a Pull Request

## Code Style Guidelines

### R Code Style

Follow the [Tidyverse Style Guide](https://style.tidyverse.org/):

- Use `snake_case` for function and variable names
- Use `<-` for assignment (not `=`)
- Indent with 2 spaces (not tabs)
- Limit line length to 80 characters
- Add comments for complex logic
- Use meaningful variable names

Example:

```r
# Good
compute_sgi_score <- function(gdp_per_capita, population_density) {
  weighted_score <- 0.6 * gdp_per_capita + 0.4 * population_density
  return(weighted_score)
}

# Bad
computeSGIScore<-function(x,y){return(0.6*x+0.4*y)}
```

### Documentation

- Add roxygen2-style comments for functions
- Include parameter descriptions and return values
- Provide usage examples where helpful
- Keep README.md up to date

Example:

```r
#' Compute SGI Score
#'
#' Calculate Social Growth Index score from standardized indicators
#'
#' @param gdp_per_capita Numeric vector of GDP per capita values
#' @param population_density Numeric vector of population density values
#' @return Numeric vector of SGI scores
#' @examples
#' compute_sgi_score(c(1.2, 0.8), c(0.9, 1.1))
compute_sgi_score <- function(gdp_per_capita, population_density) {
  # Implementation
}
```

## Testing

Before submitting a pull request:

1. **Test with example data**:
   ```bash
   Rscript scripts/generate_example_data.R
   Rscript run_sgi_analysis.R
   ```

2. **Check for errors**: Ensure all scripts run without errors

3. **Verify outputs**: Check that expected files are created in `results/`

4. **Test edge cases**: Try with missing data, different parameters, etc.

## Submitting Issues

When reporting bugs, please include:

- Description of the problem
- Steps to reproduce
- Expected vs actual behavior
- Your R version: `R.version.string`
- Package versions: `sessionInfo()`
- Error messages (if any)

Example:

```
**Bug**: GDP downscaling fails with missing NUTS3 codes

**Steps to reproduce**:
1. Load data with incomplete NUTS3 mapping
2. Run `scripts/02_frk_gdp_downscaling.R`

**Expected**: Script should handle missing NUTS3 codes gracefully
**Actual**: Error "undefined columns selected"

**Environment**:
- R version 4.2.0
- sf version 1.0-9
- Operating System: Ubuntu 20.04
```

## Pull Request Guidelines

Good pull requests:

- **Focus on one thing**: Don't mix unrelated changes
- **Include tests**: If adding functionality, add tests
- **Update documentation**: Reflect changes in README/docs
- **Follow style guide**: Match existing code style
- **Write clear commit messages**: Explain what and why

### Commit Message Format

```
[Type] Brief description (50 chars or less)

More detailed explanation if needed (wrap at 72 chars).
Explain what changed and why, not how.

Fixes #123
```

Types: `Fix`, `Feature`, `Docs`, `Test`, `Refactor`, `Style`

Example:

```
[Feature] Add sensitivity analysis for Copeland weights

Implements sensitivity analysis functionality to test how
SGI rankings change with different indicator weights.

Includes:
- New function compute_sensitivity_analysis()
- Visualization of weight impacts
- Documentation in METHODOLOGY.md

Closes #45
```

## Development Workflow

### Adding a New Feature

1. Check if an issue exists, if not, create one
2. Discuss approach in the issue
3. Fork and create a branch
4. Implement the feature
5. Add tests and documentation
6. Submit PR referencing the issue

### Fixing a Bug

1. Create an issue describing the bug
2. Fork and create a branch like `fix/issue-123`
3. Fix the bug
4. Add a test to prevent regression
5. Submit PR with "Fixes #123"

## Code Organization

### File Structure

```
scripts/
  XX_descriptive_name.R  # Use numbered prefixes for pipeline scripts
  helper_functions.R      # Utility functions used across scripts

docs/
  METHODOLOGY.md          # Technical documentation
  EXAMPLES.md             # Usage examples

tests/                    # (Future) Unit tests
  test_indicators.R
  test_copeland.R
```

### Adding a New Script

When adding a new analysis script:

1. Use appropriate numbering (e.g., `06_new_analysis.R`)
2. Include standard header:
   ```r
   #!/usr/bin/env Rscript
   ################################################################################
   # Script: 06_new_analysis.R
   # Purpose: Brief description
   # Author: Your Name
   # Date: YYYY-MM-DD
   ################################################################################
   ```
3. Follow the pattern: load → process → save → summarize
4. Add to `run_sgi_analysis.R` if part of main pipeline
5. Document in README.md

## Adding New Indicators

To add a new indicator to the SGI calculation:

1. **Compute the indicator** in `03_compute_indicators.R`:
   ```r
   mutate(
     new_indicator = calculate_new_metric(...),
     new_indicator_std = standardize_indicator(new_indicator)
   )
   ```

2. **Add to Copeland ranking** in `04_copeland_ranking.R`:
   ```r
   indicators = c(
     "gdp_per_capita_std", 
     "gdp_density_std", 
     "population_density_std",
     "new_indicator_std"  # Add here
   )
   ```

3. **Update documentation**:
   - Add description to `docs/METHODOLOGY.md`
   - Update README.md with indicator explanation
   - Document data source requirements

4. **Test thoroughly**: Ensure it works with real and example data

## Improving Performance

If optimizing code performance:

- Profile first: Use `Rprof()` to identify bottlenecks
- Document improvements: Note speed gains in PR
- Preserve correctness: Verify results don't change
- Consider trade-offs: Speed vs memory vs readability

## Documentation

### Where to Document What

- **README.md**: High-level overview, installation, quick start
- **QUICKSTART.md**: Step-by-step beginner guide
- **docs/METHODOLOGY.md**: Technical details, algorithms, math
- **Code comments**: Implementation details, tricky logic
- **Function docs**: Parameters, return values, examples

## Questions?

- Check existing issues and PRs
- Ask in a new issue
- Reach out to maintainers

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

## Code of Conduct

Be respectful and constructive in all interactions. This is an academic research project - we're all learning together.

---

Thank you for contributing to the Social Growth Index project!
