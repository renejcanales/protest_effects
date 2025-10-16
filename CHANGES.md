# Changes Made to Fix Data Processing Errors

## Date: 2025-10-16

## Summary
Fixed critical errors in the data processing scripts that were preventing the analysis from running correctly.

## Issues Fixed

### 1. Error in `select()` Function - `proc_base elsoc.R`

**Problem**: The script was using `dplyr::select(., col1, col2, ...)` inside `rowMeans()` and `rowSums()` functions, which caused "Error in select(...): unused argument" errors.

**Root Cause**: When using `dplyr::select()` inside row-wise operations like `rowMeans()` or `rowSums()`, it returns a data frame rather than properly selecting columns for row-wise calculations.

**Solution**: Replaced all instances of `dplyr::select(., ...)` with `cbind(...)` in the following locations:
- Line 178: `protesta_index` calculation
- Line 181: `protesta_index_completo` calculation
- Line 184: `protesta_intensidad_suma` calculation
- Line 185: `protesta_intensidad_suma_completa` calculation
- Line 295: `conf_instituciones` calculation
- Line 296: `eficacia_politica` calculation
- Line 321: `justicia_distributiva` calculation
- Line 322: `meritocracia` calculation

**Example**:
```r
# BEFORE (incorrect):
protesta_index = rowMeans(dplyr::select(., firma_peticion_freq, asist_marcha_freq, part_huelga_freq), na.rm = FALSE)

# AFTER (correct):
protesta_index = rowMeans(cbind(firma_peticion_freq, asist_marcha_freq, part_huelga_freq), na.rm = FALSE)
```

### 2. Non-Existent Variables in Final Selection - `proc_base elsoc.R`

**Problem**: The final `dplyr::select()` statement (lines 576-625) was trying to select variables that were never created in the processing steps:
- `participacion_amplia`
- `protesta_intensidad` (actual name: `protesta_intensidad_suma`)
- `protesta_intensidad_completa` (actual name: `protesta_intensidad_suma_completa`)
- `protesta_multiple`
- `participa_dummy`

**Solution**: 
- Removed non-existent variables
- Corrected variable names to match what was actually created
- Added `protesta_missing` flag that was created but not selected

### 3. Hard-Coded File Paths

**Problem**: Multiple files used hard-coded paths like `~/GitHub/protest_effects/...` which are not portable across different systems.

**Files Updated**:
- `processing/proc_base elsoc.R` (line 44)
- `processing/proc_data_elsoc.R` (line 41)
- `processing/01-descriptivos.qmd` (line 84)
- `processing/02-modelos.qmd` (line 87)
- `processing/proc_data_elsoc.qmd` (line 92)
- `processing/random_effects.qmd` (line 83)

**Solution**: Changed all paths to use the `here()` package function for better portability:
```r
# BEFORE:
load("~/GitHub/protest_effects/input/data/raw/elsoc_long_2016_2023.RData")

# AFTER:
load(here("input/data/raw/elsoc_long_2016_2023.RData"))
```

## Verification of Requirements

According to the problem statement analysis, the following requirements were verified:

### ✓ Protest Index Calculation
- **Requirement**: Calculate only averages for the protest index (not dichotomize)
- **Status**: CONFIRMED - The code uses `rowMeans()` to calculate averages of frequency variables (1-5 scale)
- **Location**: Lines 178, 181

### ✓ Dummy Variable for March Participation
- **Requirement**: Create dummy variable only for "asist_marcha"
- **Status**: CONFIRMED - `protesta_dummy = asist_marcha_bin` (line 189)
- **Note**: This creates a binary variable (0/1) specifically for march participation, separate from the index

### ✓ Missing Data Summary
- **Requirement**: Address missing data issues shown in the tibble
- **Status**: CONFIRMED - The missing data summary (line 512-516) correctly references the variables:
  - `protesta_index`
  - `educ_years`
  - `educ_parental_max`
  - `movilidad_years`
  - `edad`
  - `ideologia_std`

## Variables Created vs Selected

### Variables Created in Processing:
- `protesta_index` - Average of petition, march, and strike frequencies
- `protesta_index_completo` - Includes cacerolazos
- `protesta_intensidad_suma` - Sum of frequencies (intensity measure)
- `protesta_intensidad_suma_completa` - Sum including cacerolazos
- `protesta_dummy` - Binary variable for march participation only
- `protesta_missing` - Flag for missing data in index

### Variables Removed from Selection:
- `participacion_amplia` - Never created
- `protesta_intensidad` - Name mismatch (should be `protesta_intensidad_suma`)
- `protesta_intensidad_completa` - Name mismatch (should be `protesta_intensidad_suma_completa`)
- `protesta_multiple` - Never created
- `participa_dummy` - Never created

## Testing Notes

The script cannot be tested in this environment as R is not installed, but the following validations were performed:
1. Syntax check: All changes follow correct R syntax
2. Variable consistency: All selected variables are now created in earlier steps
3. Function usage: `cbind()` is the correct approach for row-wise operations
4. Path portability: `here()` package provides cross-platform compatibility

## Next Steps for User

1. Run `processing/proc_base elsoc.R` in your local R environment
2. Verify that the missing data summary produces the expected tibble
3. Check that `elsoc_final.RData` is created successfully in `input/data/proc/`
4. Run `processing/01-descriptivos.qmd` to verify downstream analysis works correctly
