# Changelog

All notable changes to the ReadFR project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-02-15

### Added
- Initial release of ReadFR SNP quality control pipeline
- Automatic MAP file version detection (V2/K format)
- Dynamic field mapping (ANIMAL_ID/ANIMAL_ARN)
- Hash table optimization for O(1) pedigree lookup
- Field index caching for performance
- Real-time per-animal progress display
- SNP order verification between FR and MAP files
- Sex chromosome validation (X/Y heterozygosity checks)
- Comprehensive QC filtering (GC Score, R Intensity, GT Score, Cluster Sep, Call Rate)
- CMake build system with Release/Debug modes
- Detailed console statistics output

### Performance
- 595 animals × 61,565 SNPs: **6 minutes 52 seconds**
- 24 animals × 76,756 SNPs: **23 seconds**
- Field index optimization: 256M searches → 7 searches
- Release build optimization: 45% faster than Debug

### Documentation
- Complete README.md with usage examples
- MAP_File_Structure.md for format details
- PED_HASH_TABLE_GUIDE.md for implementation
- SNP_QC_GUIDE.md for quality control guidelines
- PIPELINE_GUIDE.md for integration workflow

### Technical Details
- Language: Fortran 90/95
- Compiler: GNU Fortran 11.4.0+
- Build: CMake 3.10+
- Optimization flags: `-O3 -march=native`

---

## [Unreleased]

### Planned Features
- OpenMP parallel processing for multi-threading
- Additional output formats (PLINK, VCF)
- Mendelian error checking
- Hardy-Weinberg equilibrium testing
- Interactive parameter file generator
- Progress bar for long-running jobs

---

## Version History

- **v1.0.0** (2026-02-15): Initial public release
  - Full production-ready implementation
  - Comprehensive QC pipeline
  - Optimized performance
  - Complete documentation

---

**Maintained by**: Dr. Deukmin Lee (dhlee@hknu.ac.kr)  
**Repository**: https://github.com/yourusername/GPBLUP
