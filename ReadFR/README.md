# ReadFR - SNP Genotype Quality Control Pipeline

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Fortran](https://img.shields.io/badge/Fortran-90%2F95-blue.svg)](https://fortran-lang.org/)
[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)](https://cmake.org/)

A high-performance SNP genotype quality control and conversion pipeline for genomic breeding value prediction (GBLUP/ssGBLUP). Processes Illumina FinalReport files with automatic format detection, comprehensive QC filtering, and optimized performance.

**Developed by**: Dr. Deukmin Lee (Hankyong National University)  
**Email**: dhlee@hknu.ac.kr

---

## ✨ Key Features

- **High Performance**: Processes 595 animals × 61,565 SNPs in **6 minutes 52 seconds**
- **Auto-Detection**: Automatically detects MAP file version (V2/K) and field formats (ANIMAL_ID/ANIMAL_ARN)
- **Dynamic Field Mapping**: Supports both PorcineSNP60 (V2) and GGP-Porcine (K) chip formats
- **Comprehensive QC**: Filters by GC Score, R Intensity, GT Score, Cluster Separation, and Call Rate
- **Hash Table Optimization**: O(1) pedigree lookup with 465K+ records
- **Sex Chromosome Validation**: Automatic X/Y chromosome heterozygosity checks
- **Real-time Progress**: Per-animal statistics with live console output
- **File Integrity**: SNP order verification between FR and MAP files

---

## 📊 Performance Benchmarks

| Dataset | Animals | SNPs | Processing Time |
|---------|---------|------|-----------------|
| **Large-scale** | 595 | 61,565 | **6m 52s** |
| **Medium-scale** | 24 | 76,756 | **23s** |

**System**: Intel/AMD x86_64, GNU Fortran 11.4.0, Release build (`-O3 -march=native`)

**Optimizations Applied**:
- Field index caching (256M searches → 7)
- Hash table (O(n²) → O(1) PED lookup)
- Direct SNP indexing
- Compiler optimizations

---

## 🚀 Quick Start

### Prerequisites

- **Fortran Compiler**: GNU Fortran (gfortran) 9.0+
- **Build System**: CMake 3.10+
- **OS**: Linux/Unix (tested on Ubuntu 20.04+)

### Installation

```bash
# Clone repository
git clone https://github.com/yourusername/GPBLUP.git
cd GPBLUP

# Build (Release mode for optimal performance)
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build

# Executable location
./build/bin/ReadFR
```

### Basic Usage

```bash
# Create parameter file (see below)
./build/bin/ReadFR parameter_file.txt
```

---

## 📁 Input Files

### 1. Parameter File

**Example**: `parameter_V2_595animals`

```
PEDFILE   = PED_Total.txt
HEADER    = 1
DELIM     = TAB

ANIMAL_ID     = 1
ANIMAL_ARN    = 2
SIRE          = 3
DAM           = 4
BREED         = 7
LOC           = 9

SNPFILE   = PorcineSNP60_SJ_51th_595sp_DY_FinalReport.txt
HEADER    = 9
DELIM     = TAB

ANIMAL_ARN    = 2
SNP_NAME      = 5
ALLELE1_AB    = 12
ALLELE2_AB    = 13
GC_SCORE      = 14
GT_SCORE      = 16
CLUSTER_SEP   = 17

MAPFILE   = MAP_V2.txt
HEADER    = 1
DELIM     = TAB

SNP_NAME      = 1
CHR           = 2
POSITION      = 3

OUTPUT_PREFIX = test_V2_595animals

MIN_GC_SCORE      = 0.65
MIN_R_INTENSITY   = 0.4
MAX_R_INTENSITY   = 2.0
MIN_GT_SCORE      = 0.50
MIN_CLUSTER_SEP   = 0.30
MIN_CALL_RATE     = 0.70
```

**Key Parameters**:
- **ANIMAL_ID** vs **ANIMAL_ARN**: Program auto-detects which field to use as hash key
- **DELIM**: `TAB` or `SPACE`
- **QC Thresholds**: Empirically optimized for porcine genotyping

### 2. PED File

Tab/space-delimited pedigree file with animal ID, sire, dam, breed, and location.

**Example**:
```
ANIMAL_ID   ANIMAL_ARN      SIRE    DAM     SEX  BDATE    BREED  LOC
LL16001048  1234567890      S001    D001    1    20160101 LL     Farm1
```

### 3. MAP File

SNP positions with chromosome and base-pair location.

**Supported Formats**:
- **V2 (PorcineSNP60)**: SNP names starting with `ALGA`, `ASGA`, `DIAS`, `MARC`
- **K (GGP-Porcine)**: SNP names in format `CHR_POSITION` (e.g., `1_10673082`)

**Example**:
```
SNP_NAME        CHR  POSITION
ALGA0000009     1    7188013
ALGA0000014     1    18480497
```

### 4. FinalReport File

Illumina GenomeStudio output with sample genotypes and quality scores.

**Required Columns** (configurable via parameter file):
- Sample identifier (ANIMAL_ID or ANIMAL_ARN)
- SNP_NAME
- ALLELE1_AB, ALLELE2_AB
- GC_SCORE (GenCall Score)
- GT_SCORE (GenTrain Score)
- CLUSTER_SEP

---

## 📤 Output Files

### GENO File

Space-delimited genotype matrix with pedigree information.

**Format**:
```
Animal_ID BREED SIRE DAM SEX BDate LOC GENO
LL16001048 LL S001 D001 1 20160101 Farm1 0120112...
```

**Genotype Encoding**:
- `0` = AA (homozygous A)
- `1` = AB (heterozygous)
- `2` = BB (homozygous B)
- `9` = Missing/Invalid

**Filename**: `{OUTPUT_PREFIX}_YYYYMMDD_NN.geno` (auto-incremented)

### Console Output

Real-time per-animal statistics:

```
MAP version detected: V2 (Porcine60K) (First SNP: ALGA0000009)
Field indices calculated - optimization enabled
SNP order verified: FR and MAP files match

Animal[   1] LL16001048 (LL) - Total SNPs:  61565 Valid:  45292 Invalid:  16273 CallRate:  0.7357 RETAINED
Animal[   2] LL16011035D (LL) - Total SNPs:  61565 Valid:  45147 Invalid:  16418 CallRate:  0.7333 RETAINED
...
Animal[ 595] YY17055015 (YY) - Total SNPs:  61565 Valid:  45809 Invalid:  15756 CallRate:  0.7441 RETAINED

========================================
Total animals processed:          595
Animals retained (Call Rate >= 0.7):          508
Animals excluded (Low Call Rate):           87
========================================
Total Valid SNPs:     22970456
Total Invalid SNPs:     10624236
========================================
```

---

## 🔬 Quality Control Filters

### SNP-Level Filters

| Filter | Default | Description |
|--------|---------|-------------|
| **GC_SCORE** | ≥ 0.65 | GenCall score (genotype confidence) |
| **R_INTENSITY** | 0.4 - 2.0 | Total signal intensity range |
| **GT_SCORE** | ≥ 0.50 | GenTrain score (cluster quality) |
| **CLUSTER_SEP** | ≥ 0.30 | Cluster separation score |

### Animal-Level Filter

| Filter | Default | Description |
|--------|---------|-------------|
| **CALL_RATE** | ≥ 0.70 | Proportion of valid genotype calls |

### Sex Chromosome Validation

- **Chr X (20)**: All genotypes allowed
- **Chr Y (21)**: Heterozygous calls (AB) automatically set to missing (9)
- Counts homozygous/heterozygous calls for sex verification

---

## 🛠️ Advanced Features

### Automatic Format Detection

**MAP Version Detection**:
```
V2 (PorcineSNP60): First SNP matches ALGA*/ASGA*/DIAS*/MARC*
K (GGP-Porcine):   First SNP matches CHR_POSITION format
```

**Field Mapping**:
- Automatically selects `ANIMAL_ARN` or `ANIMAL_ID` based on parameter file
- Hash table uses detected field as key for O(1) pedigree lookup

### SNP Order Verification

Program validates that the first SNP in FinalReport matches the first SNP in MAP file. Stops with error if mismatch detected:

```
========================================
WARNING: SNP order mismatch detected!
  FR file first SNP: ALGA0000009
  MAP file first SNP: 1_10673082
  Please ensure correct MAP file is specified:
    - V2 FR files require MAP_V2.txt
    - K FR files require MAP_K.txt
========================================
```

### Hash Table Optimization

- **Capacity**: Auto-sized to 130% of PED records
- **Collision Handling**: Separate chaining
- **Lookup Time**: O(1) average case
- **Key Selection**: Dynamic based on ANIMAL_ID/ANIMAL_ARN parameter

---

## 📖 Module Architecture

```
ReadFR.f90 (Main Program)
├── M_Kinds.f90           - Precision definitions (r4, ki1, ki4)
├── M_Stamp.f90           - Version & timestamp utilities
├── M_Variables.f90       - Data structures (FileInfo, SNPInfo, PEDInfo)
├── M_StrEdit.f90         - String manipulation (to_upper, trim)
├── M_ReadFile.f90        - File I/O (readline, fopen)
├── M_readpar.f90         - Parameter file parser
├── M_PEDHashTable.f90    - Hash table implementation
├── M_HashTable.f90       - Generic hash functions
└── Qsort4.f90            - Sorting utilities
```

---

## 🐛 Troubleshooting

### Common Issues

**1. "Cannot open module file 'm_kinds.mod'"**
```bash
# Solution: Clean rebuild
rm -rf build
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

**2. "SNP order mismatch detected"**
- Ensure MAP file version matches FinalReport chip type
- V2 FR → use MAP_V2.txt
- K FR → use MAP_K.txt

**3. "0 animals processed"**
- Check ANIMAL_ID vs ANIMAL_ARN field mapping in parameter file
- Verify field numbers match actual file columns
- Ensure PED and FR files use matching animal identifiers

**4. Slow performance**
```bash
# Rebuild with Release mode (not Debug)
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

---

## 📝 Citation

If you use this software in your research, please cite:

```
Lee, D. (2026). ReadFR: High-performance SNP genotype quality control pipeline 
for genomic prediction. Hankyong National University.
https://github.com/yourusername/GPBLUP
```

---

## 📄 License

This project is licensed under the MIT License - see below for details:

```
MIT License

Copyright (c) 2026 Dr. Deukmin Lee

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

---

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

---

## 📧 Contact

**Dr. Deukmin Lee**  
Department of Animal Science  
Hankyong National University  
Anseong, South Korea  

- **Email**: dhlee@hknu.ac.kr
- **GitHub**: [github.com/yourusername](https://github.com/yourusername)

---

## 🙏 Acknowledgments

- Illumina GenomeStudio for FinalReport format specification
- GNU Fortran development team
- CMake project for build system
- Korean swine breeding industry collaborators

---

## 📚 Related Documentation

- [MAP File Structure Guide](MAP_File_Structure.md)
- [PED Hash Table Implementation](PED_HASH_TABLE_GUIDE.md)
- [SNP QC Guidelines](SNP_QC_GUIDE.md)
- [Pipeline Integration Guide](PIPELINE_GUIDE.md)

---

**Last Updated**: February 15, 2026  
**Version**: 1.0 Release
