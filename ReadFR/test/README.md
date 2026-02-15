# ReadFR Test Data

This directory contains sample data files for testing the ReadFR SNP quality control pipeline.

## ⚠️ Important Notice

**Sample Data Only**: All files in this directory are small sample datasets created for testing and demonstration purposes. They are NOT production-ready datasets.

**File Naming**: Original file names have been anonymized to avoid copyright issues. These samples were derived from publicly available genomics data formats.

---

## 📁 Sample Files

### PED (Pedigree) File
- **PED_Total.txt** (2.9KB)
  - Sample pedigree with animal ID, sire, dam, breed, location
  - ~40 animals for testing

### MAP Files (SNP Positions)

**K Chip Format (GGP-Porcine)**:
- **Sample_MAP_K.txt** (3.2MB) - Full MAP file (76,756 SNPs)
- **Sample_MAP_K.txt** (40KB) - Reduced sample (1,000 SNPs)
- **Sample_MAP_K_Tiny.txt** (4KB) - Minimal sample (100 SNPs)

**V2 Chip Format (PorcineSNP60)**:
- **Sample_MAP_V2.txt** (2.4MB) - Full MAP file (61,565 SNPs)

### FinalReport Files (Genotype Data)

**K Chip Format**:
- **Sample_K_FinalReport_Tiny.txt** (5KB) - 100 lines for minimal testing

**V2 Chip Format**:
- **Sample_V2_FinalReport.txt** (170KB) - ~200 animals × ~100 SNPs
- **Sample_V2_FinalReport_Tiny.txt** (18KB) - Minimal dataset for quick tests

---

## 🧪 Quick Test

### Minimal Test (< 1 second)
```bash
# Using tiny datasets
./build/bin/ReadFR parameter_tiny
```

### Standard Test (~23 seconds)
```bash
# Using K chip data (24 animals)
./build/bin/ReadFR parameter_sample
```

---

## 📋 Parameter File Examples

Sample parameter files are provided in this directory:
- `parameter_sample` - For Sample_V2_FinalReport.txt
- `parameter_tiny` - For Sample_V2_FinalReport_Tiny.txt

---

## ⚙️ Creating Custom Samples

To create samples from your own data:

```bash
# Extract header + first 1000 SNP records
head -1009 YourFinalReport.txt > Sample_FinalReport.txt

# Create tiny MAP file (100 SNPs)
head -101 YourMAP.txt > Sample_MAP_Tiny.txt
```

---

## 🔒 Data Privacy

**No Proprietary Data**: All sample files contain:
- Generic animal IDs (anonymized)
- Random genotype patterns
- No traceable breeding program information
- Randomized pedigree relationships

**Safe for Public Repository**: These files are safe to share publicly and comply with:
- Data privacy regulations
- Intellectual property guidelines
- Open-source licensing (MIT)

---

## 📊 Expected Output

Running ReadFR on these samples should produce:

```
Total animals processed: 24
Animals retained (Call Rate >= 0.7): 24
Animals excluded (Low Call Rate): 0
Total Valid SNPs: 1,445,220
Total Invalid SNPs: 396,924
```

*(Numbers vary depending on QC thresholds in parameter file)*

---

## 🛠️ Test Scripts

Automated test scripts provided:
- `test_readfr.sh` - Run all test scenarios
- `create_sample.sh` - Generate sample files from large datasets
- `create_tiny_sample.sh` - Generate minimal test files

---

## 📧 Questions?

For questions about test data or sample generation:
- **Email**: dhlee@hknu.ac.kr
- **Issues**: https://github.com/dmlee5091/GPBLUP/issues

---

**Last Updated**: February 15, 2026
