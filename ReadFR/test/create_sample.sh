#!/bin/bash

echo "🔄 FinalReport 샘플 파일 생성 중..."

# 1. 샘플 파일 생성: 헤더 + 처음 1000줄 데이터
head -1010 PorcineSNP60_FinalReport.txt > PorcineSNP60_FinalReport_SAMPLE.txt

echo "✓ 샘플 파일 생성: PorcineSNP60_FinalReport_SAMPLE.txt"
echo "  크기: $(ls -lh PorcineSNP60_FinalReport_SAMPLE.txt | awk '{print $5}')"
echo "  줄수: $(wc -l < PorcineSNP60_FinalReport_SAMPLE.txt)"

# 2. 샘플 MAP 파일 생성: 처음 1000줄
head -1010 MAP_K.txt > MAP_K_SAMPLE.txt

echo "✓ 샘플 MAP 파일 생성: MAP_K_SAMPLE.txt"
echo "  크기: $(ls -lh MAP_K_SAMPLE.txt | awk '{print $5}')"
echo "  줄수: $(wc -l < MAP_K_SAMPLE.txt)"

# 3. 샘플 문서 생성
cat > README_SAMPLE.txt << 'DOC'
📌 GitHub 테스트 데이터 샘플 설명
════════════════════════════════════════════════

이 디렉토리의 샘플 파일들은 ReadFR 프로젝트 테스트용입니다:

📄 파일 설명:
────────────────────────────────────────────

1. PorcineSNP60_FinalReport_SAMPLE.txt (약 62KB)
   - Illumina SNP FinalReport 포맷 샘플
   - 헤더 + 처음 1000개 SNP 데이터
   - 원본: PorcineSNP60_FinalReport.txt (10MB)

2. MAP_K_SAMPLE.txt (약 16KB)
   - SNP MAP 파일 샘플
   - SNP ID, 염색체, 위치 정보 포함
   - 원본: MAP_K.txt (3.2MB)

3. PED_Total.txt
   - PLINK PED 포맷 계보 정보
   - 100마리 동물의 혈연정보

🧪 테스트 방법:
────────────────────────────────────────────

parameter 파일 생성:
  SNPFILE: PorcineSNP60_FinalReport_SAMPLE.txt
  MAPFILE: MAP_K_SAMPLE.txt
  PEDFILE: PED_Total.txt
  HEADER: 10
  DELIM: TAB
  OUTPUTPREFIX: test_output

실행:
  cd /home/dhlee/GPBLUP/ReadFR/check
  ReadFR parameter

결과:
  test_output_*.geno (genotype 파일)
  test_output_*.snpqc (SNP QC 결과)

📊 데이터 통계:
────────────────────────────────────────────
- SNP 개수: ~1,000개 (샘플)
- 개체 수: 100마리 (원본 PED)
- 형식: Illumina FinalReport
- 컬럼: Sample ID, SNP ID, R, Theta, X, Y, GC Score, ...

✅ 사용 가능 대소문자 조합:
────────────────────────────────────────────
- SNPFILE / snpfile / SnpFile
- MAPFILE / mapfile / MapFile  
- PEDFILE / pedfile / PedFile
- HEADER / header / HeAdEr
- DELIM / delim / DeliM

🔗 추가 정보:
────────────────────────────────────────────
- READFR_USER_MANUAL.md: 상세 사용 설명서
- SNP_QC_GUIDE.md: SNP QC 필터링 가이드
- PIPELINE_GUIDE.md: 전체 파이프라인 설명
DOC

echo "✓ README 문서 생성: README_SAMPLE.txt"

echo ""
echo "✅ 샘플 파일 생성 완료!"
