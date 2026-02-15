#!/bin/bash

# 1. 매우 작은 샘플: 헤더 + 100줄만
head -110 PorcineSNP60_FinalReport.txt | dos2unix > PorcineSNP60_FinalReport_TINY.txt 2>/dev/null || \
sed 's/\r$//' < <(head -110 PorcineSNP60_FinalReport.txt) > PorcineSNP60_FinalReport_TINY.txt

# 2. MAP 파일: 100줄만
head -110 MAP_K.txt | dos2unix > MAP_K_TINY.txt 2>/dev/null || \
sed 's/\r$//' < <(head -110 MAP_K.txt) > MAP_K_TINY.txt

echo "✓ TINY 샘플 생성 완료"
ls -lh *_TINY.txt
