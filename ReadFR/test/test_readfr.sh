#!/bin/bash

echo "════════════════════════════════════════════════"
echo "🧪 ReadFR 통합 테스트 스크립트"
echo "════════════════════════════════════════════════"
echo ""

# 테스트 1: 기본 실행 테스트 (help 메시지)
echo "테스트 1️⃣: ReadFR 기본 실행"
echo "────────────────────────────────────"
/home/dhlee/GPBLUP/bin/ReadFR 2>&1 | head -5 || true
echo ""

# 테스트 2: 파라미터 파일 확인
echo "테스트 2️⃣: 파라미터 파일 존재 확인"
echo "────────────────────────────────────"
for file in parameter parameter_sample parameter_tiny; do
    if [ -f "$file" ]; then
        echo "✓ $file ($(wc -l < $file) 줄)"
    fi
done
echo ""

# 테스트 3: 샘플 데이터 파일 확인
echo "테스트 3️⃣: 샘플 데이터 파일 크기"
echo "────────────────────────────────────"
ls -lh *_SAMPLE.txt *_TINY.txt 2>/dev/null | awk '{print $9, "-", $5}' || true
echo ""

# 테스트 4: 라인 엔딩 확인
echo "테스트 4️⃣: 파일 라인 엔딩 검사"
echo "────────────────────────────────────"
echo "원본 파일 (CRLF 혼합):"
head -2 PorcineSNP60_FinalReport.txt | file - | head -1 || true
echo ""
echo "처리된 파일 (LF):"
head -2 PorcineSNP60_FinalReport_TINY.txt | file - | head -1 || true
echo ""

# 테스트 5: 요약
echo "════════════════════════════════════════════════"
echo "✅ 테스트 완료"
echo "════════════════════════════════════════════════"
echo ""
echo "📝 GitHub 배포용 권장 사항:"
echo "  • SAMPLE 파일 포함 (빠른 테스트)"
echo "  • README_SAMPLE.txt로 사용 방법 문서화"
echo "  • 원본 파일은 제외 (크기 문제)"
echo ""
