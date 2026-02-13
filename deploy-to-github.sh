#!/bin/bash

# GitHub 배포 자동화 스크립트
# DKBLUPF90 프로젝트를 GitHub에 푸시

set -e

echo "======================================"
echo "DKBLUPF90 GitHub 배포 자동화"
echo "======================================"
echo ""

# 1. GitHub 원격 설정
GITHUB_REPO="https://github.com/dmlee5091/DKBLUPF90.git"

echo "[1/4] GitHub 원격 저장소 설정..."
git remote add origin $GITHUB_REPO 2>/dev/null || git remote set-url origin $GITHUB_REPO
echo "✓ 원격 위치: $GITHUB_REPO"
echo ""

# 2. 브랜치 이름 변경
echo "[2/4] 브랜치 이름 변경 (master → main)..."
git branch -M main
echo "✓ 현재 브랜치: main"
echo ""

# 3. 인증 설정
echo "[3/4] Git 인증 설정..."
echo "GitHub에 푸시하기 위해 다음 정보가 필요합니다:"
echo ""
echo "옵션 1: GitHub 웹 브라우저 인증 (권장)"
echo "  - 아래 명령 실행 후 웹 브라우저에서 인증"
echo ""
echo "옵션 2: Personal Access Token (PAT)"
echo "  - GitHub Settings → Developer settings → Personal access tokens"
echo "  - 새 토큰 생성 후 아래 명령에 사용"
echo ""

# 4. 푸시
echo "[4/4] GitHub에 푸시..."
echo ""
echo "실행 명령:"
echo "  git push -u origin main"
echo ""

read -p "계속하시겠습니까? (y/n): " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "푸시 진행 중..."
    if git push -u origin main; then
        echo ""
        echo "======================================"
        echo "✓ GitHub 배포 완료!"
        echo "======================================"
        echo ""
        echo "저장소 URL: https://github.com/dmlee5091/DKBLUPF90"
        echo "커밋 로그: https://github.com/dmlee5091/DKBLUPF90/commits"
        echo ""
    else
        echo "✗ 푸시 실패"
        echo "다음을 확인하세요:"
        echo "1. GitHub에서 저장소가 생성되었는지"
        echo "2. 인증 정보가 올바른지"
        echo "3. 네트워크 연결 상태"
        exit 1
    fi
else
    echo "취소되었습니다."
    echo ""
    echo "수동으로 진행하려면:"
    echo "  git push -u origin main"
    exit 0
fi
