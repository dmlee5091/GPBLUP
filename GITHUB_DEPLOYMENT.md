# GitHub 배포 가이드

## 프로젝트를 GitHub에 공개 배포하는 방법

이 문서는 DKBLUPF90 프로젝트를 GitHub에 공개 배포하는 단계별 지침입니다.

### 방법 1: GitHub 웹 UI를 통한 배포 (권장 - 가장 간단함)

#### 1단계: GitHub 계정으로 로그인
- GitHub.com에 접속하여 dmlee5091 계정으로 로그인합니다.

#### 2단계: 새 저장소 생성
- GitHub 홈페이지에서 "New" 버튼 클릭
- 또는 https://github.com/new 직접 방문

#### 3단계: 저장소 세부사항 입력
```
Repository name:        DKBLUPF90
Description:            SNP Quality Control Pipeline - Fortran implementation for genomic data processing
Public/Private:         Public
Add a README file:      NO (이미 있으므로)
Add .gitignore:         NO (이미 있으므로)
Choose a license:       MIT License (권장)
```

#### 4단계: 로컬 Git 저장소에 원격 주소 추가
생성된 저장소 주소가 `https://github.com/dmlee5091/DKBLUPF90.git`이라면:

```bash
cd /home/dhlee/DKBLUPF90
git remote add origin https://github.com/dmlee5091/DKBLUPF90.git
git branch -M main
git push -u origin main
```

#### 5단계: GitHub 인증
- 웹 브라우저가 열리거나 인증을 요청할 때:
  1. GitHub 로그인 정보 입력 (또는 이미 로그인된 상태)
  2. "Authorize" 클릭하여 Git 작업 승인

---

### 방법 2: Personal Access Token (PAT) 사용

#### 1단계: PAT 생성 (한 번만 수행)
1. GitHub 계정 Settings → Developer settings → Personal access tokens
2. "Generate new token" 클릭
3. 토큰 이름: `DKBLUPF90-deploy`
4. 권한(Scopes) 선택:
   - [x] repo (전체 저장소 접근)
   - [x] admin:public_key (공개 키 관리)
5. "Generate token" 클릭
6. 생성된 토큰을 안전하게 복사 (다시 볼 수 없음)

#### 2단계: Git에 인증 저장
```bash
# Personal Access Token을 사용하여 인증 저장
git config --global credential.helper store

# 또는 메모리에만 저장 (재부팅 후 삭제)
git config --global credential.helper cache
```

#### 3단계: 저장소 생성 및 푸시
```bash
cd /home/dhlee/DKBLUPF90

# 원격 저장소 추가
git remote add origin https://github.com/dmlee5091/DKBLUPF90.git

# 브랜치 이름 변경 (master → main)
git branch -M main

# 푸시
git push -u origin main

# 첫 푸시 시 인증 요청됨:
# Username: dmlee5091
# Password: [PAT 토큰 붙여넣기]
```

---

### 방법 3: SSH 키 사용

#### 1단계: SSH 키 생성
```bash
ssh-keygen -t ed25519 -C "dhlee@hknu.ac.kr"
# 또는 (구형 시스템)
ssh-keygen -t rsa -b 4096 -C "dhlee@hknu.ac.kr"

# 프롬프트 응답:
# Enter file in which to save the key: /home/dhlee/.ssh/id_ed25519
# Enter passphrase: [세 번 입력 또는 엔터로 공백]
```

#### 2단계: SSH 공개 키를 GitHub에 등록
```bash
# 공개 키 표시
cat ~/.ssh/id_ed25519.pub
```

GitHubから:
1. Settings → SSH and GPG keys
2. "New SSH key" 클릭
3. Title 입력: `DKBLUPF90 Deployment`
4. Key 붙여넣기 (id_ed25519.pub 내용)
5. "Add SSH key" 클릭

#### 3단계: 저장소 생성 및 푸시
```bash
cd /home/dhlee/DKBLUPF90

# 원격 저장소 추가 (SSH 방식)
git remote add origin git@github.com:dmlee5091/DKBLUPF90.git

# 브랜치 이름 변경
git branch -M main

# 푸시
git push -u origin main
```

---

### 배포 후 확인 사항

#### 1. GitHub 저장소 확인
- https://github.com/dmlee5091/DKBLUPF90 방문
- 파일들이 업로드되었는지 확인
- README.md가 표시되는지 확인

#### 2. 저장소 설정 최적화
GitHub 저장소 Settings에서:

**About 섹션:**
- Description: "SNP Quality Control Pipeline for Genomic Data Processing"
- Website: (선택 사항) 문서 URL
- Topics 추가: `fortran`, `genomics`, `snp-analysis`, `bioinformatics`, `quality-control`

**Features:**
- Issues: Enable
- Discussions: Enable  
- Wiki: Disable (문서가 Markdown에 있음)
- Releases: Enable

**Access:**
- Repository visibility: Public

#### 3. Release 생성
1. GitHub 저장소 페이지 → "Releases" 탭
2. "Create a new release" 클릭
3. 세부사항 입력:
   ```
   Tag version:      v1.0
   Release title:    DKBLUPF90 v1.0 - SNP Quality Control Pipeline
   Description:
   
   ## Release Highlights
   - Hash table-based O(1) animal lookup
   - Case-insensitive parameter file parsing
   - Comprehensive QC filtering criteria
   - Professional documentation and installation guide
   - Support for multiple SNP chips and genotyping platforms
   
   ## Installation
   See [INSTALL.md](INSTALL.md) for detailed installation instructions.
   
   ## Documentation
   - [User Manual](READFR_USER_MANUAL.md)
   - [Installation Guide](INSTALL.md)
   - [SNP QC Guide](SNP_QC_GUIDE.md)
   ```
4. "Publish release" 클릭

---

### 배포 후 협업 설정 (선택 사항)

#### 1. Collaborators 추가
Settings → Collaborators → Add people
(협업자가 있는 경우)

#### 2. Branch Protection Rules 설정
Settings → Branches → Add rule
```
Branch name pattern: main
- [x] Require pull request reviews before merging
- [x] Require status checks to pass before merging
```

#### 3. GitHub Actions 활성화 (CI/CD - 추가 기능)
Actions 탭에서 Fortran 빌드 워크플로우 설정 가능

---

### 명령어 빠른 참조

```bash
# 원격 저장소 확인
git remote -v

# 원격 위치 변경
git remote set-url origin https://github.com/dmlee5091/DKBLUPF90.git

# 현재 상태 확인
git status

# 푸시
git push origin main

# 특정 태그 푸시
git tag v1.0
git push origin v1.0

# 모든 태그 푸시
git push origin --tags
```

---

### 배포 후 업데이트

#### 로컬에서 변경 후 GitHub 업데이트:
```bash
cd /home/dhlee/DKBLUPF90
git add .
git commit -m "Description of changes"
git push origin main
```

#### 새 버전 릴리스:
```bash
git tag -a v1.1 -m "Version 1.1 release"
git push origin v1.1
# GitHub에서 Release 페이지에서 릴리스 노트 추가
```

---

### 문제 해결

#### "fatal: 'origin' does not appear to be a 'git' repository"
```bash
# 원격 저장소가 없음
git remote add origin https://github.com/dmlee5091/DKBLUPF90.git
```

#### "Permission denied (publickey)"
- SSH 키 설정 재확인
- SSH 키의 권한 확인: `chmod 600 ~/.ssh/id_ed25519`

#### "fatal: Authentication failed"
- Personal Access Token이 만료되었는지 확인
- Git credential helper 재설정

---

## GitHub 배포 완료!

성공적으로 배포된 후:
1. GitHub에서 프로젝트 공개
2. 동료 및 커뮤니티와 공유 가능
3. Issues, Pull Requests를 통한 협업 가능
4. Releases를 통한 버전 관리 가능

### 공유 URL
https://github.com/dmlee5091/DKBLUPF90

---

**배포 일시**: February 13, 2026  
**버전**: 1.0  
**개발자**: Dr. DEUKMIN LEE
