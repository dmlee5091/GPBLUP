# ReadFR Program - MAP File Structure Documentation

## MAP 파일 종류 및 구조

프로그램은 두 가지 버전의 MAP 파일을 지원합니다:
- **MAP_V2.txt**: V2 버전 MAP 파일 (SNP ID 형식: ALGA0000009)
- **MAP_K.txt**: K 버전 MAP 파일 (SNP ID 형식: 1_10673082)

## MAP 파일 필드 구조

모든 MAP 파일은 다음과 같은 표준화된 필드를 포함합니다:

| 필드명 | 설명 | 예시 |
|--------|------|------|
| Seq | 순번 | 1, 2, 3, ... |
| SNP_ID | SNP 식별자 | ALGA0000009 또는 1_10673082 |
| Chr | 염색체 번호 | 1, 2, ..., 20, 21 |
| Pos | 염색체 내 위치 (bp) | 8685608 |
| **Address_Total** | **전체 SNP 순서 (Chr, Position 기준)** | **426** |
| **Address_Chr** | **염색체 내 SNP 순서 (nested)** | **426** |

### Address 필드 설명

#### Address_Total (ARRAY_ALL)
- **목적**: 전체 SNP을 Chr, Position으로 순서화 (모든 염색체 통합)
- **용도**: 서로 다른 MAP 버전(V2, K)에서 공통 SNP 추출 시 사용
- **정렬**: Chr 순서 → Position 순서 (1번 염색체 → 21번 염색체)

#### Address_Chr (ARRAY_CHR)
- **목적**: 각 염색체 내에서 SNP을 Position으로 순서화 (nested sorting)
- **용도**: 염색체별 분석 수행 시 사용
- **정렬**: 각 염색체 내에서만 Position 기준으로 정렬

## Parameter 파일 설정

MAP 파일은 다음과 같이 Parameter 파일에 정의됩니다:

```
# MAP File information
MAPFILE: MAP_K.txt
HEADER: 1
DELIM: TAB
NO_VARIABLES: 5
2 SNP_ID
3 CHR
4 POS
5 ARRAY_ALL      # Address_Total 필드 (전체 순서)
6 ARRAY_CHR      # Address_Chr 필드 (염색체 내 순서)
```

**중요**: 
- Parameter 파일에서는 대문자로 `ARRAY_ALL`, `ARRAY_CHR`을 지정
- 실제 MAP 파일의 컬럼명은 `Address_Total`, `Address_Chr`
- 프로그램은 Parameter 필드명으로 찾으므로 대소문자가 일치해야 함

## FR 파일 종류 및 처리

프로그램은 두 가지 형태의 FR(FinalReport) 파일을 지원합니다:

### 1. ANIMAL_ID 기반 FR 파일
- Sample ID 필드에 개체 ID가 저장된 경우
- Parameter 설정:
  ```
  1 ANIMAL_ID      # 1번째 컬럼 = Sample ID
  ```

### 2. ANIMAL_ARN 기반 FR 파일
- Sample Name 필드에 개체 ARN이 저장된 경우
- Parameter 설정:
  ```
  2 ANIMAL_ARN     # 2번째 컬럼 = Sample Name
  ```

## PED 파일 Hash 처리

PED 파일은 다음과 같이 동적으로 hash key를 선택합니다:

- **ANIMAL_ID 지정 시**: PED 파일의 `ID` 필드로 hash table 구성
- **ANIMAL_ARN 지정 시**: PED 파일의 `ARN` 필드로 hash table 구성

이를 통해:
1. FR 파일의 개체 식별 필드 (ID 또는 ARN)
2. PED 파일의 hash key (ID 또는 ARN)
3. 검색 시 사용되는 키

이 세 가지가 일치하여 정확한 개체 매칭이 이루어집니다.

## 데이터 처리 흐름

```
1. Parameter 파일 읽기
   ↓
2. ANIMAL_ID 또는 ANIMAL_ARN 필드 감지
   ↓
3. PED 파일 loading (동적 hash key 선택)
   ↓
4. MAP 파일 loading (Address_Total, Address_Chr 읽기)
   ↓
5. FR 파일 순차 처리
   - 개체 식별 (ANIMAL_ID 또는 ANIMAL_ARN)
   - PED hash table 검색
   - SNP 데이터 순차 저장 (MAP 파일 순서대로)
   - 개체별 통계 출력 (Call Rate, Valid/Invalid SNPs)
   ↓
6. GENO 파일 출력
   - 형식: Animal_ID BREED SIRE DAM SEX BDate LOC GENO
   - GENO: 76,756개 SNP (0, 1, 2, 9)
```

## 출력 예시

```
Animal[   1] DD2431675 (DD) - Total SNPs:  76756 Valid:  60239 Invalid:  16517 CallRate:  0.7848 RETAINED
Animal[   2] DD2431734 (DD) - Total SNPs:  76756 Valid:  60325 Invalid:  16431 CallRate:  0.7859 RETAINED
...
========================================
Total animals processed:           24
Animals retained (Call Rate >=   0.70):           24
Animals excluded (Low Call Rate):            0
========================================
Total Valid SNPs:      1445220
Total Invalid SNPs:       396924
========================================
```

## 프로그램 장점

1. **유연한 MAP 지원**: V2와 K 버전 모두 지원
2. **표준화된 Address 체계**: 서로 다른 MAP에서 공통 SNP 추출 가능
3. **동적 개체 식별**: ANIMAL_ID 또는 ANIMAL_ARN 자동 감지
4. **실시간 진행 확인**: 개체별 SNP 통계 화면 출력
5. **효율적인 검색**: Hash table 기반 O(1) PED 조회
6. **성능 최적화**: O(n²) → O(1) SNP 직접 인덱싱

## 테스트 파일

- `parameter_10animals`: ANIMAL_ID 기반 (Sample ID 사용)
- `parameter_10animals_ARN`: ANIMAL_ARN 기반 (Sample Name 사용)
- `MAP_K.txt`: K 버전 MAP (76,756 SNPs)
- `MAP_V2.txt`: V2 버전 MAP (다른 SNP 세트)
- `PED_Total.txt.bak`: 614,629개 개체 정보
