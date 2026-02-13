program ReadFR
  use M_Kinds
  use M_Stamp
  use M_Variables, only: FileInfo, SNPInfo, PEDInfo, QC_Metrics, QC_Counters, &
                         PEDFile, DATAFile, SNPFile, MAPFile, OutputPrefix, &
                         MAX_STR, MAX_VAR, LEN_STR
  use M_StrEdit
  use M_ReadFile
  use M_readpar, only: read_parameters, M_readpar_get_thresholds
  use M_PEDHashTable
  implicit none

  character(len=MAX_STR) :: Par_File
  character(LEN=MAX_STR) :: XC(MAX_VAR)
  real(kind=r4) :: XR(MAX_VAR)
  integer(kind=ki4) :: XI(MAX_VAR)
  integer(kind=ki1),allocatable :: GENO(:)
  type(PEDHashTable) :: PED
  type(PEDInfo) :: PED_REC
  type(SNPInfo),allocatable :: MapInfo(:)
  
  integer :: unitGENO, unitF, n, i, j, k1, k2
  integer :: NREC, nSNP, NARN, tSNP, count_miss
  integer :: countX(3), countY(3), countY_hetero_errors
  integer :: total_animals, animals_retained, animals_low_callrate, snp_count
  character(len=1) :: Allele1, Allele2
  character(len=MAX_STR) :: GenoFileName
  character(len=LEN_STR) :: Animal_ID, Animal_ARN, temp_str
  integer(kind=ki1) :: genotype
  real :: call_rate_animal
  real :: thresh_min_gc, thresh_min_r, thresh_max_r
  real :: thresh_min_gt, thresh_min_cluster, thresh_min_callrate
  
  type(QC_Metrics) :: local_qc_metrics
  type(QC_Counters) :: local_qc_counters, global_qc_counters

   call version('1.0 - GenomeQC SNP Quality Control Pipeline')
   print '(a)'
   call timestamp()
   print '(a)'
   
   call getarg(1,Par_File)
   if((len_trim(Par_File)).lt.1) stop 'Input parameter file'

   call read_parameters(Par_File)
   print*,"PED File Name=", trim(PEDFile%FileName)
   
   call M_readpar_get_thresholds(thresh_min_gc, thresh_min_r, thresh_max_r, &
                                 thresh_min_gt, thresh_min_cluster, thresh_min_callrate)
   call load_ped_file(PED, NARN, unitF)
   call load_map_file(MapInfo, tSNP, unitF)
   call get_snp_dimensions(SNPFile, unitF, nSNP, NREC)
   call initialize_qc_variables(total_animals, animals_retained, &
                                animals_low_callrate, global_qc_counters)
   call print_qc_criteria()
   
   call generate_output_filename(GenoFileName)
   unitGENO = 999
   open(unit=unitGENO, file=trim(GenoFileName), status='replace', action='write')
   write(unitGENO,'(A)') 'Animal_ID BREED SIRE DAM SEX BDate LOC GENO(1-76756)'
   
   do i=1,NREC
      GENO = 9_ki1
      countY_hetero_errors = 0
      call initialize_animal_variables(Animal_ID, genotype, count_miss, &
                                       countX, countY, snp_count, local_qc_counters)
      do j=1,nSNP
         call readline(unitF,n,XC,XR,dlm_str=trim(SNPFile%Delim_char))
         if (j == 1) then
            call extract_animal_id(Animal_ARN, Animal_ID, PED_REC, PED, SNPFile, XC)
         end if
         call extract_alleles_and_genotype(Allele1, Allele2, temp_str, genotype, &
                                          SNPFile, XC, k1, k2)
         call apply_snp_qc_filters(local_qc_metrics, local_qc_counters, MapInfo, &
                                  XR, SNPFile, Allele1, Allele2, genotype, j)
         call store_genotype(GENO, MapInfo, genotype, local_qc_metrics%pass_qc, j, count_miss)
         if (local_qc_metrics%pass_qc .and. genotype /= 9_ki1) then
            if(MapInfo(j)%Chr == 20) then
               call check_Sex(genotype, countX)
            elseif(MapInfo(j)%Chr == 21) then
               ! Y 염색체: 이질 접합 필터링 (Y 염색체는 단일 사본이므로 hetero는 오류)
               if(genotype == 1_ki1) then
                  ! 이질 접합은 미싱으로 처리
                  GENO(MapInfo(j)%Array_All) = 9_ki1
                  countY_hetero_errors = countY_hetero_errors + 1
               else
                  ! 정상 유전자형 카운팅
                  call check_Sex(genotype, countY)
               end if
            end if
         end if
         
         snp_count = snp_count + 1
      end do
      
      call calculate_call_rate(snp_count, count_miss, call_rate_animal)
      call print_animal_statistics(i, Animal_ID, total_animals, animals_retained, &
                                  animals_low_callrate, call_rate_animal, snp_count, &
                                  count_miss, countX, countY, countY_hetero_errors, &
                                  GENO, PED_REC, unitGENO, tSNP)
      call accumulate_global_qc_stats(global_qc_counters, local_qc_counters, call_rate_animal)
   end do
   close(unit=unitGENO)
   print*, ""
   print*, "GENO file saved: "//trim(GenoFileName)
   print*, "File format: OutputPrefix_YYYYMMDD_SequenceNumber.geno"
   close(unit=unitF)
   call print_qc_summary(total_animals, animals_retained, animals_low_callrate, global_qc_counters)
contains
! Load PED file and populate hash table
subroutine load_ped_file(PED, NARN, unitF)
   type(PEDHashTable), intent(out) :: PED
   integer, intent(out) :: NARN, unitF
   integer :: nrec_in_file, i, n
   
   NARN = N_recf(PEDFile%FileName)
   NARN = NARN - PEDFile%Header
   print*,"Total number of PED records to read=", NARN
   call pht_create(PED, int(NARN*1.3))
   unitF=fopen(trim(PEDFile%FileName))
   
   ! Skip header
   do i=1,PEDFile%Header
      call readline(unitF,n,XC,XI,dlm_str=trim(PEDFile%Delim_char))
   end do

   ! Read and store PED records
   do i=1,NARN
       call readline(unitF,n,XC,XI,dlm_str=trim(PEDFile%Delim_char))
       if(n < 0) exit
       call init_Ped(PED_REC)
       PED_REC%BREED= trim(XC(PEDFile%FieldLoc(find_field_index('BREED',PEDFile%FieldName))))
       PED_REC%ID   = trim(XC(PEDFile%FieldLoc(find_field_index('ID',PEDFile%FieldName))))
       PED_REC%ARN  = trim(XC(PEDFile%FieldLoc(find_field_index('ARN',PEDFile%FieldName))))
       ! Skip missing ARN
       if (len_trim(PED_REC%ARN) == 0 .or. trim(PED_REC%ARN) == "0") cycle
       PED_REC%SIRE = trim(XC(PEDFile%FieldLoc(find_field_index('SIRE',PEDFile%FieldName))))
       PED_REC%DAM  = trim(XC(PEDFile%FieldLoc(find_field_index('DAM',PEDFile%FieldName))))
       PED_REC%SEX  = XI(PEDFile%FieldLoc(find_field_index('SEX',PEDFile%FieldName)))      
       PED_REC%BDate= XI(PEDFile%FieldLoc(find_field_index('BDate',PEDFile%FieldName)))    
       ! Read LOC field as text string
       PED_REC%LOC  = adjustl(trim(XC(PEDFile%FieldLoc(find_field_index('LOC',PEDFile%FieldName)))))
       call pht_insert(PED, PED_REC)
   end do
   close(unit=unitF)
   
   print*,"Total number of non-zero ARN records in hash table=", PED%count
   NARN = PED%count
end subroutine load_ped_file
! Load MAP file with SNP information
subroutine load_map_file(MapInfo, tSNP, unitF)
   type(SNPInfo), allocatable, intent(out) :: MapInfo(:)
   integer, intent(out) :: tSNP, unitF
   integer :: i, n
   
   tSNP = N_recf(trim(MAPFile%FileName))
   tSNP = tSNP - MAPFile%Header
   print*,"Total number of SNPs in MAP file=", tSNP
   allocate(MapInfo(tSNP), GENO(tSNP))
   
   unitF = fopen(trim(MAPFile%FileName))
   do i=1,MAPFile%Header
      call readline(unitF,n,XC,XI,dlm_str=trim(MAPFile%Delim_char))
   end do
   
   do i=1,tSNP
       call readline(unitF,n,XC,XI,dlm_str=trim(MAPFile%Delim_char))
       MapInfo(i)%SNP_ID    = trim(XC(MAPFile%FieldLoc(find_field_index('SNP_ID',MAPFile%FieldName))))
       MapInfo(i)%Chr       = XI(MAPFile%FieldLoc(find_field_index('Chr',MAPFile%FieldName)))
       MapInfo(i)%Pos       = XI(MAPFile%FieldLoc(find_field_index('Pos',MAPFile%FieldName)))
       MapInfo(i)%Array_All = XI(MAPFile%FieldLoc(find_field_index('Array_All',MAPFile%FieldName)))
       MapInfo(i)%Array_Chr = XI(MAPFile%FieldLoc(find_field_index('Array_Chr',MAPFile%FieldName)))
   end do
   close(unit=unitF)
end subroutine load_map_file
! Get SNP dimensions from file header
subroutine get_snp_dimensions(SNPFile, unitF, nSNP, NREC)
   type(FileInfo), intent(in) :: SNPFile
   integer, intent(out) :: unitF, nSNP, NREC
   integer :: i, n
   
   unitF = fopen(trim(SNPFile%FileName))
   nSNP = 0
   NREC = 0
   
   do i=1,SNPFile%Header     
      call readline(unitF,n,XC,XI,dlm_str=trim(SNPFile%Delim_char))
      if(n < 0) exit
      if(trim(XC(1)) == "Num SNPs") nSNP = XI(2)
      if(trim(XC(1)) == "Num Samples") NREC = XI(2)
   end do
   
   print*,"number of SNP=", nSNP
   print*,"number of Animals=", NREC
end subroutine get_snp_dimensions
! Initialize global QC statistics
subroutine initialize_qc_variables(total_animals, animals_retained, &
                                   animals_low_callrate, global_qc_counters)
   integer, intent(out) :: total_animals, animals_retained, animals_low_callrate
   type(QC_Counters), intent(out) :: global_qc_counters
   
   total_animals = 0
   animals_retained = 0
   animals_low_callrate = 0
   global_qc_counters%fail_gc = 0
   global_qc_counters%fail_r = 0
   global_qc_counters%fail_gt = 0
   global_qc_counters%fail_cluster = 0
   global_qc_counters%fail_allele = 0
   global_qc_counters%fail_chr = 0
end subroutine initialize_qc_variables
! Initialize per-animal variables
subroutine initialize_animal_variables(Animal_ID, genotype, count_miss, &
                                       countX, countY, snp_count, local_qc_counters)
   character(len=*), intent(out) :: Animal_ID
   integer(kind=ki1), intent(out) :: genotype
   integer, intent(out) :: count_miss, snp_count
   integer, intent(out) :: countX(3), countY(3)
   type(QC_Counters), intent(out) :: local_qc_counters
   
   Animal_ID = '***'
   genotype = 9_ki1
   count_miss = 0
   countX = 0
   countY = 0
   snp_count = 0
   local_qc_counters%fail_gc = 0
   local_qc_counters%fail_r = 0
   local_qc_counters%fail_gt = 0
   local_qc_counters%fail_cluster = 0
   local_qc_counters%fail_allele = 0
   local_qc_counters%fail_chr = 0
end subroutine initialize_animal_variables

! =========================================================================
! 6. QC 기준 출력
! =========================================================================
subroutine print_qc_criteria()
   implicit none
   
   print*,""
   print*,"="//repeat("=",70)
   print*,"Starting SNP Quality Control with configured criteria:"
   print '(A,F5.2)', "  - GC Score >= ", thresh_min_gc
   print '(A,F4.2,A,F4.2)', "  - R (Intensity) ", thresh_min_r, " - ", thresh_max_r
   print '(A,F5.2)', "  - GT Score >= ", thresh_min_gt
   print '(A,F5.2)', "  - Cluster Separation >= ", thresh_min_cluster
   print '(A,F5.2)', "  - Animal Call Rate >= ", thresh_min_callrate
   print*,"="//repeat("=",70)
   print*,""
end subroutine print_qc_criteria

! =========================================================================
! 7. 동물 ID 추출
! =========================================================================
subroutine extract_animal_id(Animal_ARN, Animal_ID, PED_REC, PED, SNPFile, XC)
   character(len=*), intent(out) :: Animal_ARN, Animal_ID
   type(PEDInfo), intent(inout) :: PED_REC
   type(PEDHashTable), intent(in) :: PED
   type(FileInfo), intent(in) :: SNPFile
   character(len=*), intent(in) :: XC(:)
   
   if(find_field_index('ANIMAL_ID', SNPFile%FieldName) == -1) then
      Animal_ARN = trim(XC(SNPFile%FieldLoc(find_field_index('ANIMAL_ARN',SNPFile%FieldName))))
      call init_Ped(PED_REC)
      if(pht_search(PED, Animal_ARN, PED_REC)) then
         Animal_ID = trim(PED_REC%ID)
      else
         Animal_ID = trim(XC(SNPFile%FieldLoc(find_field_index('ANIMAL_ARN',SNPFile%FieldName))))
      end if
   else
      Animal_ID = trim(XC(SNPFile%FieldLoc(find_field_index('ANIMAL_ID',SNPFile%FieldName))))
   end if
end subroutine extract_animal_id

! =========================================================================
! 8. 대립유전자 추출 및 유전자형 결정
! =========================================================================
subroutine extract_alleles_and_genotype(Allele1, Allele2, temp_str, genotype, &
                                        SNPFile, XC, k1, k2)
   character(len=1), intent(out) :: Allele1, Allele2
   character(len=*), intent(out) :: temp_str
   integer(kind=ki1), intent(out) :: genotype
   type(FileInfo), intent(in) :: SNPFile
   character(len=*), intent(in) :: XC(:)
   integer, intent(out) :: k1, k2
   
   Allele1 = ' '
   Allele2 = ' '
   
   k1 = SNPFile%FieldLoc(find_field_index('Allele1_AB', SNPFile%FieldName))
   if (len_trim(XC(k1)) > 0) then
      temp_str = trim(XC(k1))
      Allele1 = temp_str(1:1)
   end if   
   
   k2 = SNPFile%FieldLoc(find_field_index('Allele2_AB', SNPFile%FieldName))
   if (len_trim(XC(k2)) > 0) then
      temp_str = trim(XC(k2))
      Allele2 = temp_str(1:1)
   end if
   
   genotype = SeekGeno(Allele1, Allele2)
end subroutine extract_alleles_and_genotype

! =========================================================================
! 9. SNP QC 필터 적용 - 동적 컬럼 위치 기반 (매개변수 간소화)
! =========================================================================
subroutine apply_snp_qc_filters(out_metrics, out_counters, MapInfo, &
                                XR, SNPFile, Allele1, Allele2, genotype, j)
   implicit none
   type(QC_Metrics), intent(out) :: out_metrics
   type(QC_Counters), intent(inout) :: out_counters
   type(SNPInfo), intent(in) :: MapInfo(:)
   real(kind=r4), intent(in) :: XR(:)
   type(FileInfo), intent(in) :: SNPFile
   character(len=1), intent(in) :: Allele1, Allele2
   integer(kind=ki1), intent(in) :: genotype
   integer, intent(in) :: j
   integer :: col_gc, col_ri, col_gt, col_cs, idx_gc, idx_ri, idx_gt, idx_cs
   
   ! 동적으로 컬럼 위치 찾기 (파라미터 파일에서 지정된 위치 사용)
   idx_gc = find_field_index('GC_Score', SNPFile%FieldName)
   idx_ri = find_field_index('R_Intensity', SNPFile%FieldName)
   idx_gt = find_field_index('GT_Score', SNPFile%FieldName)
   idx_cs = find_field_index('Cluster_Sep', SNPFile%FieldName)
   
   ! 유효한 인덱스 확인 (find_field_index가 -1 반환하는 경우 처리)
   if(idx_gc > 0 .and. idx_gc <= size(SNPFile%FieldLoc)) then
      col_gc = SNPFile%FieldLoc(idx_gc)
   else
      col_gc = 27  ! GC_Score 기본값
   end if
   
   if(idx_ri > 0 .and. idx_ri <= size(SNPFile%FieldLoc)) then
      col_ri = SNPFile%FieldLoc(idx_ri)
   else
      col_ri = 25  ! R-Intensity 기본값
   end if
   
   if(idx_gt > 0 .and. idx_gt <= size(SNPFile%FieldLoc)) then
      col_gt = SNPFile%FieldLoc(idx_gt)
   else
      col_gt = 29  ! GT_Score 기본값
   end if
   
   if(idx_cs > 0 .and. idx_cs <= size(SNPFile%FieldLoc)) then
      col_cs = SNPFile%FieldLoc(idx_cs)
   else
      col_cs = 30  ! Cluster_Sep 기본값
   end if
   
   ! Extract QC metrics from FinalReport (동적 컬럼 위치 사용)
   ! column 범위 확인
   if(col_gc > 0 .and. col_gc <= size(XR)) then
      out_metrics%gc_score = XR(col_gc)
   else
      out_metrics%gc_score = 0.0_r4
   end if
   
   if(col_ri > 0 .and. col_ri <= size(XR)) then
      out_metrics%r_value = XR(col_ri)
   else
      out_metrics%r_value = 0.0_r4
   end if
   
   if(col_gt > 0 .and. col_gt <= size(XR)) then
      out_metrics%gt_score = XR(col_gt)
   else
      out_metrics%gt_score = 0.0_r4
   end if
   
   if(col_cs > 0 .and. col_cs <= size(XR)) then
      out_metrics%cluster_sep = XR(col_cs)
   else
      out_metrics%cluster_sep = 0.0_r4
   end if
   
   out_metrics%pass_qc = .true.
   
   ! 1. GC Score check
   if(out_metrics%gc_score < thresh_min_gc) then
      out_metrics%pass_qc = .false.
      out_counters%fail_gc = out_counters%fail_gc + 1
   end if
   
   ! 2. Intensity (R) check
   if(out_metrics%r_value < thresh_min_r .or. &
      out_metrics%r_value > thresh_max_r) then
      out_metrics%pass_qc = .false.
      out_counters%fail_r = out_counters%fail_r + 1
   end if
   
   ! 3. GT Score check
   if(out_metrics%gt_score < thresh_min_gt) then
      out_metrics%pass_qc = .false.
      out_counters%fail_gt = out_counters%fail_gt + 1
   end if
   
   ! 4. Cluster Separation check
   if(out_metrics%cluster_sep < thresh_min_cluster) then
      out_metrics%pass_qc = .false.
      out_counters%fail_cluster = out_counters%fail_cluster + 1
   end if
   
   ! 5. Allele validation
   if(Allele1 == ' ' .or. Allele2 == ' ') then
      out_metrics%pass_qc = .false.
      out_counters%fail_allele = out_counters%fail_allele + 1
   end if
   
   ! 6. Chromosome filter (1-21 for pig)
   if(MapInfo(j)%Chr < 1 .or. MapInfo(j)%Chr > 21) then
      out_metrics%pass_qc = .false.
      out_counters%fail_chr = out_counters%fail_chr + 1
   end if
end subroutine apply_snp_qc_filters

! =========================================================================
! 10. 유전자형 저장
! =========================================================================
subroutine store_genotype(GENO, MapInfo, genotype, pass_QC, j, count_miss)
   integer(kind=ki1), intent(inout) :: GENO(:)
   type(SNPInfo), intent(in) :: MapInfo(:)
   integer(kind=ki1), intent(in) :: genotype
   logical, intent(in) :: pass_QC
   integer, intent(in) :: j
   integer, intent(inout) :: count_miss
   
   if(genotype == 9_ki1 .or. .not. pass_QC) then
      GENO(MapInfo(j)%Array_All) = 9_ki1    ! Missing
      count_miss = count_miss + 1
   else
      GENO(MapInfo(j)%Array_All) = genotype
   end if
end subroutine store_genotype

! =========================================================================
! 11. Call Rate 계산
! =========================================================================
subroutine calculate_call_rate(snp_count, count_miss, call_rate_animal)
   integer, intent(in) :: snp_count, count_miss
   real, intent(out) :: call_rate_animal
   
   if(snp_count > 0) then
      call_rate_animal = 1.0_r4 - real(count_miss)/real(snp_count)
   else
      call_rate_animal = 0.0_r4
   end if
end subroutine calculate_call_rate

! =========================================================================
! 12. 동물별 통계 출력 및 필터링
! =========================================================================
subroutine print_animal_statistics(i, Animal_ID, total_animals, animals_retained, &
                                   animals_low_callrate, call_rate_animal, &
                                   snp_count, count_miss, countX, countY, &
                                   countY_hetero_errors, GENO, PED_REC, unitGENO, nSNP)
   implicit none
   integer, intent(in) :: i, snp_count, count_miss, unitGENO, nSNP
   character(len=*), intent(in) :: Animal_ID
   integer, intent(inout) :: total_animals, animals_retained, animals_low_callrate
   real, intent(in) :: call_rate_animal
   integer, intent(in) :: countX(3), countY(3)
   integer, intent(in) :: countY_hetero_errors
   integer(kind=ki1), intent(in) :: GENO(:)
   type(PEDInfo), intent(in) :: PED_REC
   
   integer :: total_X_snps, total_Y_snps, j
   real :: x_hetero_ratio
   integer :: genomic_sex, ped_sex_match
   character(len=10) :: sex_str, genomic_sex_str, match_str
   
   !============================================
   !동물 처리 기본 정보 출력
   !============================================
   if(mod(i,100) == 1) then
      print*, ""
      print '(A,I5,2x,A,A,2x,A,F6.3,2x,A,I6,2x,A,I6)', "Animal #", i, "ID: ", trim(Animal_ID), &
            "Call Rate: ", call_rate_animal, "  Total SNPs: ", snp_count, "Missing SNPs: ", count_miss
   end if

   total_animals = total_animals + 1
   
   ! Call Rate 필터 (parameter에서 설정된 값 사용) - 낮은 개체 삭제
   if(call_rate_animal < thresh_min_callrate) then
      animals_low_callrate = animals_low_callrate + 1
      print '(A,I5,A,A,A,F6.3,A,F5.2)', "  [EXCLUDED] Animal #", i, " ID: ", &
            trim(Animal_ID), " - Low Call Rate: ", call_rate_animal, " < ", thresh_min_callrate
      return
   end if
   
   animals_retained = animals_retained + 1
   
   ! ============================================
   ! GENO와 혈통정보를 파일에 저장 (QC 통과 개체만)
   ! ============================================
   write(unitGENO,'(A)', advance='no') trim(Animal_ID) // ' '
   write(unitGENO,'(A)', advance='no') trim(PED_REC%BREED) // ' '
   write(unitGENO,'(A)', advance='no') trim(PED_REC%SIRE) // ' '
   write(unitGENO,'(A)', advance='no') trim(PED_REC%DAM) // ' '
   write(unitGENO,'(I1,A)', advance='no') PED_REC%SEX, ' '
   write(unitGENO,'(I8,A)', advance='no') PED_REC%BDate, ' '
   ! Output LOC field (text format) - remove null characters
   temp_str = adjustl(trim(PED_REC%LOC))
   write(unitGENO,'(A)', advance='no') trim(temp_str) // ' '
   do j=1,nSNP
      write(unitGENO,'(I1)', advance='no') GENO(j)
   end do
   write(unitGENO,'(A)') ''  ! New line
   
   ! ============================================
   ! X 염색체 기반 성별 예측 (정보만 표시)
   ! ============================================
   total_X_snps = countX(1) + countX(2) + countX(3)
   if(total_X_snps > 0) then
      x_hetero_ratio = real(countX(2)) / real(total_X_snps)
      
      ! 이질 접합 비율로 성별 판정 (임계값 0.30)
      if(x_hetero_ratio > 0.30) then
         genomic_sex = MALE  ! 수컷 (XY): 높은 이질 접합 비율
      else
         genomic_sex = MALE + 1  ! 암컷 (XX): 낮은 이질 접합 비율 (2 = 암컷으로 사용)
      end if
      
      ! PED 성별과 비교
      if(genomic_sex == PED_REC%SEX) then
         ped_sex_match = 1
         match_str = "MATCH"
      else
         ped_sex_match = 0
         match_str = "MISMATCH"
      end if
      
      ! 성별 문자열 설정
      if(genomic_sex == MALE) then
         genomic_sex_str = "MALE"
      else
         genomic_sex_str = "FEMALE"
      end if
      if(PED_REC%SEX == MALE) then
         sex_str = "MALE"
      else
         sex_str = "FEMALE"
      end if
      
      ! ! 성별 판정 결과 출력 (정보만 표시, 동물 제외하지 않음)
      ! print '(A,F6.4,A,I5,A,I5,A,I5)', &
      !       "  X_Chromosome: HeteroRatio=", x_hetero_ratio, &
      !       " (0/1/2)=", countX(1), "/", countX(2), "/", countX(3)
      ! print '(A,A,A,A,A,A)', "  Genomic Sex: ", trim(genomic_sex_str), &
      !       " | PED Sex: ", trim(sex_str), " | Result: ", trim(match_str)
   end if
   
   ! ! ============================================
   ! ! Y 염색체 이질 접합 오류 보고 (정보만 표시)
   ! ! ============================================
   ! if(countY_hetero_errors > 0) then
   !    print '(A,I5,A,I5,A)', "  [Y_CHR_INFO] Animal #", i, &
   !          " - Y chromosome heterozygous SNPs marked as missing: ", countY_hetero_errors, &
   !          " (These are biological errors and marked as missing data)"
   ! end if
   
   ! total_Y_snps = countY(1) + countY(2) + countY(3)
   ! if(total_Y_snps > 0) then
   !    print '(A,I5,A,I5,A,I5)', "  Y_Chromosome: SNP counts (0/1/2)=", &
   !          countY(1), "/", countY(2), "/", countY(3)
   ! end if
end subroutine print_animal_statistics

! =========================================================================
! 13. 글로벌 QC 통계 누적
! =========================================================================
subroutine accumulate_global_qc_stats(global_qc_counters, local_qc_counters, &
                                     call_rate_animal)
   implicit none
   type(QC_Counters), intent(inout) :: global_qc_counters
   type(QC_Counters), intent(in) :: local_qc_counters
   real, intent(in) :: call_rate_animal
   
   ! Call Rate 필터에 통과한 동물만 누적
   if(call_rate_animal >= 0.70_r4) then
      global_qc_counters%fail_gc = global_qc_counters%fail_gc + local_qc_counters%fail_gc
      global_qc_counters%fail_r = global_qc_counters%fail_r + local_qc_counters%fail_r
      global_qc_counters%fail_gt = global_qc_counters%fail_gt + local_qc_counters%fail_gt
      global_qc_counters%fail_cluster = global_qc_counters%fail_cluster + local_qc_counters%fail_cluster
      global_qc_counters%fail_allele = global_qc_counters%fail_allele + local_qc_counters%fail_allele
      global_qc_counters%fail_chr = global_qc_counters%fail_chr + local_qc_counters%fail_chr
   end if
end subroutine accumulate_global_qc_stats

! =========================================================================
! 14. 최종 QC 요약 출력
! =========================================================================
subroutine print_qc_summary(total_animals, animals_retained, animals_low_callrate, &
                            global_qc_counters)
   integer, intent(in) :: total_animals, animals_retained, animals_low_callrate
   type(QC_Counters), intent(in) :: global_qc_counters
   
   print*,""
   print*,"="//repeat("=",70)
   print*,"SNP QUALITY CONTROL SUMMARY"
   print*,"="//repeat("=",70)
   print*,"Total Animals Processed:       ", total_animals
   print*,"Animals Excluded (Low Call):   ", animals_low_callrate
   print*,"Animals Retained:              ", animals_retained
   print*,""
   print*,"QC Failures by Criterion (Aggregate) [with empirical thresholds]:"
   print*,"  - GC Score < 0.65:           ", global_qc_counters%fail_gc
   print*,"  - R out of range 0.4-2.0:    ", global_qc_counters%fail_r
   print*,"  - GT Score < 0.50:           ", global_qc_counters%fail_gt
   print*,"  - Cluster Sep < 0.30:        ", global_qc_counters%fail_cluster
   print*,"  - Missing Alleles:           ", global_qc_counters%fail_allele
   print*,"  - Invalid Chromosome:        ", global_qc_counters%fail_chr
   print*,"="//repeat("=",70)
end subroutine print_qc_summary

! =========================================================================
! 15. 성염색체 검증 (기존 부프로그램)
! =========================================================================
subroutine check_Sex(Sex_Genotype, cnt)
   integer(kind=ki1), intent(in) :: Sex_Genotype
   integer, intent(inout) :: cnt(3)

   if(Sex_Genotype == 9_ki1) then
      return
   else
      select case(Sex_Genotype)
         case(0_ki1)
            cnt(1) = cnt(1) + 1
         case(1_ki1)
            cnt(2) = cnt(2) + 1
         case(2_ki1)
            cnt(3) = cnt(3) + 1
      end select
   end if
end subroutine Check_Sex

subroutine init_rng()
  implicit none
  integer :: seed_size, iseed
  integer, allocatable :: seed(:)

  call random_seed(size=seed_size)
  allocate(seed(seed_size))
  do iseed = 1, seed_size
     seed(iseed) = int(1000.0 * real(iseed) + 12345)
  end do
  call random_seed(put=seed)
  deallocate(seed)
end subroutine init_rng

integer function rand_int(a, b)
  implicit none
  integer, intent(in) :: a, b
  real :: r
  call random_number(r)  ! r in [0,1)
  rand_int = a + int(r * real(b - a + 1))
end function rand_int   

integer function find_field_index(str,FieldName)
   character(len=*), intent(in) :: str
   character(len=*), dimension(:), intent(in) :: FieldName
   integer :: idx
      find_field_index = -1
      do idx=1,size(FieldName)
         if(trim(to_upper(str)) == trim(to_upper(FieldName(idx)))) then
            find_field_index = idx
            return
         end if
      end do
end function find_field_index

logical function check_SNP_ID(ID1, ID2)
  implicit none
  character(len=*), intent(in) :: ID1, ID2
  if(trim(ID1) == trim(ID2)) then
     check_SNP_ID = .true.
  else
     check_SNP_ID = .false.
  end if
end function check_SNP_ID 

integer(kind=ki1) function SeekGeno(A1, A2)
  implicit none
  character(len=1), intent(in) :: A1, A2
  integer(kind=ki1) :: A1_N, A2_N
  A1_N = changeNum(A1)     
  A2_N = changeNum(A2)     
  if(max(A1_N, A2_N) == 9_ki1) then
     SeekGeno = 9_ki1
  else   
     SeekGeno = A1_N + A2_N
  end if
end function SeekGeno

integer(kind=ki1) function changeNum(A_char) result(A_num)
  implicit none
  character(len=1), intent(in) :: A_char
  select case(A_char)
     case('A')
        A_num = 0_ki1
     case('B')
        A_num = 1_ki1
     case default
        A_num = 9_ki1
  end select
end function changeNum

subroutine generate_output_filename(GenoFileName)
  implicit none
  character(len=*), intent(out) :: GenoFileName
  character(len=10) :: date_str
  character(len=5) :: time_str
  integer :: values(8)
  integer :: seq_num
  character(len=MAX_STR) :: test_filename
  logical :: file_exists
  
  ! Get current date and time
  call date_and_time(values=values)
  
  ! Format as YYYYMMDD
  write(date_str, '(I4.4,I2.2,I2.2)') values(1), values(2), values(3)
  
  ! Try sequence numbers from 00 to 99
  do seq_num = 0, 99
     write(test_filename, '(A,A,A,A,I2.2,A)') &
        trim(OutputPrefix), '_', trim(date_str), '_', seq_num, '.geno'
     inquire(file=trim(test_filename), exist=file_exists)
     if (.not. file_exists) then
        GenoFileName = trim(test_filename)
        return
     end if
  end do
  
  ! If all numbered files exist (rare), use default
  write(GenoFileName, '(A,A,A,A)') &
     trim(OutputPrefix), '_', trim(date_str), '_99.geno'
  
end subroutine generate_output_filename

end program ReadFR
