module M_Variables
use M_Kinds
implicit none   
! define VARIABLE for PROGRAMS
INTEGER,PARAMETER,PRIVATE:: lock_y=2028, lock_m=12, lock_d=31
INTEGER, PARAMETER, PUBLIC:: missI = 0
INTEGER, PARAMETER, PUBLIC:: ZERO=missI
INTEGER, PARAMETER, PUBLIC:: LEN_KEY = 32
INTEGER, PARAMETER, PUBLIC:: LEN_STR = LEN_KEY
INTEGER, PARAMETER, PUBLIC:: MAX_STR = 128
INTEGER, PARAMETER, PUBLIC:: MAX_VAR = MAX_STR
INTEGER, PARAMETER, PUBLIC:: MAX_RECL = 512
REAL, PARAMETER, PUBLIC:: missR = 0.d0
REAL,PUBLIC:: STARTIME, FINISHTIME
integer, parameter, PUBLIC:: UNKNOWN=0,MALE=2,FEMALE=1
CHARACTER(LEN=MAX_RECL), PUBLIC :: RECORD
CHARACTER(len=1), PARAMETER, PUBLIC:: missC = achar(32)  ! " "
CHARACTER(len=1), PARAMETER, PUBLIC:: nullC = missC      ! " "
CHARACTER(len=1), PARAMETER, PUBLIC:: SPACE = missC      ! " "
CHARACTER(len=1), PARAMETER, PUBLIC:: missA = achar(48)  ! "0"
CHARACTER(len=1), PARAMETER, PUBLIC:: TAB = achar(9)

type, PUBLIC :: FileInfo
    character(len=MAX_STR) :: FileName
    character(len=MAX_STR) :: FieldName(MAX_VAR)
    integer :: FieldLoc(MAX_VAR)
    character(len=LEN_STR) :: Delim_char
    integer :: Header
    integer :: NVAR
end type FileInfo

type, PUBLIC :: SNPInfo
    character(len=LEN_STR) :: SNP_ID
    integer :: Chr
    integer :: Pos
    integer :: Array_All
    integer :: Array_Chr
end type SNPInfo

type, PUBLIC :: PEDInfo
    character(len=LEN_STR) :: BREED
    character(len=LEN_STR) :: ID
    character(len=LEN_STR) :: ARN
    character(len=LEN_STR) :: SIRE
    character(len=LEN_STR) :: DAM
    integer :: SEX
    integer :: BDate
    character(len=LEN_STR) :: LOC
end type PEDInfo

! =========================================================================
! QC 데이터 구조체 (SNP 품질 관리용)
! =========================================================================
type, PUBLIC :: QC_Metrics
   real :: gc_score              ! GC Score value
   real :: r_value               ! R - Intensity value
   real :: gt_score              ! GT Score value
   real :: cluster_sep           ! Cluster Separation value
   logical :: pass_qc            ! QC pass/fail result
end type QC_Metrics

type, PUBLIC :: QC_Counters
   integer :: fail_gc            ! GC Score failures
   integer :: fail_r             ! R-Intensity failures
   integer :: fail_gt            ! GT Score failures
   integer :: fail_cluster       ! Cluster Separation failures
   integer :: fail_allele        ! Missing allele failures
   integer :: fail_chr           ! Chromosome filter failures
end type QC_Counters

! =========================================================================
! QC Threshold 설정 (Parameter 파일에서 읽어옴)
! =========================================================================
type, PUBLIC :: QC_Thresholds
   real :: min_gc_score          ! Minimum GC Score (default: 0.65)
   real :: min_r_intensity       ! Minimum R-Intensity (default: 0.4)
   real :: max_r_intensity       ! Maximum R-Intensity (default: 2.0)
   real :: min_gt_score          ! Minimum GT Score (default: 0.50)
   real :: min_cluster_sep       ! Minimum Cluster Separation (default: 0.30)
   real :: min_call_rate         ! Minimum Call Rate (default: 0.70)
end type QC_Thresholds

! Module variables declaration with PUBLIC attribute
type(FileInfo), public :: PEDFile, DATAFile, SNPFile, MAPFile
type(QC_Thresholds), public :: QCThresholds
character(len=MAX_STR), public :: OutputPrefix = "output"  ! Default output prefix

! Public subroutines
public :: init_File, init_Ped, init_QC_Thresholds, get_QC_Threshold_values

contains

subroutine init_File(File)
    type(FileInfo), intent(out) :: File
    File%FileName = ''
    File%FieldName = ''
    File%FieldLoc = 0
    File%Delim_char = ''
    File%Header = 0
    File%NVAR = 0
end subroutine init_File

subroutine init_Ped(ped)
    type(PEDInfo), intent(out) :: ped
    ped%BREED = ''
    ped%ID = ''
    ped%ARN = ''
    ped%SIRE = ''
    ped%DAM = ''
    ped%SEX = 0
    ped%BDate = 0
    ped%LOC = ''
end subroutine init_Ped

subroutine init_QC_Thresholds(thresholds)
    type(QC_Thresholds), intent(out) :: thresholds
    ! Default values (empirical thresholds)
    thresholds%min_gc_score = 0.65
    thresholds%min_r_intensity = 0.4
    thresholds%max_r_intensity = 2.0
    thresholds%min_gt_score = 0.50
    thresholds%min_cluster_sep = 0.30
    thresholds%min_call_rate = 0.70
end subroutine init_QC_Thresholds

! Helper for ReadFR to access module QCThresholds values
subroutine get_QC_Threshold_values(min_gc, min_r, max_r, min_gt, min_cluster, min_callrate)
    real, intent(out) :: min_gc, min_r, max_r, min_gt, min_cluster, min_callrate
    min_gc = QCThresholds%min_gc_score
    min_r = QCThresholds%min_r_intensity
    max_r = QCThresholds%max_r_intensity
    min_gt = QCThresholds%min_gt_score
    min_cluster = QCThresholds%min_cluster_sep
    min_callrate = QCThresholds%min_call_rate
end subroutine get_QC_Threshold_values

end module M_Variables

