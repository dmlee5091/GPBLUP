module M_readpar
use M_kinds
use M_Variables
use M_StrEdit
use M_ReadFile
implicit none
  LOGICAL,parameter:: TRUE=.TRUE., FALSE=.FALSE., Desc=.true., Ordered=.true.
  
  ! Public interface  
  public :: M_readpar_get_thresholds
  
CONTAINS

  subroutine M_readpar_get_thresholds(min_gc, min_r, max_r, min_gt, min_cluster, min_callrate)
    real, intent(out) :: min_gc, min_r, max_r, min_gt, min_cluster, min_callrate
    min_gc = QCThresholds%min_gc_score
    min_r = QCThresholds%min_r_intensity
    max_r = QCThresholds%max_r_intensity
    min_gt = QCThresholds%min_gt_score
    min_cluster = QCThresholds%min_cluster_sep
    min_callrate = QCThresholds%min_call_rate
  end subroutine M_readpar_get_thresholds

  subroutine read_parameters(Par_file)
 !   use M_Variables, only: PEDFile, DATAFile, SNPFile, MAPFile, MAX_STR, MAX_VAR
  character(len=*), intent(in) :: Par_file  
  character(LEN=MAX_STR) :: XC(MAX_VAR), keyword_upper, ParDir
  integer(kind=ki4) :: XI(MAX_VAR)
  integer:: n, unitF, ierr
  
  ! 파라미터 파일의 디렉토리 추출
  ParDir = get_directory_path(Par_file)
  
  unitF=fopen(Par_file)

  do
    call readline(unitF,n,XC,XI)
     if(n < 0) exit
     ! Convert keyword to uppercase for case-insensitive matching
     keyword_upper = to_upper(trim(adjustl(XC(1))))
     select case(keyword_upper)
       case(CHAR(32)) ! skip empty lines
           cycle
       case("PEDFILE:")
           PEDFile%FileName=XC(2)
           PEDFile%FileName = resolve_file_path(ParDir, PEDFile%FileName)
           call readFileInfo(unitF, PEDFile, ierr)
           if (ierr /= 0) then
               print *, "ERROR: Failed to read PEDFILE info. Error code:", ierr
               call print_error_message(ierr)
               exit
           end if
       case("DATAFILE:")
           DATAFile%FileName=XC(2)
           DATAFile%FileName = resolve_file_path(ParDir, DATAFile%FileName)
           call readFileInfo(unitF, DATAFile, ierr)
           if (ierr /= 0) then
               print *, "ERROR: Failed to read DATAFILE info. Error code:", ierr
               call print_error_message(ierr)
               exit
           end if
       case("SNPFILE:")
           SNPFile%FileName=XC(2)
           SNPFile%FileName = resolve_file_path(ParDir, SNPFile%FileName)
           call readFileInfo(unitF, SNPFile, ierr)
           if (ierr /= 0) then
               print *, "ERROR: Failed to read SNPFILE info. Error code:", ierr
               call print_error_message(ierr)
               exit
           end if
       case("MAPFILE:")
           MAPFile%FileName=XC(2)
           MAPFile%FileName = resolve_file_path(ParDir, MAPFile%FileName)
           call readFileInfo(unitF, MAPFile, ierr)
           if (ierr /= 0) then
               print *, "ERROR: Failed to read MAPFILE info. Error code:", ierr
               call print_error_message(ierr)
               exit
           end if
       case("OUTPUTPREFIX:")
           OutputPrefix = trim(adjustl(XC(2)))  ! Keep as-is for file naming

       case default
            ! Skip unknown keywords silently (might be comments or future extensions)
            continue
     end select
  enddo
  close(unit=unitF)
 end subroutine

subroutine readFileInfo(unit, info, ierr)
    integer, intent(in) :: unit
    type(FileInfo), intent(inout) :: info
    integer, intent(out), optional :: ierr
    character(LEN=MAX_STR) :: XC(MAX_VAR), keyword_upper
    integer(kind=ki4) :: XI(MAX_VAR)
    integer :: n, i
    ! Default values
    info%Delim_char = ' '
    info%Header = 0
    XC = char(32)
    XI = 0
    if (present(ierr)) ierr = 0

    call readline(unit,n,XC,XI)
    keyword_upper = to_upper(trim(adjustl(XC(1))))
    if (keyword_upper=="HEADER:") info%Header = XI(2)
    
    call readline(unit,n,XC,XI)
    keyword_upper = to_upper(trim(adjustl(XC(1))))
    if (keyword_upper=="DELIM:") info%Delim_char = trim(adjustl(XC(2)))
    
    call readline(unit,n,XC,XI)
    keyword_upper = to_upper(trim(adjustl(XC(1))))
    if (keyword_upper=="NO_VARIABLES:") then
        info%NVAR = XI(2)
        if (info%NVAR <= 0) then
            if (present(ierr)) ierr = 2
            return
        else
           ! 데이터 읽기
           do i=1, info%NVAR
                call readline(unit,n,XC,XI)
                info%FieldLoc(i) = XI(1)
                ! Store field names preserving original case (case-insensitive matching handled in find_field_index)
                info%FieldName(i) = trim(adjustl(XC(2)))
                
                ! QC threshold 값이 있는 경우 처리
                if (n >= 3 .and. len_trim(XC(3)) > 0) then
                    call parse_qc_thresholds(to_upper(trim(adjustl(XC(2)))), XC(3), XC(4))
                end if
           enddo
        end if
    end if
   return
  end subroutine readFileInfo

  integer function FindPos(str,Mstr)
 character(len=*):: str
 character(len=*):: Mstr(:)
 integer:: k
 do k=1,size(Mstr)
    if(trim(str).eq.trim(Mstr(k))) then
       FindPos=k
       return
    endif
 enddo
 FindPos=0
 end function

  subroutine print_error_message(err_code)
  integer, intent(in) :: err_code
  select case(err_code)
      case(1)
          print *, "  -> First line read failed (EOF or I/O error)"
      case(2)
          print *, "  -> NO_VARIABLES value is invalid or zero"
      case(3)
          print *, "  -> Memory allocation failed for field arrays"
      case(4)
          print *, "  -> Field definition line read failed"
      case(5)
          print *, "  -> NO_VARIABLES keyword missing from input"
      case default
          print *, "  -> Unknown error"
  end select
  end subroutine print_error_message
  subroutine parse_qc_thresholds(field_name, val1_str, val2_str)
    character(len=*), intent(in) :: field_name, val1_str, val2_str
    real :: val1, val2
    integer :: stat
    
    read(val1_str, *, iostat=stat) val1
    if (stat /= 0) return
    
    select case(to_upper(trim(adjustl(field_name))))
        case("R_INTENSITY")
            QCThresholds%min_r_intensity = val1
            if (len_trim(val2_str) > 0) then
                read(val2_str, *, iostat=stat) val2
                if (stat == 0) QCThresholds%max_r_intensity = val2
            end if
        case("GC_SCORE")
            QCThresholds%min_gc_score = val1
        case("GT_SCORE")
            QCThresholds%min_gt_score = val1
        case("CLUSTER_SEP")
            QCThresholds%min_cluster_sep = val1
        case("CALL_RATE")
            QCThresholds%min_call_rate = val1
    end select
  end subroutine parse_qc_thresholds


end module

