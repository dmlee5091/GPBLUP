module M_PEDHashTable
  use M_Kinds
  use M_Variables
  implicit none
  
  ! PEDInfo를 저장하는 해시 테이블 노드
  type, public :: PEDHashNode
    character(len=LEN_STR) :: ARN_key   ! ARN을 키로 사용 (문자열)
    type(PEDInfo) :: ped_data           ! 실제 PED 정보
    type(PEDHashNode), pointer :: next => null()
  end type PEDHashNode
  
  ! PEDInfo 해시 테이블
  type, public :: PEDHashTable
    type(PEDHashNode), pointer :: table(:) => null()
    integer :: size = 0
    integer :: count = 0
  end type PEDHashTable
  
  public :: pht_create, pht_insert, pht_search, pht_delete, pht_free
  public :: pht_print_stats, pht_get_prime_size
  
contains

  !**********************************************************************
  ! 소수 계산 - 해시 테이블 크기로 적합
  ! 작은 데이터셋: 101, 소수 계산 요청 가능
  !**********************************************************************
  integer function pht_get_prime_size(min_size) result(prime)
    integer, intent(in) :: min_size
    integer :: candidate, i, is_prime
    
    candidate = max(101, min_size)
    if (mod(candidate, 2) == 0) candidate = candidate + 1
    
    do while (.true.)
      is_prime = 1
      do i = 3, int(sqrt(real(candidate))) + 1, 2
        if (mod(candidate, i) == 0) then
          is_prime = 0
          exit
        end if
      end do
      
      if (is_prime == 1) then
        prime = candidate
        return
      end if
      candidate = candidate + 2
    end do
  end function pht_get_prime_size

  !**********************************************************************
  ! 해시 함수: ARN(문자열)을 이용한 해싱
  !**********************************************************************
  integer function hash_arn(arn_key, table_size) result(hash_val)
    character(len=*), intent(in) :: arn_key
    integer, intent(in) :: table_size
    integer :: i
    integer(kind=ki8) :: hash_i8
    
    if (len_trim(arn_key) == 0) then
      hash_val = 0
      return
    end if

    hash_i8 = 0_ki8
    do i = 1, len_trim(arn_key)
      hash_i8 = mod(hash_i8 * 31_ki8 + int(iachar(arn_key(i:i)), ki8), int(table_size, ki8))
    end do
    hash_val = int(hash_i8)
  end function hash_arn

  !**********************************************************************
  ! PED 해시 테이블 생성
  ! Arguments:
  !   pht         - (out) PED 해시 테이블
  !   table_size  - (in)  테이블 크기 (소수 사용, 자동 조정)
  !**********************************************************************
  subroutine pht_create(pht, table_size)
    type(PEDHashTable), intent(out) :: pht
    integer, intent(in) :: table_size
    integer :: i, actual_size
    
    ! 소수 크기로 자동 조정
    actual_size = pht_get_prime_size(table_size)
    
    allocate(pht%table(0:actual_size-1))
    pht%size = actual_size
    pht%count = 0
    
    do i = 0, actual_size - 1
      nullify(pht%table(i)%next)
    end do
    
    print '(A,I10,A,I10)', "PED 해시 테이블 생성: 요청 크기=", table_size, &
                           " 실제 크기(소수)=", actual_size
  end subroutine pht_create

  !**********************************************************************
  ! PED 정보를 해시 테이블에 삽입
  ! Arguments:
  !   pht  - (inout) PED 해시 테이블
  !   ped  - (in)    PED 정보 (ARN이 키)
  !**********************************************************************
  subroutine pht_insert(pht, ped)
    type(PEDHashTable), intent(inout) :: pht
    type(PEDInfo), intent(in) :: ped
    integer :: hash_idx
    type(PEDHashNode), pointer :: node, current
    
    hash_idx = hash_arn(trim(ped%ARN), pht%size)
    
    ! 같은 ARN이 이미 존재하면 업데이트
    current => pht%table(hash_idx)%next
    do while (associated(current))
      if (trim(current%ARN_key) == trim(ped%ARN)) then
        current%ped_data = ped
        return
      end if
      if (.not. associated(current%next)) exit
      current => current%next
    end do
    
    ! 새로운 노드 추가
    allocate(node)
    node%ARN_key = trim(ped%ARN)
    node%ped_data = ped
    node%next => pht%table(hash_idx)%next
    pht%table(hash_idx)%next => node
    pht%count = pht%count + 1
  end subroutine pht_insert

  !**********************************************************************
  ! ARN을 키로 하여 PED 정보 검색
  ! Arguments:
  !   pht     - (in)  PED 해시 테이블
  !   arn_key - (in)  검색할 ARN
  !   ped     - (out) 찾은 PED 정보
  ! Returns:
  !   .true.  - 찾음
  !   .false. - 못 찾음
  !**********************************************************************
  logical function pht_search(pht, arn_key, ped) result(found)
    type(PEDHashTable), intent(in) :: pht
    character(len=*), intent(in) :: arn_key
    type(PEDInfo), intent(out) :: ped
    integer :: hash_idx
    type(PEDHashNode), pointer :: current
    
    found = .false.
    
    if (.not. associated(pht%table)) return
    
    hash_idx = hash_arn(trim(arn_key), pht%size)
    
    current => pht%table(hash_idx)%next
    do while (associated(current))
      if (trim(current%ARN_key) == trim(arn_key)) then
        ped = current%ped_data
        found = .true.
        return
      end if
      current => current%next
    end do
  end function pht_search

  !**********************************************************************
  ! ARN을 키로 하여 PED 정보 삭제
  ! Arguments:
  !   pht     - (inout) PED 해시 테이블
  !   arn_key - (in)    삭제할 ARN
  ! Returns:
  !   .true.  - 삭제 성공
  !   .false. - 키 없음
  !**********************************************************************
  logical function pht_delete(pht, arn_key) result(deleted)
    type(PEDHashTable), intent(inout) :: pht
    character(len=*), intent(in) :: arn_key
    integer :: hash_idx
    type(PEDHashNode), pointer :: current, prev
    
    deleted = .false.
    
    if (.not. associated(pht%table)) return
    
    hash_idx = hash_arn(trim(arn_key), pht%size)
    prev => pht%table(hash_idx)
    current => pht%table(hash_idx)%next
    
    do while (associated(current))
      if (trim(current%ARN_key) == trim(arn_key)) then
        prev%next => current%next
        deallocate(current)
        pht%count = pht%count - 1
        deleted = .true.
        return
      end if
      prev => current
      current => current%next
    end do
  end function pht_delete

  !**********************************************************************
  ! PED 해시 테이블 메모리 해제
  !**********************************************************************
  subroutine pht_free(pht)
    type(PEDHashTable), intent(inout) :: pht
    integer :: i
    type(PEDHashNode), pointer :: current, next_node
    
    if (.not. associated(pht%table)) return
    
    do i = 0, pht%size - 1
      current => pht%table(i)%next
      do while (associated(current))
        next_node => current%next
        deallocate(current)
        current => next_node
      end do
      nullify(pht%table(i)%next)
    end do
    
    deallocate(pht%table)
    pht%size = 0
    pht%count = 0
  end subroutine pht_free

  !**********************************************************************
  ! PED 해시 테이블 통계 출력
  !**********************************************************************
  subroutine pht_print_stats(pht)
    type(PEDHashTable), intent(in) :: pht
    integer :: i, chain_len, max_chain, total_chain
    type(PEDHashNode), pointer :: current
    real :: load_factor, avg_chain
    
    if (.not. associated(pht%table)) then
      print *, "테이블이 생성되지 않았습니다"
      return
    end if
    
    max_chain = 0
    total_chain = 0
    
    do i = 0, pht%size - 1
      chain_len = 0
      current => pht%table(i)%next
      do while (associated(current))
        chain_len = chain_len + 1
        current => current%next
      end do
      if (chain_len > max_chain) max_chain = chain_len
      total_chain = total_chain + chain_len
    end do
    
    load_factor = real(pht%count) / real(pht%size)
    avg_chain = real(total_chain) / real(pht%size)
    
    print *, ""
    print *, "===== PED 해시 테이블 통계 ====="
    print '(A,I10)', "테이블 크기: ", pht%size
    print '(A,I10)', "저장된 개체: ", pht%count
    print '(A,F10.4)', "로드 팩터: ", load_factor
    print '(A,F10.4)', "평균 체인 길이: ", avg_chain
    print '(A,I10)', "최대 체인 길이: ", max_chain
    print *, ""
  end subroutine pht_print_stats

  !**********************************************************************
  ! PED 정보 출력
  !**********************************************************************
  subroutine pht_print_ped_info(ped)
    type(PEDInfo), intent(in) :: ped
    
    print '(A,A,A,A10,A,A12,A,A12,A,A12,A,I3,A,I10,A,A12)', &
      "ARN:", trim(ped%ARN), " | ID:", trim(ped%ID), &
      " | BREED:", trim(ped%BREED), " | SIRE:", trim(ped%SIRE), &
      " | DAM:", trim(ped%DAM), " | SEX:", ped%SEX, &
      " | BDate:", ped%BDate, " | LOC:", trim(ped%LOC)
  end subroutine pht_print_ped_info

end module M_PEDHashTable
