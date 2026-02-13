module M_HashTable
  use M_Kinds
  use M_Variables
  implicit none
  
  ! 해시 테이블 항목 구조
  type, public :: HashNode
    character(len=MAX_STR) :: key      ! 키 (숫자 문자열)
    integer :: value                   ! 값 (인덱스 등)
    type(HashNode), pointer :: next => null()  ! 체이닝용
  end type HashNode
  
  ! 해시 테이블 구조
  type, public :: HashTable
    type(HashNode), pointer :: table(:) => null()
    integer :: size = 0
    integer :: count = 0  ! 저장된 항목 수
  end type HashTable
  
  public :: ht_create, ht_insert, ht_search, ht_delete, ht_free, hash_numeric_string
  
contains

  !**********************************************************************
  ! 해시 함수: 숫자 문자열에 최적화
  ! 각 숫자를 가중치와 함께 더함
  !**********************************************************************
  integer function hash_numeric_string(key, table_size) result(hash_val)
    character(len=*), intent(in) :: key
    integer, intent(in) :: table_size
    integer :: i, digit
    integer(kind=8) :: hash
    integer(kind=8), parameter :: BASE = 31_8
    
    hash = 0_8
    do i = 1, len_trim(key)
      if (key(i:i) >= '0' .and. key(i:i) <= '9') then
        digit = ichar(key(i:i)) - ichar('0')
        hash = mod(hash * BASE + digit, int(table_size, 8))
      end if
    end do
    
    hash_val = int(mod(hash, int(table_size, 8)))
    if (hash_val < 0) hash_val = abs(hash_val)
  end function hash_numeric_string

  !**********************************************************************
  ! 해시 테이블 생성
  ! Arguments:
  !   ht          - (out) 해시 테이블 구조
  !   table_size  - (in)  테이블 크기 (소수 권장: 101, 503, 1009, 10007, 100003)
  !**********************************************************************
  subroutine ht_create(ht, table_size)
    type(HashTable), intent(out) :: ht
    integer, intent(in) :: table_size
    integer :: i
    
    allocate(ht%table(0:table_size-1))
    ht%size = table_size
    ht%count = 0
    
    do i = 0, table_size - 1
      nullify(ht%table(i)%next)
    end do
  end subroutine ht_create

  !**********************************************************************
  ! 해시 테이블에 키-값 쌍 삽입
  ! Arguments:
  !   ht    - (inout) 해시 테이블
  !   key   - (in)    키 (숫자 문자열)
  !   value - (in)    값 (정수 인덱스)
  !**********************************************************************
  subroutine ht_insert(ht, key, value)
    type(HashTable), intent(inout) :: ht
    character(len=*), intent(in) :: key
    integer, intent(in) :: value
    integer :: hash_idx
    type(HashNode), pointer :: node, current
    
    hash_idx = hash_numeric_string(key, ht%size)
    
    ! 이미 존재하는 키인 경우 값 업데이트
    current => ht%table(hash_idx)
    do while (associated(current))
      if (current%key == trim(key)) then
        current%value = value
        return
      end if
      if (.not. associated(current%next)) exit
      current => current%next
    end do
    
    ! 새로운 노드 생성
    allocate(node)
    node%key = trim(key)
    node%value = value
    node%next => ht%table(hash_idx)%next
    ht%table(hash_idx)%next => node
    ht%count = ht%count + 1
  end subroutine ht_insert

  !**********************************************************************
  ! 해시 테이블에서 키 검색
  ! Arguments:
  !   ht    - (in)  해시 테이블
  !   key   - (in)  검색 키 (숫자 문자열)
  !   value - (out) 찾은 값 (찾지 못하면 -1)
  ! Returns:
  !   .true.  - 키 찾음
  !   .false. - 키 못 찾음
  !**********************************************************************
  logical function ht_search(ht, key, value) result(found)
    type(HashTable), intent(in) :: ht
    character(len=*), intent(in) :: key
    integer, intent(out) :: value
    integer :: hash_idx
    type(HashNode), pointer :: current
    
    found = .false.
    value = -1
    
    if (.not. associated(ht%table)) return
    
    hash_idx = hash_numeric_string(key, ht%size)
    
    current => ht%table(hash_idx)%next
    do while (associated(current))
      if (trim(current%key) == trim(key)) then
        value = current%value
        found = .true.
        return
      end if
      current => current%next
    end do
  end function ht_search

  !**********************************************************************
  ! 해시 테이블에서 키 삭제
  ! Arguments:
  !   ht  - (inout) 해시 테이블
  !   key - (in)    삭제할 키
  ! Returns:
  !   .true.  - 삭제 성공
  !   .false. - 키 없음
  !**********************************************************************
  logical function ht_delete(ht, key) result(deleted)
    type(HashTable), intent(inout) :: ht
    character(len=*), intent(in) :: key
    integer :: hash_idx
    type(HashNode), pointer :: current, prev
    
    deleted = .false.
    
    if (.not. associated(ht%table)) return
    
    hash_idx = hash_numeric_string(key, ht%size)
    prev => ht%table(hash_idx)
    current => ht%table(hash_idx)%next
    
    do while (associated(current))
      if (trim(current%key) == trim(key)) then
        prev%next => current%next
        deallocate(current)
        ht%count = ht%count - 1
        deleted = .true.
        return
      end if
      prev => current
      current => current%next
    end do
  end function ht_delete

  !**********************************************************************
  ! 해시 테이블 메모리 해제
  ! Arguments:
  !   ht - (inout) 해시 테이블
  !**********************************************************************
  subroutine ht_free(ht)
    type(HashTable), intent(inout) :: ht
    integer :: i
    type(HashNode), pointer :: current, next_node
    
    if (.not. associated(ht%table)) return
    
    do i = 0, ht%size - 1
      current => ht%table(i)%next
      do while (associated(current))
        next_node => current%next
        deallocate(current)
        current => next_node
      end do
      nullify(ht%table(i)%next)
    end do
    
    deallocate(ht%table)
    ht%size = 0
    ht%count = 0
  end subroutine ht_free

  !**********************************************************************
  ! 해시 테이블 통계 출력
  !**********************************************************************
  subroutine ht_print_stats(ht)
    type(HashTable), intent(in) :: ht
    integer :: i, chain_len, max_chain, total_chain
    type(HashNode), pointer :: current
    real :: load_factor, avg_chain
    
    if (.not. associated(ht%table)) then
      print *, "테이블이 생성되지 않았습니다"
      return
    end if
    
    max_chain = 0
    total_chain = 0
    
    do i = 0, ht%size - 1
      chain_len = 0
      current => ht%table(i)%next
      do while (associated(current))
        chain_len = chain_len + 1
        current => current%next
      end do
      if (chain_len > max_chain) max_chain = chain_len
      total_chain = total_chain + chain_len
    end do
    
    load_factor = real(ht%count) / real(ht%size)
    avg_chain = real(total_chain) / real(ht%size)
    
    print *, "===== 해시 테이블 통계 ====="
    print '(A,I10)', "테이블 크기: ", ht%size
    print '(A,I10)', "저장된 항목: ", ht%count
    print '(A,F8.4)', "로드 팩터: ", load_factor
    print '(A,F8.4)', "평균 체인 길이: ", avg_chain
    print '(A,I10)', "최대 체인 길이: ", max_chain
  end subroutine ht_print_stats

end module M_HashTable
