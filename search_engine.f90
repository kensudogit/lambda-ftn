! 検索エンジンモジュール for Fortran Lambda
! 外部サイトとS3コンテンツの検索機能を提供

module search_engine
    use json_utils
    implicit none
    
    private
    public :: search_content, check_sites_health, initialize_search_engine
    public :: search_result, external_site, search_engine_state
    
    ! 検索結果構造
    type :: search_result
        character(len=256) :: page_id      ! ページID
        character(len=512) :: title        ! タイトル
        character(len=2048) :: content    ! コンテンツ
        character(len=512) :: url         ! URL
        character(len=64) :: last_updated ! 最終更新日
        real :: score                     ! 検索スコア
        character(len=64) :: site_name    ! サイト名
    end type search_result
    
    ! 外部サイト構造
    type :: external_site
        character(len=64) :: name           ! サイト名
        character(len=256) :: bucket       ! S3バケット名
        character(len=256) :: prefix       ! コンテンツプレフィックス
        character(len=256) :: index_prefix ! インデックスプレフィックス
        logical :: is_healthy              ! 健全性フラグ
    end type external_site
    
    ! 検索エンジン状態
    type :: search_engine_state
        type(external_site), allocatable :: sites(:) ! サイト配列
        integer :: num_sites                          ! サイト数
        logical :: initialized                        ! 初期化フラグ
        character(len=256) :: content_bucket          ! コンテンツバケット
    end type search_engine_state
    
    ! グローバル検索エンジンインスタンス
    type(search_engine_state) :: global_search_engine
    
contains
    
    ! 検索エンジンの初期化
    subroutine initialize_search_engine(content_bucket)
        character(len=*), intent(in) :: content_bucket
        
        global_search_engine%content_bucket = content_bucket
        global_search_engine%num_sites = 3
        global_search_engine%initialized = .true.
        
        ! サイトの割り当てと初期化
        allocate(global_search_engine%sites(global_search_engine%num_sites))
        
        ! 情報サイト
        global_search_engine%sites(1)%name = "information"
        global_search_engine%sites(1)%bucket = content_bucket
        global_search_engine%sites(1)%prefix = "information/information/"
        global_search_engine%sites(1)%index_prefix = "information/index/"
        global_search_engine%sites(1)%is_healthy = .true.
        
        ! SLPサイト
        global_search_engine%sites(2)%name = "slp"
        global_search_engine%sites(2)%bucket = content_bucket
        global_search_engine%sites(2)%prefix = "slp/"
        global_search_engine%sites(2)%index_prefix = "slp/index/"
        global_search_engine%sites(2)%is_healthy = .true.
        
        ! ツールサイト
        global_search_engine%sites(3)%name = "tools"
        global_search_engine%sites(3)%bucket = content_bucket
        global_search_engine%sites(3)%prefix = "tools/"
        global_search_engine%sites(3)%index_prefix = "tools/index/"
        global_search_engine%sites(3)%is_healthy = .true.
        
    end subroutine initialize_search_engine
    
    ! 全外部サイトの健全性をチェック
    function check_sites_health() result(all_healthy)
        logical :: all_healthy
        integer :: i
        
        all_healthy = .true.
        
        ! 実際の実装ではS3バケットの接続性をチェック
        ! 現在は全サイトが健全と仮定
        do i = 1, global_search_engine%num_sites
            global_search_engine%sites(i)%is_healthy = .true.
        end do
        
    end function check_sites_health
    
    ! 全サイトでコンテンツを検索
    subroutine search_content(keyword, results, num_results)
        character(len=*), intent(in) :: keyword
        type(search_result), allocatable, intent(out) :: results(:)
        integer, intent(out) :: num_results
        
        integer :: i, site_results
        type(search_result), allocatable :: temp_results(:)
        type(search_result), allocatable :: site_search_results(:)
        
        num_results = 0
        
        ! 各サイトを検索
        do i = 1, global_search_engine%num_sites
            if (global_search_engine%sites(i)%is_healthy) then
                call search_site_content(global_search_engine%sites(i), keyword, &
                                       site_search_results, site_results)
                
                if (site_results > 0) then
                    ! 新しい結果に対応するため結果配列を再割り当て
                    if (allocated(temp_results)) then
                        deallocate(temp_results)
                    end if
                    
                    allocate(temp_results(num_results + site_results))
                    
                    ! 既存の結果をコピー
                    if (num_results > 0) then
                        temp_results(1:num_results) = results(1:num_results)
                    end if
                    
                    ! 新しい結果を追加
                    temp_results(num_results + 1:num_results + site_results) = &
                        site_search_results(1:site_results)
                    
                    ! 結果配列を更新
                    if (allocated(results)) then
                        deallocate(results)
                    end if
                    
                    allocate(results(num_results + site_results))
                    results = temp_results
                    num_results = num_results + site_results
                    
                    deallocate(temp_results)
                end if
                
                if (allocated(site_search_results)) then
                    deallocate(site_search_results)
                end if
            end if
        end do
        
    end subroutine search_content
    
    ! Search content within a specific site
    subroutine search_site_content(site, keyword, results, num_results)
        type(external_site), intent(in) :: site
        character(len=*), intent(in) :: keyword
        type(search_result), allocatable, intent(out) :: results(:)
        integer, intent(out) :: num_results
        
        character(len=1024), allocatable :: index_matches(:)
        integer :: num_matches
        
        num_results = 0
        
        ! Search index files for matches
        call search_index_files(site, keyword, index_matches, num_matches)
        
        if (num_matches > 0) then
            ! Fetch content for matched pages
            call fetch_matched_contents(site, index_matches, num_matches, &
                                     results, num_results)
            
            deallocate(index_matches)
        end if
        
    end subroutine search_site_content
    
    ! Search index files for keyword matches
    subroutine search_index_files(site, keyword, matches, num_matches)
        type(external_site), intent(in) :: site
        character(len=*), intent(in) :: keyword
        character(len=1024), allocatable, intent(out) :: matches(:)
        integer, intent(out) :: num_matches
        
        ! In a real implementation, this would:
        ! 1. List S3 objects with the index prefix
        ! 2. Read each index file
        ! 3. Search for keyword matches
        ! 4. Return matching page IDs
        
        ! For now, we'll create some dummy matches
        num_matches = 2
        allocate(matches(num_matches))
        matches(1) = "page_001"
        matches(2) = "page_002"
        
    end subroutine search_index_files
    
    ! Fetch content for matched pages
    subroutine fetch_matched_contents(site, page_ids, num_pages, results, num_results)
        type(external_site), intent(in) :: site
        character(len=*), dimension(:), intent(in) :: page_ids
        integer, intent(in) :: num_pages
        type(search_result), allocatable, intent(out) :: results(:)
        integer, intent(out) :: num_results
        
        integer :: i
        
        num_results = num_pages
        allocate(results(num_results))
        
        ! In a real implementation, this would:
        ! 1. Read content files from S3 for each page ID
        ! 2. Parse the content and extract metadata
        ! 3. Calculate relevance scores
        
        ! For now, we'll create dummy results
        do i = 1, num_pages
            results(i)%page_id = page_ids(i)
            results(i)%title = "Sample Title " // trim(page_ids(i))
            results(i)%content = "This is sample content for " // trim(page_ids(i))
            results(i)%url = "https://example.com/" // trim(page_ids(i))
            results(i)%last_updated = "2024-01-01"
            results(i)%score = 0.8
            results(i)%site_name = site%name
        end do
        
    end subroutine fetch_matched_contents
    
    ! Calculate search relevance score
    function calculate_relevance_score(content, keyword) result(score)
        character(len=*), intent(in) :: content
        character(len=*), intent(in) :: keyword
        real :: score
        
        integer :: keyword_count, content_length
        real :: keyword_density
        
        ! Simple relevance scoring based on keyword frequency
        keyword_count = count_keyword_occurrences(content, keyword)
        content_length = len_trim(content)
        
        if (content_length > 0) then
            keyword_density = real(keyword_count) / real(content_length)
            score = min(1.0, keyword_density * 1000.0)  ! Scale to 0-1 range
        else
            score = 0.0
        end if
        
    end function calculate_relevance_score
    
    ! Count keyword occurrences in content
    function count_keyword_occurrences(content, keyword) result(count)
        character(len=*), intent(in) :: content
        character(len=*), intent(in) :: keyword
        integer :: count
        
        integer :: pos, keyword_len, content_len
        character(len=len(content)) :: upper_content
        character(len=len(keyword)) :: upper_keyword
        
        count = 0
        pos = 1
        keyword_len = len_trim(keyword)
        content_len = len_trim(content)
        
        ! Convert to uppercase for case-insensitive search
        upper_content = content
        upper_keyword = keyword
        call to_upper(upper_content)
        call to_upper(upper_keyword)
        
        ! Count occurrences
        do while (pos <= content_len - keyword_len + 1)
            if (upper_content(pos:pos + keyword_len - 1) == upper_keyword) then
                count = count + 1
            end if
            pos = pos + 1
        end do
        
    end function count_keyword_occurrences
    
    ! Convert string to uppercase
    subroutine to_upper(str)
        character(len=*), intent(inout) :: str
        
        integer :: i
        
        do i = 1, len_trim(str)
            if (str(i:i) >= 'a' .and. str(i:i) <= 'z') then
                str(i:i) = char(ichar(str(i:i)) - 32)
            end if
        end do
        
    end subroutine to_upper
    
    ! Sort search results by relevance score
    subroutine sort_search_results(results, num_results)
        type(search_result), dimension(:), intent(inout) :: results
        integer, intent(in) :: num_results
        
        integer :: i, j
        type(search_result) :: temp
        
        ! Simple bubble sort by score (descending)
        do i = 1, num_results - 1
            do j = 1, num_results - i
                if (results(j)%score < results(j + 1)%score) then
                    temp = results(j)
                    results(j) = results(j + 1)
                    results(j + 1) = temp
                end if
            end do
        end do
        
    end subroutine sort_search_results
    
    ! Cleanup search engine resources
    subroutine cleanup_search_engine()
        if (allocated(global_search_engine%sites)) then
            deallocate(global_search_engine%sites)
        end if
        global_search_engine%initialized = .false.
    end subroutine cleanup_search_engine
    
end module search_engine
