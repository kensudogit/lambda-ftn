! データベース操作モジュール for Fortran Lambda
! データストレージと取得のためのDynamoDBライクな機能を提供

module database_ops
    use json_utils
    implicit none
    
    private
    public :: db_query, db_scan, db_get_item, db_put_item, db_delete_item
    public :: wp_post, tag_master_item, activity_report_item
    public :: initialize_database, cleanup_database
    
    ! WordPress投稿構造
    type :: wp_post
        integer :: ID                    ! 投稿ID
        character(len=512) :: post_title ! 投稿タイトル
        character(len=8192) :: post_content ! 投稿コンテンツ
        character(len=512) :: guid       ! GUID
        character(len=64) :: post_date  ! 投稿日
        character(len=64) :: post_status ! 投稿ステータス
        character(len=64) :: post_type  ! 投稿タイプ
    end type wp_post
    
    ! タグマスター項目構造
    type :: tag_master_item
        character(len=64) :: tag_code   ! タグコード
        character(len=256) :: tag_name  ! タグ名
        logical :: deleted_flg          ! 削除フラグ
        character(len=64) :: data_type  ! データタイプ
    end type tag_master_item
    
    ! アクティビティレポート項目構造
    type :: activity_report_item
        character(len=64) :: activity_report_id           ! アクティビティレポートID
        character(len=64) :: tag_code                    ! タグコード
        character(len=256) :: keyword                    ! キーワード
        logical :: delete_flag                           ! 削除フラグ
        character(len=1024), allocatable :: search_tags_list(:) ! 検索タグリスト
        integer :: num_tags                              ! タグ数
    end type activity_report_item
    
    ! データベーステーブル構造
    type :: db_table
        character(len=64) :: table_name   ! テーブル名
        character(len=64) :: primary_key  ! プライマリキー
        character(len=64) :: sort_key     ! ソートキー
        logical :: initialized            ! 初期化フラグ
        integer :: item_count            ! 項目数
    end type db_table
    
    ! グローバルデータベーステーブル
    type(db_table) :: posts_table           ! 投稿テーブル
    type(db_table) :: postmeta_table       ! 投稿メタテーブル
    type(db_table) :: tag_master_table     ! タグマスターテーブル
    type(db_table) :: activity_report_table ! アクティビティレポートテーブル
    type(db_table) :: user_master_table    ! ユーザーマスターテーブル
    
    ! デモ用のインメモリストレージ
    ! 実際の実装では実際のDynamoDBに接続
    type(wp_post), allocatable :: posts_data(:)           ! 投稿データ
    type(tag_master_item), allocatable :: tag_master_data(:) ! タグマスターデータ
    type(activity_report_item), allocatable :: activity_report_data(:) ! アクティビティレポートデータ
    
    integer :: posts_count = 0           ! 投稿数
    integer :: tag_master_count = 0     ! タグマスター数
    integer :: activity_report_count = 0 ! アクティビティレポート数
    
contains
    
    ! データベーステーブルの初期化
    subroutine initialize_database()
        ! 投稿テーブルの初期化
        posts_table%table_name = "wp_posts"
        posts_table%primary_key = "ID"
        posts_table%initialized = .true.
        posts_table%item_count = 0
        
        ! 投稿メタテーブルの初期化
        postmeta_table%table_name = "wp_postmeta"
        postmeta_table%primary_key = "meta_id"
        posts_table%initialized = .true.
        postmeta_table%item_count = 0
        
        ! タグマスターテーブルの初期化
        tag_master_table%table_name = "tag_master"
        tag_master_table%primary_key = "tag_code"
        tag_master_table%sort_key = "data_type"
        tag_master_table%initialized = .true.
        tag_master_table%item_count = 0
        
        ! アクティビティレポートテーブルの初期化
        activity_report_table%table_name = "activity_report"
        activity_report_table%primary_key = "activity_report_id"
        activity_report_table%initialized = .true.
        activity_report_table%item_count = 0
        
        ! ユーザーマスターテーブルの初期化
        user_master_table%table_name = "user_master"
        user_master_table%primary_key = "user_id"
        user_master_table%initialized = .true.
        user_master_table%item_count = 0
        
        ! サンプルデータの読み込み
        call load_sample_data()
        
    end subroutine initialize_database
    
    ! Load sample data for demonstration
    subroutine load_sample_data()
        ! Sample posts data
        posts_count = 3
        allocate(posts_data(posts_count))
        
        posts_data(1)%ID = 1
        posts_data(1)%post_title = "Sample Post 1"
        posts_data(1)%post_content = "This is the content of sample post 1"
        posts_data(1)%guid = "https://example.com/post1"
        posts_data(1)%post_date = "2024-01-01"
        posts_data(1)%post_status = "publish"
        posts_data(1)%post_type = "post"
        
        posts_data(2)%ID = 2
        posts_data(2)%post_title = "Sample Post 2"
        posts_data(2)%post_content = "This is the content of sample post 2"
        posts_data(2)%guid = "https://example.com/post2"
        posts_data(2)%post_date = "2024-01-02"
        posts_data(2)%post_status = "publish"
        posts_data(2)%post_type = "post"
        
        posts_data(3)%ID = 3
        posts_data(3)%post_title = "Sample Post 3"
        posts_data(3)%post_content = "This is the content of sample post 3"
        posts_data(3)%guid = "https://example.com/post3"
        posts_data(3)%post_date = "2024-01-03"
        posts_data(3)%post_status = "publish"
        posts_data(3)%post_type = "post"
        
        ! Sample tag master data
        tag_master_count = 2
        allocate(tag_master_data(tag_master_count))
        
        tag_master_data(1)%tag_code = "TAG001"
        tag_master_data(1)%tag_name = "Technology"
        tag_master_data(1)%deleted_flg = .false.
        tag_master_data(1)%data_type = "マスタ_タグ"
        
        tag_master_data(2)%tag_code = "TAG002"
        tag_master_data(2)%tag_name = "Business"
        tag_master_data(2)%deleted_flg = .false.
        tag_master_data(2)%data_type = "マスタ_タグ"
        
        ! Sample activity report data
        activity_report_count = 2
        allocate(activity_report_data(activity_report_count))
        
        activity_report_data(1)%activity_report_id = "AR001"
        activity_report_data(1)%tag_code = "TAG001"
        activity_report_data(1)%keyword = "technology"
        activity_report_data(1)%delete_flag = .false.
        activity_report_data(1)%num_tags = 2
        allocate(activity_report_data(1)%search_tags_list(2))
        activity_report_data(1)%search_tags_list(1) = "AI"
        activity_report_data(1)%search_tags_list(2) = "Machine Learning"
        
        activity_report_data(2)%activity_report_id = "AR002"
        activity_report_data(2)%tag_code = "TAG002"
        activity_report_data(2)%keyword = "business"
        activity_report_data(2)%delete_flag = .false.
        activity_report_data(2)%num_tags = 2
        allocate(activity_report_data(2)%search_tags_list(2))
        activity_report_data(2)%search_tags_list(1) = "Strategy"
        activity_report_data(2)%search_tags_list(2) = "Management"
        
    end subroutine load_sample_data
    
    ! Query database table with key condition
    subroutine db_query(table_name, key_condition, filter_expr, projection, &
                       items, num_items, scan_index_forward)
        character(len=*), intent(in) :: table_name
        character(len=*), intent(in) :: key_condition
        character(len=*), intent(in) :: filter_expr
        character(len=*), intent(in) :: projection
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        logical, intent(in), optional :: scan_index_forward
        
        ! Simplified query implementation
        ! In a real implementation, this would parse the key condition and filter expressions
        
        select case (trim(table_name))
            case ("wp_posts")
                call query_posts_table(key_condition, filter_expr, items, num_items)
            case ("tag_master")
                call query_tag_master_table(key_condition, filter_expr, items, num_items)
            case ("activity_report")
                call query_activity_report_table(key_condition, filter_expr, items, num_items)
            case default
                num_items = 0
        end select
        
    end subroutine db_query
    
    ! Scan database table with filter expression
    subroutine db_scan(table_name, filter_expr, projection, limit, items, num_items)
        character(len=*), intent(in) :: table_name
        character(len=*), intent(in) :: filter_expr
        character(len=*), intent(in) :: projection
        integer, intent(in) :: limit
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        
        ! Simplified scan implementation
        select case (trim(table_name))
            case ("wp_posts")
                call scan_posts_table(filter_expr, limit, items, num_items)
            case ("tag_master")
                call scan_tag_master_table(filter_expr, limit, items, num_items)
            case ("activity_report")
                call scan_activity_report_table(filter_expr, limit, items, num_items)
            case default
                num_items = 0
        end select
        
    end subroutine db_scan
    
    ! Get single item from database
    function db_get_item(table_name, key) result(item)
        character(len=*), intent(in) :: table_name
        character(len=*), intent(in) :: key
        type(wp_post) :: item
        
        ! Simplified get item implementation
        ! In a real implementation, this would retrieve the actual item
        
        ! Initialize with default values
        item%ID = 0
        item%post_title = ""
        item%post_content = ""
        item%guid = ""
        item%post_date = ""
        item%post_status = ""
        item%post_type = ""
        
    end function db_get_item
    
    ! Put item into database
    subroutine db_put_item(table_name, item)
        character(len=*), intent(in) :: table_name
        type(wp_post), intent(in) :: item
        
        ! Simplified put item implementation
        ! In a real implementation, this would store the actual item
        
        select case (trim(table_name))
            case ("wp_posts")
                ! Add to posts data
                if (posts_count >= size(posts_data)) then
                    ! Reallocate if needed
                    call reallocate_posts_data()
                end if
                posts_count = posts_count + 1
                posts_data(posts_count) = item
        end select
        
    end subroutine db_put_item
    
    ! Delete item from database
    subroutine db_delete_item(table_name, key)
        character(len=*), intent(in) :: table_name
        character(len=*), intent(in) :: key
        
        ! Simplified delete item implementation
        ! In a real implementation, this would remove the actual item
        
    end subroutine db_delete_item
    
    ! Query posts table
    subroutine query_posts_table(key_condition, filter_expr, items, num_items)
        character(len=*), intent(in) :: key_condition
        character(len=*), intent(in) :: filter_expr
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        
        ! Simplified query - return all posts for now
        num_items = posts_count
        allocate(items(num_items))
        items(1:num_items) = posts_data(1:posts_count)
        
    end subroutine query_posts_table
    
    ! Query tag master table
    subroutine query_tag_master_table(key_condition, filter_expr, items, num_items)
        character(len=*), intent(in) :: key_condition
        character(len=*), intent(in) :: filter_expr
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        
        ! This would need to be adapted for tag_master_item type
        ! For now, return empty result
        num_items = 0
        
    end subroutine query_tag_master_table
    
    ! Query activity report table
    subroutine query_activity_report_table(key_condition, filter_expr, items, num_items)
        character(len=*), intent(in) :: key_condition
        character(len=*), intent(in) :: filter_expr
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        
        ! This would need to be adapted for activity_report_item type
        ! For now, return empty result
        num_items = 0
        
    end subroutine query_activity_report_table
    
    ! Scan posts table
    subroutine scan_posts_table(filter_expr, limit, items, num_items)
        character(len=*), intent(in) :: filter_expr
        integer, intent(in) :: limit
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        
        ! Simplified scan - return posts up to limit
        num_items = min(limit, posts_count)
        allocate(items(num_items))
        items(1:num_items) = posts_data(1:num_items)
        
    end subroutine scan_posts_table
    
    ! Scan tag master table
    subroutine scan_tag_master_table(filter_expr, limit, items, num_items)
        character(len=*), intent(in) :: filter_expr
        integer, intent(in) :: limit
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        
        ! This would need to be adapted for tag_master_item type
        ! For now, return empty result
        num_items = 0
        
    end subroutine scan_tag_master_table
    
    ! Scan activity report table
    subroutine scan_activity_report_table(filter_expr, limit, items, num_items)
        character(len=*), intent(in) :: filter_expr
        integer, intent(in) :: limit
        type(wp_post), allocatable, intent(out) :: items(:)
        integer, intent(out) :: num_items
        
        ! This would need to be adapted for activity_report_item type
        ! For now, return empty result
        num_items = 0
        
    end subroutine scan_activity_report_table
    
    ! Reallocate posts data array
    subroutine reallocate_posts_data()
        type(wp_post), allocatable :: temp_data(:)
        integer :: new_size
        
        new_size = size(posts_data) * 2
        allocate(temp_data(new_size))
        temp_data(1:posts_count) = posts_data(1:posts_count)
        
        deallocate(posts_data)
        allocate(posts_data(new_size))
        posts_data = temp_data
        
        deallocate(temp_data)
        
    end subroutine reallocate_posts_data
    
    ! Get tag master data
    subroutine get_tag_master_data(tag_list, num_tags)
        type(tag_master_item), allocatable, intent(out) :: tag_list(:)
        integer, intent(out) :: num_tags
        
        num_tags = tag_master_count
        allocate(tag_list(num_tags))
        tag_list(1:num_tags) = tag_master_data(1:tag_master_count)
        
    end subroutine get_tag_master_data
    
    ! Get activity report data
    subroutine get_activity_report_data(report_list, num_reports)
        type(activity_report_item), allocatable, intent(out) :: report_list(:)
        integer, intent(out) :: num_reports
        
        num_reports = activity_report_count
        allocate(report_list(num_reports))
        report_list(1:num_reports) = activity_report_data(1:activity_report_count)
        
    end subroutine get_activity_report_data
    
    ! Cleanup database resources
    subroutine cleanup_database()
        if (allocated(posts_data)) then
            deallocate(posts_data)
        end if
        
        if (allocated(tag_master_data)) then
            deallocate(tag_master_data)
        end if
        
        if (allocated(activity_report_data)) then
            deallocate(activity_report_data)
        end if
        
        posts_count = 0
        tag_master_count = 0
        activity_report_count = 0
        
    end subroutine cleanup_database
    
end module database_ops
