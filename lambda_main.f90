! AWS Lambda関数 - Fortran実装
! コンテンツ検索サービス（DynamoDB統合版）
!
! このモジュールは、コンテンツ検索、DynamoDB操作、
! API Gateway統合の主要機能を提供します

module lambda_main
    use, intrinsic :: iso_fortran_env, only: int32, real64, character_kinds
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_null_char
    
    implicit none
    
    ! 定数定義
    character(len=*), parameter :: APPLICATION_JSON = "application/json"
    character(len=*), parameter :: ALLOWED_METHODS = "OPTIONS,POST,GET"
    character(len=*), parameter :: ALLOWED_HEADERS = "Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token"
    character(len=*), parameter :: HTML_PARSER = 'html.parser'
    character(len=*), parameter :: INTERNAL_SERVER_ERROR_MESSAGE = "Internal server error"
    
    ! 環境変数
    character(len=256) :: POSTS_TABLE_NAME
    character(len=256) :: POSTMETA_TABLE_NAME
    character(len=256) :: CONTENT_BUCKET_NAME
    
    ! テーブル名
    character(len=*), parameter :: ACTIVITY_REPORT_TABLE_NAME = 'activity_report'
    character(len=*), parameter :: USER_MASTER_TABLE_NAME = 'user_master'
    
    ! アクティビティレポート定数
    character(len=*), parameter :: ACTIVITY_REPORT_PUBLISHED = "published"
    
    ! グローバル状態
    logical :: is_initialized = .false.
    
    ! データ構造
    type :: rate_limit_info
        integer :: limit          ! リクエスト制限数
        integer :: remaining      ! 残りリクエスト数
        integer :: reset_time     ! リセット時刻
    end type rate_limit_info
    
    type :: external_site
        character(len=64) :: name           ! サイト名
        character(len=256) :: bucket       ! S3バケット名
        character(len=256) :: prefix       ! コンテンツプレフィックス
        character(len=256) :: index_prefix ! インデックスプレフィックス
    end type external_site
    
    type :: search_result
        character(len=256) :: page_id      ! ページID
        character(len=512) :: title        ! タイトル
        character(len=2048) :: content    ! コンテンツ
        character(len=512) :: url         ! URL
        character(len=64) :: last_updated ! 最終更新日
        real :: score                     ! 検索スコア
    end type search_result
    
    type :: wp_post
        integer :: ID                    ! 投稿ID
        character(len=512) :: post_title ! 投稿タイトル
        character(len=8192) :: post_content ! 投稿コンテンツ
        character(len=512) :: guid       ! GUID
        character(len=64) :: post_date  ! 投稿日
    end type wp_post
    
    type :: tag_master_item
        character(len=64) :: tag_code   ! タグコード
        character(len=256) :: tag_name  ! タグ名
        logical :: deleted_flg          ! 削除フラグ
    end type tag_master_item
    
    type :: activity_report_item
        character(len=64) :: activity_report_id           ! アクティビティレポートID
        character(len=64) :: tag_code                    ! タグコード
        character(len=256) :: keyword                    ! キーワード
        logical :: delete_flag                           ! 削除フラグ
        character(len=1024), allocatable :: search_tags_list(:) ! 検索タグリスト
    end type activity_report_item
    
    ! 外部サイト配列
    type(external_site), allocatable :: external_sites(:)
    
    ! レート制限用キャッシュ（簡易実装）
    type :: rate_limiter
        integer :: max_requests                    ! 最大リクエスト数
        integer :: time_window                     ! 時間枠
        integer, allocatable :: request_counts(:) ! リクエストカウント配列
        integer, allocatable :: first_requests(:) ! 初回リクエスト時刻配列
        character(len=45), allocatable :: client_ips(:) ! クライアントIP配列
        integer :: cache_size                      ! キャッシュサイズ
    end type rate_limiter
    
    ! グローバルレート制限器インスタンス
    type(rate_limiter) :: global_rate_limiter
    
contains
    
    ! Lambda関数のメインハンドラー
    function lambda_handler(event_json, context) result(response)
        character(len=*), intent(in) :: event_json  ! イベントJSON
        character(len=*), intent(in) :: context     ! コンテキスト
        character(len=2048) :: response             ! レスポンス
        
        character(len=512) :: action                ! アクション
        integer :: status_code                      ! ステータスコード
        character(len=1024) :: response_body        ! レスポンスボディ
        
        ! 初期化が完了していない場合は初期化処理を実行
        if (.not. is_initialized) then
            if (.not. initialize_service()) then
                response_body = '{"error": "Service initialization failed"}'
                response = create_response(500, response_body)
                return
            end if
        end if
        
        ! イベントを解析してアクションを決定
        action = extract_action(event_json)
        
        ! 異なるアクションを処理
        select case (trim(action))
            case ('health')
                response_body = '{"status": "healthy", "initialized": true}'
                response = create_response(200, response_body)
            case ('search')
                response = handle_search_request(event_json)
            case ('search-tags')
                response = handle_search_tags_request(event_json)
            case default
                response_body = '{"error": "Invalid action", "message": "Action not supported"}'
                response = create_response(400, response_body)
        end select
        
    end function lambda_handler
    
    ! サービスの初期化
    function initialize_service() result(success)
        logical :: success
        
        success = .false.
        
        ! 環境変数を設定（実際の実装ではAWSから取得）
        POSTS_TABLE_NAME = "wp_posts"
        POSTMETA_TABLE_NAME = "wp_postmeta"
        CONTENT_BUCKET_NAME = "content-bucket"
        
        ! 外部サイトを初期化
        call initialize_external_sites()
        
        ! レート制限器を初期化
        call initialize_rate_limiter()
        
        ! 初期化完了としてマーク
        is_initialized = .true.
        success = .true.
        
    end function initialize_service
    
    ! 外部サイト設定の初期化
    subroutine initialize_external_sites()
        allocate(external_sites(3))
        
        ! 情報サイト
        external_sites(1)%name = "information"
        external_sites(1)%bucket = CONTENT_BUCKET_NAME
        external_sites(1)%prefix = "information/information/"
        external_sites(1)%index_prefix = "information/index/"
        
        ! SLPサイト
        external_sites(2)%name = "slp"
        external_sites(2)%bucket = CONTENT_BUCKET_NAME
        external_sites(2)%prefix = "slp/"
        external_sites(2)%index_prefix = "slp/index/"
        
        ! ツールサイト
        external_sites(3)%name = "tools"
        external_sites(3)%bucket = CONTENT_BUCKET_NAME
        external_sites(3)%prefix = "tools/"
        external_sites(3)%index_prefix = "tools/index/"
        
    end subroutine initialize_external_sites
    
    ! レート制限器の初期化
    subroutine initialize_rate_limiter()
        global_rate_limiter%max_requests = 100    ! 最大リクエスト数
        global_rate_limiter%time_window = 3600   ! 時間枠（秒）
        global_rate_limiter%cache_size = 1000    ! キャッシュサイズ
        
        ! 配列の割り当て
        allocate(global_rate_limiter%request_counts(global_rate_limiter%cache_size))
        allocate(global_rate_limiter%first_requests(global_rate_limiter%cache_size))
        allocate(global_rate_limiter%client_ips(global_rate_limiter%cache_size))
        
        ! 初期値の設定
        global_rate_limiter%request_counts = 0
        global_rate_limiter%first_requests = 0
        global_rate_limiter%client_ips = ""
        
    end subroutine initialize_rate_limiter
    
    ! イベントJSONからアクションを抽出
    function extract_action(event_json) result(action)
        character(len=*), intent(in) :: event_json
        character(len=512) :: action
        
        ! アクションフィールドの簡易JSON解析
        ! 実際の実装では適切なJSONパーサーを使用
        if (index(event_json, '"action"') > 0) then
            ! アクション値を抽出（簡易版）
            action = "search"  ! デフォルトアクション
        else if (index(event_json, '"httpMethod"') > 0) then
            ! API Gatewayイベント
            if (index(event_json, '"POST"') > 0) then
                if (index(event_json, '"/search-tags"') > 0) then
                    action = "search-tags"
                else
                    action = "search"
                end if
            else
                action = "unknown"
            end if
        else
            action = "unknown"
        end if
        
    end function extract_action
    
    ! HTTPレスポンスの作成
    function create_response(status_code, body) result(response)
        integer, intent(in) :: status_code
        character(len=*), intent(in) :: body
        character(len=2048) :: response
        
        character(len=64) :: status_str
        
        ! ステータスコードを文字列に変換
        write(status_str, '(I0)') status_code
        
        ! ヘッダー付きレスポンスを作成
        response = '{"statusCode": ' // trim(status_str) // ', ' // &
                  '"headers": {' // &
                  '"Content-Type": "application/json",' // &
                  '"Access-Control-Allow-Origin": "*",' // &
                  '"Access-Control-Allow-Headers": "' // ALLOWED_HEADERS // '",' // &
                  '"Access-Control-Allow-Methods": "' // ALLOWED_METHODS // '"' // &
                  '}, ' // &
                  '"body": ' // trim(body) // '}'
        
    end function create_response
    
    ! 検索リクエストの処理
    function handle_search_request(event_json) result(response)
        character(len=*), intent(in) :: event_json
        character(len=2048) :: response
        
        character(len=1024) :: response_body
        character(len=256), allocatable :: keywords(:)
        integer :: num_keywords
        
        ! リクエストからキーワードを抽出（簡易版）
        call extract_keywords(event_json, keywords, num_keywords)
        
        ! 検索を実行
        if (num_keywords > 0) then
            response_body = '{"results": [], "message": "Search completed"}'
        else
            response_body = '{"error": "No keywords provided"}'
        end if
        
        response = create_response(200, response_body)
        
    end function handle_search_request
    
    ! 検索タグリクエストの処理
    function handle_search_tags_request(event_json) result(response)
        character(len=*), intent(in) :: event_json
        character(len=2048) :: response
        
        character(len=1024) :: response_body
        character(len=64) :: activity_report_id
        
        ! リクエストからactivity_report_idを抽出（簡易版）
        call extract_activity_report_id(event_json, activity_report_id)
        
        if (len_trim(activity_report_id) > 0) then
            response_body = '{"tags": [], "message": "Tags retrieved"}'
        else
            response_body = '{"error": "activity_report_id is required"}'
        end if
        
        response = create_response(200, response_body)
        
    end function handle_search_tags_request
    
    ! JSONリクエストからキーワードを抽出
    subroutine extract_keywords(event_json, keywords, num_keywords)
        character(len=*), intent(in) :: event_json
        character(len=256), allocatable, intent(out) :: keywords(:)
        integer, intent(out) :: num_keywords
        
        ! 簡易キーワード抽出
        ! 実際の実装ではJSONを適切に解析
        num_keywords = 1
        allocate(keywords(num_keywords))
        keywords(1) = "default"
        
    end subroutine extract_keywords
    
    ! JSONリクエストからアクティビティレポートIDを抽出
    subroutine extract_activity_report_id(event_json, activity_report_id)
        character(len=*), intent(in) :: event_json
        character(len=64), intent(out) :: activity_report_id
        
        ! 簡易抽出
        ! 実際の実装ではJSONを適切に解析
        activity_report_id = "default_id"
        
    end subroutine extract_activity_report_id
    
    ! クライアントのレート制限をチェック
    function check_rate_limit(client_ip) result(allowed)
        character(len=*), intent(in) :: client_ip
        logical :: allowed
        
        integer :: i, current_time
        
        ! 簡易レート制限ロジック
        allowed = .true.
        
        ! 実際の実装では、rate_limiter構造体を使用して
        ! 適切なレート制限を実装
        
    end function check_rate_limit
    
    ! リソースのクリーンアップ
    subroutine cleanup()
        if (allocated(external_sites)) then
            deallocate(external_sites)
        end if
        
        if (allocated(global_rate_limiter%request_counts)) then
            deallocate(global_rate_limiter%request_counts)
        end if
        
        if (allocated(global_rate_limiter%first_requests)) then
            deallocate(global_rate_limiter%first_requests)
        end if
        
        if (allocated(global_rate_limiter%client_ips)) then
            deallocate(global_rate_limiter%client_ips)
        end if
        
    end subroutine cleanup
    
end module lambda_main
