! JSONユーティリティモジュール for Fortran Lambda
! 基本的なJSON解析と文字列操作機能を提供

module json_utils
    implicit none
    
    private
    public :: json_get_value, json_get_array, json_escape_string, json_unescape_string
    public :: json_parse_object, json_create_object, json_create_array
    
    ! JSONトークンタイプ
    integer, parameter :: JSON_NULL = 0      ! null値
    integer, parameter :: JSON_STRING = 1    ! 文字列
    integer, parameter :: JSON_NUMBER = 2    ! 数値
    integer, parameter :: JSON_BOOL = 3      ! 真偽値
    integer, parameter :: JSON_ARRAY = 4     ! 配列
    integer, parameter :: JSON_OBJECT = 5    ! オブジェクト
    
    ! JSONトークン構造
    type :: json_token
        integer :: token_type        ! トークンタイプ
        character(len=1024) :: value ! トークン値
        integer :: start_pos         ! 開始位置
        integer :: end_pos           ! 終了位置
    end type json_token
    
contains
    
    ! JSON文字列からキーで値を抽出
    function json_get_value(json_str, key) result(value)
        character(len=*), intent(in) :: json_str
        character(len=*), intent(in) :: key
        character(len=1024) :: value
        
        integer :: key_pos, colon_pos, comma_pos, brace_pos
        integer :: start_pos, end_pos
        
        value = ""
        
        ! キーを検索
        key_pos = index(json_str, '"' // trim(key) // '":')
        if (key_pos == 0) return
        
        ! キーの後のコロンを検索
        colon_pos = key_pos + len_trim(key) + 2
        
        ! 値の開始位置を検索
        start_pos = colon_pos
        do while (start_pos <= len(json_str) .and. json_str(start_pos:start_pos) == ' ')
            start_pos = start_pos + 1
        end do
        
        ! 値の終了位置を決定
        if (json_str(start_pos:start_pos) == '"') then
            ! 文字列値
            start_pos = start_pos + 1
            end_pos = start_pos
            do while (end_pos <= len(json_str) .and. json_str(end_pos:end_pos) /= '"')
                end_pos = end_pos + 1
            end do
            if (end_pos <= len(json_str)) then
                value = json_str(start_pos:end_pos-1)
            end if
        else if (json_str(start_pos:start_pos) == '{') then
            ! オブジェクト値
            brace_pos = start_pos
            do while (brace_pos <= len(json_str))
                if (json_str(brace_pos:brace_pos) == '{') then
                    brace_pos = brace_pos + 1
                else if (json_str(brace_pos:brace_pos) == '}') then
                    exit
                else
                    brace_pos = brace_pos + 1
                end if
            end do
            if (brace_pos <= len(json_str)) then
                value = json_str(start_pos:brace_pos)
            end if
        else if (json_str(start_pos:start_pos) == '[') then
            ! 配列値
            brace_pos = start_pos
            do while (brace_pos <= len(json_str))
                if (json_str(brace_pos:brace_pos) == '[') then
                    brace_pos = brace_pos + 1
                else if (json_str(brace_pos:brace_pos) == ']') then
                    exit
                else
                    brace_pos = brace_pos + 1
                end if
            end do
            if (brace_pos <= len(json_str)) then
                value = json_str(start_pos:brace_pos)
            end if
        else
            ! 単純な値（数値、真偽値、null）
            comma_pos = index(json_str(start_pos:), ',')
            if (comma_pos > 0) then
                end_pos = start_pos + comma_pos - 2
            else
                end_pos = len(json_str)
            end if
            value = json_str(start_pos:end_pos)
        end if
        
    end function json_get_value
    
    ! JSON文字列からキーで配列を抽出
    subroutine json_get_array(json_str, key, array_values, num_values)
        character(len=*), intent(in) :: json_str
        character(len=*), intent(in) :: key
        character(len=1024), allocatable, intent(out) :: array_values(:)
        integer, intent(out) :: num_values
        
        character(len=1024) :: array_str
        integer :: i, start_pos, end_pos, brace_count
        character(len=1024), allocatable :: temp_values(:)
        
        num_values = 0
        array_str = json_get_value(json_str, key)
        
        if (len_trim(array_str) == 0) return
        
        ! 外側のブラケットを削除
        if (array_str(1:1) == '[' .and. array_str(len_trim(array_str):len_trim(array_str)) == ']') then
            array_str = array_str(2:len_trim(array_str)-1)
        end if
        
        ! 配列要素をカウント
        num_values = 0
        start_pos = 1
        do i = 1, len_trim(array_str)
            if (array_str(i:i) == '[' .or. array_str(i:i) == '{') then
                brace_count = brace_count + 1
            else if (array_str(i:i) == ']' .or. array_str(i:i) == '}') then
                brace_count = brace_count - 1
            else if (array_str(i:i) == ',' .and. brace_count == 0) then
                num_values = num_values + 1
            end if
        end do
        num_values = num_values + 1
        
        ! 配列の割り当てと値の抽出
        allocate(array_values(num_values))
        allocate(temp_values(num_values))
        
        start_pos = 1
        num_values = 0
        brace_count = 0
        
        do i = 1, len_trim(array_str)
            if (array_str(i:i) == '[' .or. array_str(i:i) == '{') then
                brace_count = brace_count + 1
            else if (array_str(i:i) == ']' .or. array_str(i:i) == '}') then
                brace_count = brace_count - 1
            else if (array_str(i:i) == ',' .and. brace_count == 0) then
                num_values = num_values + 1
                temp_values(num_values) = array_str(start_pos:i-1)
                start_pos = i + 1
            end if
        end do
        
        ! 最後の要素を取得
        if (start_pos <= len_trim(array_str)) then
            num_values = num_values + 1
            temp_values(num_values) = array_str(start_pos:len_trim(array_str))
        end if
        
        ! 出力配列にコピー
        array_values(1:num_values) = temp_values(1:num_values)
        
        deallocate(temp_values)
        
    end subroutine json_get_array
    
    ! JSON文字列内の特殊文字をエスケープ
    function json_escape_string(input_str) result(escaped_str)
        character(len=*), intent(in) :: input_str
        character(len=len(input_str)*2) :: escaped_str
        
        integer :: i, j
        character(len=1) :: ch
        
        j = 1
        do i = 1, len_trim(input_str)
            ch = input_str(i:i)
            select case (ch)
                case ('"')
                    escaped_str(j:j+1) = '\"'    ! ダブルクォート
                    j = j + 2
                case ('\')
                    escaped_str(j:j+1) = '\\'    ! バックスラッシュ
                    j = j + 2
                case (achar(8))  ! バックスペース
                    escaped_str(j:j+1) = '\b'
                    j = j + 2
                case (achar(9))  ! タブ
                    escaped_str(j:j+1) = '\t'
                    j = j + 2
                case (achar(10)) ! 改行
                    escaped_str(j:j+1) = '\n'
                    j = j + 2
                case (achar(12)) ! フォームフィード
                    escaped_str(j:j+1) = '\f'
                    j = j + 2
                case (achar(13)) ! キャリッジリターン
                    escaped_str(j:j+1) = '\r'
                    j = j + 2
                case default
                    escaped_str(j:j) = ch
                    j = j + 1
            end select
        end do
        
        escaped_str = escaped_str(1:j-1)
        
    end function json_escape_string
    
    ! JSON文字列内のエスケープされた特殊文字を復元
    function json_unescape_string(input_str) result(unescaped_str)
        character(len=*), intent(in) :: input_str
        character(len=len(input_str)) :: unescaped_str
        
        integer :: i, j
        character(len=1) :: ch
        
        j = 1
        i = 1
        do while (i <= len_trim(input_str))
            ch = input_str(i:i)
            if (ch == '\' .and. i < len_trim(input_str)) then
                i = i + 1
                ch = input_str(i:i)
                select case (ch)
                    case ('"')
                        unescaped_str(j:j) = '"'      ! ダブルクォート
                    case ('\')
                        unescaped_str(j:j) = '\'      ! バックスラッシュ
                    case ('b')
                        unescaped_str(j:j) = achar(8) ! バックスペース
                    case ('t')
                        unescaped_str(j:j) = achar(9) ! タブ
                    case ('n')
                        unescaped_str(j:j) = achar(10) ! 改行
                    case ('f')
                        unescaped_str(j:j) = achar(12) ! フォームフィード
                    case ('r')
                        unescaped_str(j:j) = achar(13) ! キャリッジリターン
                    case default
                        unescaped_str(j:j) = ch
                end select
            else
                unescaped_str(j:j) = ch
            end if
            j = j + 1
            i = i + 1
        end do
        
        unescaped_str = unescaped_str(1:j-1)
        
    end function json_unescape_string
    
    ! シンプルなJSONオブジェクトを作成
    function json_create_object(key_value_pairs, num_pairs) result(json_str)
        character(len=*), dimension(:), intent(in) :: key_value_pairs
        integer, intent(in) :: num_pairs
        character(len=2048) :: json_str
        
        integer :: i
        character(len=64) :: temp_str
        
        json_str = "{"
        
        do i = 1, num_pairs
            if (i > 1) then
                json_str = trim(json_str) // ","
            end if
            json_str = trim(json_str) // '"' // trim(key_value_pairs(i)) // '":'
            
            ! 値を追加（簡易化のため文字列値と仮定）
            json_str = trim(json_str) // '"' // trim(key_value_pairs(i)) // '"'
        end do
        
        json_str = trim(json_str) // "}"
        
    end function json_create_object
    
    ! シンプルなJSON配列を作成
    function json_create_array(values, num_values) result(json_str)
        character(len=*), dimension(:), intent(in) :: values
        integer, intent(in) :: num_values
        character(len=2048) :: json_str
        
        integer :: i
        
        json_str = "["
        
        do i = 1, num_values
            if (i > 1) then
                json_str = trim(json_str) // ","
            end if
            json_str = trim(json_str) // '"' // trim(values(i)) // '"'
        end do
        
        json_str = trim(json_str) // "]"
        
    end function json_create_array
    
    ! JSONオブジェクトの解析（簡易版）
    subroutine json_parse_object(json_str, keys, values, num_items)
        character(len=*), intent(in) :: json_str
        character(len=*), dimension(:), intent(out) :: keys
        character(len=*), dimension(:), intent(out) :: values
        integer, intent(out) :: num_items
        
        ! 簡易JSON解析
        ! 実際の実装ではより高度な処理を行う
        
        num_items = 0
        
        ! これは実際のJSON解析ロジックのプレースホルダー
        ! 実際の実装ではJSONをトークン化してキー値ペアを抽出
        
    end subroutine json_parse_object
    
end module json_utils
