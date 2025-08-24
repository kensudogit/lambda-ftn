# Fortran Lambda関数用Makefile

# コンパイラ設定
FC = gfortran
FFLAGS = -O2 -Wall -std=f2003

# ターゲット名
TARGET = lambda_ftn

# ソースファイル
SOURCES = json_utils.f90 database_ops.f90 search_engine.f90 lambda_main.f90

# オブジェクトファイル
OBJECTS = $(SOURCES:.f90=.o)

# デフォルトターゲット
all: $(TARGET)

# メインターゲットのビルド
$(TARGET): $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $^

# オブジェクトファイルの生成
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# 依存関係
lambda_main.o: json_utils.o database_ops.o search_engine.o
search_engine.o: json_utils.o
database_ops.o: json_utils.o

# クリーンアップ
clean:
	rm -f $(OBJECTS) $(TARGET) *.mod

# インストール（必要に応じて）
install: $(TARGET)
	cp $(TARGET) /usr/local/bin/

# ヘルプ
help:
	@echo "利用可能なターゲット:"
	@echo "  all      - プロジェクトをビルド"
	@echo "  clean    - ビルドファイルを削除"
	@echo "  install  - システムにインストール"
	@echo "  help     - このヘルプを表示"

# テスト実行
test: $(TARGET)
	./$(TARGET)

# デバッグビルド
debug: FFLAGS += -g -fcheck=all
debug: $(TARGET)

# リリースビルド
release: FFLAGS += -O3 -march=native
release: $(TARGET)

.PHONY: all clean install help test debug release
