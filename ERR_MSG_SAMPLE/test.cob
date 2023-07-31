      *>1.B..+....2....+....3....+....4....+....5....+....6....+....7....+....8
      *>-----------------------------------------------------------------------
      *>課題2
      *>
      *>SUMMARY_SAMPLE0001
      *>2023/07/30 EBAテック 日髙 峻幸
      *>-----------------------------------------------------------------------
       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                SUMMARY_SAMPLE01.
      *>-----------------------------------------------------------------------
      *>環境部
      *>-----------------------------------------------------------------------
       ENVIRONMENT                        DIVISION.
       INPUT-OUTPUT                       SECTION.
       FILE-CONTROL.
      *>-----------------------------------------------------------------------
      *>入力ファイル
      *>-----------------------------------------------------------------------
       SELECT    IN01-FILE   ASSIGN       TO   "IN01.txt"
                             ORGANIZATION IS LINE SEQUENTIAL
                             FILE STATUS  IS IN-FILE-STATUS.
      *>-----------------------------------------------------------------------
      *>出力ファイル
      *>-----------------------------------------------------------------------
       SELECT    OT01-FILE   ASSIGN       TO   "OT01.txt"
                             ORGANIZATION IS LINE SEQUENTIAL.
      *>-----------------------------------------------------------------------
      *>データ部
      *>-----------------------------------------------------------------------
       DATA                               DIVISION.
       FILE                               SECTION.
      *>----------------------------------------------------------------------------
      *>入力ファイルのレイアウト定義
      *>----------------------------------------------------------------------------
       FD   IN01-FILE.
       01   IN01-RECODE.
            03   IN01-ZYUTYU-BANGOU.
                 05   IN01-MISEBAN               PIC X(003).
                 05   IN01-TYUMON-BANGOU         PIC 9(005).
            03   IN01-SHOHIN-ZYOHOU.
                 05   IN01-SHOHIN-CODE.
                      07   IN01-BUNRUI-CODE      PIC X(002).
                      07   IN01-SHOHIN-NO        PIC 9(004).
            03   IN01-TYUMON-ZYOHOU.
                 05   IN01-TYUMON-SU             PIC 9(003).
                 05   IN01-UKETSUKE-NICHIZI.
                      07   IN01-HIDUKE           PIC 9(006).
                      07   IN01-ZIKAN            PIC 9(004).
                 05   IN01-TANTOSYA-CODE.
                      07   IN01-BUSHO-CODE       PIC X(003).
                      07   IN01-TANTOSYA-BANGOU  PIC 9(004).
      *>----------------------------------------------------------------------------
      *>出力ファイルのレイアウト定義
      *>----------------------------------------------------------------------------
       FD   OT01-FILE.
       01   OT01-RECODE.
            03   OT01-SHOHIN-ZYOHOU.
                 05   OT01-SHOHIN-CODE.
                      07   OT01-BUNRUI-CODE      PIC X(002).
                      07   OT01-SHOHIN-NO        PIC 9(004).
            03   OT01-TYUMON-ZYOHOU.
                 05   OT01-TYUMON-SU             PIC 9(004).
       01   OT01-ERRCODE.
            03   OT01-ERR-MESSAGE                PIC X(040).
      *>-----------------------------------------------------------------------
      *>作業領域の定義
      *>-----------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
      *>
       01   IN-FILE-STATUS                       PIC XX.
      *>
       01   WRK-SHOHIN-ZYOHOU-AREA.
            03   WK-BUNRUI-CODE-OLD              PIC X(002).
            03   WK-BUNRUI-CODE-NEW              PIC X(002).
            03   WK-SHOHIN-NO-OLD                PIC 9(004).
            03   WK-SHOHIN-NO-NEW                PIC 9(004).
      *>
       01   WK-SUM-AREA.
            03   WK-SUM-TYUMON-SU                PIC 9(004).
       01   WK-ERR-CHK-AREA.
            03   WK-ERR-FLG                      PIC 9(002).
            03   WK-ERR-COUNT                    PIC 9(004).
       01   WK-WRITE-FLG                         PIC 9(001).
       01   WK-DATA-COUNT                        PIC 9(004).
      *>-----------------------------------------------------------------------
      *>手続き部
      *>-----------------------------------------------------------------------
       PROCEDURE                          DIVISION.
      *>
           PERFORM   INIT-PROC.
      *>
           PERFORM   MAIN-PROC.
      *>
           PERFORM   TERM-PROC.
      *>
           STOP RUN.
      *>-----------------------------------------------------------------------
      *>初期処理
      *>-----------------------------------------------------------------------
       INIT-PROC                          SECTION.
      *>
      *>
           MOVE   00      TO   IN-FILE-STATUS.
           MOVE   SPACE   TO   WK-BUNRUI-CODE-OLD.
           MOVE   SPACE   TO   WK-BUNRUI-CODE-NEW.
           MOVE   ZERO    TO   WK-SHOHIN-NO-OLD.
           MOVE   ZERO    TO   WK-SHOHIN-NO-NEW.
           MOVE   ZERO    TO   IN01-TYUMON-SU.
           MOVE   ZERO    TO   WK-SUM-TYUMON-SU.
           MOVE   ZERO    TO   WK-ERR-FLG.
           MOVE   ZERO    TO   WK-ERR-COUNT.
           MOVE   SPACE   TO   OT01-ERR-MESSAGE.
           MOVE   ZERO    TO   WK-WRITE-FLG.
           MOVE   ZERO    TO   WK-DATA-COUNT.
      *>
      *>   ファイルのオープン
           OPEN   INPUT    IN01-FILE
                  OUTPUT   OT01-FILE.
      *>
           READ IN01-FILE
                AT     END
                DISPLAY   "READ END"
      *>   読み込んだファイルが0件だった場合ファイルを閉じて終了する。
                IF     WK-DATA-COUNT   =  0
                    MOVE   WK-DATA-COUNT"件のデータを読み込みました。" 
                                             TO  OT01-ERR-MESSAGE
                    WRITE     OT01-ERRCODE
                    PERFORM   TERM-PROC
                END-IF
      *>
                NOT   AT   END
                MOVE   IN01-BUNRUI-CODE      TO   WK-BUNRUI-CODE-NEW
                                                  WK-BUNRUI-CODE-OLD
                MOVE   IN01-SHOHIN-NO        TO   WK-SHOHIN-NO-OLD
                                                  WK-SHOHIN-NO-NEW
                MOVE   IN01-TYUMON-SU        TO   WK-SUM-TYUMON-SU
                ADD    1                     TO   WK-DATA-COUNT
           END-READ.
       INIT-PROC-EXIT.
      *>
           EXIT.
      *>-----------------------------------------------------------------------
      *>主処理
      *>-----------------------------------------------------------------------
       MAIN-PROC                          SECTION.
      *>
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
      *>   IN01を読む
           READ   IN01-FILE
      *    読み終わったら
               AT   END
      *>   READ ENDを表示
               DISPLAY   "READ END"
      *>   分類コードを渡す
               MOVE   WK-BUNRUI-CODE-OLD   TO   OT01-BUNRUI-CODE
      *>   商品番号を渡す
               MOVE   WK-SHOHIN-NO-OLD     TO   OT01-SHOHIN-NO
      *>   上記2つが商品コード
      *>   注文数を渡す
               MOVE   WK-SUM-TYUMON-SU     TO   OT01-TYUMON-SU
      *>   改行がいらないので全て詰めてWrite命令をする
               WRITE   OT01-RECODE
      *>   ファイルがまだある場合
               NOT   AT   END
      *>   エラーチェック実施
               PERFORM       ERR-CHK-PROC
               MOVE   IN01-BUNRUI-CODE     TO   WK-BUNRUI-CODE-NEW
               MOVE   IN01-SHOHIN-NO       TO   WK-SHOHIN-NO-NEW
      *>   何個目のファイルでエラーがあったかカウント
               ADD    1                    TO   WK-ERR-COUNT
      *>   エラーチェックでフラグが立った場合
               IF   WK-ERR-FLG     NOT  =  0
      *>   エラーメッセージにファイル件数とエラーコードを挿入
                    MOVE  WK-ERR-COUNT "件目のファイルにエラーがある為、
                    プログラムを終了致します。エラーコード:" WK-ERR-FLG
                                           TO  OT01-ERR-MESSAGE
      *>   前回ファイル出力をしているか確認
      *>   フラグに1が立っていた場合Write処理
                    IF   WK-WRITE-FLG   =  1
                    THEN
      *>   フラグが立っていた場合は計算途中なのでOT01RECODEを記載する
                         WRITE     OT01-RECODE
                         WRITE     OT01-ERRCODE
                         PERFORM   TERM-PROC
                    ELSE
      *>   フラグがない場合は記載後なのでエラーコードのみ記載して終了
                         WRITE     OT01-ERRCODE
                         PERFORM   TERM-PROC
                    END-IF
               END-IF
      *>   商品コードを上書き
      *>       キーブレイク
               IF   WK-BUNRUI-CODE-NEW  =  WK-BUNRUI-CODE-OLD   AND
                    WK-SHOHIN-NO-OLD    =  WK-SHOHIN-NO-NEW
      *>
      *>       データ集計
               THEN
                    MOVE      1                 TO   WK-WRITE-FLG
      *    ソート済のためこれでOK
                    COMPUTE   WK-SUM-TYUMON-SU =
                                   WK-SUM-TYUMON-SU + IN01-TYUMON-SU
      *>   商品コードが違ったらWrite命令
      *>       ファイル出力
               ELSE
                    MOVE   WK-BUNRUI-CODE-OLD   TO
                                                       OT01-BUNRUI-CODE
                    MOVE   WK-SHOHIN-NO-OLD     TO
                                                       OT01-SHOHIN-NO
                    MOVE   WK-SUM-TYUMON-SU     TO     OT01-TYUMON-SU
                    WRITE  OT01-RECODE
      *>
      *>   次のキーをセット
                   MOVE WK-BUNRUI-CODE-NEW TO WK-BUNRUI-CODE-OLD
                   MOVE WK-SHOHIN-NO-NEW   TO WK-SHOHIN-NO-OLD
                   MOVE IN01-TYUMON-SU     TO WK-SUM-TYUMON-SU
               END-IF
           END-READ
       END-PERFORM.

       MAIN-PROC-EXIT.
      *>
           EXIT.
      *>-----------------------------------------------------------------------
      *>エラーチェック処理
      *>-----------------------------------------------------------------------
       ERR-CHK-PROC
      *>
      *>   店番
       IF  IN01-MISEBAN         =   SPACE
           DISPLAY   "店番エラー"
      *>   エラー時にフラグを立てる
           MOVE      1     TO  WK-ERR-FLG
       END-IF.
      *>   注文番号
       IF  IN01-TYUMON-BANGOU   =   SPACE
           DISPLAY   "注文番号エラー"
      *>   エラー時にフラグを立てる
           MOVE      2     TO  WK-ERR-FLG
       END-IF.
      *>   分類コード
       IF  IN01-BUNRUI-CODE     =   SPACE
           DISPLAY   "分類コードエラー"
      *>   エラー時にフラグを立てる
           MOVE      3     TO  WK-ERR-FLG
       END-IF.
      *>   商品No
       IF  IN01-SHOHIN-NO       =   SPACE
           DISPLAY   "商品Noエラー"
      *>   エラー時にフラグを立てる
           MOVE      4     TO  WK-ERR-FLG
       END-IF.
      *>   注文数
       IF  IN01-TYUMON-SU       =   SPACE
           DISPLAY   "注文数エラー"
      *>   エラー時にフラグを立てる
           MOVE      5     TO  WK-ERR-FLG
       END-IF.
      *>   日付
       IF  IN01-HIDUKE          =   SPACE
           DISPLAY   "日付エラー"
      *>   エラー時にフラグを立てる
           MOVE      6     TO  WK-ERR-FLG
       END-IF.
      *>   時間
       IF  IN01-ZIKAN           =   SPACE
           DISPLAY   "時間エラー"
      *>   エラー時にフラグを立てる
           MOVE      7     TO  WK-ERR-FLG
       END-IF.
      *>   部署コード
       IF  IN01-BUSHO-CODE      =   SPACE
           DISPLAY   "部署コードエラー"
      *>   エラー時にフラグを立てる
           MOVE      8     TO  WK-ERR-FLG
           END-IF.
      *>   担当者番号
       IF  IN01-TANTOU-BANGOU   =   SPACE
           DISPLAY   "担当者番号エラー"
      *>   エラー時にフラグを立てる
           MOVE      9     TO  WK-ERR-FLG
       END-IF.
       ERR-CHK-PROC-EXIT.
      *>-----------------------------------------------------------------------
      *>終了処理
      *>-----------------------------------------------------------------------
       TERM-PROC                         SECTION.
       CLOSE   IN01-FILE
               OT01-FILE.
       TERM-PROC-EXIT.
      *>
           EXIT.
