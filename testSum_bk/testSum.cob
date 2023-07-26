      *>1.B..+....2....+....3....+....4....+....5....+....6....+....7....+....8
      *>-----------------------------------------------------------------------
      *>サマリーのプログラムサンプル01_bk
      *>SUMMARY_SAMPLE0001
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
                 05   UKETSUKE-NICHIZI.
                      07   HIDUKE                PIC 9(006).
                      07   ZIKAN                 PIC 9(004).
                 05   TANTOSYA-CODE.
                      07   BUSHO-CODE            PIC X(003).
                      07   TANTOSYA-BANGOU       PIC 9(004).
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
      *>-----------------------------------------------------------------------
      *>作業領域の定義
      *>-----------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
      *>
       01   IN-FILE-STATUS           PIC XX.
      *>
       01   WRK-SHOHIN-ZYOHOU-AREA.
            03   WK-BUNRUI-CODE-OLD          PIC X(002).
            03   WK-BUNRUI-CODE-NEW          PIC X(002).
            03   WK-SHOHIN-NO-OLD            PIC 9(004).
            03   WK-SHOHIN-NO-NEW            PIC 9(004).
      *>
       01   WK-SUM-AREA.
            03   WK-SUM-TYUMON-SU            PIC 9(004).
      *>-----------------------------------------------------------------------
      *>手続き部
      *>-----------------------------------------------------------------------
       PROCEDURE                          DIVISION.
      *>
           PERFORM   MAIN-PROC.
      *>
           PERFORM   TERM-PROC.
      *>
           STOP RUN.
      *>-----------------------------------------------------------------------
      *>主処理
      *>-----------------------------------------------------------------------
       MAIN-PROC                          SECTION.
      *>
       PERFORM   INIT-PROC.
      *>
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
      *>
      *>   IN01-FILEを読み込む
           READ   IN01-FILE
      *>
      *>       読み込み終了
               AT   END
      *>
      *>       READ ENDを表示する
               DISPLAY   "READ END"
      *>
      *>       商品コード：分類コードを出力ファイルに渡す
               MOVE   WK-BUNRUI-CODE-OLD   TO   OT01-BUNRUI-CODE
      *>
      *>       商品コード：商品Noを出力ファイルに渡す
               MOVE   WK-SHOHIN-NO-OLD     TO   OT01-SHOHIN-NO
      *>
      *>       合計の注文数を出力ファイルに渡す
               MOVE   WK-SUM-TYUMON-SU     TO   OT01-TYUMON-SU
      *>
      *>       改行なしで値を詰めた状態でWRITEの命令をする
               WRITE   OT01-RECODE
      *>
      *>       ファイルがある場合
               NOT   AT   END
      *>
      *>       商品コードを上書きする
               MOVE   IN01-BUNRUI-CODE     TO   WK-BUNRUI-CODE-NEW
               MOVE   IN01-SHOHIN-NO       TO   WK-SHOHIN-NO-NEW
      *>
      *>
      *>       キーブレイク
               IF   WK-BUNRUI-CODE-NEW  =  WK-BUNRUI-CODE-OLD   AND
                    WK-SHOHIN-NO-OLD    =  WK-SHOHIN-NO-NEW
      *>
      *>       データ集計
               THEN
      *>
      *>          ソート済みのため以下の記述で問題ない
                   COMPUTE   WK-SUM-TYUMON-SU =
                                    WK-SUM-TYUMON-SU + IN01-TYUMON-SU
      *>
      *>       商品コードが異なった場合はWRITE処理を行う
      *>       ファイル出力
               ELSE
                   MOVE   WK-BUNRUI-CODE-OLD   TO
                                                  OT01-BUNRUI-CODE
                   MOVE   WK-SHOHIN-NO-OLD     TO
                                                  OT01-SHOHIN-NO
                   MOVE   WK-SUM-TYUMON-SU     TO   OT01-TYUMON-SU
                   WRITE   OT01-RECODE
      *>
      *>           次のキーをセット
      *>           商品コード：新しい分類コードを古いコードに渡す
                   MOVE WK-BUNRUI-CODE-NEW TO WK-BUNRUI-CODE-OLD
      *>
      *>           商品コード：新しい分類コードを古いコードに渡す
                   MOVE WK-SHOHIN-NO-NEW   TO WK-SHOHIN-NO-OLD
      *>
      *>           IN01ファイルに入っている注文数を変数に渡す
                   MOVE IN01-TYUMON-SU TO WK-SUM-TYUMON-SU
               END-IF
           END-READ
       END-PERFORM.

       MAIN-PROC-EXIT.
      *>
           EXIT.
      *>-----------------------------------------------------------------------
      *>初期処理
      *>-----------------------------------------------------------------------
       INIT-PROC                          SECTION.
      *>
      *>
           MOVE   SPACE   TO   IN-FILE-STATUS.
           MOVE   SPACE   TO   WK-BUNRUI-CODE-OLD.
           MOVE   SPACE   TO   WK-BUNRUI-CODE-NEW.
           MOVE   ZERO    TO   WK-SHOHIN-NO-OLD.
           MOVE   ZERO    TO   WK-SHOHIN-NO-NEW.
           MOVE   ZERO    TO   IN01-TYUMON-SU.
           MOVE   ZERO    TO   WK-SUM-TYUMON-SU.
      *>
      *>  ファイルのオープン
           OPEN   INPUT    IN01-FILE
                  OUTPUT   OT01-FILE.
      *>
           READ IN01-FILE
                 AT     END
                 DISPLAY   "READ END"
      *>
                NOT   AT   END
                MOVE   IN01-BUNRUI-CODE      TO   WK-BUNRUI-CODE-NEW
                                                  WK-BUNRUI-CODE-OLD
                MOVE   IN01-SHOHIN-NO        TO   WK-SHOHIN-NO-OLD
                                                  WK-SHOHIN-NO-NEW
                MOVE   IN01-TYUMON-SU        TO   WK-SUM-TYUMON-SU
           END-READ.
       INIT-PROC-EXIT.
      *>
           EXIT.
      *>-----------------------------------------------------------------------
      *>終了処理
      *>-----------------------------------------------------------------------
       TERM-PROC                         SECTION.
       CLOSE   IN01-FILE
               OT01-FILE.
       TERM-PROC-EXIT.
      *>
           EXIT.
