       *>1.B..+....2....+....3....+....4....+....5....+....6....+....7....+....8
       *>-----------------------------------------------------------------------
       *>サマリーのプログラムサンプル01
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
            03   IN01-BUNRUI         PIC X(002).
            03   IN01-TYUMON-SU      PIC 9(003).
       *>----------------------------------------------------------------------------
       *>出力ファイルのレイアウト定義
       *>----------------------------------------------------------------------------
       FD   OT01-FILE.
       01   OT01-RECODE.
            03   OT01-BUNRUI         PIC X(002).
            03   OT01-TYUMON-SU      PIC 9(004).
       *>-----------------------------------------------------------------------
       *>作業領域の定義
       *>-----------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
       *>
       01   IN-FILE-STATUS           PIC XX.
       *>
       01   WRK-LEY-AREA.
            03   WK-KEY-OLD          PIC X(002).
            03   WK-KEY-NEW          PIC X(002).
       *>
       01   WK-SUM-AREA.
            03   WK-SUM-TYUMON-SU    PIC 9(004).
       *>-----------------------------------------------------------------------
       *>初期処理（ファイルのオープン）
       *>-----------------------------------------------------------------------
       PROCEDURE                          DIVISION.
             MOVE   SPACE   TO   IN-FILE-STATUS.
             MOVE   SPACE   TO   WK-KEY-OLD.
             MOVE   SPACE   TO   WK-KEY-NEW.
             MOVE   ZERO    TO   WK-SUM-TYUMON-SU.
       *>
       *>    ファイルのオープン
             OPEN   INPUT    IN01-FILE
                    OUTPUT   OT01-FILE.
       *>
           READ IN01-FILE
                AT     END
                DISPLAY   "READ END"
           *>
               NOT   AT   END
               MOVE   IN01-BUNRUI      TO   WK-KEY-NEW
                                            WK-KEY-OLD
               MOVE   IN01-TYUMON-SU   TO   WK-SUM-TYUMON-SU
               DISPLAY"NOT AT初期WK-KEY-NEW:"WK-KEY-NEW
               DISPLAY"NOT AT初期WK-KEY-OLD:"WK-KEY-OLD
               DISPLAY"NOT AT初期WK-SUM-TYUMON-SU:"WK-SUM-TYUMON-SU
           END-READ.
       *>-----------------------------------------------------------------------
       *>主処理
       *>-----------------------------------------------------------------------
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
       *>
           READ   IN01-FILE
               AT   END
               DISPLAY   "READ END"
               MOVE   WK-KEY-OLD         TO   OT01-BUNRUI
               MOVE   WK-SUM-TYUMON-SU   TO   OT01-TYUMON-SU
          DISPLAY"読み込み終了 AT END OT01-BUNRUI:"OT01-BUNRUI
          DISPLAY"読み込み終了 AT END OT01-TYUMON-SU:"
          OT01-TYUMON-SU
               WRITE   OT01-RECODE
       *>
               NOT   AT   END
               MOVE   IN01-BUNRUI     TO   WK-KEY-NEW
               DISPLAY"NOT AT END IN01-BUNRUI:"IN01-BUNRUI
       *>
       *>      キーブレイク
               IF   WK-KEY-NEW  =  WK-KEY-OLD
       *>
       *>      データ集計
               THEN
               DISPLAY"キーブレイクWK-KEY-NEW:"WK-KEY-NEW
               DISPLAY"キーブレイクWK-KEY-OLD:"WK-KEY-OLD
                   COMPUTE   WK-SUM-TYUMON-SU =
                                    WK-SUM-TYUMON-SU + IN01-TYUMON-SU
               DISPLAY"キーブレイクWK-SUM-TYUMON-SU:"
               WK-SUM-TYUMON-SU
               DISPLAY"キーブレイクIN01-TYUMON-SU:"
               IN01-TYUMON-SU
       *>
       *>      ファイル出力
               ELSE
                   MOVE   WK-KEY-OLD         TO   OT01-BUNRUI
                   MOVE   WK-SUM-TYUMON-SU   TO   OT01-TYUMON-SU
              DISPLAY"ファイル出力OT01-BUNRUI:"OT01-BUNRUI
              DISPLAY"ファイル出力OT01-TYUMON-SU:"OT01-TYUMON-SU
                   WRITE   OT01-RECODE
                   DISPLAY"ELSE WRITE OT01-RECODE:"OT01-RECODE
       *>
       *>次のキーをセット
                   MOVE WK-KEY-NEW TO WK-KEY-OLD
                   MOVE IN01-TYUMON-SU TO WK-SUM-TYUMON-SU
                   DISPLAY"次キーセットWK-KEY-NEW:"WK-KEY-NEW
                   DISPLAY"次キーセットWK-KEY-OLD:"WK-KEY-OLD
               END-IF
           END-READ
       END-PERFORM.
       *>-----------------------------------------------------------------------
       *>終了処理
       *>-----------------------------------------------------------------------
       CLOSE   IN01-FILE
               OT01-FILE.
       STOP RUN.
