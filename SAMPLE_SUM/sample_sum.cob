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
            03   IN01-YEAR      PIC X(004).
            03   IN01-SUJI      PIC 99.
       *>----------------------------------------------------------------------------
       *>出力ファイルのレイアウト定義
       *>----------------------------------------------------------------------------
       FD   OT01-TYUMON-SU-FILE.
       01   OT01-RECODE.
            03   OT01-YEAR      PIC X(004).
            03   OT01-SUJI      PIC 999.
       *>-----------------------------------------------------------------------
       *>作業領域の定義
       *>-----------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
       *>
       01   IN-FILE-STATUS      PIC XX.
       *>
       01   WRK-LEY-AREA.
            03   WK-KEY-OLD     PIC X(004).
            03   WK-KEY-NEW     PIC X(004).
       *>
       01   WK-SUM-AREA.
            03   WK-SUM-SUJI    PIC 999.
       *>-----------------------------------------------------------------------
       *>初期処理（ファイルのオープン）
       *>-----------------------------------------------------------------------
             MOVE   SPACE   TO   IN-FILE-STATUS.
             MOVE   SPACE   TO   WK-KEY-OLD.
             MOVE   SPACE   TO   WK-KEY-NEW.
       *>
       *>    ファイルのオープン
             OPEN   INPUT    IN01-ZYUTYU-FILE
                    OUTPUT   OT01-TYUMON-SU-FILE.
       *>
           READ IN01-FILE
                AT     END
                DISPLAY   "READ END"
           *>
               NOT   AT   END
               MOVE   IN01-YEAR   TO   WK-KEY-NOW
                                       WK-KEY-OLD
               MOVE   IN01-SUJI   TO   WK-SUM-SUJI
           END-READ.
       *>-----------------------------------------------------------------------
       *>主処理
       *>-----------------------------------------------------------------------
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
       *>
           READ   IN01-ZYUTYU-FILE
               AT   END
               DISPLAY   "READ END"
               MOVE   WK-KEY-OLD    TO   OT01-YEAR
               MOVE   WK-SUM-SUJI   TO   OT01-SUJI
               WRITE   OT01-RECODE
       *>
               NOT   AT   END
               MOVE   IN01-YEAR     TO   WK-KEY-NEW
       *>
       *>      キーブレイク
               IF   WK-KEY-NEW  =  WK-KEY-OLD
       *>
       *>      データ集計
               THEN
                   COMPUTE   WK-SUM-SUJI = WK-SUM-SUJI + IN01-SUJI
       *>
       *>      ファイル出力
               ELSE
                   MOVE   WK-KEY-OLD    TO   OT01-YEAR
                   MOVE   WK-SUM-SUJI   TO   OT01-SUJI
                   WRITE   OT01-RECODE
       *>
       *>次のキーをセット
                   MOVE WK-KEY-NEW TO WK-KEY-OLD
                   MOVE IN01-SUJI TO WK-KEY-SUJI
               END-IF
           END-READ
       END-PERFORM.
