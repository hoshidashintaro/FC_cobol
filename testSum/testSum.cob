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
            03   IN01-SHOHIN-ZYOHOU.
                 05   IN01-SHOHIN-CODE.
                      07   IN01-BUNRUI-CODE      PIC X(002).
                      07   IN01-SHOHIN-NO        PIC 9(004).
            03   IN01-TYUMON-ZYOHOU.
                 05   IN01-TYUMON-SU             PIC 9(003).
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
       *>初期処理（ファイルのオープン）
       *>-----------------------------------------------------------------------
       PROCEDURE                          DIVISION.
             MOVE   SPACE   TO   IN-FILE-STATUS.
             MOVE   SPACE   TO   WK-BUNRUI-CODE-OLD.
             MOVE   SPACE   TO   WK-BUNRUI-CODE-NEW.
             MOVE   ZERO    TO   WK-SHOHIN-NO-OLD.
             MOVE   ZERO    TO   WK-SHOHIN-NO-NEW.
             MOVE   ZERO    TO   IN01-TYUMON-SU.
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
               MOVE   IN01-BUNRUI-CODE      TO   WK-BUNRUI-CODE-NEW
                                                 WK-BUNRUI-CODE-OLD
               MOVE   IN01-SHOHIN-NO        TO   WK-SHOHIN-NO-OLD
                                                 WK-SHOHIN-NO-NEW
               MOVE   IN01-TYUMON-SU        TO   WK-SUM-TYUMON-SU
               DISPLAY"NOT AT初期WK-BUNRUI-CODE-NEW:"
               WK-BUNRUI-CODE-NEW
               DISPLAY"NOT AT初期WK-BUNRUI-CODE-OLD:"
               WK-BUNRUI-CODE-OLD
               DISPLAY"NOT AT初期IN01-TYUMON-SU:"IN01-TYUMON-SU
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
               MOVE   WK-BUNRUI-CODE-OLD   TO   OT01-BUNRUI-CODE
               MOVE   WK-SHOHIN-NO-OLD     TO   OT01-SHOHIN-NO
               MOVE   WK-SUM-TYUMON-SU     TO   OT01-TYUMON-SU
          DISPLAY"読み込み終了 AT END OT01-BUNRUI-CODE:"
          OT01-BUNRUI-CODE
          DISPLAY"読み込み終了 AT END OT01-TYUMON-SU:"
          OT01-TYUMON-SU
               WRITE   OT01-RECODE
       *>
               NOT   AT   END
               MOVE   IN01-BUNRUI-CODE     TO   WK-BUNRUI-CODE-NEW
               MOVE   IN01-SHOHIN-NO       TO   WK-SHOHIN-NO-NEW
               DISPLAY"NOT AT END IN01-BUNRUI-CODE:"IN01-BUNRUI-CODE
       *>
       *>      キーブレイク
               IF   WK-BUNRUI-CODE-NEW  =  WK-BUNRUI-CODE-OLD   AND
                    WK-SHOHIN-NO-OLD    =  WK-SHOHIN-NO-NEW
       *>
       *>      データ集計
               THEN
               DISPLAY"キーブレイクWK-BUNRUI-CODE-NEW:"
               WK-BUNRUI-CODE-NEW
               DISPLAY"キーブレイクWK-BUNRUI-CODE-OLD:"
               WK-BUNRUI-CODE-OLD
                   COMPUTE   WK-SUM-TYUMON-SU =
                                    WK-SUM-TYUMON-SU + IN01-TYUMON-SU
               DISPLAY"キーブレイクWK-SUM-TYUMON-SU:"
               WK-SUM-TYUMON-SU
               DISPLAY"キーブレイクIN01-TYUMON-SU:"
               IN01-TYUMON-SU
       *>
       *>      ファイル出力
               ELSE
                   MOVE   WK-BUNRUI-CODE-OLD   TO
                                                  OT01-BUNRUI-CODE
                   MOVE   WK-SHOHIN-NO-OLD   TO
                                                  OT01-SHOHIN-NO
                   MOVE   WK-SUM-TYUMON-SU     TO   OT01-TYUMON-SU
              DISPLAY"ファイル出力OT01-BUNRUI-CODE:"
              OT01-BUNRUI-CODE
              DISPLAY"ファイル出力OT01-TYUMON-SU:"
              OT01-TYUMON-SU
                   WRITE   OT01-RECODE
                   DISPLAY"ELSE WRITE OT01-RECODE:"OT01-RECODE
       *>
       *>次のキーをセット
                   MOVE WK-BUNRUI-CODE-NEW TO WK-BUNRUI-CODE-OLD
                   MOVE WK-SHOHIN-NO-NEW   TO WK-SHOHIN-NO-OLD
                   MOVE IN01-TYUMON-SU TO WK-SUM-TYUMON-SU
                   DISPLAY"次キーセットWK-BUNRUI-CODE-NEW:"
                   WK-BUNRUI-CODE-NEW
                   DISPLAY"次キーセットWK-BUNRUI-CODE-OLD:"
                   WK-BUNRUI-CODE-OLD
               END-IF
           END-READ
       END-PERFORM.
       *>-----------------------------------------------------------------------
       *>終了処理
       *>-----------------------------------------------------------------------
       CLOSE   IN01-FILE
               OT01-FILE.
       STOP RUN.
