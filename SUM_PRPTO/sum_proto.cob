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
       SELECT    IN01-ZYUTYU-FILE      ASSIGN       TO   "IN01.txt"
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS  IS IN-FILE-STATUS.
      *>-----------------------------------------------------------------------
      *>出力ファイル
      *>-----------------------------------------------------------------------
       SELECT    OT01-TYUMON-SU-FILE   ASSIGN       TO   "OT01.txt"
                                       ORGANIZATION IS LINE SEQUENTIAL.
      *>-----------------------------------------------------------------------
      *>データ部
      *>-----------------------------------------------------------------------
       DATA                               DIVISION.
       FILE                               SECTION.
      *>----------------------------------------------------------------------------
      *>入力ファイルのレイアウト定義
      *>----------------------------------------------------------------------------
       FD   IN01-ZYUTYU-FILE.
       01   IN01-RECODE.
            03   IN01-SHOHIN-ZYOHOU.
                 05   IN01-SHOHIN-ZCODE.
                      07   IN01-BUNRUI-CODE      PIC X(002).
                      07   IN01-SHOHIN-NO        PIC 9(004).
            03   IN01-TYUMON-ZYOHOU.
                 05   IN01-TYUMON-SU             PIC 9(003).
      *>----------------------------------------------------------------------------
      *>出力ファイルのレイアウト定義
      *>----------------------------------------------------------------------------
       FD   OT01-TYUMON-SU-FILE.
       01   OT01-RECODE.
            03   OT01-SHOHIN-ZYOHOU.
                 05   OT01-SHOHIN-ZCODE.
                      07   OT01-BUNRUI-CODE      PIC X(002).
                      07   OT01-SHOHIN-NO        PIC 9(004).
            03   OT01-TYUMON-ZYOHOU.
                 05   OT01-TYUMON-SU             PIC 9(004).
      *>-----------------------------------------------------------------------
      *>作業領域の定義
      *>-----------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
      *>
       01   IN-FILE-STATUS                    PIC XX.
      *>
       01   WRK-WORK-AREA.
            03   WRK-TYUMON-SU-TOTAL          PIC 9(004).
            03   WRK-TYUMON-SU                PIC 9(003).
      *>
      *>前レコードの集計キー保存用
       01   KEY-SUMMARY.
            03   KEY-BUNRUI-CODE              PIC X(002).
            03   KEY-SHOHIN-NO                PIC 9(004).
      *>
      *>01   MS1-MESSAGE-AREA.
      *>     03   FILLER                       PIC X(018) VALUE "正常終了".
      *>-----------------------------------------------------------------------
      *>初期処理
      *>-----------------------------------------------------------------------
       PROCEDURE                          DIVISION.
      *>INIT-PROC                          SECTION.
      *>
             MOVE   SPACE   TO   IN-FILE-STATUS.
             MOVE   ZERO    TO   WRK-TYUMON-SU.
             MOVE   ZERO    TO   WRK-TYUMON-SU-TOTAL.
             MOVE   SPACE   TO   KEY-BUNRUI-CODE.
             MOVE   ZERO    TO   KEY-SHOHIN-NO.
      *>
      *>    ファイルのオープン
             OPEN   INPUT    IN01-ZYUTYU-FILE
                    OUTPUT   OT01-TYUMON-SU-FILE.

      *>INIT-PROC-EXIT.
      *>
      *>    EXIT.
      *>-----------------------------------------------------------------------
      *>初期処理（ファイル読み込み処理）
      *>-----------------------------------------------------------------------
      *>IN01-ZYUTYU-FILE-READ-PROC                          SECTION.
      *>
           READ IN01-ZYUTYU-FILE
                AT     END
                DISPLAY   "READ END"
      *>
               NOT   AT   END
               MOVE   IN01-BUNRUI-CODE   TO   KEY-BUNRUI-CODE
               MOVE   IN01-SHOHIN-NO     TO   KEY-SHOHIN-NO
               MOVE   IN01-TYUMON-SU     TO   WRK-TYUMON-SU
          DISPLAY"初期IN01-BUNRUI-CODE:"IN01-BUNRUI-CODE
          DISPLAY"初期IN01-SHOHIN-NO:"IN01-SHOHIN-NO
          DISPLAY"初期IN01-TYUMON-SU:"IN01-TYUMON-SU
           END-READ.
      *>
      *>      PERFORM   SUMMARY-MAIN-PROC
      *>                                UNTIL   WRK-AT-END  =  CST-END.
      *>IN01-ZYUTYU-FILE-READ-PROC-EXIT.
      *>
      *>    EXIT.
      *>-----------------------------------------------------------------------
      *>主処理
      *>-----------------------------------------------------------------------
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
      *>
           READ   IN01-ZYUTYU-FILE
               AT   END
               DISPLAY "READ END"
               MOVE   KEY-BUNRUI-CODE   TO   OT01-BUNRUI-CODE
               MOVE   KEY-SHOHIN-NO     TO   OT01-SHOHIN-NO
               MOVE   WRK-TYUMON-SU-TOTAL   TO   OT01-TYUMON-SU

               WRITE OT01-RECODE
      *>
               NOT   AT   END
               MOVE   IN01-BUNRUI-CODE   TO   KEY-BUNRUI-CODE
               MOVE   IN01-SHOHIN-NO     TO   KEY-SHOHIN-NO
               MOVE   IN01-TYUMON-SU     TO   WRK-TYUMON-SU
          DISPLAY"主処理IN01-BUNRUI-CODE:"IN01-BUNRUI-CODE
          DISPLAY"主処理IN01-SHOHIN-NO:"IN01-SHOHIN-NO
          DISPLAY"主処理IN01-TYUMON-SU:"IN01-TYUMON-SU

      *>
      *>      キーブレイク
               IF   IN01-BUNRUI-CODE   =   KEY-BUNRUI-CODE   AND
                    IN01-SHOHIN-NO     =   KEY-SHOHIN-NO     THEN
          DISPLAY"キーブレイクIN01-BUNRUI-CODE:"IN01-BUNRUI-CODE
          DISPLAY"キーブレイクKEY-BUNRUI-CODE:"KEY-BUNRUI-CODE
          DISPLAY"キーブレイクIN01-SHOHIN-NO:"IN01-SHOHIN-NO
          DISPLAY"キーブレイクKEY-SHOHIN-NO:"KEY-SHOHIN-NO

      *>
      *>      データ集計
               COMPUTE   WRK-TYUMON-SU-TOTAL =
                            WRK-TYUMON-SU-TOTAL + IN01-TYUMON-SU
         DISPLAY"データ集計WRK-TYUMON-SU-TOTAL:"WRK-TYUMON-SU-TOTAL
         DISPLAY"データ集計IN01-TYUMON-SU:"IN01-TYUMON-SU

      *>
      *>      ファイル出力
               ELSE
               *>IF
                   *>IN01-BUNRUI-CODE   NOT =   KEY-BUNRUI-CODE   AND
                   *>IN01-SHOHIN-NO   NOT  =   KEY-SHOHIN-NO     THEN

                   MOVE   KEY-BUNRUI-CODE       TO   OT01-BUNRUI-CODE
                   MOVE   KEY-SHOHIN-NO         TO   OT01-SHOHIN-NO
                   MOVE   WRK-TYUMON-SU-TOTAL   TO   OT01-TYUMON-SU
                   WRITE   OT01-RECODE
         DISPLAY"ファイル出力KEY-BUNRUI-CODE:"KEY-BUNRUI-CODE
         DISPLAY"ファイル出力KEY-SHOHIN-NO:"KEY-SHOHIN-NO
         DISPLAY"出力WRK-TYUMON-SU-TOTAL:"WRK-TYUMON-SU-TOTAL
      *>
      *>      次のキーをセット
                   MOVE   IN01-BUNRUI-CODE   TO   KEY-BUNRUI-CODE
                   MOVE   IN01-SHOHIN-NO     TO   KEY-SHOHIN-NO
                   MOVE   IN01-TYUMON-SU     TO   WRK-TYUMON-SU
         DISPLAY"次キーIN01-BUNRUI-CODE:"IN01-BUNRUI-CODE
         DISPLAY"次キーIN01-SHOHIN-NO:"IN01-SHOHIN-NO
         DISPLAY"次キーIN01-TYUMON-SU:"IN01-TYUMON-SU
                END-IF
           END-READ
       END-PERFORM.
      *>-----------------------------------------------------------------------
      *>終了処理
      *>-----------------------------------------------------------------------
       CLOSE   IN01-ZYUTYU-FILE
               OT01-TYUMON-SU-FILE.
       STOP RUN.
