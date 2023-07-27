      *>----------------------------------------------------------------------------
      *>課題３ 条件一致のみ出力
      *>----------------------------------------------------------------------------
      *>見出し部
      *>----------------------------------------------------------------------------
       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                        SAMPLE03.
      *>----------------------------------------------------------------------------
      *>環境部
      *>----------------------------------------------------------------------------
       ENVIRONMENT                        DIVISION.
       INPUT-OUTPUT                       SECTION.
       FILE-CONTROL.
      *>----------------------------------------------------------------------------
      *>入力ファイル
      *>----------------------------------------------------------------------------
       SELECT    IN01-ZYUTYU-FILE   ASSIGN       TO   "IN01.txt"
                                    ORGANIZATION IS LINE SEQUENTIAL
                                    STATUS IN-FILE-STATUS.
      *>----------------------------------------------------------------------------
      *>出力ファイル
      *>----------------------------------------------------------------------------
       SELECT    OT01-ZYUTYU-FILE   ASSIGN       TO   "OT01.txt"
                                    ORGANIZATION IS LINE SEQUENTIAL.
      *>----------------------------------------------------------------------------
      *>データ部
      *>----------------------------------------------------------------------------
       DATA                               DIVISION.
       FILE                               SECTION.
      *>----------------------------------------------------------------------------
      *>入力ファイルのレイアウト定義
      *>----------------------------------------------------------------------------
       FD   IN01-ZYUTYU-FILE.
       01   IN01-RECODE.
            03   IN01-MISEBAN            PIC X(003).
            03   IN01-TYUMON-BANGOU      PIC 9(005).
      *>----------------------------------------------------------------------------
      *>出力ファイルのレイアウト定義
      *>----------------------------------------------------------------------------
       FD   OT01-ZYUTYU-FILE.
       01   OT01-RECODE.
            03   OT01-MISEBAN            PIC X(003).
            03   OT01-TYUMON-BANGOU      PIC 9(005).
      *>----------------------------------------------------------------------------
      *>作業領域の定義
      *>----------------------------------------------------------------------------
       WORKING-STORAGE                    SECTION.
      *>
       01   COUNT-AREA.
            03   CNT-IN01                     PIC 9(006).
            03   CNT-OT01                     PIC 9(006).
      *>
      *>処理が終了したときに終了したことを証明するメッセージを表記する
       01   MS1-DISPLAY-AREA.
            03   DSP-CLOSING-MSG.
                 05   FILLER                  PIC X(040)
                                VALUE "SAMPLE03の出力結果".
      *>
      *>処理が終了した際に入力件数を表示する
       01   IN01-DISPLAY-AREA.
            03   DSP-IN01.
                 05   FILLER                  PIC X(012)
                                      VALUE "IN01 COUNT:".
                 05   DSP-IN01-CNT            PIC ZZ9.
      *>
      *>処理が終了した際に出力件数を表示する
       01   OT01-DISPLAY-AREA.
            03   DSP-OT01.
                 05   FILLER                  PIC X(012)
                                      VALUE "OT01 COUNT:".
                 05   DSP-OT01-CNT            PIC ZZ9.
      *>
       01   IN-FILE-STATUS                    PIC XX.
      *>----------------------------------------------------------------------------
      *>手続き部
      *>----------------------------------------------------------------------------
       PROCEDURE                         DIVISION.
      *>
           PERFORM   INITIAL-PROC.
      *>
           PERFORM   MAIN-PROC.
      *>
           PERFORM   FINAL-PROC.
      *>
           STOP RUN.
      *>----------------------------------------------------------------------------
      *>初期処理
      *>----------------------------------------------------------------------------
       INITIAL-PROC                         SECTION.
      *>
      *>  ファイルステータスの初期化
           MOVE   SPACE      TO   IN-FILE-STATUS.
      *>
      *>  作業領域の初期化
           MOVE   ZERO       TO   CNT-IN01.
           MOVE   ZERO       TO   CNT-OT01.
      *>
       INITIAL-PROC-EXIT.
      *>
           EXIT.
      *>----------------------------------------------------------------------------
      *>主処理
      *>----------------------------------------------------------------------------
       MAIN-PROC                          SECTION.
      *>
      *>  ファイルのオープン
           OPEN   INPUT    IN01-ZYUTYU-FILE
                  OUTPUT   OT01-ZYUTYU-FILE.
      *>
      *>  受注ファイルの読み込み
        PERFORM UNTIL IN-FILE-STATUS  = "ED"
      *>
           IF     IN01-MISEBAN  =  "T01"   THEN
      *>
                  PERFORM   WRITE-PROC
      *>
                  PERFORM   ZYUTYU-FILE-READ-PROC
      *>
           ELSE   IF   IN01-MISEBAN NOT  =  "T01"   THEN
      *>
               PERFORM   ZYUTYU-FILE-READ-PROC
      *>
           END-IF
      *>
           END-PERFORM.
      *>
       MAIN-PROC-EXIT.
      *>
           EXIT.
      *>----------------------------------------------------------------------------
      *>終了処理
      *>----------------------------------------------------------------------------
       FINAL-PROC                         SECTION.
      *>
      *>入出力件数の表示
           MOVE   CNT-IN01  TO DSP-IN01-CNT.
           MOVE   CNT-OT01  TO DSP-OT01-CNT.
      *>
           DISPLAY   MS1-DISPLAY-AREA  UPON CONSOLE.
           DISPLAY   IN01-DISPLAY-AREA UPON CONSOLE.
           DISPLAY   OT01-DISPLAY-AREA UPON CONSOLE.
      *>
      *>ファイルのクローズ
           CLOSE   IN01-ZYUTYU-FILE
                   OT01-ZYUTYU-FILE.
       FINAL-PROC-EXIT.
      *>
           EXIT.
      *>----------------------------------------------------------------------------
      *>受注ファイルの読み込み
      *>----------------------------------------------------------------------------
       ZYUTYU-FILE-READ-PROC              SECTION.
      *>
           READ   IN01-ZYUTYU-FILE
             AT   END
                  MOVE "ED" TO IN-FILE-STATUS
      *>
            NOT   AT   END
      *>
            IF   IN01-RECODE = SPACE THEN
                 MOVE "END"       TO IN-FILE-STATUS
      *>
            ELSE IF   IN01-RECODE >= 1 THEN
                 ADD   1          TO   CNT-IN01
            END-IF
           END-READ.
      *>
       ZYUTYU-FILE-READ-PROC-EXIT.
      *>
           EXIT.
      *>----------------------------------------------------------------------------
      *>書き込み処理
      *>----------------------------------------------------------------------------
       WRITE-PROC                         SECTION.
      *>
           MOVE    IN01-MISEBAN         TO   OT01-MISEBAN.
           MOVE    IN01-TYUMON-BANGOU   TO   OT01-TYUMON-BANGOU.
      *>
           WRITE   OT01-RECODE.
      *>
           ADD     1                    TO   CNT-OT01.
      *>
       WRITE-PROC-EXIT.
      *>
           EXIT.
