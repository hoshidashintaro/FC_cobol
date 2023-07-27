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
       01   WRK-WORK-AREA.
            03   WRK-IN-COUNT                 PIC 9(006).
            03   WRK-OUT-COUNT                PIC 9(006).
      *>
      *>処理が終了したときに終了したことを証明するメッセージを表記する
       01   MS1-MESSAGE-AREA.
            03   FILLER                       PIC X(040)
                          VALUE "SAMPLE03の出力結果".
      *>
      *>処理が終了した際に入力件数を表示する
       01   MS2-MESSAGE-AREA.
            03   FILLER                       PIC X(024)
                                 VALUE "入力ファイル件数：".
            03   MSG2-COUNT                   PIC ZZZ,ZZ9.
      *>
      *>処理が終了した際に出力件数を表示する
       01   MS3-MESSAGE-AREA.
            03   FILLER                       PIC X(024)
                                 VALUE "出力ファイル件数：".
            03   MSG3-COUNT                   PIC ZZZ,ZZ9.
      *>
       01   IN-FILE-STATUS                    PIC XX.
      *>----------------------------------------------------------------------------
      *>手続き部
      *>----------------------------------------------------------------------------
       PROCEDURE                         DIVISION.
      *>
           PERFORM   MAIN-PROC.
      *>
           PERFORM   TERM-PROC.
      *>
           STOP RUN.
      *>----------------------------------------------------------------------------
      *>初期処理
      *>----------------------------------------------------------------------------
       INIT-PROC                         SECTION.
      *>
      *>  ファイルステータスの初期化
           MOVE   SPACE      TO   IN-FILE-STATUS.
      *>
      *>  作業領域の初期化
           MOVE   ZERO       TO   WRK-IN-COUNT.
           MOVE   ZERO       TO   WRK-OUT-COUNT.
      *>
       INIT-PROC-EXIT.
      *>
           EXIT.
      *>----------------------------------------------------------------------------
      *>終了処理
      *>----------------------------------------------------------------------------
       TERM-PROC                         SECTION.
      *>
      *>ファイルのクローズ
           CLOSE   IN01-ZYUTYU-FILE
                   OT01-ZYUTYU-FILE.
      *>
      *>入出力件数の表示
           MOVE   WRK-IN-COUNT  TO MSG2-COUNT.
           MOVE   WRK-OUT-COUNT TO MSG3-COUNT.
      *>
           DISPLAY   MS1-MESSAGE-AREA UPON CONSOLE.
           DISPLAY   MS2-MESSAGE-AREA UPON CONSOLE.
           DISPLAY   MS3-MESSAGE-AREA UPON CONSOLE.
      *>
       TERM-PROC-EXIT.
      *>
           EXIT.
      *>----------------------------------------------------------------------------
      *>主処理
      *>----------------------------------------------------------------------------
       MAIN-PROC                          SECTION.
      *>
           PERFORM   INIT-PROC.
      *>
      *>  ファイルのオープン
           OPEN   INPUT    IN01-ZYUTYU-FILE
                  OUTPUT   OT01-ZYUTYU-FILE.
      *>
      *>  受注ファイルの読み込み
           PERFORM ZYUTYU-FILE-READ-PROC.
      *>
           PERFORM UNTIL IN-FILE-STATUS NOT = "00"
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
      *>書き込み処理
      *>----------------------------------------------------------------------------
       WRITE-PROC                         SECTION.
      *>
           MOVE    IN01-MISEBAN         TO   OT01-MISEBAN.
           MOVE    IN01-TYUMON-BANGOU   TO   OT01-TYUMON-BANGOU.
      *>
           WRITE   OT01-RECODE.
      *>
           ADD     1                    TO   WRK-OUT-COUNT.
      *>
       WRITE-PROC-EXIT.
      *>
           EXIT.
      *>----------------------------------------------------------------------------
      *>受注ファイルの読み込み
      *>----------------------------------------------------------------------------
       ZYUTYU-FILE-READ-PROC              SECTION.
      *>
           READ   IN01-ZYUTYU-FILE
             AT   END
                  DISPLAY "READ END"
      *>
            NOT   AT   END
                  ADD   1   TO   WRK-IN-COUNT
           END-READ.
      *>
       ZYUTYU-FILE-READ-PROC-EXIT.
      *>
           EXIT.
