      *>************************************************************************
      *>テスト用 入力ファイルの判定プログラム
      *>************************************************************************
      *>見出し部
      *>************************************************************************
       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   JUDGE001.
      *>************************************************************************
      *>環境部
      *>************************************************************************
       ENVIRONMENT                   DIVISION.
       CONFIGURATION                 SECTION.
       INPUT-OUTPUT                  SECTION.
       FILE-CONTROL.
      *>************************************************************************
      *>IN01入力ファイル
      *>************************************************************************
       SELECT   IN01-FILE     ASSIGN        TO "IN01.txt"
                              ORGANIZATION IS LINE SEQUENTIAL
                              STATUS IN-FILE-STATUS.
      *>************************************************************************
      *>PT01プリントファイル
      *>************************************************************************
       SELECT   OT01-FILE     ASSIGN         TO "0T01.txt"
                              ORGANIZATION IS LINE SEQUENTIAL.
      *>************************************************************************
      *>データ部
      *>************************************************************************
       DATA                          DIVISION.
       FILE                          SECTION.
      *>************************************************************************
      *>IN01-FILEのレイアウト定義
      *>************************************************************************
       FD   IN01-FILE.
       01   IN01-RECODE.
          03   IN01-ZYUTYU-BANGOU.
                05   IN01-MISEBAN                   PIC X(003).
                05   IN01-TYUMON-BANGOU             PIC 9(005).
      *>************************************************************************
      *>OT01-FILEのレイアウト定義
      *>************************************************************************
       FD   OT01-FILE.
       01   OT01-RECODE.
          03   OT01-ZYUTYU-BANGOU.
                05   OT01-MISEBAN                   PIC X(003).
                05   OT01-TYUMON-BANGOU             PIC 9(005).
      *>************************************************************************
      *>作業領域の定義
      *>************************************************************************
       WORKING-STORAGE               SECTION.
      *>
       01   WRK-WOEK-AREA.
             03   WRK-COUNT                        PIC 9(006).
      *>
      *>出力件数を表示する領域
       01   MS3-MESSAGE-AREA.
            03   FILLER                       PIC X(020)
                                        VALUE "出力件数：".
            03   MSG3-COUNT                   PIC ZZZ,ZZ9.
      *>ステータスの領域を定義を設定する
       01  IN-FILE-STATUS                           PIC XX.
      *>************************************************************************
      *>手続き部
      *>************************************************************************
       PROCEDURE                     DIVISION.
      *>
      *>    主処理
             PERFORM   MAIN-PROC.
      *>
      *>    終了処理
             PERFORM   TERM-PROC.
      *>
       STOP RUN.
      *>************************************************************************
      *>主処理
      *>************************************************************************
       MAIN-PROC                     SECTION.
      *>
      *>    初期処理を行う（遷移する）
             PERFORM   INIT-PROC.
      *>
      *>  ファイルのオープン
           OPEN   INPUT    IN01-FILE
                  OUTPUT   OT01-FILE.
      *>
      *>  入力ファイルの読み込み
           PERFORM    IN01-FILE-READ-PROC.
      *>
       MAIN-PROC-EXIT.
      *>
           EXIT.
      *>************************************************************************
      *>初期処理
      *>************************************************************************
       INIT-PROC                     SECTION.
      *>
      *>  作業領域の初期化
           MOVE   ZERO        TO   WRK-COUNT.
           MOVE   SPACE       TO   IN-FILE-STATUS.
      *>
       INIT-PROC-EXIT.
      *>
           EXIT.
      *>************************************************************************
      *>終了処理
      *>************************************************************************
       TERM-PROC                     SECTION.
      *>
      *>  ファイルのクローズ
           CLOSE   IN01-FILE
                   OT01-FILE.
      *>
      *>  出力件数の表示
       IF WRK-COUNT = ZERO THEN
       DISPLAY "IN01-FILEが空です"
       ELSE
           MOVE   WRK-COUNT TO MSG3-COUNT
           DISPLAY   MS3-MESSAGE-AREA UPON CONSOLE
       END-IF.
      *>
       TERM-PROC-EXIT.
      *>
           EXIT.
      *>************************************************************************
      *>IN01-FILEファイルの読み込み・書き込み処理
      *>************************************************************************
       IN01-FILE-READ-PROC       SECTION.
      *>
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
      *>
      *>  読み込み終了時
           READ IN01-FILE
               AT    END
      *>
      *>            READ ENDを表示して処理を終了する
                     DISPLAY "READ END"
      *>
      *>      読み込み時
               NOT   AT     END
      *>
      *>      IN01-FILEにレコードがない場合
               IF IN01-RECODE = SPACE THEN
               DISPLAY"IN01-RECODE AT END:"IN01-RECODE
      *>
      *>      IN01-FILEがないエラーを出力する
                     MOVE   ZERO   TO   WRK-COUNT
      *>
      *>      IN01-FILEにレコードがある場合
               ELSE IF IN01-RECODE >= 1 THEN
      *>
      *>      IN01-MISEBANが文字列であるかを判定する
       IF   IN01-TYUMON-BANGOU IS NOT NUMERIC
       OR   FUNCTION STORED-CHAR-LENGTH(IN01-TYUMON-BANGOU) NOT = 5
       *>OR   IN01-TYUMON-BANGOU  = SPACE
       THEN
          DISPLAY  "不適切な値です"
          STOP RUN
       *>ELSE
       END-IF
       END-IF
      *>
               MOVE      IN01-MISEBAN         TO   OT01-MISEBAN
               MOVE      IN01-TYUMON-BANGOU   TO   OT01-TYUMON-BANGOU
               WRITE     OT01-RECODE
                     ADD   1   TO   WRK-COUNT

           END-READ
       END-PERFORM.
      *>
       IN01-FILE-READ-PROC-EXIT.
      *>
           EXIT.
      *>************************************************************************
      *>IN01-FILEファイルの読み込み・書き込み処理
      *>************************************************************************
       *>IN01-FILE-READ-PROC       SECTION.
      *>
       *>PERFORM UNTIL IN-FILE-STATUS NOT = "00"
      *>
      *>  読み込み終了時
      *>     READ IN01-FILE
      *>         AT    END
      *>
      *>            READ ENDを表示して処理を終了する
      *>               DISPLAY "READ END"
      *>
      *>      読み込み時
      *>         NOT   AT     END
      *>
      *>      IN01-FILEにレコードがない場合
      *>         *>IF IN01-RECODE = SPACE THEN
      *>
      *>      WRK-COUNTに ZERO を代入して０件を出力する
      *>         *>MOVE   ZERO   TO   WRK-COUNT
      *>
      *>      IN01-FILEにレコードがある場合
      *>         *>ELSE IF IN01-RECODE >= 1 THEN
      *>
      *>      IN01-FILEの値をOT01-FILEに代入する
      *>
      *>         MOVE      IN01-MISEBAN         TO   OT01-MISEBAN
      *>         MOVE      IN01-TYUMON-BANGOU   TO   OT01-TYUMON-BANGOU
      *>         WRITE     OT01-RECODE
      *>               ADD   1   TO   WRK-COUNT
      *>
      *>     END-READ
      *> END-PERFORM.
      *>
      *> IN01-FILE-READ-PROC-EXIT.
      *>
      *>     EXIT.
