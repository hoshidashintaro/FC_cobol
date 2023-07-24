      *>************************************************************************
      *>FC課題2 プリント処理のプログラム
      *>************************************************************************
      *>見出し部
      *>************************************************************************
       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   PRINT002.
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
       SELECT   PRT-FILE      ASSIGN         TO "PT01.txt"
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
                05   IN01-TYUMON-BANDOU             PIC 9(005).
      *>************************************************************************
      *>件数印刷のレイアウト定義
      *>************************************************************************
       FD   PRT-FILE.
       01   PRT-RECODE                              PIC ZZZ,ZZ9.
      *>************************************************************************
      *>作業領域の定義
      *>************************************************************************
       WORKING-STORAGE               SECTION.
      *>
       01   WRK-WOEK-AREA.
             03   WRK-COUNT                        PIC 9(006).
      *>
      *>ステータスの領域を定義を設定する
       01  IN-FILE-STATUS                           PIC XX.
      *>************************************************************************
      *>印刷用パーツ定義
      *>************************************************************************
       01   HD01-PRT-COUNT.
             03   PRT-COUNT                        PIC ZZZ,ZZ9.
      *>************************************************************************
      *>手続き部
      *>************************************************************************
       PROCEDURE                     DIVISION.
      *>
             PERFORM   MAIN-PROC.
      *>
             PERFORM   PRINT-PROC.
      *>
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
                  OUTPUT   PRT-FILE.
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
      *>  印刷用パーツ定義の初期化
           MOVE   ZERO        TO   PRT-COUNT.
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
                   PRT-FILE.
      *>
       TERM-PROC-EXIT.
      *>
           EXIT.
      *>************************************************************************
      *>IN01-FILEファイルの読み込み
      *>************************************************************************
       IN01-FILE-READ-PROC       SECTION.
      *>
       PERFORM UNTIL IN-FILE-STATUS NOT = "00"
           READ IN01-FILE
               AT    END
                     DISPLAY "READ END"
      *>
               NOT   AT     END
               IF IN01-RECODE = SPACE THEN
               MOVE   ZERO   TO   WRK-COUNT
               *>
               ELSE IF IN01-RECODE >= 1 THEN
                     ADD   1   TO   WRK-COUNT
      *>
           END-READ
       END-PERFORM.
      *>************************************************************************
      *>印刷処理
      *>************************************************************************
       PRINT-PROC       SECTION.

      *>
      *>      件数の代入と印刷処理
               MOVE      WRK-COUNT            TO   PRT-COUNT.
      *>
               WRITE     PRT-RECODE         FROM   PRT-COUNT.
      *>
       PRINT-PROC-EXIT.
      *>
           EXIT.
       *>************************************************************************
