       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 LOOP_SAMPLE02.
      *>
       ENVIRONMENT                 DIVISION.
       DATA                        DIVISION.
      *>
       WORKING-STORAGE             SECTION.
           01 WK-SUJI-I              PIC 99.
           01 WK-SUJI-TOTAL          PIC 99.
       PROCEDURE DIVISION.
       MOVE  ZERO TO  WK-SUJI-I.
       MOVE  ZERO TO  WK-SUJI-TOTAL.
      *>
      *>指定回数繰り返す処理
       PERFORM VARYING WK-SUJI-I FROM 1 BY 1 UNTIL WK-SUJI-I > 10
           COMPUTE WK-SUJI-TOTAL = WK-SUJI-TOTAL + WK-SUJI-I
       END-PERFORM.
      *>
      *>最終合計数を表示する
       DISPLAY WK-SUJI-TOTAL.
       STOP RUN.
