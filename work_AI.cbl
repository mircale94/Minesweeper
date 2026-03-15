       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORK_AI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * P=旗子, O=未知, X=地雷
      * 1-8=周圍雷數, .=安全(0雷), T=爆炸
      * F=標對(Flagged), I=標錯(Incorrect), M=漏抓(Missed)

      * --- 暫存變數 (修復變數位置錯誤) ---
       01  TEMP-L PIC 999.
       01  TEMP-E PIC 999.

       01  PLAY-LIST.
           05 PLAY-LIST-RP OCCURS 10 TIMES.
               10 PLAY-LIST-BOOM PIC X(10) VALUE "OOOOOOOOOO".

       01  PLAY-LIST-USER.
           05 PLAY-LIST-RP-USER OCCURS 10 TIMES.
               10 PLAY-LIST-BOOM-USER PIC X(10) VALUE "OOOOOOOOOO".
      
       01  PLAYER-INPUT.
           05 PLAYER-INPUT-E PIC 999 VALUE 0.
           05 PLAYER-INPUT-L PIC 999 VALUE 0.
           05 PLAYER-INPUT-BOOM PIC 999 VALUE 0.

       01  WS-RAND-FLOAT      PIC V9(9).
       01  WS-RAND-INT        PIC 9(3).
       01  WS-MIN             PIC 9(3) VALUE 0.
       01  WS-MAX             PIC 9(3) VALUE 100.
       01  WS-RANGE           PIC 9(3).
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURR-DATE   PIC 9(8).
           05  WS-CURR-TIME   PIC 9(8).
       01  WS-SEED            PIC 9(9).
       
       01  LIST-INT PIC 99 VALUE 0.
       01  WS-ROW-DISP PIC 99.

       01  LIST-INDEX-E PIC 999 VALUE 0.
       01  LIST-INDEX-L PIC 999 VALUE 0.

       01  BOOM-INT PIC 999 VALUE 0.
       
       01  USER-INPUT-L PIC 999.
       01  USER-INPUT-E PIC 999.

       01  USER-INPUT-L-STR PIC X(3).
       01  USER-INPUT-E-STR PIC X(3).
       01  USER-INPUT-DO-WHAT PIC X VALUE "0".

       01  USER-ERROR PIC 999 VALUE 0.
       01  USER-ISRT PIC 999 VALUE 0.

      * 掃描與計算用變數
       01  SCAN-R PIC S99.
       01  SCAN-C PIC S99.
       01  CHECK-L PIC 999.
       01  CHECK-E PIC 999.
       01  NEARBY-MINES PIC 9.
       01  CHANGED-FLAG PIC X VALUE "N".

      * 顯示緩衝區
       01  I PIC 99.
       01  WS-DISPLAY-BUFFER.
           05 WS-CELLS OCCURS 10 TIMES.
               10 WS-SEP PIC X VALUE "|".
               10 WS-VAL PIC X.
           05 WS-FINAL-SEP PIC X VALUE "|".
       
       PROCEDURE DIVISION.
      * INIT
           PERFORM 999-TO-START THRU 999-EXIT.
       
           DISPLAY "請輸入地雷數量 (上限 30):".
           PERFORM UNTIL PLAYER-INPUT-BOOM > 0 AND <= 30
               ACCEPT PLAYER-INPUT-BOOM
               IF PLAYER-INPUT-BOOM <= 0 OR > 30
                   DISPLAY "數量錯誤! 請輸入 1~30:"
               END-IF
           END-PERFORM.
           
           PERFORM UNTIL USER-ISRT + USER-ERROR = PLAYER-INPUT-BOOM
               PERFORM 000-GAME-LOOP
           END-PERFORM.

           DISPLAY "=============================================".
           DISPLAY "遊戲結束".
           DISPLAY "=============================================".
      *    呼叫新的結算地圖
           PERFORM 109-SHOW-FINAL-MAP THRU 109-EXIT.
           
           DISPLAY "---------------------------------------------".
           DISPLAY "標對地雷: " USER-ISRT.
           DISPLAY "踩爆地雷: " USER-ERROR.
           STOP RUN.

       000-GAME-LOOP.
           PERFORM UNTIL BOOM-INT = PLAYER-INPUT-BOOM
               PERFORM 100-START-RD THRU 100-EXIT
               PERFORM 102-SET-BOOM THRU 102-EXIT
           END-PERFORM.
           
           PERFORM 104-USER-INPUT THRU 104-EXIT.
     
           DISPLAY "請選擇動作:".
           DISPLAY "---------------".
           DISPLAY "P = 插旗 (標記地雷)".
           DISPLAY "O = 挖開 (自動擴散)".
           DISPLAY "---------------".
           ACCEPT USER-INPUT-DO-WHAT.
           
           EVALUATE USER-INPUT-DO-WHAT
               WHEN "P"
               WHEN "p"
                   PERFORM 106-ACTION-FLAG THRU 106-EXIT
                   
               WHEN "O"
               WHEN "o"
                   PERFORM 107-ACTION-OPEN THRU 107-EXIT

               WHEN OTHER
                   DISPLAY "無效的指令，請重試"
           END-EVALUATE.
           
           DISPLAY "目前進度:".
           DISPLAY "已標記 " USER-ISRT " + 已踩爆 " USER-ERROR.
           DISPLAY "總目標: " PLAYER-INPUT-BOOM.

       100-START-RD.
           COMPUTE WS-RAND-FLOAT = FUNCTION RANDOM
           COMPUTE WS-RAND-INT = WS-MIN + 
                   (WS-RAND-FLOAT * WS-RANGE).
       100-EXIT.
       
       101-BOOM-WHERE.
           DISPLAY "   解答地圖 (Debug):"
           DISPLAY "     1 2 3 4 5 6 7 8 9 0"
           DISPLAY "---------------------------------------------------"
           
           PERFORM VARYING LIST-INT FROM 1 BY 1 UNTIL LIST-INT > 10
               MOVE "|" TO WS-FINAL-SEP
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                   MOVE "|" TO WS-SEP(I)
      *            將解答轉為符號: X->* (雷), O->. (空)
                   IF PLAY-LIST-BOOM(LIST-INT)(I:1) = "X"
                       MOVE "*" TO WS-VAL(I)
                   ELSE
                       MOVE "." TO WS-VAL(I)
                   END-IF
               END-PERFORM
               
               IF LIST-INT < 10
                   MOVE LIST-INT TO WS-ROW-DISP
                   DISPLAY " 0" WS-ROW-DISP WS-DISPLAY-BUFFER
               ELSE
                   DISPLAY " 000" WS-DISPLAY-BUFFER
               END-IF
           END-PERFORM.
       101-EXIT.
           
       102-SET-BOOM. 
           DIVIDE WS-RAND-INT BY 10 GIVING LIST-INDEX-L 
                   REMAINDER LIST-INDEX-E
           
           ADD 1 TO LIST-INDEX-L
           ADD 1 TO LIST-INDEX-E

           IF LIST-INDEX-L <= 10 AND LIST-INDEX-E <= 10
               IF PLAY-LIST-BOOM(LIST-INDEX-L)(LIST-INDEX-E:1) = "O"
                   ADD 1 TO BOOM-INT
                   MOVE "X" TO 
                     PLAY-LIST-BOOM(LIST-INDEX-L)(LIST-INDEX-E:1)
               END-IF
           END-IF.
       102-EXIT.

       103-USER-MAP.
           DISPLAY "   玩家地圖:"
           DISPLAY "     1 2 3 4 5 6 7 8 9 0"
           DISPLAY "---------------------------------------------------"
           
           PERFORM VARYING LIST-INT FROM 1 BY 1 UNTIL LIST-INT > 10
               MOVE "|" TO WS-FINAL-SEP
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                   MOVE "|" TO WS-SEP(I)
                   MOVE PLAY-LIST-BOOM-USER(LIST-INT)(I:1) 
                     TO WS-VAL(I)
               END-PERFORM
               
               IF LIST-INT < 10
                   MOVE LIST-INT TO WS-ROW-DISP
                   DISPLAY " 0" WS-ROW-DISP WS-DISPLAY-BUFFER
               ELSE
                   DISPLAY " 000" WS-DISPLAY-BUFFER
               END-IF
           END-PERFORM.
       103-EXIT.

       104-USER-INPUT.
           DISPLAY "-------------------"
           DISPLAY "輸入 X 座標 (1~A/10):"
           DISPLAY "(map=看地圖, bug=看解答, hp=看說明)"
           ACCEPT USER-INPUT-E-STR
           
           EVALUATE USER-INPUT-E-STR
               WHEN "bug"
                 PERFORM 101-BOOM-WHERE THRU 101-EXIT
                 GO TO 104-USER-INPUT
               WHEN "map"
                 PERFORM 103-USER-MAP THRU 103-EXIT
                 GO TO 104-USER-INPUT
               WHEN "hp "
                 DISPLAY "P=旗子, O=未知, .=安全, 數字=雷數"
                 GO TO 104-USER-INPUT
           END-EVALUATE

           MOVE USER-INPUT-E-STR TO USER-INPUT-E
           
           PERFORM UNTIL USER-INPUT-E >= 1 AND <= 10
               DISPLAY "錯誤! 請輸入 1~10:"
               ACCEPT USER-INPUT-E-STR
               MOVE USER-INPUT-E-STR TO USER-INPUT-E
           END-PERFORM

           DISPLAY "輸入 Y 座標 (1~A/10):"
           ACCEPT USER-INPUT-L
           
           PERFORM UNTIL USER-INPUT-L >= 1 AND <= 10
               DISPLAY "錯誤! 請輸入 1~10:"
               ACCEPT USER-INPUT-L
           END-PERFORM.
       104-EXIT.

       106-ACTION-FLAG.
           IF PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1) 
              NOT = "O" AND NOT = "P"
               DISPLAY ">>> 這裡已經開過了!"
           ELSE
               MOVE "P" TO 
                 PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               
               IF PLAY-LIST-BOOM(USER-INPUT-L)(USER-INPUT-E:1) = "X"
                   ADD 1 TO USER-ISRT
                   DISPLAY ">>> 標記成功"
               ELSE
                   DISPLAY ">>> 標記完成"
               END-IF
           END-IF.
       106-EXIT.

       107-ACTION-OPEN.
           IF PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1) 
              NOT = "O" AND NOT = "P"
                DISPLAY ">>> 這裡已經開過了!"
           ELSE
                IF PLAY-LIST-BOOM(USER-INPUT-L)(USER-INPUT-E:1) = "X"
      *             踩到雷
                    MOVE "T" TO 
                      PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
                    ADD 1 TO USER-ERROR
                    DISPLAY "!!! 爆炸 !!!"
                ELSE
      *             沒踩到雷，計算周圍雷數並擴散
                    PERFORM 110-CALC-AND-FILL 
                            THRU 110-EXIT
                    PERFORM 111-AUTO-EXPAND 
                            THRU 111-EXIT
                    PERFORM 103-USER-MAP THRU 103-EXIT
                END-IF
           END-IF.
       107-EXIT.

      * --- 計算單格雷數並填入 ---
       110-CALC-AND-FILL.
           MOVE 0 TO NEARBY-MINES
           
           PERFORM VARYING SCAN-R FROM -1 BY 1 UNTIL SCAN-R > 1
             PERFORM VARYING SCAN-C FROM -1 BY 1 UNTIL SCAN-C > 1
                COMPUTE CHECK-L = USER-INPUT-L + SCAN-R
                COMPUTE CHECK-E = USER-INPUT-E + SCAN-C
                
                IF CHECK-L >= 1 AND <= 10 AND 
                   CHECK-E >= 1 AND <= 10
                    IF PLAY-LIST-BOOM(CHECK-L)(CHECK-E:1) = "X"
                        ADD 1 TO NEARBY-MINES
                    END-IF
                END-IF
             END-PERFORM
           END-PERFORM.

      *    將數字轉為字元 (1~9, A)
           EVALUATE NEARBY-MINES
               WHEN 0
                   MOVE "." TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 1
                   MOVE "1" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 2
                   MOVE "2" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 3
                   MOVE "3" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 4
                   MOVE "4" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 5
                   MOVE "5" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 6
                   MOVE "6" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 7
                   MOVE "7" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN 8
                   MOVE "8" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
               WHEN OTHER
                   MOVE "A" TO 
                     PLAY-LIST-BOOM-USER(USER-INPUT-L)(USER-INPUT-E:1)
           END-EVALUATE.
       110-EXIT.

      * --- 自動擴散 (Flood Fill 模擬) ---
       111-AUTO-EXPAND.
           MOVE "Y" TO CHANGED-FLAG
           
           PERFORM UNTIL CHANGED-FLAG = "N"
               MOVE "N" TO CHANGED-FLAG
               
               PERFORM VARYING LIST-INT FROM 1 BY 1 UNTIL LIST-INT > 10
                 PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
      *            如果發現一個已經被標記為安全(.)的格子
                   IF PLAY-LIST-BOOM-USER(LIST-INT)(I:1) = "."
                       
      *                檢查它周圍 8 格是否有未開的(O)
               PERFORM VARYING SCAN-R FROM -1 BY 1 UNTIL SCAN-R > 1
                 PERFORM VARYING SCAN-C FROM -1 BY 1 UNTIL SCAN-C > 1
                    COMPUTE CHECK-L = LIST-INT + SCAN-R
                    COMPUTE CHECK-E = I + SCAN-C
                    
                    IF CHECK-L >= 1 AND <= 10 AND 
                       CHECK-E >= 1 AND <= 10
                        
                        IF PLAY-LIST-BOOM-USER(CHECK-L)(CHECK-E:1) = "O"
                            
      *                     保存當前 USER-INPUT
                            MOVE USER-INPUT-L TO TEMP-L
                            MOVE USER-INPUT-E TO TEMP-E
                            
      *                     移動去算那一格
                            MOVE CHECK-L TO USER-INPUT-L
                            MOVE CHECK-E TO USER-INPUT-E
                            PERFORM 110-CALC-AND-FILL THRU 110-EXIT
                            
      *                     還原
                            MOVE TEMP-L TO USER-INPUT-L
                            MOVE TEMP-E TO USER-INPUT-E

                                    MOVE "Y" TO CHANGED-FLAG
                                END-IF
                            END-IF
                         END-PERFORM
                       END-PERFORM
                   END-IF
                 END-PERFORM
               END-PERFORM
           END-PERFORM.
       111-EXIT.

      * --- 結算地圖顯示 ---
       109-SHOW-FINAL-MAP.
           DISPLAY "   最終結算地圖:"
           DISPLAY "     1 2 3 4 5 6 7 8 9 0"
           DISPLAY "---------------------------------------------------"
           
           PERFORM VARYING LIST-INT FROM 1 BY 1 UNTIL LIST-INT > 10
               MOVE "|" TO WS-FINAL-SEP
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                   MOVE "|" TO WS-SEP(I)
                   
      *            取出玩家當前狀態
                   MOVE PLAY-LIST-BOOM-USER(LIST-INT)(I:1) 
                     TO WS-VAL(I)
                     
      *            結算邏輯:
      *            1. 如果是爆炸(T)，保持 T
      *            2. 如果是標記(P)，檢查是否是對的
      *               - 對的 (底層是X) -> 顯示 F (Found)
      *               - 錯的 (底層是O) -> 顯示 I (Incorrect)
      *            3. 如果是未知(O)
      *               - 底層是雷(X) -> 顯示 M (Missed)
      *               - 底層是空(O) -> 顯示 . (Safe)
                   
                   EVALUATE WS-VAL(I)
                       WHEN "T"
                           CONTINUE
                       WHEN "P"
                           IF PLAY-LIST-BOOM(LIST-INT)(I:1) = "X"
                               MOVE "F" TO WS-VAL(I)
                           ELSE
                               MOVE "I" TO WS-VAL(I)
                           END-IF
                       WHEN "O"
                           IF PLAY-LIST-BOOM(LIST-INT)(I:1) = "X"
                               MOVE "M" TO WS-VAL(I)
                           ELSE
                               MOVE "." TO WS-VAL(I)
                           END-IF
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE

               END-PERFORM
               
               IF LIST-INT < 10
                   MOVE LIST-INT TO WS-ROW-DISP
                   DISPLAY " 0" WS-ROW-DISP WS-DISPLAY-BUFFER
               ELSE
                   DISPLAY " 000" WS-DISPLAY-BUFFER
               END-IF
           END-PERFORM.
           
           DISPLAY "圖例: [1-8]=雷數, .=安全, F=標對, I=標錯,"
           DISPLAY "      T=踩爆, M=漏掉".
       109-EXIT.

       999-TO-START.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           COMPUTE WS-SEED = FUNCTION NUMVAL(WS-CURR-TIME)
           COMPUTE WS-RAND-FLOAT = FUNCTION RANDOM(WS-SEED)
           COMPUTE WS-RANGE = WS-MAX - WS-MIN + 1.
       999-EXIT.

       END PROGRAM WORK_AI.
