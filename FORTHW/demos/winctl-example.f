
 


Численные методы Лаба 2. yGREK heretix  КА-21 ИПСА



Для адаптации к СП-Форту перевел исходный текст
ассемблера в верхний регистр.
Необходимые дополнения к ядру СП-Форта сделаны
в EXT\SPF-ASM.F (он и подгружает ассемблер).


 языке программирования FORTH. Раньше здесь только о Форте и говорилось (старый индекс) - это был первый русский сайт, посвященной этому языку. Здесь я размещал свои трансляторы Форта для DOS и Windows. Никуда эти ресурсы не делись, я по-прежнему программирую на Форте, по-прежнему развиваю свой (уже не только свой :) транслятор "СП-Форт" (SPF), по прежнему раздаю этот транслятор всем интересующимся, только страницу для Форта сделал отдельную. Первый свой транслятор Форта я написал в 1990 году. СП-Форт появился в 1992 году. Жмите сюда, если вы интересуетесь языком Форт! Новости о Форте теперь (с 1999 г) будут публиковаться в основном на новом сайте, который я сделал специально для языка Форт http://www.forth.org.ru/ (RU FIG). А здесь еще о нескольких нетрадиционных языках программирования. 
Извините, сервер на реконструкции. По срочным вопросам пишите 

создает объект
 адрес этой строки
          длина
  на  
    на запуск форт системы
  на  
  доп.буфер для  
  размер этого буфера

 Число для расчета случайного числа
 будет логгировать НЕ через
оконная функция

возвращает истину, если объект освобожден другим потоком
и после этого занят текущим
размер одной строки в буфере текста
размер одной строки в буфере текста
число запоминаемых сообщений lru










DIS-OPT \ для СП-Форта ниже 4.10
REQUIRE button ~yz/lib/winctl.f
SET-OPT

0 VALUE ls
0 VALUE g
0 VALUE c
0 VALUE e

PROC: adds { \ [ 255 ] str }
  str e -text@  str c addstring
PROC;

: newcolor 
  thisctl -pos@ 255 SWAP - DUP 255 rgb thisctl -bgcolor! 
  thisctl -pos@ S>D <# 0 HOLD #S #> DROP ls -text! ;

: newpos ( incr -- )
  thisctl -pos@ + thisctl -min@ MAX thisctl -max@ MIN thisctl -pos!
  newcolor ;

MESSAGES: scroll
M: sb_lineleft  -1  newpos M;
M: sb_lineright  1  newpos M;
M: sb_pageleft  -10 newpos M;
M: sb_pageright  10 newpos M;
M: sb_thumbposition
  wparam HIWORD thisctl -pos! newcolor
M;
MESSAGES;

GROUP mm

PROC: dia
  " Пример диалога" MODAL...
    GRID 
      mm start-group
      1 " Мене" radio | 2 " Текел" radio | 3 " Фарес" radio |
      ===
      multiedit (/ -name e -size 120 25 /) -middle | 
      " >>" button (/ -command adds /) | 
      combo (/ -name c -size 150 300 /) -middle |
      ===
      " 0" label (/ -name ls  -bgcolor blue  -color white /) 10 -width | 
      hscroll (/ -max 255  -notify scroll  -bgcolor white /) -xspan |
    GRID; TO g
    GRID
      1 IMAGE-BASE LoadIconA icon 10 -width | hline -xspan -middle |
      ===
      listbox  " *.*" 0 this lb-dir  (/ -size 120 150 /) -yspan | g |
      ===
      hline -xspan |
      ===
      filler 40 -width |
      "    Ok   " ['] dialog-ok ok-button -right | 
      " Отмена" cancel-button |
    GRID; SHOW
    dialog-termination W: idcancel = IF 
      ." Диалог был отменен" CR
    ELSE
      mm @ -1 = IF ." Ничего не выбрано" ELSE ." Выбор: " mm @ . THEN CR
      ." В комбинированном списке " 
      c -selected@ -1 = IF  ." ничего не выбрано"
      ELSE ." выбрано: " HERE c -selected@ c fromcombo HERE .ASCIIZ THEN
      CR
    THEN
  ...MODAL
PROC;

KEYTABLE
  dia ONKEY vk_space
KEYTABLE;

: run
  WINDOWS...
  0 dialog-window TO winmain
  " Элементы управления" winmain -text!
  winmain create-status
  " Нажмите пробел, чтобы увидеть диалоговое окно..." 0 winmain set-status
  winmain winshow
  ...WINDOWS
  BYE
;

\ 0 TO SPF-INIT?
 ' ANSI>OEM TO ANSI><OEM
\ TRUE TO ?GUI
\ ' run MAINX !
\ S" winctl-example.exe" SAVE  
run
BYE
